#include "clover/analyze.h"
#include "clover/ast.h"
#include "clover/compilation.h"
#include "clover/typecheck.h"
#include "clover/error.h"
#include "clover/limits.h"
#include "clover/type.h"
#include "util/bits.h"
#include "util/config.h"
#include "util/pool.h"
#include "util/deque.h"

namespace clover {
    using RegionIndex = u32;
    constexpr RegionIndex InvalidRegion = 0x3fffffffu;

    struct RegionWord {
        enum Kind : u32 {
            Singleton,
            Ref,
            Tuple
        };

        constexpr static RegionIndex ExtParent = 0xffffu;

        union {
            struct { Kind kind : 2; RegionIndex parent : 30; };
            struct { u32 tupleSize; };
            struct { RegionIndex fieldId; };
            struct { u32 isOwn : 1; RegionIndex ref : 31; };
        };
    };

    using RegionKind = RegionWord::Kind;

    struct Region;

    struct Regions {
        vec<RegionWord> words;
        biasedset<256> ptrTypes, ptrTypesInited;

        static constexpr RegionIndex Heap = 0, Globals = 1, Stack = 2;

        inline Regions() {
            words.push({ .kind = RegionKind::Singleton, .parent = InvalidRegion }); // Heap
            words.push({ .kind = RegionKind::Singleton, .parent = InvalidRegion }); // Globals
            words.push({ .kind = RegionKind::Singleton, .parent = InvalidRegion }); // Stack
        }

        inline bool hasPointers(Type type) {
            if (ptrTypesInited[type.index])
                return ptrTypes[type.index];

            bool hasPtrs = false;
            switch (type.kind()) {
                case TypeKind::Pointer:
                case TypeKind::Slice:
                    hasPtrs = true;
                    break;
                case TypeKind::Struct:
                    for (u32 i = 0; i < type.asStruct().count(); i ++) if (hasPointers(expand(type.asStruct().fieldType(i)))) {
                        hasPtrs = true;
                        break;
                    }
                    break;
                case TypeKind::Tuple:
                    for (u32 i = 0; i < type.asTuple().count(); i ++) if (hasPointers(expand(type.asTuple().fieldType(i)))) {
                        hasPtrs = true;
                        break;
                    }
                    break;
                case TypeKind::Array:
                case TypeKind::Union:
                    unreachable("TODO: Implement pointer containing semantics for type ", type);
                default:
                    break;
            }
            ptrTypesInited.on(type.index);
            if (hasPtrs)
                ptrTypes.on(type.index);
            return hasPtrs;
        }

        inline Region get(RegionIndex index);
        inline Region def(RegionIndex parent);
        inline Region ref(RegionIndex parent, RegionIndex target, bool isOwn);
        inline Region tuple(RegionIndex parent, u32 size);
        inline Region instance(RegionIndex parent, Type type);
    };

    struct PointerField {
        Regions* regions;
        u32 fieldId;
        u32 own : 1;
        u32 ref : 31;

        inline PointerField(Regions* regions_in, u32 isOwn, u32 fieldId_in, u32 ref_in):
            regions(regions_in), fieldId(fieldId_in), own(isOwn), ref(ref_in) {}

        inline bool owning() const {
            return own;
        }

        inline Region deref();
    };

    struct Region {
        Regions* regions;
        RegionIndex index;

        inline Region(): regions(nullptr), index(InvalidRegion) {}

        inline Region(Regions* regions_in, RegionIndex index_in):
            regions(regions_in), index(index_in) {}

        inline explicit operator bool() const {
            return index != InvalidRegion;
        }

        inline RegionWord nthWord(u32 i) const {
            return regions->words[index + i];
        }

        inline RegionWord& nthWord(u32 i) {
            return regions->words[index + i];
        }

        inline Region parent() {
            return Region(regions, nthWord(0).parent);
        }

        inline bool isRef() {
            return nthWord(0).kind == RegionKind::Ref;
        }

        inline bool isTuple() {
            return nthWord(0).kind == RegionKind::Tuple;
        }

        inline u32 size() {
            assert(isTuple());
            return nthWord(1).tupleSize;
        }

        inline PointerField ptrField(u32 i) {
            assert(isTuple());
            return { regions, nthWord(2 + i * 2).isOwn, nthWord(2 + i * 2).fieldId, nthWord(3 + i * 2).ref };
        }

        inline Region deref() {
            assert(isRef());
            return { regions, nthWord(1).ref };
        }

        inline bool isOwn() {
            assert(isRef());
            return nthWord(1).isOwn;
        }

        inline void setPtrField(u32 i, bool isOwn, u32 fieldId, RegionIndex ref) {
            nthWord(2 + i * 2).fieldId = fieldId;
            nthWord(3 + i * 2).isOwn = !!isOwn;
            nthWord(3 + i * 2).ref = ref;
        }
    };

    inline Region PointerField::deref() {
        return Region(regions, ref);
    }

    inline Region Regions::def(RegionIndex parent) {
        words.push({ .kind = RegionKind::Singleton, .parent = parent });
        return Region(this, words.size() - 1);
    }

    inline Region Regions::ref(RegionIndex parent, RegionIndex target, bool isOwn) {
        words.push({ .kind = RegionKind::Ref, .parent = parent });
        words.push({ .isOwn = !!isOwn, .ref = target });
        return Region(this, words.size() - 2);
    }

    inline Region Regions::tuple(RegionIndex parent, u32 size) {
        u32 index = words.size();
        words.push({ .kind = RegionKind::Tuple, .parent = parent });
        words.push({ .tupleSize = size });
        for (u32 i = 0; i < size; i ++) {
            words.push({ .fieldId = 0 });
            words.push({ .ref = InvalidRegion });
        }
        return Region(this, index);
    }

    inline Region Regions::get(RegionIndex index) {
        return Region(this, index);
    }

    inline bool isSomePointer(Type type) {
        return type.isPtr() || type.isSlice();
    }

    inline bool isOwn(Type type) {
        return (type.isPtr() && type.asPtr().isOwn()) || (type.isSlice() && type.asSlice().isOwn());
    }

    template<typename AggregateType>
    inline Region instanceAggregate(Regions* regions, RegionIndex parent, AggregateType type) {
        if (!regions->hasPointers(type))
            return regions->get(parent);
        u32 ptrFieldCount = 0;
        for (u32 i = 0; i < type.count(); i ++)
            if (regions->hasPointers(expand(type.fieldType(i))))
                ptrFieldCount ++;
        Region tup = regions->tuple(parent, ptrFieldCount);
        ptrFieldCount = 0;
        for (u32 i = 0; i < type.count(); i ++) {
            auto member = expand(type.fieldType(i));
            if (regions->hasPointers(member))
                tup.setPtrField(ptrFieldCount ++, isOwn(member), i, regions->instance(tup.index, member).index);
        }
        return tup;
    }

    inline Region Regions::instance(RegionIndex parent, Type type) {
        switch (type.kind()) {
            case TypeKind::Numeric:
            case TypeKind::Primitive:
                return def(parent);
            case TypeKind::Function:
                // TODO: Revisit this once closures are a thing.
                return def(parent);
            case TypeKind::Named:
                // Even though we might have a tag field, that tag will never
                // be a pointer, so we ignore it and just instantiate the inner
                // type.
                return instance(parent, expand(type.asNamed().innerType()));
            case TypeKind::Struct:
                return instanceAggregate(this, parent, type.asStruct());
            case TypeKind::Tuple:
                return instanceAggregate(this, parent, type.asTuple());
            case TypeKind::Pointer:
                return ref(parent, instance(parent, expand(type.asPtr().elementType())).index, type.asPtr().isOwn());

            case TypeKind::Array:
                // Kinda shaky on this!!! We can't generally assume we see
                // inside an array, but if it contains owning pointers, do we
                // maybe still want to label its region differently? Hard to
                // say, maybe this will become a different kind of region in
                // the future.
                return def(parent);
            case TypeKind::Slice:
                // Likewise, treating slices the same as pointers prevents us
                // from seeing their interior structure. Which is maybe
                // desirable? But maybe not.
                return ref(parent, instance(parent, expand(type.asSlice().elementType())).index, type.asSlice().isOwn());
            case TypeKind::Union:
                unreachable("TODO: Implement region instancing for union types.");
            case TypeKind::Var:
            case TypeKind::Range:
                unreachable("Shouldn't try and instance a variable or range type in analysis pass.");
        }
    }

    struct State {
        Module* module;
        Function* function;
        Regions* regions;
        State* parent;
        vec<u32> scopes;
        vec<u32> locals;
        vec<pair<u32, u32>> variableRegions;
        bitset<256> regionLiveness;

        static State initial(Regions* regions, Module* module) {
            State state;
            state.module = module;
            state.function = nullptr;
            state.regions = regions;
            state.parent = nullptr;
            state.variableRegions.expandTo(module->globals.size(), pair<u32, u32> { InvalidRegion, InvalidRegion });
            return state;
        }

        static State initial(Regions* regions, Function* function) {
            State state;
            state.module = function->module;
            state.function = function;
            state.regions = regions;
            state.parent = nullptr;
            state.variableRegions.expandTo(function->locals.size(), pair<u32, u32> { InvalidRegion, InvalidRegion });
            return state;
        }

        State fork() {
            return *this; // Just a super gross deep copy for now.
        }

        void beginScope() {
            scopes.push(locals.size());
        }

        void createDestructor(vec<IndexedAST>& nodes, Scope* scope, Type type, IndexedAST expr, u32 self, u32 ref) {
            switch (type.kind()) {
                case TypeKind::Pointer:
                    if (type.asPtr().isOwn()) {
                        createDestructor(nodes, scope, type.asPtr().elementType(), module->add(ASTKind::Deref, scope, type.asPtr().elementType(), expr).positionless(), ref, InvalidRegion);
                        nodes.push(module->add(ASTKind::Del, scope, type, expr).positionless());
                    }
                    break;
                case TypeKind::Slice:
                    if (type.asPtr().isOwn()) {
                        nodes.push(module->add(ASTKind::Del, scope, type, expr).positionless());
                    }
                    break;
                default:
                    break;
            }
        }

        void destroy(vec<IndexedAST>& nodes, Scope* scope, u32 var) {
            const auto& info = function ? function->locals[var] : module->globals[var];
            if (info.kind != VariableInfo::Variable)
                return;
            Type type = expand(module->types->get(info.type));
            AST expr = function ? module->add(ASTKind::Local, Local(var)) : module->add(ASTKind::Global, Global(var));
            createDestructor(nodes, scope, type, expr.positionless(), variableRegions[var].first, variableRegions[var].second);
        }

        void endScope(Scope* scope, ChangePosition location) {
            const_slice<u32> inScope = locals[{scopes.back(), locals.size()}];
            vec<IndexedAST> dtors;
            for (u32 var : inScope)
                destroy(dtors, scope, var);
            if (dtors.size()) {
                IndexedAST dtorBlock = module->add(ASTKind::Do, InvalidScope, module->voidType(), dtors).positionless();
                location.replaceWith(module->add(ASTKind::Then, InvalidScope, module->voidType(), location.indexedCurrent(), dtorBlock));
            }
            locals.shrinkBy(inScope.size());
            scopes.pop();
        }

        u32 initialize(RegionIndex parent, Type type, u32 var) {
            switch (type.kind()) {
                case TypeKind::Pointer:
                    if (type.asPtr().isOwn())
                        variableRegions[var] = { parent, regions->def(Regions::Heap).index };
                    else
                        variableRegions[var] = { parent, InvalidRegion };
                    return parent;
                case TypeKind::Slice:
                    if (type.asSlice().isOwn())
                        variableRegions[var] = { parent, regions->def(Regions::Heap).index };
                    else
                        variableRegions[var] = { parent, InvalidRegion };
                    return parent;
                default:
                    variableRegions[var] = { parent, InvalidRegion };
                    return parent;
            }
        }

        void def(u32 var) {
            const auto& info = function ? function->locals[var] : module->globals[var];
            if (info.kind != VariableInfo::Variable)
                return;
            Type type = expand(module->types->get(info.type));
            initialize(Regions::Stack, type, var);
            if (scopes.size())
                locals.push(var);
        }

        pair<RegionIndex, RegionIndex>& operator[](i32 var) {
            return variableRegions[var];
        }

        pair<RegionIndex, RegionIndex> operator[](i32 var) const {
            return variableRegions[var];
        }

        pair<RegionIndex, RegionIndex> unify(Type type, pair<RegionIndex, RegionIndex> a, pair<RegionIndex, RegionIndex> b) {
            // if (a.first == InvalidRegion || b.first == InvalidRegion)
            //     return { InvalidRegion, InvalidRegion };
            // if (a.first != b.first) switch (type.kind()) {
            // }
            // if (a.second != b.second) switch (type.kind()) {
            // }
            return a;
        }

        void unify(State& other) {
            variableRegions.expandTo(other.variableRegions.size(), pair<RegionIndex, RegionIndex> { InvalidRegion, InvalidRegion });
            other.variableRegions.expandTo(variableRegions.size(), pair<RegionIndex, RegionIndex> { InvalidRegion, InvalidRegion });

            for (u32 i = 0; i < variableRegions.size(); i ++) {
                TypeIndex type = function ? function->locals[i].type : module->globals[i].type;
                variableRegions[i] = unify(module->types->get(type), variableRegions[i], other.variableRegions[i]);
            }
        }
    };

    struct RegionValue {
        NodeIndex owner;
        i32 childOrVar;
        RegionIndex selfIndex;
        RegionIndex refIndex;

        inline RegionValue():
            owner(InvalidNode), childOrVar(-1), selfIndex(InvalidRegion), refIndex(InvalidRegion) {}

        inline RegionValue(ChangePosition pos, RegionIndex self_in):
            owner(pos.ast.node), childOrVar(pos.child), selfIndex(self_in), refIndex(InvalidRegion) {}

        inline RegionValue(ChangePosition pos, RegionIndex self_in, RegionIndex ref_in):
            owner(pos.ast.node), childOrVar(pos.child), selfIndex(self_in), refIndex(ref_in) {}

        inline RegionValue(i32 var):
            owner(InvalidNode), childOrVar(-2 - var), selfIndex(InvalidRegion), refIndex(InvalidRegion) {}

        inline explicit operator bool() const {
            return selfIndex != InvalidRegion || childOrVar < -1;
        }

        inline bool isVar() const {
            return childOrVar < -1;
        }

        inline i32 variable() const {
            return -(childOrVar + 2);
        }

        inline Region self(State& state) const {
            if (isVar())
                return state.regions->get(state.variableRegions[variable()].first);
            return state.regions->get(selfIndex);
        }

        inline Region deref(State& state) const {
            if (isVar())
                return state.regions->get(state.variableRegions[variable()].second);
            return state.regions->get(refIndex);
        }

        inline Type type(const State& state) const {
            if (isVar()) {
                auto type = (state.function ? state.function->locals[variable()] : state.module->globals[variable()]).type;
                return expand(state.module->types->get(type));
            }
            return childOrVar == -1 ? typeOf(state.module->node(owner)) : typeOf(state.module->node(owner), childOrVar);
        }

        inline ChangePosition changePos(const State& state) {
            assert(!isVar());
            if (childOrVar == -1)
                return ChangePosition(state.module->node(owner));
            return ChangePosition(state.module->node(owner), childOrVar);
        }

        inline i32 ensureVar(State& state) {
            if (isVar())
                return variable();

            auto pos = changePos(state);
            auto ast = pos.indexedCurrent();
            if (ast.kind() == ASTKind::Local || ast.kind() == ASTKind::Global)
                return ast.variable();
            if (ast.kind() == ASTKind::Bind)
                return ast.child(0).variable();
            auto type = typeOf(pos);
            IndexedAST decl;
            if (state.function)
                decl = state.module->add(ASTKind::Bind, ast.origin(), pos.ast.scope(), type, state.function->addTemp(type.index), ast).reconstituteOrigin();
            else
                decl = state.module->add(ASTKind::Bind, ast.origin(), pos.ast.scope(), type, state.module->addTemp(type.index), ast).reconstituteOrigin();
            pos.replaceWith(decl);
            state.variableRegions.expandTo(decl.child(0).variable() + 1, pair<u32, u32> { InvalidRegion, InvalidRegion });
            return decl.child(0).variable();
        }

        inline pair<RegionIndex, RegionIndex> indices(State& state) {
            if (isVar())
                return state[variable()];
            return { selfIndex, refIndex };
        }
    };

    inline RegionValue none() {
        return RegionValue();
    }

    inline RegionValue data(ChangePosition pos, RegionIndex region) {
        return RegionValue(pos, region);
    }

    inline RegionValue ref(ChangePosition pos, RegionIndex region, RegionIndex ref) {
        return RegionValue(pos, region, ref);
    }

    void invalidate(State& state, i32 var, ChangePosition pos) {
        Type type = RegionValue(var).type(state);
        if (!state.regions->hasPointers(type))
            return;
        switch (type.kind()) {
            case TypeKind::Pointer:
            case TypeKind::Slice:
                state[var].second = InvalidRegion;
                break;
            // case TypeKind::Struct:
            // case TypeKind::Tuple:
            default:
                unreachable("Unexpected type ", type, " - this type should not be able to contain pointers.");
        }
    }

    void checkRead(State& state, RegionValue value, ChangePosition pos) {
        auto indices = value.indices(state);
        bool didError = false;
        if (indices.first == InvalidRegion)
            didError = true;
        else {
            Type type = value.type(state);
            if (!state.regions->hasPointers(type))
                return;
            switch (type.kind()) {
                case TypeKind::Primitive:
                case TypeKind::Function:
                    break;
                case TypeKind::Pointer:
                case TypeKind::Slice:
                    if (indices.second == InvalidRegion)
                        didError = true;
                    break;
                // case TypeKind::Struct:
                // case TypeKind::Tuple:
                default:
                    unreachable("Unexpected type ", type, " - this type should not be able to contain pointers.");
            }
        }
        if (didError)
            error(state.module, value.isVar() ? pos : value.changePos(state), "Tried to read dead value.");
    }

    void destroy(State& state, vec<i32>& toDestroy, RegionValue value, ChangePosition pos, bool immediately) {
        Type type = value.type(state);
        auto indices = value.indices(state);
        if (!state.regions->hasPointers(type))
            return;
        switch (type.kind()) {
            case TypeKind::Pointer:
                if (type.asPtr().isOwn() && indices.second != InvalidRegion) {
                    i32 var = value.ensureVar(state);
                    if (immediately) {
                        if (state.function)
                            pos.replaceWith(state.module->add(ASTKind::DelExpr, pos.origin(), pos.ast.scope(), type, pos.indexedCurrent(), Local(var)));
                        else
                            pos.replaceWith(state.module->add(ASTKind::DelExpr, pos.origin(), pos.ast.scope(), type, pos.indexedCurrent(), Global(var)));
                    } else
                        toDestroy.push(value.ensureVar(state));
                }
                break;
            case TypeKind::Slice:
                if (type.asPtr().isOwn() && indices.second != InvalidRegion) {
                    i32 var = value.ensureVar(state);
                    if (immediately) {
                        if (state.function)
                            pos.replaceWith(state.module->add(ASTKind::DelExpr, pos.origin(), pos.ast.scope(), type, pos.indexedCurrent(), Local(var)));
                        else
                            pos.replaceWith(state.module->add(ASTKind::DelExpr, pos.origin(), pos.ast.scope(), type, pos.indexedCurrent(), Global(var)));
                    } else
                        toDestroy.push(value.ensureVar(state));
                }
                break;
            // case TypeKind::Struct:
            // case TypeKind::Tuple:
            default:
                unreachable("Unexpected type ", type, " - this type should not be able to contain pointers.");
        }
    }

    void consume(State& state, vec<i32>& toDestroy, RegionValue value, ChangePosition pos) {
        if (!value)
            return;
        if (value.isVar())
            return;
        destroy(state, toDestroy, value, pos, false);
    }

    void use(State& state, vec<i32>& toDestroy, RegionValue value, ChangePosition pos) {
        checkRead(state, value, pos);
        consume(state, toDestroy, value, pos);
    }

    void move(State& state, vec<i32>& toDestroy, RegionValue dest, RegionValue src, ChangePosition pos, bool initial) {
        assert(!!dest);
        assert(!!src);

        if (!initial) {
            // Unless this is a fresh initialization, we need to destroy the
            // previous value.
            destroy(state, toDestroy, dest, pos, true);
        }

        checkRead(state, src, pos);

        if (dest.isVar())
            state[dest.variable()] = src.indices(state);
        if (src.isVar())
            invalidate(state, src.variable(), pos);
    }

    void emitAnyDestructors(State& state, const_slice<i32> toDestroy, ChangePosition pos) {
        if (toDestroy.size() == 0)
            return;

        vec<IndexedAST> vars;
        for (i32 var : toDestroy) {
            if (state.function)
                vars.push(state.module->add(ASTKind::Local, Local(var)).positionless());
            else
                vars.push(state.module->add(ASTKind::Global, Global(var)).positionless());
        }
        pos.replaceWith(state.module->add(ASTKind::DelExpr, pos.origin(), pos.ast.scope(), typeOf(pos), pos.indexedCurrent(), vars));
    }

    void emitAnyDestructors(State& state, const vec<i32>& toDestroy, ChangePosition pos) {
        return emitAnyDestructors(state, (const_slice<i32>)toDestroy, pos);
    }

    void discard(State& state, vec<i32>& toDestroy, RegionValue value, ChangePosition pos) {
        // Used when we are consuming something that is discarded. Normally, we
        // want to call consume() first, then collect the results of those
        // calls and move them into a destructor call at the end of the parent
        // expression. But often (in statements) there is no real parent
        // expression and we only care about side effects that already
        // happened. So we don't need to delay or modify the overall toDestroy
        // list.

        if (!value)
            return;

        u32 initialSize = toDestroy.size();
        consume(state, toDestroy, value, pos);
        emitAnyDestructors(state, toDestroy[{initialSize, toDestroy.size()}], value.changePos(state));
        toDestroy.shrinkBy(toDestroy.size() - initialSize);
    }

    RegionValue analyze(State& state, ChangePosition pos) {
        Regions* regions = state.regions;
        RegionValue result;
        vec<i32> toDestroy;
        AST ast = pos.current();
        switch (ast.kind()) {
            case ASTKind::Global:
            case ASTKind::Local:
                return RegionValue(ast.variable());

            case ASTKind::Int:
            case ASTKind::Unsigned:
            case ASTKind::Char:
            case ASTKind::Bool:
                return RegionValue(pos, Regions::Stack);

            case ASTKind::String:
                // TODO: Do we need to consider that it's an array here?
                return RegionValue(pos, Regions::Stack);

            case ASTKind::Deref: {
                auto value = analyze(state, { ast, 0 });
                auto region = value.deref(state);

                if (!region) {
                    error(state.module, ast.indexedChild(0), "Tried to dereference dangling pointer.");
                    return none();
                }
                result = RegionValue(pos, Regions::Stack, region.index);

                use(state, toDestroy, value, pos);
                emitAnyDestructors(state, toDestroy, pos);
                return result;
            }

            case ASTKind::New: {
                auto init = analyze(state, { ast, 0 });
                Region result = regions->instance(Regions::Heap, expand(typeOf(ast).asPtr().elementType()));
                return RegionValue(pos, Regions::Stack, result.index);
            }

            case ASTKind::VarDecl:
                if (!ast.child(1).isVariable())
                    unreachable("TODO: Handle patterns.");
                if (!ast.child(2).missing()) {
                    auto value = analyze(state, { ast, 2 });
                    move(state, toDestroy, RegionValue(ast.child(1).variable()), value, { ast, 2 }, true);
                } else
                    state[ast.child(1).variable()] = { InvalidRegion, InvalidRegion };
                emitAnyDestructors(state, toDestroy, pos);
                state.def(ast.child(1).variable());
                return none();

            case ASTKind::Assign: {
                auto value = analyze(state, { ast, 1 });
                move(state, toDestroy, RegionValue(ast.child(0).variable()), value, { ast, 1 }, false);
                emitAnyDestructors(state, toDestroy, pos);
                return none();
            }

            case ASTKind::IfElse:
                state.beginScope();

                // Condition
                discard(state, toDestroy, analyze(state, { ast, 0 }), { ast, 0 });

                // If-true branch
                state.beginScope();
                discard(state, toDestroy, analyze(state, { ast, 1 }), { ast, 1 });
                state.endScope(ast.child(1).scope(), { ast, 1 });

                // If-false branch
                state.beginScope();
                discard(state, toDestroy, analyze(state, { ast, 1 }), { ast, 1 });
                state.endScope(ast.child(2).scope(), { ast, 2 });

                state.endScope(ast.scope(), ast);
                return none();

            case ASTKind::If:
            case ASTKind::While:
                state.beginScope();
                discard(state, toDestroy, analyze(state, { ast, 0 }), { ast, 0 });
                discard(state, toDestroy, analyze(state, { ast, 1 }), { ast, 1 });
                state.endScope(ast.scope(), { ast });
                return result;

            case ASTKind::Do:
            case ASTKind::DoScoped:
            case ASTKind::TopLevel:
                if (ast.kind() != ASTKind::Do)
                    state.beginScope();
                for (u32 i : indices(ast)) {
                    if (i == ast.arity() - 1)
                        result = analyze(state, { ast, i });
                    else
                        discard(state, toDestroy, analyze(state, { ast, i }), { ast, i });
                }
                if (ast.kind() != ASTKind::Do)
                    state.endScope(ast.scope(), { ast, ast.arity() - 1 });
                return result;

            default:
                for (u32 i : indices(ast))
                    consume(state, toDestroy, analyze(state, { ast, i }), { ast, i });
                emitAnyDestructors(state, toDestroy, pos);
                return result;
        }
    }

    NOINLINE Artifact* analyze(Artifact* artifact) {
        assert(artifact->kind == ArtifactKind::CheckedAST);

        Module* module = artifact->as<Module>();

        Regions regions;
        auto state = State::initial(&regions, module);
        analyze(state, module->getTopLevel());

        if UNLIKELY(config::printAnalyzedTree)
            println(Multiline(module->getTopLevel()));

        artifact->update(ArtifactKind::AnalyzedAST, module);
        return artifact;
    }
}