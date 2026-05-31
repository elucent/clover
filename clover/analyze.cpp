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
            struct { u32 isOwn : 1; u32 isTuple : 1; RegionIndex ref : 30; };
        };
    };

    using RegionKind = RegionWord::Kind;

    struct Region;

    struct Regions {
        vec<RegionWord> words;
        biasedset<256> ownPtrTypes, ptrTypes, ptrTypesInited;

        static constexpr RegionIndex Heap = 0, Globals = 1, Stack = 2, Atoms = 3, MaxBuiltinRegion = Atoms;

        inline Regions() {
            words.push({ .kind = RegionKind::Singleton, .parent = InvalidRegion }); // Heap
            words.push({ .kind = RegionKind::Singleton, .parent = InvalidRegion }); // Globals
            words.push({ .kind = RegionKind::Singleton, .parent = InvalidRegion }); // Stack
            words.push({ .kind = RegionKind::Singleton, .parent = InvalidRegion }); // Atoms
        }

        inline pair<bool, bool> populatePointerTable(Type type) {
            bool hasPtrs = false, hasOwnPtrs = false;
            switch (type.kind()) {
                case TypeKind::Pointer:
                    hasPtrs = true;
                    hasOwnPtrs = type.asPtr().isOwn();
                    break;
                case TypeKind::Slice:
                    hasPtrs = true;
                    hasOwnPtrs = type.asSlice().isOwn();
                    break;
                case TypeKind::Named:
                    hasPtrs = hasPointers(expand(type.asNamed().innerType()));
                    hasOwnPtrs = hasOwnPointers(expand(type.asNamed().innerType()));
                    break;
                case TypeKind::Struct:
                    for (u32 i = 0; i < type.asStruct().count(); i ++) {
                        auto fieldType = expand(type.asStruct().fieldType(i));
                        if (hasPointers(fieldType))
                            hasPtrs = true;
                        if (hasOwnPointers(fieldType))
                            hasOwnPtrs = true;
                        if (hasPtrs && hasOwnPtrs)
                            break;
                    }
                    break;
                case TypeKind::Tuple:
                    for (u32 i = 0; i < type.asTuple().count(); i ++) {
                        auto fieldType = expand(type.asTuple().fieldType(i));
                        if (hasPointers(fieldType))
                            hasPtrs = true;
                        if (hasOwnPointers(fieldType))
                            hasOwnPtrs = true;
                        if (hasPtrs && hasOwnPtrs)
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
            if (hasOwnPtrs)
                ownPtrTypes.on(type.index);
            return { hasPtrs, hasOwnPtrs };
        }

        inline bool hasPointers(Type type) {
            if (ptrTypesInited[type.index])
                return ptrTypes[type.index];

            auto info = populatePointerTable(type);
            return info.first;
        }

        inline bool hasOwnPointers(Type type) {
            if (ptrTypesInited[type.index])
                return ownPtrTypes[type.index];

            auto info = populatePointerTable(type);
            return info.second;
        }

        inline Region get(RegionIndex index);
        inline Region def(RegionIndex parent);
        inline Region ref(RegionIndex parent, RegionIndex target, bool isOwn);
        inline Region tuple(RegionIndex parent, u32 size);
        inline Region instance(RegionIndex parent, Type type);
    };

    struct RegionField {
        Regions* regions;
        RegionIndex parent;
        u32 fieldId;
        u32 own : 1, tup : 1;
        u32 ref : 31;

        inline RegionField(Regions* regions_in, RegionIndex parent_in, u32 isOwn, u32 isTuple, u32 fieldId_in, u32 ref_in):
            regions(regions_in), parent(parent_in), fieldId(fieldId_in), own(isOwn), tup(isTuple), ref(ref_in) {}

        inline RegionField(): regions(nullptr), parent(InvalidRegion), fieldId(0), own(false), tup(false), ref(InvalidRegion) {}

        inline bool isOwn() const {
            return own;
        }

        inline bool isTuple() const {
            return tup;
        }

        inline pair<RegionIndex, RegionIndex> region() const;

        inline explicit operator bool() const {
            return ref != InvalidRegion;
        }
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

        inline Region parent() const {
            return Region(regions, nthWord(0).parent);
        }

        inline bool isRef() const {
            return nthWord(0).kind == RegionKind::Ref;
        }

        inline bool isTuple() const {
            return nthWord(0).kind == RegionKind::Tuple;
        }

        inline u32 size() const {
            assert(isTuple());
            return nthWord(1).tupleSize;
        }

        inline RegionField field(u32 i) const {
            assert(isTuple());
            return { regions, index, nthWord(2 + i * 2).isOwn, nthWord(2 + i * 2).isTuple, nthWord(2 + i * 2).fieldId, nthWord(3 + i * 2).ref };
        }

        inline i32 byFieldId(u32 id) const {
            for (u32 i = 0; i < size(); i ++)
                if (field(i).fieldId == id)
                    return i;
            return -1;
        }

        inline Region with(u32 index, bool isOwn, bool isTuple, u32 fieldId, RegionIndex ref) const {
            auto newTuple = regions->tuple(parent().index, size());
            for (u32 i = 0; i < size(); i ++) {
                if (i == index)
                    newTuple.setField(i, isOwn, isTuple, fieldId, ref);
                else
                    newTuple.setField(i, field(i));
            }
            return newTuple;
        }

        inline Region deref() const {
            assert(isRef());
            return { regions, nthWord(1).ref };
        }

        inline bool isOwn() const {
            assert(isRef());
            return nthWord(1).isOwn;
        }

        inline void setField(u32 i, RegionField field) {
            nthWord(2 + i * 2).fieldId = field.fieldId;
            nthWord(3 + i * 2).isOwn = field.own;
            nthWord(3 + i * 2).isTuple = field.tup;
            nthWord(3 + i * 2).ref = field.ref;
        }

        inline void setField(u32 i, bool isOwn, bool isTuple, u32 fieldId, RegionIndex ref) {
            nthWord(2 + i * 2).fieldId = fieldId;
            nthWord(3 + i * 2).isOwn = isOwn;
            nthWord(3 + i * 2).isTuple = isTuple;
            nthWord(3 + i * 2).ref = ref;
        }
    };

    inline pair<RegionIndex, RegionIndex> RegionField::region() const {
        if (tup)
            return { ref, InvalidRegion };
        else
            return { parent, ref };
    }

    inline Region Regions::def(RegionIndex parent) {
        words.push({ .kind = RegionKind::Singleton, .parent = parent });
        return Region(this, words.size() - 1);
    }

    inline Region Regions::ref(RegionIndex parent, RegionIndex target, bool isOwn) {
        words.push({ .kind = RegionKind::Ref, .parent = parent });
        words.push({ .isOwn = isOwn, .ref = target });
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
            if (isSomePointer(member))
                tup.setField(ptrFieldCount ++, isOwn(member), false, i, InvalidRegion);
            else if (regions->hasPointers(member))
                tup.setField(ptrFieldCount ++, isOwn(member), !isSomePointer(member), i, regions->instance(tup.index, member).index);
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
                return ref(parent, InvalidRegion, type.asPtr().isOwn());

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
                return ref(parent, InvalidRegion, type.asSlice().isOwn());
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

        inline RegionValue(ChangePosition pos, Region region):
            owner(pos.ast.node), childOrVar(pos.child) {
            selfIndex = region.index;
            refIndex = region.isRef() ? region.deref().index : InvalidRegion;
        }

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

        inline IndexedAST ensureLval(State& state) {
            i32 var = ensureVar(state);
            if (state.function)
                return state.module->add(ASTKind::Local, Local(var)).positionless();
            return state.module->add(ASTKind::Global, Global(var)).positionless();
        }

        inline pair<RegionIndex, RegionIndex> indices(State& state) const {
            if (isVar())
                return state[variable()];
            return { selfIndex, refIndex };
        }

        inline void setIndices(State& state, pair<RegionIndex, RegionIndex> indices) {
            if (isVar())
                state[variable()] = indices;
            else {
                selfIndex = indices.first;
                refIndex = indices.second;
            }
        }

        inline void setRegion(State& state, Region region) {
            if (isVar()) {
                state[variable()].first = region.index;
                state[variable()].second = region.isRef() ? region.deref().index : InvalidRegion;
            } else {
                selfIndex = region.index;
                refIndex = region.isRef() ? region.deref().index : InvalidRegion;
            }
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

    template<typename IO, typename Format = Formatter<IO>>
    inline IO format_impl(IO io, const Region& region) {
        if (region.index <= Regions::MaxBuiltinRegion) switch (region.index) {
            case Regions::Stack:
                return format(io, "Stack");
            case Regions::Globals:
                return format(io, "Globals");
            case Regions::Heap:
                return format(io, "Heap");
            case Regions::Atoms:
                return format(io, "Atoms");
            default:
                unreachable("That should be all the built-in regions.");
        }
        io = format(io, '@', region.index);
        if (region.isRef())
            return format(io, '[', region.isOwn() ? "&own @" : "& @", region.deref().index, ']');
        if (region.isTuple()) {
            io = format(io, '(');
            for (u32 i = 0; i < region.size(); i ++) {
                if (i > 0)
                    io = format(io, ", ");
                auto field = region.field(i);
                if (field.isTuple())
                    io = format(io, region.regions->get(field.ref));
                else
                    io = format(io, field.isOwn() ? "&own @" : "& @", field.ref);
            }
            return format(io, ')');
        }
        return io;
    }

    void printState(State& state) {
        println("[ANA]\tCurrent analysis state is:");
        for (u32 i = 0; i < state.variableRegions.size(); i ++) {
            if (state.variableRegions[i].first == InvalidRegion)
                continue;
            const auto& info = state.function ? state.function->locals[i] : state.module->globals[i];
            print("[ANA]\t - ", state.module->str(info.name), " = ", state.regions->get(state.variableRegions[i].first));
            if (state.variableRegions[i].second != InvalidRegion) {
                RegionValue value(i);
                print("\t with ", isOwn(value.type(state)) ? "&own @" : "& @", value.indices(state).second);
            }
            println();
        }
    }

    bool invalidated(Region region) {
        if (region.isRef())
            return !region.deref();
        if (region.isTuple()) for (u32 i = 0; i < region.size(); i ++)
            if (!region.field(i))
                return true;
        return false;
    }

    bool invalidated(State& state, RegionIndex region) {
        if (region == InvalidRegion)
            return true;
        return invalidated(state.regions->get(region));
    }

    void invalidate(State& state, RegionValue value, ChangePosition pos) {
        if (!value.isVar())
            return;
        i32 var = value.variable();
        Type type = RegionValue(var).type(state);
        if (!state.regions->hasOwnPointers(type))
            return;
        switch (type.kind()) {
            case TypeKind::Pointer:
            case TypeKind::Slice:
                state[var].second = InvalidRegion;
                break;
            case TypeKind::Struct:
            case TypeKind::Tuple: {
                Region region = state.regions->get(state[var].first);
                if (invalidated(region))
                    return;
                for (u32 i = 0; i < region.size(); i ++) {
                    auto prev = region.field(i);
                    prev.ref = InvalidRegion;
                    region.setField(i, prev);
                }
                break;
            }
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

    void destroy(State& state, vec<IndexedAST>& toDestroy, RegionValue value, ChangePosition pos, bool immediately) {
        Type type = value.type(state);
        auto indices = value.indices(state);
        if (!state.regions->hasOwnPointers(type))
            return;
        if (invalidated(state, indices.first) || (isSomePointer(type) && invalidated(state, indices.second)))
            return;
        i32 var = value.ensureVar(state);
        if (immediately) {
            if (state.function)
                pos.replaceWith(state.module->add(ASTKind::DelExpr, pos.origin(), pos.ast.scope(), type, pos.indexedCurrent(), Local(var)));
            else
                pos.replaceWith(state.module->add(ASTKind::DelExpr, pos.origin(), pos.ast.scope(), type, pos.indexedCurrent(), Global(var)));
        } else
            toDestroy.push(value.ensureLval(state));
    }

    void destroyField(State& state, vec<IndexedAST>& toDestroy, RegionValue tuple, u32 i, pair<RegionIndex, RegionIndex> indices, ChangePosition pos, bool immediately) {
        Type type = tuple.type(state);
        type = type.isStruct() ? type.asStruct().fieldType(i) : type.asTuple().fieldType(i);
        if (!state.regions->hasOwnPointers(type))
            return;
        if (invalidated(state, indices.first) || invalidated(state, indices.second))
            return;
        state.module->needsDestructor.on(type.cloneExpand().index);
        i32 var = tuple.ensureVar(state);
        IndexedAST toDelete = state.module->add(ASTKind::GetField, pos.origin(), pos.ast.scope(), type, tuple.ensureLval(state), FieldId(i)).reconstituteOrigin();
        if (immediately) {
            if (state.function)
                pos.replaceWith(state.module->add(ASTKind::DelExpr, pos.origin(), pos.ast.scope(), type, pos.indexedCurrent(), toDelete));
            else
                pos.replaceWith(state.module->add(ASTKind::DelExpr, pos.origin(), pos.ast.scope(), type, pos.indexedCurrent(), toDelete));
        } else
            toDestroy.push(toDelete);
    }

    void consume(State& state, vec<IndexedAST>& toDestroy, RegionValue value, ChangePosition pos) {
        if (!value)
            return;
        if (value.isVar())
            return;
        destroy(state, toDestroy, value, pos, false);
    }

    void use(State& state, vec<IndexedAST>& toDestroy, RegionValue value, ChangePosition pos) {
        checkRead(state, value, pos);
        consume(state, toDestroy, value, pos);
    }

    void move(State& state, vec<IndexedAST>& toDestroy, Type destType, i32 var, RegionValue src, ChangePosition beforePos) {
        assert(!!src);

        Type srcType = src.type(state);

        if (!state.regions->hasPointers(destType) && !state.regions->hasPointers(srcType)) {
            // Trivial copy. We can just return.
            return;
        }

        // We basically need to do a double dispatch now, but of course we
        // prune the possible cases down quite a lot via the type system.
        switch (destType.kind()) {
            case TypeKind::Pointer: {
                assert(srcType.isPtr() || isAtom(srcType));
                if (isAtom(srcType)) {
                    if (destType.asPtr().isOwn())
                        destroy(state, toDestroy, RegionValue(var), beforePos, true);
                    state.variableRegions[var].second = Regions::Atoms;
                } else if (destType.asPtr().isOwn() && srcType.asPtr().isOwn()) {
                    destroy(state, toDestroy, RegionValue(var), beforePos, true);
                    state.variableRegions[var].second = src.indices(state).second;
                    invalidate(state, src, beforePos);
                } else if (srcType.asPtr().isOwn()) {
                    // Downcast. This is basically always wrong...maybe we
                    // should do something more aggressive about this in the
                    // future.
                    consume(state, toDestroy, src, beforePos);
                    state.variableRegions[var].second = InvalidRegion;
                } else
                    state.variableRegions[var].second = src.indices(state).second;
                break;
            }

            case TypeKind::Slice: {
                // Because T[N]* is a subtype of T[], we have to consider if
                // src is a pointer. But this is a trivial difference (is it?)
                // from a borrow checking perspective, so the behavior is
                // essentially the same.

                assert(srcType.isSlice() || srcType.isPtr() && (expand(srcType.asPtr().elementType()).isArray()));
                bool srcOwn = srcType.isSlice() ? srcType.asSlice().isOwn() : srcType.asPtr().isOwn();
                if (destType.asSlice().isOwn() && srcOwn) {
                    destroy(state, toDestroy, RegionValue(var), beforePos, true);
                    state.variableRegions[var].second = src.indices(state).second;
                    invalidate(state, src, beforePos);
                } else if (srcOwn) {
                    // Downcast. This is basically always wrong...maybe we
                    // should do something more aggressive about this in the
                    // future.
                    consume(state, toDestroy, src, beforePos);
                    state.variableRegions[var].second = InvalidRegion;
                } else
                    state.variableRegions[var].second = src.indices(state).second;
                break;
            }

            case TypeKind::Tuple:
                // TODO: Tuple subtyping rules are probably especially gnarly
                // here.
                unreachable("TODO: Implement tuples or something.");

            case TypeKind::Struct: {
                assert(destType == srcType);
                destroy(state, toDestroy, RegionValue(var), beforePos, true);
                state.variableRegions[var].first = src.indices(state).first;
                invalidate(state, src, beforePos);
                break;
            }

            default:
                unreachable("Unexpected pointer-containing type ", destType);
        }
    }

    void setField(State& state, vec<IndexedAST>& toDestroy, RegionValue dest, u32 field, RegionValue src, ChangePosition beforePos) {
        assert(!!src);

        Type aggregateType = dest.type(state);
        Type destType = expand(aggregateType.isStruct() ? aggregateType.asStruct().fieldType(field) : aggregateType.asTuple().fieldType(field));
        Type srcType = src.type(state);

        if (!state.regions->hasPointers(destType) && !state.regions->hasPointers(srcType)) {
            // Trivial copy. We can just return.
            return;
        }

        Region tuple = state.regions->get(dest.selfIndex);
        assert(tuple.isTuple());

        i32 ptrFieldIndex = tuple.byFieldId(field);
        assert(ptrFieldIndex != -1);
        RegionField tupField = tuple.field(ptrFieldIndex);
        auto fieldIndices = tupField.region();

        // We basically need to do a double dispatch now, but of course we
        // prune the possible cases down quite a lot via the type system.
        switch (destType.kind()) {
            case TypeKind::Pointer: {
                assert(srcType.isPtr() || isAtom(srcType));
                if (isAtom(srcType)) {
                    if (destType.asPtr().isOwn())
                        destroyField(state, toDestroy, dest, field, fieldIndices, beforePos, true);
                    tuple.setField(ptrFieldIndex, true, false, field, Regions::Atoms);
                } else if (destType.asPtr().isOwn() && srcType.asPtr().isOwn()) {
                    destroyField(state, toDestroy, dest, field, fieldIndices, beforePos, true);
                    tuple.setField(ptrFieldIndex, true, false, field, src.refIndex);
                    invalidate(state, src, beforePos);
                } else if (srcType.asPtr().isOwn()) {
                    // Downcast. This is basically always wrong...maybe we
                    // should do something more aggressive about this in the
                    // future.
                    consume(state, toDestroy, src, beforePos);
                    tuple.setField(ptrFieldIndex, false, false, field, InvalidRegion);
                } else
                    tuple.setField(ptrFieldIndex, false, false, field, src.indices(state).second);
                break;
            }

            case TypeKind::Slice: {
                // Because T[N]* is a subtype of T[], we have to consider if
                // src is a pointer. But this is a trivial difference (is it?)
                // from a borrow checking perspective, so the behavior is
                // essentially the same.

                assert(srcType.isSlice() || srcType.isPtr() && (expand(srcType.asPtr().elementType()).isArray()));
                bool srcOwn = srcType.isSlice() ? srcType.asSlice().isOwn() : srcType.asPtr().isOwn();
                if (destType.asSlice().isOwn() && srcOwn) {
                    destroyField(state, toDestroy, dest, field, fieldIndices, beforePos, true);
                    tuple.setField(ptrFieldIndex, true, false, field, src.refIndex);
                    invalidate(state, src, beforePos);
                } else if (srcType.asPtr().isOwn()) {
                    // Downcast. This is basically always wrong...maybe we
                    // should do something more aggressive about this in the
                    // future.
                    consume(state, toDestroy, src, beforePos);
                    tuple.setField(ptrFieldIndex, false, false, field, InvalidRegion);
                } else
                    tuple.setField(ptrFieldIndex, false, false, field, src.indices(state).second);

                break;
            }

            case TypeKind::Tuple:
                // TODO: Tuple subtyping rules are probably especially gnarly
                // here.
                unreachable("TODO: Implement tuples or something.");

            case TypeKind::Struct: {
                assert(destType == srcType);
                destroyField(state, toDestroy, dest, field, fieldIndices, beforePos, true);
                tuple.setField(ptrFieldIndex, false, true, field, src.indices(state).first);
                invalidate(state, src, beforePos);
                break;
            }

            default:
                unreachable("Unexpected pointer-containing type ", destType);
        }
    }

    void emitAnyDestructors(State& state, const_slice<IndexedAST> toDestroy, ChangePosition pos) {
        if (toDestroy.size() == 0)
            return;

        pos.replaceWith(state.module->add(ASTKind::DelExpr, pos.origin(), pos.ast.scope(), typeOf(pos), pos.indexedCurrent(), toDestroy));
    }

    void emitAnyDestructors(State& state, const vec<IndexedAST>& toDestroy, ChangePosition pos) {
        return emitAnyDestructors(state, (const_slice<IndexedAST>)toDestroy, pos);
    }

    void discard(State& state, vec<IndexedAST>& toDestroy, RegionValue value, ChangePosition pos) {
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
        vec<IndexedAST> toDestroy;
        AST ast = pos.current();
        switch (ast.kind()) {
            case ASTKind::Global:
            case ASTKind::Local:
                return RegionValue(ast.variable());

            case ASTKind::Int:
            case ASTKind::Unsigned:
            case ASTKind::Char:
            case ASTKind::Bool:
            case ASTKind::Const:
            case ASTKind::GlobalConst:
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

            case ASTKind::Construct: {
                auto type = expand(ast.type());
                switch (type.kind()) {
                    case TypeKind::Struct: {
                        Region region = state.regions->instance(Regions::Stack, type);
                        for (u32 i = 0; i < type.asStruct().count(); i ++) {
                            auto init = analyze(state, { ast, i });
                            setField(state, toDestroy, RegionValue(pos, region), i, init, { ast, i });
                        }
                        return RegionValue(pos, region);
                    }
                    default:
                        unreachable("TODO: Other types of constructor.");
                }
                break;
            }

            case ASTKind::VarDecl:
                if (!ast.child(1).isVariable())
                    unreachable("TODO: Handle patterns.");
                if (!ast.child(2).missing()) {
                    auto value = analyze(state, { ast, 2 });
                    move(state, toDestroy, typeOf(ast), ast.child(1).variable(), value, { ast, 2 });
                } else
                    state[ast.child(1).variable()] = { InvalidRegion, InvalidRegion };
                emitAnyDestructors(state, toDestroy, pos);
                state.def(ast.child(1).variable());
                return none();

            case ASTKind::Assign: {
                auto value = analyze(state, { ast, 1 });
                move(state, toDestroy, typeOf(ast, 0), ast.child(0).variable(), value, { ast, 1 });
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
                    if (i == ast.arity() - 1 && ast.kind() != ASTKind::TopLevel)
                        result = analyze(state, { ast, i });
                    else
                        discard(state, toDestroy, analyze(state, { ast, i }), { ast, i });
                }
                if (ast.kind() != ASTKind::Do)
                    state.endScope(ast.scope(), { ast, ast.arity() - 1 });
                return result;

            case ASTKind::AliasDecl:
            case ASTKind::NamedDecl:
            case ASTKind::NamedCaseDecl:
            case ASTKind::GenericNamedDecl:
            case ASTKind::StructDecl:
            case ASTKind::StructCaseDecl:
            case ASTKind::GenericStructDecl:
            case ASTKind::UnionDecl:
            case ASTKind::UnionCaseDecl:
            case ASTKind::GenericUnionDecl:
            case ASTKind::ArrayType:
            case ASTKind::SliceType:
            case ASTKind::PtrType:
            case ASTKind::TupleType:
            case ASTKind::FunType:
            case ASTKind::Typename:
            case ASTKind::GlobalTypename:
            case ASTKind::Projection:
            case ASTKind::TypeField: {
                Type type = evaluateType(state.module, state.function, ast);
                if (isAtom(type))
                    return RegionValue(pos, Regions::Atoms);
                return {};
            }

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

        module->needsDestructor |= regions.ownPtrTypes;
        module->needsConstructor |= regions.ownPtrTypes;
        module->constructorDestructorInited |= regions.ptrTypesInited;

        if UNLIKELY(config::printAnalyzedTree)
            println(Multiline(module->getTopLevel()));

        artifact->update(ArtifactKind::AnalyzedAST, module);
        return artifact;
    }
}