#include "clover/analyze.h"
#include "clover/ast.h"
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
            Tuple
        };

        constexpr static RegionIndex ExtParent = 0xffffu;

        union {
            struct { Kind kind : 2; RegionIndex parent : 30; };
            struct { u32 tupleSize; };
            struct { u32 isOwn : 1; RegionIndex fieldId : 31; };
            struct { RegionIndex ref; };
        };
    };

    using RegionKind = RegionWord::Kind;

    struct Region;

    struct Regions {
        vec<RegionWord> words;

        static constexpr RegionIndex Heap = 0, Globals = 1, Stack = 2;

        inline Regions() {
            words.push({ .kind = RegionKind::Singleton, .parent = InvalidRegion }); // Heap
            words.push({ .kind = RegionKind::Singleton, .parent = InvalidRegion }); // Globals
            words.push({ .kind = RegionKind::Singleton, .parent = InvalidRegion }); // Stack
        }

        inline Region def(RegionIndex parent);
        inline Region tuple(RegionIndex parent, u32 size);
    };

    struct PointerField {
        Regions* regions;
        u32 isOwn : 1;
        u32 fieldId : 31;
        u32 ref;

        inline PointerField(Regions* regions_in, u32 isOwn_in, u32 fieldId_in, u32 ref_in):
            regions(regions_in), isOwn(isOwn_in), fieldId(fieldId_in), ref(ref_in) {}

        inline Region referent();
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

        inline void setPtrField(u32 i, bool isOwn, u32 fieldId, RegionIndex ref) {
            nthWord(2 + i * 2).isOwn = isOwn;
            nthWord(2 + i * 2).fieldId = fieldId;
            nthWord(3 + i * 2).ref = ref;
        }
    };

    inline Region PointerField::referent() {
        return Region(regions, ref);
    }

    inline Region Regions::def(RegionIndex parent) {
        words.push({ .kind = RegionKind::Singleton, .parent = parent });
        return Region(this, words.size() - 1);
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
            state.variableRegions.expandTo(module->globals.size(), pair { InvalidRegion, InvalidRegion });
            return state;
        }

        static State initial(Regions* regions, Function* function) {
            State state;
            state.module = function->module;
            state.function = function;
            state.regions = regions;
            state.parent = nullptr;
            state.variableRegions.expandTo(function->locals.size(), pair { InvalidRegion, InvalidRegion });
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
    };

    void analyze(State& state, AST ast) {
        switch (ast.kind()) {
            case ASTKind::VarDecl:
                analyze(state, ast.child(2));
                if (ast.child(1).isVariable())
                    state.def(ast.child(1).variable());
                else
                    unreachable("TODO: Handle patterns.");
                break;
            case ASTKind::IfElse:
                state.beginScope();
                analyze(state, ast.child(0));
                state.beginScope();
                analyze(state, ast.child(1));
                state.endScope(ast.child(1).scope(), { ast, 1 });
                state.beginScope();
                analyze(state, ast.child(2));
                state.endScope(ast.child(2).scope(), { ast, 2 });
                state.endScope(ast.scope(), ast);
                break;
            case ASTKind::If:
            case ASTKind::While:
                state.beginScope();
                analyze(state, ast.child(0));
                analyze(state, ast.child(1));
                state.endScope(ast.scope(), { ast });
                break;
            case ASTKind::DoScoped:
            case ASTKind::TopLevel:
                state.beginScope();
                for (AST child : ast)
                    analyze(state, child);
                state.endScope(ast.scope(), { ast, ast.arity() - 1 });
                break;
            default:
                for (AST child : ast)
                    analyze(state, child);
                break;
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