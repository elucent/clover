#include "clover/ast.h"
#include "clover/type.h"
#include "util/config.h"

namespace clover {
    #define DEFINE_NAME(upper, ...) #upper,
    const i8* AST_NAMES_UPPER[] = {
        FOR_EACH_AST_KIND(DEFINE_NAME)
    };
    #undef DEFINE_NAME

    #define DEFINE_NAME(upper, lower, ...) #lower,
    const i8* AST_NAMES_LOWER[] = {
        FOR_EACH_AST_KIND(DEFINE_NAME)
    };
    #undef DEFINE_NAME

    #define DEFINE_NAME(upper, lower, code, ...) #code,
    const i8* AST_NAMES_SYMBOLIC[] = {
        FOR_EACH_AST_KIND(DEFINE_NAME)
    };
    #undef DEFINE_NAME

    Type Function::cloneGenericType() {
        assert(isGeneric);
        assert(genericType != InvalidType);
        Type type = module->types->get(genericType);

        type.forEachVar([&](Type varType) {
            if (!varType.asVar().isEqual())
                varType.asVar().makeEqual(module->varType());
        });

        Type clone = type.cloneExpand();

        type.forEachVar([&](Type varType) {
            if (varType.asVar().isEqual()) {
                varType.asVar().setNotEqual();
                varType.asVar().setLowerBound(Bottom);
                varType.asVar().setUpperBound(Any);
            }
        });

        return clone;
    }

    u64 computeSloppyHash(AST ast) {
        auto kh = intHash(ast.kind());
        switch (ast.kind()) {
            case ASTKind::Local:
            case ASTKind::Global:
            case ASTKind::Typename:
            case ASTKind::GlobalTypename:
                return mixHash(kh, intHash(ast.variable()));
            case ASTKind::Const:
            case ASTKind::GlobalConst:
                return mixHash(kh, intHash(ast.constId()));
            case ASTKind::Field:
                return mixHash(kh, intHash(ast.fieldId()));
            case ASTKind::Int:
                return mixHash(kh, intHash(ast.intConst()));
            case ASTKind::Unsigned:
                return mixHash(kh, intHash(ast.uintConst()));
            case ASTKind::Float:
                return mixHash(kh, intHash(bitcast<u64>(ast.floatConst())));
            case ASTKind::String:
                return mixHash(kh, ::hash(ast.module->str(ast.stringConst())));
            case ASTKind::Bool:
                return mixHash(kh, intHash(!!ast.boolConst()));
            case ASTKind::Char:
                return mixHash(kh, intHash(ast.charConst()));
            case ASTKind::Uninit:
            case ASTKind::ResolvedFunction:
            case ASTKind::Missing:
            case ASTKind::AnyType:
            case ASTKind::Wildcard:
                return kh;
            default:
                for (u32 i = 0; i < ast.arity(); i ++)
                    kh = mixHash(kh, computeSloppyHash(ast.child(i)));
                return kh;
        }
    }

    u64 Function::computeHash() {
        if (genericHash)
            return genericHash;
        assert(decl != InvalidNode);
        genericHash = computeSloppyHash(module->node(decl));
        if (!genericHash)
            genericHash = 1;
        return genericHash;
    }

    Module::~Module() {
        if (source.data())
            delete[] source.data();
    }

    void Module::printScopes(Compilation* compilation) {
        vec<vec<Scope*>> childScopes;
        childScopes.expandTo(scopes.size());
        for (Scope* scope : scopes) if (scope->parent && scope->parent->module == this) {
            childScopes[scope->parent->index].push(scope);
        }
        printScope(compilation, getTopLevel().scope(), childScopes, 0);
    }

    void Module::printScope(Compilation* compilation, Scope* scope, const vec<vec<Scope*>>& childScopes, u32 amount) {
        auto indent = [](u32 n) {
            for (u32 i = 0; i < n; i ++)
                ::print(' ');
        };
        indent(amount);
        ::print(AST_NAMES_UPPER[node(scope->owner).kind()]);
        switch (node(scope->owner).kind()) {
            case ASTKind::FunDecl:
            case ASTKind::ConstFunDecl:
                ::print(" ", node(scope->owner).child(1));
                break;
            case ASTKind::StructDecl:
            case ASTKind::UnionDecl:
            case ASTKind::NamedDecl:
                ::print(" ", node(scope->owner).child(0));
                break;
            default:
                break;
        }
        println(" (", scope->entries.size(), " entries)");
        for (const auto& [k, v] : scope->entries) {
            const auto& info = scope->function ? scope->function->locals[v] : globals[v];
            indent(amount);
            ::print("  ", VariableInfo::ScopeNames[(u32)info.scope], " ");
            switch (info.kind) {
                case VariableKind::Type:
                case VariableKind::GenericType:
                    ::print(VariableInfo::KindNamesUpper[(u32)info.kind], " ", compilation->str(info.name));
                    if (info.type != InvalidType)
                        ::print(" = ", compilation->types->get(info.type));
                    println();
                    break;
                case VariableKind::Variable:
                case VariableKind::Function:
                case VariableKind::ConstFunction:
                case VariableKind::GenericFunction:
                    ::print(VariableInfo::KindNamesUpper[(u32)info.kind], " ", compilation->str(info.name));
                    if (info.type != InvalidType)
                        ::print(" : ", compilation->types->get(info.type));
                    println();
                    break;
                case VariableKind::Constant:
                    ::print(VariableInfo::KindNamesUpper[(u32)info.kind], " #", info.constantIndex);
                    if (info.type != InvalidType)
                        ::print(" : ", compilation->types->get(info.type));
                    println();
                    break;
                case VariableKind::OverloadedFunction:
                    ::println(VariableInfo::KindNamesUpper[(u32)info.kind], " ", compilation->str(info.name));
                    break;
                default:
                    break;
            }
        }
        for (Scope* child : childScopes[scope->index])
            printScope(compilation, child, childScopes, amount + 2);
    }

    void Module::print(Compilation* compilation) {
        ::print(Multiline(getTopLevel()));
        if (nodeTypes.size()) {
            ::println();
            ::println("where:");
            printTypeVariableState(compilation->types);
        }
    }

    u64 Module::reportSize() {
        u64 size = 0;
        size += sizeof(ASTWord) * astWords.size();
        size += sizeof(NodeIndex) * ast.size();
        size += sizeof(Pos) * nodePositions.size();
        size += sizeof(ScopeIndex) * nodeScopes.size();
        size += sizeof(TypeIndex) * nodeTypes.size();
        size += sizeof(Scope*) * scopes.size();
        size += source.size();
        return size;
    }

    void validateAggressively(Module* module, Function* function, AST ast) {
        assert(ast.kind() < ASTKind::NumKinds);

        switch (ast.kind()) {
            #define DEFINE_CASE(upper, lower, sym, _isLeaf, _arity) \
                case ASTKind:: upper : \
                    if (_isLeaf) \
                        assert(ast.isLeaf()); \
                    else { \
                        assert(!ast.isLeaf()); \
                        assert(_arity == -1 || ast.arity() == _arity); \
                    } \
                    break;
            FOR_EACH_AST_KIND(DEFINE_CASE)
            #undef DEFINE_CASE
            default:
                unreachable("Found invalid AST kind");
        }

        if (!ast.isLeaf()) for (u32 i = 0; i < ast.arity(); i ++)
            validateAggressively(module, function, ast.child(i));
    }

    void Module::setTempTopLevel(vec<AST, 32>& top) {
        tempTopLevel = &top;
    }

    void Module::validateAggressively(AST ast) {
        clover::validateAggressively(this, nullptr, ast);
    }

    void Module::validateAggressively() {
        if (tempTopLevel)
            for (AST top : *tempTopLevel)
                clover::validateAggressively(this, nullptr, top);
        else
            clover::validateAggressively(this, nullptr, getTopLevel());
    }
}