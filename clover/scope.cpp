#include "clover/scope.h"
#include "clover/ast.h"
#include "clover/compilation.h"
#include "clover/error.h"
#include "clover/value.h"
#include "util/config.h"

namespace clover {
    void Scope::computeInChain() {
        if (hasInChain)
            return;
        if (parent) {
            parent->computeInChain();
            inChain |= parent->inChain;
        }
        inChain |= inTable;
        hasInChain = true;
    }

    void Scope::addToRoot(VariableKind kind, TypeIndex type, Symbol name) {
        assert(entries.find(name) == entries.end());
        assert(!parent);
        auto var = module->addRootGlobal(kind, type, name).index;
        entries.put(name, var);
        inTable.add(name.symbol);
    }

    void Scope::add(VariableKind kind, const AST& decl, Symbol name) {
        assert(kind != VariableKind::Constant && kind != VariableKind::Function && kind != VariableKind::ConstFunction);
        if (entries.find(name) != entries.end()) {
            auto prev = entries.find(name)->value;
            const auto& varInfo = function ? function->locals[prev] : module->globals[prev];
            error(decl.module, decl.pos(), "Duplicate definition of symbol '", decl.module->str(name), "'.")
                .note(module->node(varInfo.decl).pos(), "Previous definition was here.");
            return;
        }
        u32 var;
        if (function)
            var = function->addLocal(kind, decl, name).index;
        else
            var = module->addGlobal(kind, decl, name).index;
        entries.put(name, var);
        inTable.add(name.symbol);
    }

    void Scope::add(VariableKind kind, const AST& decl, TypeIndex type, Symbol name) {
        assert(kind != VariableKind::Constant && kind != VariableKind::Function && kind != VariableKind::ConstFunction);
        if (entries.find(name) != entries.end()) {
            auto prev = entries.find(name)->value;
            const auto& varInfo = function ? function->locals[prev] : module->globals[prev];
            error(decl.module, decl.pos(), "Duplicate definition of symbol '", decl.module->str(name), "'.")
                .note(module->node(varInfo.decl).pos(), "Previous definition was here.");
            return;
        }
        u32 var;
        if (function)
            var = function->addLocal(kind, decl, type, name).index;
        else
            var = module->addGlobal(kind, decl, type, name).index;
        entries.put(name, var);
        inTable.add(name.symbol);
    }

    void Scope::addConstant(VariableKind kind, const AST& decl, Symbol name) {
        if (entries.find(name) != entries.end()) {
            auto prev = entries.find(name)->value;
            const auto& varInfo = function ? function->locals[prev] : module->globals[prev];
            error(decl.module, decl.pos(), "Duplicate definition of symbol '", decl.module->str(name), "'.")
                .note(module->node(varInfo.decl).pos(), "Previous definition was here.");
            return;
        }

        u32 var;
        if (function)
            var = function->addLocalConstant(kind, decl, name).index;
        else
            var = module->addGlobalConstant(kind, decl, name).index;
        entries.put(name, var);
        inTable.add(name.symbol);
    }

    void Scope::addFunction(VariableKind kind, Function* function) {
        Symbol name = function->name;
        if (entries.find(name) != entries.end()) {
            auto prev = entries.find(name)->value;
            const auto& varInfo = function ? function->locals[prev] : module->globals[prev];
            error(module, Pos(), "Duplicate definition of symbol '", module->str(name), "'.");
            return;
        }
        u32 var;
        if (this->function)
            var = this->function->addLocalFunction(kind, function).index;
        else
            var = module->addGlobalFunction(kind, function).index;
        entries.put(name, var);
        inTable.add(name.symbol);
    }

    void Scope::addImport(VariableKind kind, TypeIndex type, Symbol name) {
        if (entries.find(name) != entries.end()) {
            auto prev = entries.find(name)->value;
            const auto& varInfo = function ? function->locals[prev] : module->globals[prev];
            error(module, Pos(), "Duplicate definition of symbol '", module->str(name), "'.");
            return;
        }
        u32 var;
        if (function)
            var = function->addLocalImport(kind, type, name).index;
        else
            var = module->addGlobalImport(kind, type, name).index;
        entries.put(name, var);
        inTable.add(name.symbol);
    }

    void Scope::addOverloadedFunction(Overloads* overloads, Symbol name) {
        u32 var;
        if (function)
            var = function->addLocalOverload(module->overloadIndex(overloads), name).index;
        else
            var = module->addGlobalOverload(module->overloadIndex(overloads), name).index;
        entries.put(name, var);
        inTable.add(name.symbol);
    }

    void Scope::addConstantIndirect(Module* module, const AST& ast, Scope* defScope, u32 constantIndex, Symbol name) {
        if (entries.find(name) != entries.end()) {
            auto prev = entries.find(name)->value;
            const auto& varInfo = function ? function->locals[prev] : module->globals[prev];
            error(module, ast.pos(), "Duplicate definition of symbol '", module->str(name), "'.")
                .note(module->node(varInfo.decl).pos(), "Previous definition was here.");
            return;
        }

        u32 var;
        if (function)
            var = function->addConstantIndirect(defScope->index, constantIndex).index;
        else
            var = module->addConstantIndirect(defScope->index, constantIndex).index;
        entries.put(name, var);
        inTable.add(name.symbol);
    }

    void Scope::addIndirect(Module* module, const AST& ast, Scope* defScope, u32 index, Symbol name) {
        if (entries.find(name) != entries.end()) {
            auto prev = entries.find(name)->value;
            const auto& varInfo = function ? function->locals[prev] : module->globals[prev];
            error(module, ast.pos(), "Duplicate definition of symbol '", module->str(name), "'.")
                .note(module->node(varInfo.decl).pos(), "Previous definition was here.");
            return;
        }

        u32 var;
        if (function)
            var = function->addLocalIndirect(defScope->index, index).index;
        else
            var = module->addGlobalIndirect(defScope->index, index).index;
        entries.put(name, var);
        inTable.add(name.symbol);
    }

    struct Imports {
        vec<pair<NodeIndex, ScopeIndex>> uses;

        void add(AST use) {
            uses.push({ use.node, use.scope()->index });
        }
    };

    void defineFunctionOrOverloads(Scope* scope, Symbol name, Pos pos, void* ptr, bool isOverloads) {
        auto existing = scope->find(name);
        auto module = scope->module;
        if (existing) {
            VariableInfo* info;
            if (existing.isGlobal())
                info = &existing.scope->module->globals[existing.global().index];
            else
                info = &existing.scope->function->locals[existing.local().index];
            Overloads* overloads = nullptr;
            if (info->kind == VariableKind::Function) {
                if (existing.scope == scope) {
                    info->kind = VariableKind::OverloadedFunction;
                    Overloads* inPlaceOverloads = module->addOverloads(module->functions[info->functionIndex]);
                    if (isOverloads)
                        inPlaceOverloads->add((Overloads*)ptr);
                    else
                        inPlaceOverloads->add((Function*)ptr);
                    info->overloads = inPlaceOverloads->index;
                } else
                    overloads = module->addOverloads(module->functions[info->functionIndex]);
            } else if (info->kind == VariableKind::OverloadedFunction) {
                if (existing.scope == scope) {
                    if (isOverloads)
                        module->overloads[info->overloads]->add((Overloads*)ptr);
                    else
                        module->overloads[info->overloads]->add((Function*)ptr);
                } else
                    overloads = module->addOverloads(module->overloads[info->overloads]);
            } else
                error(scope->module, pos, "Tried to define function '", module->str(name), "' but it was already declared as a ", VariableInfo::KindNamesLower[info->kind], ".");
            if (overloads) {
                if (isOverloads)
                    overloads->add((Overloads*)ptr);
                else
                    overloads->add((Function*)ptr);
                scope->addOverloadedFunction(overloads, name);
            }
        } else {
            if (isOverloads)
                scope->addOverloadedFunction((Overloads*)ptr, name);
            else
                scope->addFunction(VariableKind::Function, (Function*)ptr);
        }
    }

    void defineFunction(Scope* scope, Symbol name, Pos pos, Function* function) {
        defineFunctionOrOverloads(scope, name, pos, function, false);
    }

    void defineOverloads(Scope* scope, Symbol name, Pos pos, Overloads* overloads) {
        defineFunctionOrOverloads(scope, name, pos, overloads, true);
    }

    void setScopes(Module* module, Scope* scope, AST ast) {
        if (ast.isLeaf())
            return;
        ast.setScope(scope);
        for (AST child : ast)
            setScopes(module, scope, child);
    }

    void computeScopes(Module* module, Imports& imports, Scope* currentScope, AST ast) {
        switch (ast.kind()) {
            case ASTKind::Local:
            case ASTKind::Const:
            case ASTKind::Typename:
            case ASTKind::GenericTypename:
            case ASTKind::Capture:
            case ASTKind::Global:
            case ASTKind::GlobalConst:
            case ASTKind::GlobalTypename:
            case ASTKind::Exp:
            case ASTKind::ArrayType:
            case ASTKind::FunType:
                // We're revisiting an already-resolved node.
                break;
            case ASTKind::Ident:
            case ASTKind::Int:
            case ASTKind::Unsigned:
            case ASTKind::Float:
            case ASTKind::Bool:
            case ASTKind::Char:
            case ASTKind::String:
            case ASTKind::Missing:
                break;
            case ASTKind::Stars:
                // We have just two expressions as children, which shouldn't
                // entail any definitions. We can scope the first one, but
                // not the second, because what we think is a call might be
                // a function declaration, a la i32* f(i32 x). We don't want
                // to erroneously define x in the current scope. This means we
                // have to fix up these scopes when Stars is eliminated in the
                // subsequent pass!
                ast.setScope(currentScope);
                computeScopes(module, imports, currentScope, ast.child(0));
                break;
            case ASTKind::Plus:
            case ASTKind::Minus:
            case ASTKind::Not:
            case ASTKind::BitNot:
            case ASTKind::SliceType:
            case ASTKind::PtrType:
            case ASTKind::Paren:
            case ASTKind::Length:
            case ASTKind::Return:
            case ASTKind::Deref:
            case ASTKind::AddressOf:
            case ASTKind::PreIncr:
            case ASTKind::PostIncr:
            case ASTKind::PreDecr:
            case ASTKind::PostDecr:
            case ASTKind::New:
            case ASTKind::NewArray:
            case ASTKind::OwnType:
            case ASTKind::UninitType:
            case ASTKind::Splat:
                ast.setScope(currentScope);
                computeScopes(module, currentScope, ast.child(0));
                break;
            case ASTKind::Add:
            case ASTKind::Sub:
            case ASTKind::Mul: // Can arise due to certain syntaxes.
            case ASTKind::Div:
            case ASTKind::Rem:
            case ASTKind::BitAnd:
            case ASTKind::BitOr:
            case ASTKind::BitXor:
            case ASTKind::BitShl:
            case ASTKind::BitShr:
            case ASTKind::BitRol:
            case ASTKind::BitRor:
            case ASTKind::Assign:
            case ASTKind::AddEq:
            case ASTKind::SubEq:
            case ASTKind::MulEq:
            case ASTKind::DivEq:
            case ASTKind::RemEq:
            case ASTKind::ExpEq:
            case ASTKind::BitAndEq:
            case ASTKind::BitOrEq:
            case ASTKind::BitXorEq:
            case ASTKind::BitShlEq:
            case ASTKind::BitShrEq:
            case ASTKind::BitRolEq:
            case ASTKind::BitRorEq:
            case ASTKind::Less:
            case ASTKind::LessEq:
            case ASTKind::Greater:
            case ASTKind::GreaterEq:
            case ASTKind::Equal:
            case ASTKind::NotEqual:
            case ASTKind::And:
            case ASTKind::Or:
            case ASTKind::In:
            case ASTKind::GetField:
            case ASTKind::GetIndex:
            case ASTKind::NamedParameter:
                ast.setScope(currentScope);
                computeScopes(module, imports, currentScope, ast.child(0));
                computeScopes(module, imports, currentScope, ast.child(1));
                break;
            case ASTKind::GetSlice:
                ast.setScope(currentScope);
                computeScopes(module, imports, currentScope, ast.child(0));
                computeScopes(module, imports, currentScope, ast.child(1));
                computeScopes(module, imports, currentScope, ast.child(2));
                break;
            case ASTKind::List:
            case ASTKind::Tuple:
            case ASTKind::GetFields:
            case ASTKind::GetIndices:
                ast.setScope(currentScope);
                for (AST child : ast)
                    computeScopes(module, imports, currentScope, child);
                break;
            case ASTKind::Call:
            case ASTKind::CallMethod:
            case ASTKind::Construct: {
                ast.setScope(currentScope);
                for (AST child : ast)
                    computeScopes(module, imports, currentScope, child);
                break;
            }
            case ASTKind::UseType:
            case ASTKind::UseModule:
                ast.setScope(currentScope);
                imports.add(ast);
                break;
            case ASTKind::Do: {
                ast.setScope(currentScope);
                for (AST child : ast)
                    computeScopes(module, imports, currentScope, child);
                break;
            }
            case ASTKind::DoScoped: {
                Scope* newScope = module->addScope(ScopeKind::Block, ast.node, currentScope);
                ast.setScope(newScope);
                ast.setKind(ASTKind::Do);
                for (AST child : ast)
                    computeScopes(module, imports, newScope, child);
                break;
            }
            case ASTKind::If: {
                Scope* newScope = module->addScope(ScopeKind::Block, ast.node, currentScope);
                ast.setScope(newScope);
                computeScopes(module, imports, newScope, ast.child(0));
                computeScopes(module, imports, newScope, ast.child(1));
                break;
            }
            case ASTKind::IfElse:
            case ASTKind::Ternary: {
                Scope* newScope = module->addScope(ScopeKind::Block, ast.node, currentScope);
                Scope* ifTrueScope = module->addScope(ScopeKind::Block, ast.node, newScope);
                Scope* ifFalseScope = module->addScope(ScopeKind::Block, ast.node, newScope);
                ast.setScope(newScope);
                computeScopes(module, imports, newScope, ast.child(0));
                computeScopes(module, imports, ifTrueScope, ast.child(1));
                computeScopes(module, imports, ifFalseScope, ast.child(2));
                break;
            }
            case ASTKind::While: {
                Scope* newScope = module->addScope(ScopeKind::Block, ast.node, currentScope);
                ast.setScope(newScope);
                computeScopes(module, imports, newScope, ast.child(0));
                computeScopes(module, imports, newScope, ast.child(1));
                break;
            }
            case ASTKind::Then:
                ast.setScope(currentScope);
                computeScopes(module, imports, currentScope, ast.child(0));
                computeScopes(module, imports, currentScope, ast.child(1));
                break;
            case ASTKind::Break:
            case ASTKind::Continue:
                ast.setScope(currentScope);
                break;
            case ASTKind::Raise:
                ast.setScope(currentScope);
                computeScopes(module, imports, currentScope, ast.child(0));
                break;
            case ASTKind::On:
                ast.setScope(currentScope);
                computeScopes(module, imports, currentScope, ast.child(0));
                computeScopes(module, imports, currentScope, ast.child(1));
                break;
            case ASTKind::Match: {
                // TODO: This might be superfluous, since the cases have their
                // own scopes.
                Scope* newScope = module->addScope(ScopeKind::Block, ast.node, currentScope);
                ast.setScope(newScope);
                computeScopes(module, imports, newScope, ast.child(0));
                for (u32 i = 1; i < ast.arity(); i ++)
                    computeScopes(module, imports, newScope, ast.child(i));
                break;
            }
            case ASTKind::Case: {
                Scope* newScope = module->addScope(ScopeKind::Block, ast.node, currentScope);
                ast.setScope(newScope);
                computeScopes(module, imports, newScope, ast.child(0));
                computeScopes(module, imports, newScope, ast.child(1));
                break;
            }
            case ASTKind::Is: {
                ast.setScope(currentScope);
                computeScopes(module, imports, currentScope, ast.child(0));
                computeScopes(module, imports, currentScope, ast.child(1));
                break;
            }
            case ASTKind::ConstVarDecl: {
                ast.setScope(currentScope);
                AST name = ast.child(0);
                assert(name.kind() == ASTKind::Ident);
                currentScope->addConstant(VariableKind::Constant, ast, name.symbol());
                computeScopes(module, imports, currentScope, ast.child(1));
                (currentScope->function ? currentScope->function->constDeclOrder : module->constDeclOrder).push(ast.node);
                break;
            }
            case ASTKind::ConstFunDecl: {
                AST name = ast.child(0);
                assert(name.kind() == ASTKind::Ident);
                Function* function = module->addFunction(ast, currentScope->function);
                Scope* newScope = module->addScope(ScopeKind::Function, ast.node, currentScope, function);
                ast.setScope(newScope);

                defineFunction(currentScope, name.symbol(), ast.pos(), function);

                // No need to skip the parameters like for a normal function -
                // const function parameters are syntactically required to be
                // simpler.
                computeScopes(module, imports, newScope, ast.child(1));

                // Likewise, we can go ahead and compute scopes for the body
                // here too.
                computeScopes(module, imports, newScope, ast.child(2));
                break;
            }
            case ASTKind::VarDecl: {
                ast.setScope(currentScope);
                AST pattern = ast.child(1);
                if (pattern.kind() == ASTKind::Ident) {
                    VariableKind kind;
                    if (currentScope->kind == Scope::Kind::Type)
                        currentScope->add(VariableKind::Member, ast, pattern.symbol());
                    else
                        currentScope->add(ast.kind() == ASTKind::VarDecl ? VariableKind::Variable : VariableKind::Constant, ast, pattern.symbol());
                } else {
                    // This must be some kind of pattern...
                    computeScopes(module, imports, currentScope, ast.child(1));
                }
                computeScopes(module, imports, currentScope, ast.child(0));
                if (ast.child(2).kind() != ASTKind::Uninit)
                    computeScopes(module, imports, currentScope, ast.child(2));
                break;
            }
            case ASTKind::FunDecl: {
                AST name = ast.child(1);
                assert(name.kind() == ASTKind::Ident);
                computeScopes(module, imports, currentScope, ast.child(3)); // Raises decl (should be independent of function, and can't reference it)
                Function* function = module->addFunction(ast, currentScope->function);
                Scope* newScope = module->addScope(ScopeKind::Function, ast.node, currentScope, function);

                defineFunction(currentScope, name.symbol(), ast.pos(), function);

                ast.setScope(newScope);
                computeScopes(module, imports, newScope, ast.child(0)); // Return type
                for (AST arg : ast.child(2)) {
                    if (arg.kind() == ASTKind::VarDecl && arg.child(0).missing() && arg.child(2).missing()) {
                        arg.setScope(newScope);
                        continue; // Skip these until name resolution.
                    }
                    computeScopes(module, imports, newScope, arg);
                }
                ast.child(2).setScope(newScope);

                // We *don't* compute scopes for the body here - this is
                // deferred to either name resolution (when we know the
                // function isn't generic) or generic instantiation time.
                break;
            }
            case ASTKind::AliasDecl: {
                ast.setScope(currentScope);
                assert(ast.child(0).kind() == ASTKind::Ident);
                currentScope->add(VariableKind::Type, ast, ast.child(0).symbol()); // Type name
                computeScopes(module, imports, currentScope, ast.child(2));
                break;
            }
            case ASTKind::NamedDecl:
            case ASTKind::NamedCaseDecl: {
                assert(ast.child(0).kind() == ASTKind::Ident);
                currentScope->add(VariableKind::Type, ast, ast.child(0).symbol()); // Type name
                Scope* newScope = module->addScope(ScopeKind::Type, ast.node, currentScope);
                ast.setScope(newScope);
                computeScopes(module, imports, newScope, ast.child(2));
                break;
            }
            case ASTKind::StructDecl:
            case ASTKind::StructCaseDecl: {
                assert(ast.child(0).kind() == ASTKind::Ident);

                if (!ast.child(1).missing()) {
                    // Generic struct with a parameter list.
                    currentScope->add(VariableKind::GenericType, ast, ast.child(0).symbol()); // Type name

                    // We don't set up a new scope or investigate the body,
                    // instead we wait until the type is instantiated.
                    ast.setScope(currentScope);

                    // We do create a GenericType in the module to keep track
                    // of this template for future reference.
                    unreachable("TODO: Implement generic types.");
                    // GenericType* genericType =
                }

                currentScope->add(VariableKind::Type, ast, ast.child(0).symbol()); // Type name
                Scope* newScope = module->addScope(ScopeKind::Type, ast.node, currentScope);
                ast.setScope(newScope);
                for (u32 i = 2; i < ast.arity(); i ++)
                    computeScopes(module, imports, newScope, ast.child(i));
                break;
            }
            case ASTKind::UnionDecl:
            case ASTKind::UnionCaseDecl: {
                assert(ast.child(0).kind() == ASTKind::Ident); // TODO: Generic types
                currentScope->add(VariableKind::Type, ast, ast.child(0).symbol()); // Type name
                Scope* newScope = module->addScope(ScopeKind::Type, ast.node, currentScope);
                ast.setScope(newScope);
                for (u32 i = 2; i < ast.arity(); i ++)
                    computeScopes(module, imports, newScope, ast.child(i));
                break;
            }
            case ASTKind::TopLevel: {
                assert(currentScope->kind == ScopeKind::Root);
                Scope* topLevel = module->addScope(ScopeKind::TopLevel, module->getTopLevel().node, currentScope);
                ast.setScope(topLevel);
                for (u32 i = 0; i < ast.arity(); i ++)
                    computeScopes(module, imports, topLevel, ast.child(i));
                break;
            }
            default:
                unreachable("TODO: Implement compute scopes for ", ast);
        }
    }

    Scope* getRootScope(Module* module) {
        if (module->compilation->rootScope)
            return module->compilation->rootScope;
        Scope* root = new Scope(ScopeKind::Root, module->compilation->rootModule, nullptr, InvalidNode, InvalidScope);
        root->addToRoot(VariableKind::Type, I8, BuiltinI8);
        root->addToRoot(VariableKind::Type, I16, BuiltinI16);
        root->addToRoot(VariableKind::Type, I32, BuiltinI32);
        root->addToRoot(VariableKind::Type, I64, BuiltinI64);
        root->addToRoot(VariableKind::Type, U8, BuiltinU8);
        root->addToRoot(VariableKind::Type, U16, BuiltinU16);
        root->addToRoot(VariableKind::Type, U32, BuiltinU32);
        root->addToRoot(VariableKind::Type, U64, BuiltinU64);
        root->addToRoot(VariableKind::Type, F32, BuiltinF32);
        root->addToRoot(VariableKind::Type, F64, BuiltinF64);
        root->addToRoot(VariableKind::Type, Void, BuiltinVoid);
        root->addToRoot(VariableKind::Type, Bool, BuiltinBool);
        root->addToRoot(VariableKind::Type, Char, BuiltinChar);
        module->compilation->rootScope = root;
        return root;
    }

    void processImports(Module* module, const Imports& imports) {
        for (const auto [n, s] : imports.uses) {
            AST ast = module->node(n);
            Scope* scope = module->scopes[s];

            Symbol alias = InvalidSymbol;
            AST spec = ast.child(0);
            if (spec.kind() == ASTKind::As) {
                alias = spec.child(1).symbol();
                spec = spec.child(0);
            }

            vec<Symbol> path;
            while (spec.kind() == ASTKind::GetField) {
                if (spec.child(1).kind() == ASTKind::Wildcard) {
                    assert(path.size() == 0);
                    path.push(InvalidSymbol);
                } else
                    path.push(spec.child(1).symbol());
                spec = spec.child(0);
            }
            assert(spec.kind() == ASTKind::Ident);
            path.push(spec.symbol());
            for (u32 i = 0; i < path.size() / 2; i ++)
                swap(path[i], path[path.size() - i - 1]);

            if (ast.kind() == ASTKind::UseType) {
                Scope* defScope = scope;
                for (u32 i = 0; i < path.size() - 1; i ++) {
                    auto entry = defScope->find(path[i]);
                    if (!entry) {
                        error(module, ast.pos(), "Couldn't find symbol ", module->str(path[i]), " within type ", module->str(path[i]), ".");
                        return;
                    }
                    if (entry.isGlobal()) {
                        auto global = module->globals[entry.index];
                        defScope = module->node(global.decl).scope();
                    } else {
                        auto local = defScope->function->locals[entry.index];
                        defScope = module->node(local.decl).scope();
                    }
                }
                if (path.back() == InvalidSymbol) {
                    for (const auto [k, v] : defScope->entries) {
                        VariableInfo info = defScope->function ? defScope->function->locals[v] : module->globals[v];
                        if (info.kind == VariableKind::Constant)
                            scope->addConstantIndirect(module, ast, defScope, info.constantIndex, k);
                        else
                            scope->addIndirect(module, ast, defScope, v, k);
                    }
                } else {
                    auto entry = defScope->findLocal(path.back());
                    if (!entry) {
                        error(module, ast.pos(), "Couldn't find symbol ", module->str(path.back()), " within type ", module->str(path[path.size() - 2]), ".");
                        return;
                    }
                    VariableInfo info = defScope->function ? defScope->function->locals[entry.index] : module->globals[entry.index];
                    if (info.kind == VariableKind::Constant)
                        scope->addConstantIndirect(module, ast, defScope, info.constantIndex, path.back());
                    else
                        scope->addIndirect(module, ast, defScope, entry.index, path.back());
                }
            } else {
                Path filepath(module->compilation->cwd);
                for (u32 i = 0; i < path.size(); i ++) {
                    if (i == path.size() - 1) {
                        auto withext = tostring(module->str(path[i]), ".cl");
                        filepath.append(withext);
                        delete[] withext.data();
                    } else
                        filepath.append(module->str(path[i]));
                }
                Artifact* artifact = addSourceFile(module->compilation, filepath);
                artifact = compileUntil(module->compilation, ArtifactKind::CheckedAST, artifact);
                Module* otherModule = artifact->as<Module>();
                module->artifact->imports.insert(artifact);

                Scope* topLevelScope = otherModule->getTopLevel().scope();
                for (const auto& [k, v] : topLevelScope->entries) {
                    VariableInfo info = otherModule->globals[v];
                    if (info.kind == VariableKind::Function) {
                        Function* function;
                        function = otherModule->functions[info.functionIndex];
                        defineFunction(scope, k, ast.pos(), function);
                    } else if (info.kind == VariableKind::OverloadedFunction) {
                        Overloads* overloads = otherModule->overloads[info.overloads];
                        defineOverloads(scope, k, ast.pos(), overloads);
                    } else if (scope->findLocal(k)) {
                        error(module, ast.pos(), "Duplicate symbol definition ", module->str(k), " from module ", module->str(path.back()), ".");
                        break;
                    } else
                        scope->addImport(info.kind, expand(module->types->get(info.type)).index, k);
                }
            }
        }
    }

    void computeScopes(Module* module, Scope* currentScope, AST ast) {
        Imports imports;
        computeScopes(module, imports, currentScope, ast);
        processImports(module, imports);
        if UNLIKELY(config::printScopeTree)
            module->printScopes(module->compilation);
    }

    NOINLINE Artifact* computeScopes(Artifact* artifact) {
        assert(artifact->kind == ArtifactKind::ParsedAST);

        Module* module = artifact->as<Module>();
        module->nodeScopes.expandTo(module->ast.size(), InvalidScope);
        Scope* root = getRootScope(module);
        Imports imports;
        computeScopes(module, imports, root, module->getTopLevel());
        processImports(module, imports);
        for (Scope* scope : module->scopes)
            scope->computeInChain();
        artifact->update(ArtifactKind::ScopedAST, module);
        if UNLIKELY(config::printScopeTree)
            module->printScopes(module->compilation);
        return artifact;
    }
}