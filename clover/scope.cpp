#include "clover/scope.h"
#include "clover/ast.h"
#include "clover/compilation.h"
#include "clover/error.h"
#include "clover/value.h"
#include "util/config.h"

namespace clover {
    Scope::Scope(Kind kind_in, Module* module_in, Function* function_in, Scope* parent_in, NodeIndex owner_in, ScopeIndex index_in):
        module(module_in), parent(parent_in), function(function_in), owner(owner_in), index(index_in), kind(kind_in) {
        globalIndex = module->compilation->numScopes ++;
    }

    Scope::Scope(Kind kind_in, Module* module_in, Scope* parent_in, NodeIndex owner_in, ScopeIndex index_in):
        module(module_in), parent(parent_in), function(nullptr), owner(owner_in), index(index_in), kind(kind_in) {
        globalIndex = module->compilation->numScopes ++;
        if (parent) {
            Scope* p = parent;
            while (p->kind != Kind::Root) {
                if (p->kind == Kind::Function) {
                    function = p->function;
                    break;
                }
                p = p->parent;
            }
        }
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
                .note(module, module->node(varInfo.decl).pos(), "Previous definition was here.");
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

    const VariableInfo& Scope::FindResult::info() const {
        const auto& info = scope->function ? scope->function->locals[index] : scope->module->globals[index];
        if (info.kind == VariableKind::Forward) {
            const Scope* defScope = scope->module->scopes[info.defScope];
            return defScope->isGlobal() ? defScope->module->globals[info.index] : defScope->function->locals[info.index];
        }
        return info;
    }

    VariableInfo& Scope::FindResult::info() {
        auto& info = scope->function ? scope->function->locals[index] : scope->module->globals[index];
        if (info.kind == VariableKind::Forward) {
            Scope* defScope = scope->module->scopes[info.defScope];
            return defScope->isGlobal() ? defScope->module->globals[info.index] : defScope->function->locals[info.index];
        }
        return info;
    }

    Scope::FindResult Scope::find(Symbol name, bool searchParent) {
        if (inTable.mayContain(name.symbol)) {
            auto it = entries.find(name);
            if (it != entries.end())
                return FindResult(this, it->value, !function);
        }
        if (searchParent) {
            if (instParent) {
                auto result = instParent->findMethodOnly(name);
                if (result)
                    return result;
            }
            if (kind == ScopeKind::Namespace) {
                auto result = module->namespaces[ns]->lookup(name);
                if (result)
                    return result;
            }
            if (parent)
                return parent->find(name);
        }
        return {};
    }

    Scope::FindResult Scope::findMethodOnly(Symbol name, bool searchParent) {
        if (inTable.mayContain(name.symbol)) {
            auto it = entries.find(name);
            if (it != entries.end()) {
                auto result = FindResult(this, it->value, !function);
                if (result.info().kind == VariableKind::Function || result.info().kind == VariableKind::OverloadedFunction || result.info().kind == VariableKind::GenericFunction)
                    return result;
            }
        }
        if (searchParent) {
            if (instParent) {
                auto result = instParent->findMethodOnly(name);
                if (result)
                    return result;
            }
            if (kind == ScopeKind::Namespace) {
                auto result = module->namespaces[ns]->lookupMethodOnly(name);
                if (result)
                    return result;
            }
            if (parent && searchParent)
                return parent->findMethodOnly(name);
        }
        return {};
    }

    void Scope::add(VariableKind kind, const AST& decl, TypeIndex type, Symbol name) {
        assert(kind != VariableKind::Constant && kind != VariableKind::Function && kind != VariableKind::ConstFunction);
        if (entries.find(name) != entries.end()) {
            auto prev = entries.find(name)->value;
            const auto& varInfo = function ? function->locals[prev] : module->globals[prev];
            error(decl.module, decl.pos(), "Duplicate definition of symbol '", decl.module->str(name), "'.")
                .note(module, module->node(varInfo.decl).pos(), "Previous definition was here.");
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
                .note(module, module->node(varInfo.decl).pos(), "Previous definition was here.");
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
        definesFunctions = true;
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

    void Scope::addOverloadedFunction(Overloads* overloads) {
        u32 var;
        if (function)
            var = function->addLocalOverload(overloads).index;
        else
            var = module->addGlobalOverload(overloads).index;
        definesFunctions = true;
        entries.put(overloads->name, var);
        inTable.add(overloads->name.symbol);
    }

    void Scope::addConstantIndirect(Module* module, const AST& ast, Scope* defScope, u32 constantIndex, Symbol name) {
        if (entries.find(name) != entries.end()) {
            auto prev = entries.find(name)->value;
            const auto& varInfo = function ? function->locals[prev] : module->globals[prev];
            error(module, ast.pos(), "Duplicate definition of symbol '", module->str(name), "'.")
                .note(module, module->node(varInfo.decl).pos(), "Previous definition was here.");
            return;
        }

        ScopeIndex scopeIndex = module->scopeIndex(defScope);

        u32 var;
        if (function)
            var = function->addConstantIndirect(scopeIndex, constantIndex).index;
        else
            var = module->addConstantIndirect(scopeIndex, constantIndex).index;
        entries.put(name, var);
        inTable.add(name.symbol);
    }

    void Scope::addIndirect(Module* module, const AST& ast, Scope* defScope, u32 index, Symbol name) {
        if (entries.find(name) != entries.end()) {
            auto prev = entries.find(name)->value;
            const auto& varInfo = function ? function->locals[prev] : module->globals[prev];
            error(module, ast.pos(), "Duplicate definition of symbol '", module->str(name), "'.")
                .note(module, module->node(varInfo.decl).pos(), "Previous definition was here.");
            return;
        }

        ScopeIndex scopeIndex = module->scopeIndex(defScope);

        u32 var;
        if (function)
            var = function->addLocalIndirect(scopeIndex, index).index;
        else
            var = module->addGlobalIndirect(scopeIndex, index).index;
        entries.put(name, var);
        inTable.add(name.symbol);
    }

    void Scope::addNamespace(const AST& ast, Symbol name, Namespace* ns) {
        auto it = entries.find(name);
        if (it != entries.end()) {
            auto& varInfo = function ? function->locals[it->value] : module->globals[it->value];
            error(module, ast.pos(), "Duplicate definition of symbol '", module->str(name), "'.")
                .note(module, module->node(varInfo.decl).pos(), "Previous definition was here.");
            return;
        }

        u32 var;
        if (function)
            var = function->addLocalNamespace(ns).index;
        else
            var = module->addGlobalNamespace(ns).index;
        entries.put(name, var);
        inTable.add(name.symbol);
    }

    void Scope::addGenericType(const AST& decl, GenericType* genericType) {
        auto it = entries.find(genericType->name);
        if (it != entries.end()) {
            auto& varInfo = function ? function->locals[it->value] : module->globals[it->value];
            error(module, decl.pos(), "Duplicate definition of symbol '", module->str(genericType->name), "'.")
                .note(module, module->node(varInfo.decl).pos(), "Previous definition was here.");
            return;
        }

        u32 var;
        if (function)
            var = function->addLocalGenericType(genericType).index;
        else
            var = module->addGlobalGenericType(genericType).index;
        entries.put(genericType->name, var);
        inTable.add(genericType->name.symbol);
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
                    info->overloadsIndex = inPlaceOverloads->index;
                } else
                    overloads = module->addOverloads(module->functions[info->functionIndex]);
            } else if (info->kind == VariableKind::OverloadedFunction) {
                if (existing.scope == scope) {
                    if (isOverloads)
                        module->overloads[info->overloadsIndex]->add((Overloads*)ptr);
                    else
                        module->overloads[info->overloadsIndex]->add((Function*)ptr);
                } else
                    overloads = module->addOverloads(module->overloads[info->overloadsIndex]);
            } else
                error(scope->module, pos, "Tried to define function '", module->str(name), "' but it was already declared as a ", VariableInfo::KindNamesLower[info->kind], ".");
            if (overloads) {
                if (isOverloads)
                    overloads->add((Overloads*)ptr);
                else
                    overloads->add((Function*)ptr);
                scope->addOverloadedFunction(overloads);
            }
        } else {
            if (isOverloads)
                scope->addOverloadedFunction((Overloads*)ptr);
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

    void defineNamespace(Scope* scope, Namespace* relative, Namespace* ns, AST pos) {
        Module* module = scope->module;

        // First, we figure out what the new tree node of this namespace is.
        // Since moving a namespace out from its previous parent may change its
        // top-level symbol, we need to reconstruct its path from the root.
        //
        // For example, if we have a namespace foo.bar, and use foo.bar, then
        // in that scope foo.bar will be renamed to bar. We want to treat it in
        // that scope as if it was declared simply as bar from the start, so it
        // can comingle with other definitions in that scope that are simply
        // under the bar namespace.

        NamespaceTree* newNode = ns->node;
        if (relative) {
            NamespaceTree* iter = ns->node;
            vec<Symbol, 8> path;
            while (iter != relative->node) {
                assert(iter);
                path.push(iter->name);
                iter = iter->parent;
            }
            newNode = scope->module->compilation->ensureNamespace(path.pop());
            while (path.size())
                newNode = newNode->ensureChild(path.pop());
        }

        // We always construct a new namespace at the moment. In many cases, we
        // need to do this because our node may have changed. But even if not,
        // I think this probably helps prevent some kind of coupling issues -
        // we ensure that the imported namespace can't affect its parent(s).

        Namespace* newNs = scope->module->addNamespace(newNode);
        newNs->addParent(ns);

        auto existing = scope->find(ns->name());
        if (existing) {
            Module* otherModule = existing.scope->module;
            const auto& info = existing.info();
            NamespaceTree* oldNode = otherModule->namespaces[info.namespaceIndex]->node;
            if (info.kind == VariableKind::Namespace && otherModule->namespaces[info.namespaceIndex]->node == newNode) {
                // This is the same namespace, which already exists in our
                // scope. We want to add this as a parent to our new namespace too.
                if (otherModule->namespaces[info.namespaceIndex] != ns)
                    newNs->addParent(otherModule->namespaces[info.namespaceIndex]);
            } else {
                auto& e = error(module, pos.pos(), "Duplicate definition of symbol '", module->str(info.name), "'.");
                if (!info.isImport) {
                    AST decl = otherModule->node(info.decl);
                    if (!decl.isLeaf())
                        e.note(otherModule, decl.pos(), "Previous definition was here.");
                }
            }
        }

        scope->addNamespace(pos, ns->name(), newNs);
    }

    void importNamespace(Scope* scope, Namespace* ns, AST use) {
        Module* module = scope->module;

        ns->forEachDefinition([&](Module* otherModule, Function* otherFunction, Scope* otherScope, const VariableInfo& info, u32 index) {
            switch (info.kind) {
                case VariableKind::Variable:
                case VariableKind::Type: {
                    auto existing = scope->findLocal(info.name);
                    if (existing) {
                        auto& e = error(module, use.pos(), "Duplicate definition of symbol '", module->str(info.name), "'.");
                        if (!info.isImport) {
                            AST decl = otherModule->node(info.decl);
                            if (!decl.isLeaf())
                                e.note(otherModule, decl.pos(), "Previous definition was here.");
                        }
                        break;
                    }
                    if (otherModule == module)
                        scope->addIndirect(module, use, otherScope, index, info.name);
                    else
                        scope->addImport(info.kind, info.type, info.name);
                    break;
                }

                case VariableKind::Constant: {
                    auto existing = scope->findLocal(info.name);
                    if (existing) {
                        auto& e = error(module, use.pos(), "Duplicate definition of symbol '", module->str(info.name), "'.");
                        if (!info.isImport) {
                            AST decl = otherModule->node(info.decl);
                            if (!decl.isLeaf())
                                e.note(otherModule, decl.pos(), "Previous definition was here.");
                        }
                        break;
                    }
                    scope->addConstantIndirect(module, use, otherScope, index, info.name);
                    break;
                }

                case VariableKind::Function: {
                    assert(info.isImport);
                    Function* function = otherModule->functions[info.functionIndex];
                    defineFunction(scope, info.name, use.pos(), function);
                    break;
                }

                case VariableKind::OverloadedFunction: {
                    assert(info.isImport);
                    Overloads* overloads = otherModule->overloads[info.overloadsIndex];
                    defineOverloads(scope, info.name, use.pos(), overloads);
                    break;
                }

                case VariableKind::Namespace: {
                    Namespace* importedNs = otherModule->namespaces[info.namespaceIndex];
                    defineNamespace(scope, ns, importedNs, use);
                    break;
                }

                default:
                    unreachable("Tried to import unknown variable kind ", VariableInfo::KindNamesUpper[(u32)info.kind], " from namespace ", module->str(ns->name()));
            }
        });
    }

    void setScopes(Module* module, Scope* scope, AST ast) {
        if (ast.isLeaf())
            return;
        ast.setScope(scope);
        for (AST child : ast)
            setScopes(module, scope, child);
    }

    void addGenericType(Module* module, Scope* currentScope, AST ast, Symbol name, ASTKind genericKind) {
        // We define a pseudo-scope for generic types. They're kind of a
        // weird situation overall - we can't fully resolve them, because we
        // need to keep the raw identifiers around until instantiation time,
        // otherwise we'd be relying on the indexing of the global defs being
        // the same for each instantiation (which is basically never true since
        // each instantiation owns its own definitions). But, we do need to try
        // to resolve the body, to discover any tacit type parameters that
        // weren't explicitly declared. So, we need a real scope attached, with
        // at least dummy entries for our known type parameters.
        Scope* newScope = module->addScope(ScopeKind::GenericType, ast.node, currentScope);
        ast.setScope(newScope);
        for (AST child : ast.children(1))
            computeScopes(module, newScope, child);

        // We do create a GenericType in the module to keep track
        // of this template for future reference.
        GenericType* genericType = module->addGenericType(name, ast, currentScope);
        ast.setKind(genericKind);

        // We'll use this to represent a linked list of instantiations. Unlike
        // a generic function, which can be trivially recovered from the scope,
        // we want to be able to dig into this list and find the GenericType
        // instance. So we insert it as the tail of the list.
        ast.setChild(0, module->add(ASTKind::ResolvedGenericType, genericType));

        currentScope->addGenericType(ast, genericType);
    }

    void computeScopes(Module* module, Imports& imports, Scope* currentScope, AST ast) {
        switch (ast.kind()) {
            case ASTKind::Local:
            case ASTKind::Const:
            case ASTKind::Typename:
            case ASTKind::Capture:
            case ASTKind::Global:
            case ASTKind::GlobalConst:
            case ASTKind::GlobalTypename:
            case ASTKind::Exp:
            case ASTKind::ArrayType:
            case ASTKind::FunType:
            case ASTKind::ResolvedGenericType:
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
            case ASTKind::AddrField:
            case ASTKind::AddrIndex:
            case ASTKind::EnsureAddrField:
            case ASTKind::EnsureAddrIndex:
            case ASTKind::NamedParameter:
                ast.setScope(currentScope);
                computeScopes(module, imports, currentScope, ast.child(0));
                computeScopes(module, imports, currentScope, ast.child(1));
                break;
            case ASTKind::GetSlice:
            case ASTKind::SetField:
            case ASTKind::SetIndex:
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
            case ASTKind::Construct:
            case ASTKind::GenericInst: {
                ast.setScope(currentScope);
                for (AST child : ast)
                    computeScopes(module, imports, currentScope, child);
                break;
            }
            case ASTKind::UseLocal:
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
                if (name.kind() == ASTKind::GetField) // Method-style function declaration. We don't worry about it in this pass.
                    name = name.child(1);
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
                Symbol name;
                if LIKELY(ast.child(0).kind() == ASTKind::Ident)
                    name = ast.child(0).symbol();
                else {
                    // This can happen in the case that we instantiate a
                    // generic type in the middle of resolving its body. In
                    // this case, the nodes can be in a half-resolved state.
                    assert(ast.kind() == ASTKind::NamedCaseDecl);
                    name = ast.child(0).varInfo(currentScope->function).name;
                }

                if (!ast.child(1).missing()) {
                    if (ast.kind() == ASTKind::NamedCaseDecl) {
                        // Unsigned is allowed because we use the 2nd slot as
                        // a bit of a hack to stash the case's parent pointer.
                        if (ast.child(1).kind() != ASTKind::Unsigned)
                            error(module, ast.pos(), "Type parameters are not allowed in case types.");
                    } else
                        return addGenericType(module, currentScope, ast, name, ASTKind::GenericNamedDecl);
                }

                currentScope->add(VariableKind::Type, ast, name); // Type name
                Scope* newScope = module->addScope(ScopeKind::Type, ast.node, currentScope);
                ast.setScope(newScope);
                computeScopes(module, imports, newScope, ast.child(2));
                break;
            }
            case ASTKind::StructDecl:
            case ASTKind::StructCaseDecl: {
                Symbol name;
                if LIKELY(ast.child(0).kind() == ASTKind::Ident)
                    name = ast.child(0).symbol();
                else {
                    // This can happen in the case that we instantiate a
                    // generic type in the middle of resolving its body. In
                    // this case, the nodes can be in a half-resolved state.
                    assert(ast.kind() == ASTKind::StructCaseDecl);
                    name = ast.child(0).varInfo(currentScope->function).name;
                }

                if (!ast.child(1).missing()) {
                    if (ast.kind() == ASTKind::StructCaseDecl) {
                        // Unsigned is allowed because we use the 2nd slot as
                        // a bit of a hack to stash the case's parent pointer.
                        if (ast.child(1).kind() != ASTKind::Unsigned)
                            error(module, ast.pos(), "Type parameters are not allowed in case types.");
                    } else
                        return addGenericType(module, currentScope, ast, name, ASTKind::GenericStructDecl);
                }

                currentScope->add(VariableKind::Type, ast, name); // Type name
                Scope* newScope = module->addScope(ScopeKind::Type, ast.node, currentScope);
                ast.setScope(newScope);
                for (u32 i = 2; i < ast.arity(); i ++)
                    computeScopes(module, imports, newScope, ast.child(i));
                break;
            }
            case ASTKind::UnionDecl:
            case ASTKind::UnionCaseDecl: {
                Symbol name;
                if LIKELY(ast.child(0).kind() == ASTKind::Ident)
                    name = ast.child(0).symbol();
                else {
                    // This can happen in the case that we instantiate a
                    // generic type in the middle of resolving its body. In
                    // this case, the nodes can be in a half-resolved state.
                    assert(ast.kind() == ASTKind::UnionCaseDecl);
                    name = ast.child(0).varInfo(currentScope->function).name;
                }

                if (!ast.child(1).missing()) {
                    if (ast.kind() == ASTKind::UnionCaseDecl) {
                        // Unsigned is allowed because we use the 2nd slot as
                        // a bit of a hack to stash the case's parent pointer.
                        if (ast.child(1).kind() != ASTKind::Unsigned)
                            error(module, ast.pos(), "Type parameters are not allowed in case types.");
                    } else
                        return addGenericType(module, currentScope, ast, name, ASTKind::GenericUnionDecl);
                }

                currentScope->add(VariableKind::Type, ast, name); // Type name
                Scope* newScope = module->addScope(ScopeKind::Type, ast.node, currentScope);
                ast.setScope(newScope);
                for (u32 i = 2; i < ast.arity(); i ++) {
                    switch (ast.child(i).kind()) {
                        case ASTKind::NamedCaseDecl:
                        case ASTKind::StructCaseDecl:
                        case ASTKind::UnionCaseDecl:
                            ast.child(i).setChild(1, module->add(ASTKind::Unsigned, Constant::UnsignedConst(ast.node)));
                            break;
                        default:
                            break;
                    }
                    computeScopes(module, imports, newScope, ast.child(i));
                }
                break;
            }
            case ASTKind::Namespace: {
                assert(ast.child(0).kind() == ASTKind::Ident);
                Symbol name = ast.child(0).symbol();

                // We need to see if we are inside a namespace. Only our
                // immediate parent counts when determining what namespace
                // we're in.
                Namespace* parentNamespace = nullptr;
                if (currentScope && currentScope->kind == ScopeKind::Namespace)
                    parentNamespace = module->node(currentScope->owner).child(0).resolvedNamespace();
                NamespaceTree* node = parentNamespace
                    ? parentNamespace->node->ensureChild(name)
                    : module->compilation->ensureNamespace(name);

                Namespace* ns = nullptr;
                Namespace* parentNs = nullptr;

                // See if there is already an instance of our namespace in
                // this exact scope. If so, we can simply use that and move on.
                auto entry = currentScope->find(name);
                if (entry) {
                    const auto& info = entry.isGlobal() ? module->globals[entry.index] : currentScope->function->locals[entry.index];
                    if (info.kind == VariableKind::Namespace && module->namespaces[info.namespaceIndex]->node == node)
                        (entry.scope == currentScope ? ns : parentNs) = entry.scope->module->namespaces[info.namespaceIndex];
                    else {
                        error(module, ast.pos(), "Duplicate definition of symbol '", module->str(name), "'.")
                            .note(module, module->node(info.decl).pos(), "Previous definition was here.");
                        break;
                    }
                }
                if (!ns) {
                    ns = module->addNamespace(node);
                    if (parentNs)
                        ns->addParent(parentNs);
                    currentScope->addNamespace(ast, ast.child(0).symbol(), ns);
                }
                ast.setChild(0, module->add(ASTKind::ResolvedNamespace, ns));

                Scope* newScope = module->addScope(ScopeKind::Namespace, ast.node, currentScope);
                newScope->ns = ns->index;
                ns->addScope(newScope);
                ast.setScope(newScope);
                computeScopes(module, imports, newScope, ast.child(1));
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

            if (ast.kind() == ASTKind::UseLocal) {
                Scope* defScope = scope;
                Namespace* defNs = nullptr;
                bool inType = false;
                for (u32 i = 0; i < path.size() - 1; i ++) {
                    auto entry = defNs ? defNs->lookup(path[i]) : defScope->find(path[i]);
                    if (!entry) {
                        error(module, ast.pos(), "Couldn't find symbol ", module->str(path[i]), " within ", module->str(path[i]), ".");
                        return;
                    }
                    if (entry.isGlobal()) {
                        const auto& global = entry.scope->module->globals[entry.index];
                        if (global.kind == VariableKind::Namespace)
                            defScope = nullptr, defNs = entry.scope->module->namespaces[global.namespaceIndex];
                        else {
                            defNs = nullptr;
                            if (global.kind == VariableKind::Type)
                                defScope = entry.scope->module->node(global.decl).scope();
                            else if (global.kind == VariableKind::GenericType) {
                                GenericType* generic = entry.scope->module->genericTypes[entry.info().genericTypeIndex];
                                defScope = generic->module->node(generic->decl).scope();
                            }
                            if (!inType) {
                                inType = true;
                                if (defScope->module == module)
                                    module->possiblyGenericTypeImports.push({ defScope, defScope->owner });
                            }
                        }
                    } else {
                        const auto& local = entry.scope->function->locals[entry.index];
                        if (local.kind == VariableKind::Namespace)
                            defScope = nullptr, defNs = entry.scope->module->namespaces[local.namespaceIndex];
                        else {
                            defNs = nullptr;
                            if (local.kind == VariableKind::Type)
                                defScope = entry.scope->module->node(local.decl).scope();
                            else if (local.kind == VariableKind::GenericType) {
                                GenericType* generic = entry.scope->module->genericTypes[entry.info().genericTypeIndex];
                                defScope = generic->module->node(generic->decl).scope();
                            }
                            if (!inType) {
                                inType = true;
                                if (defScope->module == module)
                                    module->possiblyGenericTypeImports.push({ defScope, defScope->owner });
                            }
                        }
                    }
                }
                if (path.back() == InvalidSymbol) {
                    if (defNs)
                        importNamespace(scope, defNs, ast);
                    else {
                        assert(defScope->kind == ScopeKind::Type || defScope->kind == ScopeKind::GenericType);
                        for (const auto [k, v] : defScope->entries) {
                            VariableInfo info = defScope->function ? defScope->function->locals[v] : defScope->module->globals[v];
                            if (info.kind == VariableKind::Constant)
                                scope->addConstantIndirect(module, ast, defScope, info.constantIndex, k);
                            else if (info.kind == VariableKind::Namespace)
                                defineNamespace(scope, nullptr, defScope->module->namespaces[info.namespaceIndex], ast);
                            else
                                scope->addIndirect(module, ast, defScope, v, k);
                        }
                    }
                } else {
                    auto entry = defNs ? defNs->lookup(path.back()) : defScope->findLocal(path.back());
                    if (!entry) {
                        error(module, ast.pos(), "Couldn't find symbol ", module->str(path.back()), " within ", module->str(path[path.size() - 2]), ".");
                        return;
                    }
                    defScope = entry.scope;
                    VariableInfo info = defScope->function ? defScope->function->locals[entry.index] : module->globals[entry.index];
                    if (info.kind == VariableKind::Constant)
                        scope->addConstantIndirect(module, ast, defScope, info.constantIndex, path.back());
                    else if (info.kind == VariableKind::Namespace) {
                        Namespace* root = nullptr;
                        if (defScope->kind == ScopeKind::Namespace)
                            root = module->node(defScope->owner).child(0).resolvedNamespace();
                        defineNamespace(scope, root, module->namespaces[info.namespaceIndex], ast);
                    } else {
                        scope->addIndirect(module, ast, defScope, entry.index, path.back());
                    }
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
                if (!artifact) {
                    auto pathstr = filepath.to_bytes();
                    error(module, ast.pos(), "Could not load module at path '", pathstr, "'.");
                    delete[] pathstr.data();
                    continue;
                }
                artifact = compileUntil(module->compilation, ArtifactKind::CheckedAST, artifact);
                Module* otherModule = artifact->as<Module>();
                module->artifact->imports.insert(artifact);

                Scope* topLevelScope = otherModule->getTopLevel().scope();
                for (const auto& [k, v] : topLevelScope->entries) {
                    VariableInfo info = otherModule->globals[v];
                    if (info.kind == VariableKind::Namespace) {
                        Namespace* ns = otherModule->namespaces[info.functionIndex];
                        defineNamespace(scope, nullptr, ns, ast);
                    } else if (info.kind == VariableKind::Function) {
                        Function* function = otherModule->functions[info.functionIndex];
                        defineFunction(scope, k, ast.pos(), function);
                    } else if (info.kind == VariableKind::OverloadedFunction) {
                        Overloads* overloads = otherModule->overloads[info.overloadsIndex];
                        defineOverloads(scope, k, ast.pos(), overloads);
                    } else if (info.kind == VariableKind::GenericType) {
                        GenericType* genericType = otherModule->genericTypes[info.genericTypeIndex];
                        scope->addGenericType(ast, genericType);
                    } else if (info.kind == VariableKind::Forward) {
                        Scope* origin = otherModule->scopes[info.defScope];
                        module->scopeIndex(origin); // Do this for the side effects, to ensure we assign the origin scope an index in our module.
                        scope->addIndirect(module, ast, origin, info.index, k);
                    } else if (auto result = scope->findLocal(k)) {
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
        artifact->update(ArtifactKind::ScopedAST, module);
        if UNLIKELY(config::printScopeTree)
            module->printScopes(module->compilation);
        return artifact;
    }
}