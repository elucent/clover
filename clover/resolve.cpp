#include "clover/resolve.h"
#include "util/config.h"

namespace clover {
    bool isTypeExpression(AST ast) {
        switch (ast.kind()) {
            case ASTKind::FunType:
            case ASTKind::Typename:
            case ASTKind::GlobalTypename:
            case ASTKind::ArrayType:
            case ASTKind::SliceType:
            case ASTKind::PtrType:
            case ASTKind::OwnType:
            case ASTKind::UninitType:
            case ASTKind::TupleType:
            case ASTKind::TypeField:
            case ASTKind::GenericInst:
                return true;
            default:
                return false;
        }
    }

    struct LateUse {
        ScopeIndex scope;
        NodeIndex node;

        inline LateUse(ScopeIndex scope_in, NodeIndex node_in):
            scope(scope_in), node(node_in) {}
    };

    struct Fixups {
        vec<LateUse, 8> lateUses;
        vec<NodeIndex, 8> addressOfs;
        vec<NodeIndex, 8> assignments;

        void reportAddressOf(NodeIndex node) {
            addressOfs.push(node);
        }

        void reportAssignment(NodeIndex node) {
            assignments.push(node);
        }

        void use(ScopeIndex scope, NodeIndex node) {
            lateUses.push(LateUse(scope, node));
        }
    };

    enum Expectation {
        ExpectType,
        ExpectValue
    };

    AST resolve(Module* module, Fixups& fixups, RefTraits refTraits, Scope* scope, maybe<AST> parent, AST ast, Expectation expectation);

    inline ALWAYSINLINE AST resolveChild(Module* module, Fixups& fixups, RefTraits refTraits, AST ast, u32 i, Expectation expectation) {
        ast.setChild(i, resolve(module, fixups, refTraits, ast.scope(), some<AST>(ast), ast.child(i), expectation));
        return ast.child(i);
    }

    Type resolveTypeForDecl(Module* module, Fixups& fixups, RefTraits refTraits, AST ast);

    Type evaluateType(Module* module, Fixups& fixups, Scope* scope, AST ast) {
        switch (ast.kind()) {
            case ASTKind::Typename: {
                Type type = expand(module->types->get(ast.varInfo(scope->function).type));
                ast.varInfo(scope->function).type = type.index;
                return type;
            }
            case ASTKind::GlobalTypename: {
                Type type = expand(module->types->get(ast.varInfo().type));
                ast.varInfo().type = type.index;
                return type;
            }
            case ASTKind::AliasDecl:
            case ASTKind::NamedDecl:
            case ASTKind::NamedCaseDecl:
            case ASTKind::StructDecl:
            case ASTKind::StructCaseDecl:
            case ASTKind::UnionDecl:
            case ASTKind::UnionCaseDecl:
                if (!ast.type())
                    return resolveTypeForDecl(module, fixups, NoRefTraits, ast);
                else
                    return ast.type();
            case ASTKind::PtrType:
            case ASTKind::OwnType:
            case ASTKind::UninitType:
            case ASTKind::FunType:
            case ASTKind::ArrayType:
            case ASTKind::SliceType:
            case ASTKind::TupleType:
            case ASTKind::TypeField:
                assert(ast.type());
                return ast.type();
            case ASTKind::GenericInst:
                unreachable("TODO: Implement generic types.");
            default:
                unreachable("Can't evaluate non-type expression ", ast);
        }
    }

    Type resolveTypeForDecl(Module* module, Fixups& fixups, RefTraits refTraits, AST ast) {
        if (ast.type())
            return ast.type();
        switch (ast.kind()) {
            case ASTKind::AliasDecl:
                resolveChild(module, fixups, refTraits, ast, 2, ExpectType);
                assert(isTypeExpression(ast.child(2)));
                ast.setType(evaluateType(module, fixups, ast.scope(), ast.child(2)));
                resolveChild(module, fixups, refTraits, ast, 0, ExpectType);
                return ast.type();
            case ASTKind::NamedDecl:
            case ASTKind::NamedCaseDecl: {
                Type placeholder = module->varType();
                ast.setType(placeholder);
                AST child = resolveChild(module, fixups, refTraits, ast, 2, ExpectType);
                if (child.kind() == ASTKind::VarDecl) {
                    // We discovered we're a struct after resolving a confusing
                    // pointer decl, i.e. type Foo: i32 * x
                    assert(ast.child(0).kind() == ASTKind::Ident);
                    auto typeName = ast.child(0).symbol();
                    assert(!child.child(0).missing());
                    Type type = child.type();
                    Symbol name = child.child(1).varInfo(ast.function()).name;
                    if (ast.kind() == ASTKind::NamedCaseDecl)
                        ast.setType(module->structType(typeName, ast.scope(), Field(module->types, name, type), IsCase));
                    else
                        ast.setType(module->structType(typeName, ast.scope(), Field(module->types, name, type)));
                    ast.setKind(ast.kind() == ASTKind::NamedCaseDecl ? ASTKind::StructCaseDecl : ASTKind::StructDecl);
                } else {
                    assert(isTypeExpression(ast.child(2)) || ast.child(2).missing());
                    Type type = ast.child(2).missing() ? module->voidType() : evaluateType(module, fixups, ast.scope(), ast.child(2));
                    if (ast.kind() == ASTKind::NamedCaseDecl)
                        ast.setType(module->namedType(ast.child(0).symbol(), ast.scope(), type, IsCase));
                    else
                        ast.setType(module->namedType(ast.child(0).symbol(), ast.scope(), type));
                }
                assert(!placeholder.asVar().isEqual());
                placeholder.asVar().makeEqual(ast.type());
                resolveChild(module, fixups, refTraits, ast, 0, ExpectType);
                return ast.type();
            }
            case ASTKind::StructDecl:
            case ASTKind::StructCaseDecl: {
                assert(ast.child(0).kind() == ASTKind::Ident);
                auto sym = ast.child(0).symbol();
                Type placeholder = module->varType();
                ast.setType(placeholder);
                auto builder = StructBuilder(module->types, sym);
                builder.isCase = ast.kind() == ASTKind::StructCaseDecl;
                for (u32 i = 2; i < ast.arity(); i ++) {
                    AST member = resolveChild(module, fixups, refTraits, ast, i, ExpectValue);
                    assert(member.kind() != ASTKind::NamedCaseDecl && member.kind() != ASTKind::StructCaseDecl && member.kind() != ASTKind::UnionCaseDecl);
                    if (member.kind() == ASTKind::VarDecl) {
                        assert(!member.child(0).missing());
                        Type type = member.type();
                        Symbol name = member.child(1).varInfo(ast.function()).name;
                        builder.add(Field(module->types, name, type));
                    }
                }
                builder.add(ast.scope());
                ast.setType(builder.build(module->types));
                assert(!placeholder.asVar().isEqual());
                placeholder.asVar().makeEqual(ast.type());
                resolveChild(module, fixups, refTraits, ast, 0, ExpectType);
                return ast.type();
            }
            case ASTKind::UnionDecl:
            case ASTKind::UnionCaseDecl: {
                assert(ast.child(0).kind() == ASTKind::Ident);
                auto sym = ast.child(0).symbol();
                Type placeholder = module->varType();
                ast.setType(placeholder);
                auto builder = UnionBuilder(sym);
                builder.isCase = ast.kind() == ASTKind::UnionCaseDecl;
                for (u32 i = 2; i < ast.arity(); i ++) {
                    AST member = resolveChild(module, fixups, refTraits, ast, i, ExpectValue);
                    assert(member.kind() == ASTKind::NamedCaseDecl || member.kind() == ASTKind::StructCaseDecl || member.kind() == ASTKind::UnionCaseDecl);
                    Type type = evaluateType(module, fixups, ast.scope(), ast.child(i));
                    builder.add(type);
                }
                builder.add(ast.scope());
                ast.setType(builder.build(module->types));
                assert(!placeholder.asVar().isEqual());
                placeholder.asVar().makeEqual(ast.type());
                resolveChild(module, fixups, refTraits, ast, 0, ExpectType);
                return ast.type();
            }
            default:
                unreachable("Not a type decl.");
        }
    }

    AST resolveIdentifier(Module* module, Fixups& fixups, Scope* scope, AST parent, AST ast, VariableHandle entry) {
        if (!entry) {
            assert(parent);
            fixups.use(scope->index, parent.node);
            return ast;
        }
        switch (entry.kind()) {
            case VariableKind::Type:
                if (entry.type() == InvalidType && entry.hasDecl())
                    entry.setType(resolveTypeForDecl(entry.scope->module, fixups, NoRefTraits, entry.decl()));
                if (entry.isGlobal())
                    return module->add(ASTKind::GlobalTypename, Global(entry.index()));
                else
                    return module->add(ASTKind::Typename, Local(entry.index()));
            case VariableKind::Variable:
            case VariableKind::OverloadedFunction:
            case VariableKind::Member:
                if (entry.isGlobal())
                    return module->add(ASTKind::Global, Global(entry.index()));
                else
                    return module->add(ASTKind::Local, Local(entry.index()));
            case VariableKind::Constant: {
                if (entry.isGlobal() && !scope->function && entry.scope->module == module)
                    return module->add(ASTKind::GlobalConst, ConstId(module->globals[entry.index()].constantIndex));
                if (entry.scope->function == scope->function)
                    return module->add(ASTKind::Const, ConstId(scope->function->locals[entry.index()].constantIndex));

                // It's an external constant, so we need to close over
                // it.
                VariableInfo info = entry.isGlobal() ? entry.scope->module->globals[entry.index()] : entry.scope->function->locals[entry.index()];
                scope->addConstantIndirect(module, parent, entry.scope, info.constantIndex, ast.symbol());
                auto reentry = scope->findLocal(ast.symbol());
                assert(reentry);
                return module->add(ASTKind::Const, ConstId(scope->function->locals[reentry.index].constantIndex));
            }
            case VariableKind::ConstFunction: {
                const VariableInfo& info = entry.isGlobal()
                    ? entry.scope->module->globals[entry.index()]
                    : entry.scope->function->locals[entry.index()];
                Function* function = module->functions[info.functionIndex];
                return module->add(ASTKind::ResolvedFunction, function);
            }

            case VariableKind::Function: {
                const VariableInfo& info = entry.isGlobal()
                    ? entry.scope->module->globals[entry.index()]
                    : entry.scope->function->locals[entry.index()];
                Function* function = entry.scope->module->functions[info.functionIndex];
                return module->add(ASTKind::ResolvedFunction, function);
            }

            case VariableKind::Namespace: {
                const VariableInfo& info = entry.isGlobal()
                    ? entry.scope->module->globals[entry.index()]
                    : entry.scope->function->locals[entry.index()];
                Namespace* ns = entry.scope->module->namespaces[info.namespaceIndex];
                return module->add(ASTKind::ResolvedNamespace, ns);
            }

            case VariableKind::ThisAccess:
                return resolve(module, fixups, NoRefTraits, scope, some<AST>(parent), module->add(ASTKind::GetField, parent.pos(), scope, InvalidType, module->add(ASTKind::Ident, Identifier(KeywordThis)), ast), ExpectValue);

            default:
                return ast;
        }
        unreachable("Should have returned already.");
    }

    AST fixupAccessBase(Module* module, AST ast) {
        // This function recursively fixes up accessor chains to ensure mutable
        // access at the end refers back to the original object. In general, we
        // promote GetField and GetIndex nodes to EnsureAddrField and
        // EnsureAddrIndex in order to make sure we pass along the base
        // address. The exception to this case is that any GetField to an
        // AddrFields node, or any chain of GetFields nodes whose base is an
        // AddrFields, remains as a GetField, since we already materialized the
        // address in the intermediate tuple.

        // We do this for both mutable and immutable accesses, because even for
        // chained reads, it's better to avoid having intermediate products
        // with value semantics that we'll just need to eliminate later.

        // One exception to all of this that can't be handled during this phase
        // is swizzled field accesses. We don't know at this point if an access
        // like foo.xy will really turn into foo.(x, y) once we determine foo's
        // type. We handle this during the typechecking phase and are then
        // responsible for ensuring the path remains semantically correct.

        // Notably, we use EnsureAddrField and EnsureAddrIndex, not the normal
        // AddrField and AddrIndex forms. The semantics of these nodes are such
        // that they behave like AddrField and AddrIndex, *unless* the value
        // being accessed is itself a pointer, in which case they behave like
        // GetField and GetIndex. This is because if we have a field access,
        // and as an interstitial access in that chain we get a pointer value,
        // we are going to be loading/storing through that pointer - we no
        // longer want to point back to a subfield of the parent instance. So
        // we don't need to propagate its address through.

        switch (ast.kind()) {
            case ASTKind::GetField:
            case ASTKind::GetFields: {
                ast.setChild(0, fixupAccessBase(module, ast.child(0)));
                AST base = ast.child(0);
                if (base.kind() == ASTKind::AddrFields || base.kind() == ASTKind::AddrIndices || base.kind() == ASTKind::GetFields
                    || base.kind() == ASTKind::EnsureAddrFields || base.kind() == ASTKind::EnsureAddrIndices) {
                    // Per the aforementioned exception, if we are getting a
                    // field from a tuple of computed addresses, it's okay if
                    // we remain a GetField. Likewise, if our parent - which
                    // we just fixed up - remains a GetFields, then it must be
                    // getting a sub-tuple of an AddrFields or AddrIndices in
                    // its own right. In either of these cases, we leave the
                    // current node as it is.
                    return ast;
                }

                // Otherwise, we become an address-of.
                ast.setKind(ast.kind() == ASTKind::GetField ? ASTKind::EnsureAddrField : ASTKind::EnsureAddrFields);
                return ast;
            }
            case ASTKind::GetIndex:
            case ASTKind::GetIndices: {
                ast.setChild(0, fixupAccessBase(module, ast.child(0)));

                // Because a variadic access always returns a tuple, which can
                // only be accessed by field and not by index, index getters
                // along the path have no exception and are always promoted to
                // address-of operations.
                ast.setKind(ast.kind() == ASTKind::GetIndex ? ASTKind::EnsureAddrIndex : ASTKind::EnsureAddrIndices);
                return ast;
            }
            default:
                return ast;
        }
    }

    maybe<AST> resolveTypeField(Module* module, Fixups& fixups, Scope* scope, AST base, AST child, Pos pos) {
        Type baseType = evaluateType(module, fixups, scope, base);

        // If the base type is an atom, it's possible this is in fact a field
        // or method access, and the type should be interpreted as a value. We
        // check for this here, and later when we would normally report an
        // error, we instead just return an empty optional which indicates to
        // the caller that we did not successfully resolve a type field.
        bool atom = isAtom(baseType);

        Scope* typeScope;
        switch (baseType.kind()) {
            case TypeKind::Named:
                typeScope = baseType.as<TypeKind::Named>().scope();
                break;
            case TypeKind::Struct:
                typeScope = baseType.as<TypeKind::Struct>().scope();
                break;
            case TypeKind::Union:
                typeScope = baseType.as<TypeKind::Union>().scope();
                break;
            default:
                unreachable("Tried to access field ", module->str(child.symbol()), " of non-named type ", baseType);
        }
        assert(child.kind() == ASTKind::Ident);
        auto result = typeScope->findLocal(child.symbol());
        if (!result) {
            if (atom)
                return none<AST>();
            unreachable("Unknown field ", module->str(child.symbol()), " within type ", baseType);
        }

        VariableInfo info;
        if (result.isGlobal())
            info = typeScope->module->globals[result.index];
        else
            info = typeScope->function->locals[result.index];

        switch (info.kind) {
            case VariableKind::Type:
                return some<AST>(module->add(ASTKind::TypeField, pos, scope, info.type, base, module->add(ASTKind::Field, FieldId(0))));
            case VariableKind::Constant: {
                if (result.isGlobal() && typeScope->module == module)
                    return some<AST>(module->add(ASTKind::GlobalConst, ConstId(info.constantIndex)));
                else if (scope->function && scope->function == typeScope->function)
                    return some<AST>(module->add(ASTKind::Const, ConstId(info.constantIndex)));
                else {
                    auto& importedConstants = scope->function ? scope->function->importedConstants : module->importedConstants;
                    auto it = importedConstants.find({ typeScope, info.constantIndex });
                    if (it != importedConstants.end())
                        return some<AST>(module->add(ASTKind::Const, ConstId(it->value)));

                    ConstInfo constInfo = (typeScope->function ? typeScope->function->constants : typeScope->module->globalConstants)[info.constantIndex];
                    auto& constants = scope->function ? scope->function->constants : module->globalConstants;
                    auto id = constants.size();
                    constants.push(constInfo);
                    importedConstants.put({ typeScope, info.constantIndex }, id);
                    return some<AST>(module->add(ASTKind::Const, ConstId(id)));
                }
            }
            default: {
                if (atom)
                    return none<AST>();
                unreachable("Can't access non-constant, non-type field ", module->str(child.symbol()), " from type ", baseType);
            }
        }
    }

    AST resolveCall(Module* module, Fixups& fixups, RefTraits refTraits, Scope* scope, AST call, Expectation expectation) {
        AST func;

        if (call.kind() == ASTKind::CallMethod) {
            AST base = resolveChild(module, fixups, refTraits, call, 1, expectation);

            if (base.kind() == ASTKind::ResolvedNamespace) {
                // This handles the case that we mistook a namespace access for
                // a method call, i.e. foo.bar(x). In this scenario, we resolve
                // the access from the namespace, then treat the call like a
                // non-method call and continue.
                type_assert(call.child(0).kind() == ASTKind::Ident);
                auto result = base.resolvedNamespace()->lookup(call.child(0).symbol());
                type_assert(result);
                call.setChild(0, resolveIdentifier(module, fixups, scope, call, call.child(0), module->naturalize({ result.scope, result.index })));
                for (u32 i = 2; i < call.arity(); i ++)
                    call.setChild(i - 1, call.child(i));
                call.setArity(call.arity() - 1);
                call.setKind(ASTKind::Call);
                func = call.child(0);
            }

            if (isTypeExpression(base) && call.kind() == ASTKind::CallMethod && call.child(0).kind() == ASTKind::Ident) {
                // This handles the case that we mistook a nested type
                // expression for a method call, i.e. Foo.Bar(x). In this
                // scenario, we turn the method name and base into a type, turn
                // the call into a normal call, then proceed.
                if (auto newAST = resolveTypeField(module, fixups, scope, base, call.child(0), call.pos())) {
                    call.setChild(1, *newAST);
                    for (u32 i = 0; i < call.arity() - 1; i ++)
                        call.setChild(i, call.child(i + 1));
                    call.setArity(call.arity() - 1);
                    call.setKind(ASTKind::Call);
                    func = call.child(0);
                } else {
                    // If we failed to resolve the type field, either we just
                    // reported an error, or this is *actually* a method call
                    // on an atom type instance. In either case, we just
                    // continue with the remaining logic.
                }
            }
        }
        if (!func)
            func = resolveChild(module, fixups, refTraits, call, 0, expectation);
        for (u32 i = 1; i < call.arity(); i ++)
            resolveChild(module, fixups, NoRefTraits, call, i, expectation);

        if (func.kind() == ASTKind::GetField && isTypeExpression(func.child(1))) {
            // This handles cases like x.i32(i32)(). First, x.i32(i32) resolves itself to a "field access",
            // accessing i32(i32) from x. This is nonsensical, but if the parent of that expression is a
            // call, we reach this condition, and can transform it into the correct i32(i32)(x) constructor
            // that we want it to be.
            // We still have to consider if this is a function type or a constructor though, because of
            // cases like x.i32(i32)(i32)().
            assert(call.kind() == ASTKind::Call); // CallMethod shouldn't be able to call a GetField.
            AST base = func.child(0);
            func = func.child(1);
            vec<AST> otherArgs;
            for (u32 i = 1; i < call.arity(); i ++)
                otherArgs.push(call.child(i));
            call = module->add(ASTKind::CallMethod, call.pos(), call.scope(), InvalidType, func, base, otherArgs);

            // Fall through to further processing...
        }

        if (isTypeExpression(func)) {
            Type type = evaluateType(module, fixups, scope, func);
            // Either a constructor or a function type. It's a function type if:
            //  - The call has one or more arguments, all of which are type expressions.
            //  - The call has zero arguments and we ExpectType.
            // Otherwise, it's a constructor.
            if (call.arity() == 1) { // No arguments case. Does not apply to methods - methods always have at least one argument.
                call.setKind(expectation == ExpectType ? ASTKind::FunType : ASTKind::Construct);
                if (call.kind() == ASTKind::FunType)
                    call.setType(module->funType(type));
                else {
                    call.setType(type);
                    call.setArity(0);
                }
                return call;
            }

            if (call.kind() == ASTKind::CallMethod && call.arity() == 2) {
                // This is the case where we have x.foo(). We don't support UFCS for function types
                // (i.e. no i32.i32() for the i32(i32) function type), so this must be a constructor.
                call.setChild(0, call.child(1));
                call.setArity(call.arity() - 1);
                call.setKind(ASTKind::Construct);
                call.setType(type);
                return call;
            }

            // Now we check if we have all type parameters, and consider ourselves a function type expression
            // if so. There's a weird bit of method-specific behavior here. Consider x.i32(i32)(). When we're
            // resolving the initial x.i32(i32), that seems kind of nonsensical - i32(x, i32). It is currently
            // invalid for a type parameter to occur in a constructor list generally. But we don't error right
            // away, because really, we want the parent expression to be seen as an i32(i32) constructor passed
            // x as a parameter. So, we skip over the first parameter for methods, and if the remaining args
            // would look like a function type, we actually turn ourselves into a GetField - and handle that for
            // Call at the start of this function.

            u32 firstArg = call.kind() == ASTKind::Call ? 1 : 2;

            bool hasAnyNonType = false;
            vec<Type> types;
            for (u32 i = firstArg; i < call.arity(); i ++) {
                resolveChild(module, fixups, NoRefTraits, call, i, ExpectValue);
                if (isTypeExpression(call.child(i))) {
                    Type type = evaluateType(module, fixups, scope, call.child(i));
                    if (type == Void)
                        assert(i == firstArg);
                    else
                        types.push(type);
                } else
                    hasAnyNonType = true;
            }
            if (hasAnyNonType) {
                for (u32 i = 1; i < call.arity(); i ++)
                    call.setChild(i - 1, call.child(i));
                call.setArity(call.arity() - 1);
                call.setKind(ASTKind::Construct);
                call.setType(type);
                return call;
            }

            if (call.kind() == ASTKind::CallMethod) {
                // This is really a dirty hack. We should check for this more responsibly.
                vec<AST> funTypeArgs;
                for (u32 i = 2; i < call.arity(); i ++)
                    funTypeArgs.push(call.child(i));
                AST funType = module->add(ASTKind::FunType, call.pos(), scope, module->funType(type, types), func, funTypeArgs);
                call.setKind(ASTKind::GetField);
                call.setChild(0, call.child(1));
                call.setChild(1, funType);
                call.trimChildrenTo(2);
                return call;
            }

            FunctionBuilder funType(module->types, type);
            funType.addParameters(types);
            call.setKind(ASTKind::FunType);
            call.setType(module->funType(funType));
            return call;
        }

        return call;
    }

    AST resolveStars(Module* module, Fixups& fixups, RefTraits refTraits, Scope* scope, AST ast, Expectation expectation) {
        AST lhs = resolveChild(module, fixups, NoRefTraits, ast, 0, ExpectValue);
        if (!isTypeExpression(lhs) && (lhs.kind() != ASTKind::GetField || !isTypeExpression(lhs.child(1)))) {
            // Not a type, so we just have to handle multiple stars. If there
            // multiple star operators (* or **) in the source, and both
            // operands are values not types, then:
            //  - The first star operator is the binary one.
            //  - All other star operators become dereference operators.
            // So x ** ** * y is (x ** (* (* (* y)))).
            computeScopes(module, scope, ast.child(1));
            AST rhs = resolveChild(module, fixups, NoRefTraits, ast, 1, ExpectValue);
            ASTKind binOp = ast.child(2).uintConst() == 2 ? ASTKind::Exp : ASTKind::Mul;
            for (u32 i = 3; i < ast.arity(); i ++) {
                for (u32 j = 0; j < ast.child(i).uintConst(); j ++)
                    rhs = module->add(ASTKind::Deref, ast.pos(), ast.scope(), InvalidType, rhs);
            }
            ast.setChild(1, rhs);
            ast.trimChildrenTo(2);
            ast.setKind(binOp);
            return ast;
        }

        // Now things get complicated.

        // First we preprocess our arguments. On the left, we default to the
        // lhs of the stars. But because of general dirty evil hacks, we may
        // need to extract the type from the right-hand side of a GetField,
        // so that we correctly identify a case like ptr.i32*(i32) as a
        // constructor called method-style.
        AST base = lhs;
        bool lhsIsField = lhs.kind() == ASTKind::GetField;
        if (lhsIsField)
            base = lhs.child(1);
        Type baseType = evaluateType(module, fixups, scope, base); // Should already be resolved.

        // We know we're going to be making a pointer, so we need to compute
        // the pointer type. If the lhs is a type, then the only valid parse
        // is for every star to be a pointer type operator. So we add layers
        // of pointers equal to the number of star operators.
        AST ptrNode = base;
        Type ptrType = baseType;
        for (u32 i = 2; i < ast.arity(); i ++) {
            for (u32 j = 0; j < ast.child(i).uintConst(); j ++) {
                ptrType = module->ptrType((i == ast.arity() - 1 && j == ast.child(i).uintConst() - 1 ? refTraits : NoRefTraits), ptrType);
                ptrNode = module->add(ASTKind::PtrType, ast.pos(), ast.scope(), ptrType, ptrNode);
            }
        }

        // On the right, if our rhs is a call, we separate out the function.
        // This is needed to handle cases like i32*[4](x) - [4](x) parses as
        // calling a list on x, which is nonsensical, but makes sense in the
        // context of i32*.
        AST right = ast.child(1);
        bool rhsIsCall = right.kind() == ASTKind::Call;
        vec<AST> calls;
        while (right.kind() == ASTKind::Call) {
            calls.push(right);
            right = right.child(0);
        }

        AST resultNode;
        Type resultType;

        switch (right.kind()) {
            case ASTKind::List:
                // Can be a slice, a la i32*[], or an array, a la i32*[4].
                computeScopes(module, ast.scope(), ast.child(1)); // We have to do this because we couldn't safely compute scopes here in the last pass.
                if (right.arity() == 0) {
                    resultType = module->sliceType(ptrType);
                    resultNode = module->add(ASTKind::SliceType, ast.pos(), ast.scope(), resultType, ptrNode);
                } else if (right.arity() == 1) {
                    assert(right.child(0).kind() == ASTKind::Unsigned);
                    auto size = right.child(0).uintConst();
                    assert(size >= 0 && size < 1ull << 32ull);
                    resultType = module->arrayType(ptrType, (u32)size);
                    resultNode = module->add(ASTKind::ArrayType, ast.pos(), ast.scope(), resultType, ptrNode, right.child(0));
                } else
                    unreachable("Malformed list in pointer type.");
                break;
            case ASTKind::Paren:
            case ASTKind::Tuple: {
                // Either a function type, a la i32*(i32) or i32*(i32, i32); or
                // a constructor call, like i32*(0)
                computeScopes(module, ast.scope(), ast.child(1)); // Like the previous case, we have to ensure the scopes are computed.
                vec<AST> children;
                vec<Type> childTypes;
                if (right.kind() == ASTKind::Paren) {
                    children.push(resolveChild(module, fixups, NoRefTraits, right, 0, ExpectValue));
                    if (isTypeExpression(right.child(0)))
                        childTypes.push(evaluateType(module, fixups, ast.scope(), right.child(0)));
                } else for (u32 i = 0; i < right.arity(); i ++) {
                    children.push(resolveChild(module, fixups, NoRefTraits, right, i, ExpectValue));
                    if (isTypeExpression(right.child(i)))
                        childTypes.push(evaluateType(module, fixups, ast.scope(), right.child(i)));
                }
                if (childTypes.size() == children.size() && children.size() > 0) {
                    // If all children are types, assume it's a function type.
                    resultType = module->funType(ptrType, childTypes);
                    resultNode = module->add(ASTKind::FunType, ast.pos(), ast.scope(), resultType, ptrNode, children);
                } else {
                    assert(childTypes.size() == 0); // Shouldn't have any types for a constructor.
                    vec<AST, 1> maybeFirst;
                    if (lhsIsField) // Method-style constructor call. If we do x.i32*(y), x is an argument.
                        maybeFirst.push(lhs.child(0));
                    resultType = ptrType;
                    resultNode = module->add(ASTKind::Construct, ast.pos(), ast.scope(), resultType, maybeFirst, children);
                    return resultNode;
                }
                break;
            }
            case ASTKind::Ident:
                // A decl. It's a variable decl if the rhs was just an ident, a
                // la i32* x. It's a function decl if the rhs was a call.
                if (rhsIsCall) {
                    AST call = calls.back();
                    vec<AST> arguments;
                    for (u32 i = 1; i < call.arity(); i ++) {
                        AST child = call.child(i);
                        if (child.kind() == ASTKind::NamedParameter) {
                            // Untyped argument with a default value.
                            arguments.push(module->add(ASTKind::VarDecl, child.pos(), InvalidScope, InvalidType, module->add(ASTKind::Missing), child.child(0), child.child(1)));
                        } else if (child.kind() == ASTKind::Ident) {
                            // Untyped argument with no default value.
                            arguments.push(module->add(ASTKind::VarDecl, call.pos(), InvalidScope, InvalidType, module->add(ASTKind::Missing), child, module->add(ASTKind::Missing)));
                        } else
                            arguments.push(child);
                    }
                    AST argsTuple = module->add(ASTKind::Tuple, ast.pos(), InvalidScope, InvalidType, arguments);
                    // If there was a raises clause or a body, we'd already
                    // know it was a fundecl at parse-time.
                    AST decl = module->add(ASTKind::FunDecl, ast.pos(), InvalidScope, InvalidType, ptrNode, right, argsTuple, module->add(ASTKind::Missing), module->add(ASTKind::Missing));
                    computeScopes(module, scope, decl);
                    return resolve(module, fixups, NoRefTraits, scope, none<AST>(), decl, ExpectValue);
                } else {
                    AST decl = module->add(ASTKind::VarDecl, ast.pos(), InvalidScope, InvalidType, ptrNode, right, module->add(ASTKind::Missing));
                    computeScopes(module, scope, decl);
                    return resolve(module, fixups, NoRefTraits, scope, none<AST>(), decl, ExpectValue);
                }
                break;
            default:
                unreachable("Invalid declarator '", right, "' in declaration.");
        }

        if (lhsIsField) {
            assert(isTypeExpression(resultNode)); // Should return early if this is a definition.
            lhs.setChild(1, resultNode);
            resultNode = lhs;
        }
        for (i64 i = i64(calls.size()) - 1; i >= 0; i --) {
            // Go back through any nested calls and resolve them. This handles
            // cases like i32*(i32)(x), where we need to first identify the
            // function type and then in the same expression identify the
            // constructor.
            calls[i].setChild(0, resultNode);
            resultNode = resolve(module, fixups, NoRefTraits, scope, none<AST>(), calls[i], ExpectValue);
        }
        return resultNode;
    }

    AST resolvePatternBinding(Module* module, Fixups& fixups, RefTraits refTraits, Scope* scope, Symbol name) {
        assert(!scope->findLocal(name));
        AST decl = module->add(ASTKind::VarDecl, {}, scope->index, InvalidType, module->add(ASTKind::Missing), Identifier(name), module->add(ASTKind::Missing));
        scope->add(VariableKind::Variable, decl, name);
        return resolve(module, fixups, refTraits, scope, none<AST>(), decl, ExpectValue);
    }

    AST resolvePattern(Module* module, Fixups& fixups, RefTraits refTraits, Scope* scope, AST pattern, bool allowSplat) {
        switch (pattern.kind()) {
            case ASTKind::Int:
            case ASTKind::Unsigned:
            case ASTKind::Float:
            case ASTKind::Bool:
            case ASTKind::Char:
                return pattern;

            case ASTKind::Splat:
                assert(allowSplat);
                if (!pattern.child(0).missing()) {
                    assert(pattern.child(0).kind() == ASTKind::Local
                        || pattern.child(0).kind() == ASTKind::Global
                        || pattern.child(0).kind() == ASTKind::Ident);
                    pattern.setChild(0, resolvePattern(module, fixups, NoRefTraits, scope, pattern.child(0), false));
                }
                return pattern;

            case ASTKind::Local:
                return resolvePatternBinding(module, fixups, refTraits, scope, pattern.varInfo(scope->function).name);
            case ASTKind::Global:
                return resolvePatternBinding(module, fixups, refTraits, scope, pattern.varInfo().name);
            case ASTKind::Ident:
                return resolvePatternBinding(module, fixups, refTraits, scope, pattern.symbol());

            case ASTKind::VarDecl:
                assert(pattern.child(2).missing()); // Can't initialize within a pattern.
                return pattern;

            case ASTKind::Construct:
                for (u32 i = 0; i < pattern.arity(); i ++)
                    pattern.setChild(i, resolvePattern(module, fixups, refTraits, scope, pattern.child(i), true));
                return pattern;

            case ASTKind::List:
                for (u32 i = 0; i < pattern.arity(); i ++)
                    pattern.setChild(i, resolvePattern(module, fixups, refTraits, scope, pattern.child(i), true));
                return pattern;

            case ASTKind::Tuple:
                for (u32 i = 0; i < pattern.arity(); i ++)
                    pattern.setChild(i, resolvePattern(module, fixups, refTraits, scope, pattern.child(i), true));
                return pattern;

            case ASTKind::Typename:
            case ASTKind::GlobalTypename:
            case ASTKind::TypeField:
            case ASTKind::PtrType:
            case ASTKind::SliceType:
            case ASTKind::ArrayType:
            case ASTKind::TupleType:
            case ASTKind::FunType:
            case ASTKind::OwnType:
            case ASTKind::UninitType:
            case ASTKind::GenericInst:
                return pattern; // Bare type expressions can be used to check if a value matches a specific type.

            default:
                unreachable("Found illegal node in pattern ", pattern, ".");
        }
    }

    AST resolveFieldnameChild(Module* module, Fixups& fixups, AST ast, u32 i, Expectation expectation) {
        auto scope = ast.scope();
        AST child = ast.child(i);
        if (child.kind() != ASTKind::Ident)
            return child;
        auto entry = module->naturalize(module->lookup(scope, child.symbol()));
        if (!entry)
            return child; // It's fine. It just means this child wasn't resolvable to a type.
        if (entry.kind() != VariableKind::Type)
            return child;
        if (entry.hasDecl())
            entry.setType(resolveTypeForDecl(module, fixups, NoRefTraits, entry.decl()));
        if (entry.isGlobal())
            ast.setChild(i, module->add(ASTKind::GlobalTypename, Global(entry.index())));
        else
            ast.setChild(i, module->add(ASTKind::Typename, Local(entry.index())));
        return ast.child(i);
    }

    AST resolveNew(Module* module, Fixups& fixups, RefTraits refTraits, AST ast) {
        if (ast.child(0).kind() == ASTKind::GetIndex) {
            AST base = resolveChild(module, fixups, NoRefTraits, ast.child(0), 0, ExpectType);
            if (isTypeExpression(base)) {
                // new T[n]
                ast.setKind(ASTKind::NewArray);
                ast.setType(module->sliceType(Own | Uninit, evaluateType(module, fixups, ast.scope(), base)));
                ast.setChild(0, resolveChild(module, fixups, refTraits, ast.child(0), 1, ExpectValue));
                return ast;
            }
        }
        AST child = resolveChild(module, fixups, NoRefTraits, ast, 0, ExpectValue);
        if (isTypeExpression(child)) {
            // new T
            ast.setType(evaluateType(module, fixups, ast.scope(), child));
            ast.setChild(0, module->add(ASTKind::Missing));
            ast.setType(module->ptrType(Own | Uninit, evaluateType(module, fixups, ast.scope(), child)));
            return ast;
        }

        if (child.kind() == ASTKind::Construct) {
            // new T(...)
            ast.setType(module->ptrType(Own, child.type()));
        }

        // new <expr>
        return ast;
    }

    bool validateCondition(AST ast, bool allowIs) {
        switch (ast.kind()) {
            case ASTKind::Is:
                return allowIs;
            case ASTKind::Paren:
                return validateCondition(ast.child(0), allowIs);
            case ASTKind::And:
                return validateCondition(ast.child(0), allowIs) && validateCondition(ast.child(1), allowIs);
            default:
                for (AST child : ast) {
                    if (!validateCondition(child, false))
                        return false;
                }
                return true;
        }
    }

    AST resolve(Module* module, Fixups& fixups, RefTraits refTraits, Scope* scope, maybe<AST> parent, AST ast, Expectation expectation) {
        switch (ast.kind()) {
            case ASTKind::Local:
            case ASTKind::Global:
            case ASTKind::Const:
            case ASTKind::GlobalConst:
            case ASTKind::Typename:
            case ASTKind::GlobalTypename:
            case ASTKind::GenericTypename:
                // Already resolved, somehow.
                return ast;
            case ASTKind::Missing:
                return ast;
            case ASTKind::Ident: {
                auto entry = module->naturalize(module->lookup(scope, ast.symbol()));
                return resolveIdentifier(module, fixups, scope, *parent, ast, entry);
            }
            case ASTKind::GetIndex: {
                AST lhs = resolveChild(module, fixups, refTraits, ast, 0, ExpectValue);
                if (isTypeExpression(lhs)) {
                    assert(ast.child(1).kind() == ASTKind::Unsigned);
                    auto size = ast.child(1).uintConst();
                    assert(size >= 0 && size < 1ull << 32ull);
                    ast.setKind(ASTKind::ArrayType);
                    ast.setType(module->arrayType(evaluateType(module, fixups, scope, lhs), u32(size)));
                } else if (lhs.kind() == ASTKind::GetField && isTypeExpression(lhs.child(1))) {
                    Type fieldType = evaluateType(module, fixups, scope, lhs.child(1));
                    assert(ast.child(1).kind() == ASTKind::Unsigned);
                    auto size = ast.child(1).uintConst();
                    assert(size >= 0 && size < 1ull << 32ull);
                    lhs.setChild(1, module->add(ASTKind::ArrayType, ast.pos(), ast.scope(), module->arrayType(fieldType, u32(size)), lhs.child(1), ast.child(1)));
                    return lhs;
                } else {
                    resolveChild(module, fixups, NoRefTraits, ast, 1, ExpectValue);
                }
                return ast;
            }
            case ASTKind::GetSlice: {
                resolveChild(module, fixups, refTraits, ast, 0, ExpectValue);
                if (!ast.child(1).missing())
                    resolveChild(module, fixups, refTraits, ast, 1, ExpectValue);
                if (!ast.child(2).missing())
                    resolveChild(module, fixups, refTraits, ast, 2, ExpectValue);
                return ast;
            }
            case ASTKind::GetIndices: {
                resolveChild(module, fixups, refTraits, ast, 0, ExpectValue);
                for (u32 i = 1; i < ast.arity(); i ++)
                    resolveChild(module, fixups, NoRefTraits, ast, i, ExpectValue);
                return ast;
            }
            case ASTKind::SliceType: {
                if (ast.typeIndex() != InvalidType)
                    return ast;
                AST lhs = resolveChild(module, fixups, NoRefTraits, ast, 0, ExpectType);
                if (isTypeExpression(lhs)) {
                    ast.setType(module->sliceType(refTraits, evaluateType(module, fixups, scope, lhs)));
                    return ast;
                } else if (lhs.kind() == ASTKind::GetField && isTypeExpression(lhs.child(1))) {
                    Type fieldType = evaluateType(module, fixups, scope, lhs.child(1));
                    lhs.setChild(1, module->add(ASTKind::SliceType, ast.pos(), ast.scope(), module->sliceType(fieldType), lhs.child(1)));
                    return lhs;
                }
                unreachable("lhs of slice type should be a type.");
            }
            case ASTKind::PtrType: {
                if (ast.typeIndex() != InvalidType)
                    return ast;
                resolveChild(module, fixups, NoRefTraits, ast, 0, ExpectType);
                assert(isTypeExpression(ast.child(0)));
                ast.setType(module->ptrType(refTraits, evaluateType(module, fixups, scope, ast.child(0))));
                return ast;
            }
            case ASTKind::OwnType:
            case ASTKind::UninitType: {
                if (ast.typeIndex() != InvalidType)
                    return ast;
                RefTraits traits = ast.kind() == ASTKind::OwnType ? Own : Uninit;
                return resolveChild(module, fixups, refTraits | traits, ast, 0, ExpectType);
            }
            case ASTKind::New:
                return resolveNew(module, fixups, refTraits, ast);
            case ASTKind::GetField: {
                resolveChild(module, fixups, refTraits, ast, 0, ExpectValue);
                if (ast.child(0).kind() == ASTKind::ResolvedNamespace) {
                    type_assert(ast.child(1).kind() == ASTKind::Ident);
                    auto result = ast.child(0).resolvedNamespace()->lookup(ast.child(1).symbol());
                    type_assert(result);
                    AST resolved = resolveIdentifier(module, fixups, scope, ast, ast.child(1), module->naturalize({ result.scope, result.index }));
                    return resolved;
                }

                if (ast.child(1).kind() == ASTKind::OwnType || ast.child(1).kind() == ASTKind::UninitType) {
                    AST resolvedField = resolveChild(module, fixups, refTraits, ast, 1, ExpectType);
                    assert(resolvedField.kind() == ASTKind::Construct); // Must be the case, for this to be valid.
                    vec<AST> nestedChildren;
                    for (AST child : resolvedField)
                        nestedChildren.push(child);
                    return module->add(ASTKind::Construct, resolvedField.pos(), resolvedField.scope(), resolvedField.type(), ast.child(0), nestedChildren);
                }

                if (isTypeExpression(ast.child(0))) {
                    if (auto typeField = resolveTypeField(module, fixups, scope, ast.child(0), ast.child(1), ast.pos()))
                        return *typeField;
                    // Otherwise, assume the base is meant to be interpreted
                    // as a value, i.e. it's an atom type. This is a little
                    // weird, but we might as well leave the error reporting to
                    // the typechecker.
                }
                resolveFieldnameChild(module, fixups, ast, 1, ExpectValue);
                return ast;
            }
            case ASTKind::GetFields: {
                vec<Type> types;
                vec<AST> children;
                bool foundAnyNonType = false;
                for (u32 i = 1; i < ast.arity(); i ++) {
                    AST child = resolveFieldnameChild(module, fixups, ast, i, ExpectValue);
                    if (!isTypeExpression(child)) {
                        foundAnyNonType = true;
                        break;
                    }
                    children.push(child);
                    types.push(evaluateType(module, fixups, scope, child));
                }
                if (foundAnyNonType) {
                    assert(types.size() == 0); // Should either be all values or all types.
                    resolveChild(module, fixups, refTraits, ast, 0, ExpectValue);
                    return ast;
                }
                ast.setKind(ASTKind::GetField);
                ast.trimChildrenTo(2);
                ast.setChild(1, module->add(ASTKind::TupleType, ast.pos(), ast.scope(), module->tupleType(types), children));
                return ast;
            }
            case ASTKind::Stars:
                return resolveStars(module, fixups, refTraits, scope, ast, expectation);
            case ASTKind::Case: {
                // We don't need a rule for match itself.
                AST pattern = resolveChild(module, fixups, refTraits, ast, 0, ExpectValue);
                if (!pattern.missing())
                    ast.setChild(0, resolvePattern(module, fixups, refTraits, ast.scope(), pattern, false));

                // Our scope should be fully populated now. So we can resolve the body.
                resolveChild(module, fixups, refTraits, ast, 1, ExpectValue);
                return ast;
            }
            case ASTKind::Is: {
                resolveChild(module, fixups, refTraits, ast, 0, ExpectValue);
                AST pattern = resolveChild(module, fixups, refTraits, ast, 1, ExpectValue);
                if (!pattern.missing())
                    ast.setChild(1, resolvePattern(module, fixups, refTraits, ast.scope(), pattern, false));
                return ast;
            }
            case ASTKind::AliasDecl:
            case ASTKind::NamedDecl:
            case ASTKind::NamedCaseDecl:
            case ASTKind::StructDecl:
            case ASTKind::StructCaseDecl:
            case ASTKind::UnionDecl:
            case ASTKind::UnionCaseDecl:
                resolveTypeForDecl(module, fixups, NoRefTraits, ast);
                return ast;
            case ASTKind::UseModule:
            case ASTKind::UseLocal:
                return module->add(ASTKind::Do, ast.pos(), ast.scope(), InvalidType);
            case ASTKind::VarDecl: {
                if (ast.child(0).missing())
                    ast.setType(module->varType());
                else {
                    resolveChild(module, fixups, refTraits, ast, 0, ExpectType);
                    assert(isTypeExpression(ast.child(0)));
                    ast.setType(evaluateType(module, fixups, scope, ast.child(0)));
                }
                if (ast.child(1).kind() == ASTKind::Ident) {
                    auto entry = module->lookup(scope, ast.child(1).symbol());
                    assert(entry);
                    entry.setType(ast.type());
                    resolveChild(module, fixups, refTraits, ast, 1, ExpectValue);
                } else if (!ast.child(1).missing() && ast.child(1).kind() != ASTKind::Local && ast.child(1).kind() != ASTKind::Global) {
                    // Must be some kind of pattern.
                    assert(!ast.child(2).missing() && ast.child(2).kind() != ASTKind::Uninit);
                    AST pattern = resolveChild(module, fixups, refTraits, ast, 1, ExpectValue);
                    resolvePattern(module, fixups, refTraits, scope, pattern, false);
                }
                if (ast.child(2).kind() != ASTKind::Uninit)
                    resolveChild(module, fixups, refTraits, ast, 2, ExpectValue);
                return ast;
            }
            case ASTKind::FunDecl: {
                vec<Type> argumentTypes;
                Function* function = ast.function();

                if (ast.child(0).missing())
                    function->isGeneric = true;

                // First we have to see if the function is generic.
                for (auto [i, a] : enumerate(ast.child(2))) switch (a.kind()) {
                    case ASTKind::AliasDecl: {
                        AST arg = a;
                        function->isGeneric = true;
                        arg.setType(module->varType());
                        auto entry = ast.scope()->findLocal(a.child(0).symbol());
                        assert(entry);
                        function->locals[entry.index].type = arg.typeIndex();
                        break;
                    }
                    case ASTKind::VarDecl:
                        if (a.child(0).missing() && a.child(2).missing()) {
                            AST arg = a;
                            auto entry = module->naturalize(module->lookup(ast.scope(), arg.child(1).symbol()));
                            if (!entry) {
                                // Must be a generic parameter declaration.
                                function->isGeneric = true;
                                computeScopes(module, ast.scope(), arg);
                            }
                            AST resolvedName = resolve(module, fixups, refTraits, ast.scope(), some<AST>(arg), arg.child(1), ExpectValue);
                            if (isTypeExpression(resolvedName)) {
                                arg.setChild(0, resolvedName);
                                arg.setChild(1, module->add(ASTKind::Missing));
                                break;
                            }
                        }
                        break;
                    default:
                        break;
                }

                // Get the receiver type if we're a method, before we clobber
                // the function's name.
                if (ast.child(1).kind() == ASTKind::GetField) {
                    setScopes(module, scope, ast.child(1));
                    AST receiver = resolve(module, fixups, NoRefTraits, scope, some<AST>(ast.child(1)), ast.child(1).child(0), ExpectType);
                    Type receiverType = evaluateType(module, fixups, scope, receiver);
                    ast.scope()->add(VariableKind::Variable, ast, receiverType.index, KeywordThis);
                    argumentTypes.push(receiverType);

                    if (receiverType.is<TypeKind::Pointer>())
                        receiverType = receiverType.as<TypeKind::Pointer>().elementType();
                    if (isNamed(receiverType.kind())) {
                        Scope* scope = getScope(receiverType);
                        for (const auto& e : scope->entries)
                            ast.scope()->add(VariableKind::ThisAccess, ast, e.key);
                    }
                }

                if (function->isGeneric) {
                    ast.setKind(ASTKind::GenericFunDecl);

                    // Since the name is now saved in the Function struct, we
                    // reuse this field of the FunDecl to store a linked list
                    // of all instantiations of this function. This roots them
                    // to the tree instead of needing to be stored separately.
                    ast.setChild(1, module->add(ASTKind::Missing));
                }

                for (auto [i, a] : enumerate(ast.child(2))) switch (a.kind()) {
                    case ASTKind::AliasDecl:
                        resolveChild(module, fixups, refTraits, a, 0, ExpectType);
                        break;
                    case ASTKind::ConstVarDecl:
                        unreachable("TODO: Implement constant, type, and generic parameters.");
                    default: {
                        bool alreadyResolved = false;
                        AST arg = a;
                        if (arg.kind() != ASTKind::VarDecl) {
                            AST resolvedArg = resolve(module, fixups, refTraits, arg.scope(), some<AST>(ast.child(2)), arg, ExpectType);
                            assert(isTypeExpression(resolvedArg) || resolvedArg.kind() == ASTKind::VarDecl);
                            if (resolvedArg.kind() == ASTKind::VarDecl)
                                arg = resolvedArg, alreadyResolved = true;
                            else
                                arg = module->add(ASTKind::VarDecl, resolvedArg.pos(), resolvedArg.scope(), InvalidType, resolvedArg, module->add(ASTKind::Missing), module->add(ASTKind::Missing));
                        }
                        AST resolvedArg = alreadyResolved ? arg : resolve(module, fixups, refTraits, ast.scope(), some<AST>(ast.child(2)), arg, ExpectValue);
                        ast.child(2).setChild(i, resolvedArg);
                        if (resolvedArg.child(1).kind() != ASTKind::Missing) {
                            auto& entry = resolvedArg.child(1).varInfo(ast.function());
                            entry.type = resolvedArg.type().index;
                        }
                        argumentTypes.push(resolvedArg.type());
                        break;
                    }
                }

                Type returnType;
                if (ast.child(0).missing())
                    returnType = module->varType();
                else {
                    AST ret = resolveChild(module, fixups, refTraits, ast, 0, ExpectType);
                    assert(isTypeExpression(ret));
                    returnType = evaluateType(module, fixups, scope, ret);
                }

                auto entry = module->lookup(scope, function->name);
                assert(entry);
                Type funcType = module->funType(returnType, argumentTypes);
                if (function->isGeneric)
                    function->genericType = funcType.index;

                entry.setType(funcType);
                ast.setType(funcType);
                function->typeIndex = funcType.index;

                if (!function->isGeneric) {
                    ast.setChild(1, module->add(ASTKind::ResolvedFunction, function));
                    resolveChild(module, fixups, refTraits, ast, 3, ExpectValue);

                    // We intentionally skipped resolving scopes earlier, since
                    // we didn't know if the function was generic or not.
                    computeScopes(module, ast.scope(), ast.child(4));
                    resolveChild(module, fixups, refTraits, ast, 4, ExpectValue);
                }

                return ast;
            }
            case ASTKind::AddressOf: {
                fixups.reportAddressOf(ast.node);
                resolveChild(module, fixups, refTraits, ast, 0, ExpectValue);
                return ast;
            }
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
                fixups.reportAssignment(ast.node);
                resolveChild(module, fixups, refTraits, ast, 0, ExpectValue);
                resolveChild(module, fixups, refTraits, ast, 1, ExpectValue);
                return ast;
            case ASTKind::PreIncr:
            case ASTKind::PostIncr:
            case ASTKind::PreDecr:
            case ASTKind::PostDecr:
                fixups.reportAssignment(ast.node);
                resolveChild(module, fixups, refTraits, ast, 0, ExpectValue);
                return ast;
            case ASTKind::Paren: {
                resolveChild(module, fixups, refTraits, ast, 0, ExpectValue);
                if (isTypeExpression(ast.child(0))) {
                    ast.setKind(ASTKind::TupleType);
                    ast.setType(module->tupleType(evaluateType(module, fixups, scope, ast.child(0))));
                }
                return ast;
            }
            case ASTKind::Length: {
                resolveChild(module, fixups, refTraits, ast, 0, ExpectValue);
                if (isTypeExpression(ast.child(0)))
                    ast.setKind(ASTKind::SizeOf);
                return ast;
            }
            case ASTKind::Construct: {
                // Most instances of Construct arise from constructor calls
                // resolved during this pass; those shouldn't flow to here, but
                // if they do, they will already have their type set - and we
                // shouldn't re-resolve them.
                if (ast.type() != InvalidType)
                    return ast;

                // The remaining case we have a Construct node that flows into
                // type resolution already formed is through the `as` operator,
                // and that's basically what the following impl is meant to
                // apply to.
                resolveChild(module, fixups, refTraits, ast, 0, ExpectType);
                type_assert(isTypeExpression(ast.child(0)));

                ast.setType(evaluateType(module, fixups, scope, ast.child(0)));
                for (u32 i = 0; i < ast.arity() - 1; i ++)
                    ast.setChild(i, resolve(module, fixups, refTraits, scope, some<AST>(ast), ast.child(i + 1), ExpectValue));
                ast.setArity(ast.arity() - 1);
                return ast;
            }
            case ASTKind::Tuple: {
                bool hasAnyType = false, hasAnyNonType = false;
                vec<Type> types;
                for (u32 i = 0; i < ast.arity(); i ++) {
                    resolveChild(module, fixups, refTraits, ast, i, ExpectValue);
                    if (isTypeExpression(ast.child(i))) {
                        hasAnyType = true;
                        types.push(evaluateType(module, fixups, scope, ast.child(i)));
                    } else
                        hasAnyNonType = true;
                }
                if (hasAnyType && !hasAnyNonType) {
                    ast.setKind(ASTKind::TupleType);
                    ast.setType(module->tupleType(types));
                }
                return ast;
            }
            case ASTKind::CallMethod:
                return resolveCall(module, fixups, refTraits, scope, ast, ExpectValue);
            case ASTKind::Call:
                return resolveCall(module, fixups, refTraits, scope, ast, ExpectValue);
            case ASTKind::If:
            case ASTKind::While: {
                // We do some validation to make sure if these constructs
                // contain `is` expressions, that they only appear in
                // conjunctions.
                type_assert(validateCondition(ast.child(0), true));
                for (u32 i : indices(ast))
                    resolveChild(module, fixups, refTraits, ast, i, ExpectValue);
                return ast;
            }
            case ASTKind::Break:
            case ASTKind::Continue:
                while (scope) {
                    if (scope->kind == ScopeKind::Block && module->node(scope->owner).kind() == ASTKind::While)
                        return ast;
                    assert(scope->kind == ScopeKind::Block);
                    scope = scope->parent;
                }
                unreachable("Should have found a loop enclosing break/continue.");
            default:
                if (ast.isLeaf()) // Nothing to do by default, we handle the necessary leaves above.
                    return ast;
                for (u32 i : indices(ast))
                    resolveChild(module, fixups, refTraits, ast, i, ExpectValue);
                return ast;
        }
    }

    void resolveLateUses(Module* module, Fixups& fixups) {
        for (const LateUse& use : fixups.lateUses) {
            Scope* scope = module->scopes[use.scope];
            AST ast = module->node(use.node);
            for (u32 i = 0; i < ast.arity(); i ++) if (ast.child(i).kind() == ASTKind::Ident) {
                auto entry = module->naturalize(module->lookup(scope, ast.symbol()));
                if (!entry)
                    unreachable("Undefined variable '", module->str(ast.symbol()), "'.");
                switch (entry.kind()) {
                    case VariableKind::Variable:
                    case VariableKind::Function:
                        if (entry.isGlobal())
                            ast.setChild(i, module->add(ASTKind::Global, Global(entry.index())));
                        else
                            ast.setChild(i, module->add(ASTKind::Local, Local(entry.index())));
                    default:
                        unreachable("Late defs should be variables or functions.");
                }
            }
        }
    }

    void resolveAccessChains(Module* module, AST ast) {
        switch (ast.kind()) {
            case ASTKind::GetField:
            case ASTKind::GetFields:
            case ASTKind::GetSlice:
            case ASTKind::GetIndex:
            case ASTKind::GetIndices:
                ast.setChild(0, fixupAccessBase(module, ast.child(0)));
                break;
            default:
                break;
        }
        for (u32 i : indices(ast))
            resolveAccessChains(module, ast.child(i));
    }

    void resolveAssignments(Module* module, Fixups& fixups) {
        for (NodeIndex n : fixups.assignments) {
            AST ast = module->node(n);
            AST base = ast.child(0);
            AST value;
            if (ast.arity() > 1)
                value = ast.child(1);
            if (ast.kind() == ASTKind::Assign) {
                switch (base.kind()) {
                    case ASTKind::GetIndex:
                        module->replace(ast, ASTKind::SetIndex, base.child(0), base.child(1), value);
                        break;
                    case ASTKind::GetSlice:
                        module->replace(ast, ASTKind::SetSlice, base.child(0), base.child(1), base.child(2), value);
                        break;
                    case ASTKind::GetIndices: {
                        vec<AST> indices;
                        for (AST child : base.children(1))
                            indices.push(child);
                        module->replace(ast, ASTKind::SetIndices, base.child(0), indices, value);
                        break;
                    }
                    case ASTKind::GetField:
                        module->replace(ast, ASTKind::SetField, base.child(0), base.child(1), value);
                        break;
                    case ASTKind::GetFields: {
                        vec<AST> fields;
                        for (AST child : base.children(1))
                            fields.push(child);
                        module->replace(ast, ASTKind::SetFields, base.child(0), fields, value);
                        break;
                    }
                    case ASTKind::Deref:
                        module->replace(ast, ASTKind::Store, base.child(0), value);
                        break;
                    default:
                        // We should only be doing direct assignment on variables.
                        assert(base.kind() == ASTKind::Local || base.kind() == ASTKind::Global);
                        break;
                }
            } else {
                // The rest of these are complicated, so we cheat a little bit.
                // For pre/post increment/decrement, and compound assignment,
                // we always take the address. Optimizing back out of memory
                // operations can be done at the codegen level.
                if (base.kind() == ASTKind::Deref)
                    ast.setChild(0, base.child(0));
                else {
                    ast.setChild(0, module->add(ASTKind::AddressOf, ast.pos(), ast.scope(), InvalidType, base));
                    fixups.reportAddressOf(ast.child(0).node);
                }
            }
        }
    }

    void resolveAddressOfs(Module* module, Fixups& fixups) {
        for (NodeIndex n : fixups.addressOfs) {
            AST ast = module->node(n);
            AST base = ast.child(0);
            switch (base.kind()) {
                case ASTKind::GetIndex:
                    base.setPos(ast.pos());
                    base.setKind(ASTKind::AddrIndex);
                    module->replace(ast, base);
                    break;
                case ASTKind::GetIndices:
                    base.setPos(ast.pos());
                    base.setKind(ASTKind::AddrIndices);
                    module->replace(ast, base);
                    break;
                case ASTKind::GetField:
                    base.setPos(ast.pos());
                    base.setKind(ASTKind::AddrField);
                    module->replace(ast, base);
                    break;
                case ASTKind::GetFields:
                    base.setPos(ast.pos());
                    base.setKind(ASTKind::AddrFields);
                    module->replace(ast, base);
                    break;
                default:
                    break;
            }
        }
    }

    void validateResolution(Module* module, Function* fn, maybe<AST> parent, AST ast) {
        switch (ast.kind()) {
            case ASTKind::Ident:
                unreachable("Found unresolved identifier node '", ast, "' in ", *parent, ".");
            case ASTKind::Stars:
                unreachable("Found stars node '", ast, "', should have broken this into subexpressions.");
            case ASTKind::GlobalTypename:
                assert(ast.typeIndex(module) != InvalidType);
                break;
            case ASTKind::Typename:
                assert(fn);
                assert(ast.typeIndex(fn) != InvalidType);
                break;
            case ASTKind::VarDecl:
                assert(ast.typeIndex() != InvalidType);
                break;
            case ASTKind::FunDecl:
                assert(ast.typeIndex() != InvalidType);
                assert(ast.type().is<TypeKind::Function>());
                fn = ast.scope()->function;
                break;
            case ASTKind::Construct:
                assert(ast.typeIndex() != InvalidType);
                break;
            case ASTKind::AliasDecl:
                assert(ast.typeIndex() != InvalidType);
                break;
            case ASTKind::NamedDecl:
                assert(ast.typeIndex() != InvalidType);
                assert(ast.type().is<TypeKind::Named>());
                break;
            case ASTKind::NamedCaseDecl:
                assert(ast.typeIndex() != InvalidType);
                assert(ast.type().is<TypeKind::Named>() || ast.type().is<TypeKind::Struct>());
                break;
            case ASTKind::StructDecl:
            case ASTKind::StructCaseDecl:
                assert(ast.typeIndex() != InvalidType);
                assert(ast.type().is<TypeKind::Struct>() || isAtom(ast.type()));
                break;
            case ASTKind::UnionDecl:
            case ASTKind::UnionCaseDecl:
                assert(ast.typeIndex() != InvalidType);
                assert(ast.type().is<TypeKind::Union>());
                break;
            case ASTKind::GetField:
            case ASTKind::AddrField:
            case ASTKind::EnsureAddrField:
                validateResolution(module, fn, some<AST>(ast), ast.child(0));
                assert(ast.child(1).kind() == ASTKind::Ident);
                return;
            case ASTKind::GetFields:
            case ASTKind::AddrFields:
            case ASTKind::EnsureAddrFields:
                validateResolution(module, fn, some<AST>(ast), ast.child(0));
                for (u32 i = 1; i < ast.arity(); i ++)
                    assert(ast.child(i).kind() == ASTKind::Ident);
                return;
            case ASTKind::SetField:
                validateResolution(module, fn, some<AST>(ast), ast.child(0));
                assert(ast.child(1).kind() == ASTKind::Ident);
                validateResolution(module, fn, some<AST>(ast), ast.child(2));
                return;
            case ASTKind::SetFields:
                validateResolution(module, fn, some<AST>(ast), ast.child(0));
                for (u32 i = 1; i < ast.arity() - 1; i ++)
                    assert(ast.child(i).kind() == ASTKind::Ident);
                validateResolution(module, fn, some<AST>(ast), ast.child(ast.arity() - 1));
                return;
            case ASTKind::GenericFunDecl:
                return;
            default:
                break;
        }
        for (AST child : ast)
            validateResolution(module, fn, some<AST>(ast), child);
    }

    void validateResolution(Artifact* artifact) {
        assert(artifact->kind == ArtifactKind::ResolvedAST);
        Module* module = artifact->as<Module>();
        validateResolution(module, nullptr, none<AST>(), module->getTopLevel());
    }

    AST resolveNode(Scope* scope, AST parent, AST ast) {
        Module* module = ast.module;
        Fixups fixups;
        AST resolvedNode = resolve(module, fixups, NoRefTraits, scope, some<AST>(parent), ast, ExpectValue);
        resolveLateUses(module, fixups);
        resolveAccessChains(module, ast);
        resolveAssignments(module, fixups);
        resolveAddressOfs(module, fixups);
        return resolvedNode;
    }

    NOINLINE Artifact* resolveNamesAndTypes(Artifact* artifact) {
        assert(artifact->kind == ArtifactKind::ScopedAST);

        Module* module = artifact->as<Module>();
        module->nodeTypes.expandTo(module->ast.size(), InvalidType);
        Fixups fixups;
        resolve(module, fixups, NoRefTraits, module->getTopLevel().scope(), none<AST>(), module->getTopLevel(), ExpectValue);
        resolveLateUses(module, fixups);
        resolveAccessChains(module, module->getTopLevel());
        resolveAssignments(module, fixups);
        resolveAddressOfs(module, fixups);
        artifact->update(ArtifactKind::ResolvedAST, module);
        if UNLIKELY(config::printResolvedTree)
            module->print(module->compilation), println();
        if UNLIKELY(config::validateResolution)
            validateResolution(artifact);
        return artifact;
    }
}