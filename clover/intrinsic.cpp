#include "clover/intrinsic.h"
#include "clover/ast.h"
#include "clover/type.h"

namespace clover {
    Function* createSliceNext(Compilation* compilation) {
        auto types = compilation->types;
        Type var = types->var();
        Type type = FunctionBuilder(types, types->encode<TypeKind::Slice>(NoRefTraits, var), types->encode<TypeKind::Slice>(NoRefTraits, var)).build(types);

        Intrinsic intrinsic;
        intrinsic.typeFactory = [](InferenceContext& ctx, Module* module) -> TypeIndex {
            Type var = module->varType();
            return module->funType(module->sliceType(NoRefTraits, var), module->sliceType(NoRefTraits, var)).index;
        };
        intrinsic.transform = [](Module* module, IndexPair<u32, u32> origin, Scope* scope, const_slice<ASTWord> params, TypeIndex returnType, const_slice<TypeIndex> paramTypes) -> AST {
            assert(params.size() == 1);
            return module->add(ASTKind::GetSlice, origin, scope, paramTypes[0], params[0], Constant::IntConst(1), Missing);
        };
        return new Function(compilation, type.index, MethodNext, true, intrinsic);
    }

    Function* createSliceRead(Compilation* compilation) {
        auto types = compilation->types;
        Type var = types->var();
        Type type = FunctionBuilder(types, var, types->encode<TypeKind::Slice>(NoRefTraits, var)).build(types);

        Intrinsic intrinsic;
        intrinsic.typeFactory = [](InferenceContext& ctx, Module* module) -> TypeIndex {
            Type var = module->varType();
            return module->funType(var, module->sliceType(NoRefTraits, var)).index;
        };
        intrinsic.transform = [](Module* module, IndexPair<u32, u32> origin, Scope* scope, const_slice<ASTWord> params, TypeIndex returnType, const_slice<TypeIndex> paramTypes) -> AST {
            assert(params.size() == 1);
            return module->add(ASTKind::GetIndex, origin, scope, returnType, params[0], Constant::IntConst(0));
        };
        return new Function(compilation, type.index, MethodRead, true, intrinsic);
    }

    Function* createSliceDone(Compilation* compilation) {
        auto types = compilation->types;
        Type var = types->var();
        Type type = FunctionBuilder(types, Bool, types->encode<TypeKind::Slice>(NoRefTraits, var)).build(types);

        Intrinsic intrinsic;
        intrinsic.typeFactory = [](InferenceContext& ctx, Module* module) -> TypeIndex {
            Type var = module->varType();
            return module->funType(Bool, module->sliceType(NoRefTraits, var)).index;
        };
        intrinsic.transform = [](Module* module, IndexPair<u32, u32> origin, Scope* scope, const_slice<ASTWord> params, TypeIndex returnType, const_slice<TypeIndex> paramTypes) -> AST {
            assert(params.size() == 1);
            return module->add(
                ASTKind::Equal, scope, module->boolType(),
                Constant::IntConst(0),
                module->add(ASTKind::Length, origin, scope, module->u64Type(), params[0])
            );
        };
        return new Function(compilation, type.index, MethodDone, true, intrinsic);
    }

    Function* createSliceIter(Compilation* compilation) {
        auto types = compilation->types;
        Type var = types->var();
        Type type = FunctionBuilder(types, types->encode<TypeKind::Slice>(NoRefTraits, var), types->encode<TypeKind::Slice>(NoRefTraits, var)).build(types);

        Intrinsic intrinsic;
        intrinsic.typeFactory = [](InferenceContext& ctx, Module* module) -> TypeIndex {
            Type var = module->varType();
            return module->funType(module->sliceType(NoRefTraits, var), module->sliceType(NoRefTraits, var)).index;
        };
        intrinsic.transform = [](Module* module, IndexPair<u32, u32> origin, Scope* scope, const_slice<ASTWord> params, TypeIndex returnType, const_slice<TypeIndex> paramTypes) -> AST {
            if (params[0].isRef())
                return module->node(params[0].child);
            else
                return AST(module, params[0]);
        };
        return new Function(compilation, type.index, MethodIter, true, intrinsic);
    }

    Function* createSliceContains(Compilation* compilation) {
        auto types = compilation->types;
        Type var = types->var();
        Type type = FunctionBuilder(types, types->get(Bool), types->encode<TypeKind::Slice>(NoRefTraits, var), var).build(types);

        Intrinsic intrinsic;
        intrinsic.typeFactory = [](InferenceContext& ctx, Module* module) -> TypeIndex {
            Type var = module->varType();
            return module->funType(module->boolType(), module->sliceType(NoRefTraits, var), var).index;
        };
        intrinsic.transform = [](Module* module, IndexPair<u32, u32> origin, Scope* scope, const_slice<ASTWord> params, TypeIndex returnType, const_slice<TypeIndex> paramTypes) -> AST {
            IndexedAST idx = scope->function
                ? module->add(ASTKind::Local, scope->function->addTemp(U64)).positionless()
                : module->add(ASTKind::Global, module->addTemp(U64)).positionless();
            IndexedAST size = scope->function
                ? module->add(ASTKind::Local, scope->function->addTemp(U64)).positionless()
                : module->add(ASTKind::Global, module->addTemp(U64)).positionless();
            IndexedAST result = scope->function
                ? module->add(ASTKind::Local, scope->function->addTemp(Bool)).positionless()
                : module->add(ASTKind::Global, module->addTemp(Bool)).positionless();
            return module->add(ASTKind::Do, origin, scope, Bool,
                module->add(ASTKind::VarDecl, origin, scope, U64, module->add(ASTKind::Missing).positionless(), idx, module->add(ASTKind::Unsigned, Constant::UnsignedConst(0)).positionless()),
                module->add(ASTKind::VarDecl, origin, scope, U64, module->add(ASTKind::Missing).positionless(), size,
                    module->add(ASTKind::Length, origin, scope, U64, params[0])),
                module->add(ASTKind::VarDecl, origin, scope, Bool, module->add(ASTKind::Missing).positionless(), result, module->add(ASTKind::Bool, Constant::BoolConst(false))),
                module->add(ASTKind::While, origin, scope, Void,
                    module->add(ASTKind::Less, origin, scope, paramTypes[1], idx, size),
                    module->add(ASTKind::IfElse, origin, scope, Void,
                        module->add(ASTKind::Equal, origin, scope, paramTypes[1],
                            module->add(ASTKind::GetIndex, origin, scope, paramTypes[1], params[0], idx),
                            params[1]),
                        module->add(ASTKind::Do, origin, scope, Void,
                            module->add(ASTKind::Assign, origin, scope, Void, result, module->add(ASTKind::Bool, Constant::BoolConst(true)).positionless()),
                            module->add(ASTKind::Break, origin, scope, Void)),
                        module->add(ASTKind::AddEq, origin, scope, Void, module->add(ASTKind::AddressOf, origin, scope, module->ptrType(U64), idx).reconstituteOrigin(), module->add(ASTKind::Unsigned, Constant::UnsignedConst(1)).positionless()))
                ),
                result
            );
        };
        return new Function(compilation, type.index, MethodContains, true, intrinsic);
    }

    void addIntrinsicsTo(Scope* scope) {
        Compilation* compilation = scope->module->compilation;
        scope->addFunction(VariableKind::Function, createSliceNext(compilation));
        scope->addFunction(VariableKind::Function, createSliceDone(compilation));
        scope->addFunction(VariableKind::Function, createSliceRead(compilation));
        scope->addFunction(VariableKind::Function, createSliceIter(compilation));
        scope->addFunction(VariableKind::Function, createSliceContains(compilation));
    }
}