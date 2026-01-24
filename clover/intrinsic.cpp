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
        intrinsic.transform = [](Module* module, Pos pos, Scope* scope, const_slice<ASTWord> params, TypeIndex returnType, const_slice<TypeIndex> paramTypes) -> AST {
            assert(params.size() == 1);
            return module->add(ASTKind::GetSlice, pos, scope, paramTypes[0], params[0], Constant::IntConst(1), Missing);
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
        intrinsic.transform = [](Module* module, Pos pos, Scope* scope, const_slice<ASTWord> params, TypeIndex returnType, const_slice<TypeIndex> paramTypes) -> AST {
            assert(params.size() == 1);
            return module->add(ASTKind::GetIndex, pos, scope, returnType, params[0], Constant::IntConst(0));
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
        intrinsic.transform = [](Module* module, Pos pos, Scope* scope, const_slice<ASTWord> params, TypeIndex returnType, const_slice<TypeIndex> paramTypes) -> AST {
            assert(params.size() == 1);
            return module->add(
                ASTKind::Equal, pos, scope, module->boolType(),
                Constant::IntConst(0),
                module->add(ASTKind::Length, pos, scope, module->u64Type(), params[0])
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
        intrinsic.transform = [](Module* module, Pos pos, Scope* scope, const_slice<ASTWord> params, TypeIndex returnType, const_slice<TypeIndex> paramTypes) -> AST {
            if (params[0].isRef())
                return module->node(params[0].child);
            else
                return AST(module, params[0]);
        };
        return new Function(compilation, type.index, MethodIter, true, intrinsic);
    }

    void addIntrinsicsTo(Scope* scope) {
        Compilation* compilation = scope->module->compilation;
        scope->addFunction(VariableKind::Function, createSliceNext(compilation));
        scope->addFunction(VariableKind::Function, createSliceDone(compilation));
        scope->addFunction(VariableKind::Function, createSliceRead(compilation));
        scope->addFunction(VariableKind::Function, createSliceIter(compilation));
    }
}