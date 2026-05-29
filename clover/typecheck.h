#ifndef CLOVER_TYPECHECK_H
#define CLOVER_TYPECHECK_H

#include "clover/compilation.h"
#include "clover/type.h"

namespace clover {
    struct Function;
    struct ChangePosition;

    Type naturalSignedType(Module* module, i64 value);
    Type naturalUnsignedType(Module* module, u64 value);
    Type naturalType(AST ast);
    Type typeOf(Function* function, AST ast);
    Type typeOf(AST ast, u32 i);
    Type typeOf(AST ast);
    Type typeOf(ChangePosition pos);

    Type evaluateType(Module* module, Function* function, AST ast);
    NOINLINE Artifact* inferAndCheckTypes(Artifact* artifact);
}

#endif