#ifndef CLOVER_TYPECHECK_H
#define CLOVER_TYPECHECK_H

#include "clover/compilation.h"
#include "clover/type.h"

namespace clover {
    struct Function;

    Type naturalSignedType(Module* module, i64 value);
    Type naturalUnsignedType(Module* module, u64 value);
    Type evaluateType(Module* module, Function* function, AST ast);
    NOINLINE Artifact* inferAndCheckTypes(Artifact* artifact);
}

#endif