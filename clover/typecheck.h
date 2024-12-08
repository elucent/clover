#ifndef CLOVER_TYPECHECK_H
#define CLOVER_TYPECHECK_H

#include "clover/compilation.h"
#include "clover/type.h"

namespace clover {
    struct Function;

    Type evaluateType(Module* module, Function* function, AST ast);
    NOINLINE Artifact* inferAndCheckTypes(Artifact* artifact);
}

#endif