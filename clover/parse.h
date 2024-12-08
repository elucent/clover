#ifndef CLOVER_PARSE_H
#define CLOVER_PARSE_H

#include "clover/compilation.h"
#include "clover/ast.h"

namespace clover {
    NOINLINE Artifact* parseAsSexp(Artifact* artifact);
    NOINLINE Artifact* parse(Artifact* artifact);
}

#endif