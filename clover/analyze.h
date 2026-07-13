#ifndef CLOVER_ANALYZE_H
#define CLOVER_ANALYZE_H

#include "clover/compilation.h"

namespace clover {
    struct ParameterConstraint {
        enum Kind : u32 {
            Outlives
        };
        Kind kind : 2;
        u32 other : 30;
    };

    NOINLINE Artifact* analyze(Artifact* artifact);
}

#endif