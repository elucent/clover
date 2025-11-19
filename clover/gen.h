#ifndef CLOVER_GEN_H
#define CLOVER_GEN_H

#include "clover/compilation.h"
#include "jasmine/capi.h"

namespace clover {
    NOINLINE Artifact* generateJasmine(Artifact* artifact, u32 optimizationLevel);
    NOINLINE Artifact* emitAssembly(Artifact* artifact, u32 optimizationLevel);
    JasmineExecutable load(Artifact* artifact);
    JasmineAssembly takeAssembly(Artifact* artifact);
    JasmineAssembly createEntrypoint(Compilation* compilation);
    void writeELF(Artifact* artifact, const_slice<i8> path);
}

#endif