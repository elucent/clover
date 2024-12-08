#ifndef CLOVER_CLI_JIT_RUNTIME_H
#define CLOVER_CLI_JIT_RUNTIME_H

#include "rt/def.h"
#include "util/sym.h"

struct Assembly;

namespace clover {
    struct Symbol;

    struct JITRuntimeShims {
        Assembly* assembly;

        JITRuntimeShims(SymbolTable& symtab);
        ~JITRuntimeShims();

        void define(Symbol name, void* ptr);
        void define(const_slice<i8> name, void* ptr);
        void define(const i8* name, void* ptr);
    };

    void initializeJITRuntime(JITRuntimeShims& shims);
}

#endif