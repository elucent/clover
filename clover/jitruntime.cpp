#include "asm/arch.h"
#include "util/sym.h"
#include "clover/jitruntime.h"
#include "clover/compilation.h"
#include "jasmine/mod.h"
#include "asm/arch.h"

namespace clover {
    JITRuntimeShims::JITRuntimeShims(SymbolTable& symtab):
        assembly(new Assembly(symtab)) {}

    JITRuntimeShims::~JITRuntimeShims() {
        delete assembly;
    }

    using ASM = DefaultTarget;

    void JITRuntimeShims::define(Symbol name, void* ptr) {
        mreg tmp = ASM::caller_saved_gps().next();

        ASM::global(*assembly, Label::fromSym(name.symbol));
        ASM::lc(*assembly, GP(tmp), Imm64(bitcast<i64>(ptr)));
        ASM::br(*assembly, GP(tmp));
    }

    void JITRuntimeShims::define(const_slice<i8> name, void* ptr) {
        define(assembly->symtab[name], ptr);
    }

    void JITRuntimeShims::define(const i8* name, void* ptr) {
        define(assembly->symtab[name], ptr);
    }

    extern u8* memory_alloc(u64) ASMLABEL("memory.alloc");
    extern void memory_free(void*) ASMLABEL("memory.free");
    extern i64 math_ipow(i64, i64) ASMLABEL("math.pow(i64)");
    extern u64 math_upow(u64, u64) ASMLABEL("math.pow(u64)");
    extern f64 math_fpow(f64, f64) ASMLABEL("math.pow(f64)");

    void initializeJITRuntime(JITRuntimeShims& shims) {
        shims.define(BuiltinMemoryAlloc, (void*)memory_alloc);
        shims.define(BuiltinMemoryFree, (void*)memory_free);
        shims.define(BuiltinMathIpow, (void*)math_ipow);
        shims.define(BuiltinMathUpow, (void*)math_upow);
        shims.define(BuiltinMathFpow, (void*)math_fpow);
    }
}
