#ifndef BASIL_JASMINE_ARCH_AMD64_H
#define BASIL_JASMINE_ARCH_AMD64_H

#include "jasmine/arch.h"

struct AMD64Target {
    static constexpr mreg 
        RAX = 0, RCX = 1, RDX = 2, RBX = 3, RSP = 4, RBP = 5, RSI = 6, RDI = 7,
        R8 = 8, R9 = 9, R10 = 10, R11 = 11, R12 = 12, R13 = 13, R14 = 14, R15 = 15,
        XMM0 = 0, XMM1 = 1, XMM2 = 2, XMM3 = 3, XMM4 = 4, XMM5 = 5, XMM6 = 6, XMM7 = 7,
        XMM8 = 8, XMM9 = 9, XMM10 = 10, XMM11 = 11, XMM12 = 12, XMM13 = 13, XMM14 = 14, XMM15 = 15;
    static const mreg GP_REGS[14], FP_REGS[16];
    static constexpr const i8* GP_REG_NAMES[16] = {
        "rax", "rcx", "rdx", "rbx", "rsp", "rbp", "rsi", "rdi", 
        "r8", "r9", "r10", "r11", "r12", "r13", "r14", "r15"
    };
    static constexpr const i8* FP_REG_NAMES[16] = {
        "xmm0", "xmm1", "xmm2", "xmm3", "xmm4", "xmm5", "xmm6", "xmm7", 
        "xmm8", "xmm9", "xmm10", "xmm11", "xmm12", "xmm13", "xmm14", "xmm15" 
    };

    static inline const_slice<mreg> gpregs() {
        return { GP_REGS, 14 };
    }

    static inline const_slice<mreg> fpregs() {
        return { FP_REGS, 16 };
    }

    static inline const_slice<i8> gpreg_name(mreg r) {
        return { GP_REG_NAMES[r], cidx(GP_REG_NAMES[r], 0) };
    }

    static inline const_slice<i8> fpreg_name(mreg r) {
        return { FP_REG_NAMES[r], cidx(FP_REG_NAMES[r], 0) };
    }

    static inline u32 primsize(typeidx t) {
        static constexpr u32 TYPE_SIZES[14] = {
            1, 2, 4, 8, // I8 - I64
            8, 8,       // IWORD, PTR
            4, 8,       // F32, F64
            1, 2, 4, 8, // U8 - U64
            8, 8        // UWORD, REF
        };
        return TYPE_SIZES[-t - 1];
    }

    static inline u32 primalign(typeidx t) {
        return primsize(t);
    }

    static void lower(const Function& fn, bytebuf<arena>& buf);
};

struct AMD64LinuxTarget : public AMD64Target {
    static const mreg GP_ARGS[6], FP_ARGS[8];
    static const TargetDesc DESC;

    static Placement place_ret(const TypeTable& tab, typeidx t);
    static Placement place_arg(const TypeTable& tab, typeidx t, UsageState usage);
};

struct AMD64DarwinTarget : public AMD64LinuxTarget {};

#endif