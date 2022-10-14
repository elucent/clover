#include "jasmine/arch/amd64.h"
#include "jasmine/insn.h"

const mreg AMD64Target::GP_REGS[14] = { RAX, RCX, RDX, RSI, RDI, RBX, R8, R9, R10, R11, R12, R13, R14, R15 };
const mreg AMD64Target::FP_REGS[16] = { XMM0, XMM1, XMM2, XMM3, XMM4, XMM5, XMM6, XMM7, XMM8, XMM9, XMM10, XMM11, XMM12, XMM13, XMM14, XMM15 };

const mreg AMD64LinuxTarget::GP_ARGS[6] = { RDI, RSI, RDX, RCX, R8, R9 };
const mreg AMD64LinuxTarget::FP_ARGS[8] = { XMM0, XMM1, XMM2, XMM3, XMM4, XMM5, XMM6, XMM7 };
const TargetDesc AMD64LinuxTarget::DESC = TargetDesc(OS_LINUX, ARCH_AMD64);

Placement AMD64LinuxTarget::place_ret(const TypeTable& tab, typeidx t) {
    if (t < 0) switch (i8(t)) {
        case T_F32:
        case T_F64:
            return {fpreg_slot(XMM0), {0, 1, 0}};
        default:
            return {gpreg_slot(RAX), {0, 0, 0}};
    }
    const Type& type = tab.types[t];
    if (type.size > 16) return {gpreg_slot(RDI), {1, 0, type.size}};
    else if (type.size <= 8) {
        if (type.flags & TF_HAS_INT) return {gpreg_slot(RAX), {0, 0, 0}};
        else return {fpreg_slot(XMM0), {0, 1, 0}};
    }
    else if (type.kind == TK_ARR) {
        if (type.nelts == 1) return place_ret(tab, type.elt);
    }
    else {
        if (type.len == 1) return place_ret(tab, type.members[0]);
        bool isfp[2] = {true, true};
        bool ismem = false;
        u32 bytes = 0;
        for (u32 i = 0; i < type.len && !ismem; i ++) {
            typeidx m = type.members[i];
            if (m != T_F32 && m != T_F64 && (m < 0 || tab.types[m].flags & TF_HAS_INT)) isfp[bytes >> 3] = false;
            u32 align = m < 0 ? primalign(m) : tab.types[m].align;
            if (bytes & align - 1) ismem = true;
            bytes += m < 0 ? primsize(m) : tab.types[m].size;
        }
        if (ismem) return {gpreg_slot(RDI), {1, 0, type.size}};
    }
}

Placement AMD64LinuxTarget::place_arg(const TypeTable& tab, typeidx t, UsageState usage) {
    if (t < 0) switch (i8(t)) {
        case T_F32:
        case T_F64:
            return usage.n_fpregs_used >= 8 ? Placement{stack_slot(usage.stack_used), {0, 0, primsize(t)}}
                : Placement{fpreg_slot(FP_ARGS[usage.n_fpregs_used]), {0, 1, 0}};
        default:
            return usage.n_gpregs_used >= 6 ? Placement{stack_slot(usage.stack_used), {0, 0, primsize(t)}}
                : Placement{gpreg_slot(GP_ARGS[usage.n_gpregs_used]), {1, 0, 0}};
    }
    const Type& type = tab.types[t];
    if (type.size > 16) return {stack_slot(usage.stack_used), {0, 0, type.size}};
    else if (type.size <= 8) {
        if (type.flags & TF_HAS_INT) 
            return usage.n_gpregs_used >= 6 ? Placement{stack_slot(usage.stack_used), {0, 0, primsize(t)}}
                : Placement{gpreg_slot(GP_ARGS[usage.n_gpregs_used]), {1, 0, 0}};
        else return usage.n_fpregs_used >= 8 ? Placement{stack_slot(usage.stack_used), {0, 0, primsize(t)}}
                : Placement{fpreg_slot(FP_ARGS[usage.n_fpregs_used]), {0, 1, 0}};
    }
    else if (type.kind == TK_ARR) {
        if (type.nelts == 1) return place_arg(tab, type.elt, usage);
        else return {stack_slot(usage.stack_used), {0, 0, type.size}};
    }
    else {
        if (type.len == 1) return place_arg(tab, type.members[0], usage);
        bool isfp[2] = {true, true};
        bool ismem = false;
        u32 bytes = 0;
        for (u32 i = 0; i < type.len && !ismem; i ++) {
            typeidx m = type.members[i];
            if (m != T_F32 && m != T_F64 && (m < 0 || tab.types[m].flags & TF_HAS_INT)) isfp[bytes >> 3] = false;
            u32 align = m < 0 ? primalign(m) : tab.types[m].align;
            if (bytes & align - 1) ismem = true;
            bytes += m < 0 ? primsize(m) : tab.types[m].size;
        }
        if (ismem) return {stack_slot(usage.stack_used), {0, 0, type.size}};
    }
}