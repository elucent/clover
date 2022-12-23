#include "jasmine/arch/amd64.h"
#include "core/util.h"

MODULE(jasmine)

const mreg AMD64Target::GP_REGS[14] = { RAX, RCX, RDX, RSI, RDI, RBX, R8, R9, R10, R11, R12, R13, R14, R15 };
const mreg AMD64Target::FP_REGS[16] = { XMM0, XMM1, XMM2, XMM3, XMM4, XMM5, XMM6, XMM7, XMM8, XMM9, XMM10, XMM11, XMM12, XMM13, XMM14, XMM15 };

const mreg AMD64LinuxTarget::GP_ARGS[6] = { RDI, RSI, RDX, RCX, R8, R9 };
const mreg AMD64LinuxTarget::FP_ARGS[8] = { XMM0, XMM1, XMM2, XMM3, XMM4, XMM5, XMM6, XMM7 };
const TargetDesc AMD64LinuxTarget::DESC = TargetDesc(OS_LINUX, ARCH_AMD64);

enum TypeCharacter {
    GP, FP, COMPOUND, PAD, GPGP, GPFP, FPGP, FPFP
};

TypeCharacter classify(const TypeTable& typetab, typeidx type) {
    if (type < 0) {
        if (type == T_F32 || type == T_F64) return FP;
        else return GP;
    }
    else {
        const Type& tval = typetab.types[type];
        i32 size = typetab.native_sizeof<AMD64LinuxTarget>(type);
        if (tval.kind == TK_FUN) return GP;
        if (size > 16) return COMPOUND;
        if (tval.kind == TK_ARR) {
            TypeCharacter ch = classify(typetab, tval.elt);
            if (size > 8) return ch == FP ? FPFP : GPGP;
            else return ch;
        }
        if (tval.kind == TK_TUP) {
            if (tval.size == 1) return classify(typetab, tval.elt);
            else if (size <= 8) {
                TypeCharacter ch = FP;
                for (typeidx id : tval.members)
                    if (classify(typetab, id) != FP)
                        ch = GP;
                return ch;
            }
            else {
                TypeCharacter ch1 = FP, ch2 = FP;
                i32 count = 0;
                for (typeidx id : tval.members) {
                    TypeCharacter t = classify(typetab, id);
                    if (count < 8 && t != FP) ch1 = GP;
                    else if (t != FP) ch2 = GP;
                    count += typetab.native_sizeof<AMD64LinuxTarget>(id);
                }
                return TypeCharacter((ch1 | ch2 << 1) << 2);
            }
        }
        unreachable("Unsupported typekind.");
        return COMPOUND;
    }
}

void AMD64LinuxTarget::place_call(CallBindings& call, const TypeTable& typetab, typeidx fntype) {
    // SysV-AMD64 calling convention. 
    assert(fntype >= 0);
    const Type& fn = typetab.types[fntype];

    i32 caller_stack = 0;
    i32 retsize = typetab.native_sizeof<AMD64LinuxTarget>(fn.ret);
    if (retsize <= 8) {
        auto ch = classify(typetab, fn.ret);
        if (ch == FP) call.place_ret(Binding::fp(XMM0));
        else call.place_ret(Binding::gp(RAX));
    }
    else if (retsize <= 16) {
        auto ch = classify(typetab, fn.ret);
        switch (ch) {
        case GPGP:
            call.place_ret(Binding::gp(RAX), Binding::gp(RDX));
            break;
        case FPGP:
            call.place_ret(Binding::gp(XMM0), Binding::gp(RDX));
            break;
        case GPFP:
            call.place_ret(Binding::gp(RAX), Binding::gp(XMM0));
            break;
        case FPFP:
            call.place_ret(Binding::gp(XMM0), Binding::gp(XMM1));
            break;
        default:
            unreachable("Should have classified 16-byte type as double register.");
        }
    }
    else call.place_ret(Binding::stack(caller_stack)), caller_stack += retsize;

    i32 gpargs_used = 0, fpargs_used = 0;
    for (typeidx arg : fn.members) {
        i32 argsize = typetab.native_sizeof<AMD64LinuxTarget>(arg);
        if (argsize <= 8) {
            auto ch = classify(typetab, arg);
            if (ch == FP && fpargs_used < 8) 
                call.add_param(Binding::fp(FP_ARGS[fpargs_used ++]));
            else if (ch == GP && gpargs_used < 6) 
                call.add_param(Binding::gp(GP_ARGS[gpargs_used ++]));
            else
                call.add_param(Binding::stack(caller_stack)), caller_stack += argsize;
        }
        else if (argsize <= 16) {
            auto ch = classify(typetab, arg);
            switch (ch) {
            case GPGP:
                if (gpargs_used < 5)
                    call.add_param(Binding::gp(GP_ARGS[gpargs_used]), Binding::gp(GP_ARGS[gpargs_used + 1])), gpargs_used += 2;
                else
                    call.add_param(Binding::stack(caller_stack)), caller_stack += argsize;
                break;
            case FPGP:
                if (gpargs_used < 6 && fpargs_used < 8)
                    call.add_param(Binding::fp(FP_ARGS[fpargs_used ++]), Binding::gp(GP_ARGS[gpargs_used ++]));
                else
                    call.add_param(Binding::stack(caller_stack)), caller_stack += argsize;
                break;
            case GPFP:
                if (gpargs_used < 6 && fpargs_used < 8)
                    call.add_param(Binding::gp(GP_ARGS[gpargs_used ++]), Binding::fp(FP_ARGS[fpargs_used ++]));
                else
                    call.add_param(Binding::stack(caller_stack)), caller_stack += argsize;
                break;
            case FPFP:
                if (fpargs_used < 5)
                    call.add_param(Binding::fp(FP_ARGS[fpargs_used]), Binding::fp(FP_ARGS[fpargs_used + 1])), fpargs_used += 2;
                else
                    call.add_param(Binding::stack(caller_stack)), caller_stack += argsize;
                break;
            default:
                unreachable("Should have classified 16-byte type as double register.");
            }
        }
        else call.add_param(Binding::stack(caller_stack)), caller_stack += argsize;
    }
}

ENDMODULE()
