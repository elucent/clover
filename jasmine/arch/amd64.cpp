#include "jasmine/arch/amd64.h"

const const_slice<i8> AMD64Target::GP_REG_NAMES[16] = {
    {"rax", 3}, {"rcx", 3}, {"rdx", 3}, {"rbx", 3}, {"rsp", 3}, {"rbp", 3}, {"rsi", 3}, {"rdi", 3},
    {"r8", 2}, {"r9", 2}, {"r10", 3}, {"r11", 3}, {"r12", 3}, {"r13", 3}, {"r14", 3}, {"r15", 3}
};

const const_slice<i8> AMD64Target::FP_REG_NAMES[16] = {
    {"xmm0", 4}, {"xmm1", 4}, {"xmm2", 4}, {"xmm3", 4}, {"xmm4", 4}, {"xmm5", 4}, {"xmm6", 4}, {"xmm7", 4},
    {"xmm8", 4}, {"xmm9", 4}, {"xmm10", 5}, {"xmm11", 5}, {"xmm12", 5}, {"xmm13", 5}, {"xmm14", 5}, {"xmm15", 5}
};

const mreg AMD64Target::GP_REGS[14] = { RAX, RCX, RDX, RSI, RDI, RBX, R8, R9, R10, R11, R12, R13, R14, R15 };
const mreg AMD64Target::FP_REGS[16] = { XMM0, XMM1, XMM2, XMM3, XMM4, XMM5, XMM6, XMM7, XMM8, XMM9, XMM10, XMM11, XMM12, XMM13, XMM14, XMM15 };

void AMD64Target::lower(const Function& fn, bytebuf<arena>& buf) {
    
}

const mreg AMD64LinuxTarget::GP_ARGS[6] = { RDI, RSI, RDX, RCX, R8, R9 };
const mreg AMD64LinuxTarget::FP_ARGS[8] = { XMM0, XMM1, XMM2, XMM3, XMM4, XMM5, XMM6, XMM7 };