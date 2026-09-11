#ifndef JASMINE_PASS_TARGET_H
#define JASMINE_PASS_TARGET_H

#include "jasmine/pass/validate.h"

#ifdef RT_AMD64
#include "jasmine/pass/amd64.h"
#endif

#ifdef RT_RISCV64
#include "jasmine/pass/riscv64.h"
#endif

namespace jasmine {
    template<typename Target>
    TargetSpecificPasses<Target>::TargetSpecificPasses(PassContext& ctx_in):
        context(&ctx_in) {
        RegSet gpSet = Target::caller_saved_gps();
        RegSet fpSet = Target::caller_saved_fps();
        for (u32 i = 0; i < NUM_ASM_OPCODES; i ++) {
            RegSet clobbers = Target::clobbers((ASMOpcode)i);
            gpSet -= clobbers;
            fpSet -= clobbers;
        }
        assert(gpSet.size() >= 2);
        assert(fpSet.size() >= 2);
        for (auto [i, r] : enumerate(gpSet)) {
            if (i >= 8)
                break;
            gpScratches.push(r);
        }
        for (auto [i, r] : enumerate(fpSet)) {
            if (i >= 8)
                break;
            fpScratches.push(r);
        }
    }

    template<typename Target>
    void TargetSpecificPasses<Target>::lower(PassContext& ctx, Function& fn, Block& block, u32 indexInBlock, Node& node) {
        #ifdef RT_AMD64
            if constexpr (Target::arch() == ARCH_AMD64)
                return amd64::lower(ctx, fn, block, indexInBlock, node);
        #endif
        #ifdef RT_RISCV64
            if constexpr (Target::arch() == ARCH_RISCV64)
                return riscv64::lower(ctx, fn, block, indexInBlock, node);
        #endif
    }

    template<typename Target>
    RegSet* TargetSpecificPasses<Target>::validRegsFor(Node node) {
        #ifdef RT_AMD64
            if constexpr (Target::arch() == ARCH_AMD64)
                return amd64::validRegsFor(node);
        #endif
        #ifdef RT_RISCV64
            if constexpr (Target::arch() == ARCH_RISCV64)
                return riscv64::validRegsFor(node);
        #endif
        unreachable("Tried to get valid registers for unknown architecture.");
    }

    template<typename Target>
    RegSet TargetSpecificPasses<Target>::validRegsFor(Node node, u32 operand) {
        #ifdef RT_AMD64
            if constexpr (Target::arch() == ARCH_AMD64)
                return amd64::validRegsFor(node, operand);
        #endif
        #ifdef RT_RISCV64
            if constexpr (Target::arch() == ARCH_RISCV64)
                return riscv64::validRegsFor(node, operand);
        #endif
        unreachable("Tried to get valid registers for unknown architecture.");
    }
}

#endif