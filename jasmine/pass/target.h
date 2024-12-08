#ifndef JASMINE_PASS_TARGET_H
#define JASMINE_PASS_TARGET_H

#include "jasmine/pass/helpers.h"
#include "jasmine/pass/regalloc.h"
#include "jasmine/pass/stackalloc.h"
#include "jasmine/pass/lower.h"
#include "jasmine/pass/generate.h"

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
}

#endif