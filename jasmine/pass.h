#ifndef BASIL_JASMINE_PASS_H
#define BASIL_JASMINE_PASS_H

#include "jasmine/insn.h"

MODULE(jasmine)

using Pred = vec<localidx, 2>;

struct BasicBlock {
    vec<localidx> seq;
};

struct PassInfo {
    enum Phase : i32 {
        CFG = 1,
        PIE = 2,
        INLINE = 4,
        RENUMBER = 8,
        SCHEDULE = 16,
        LIVENESS = 32,
        IRG = 64,
        DCE = 128,
        FOLDC = 256,
        UNROLL = 512,
        REGALLOC = 1024,
        PREISEL = 2048
    };

    // vec<vec<LiveInterval, 2>> live;
    // vec<GraphNode> rg;
    // CallBindings callinfo;
    vec<BasicBlock> bb;
    vec<localidx> ib;
    vec<Pred> pred;
    i32 stack;
    i32 status = 0;

    inline bool didPhase(Phase phase) {
        return status & phase;
    }

    inline void complete(Phase phase) {
        status |= phase;
    }

    inline void invalidate(Phase phase) {
        status &= ~phase;
    }
};

// Reduce strength of certain operations.
void reduce(PassInfo& info, Function& fn);

// Eliminate phi instructions, replacing them with moves from predecessors. 
void pie(PassInfo& info, Function& fn);

// Analyzes the control flow graph, producing the following information:
//  - Finds all basic block node sequences, that is node sequences that have
//    only one exit branch.
//  - Builds lists of predecessors for all nodes.
void cfg(PassInfo& info, Function& fn);

// Removes unreachable and useless code (NOPs) from the function.
void dce(PassInfo& info, Function& fn);

// Transforms constant expressions into immediates.
void foldc(PassInfo& info, Function& fn);

// Renumber all nodes so that basic blocks are at contiguous indices.
void renumber(PassInfo& info, Function& fn);

// Schedule instructions by topological sort. Instructions are scheduled
// so that consecutive instructions have similar dependency levels, and
// are more likely to be executed in parallel. Ordering of instructions
// with possible side effects is preserved.
void sched(PassInfo& info, Function& fn);

// Detect and unroll loops.
void unroll(PassInfo& info, Function& fn);

// Attempt to inline calls to known functions.
void tryinline(PassInfo& info, Function& fn);

// Compute live intervals of each variable.
void live(PassInfo& info, Function& fn);

// Build interference graph between all variables.
void irg(PassInfo& info, Function& fn);

// Assign registers to variables.
template<typename Target>
void regalloc(PassInfo& info, Function& fn);

// Do IR-level peephole optimization based on the target architecture.
template<typename Target>
void preisel(PassInfo& info, Function& fn);

// Lower instructions to machine code.
template<typename Target>
void lower(PassInfo& info, Function& fn, Assembly& as);

ENDMODULE()

#endif