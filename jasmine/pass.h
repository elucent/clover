#ifndef BASIL_JASMINE_PASS_H
#define BASIL_JASMINE_PASS_H

#include "lib/math.h"
#include "jasmine/insn.h"
#include "jasmine/arch.h"
#include "jasmine/obj.h"

struct GraphNode {
    vec<localidx, 4> edges;
    Binding binding;
    RegSet gpregs, fpregs;
    mreg hint;
    bool is_clobber_set; // True

    inline GraphNode(RegSet gpreg_clobbers, RegSet fpreg_clobbers):
        binding(Binding::none()), gpregs(gpreg_clobbers), fpregs(fpreg_clobbers), hint(-1), is_clobber_set(true) {}
    inline GraphNode(): binding(Binding::none()), hint(-1), is_clobber_set(false) {}
};

struct BasicBlock {
    i32 start, end;
    vec<i32, 2> pred, succ;

    inline BasicBlock(i32 start_in, i32 end_in): start(start_in), end(end_in) {}
};

struct LiveInterval {
    localidx start, end;
};

struct Succ {
    localidx succ[2]; // No operation can have more than two successors.

    inline Succ() { succ[0] = succ[1] = -1; }
    inline Succ(localidx s) { succ[0] = s, succ[1] = -1; }
    inline Succ(localidx a, localidx b) { succ[0] = a, succ[1] = b; }

    inline slice<localidx> items() {
        if (succ[0] < 0) return slice<localidx>{succ, (iptr)0};
        else if (succ[1] < 0) return slice<localidx>{succ, 1};
        else return slice<localidx>{succ, 2}; 
    }

    inline const_slice<localidx> items() const {
        if (succ[0] < 0) return const_slice<localidx>{succ, (iptr)0};
        else if (succ[1] < 0) return const_slice<localidx>{succ, 1};
        else return const_slice<localidx>{succ, 2};
    }
};

using Pred = vec<localidx, 2>;

struct PassInfo {
    vec<BasicBlock> bb;
    vec<i32> ib;
    vec<vec<LiveInterval, 2>> live;
    vec<GraphNode> rg;
    vec<Succ> succ;
    vec<Pred> pred;
    CallBindings callinfo;
    i32 stack;
};

PassInfo* makepassinfo();

void init(Function& fn, PassInfo& info);

// Inline functions.
void inlining(Function& fn, PassInfo& info);

// Compute control flow graph/basic blocks.
void cfg(Function& fn, PassInfo& info);

// Fold constant expressions.
void foldc(Function& fn, PassInfo& info);

// Eliminate dead code.
void dce(Function& fn, PassInfo& info);

// Compute live intervals of each variable.
void live(Function& fn, PassInfo& info);

inline u64 hash(const pair<localidx, localidx>& p) {
    return hash(p.first) * 31ul + hash(p.second);
}

inline void write_impl(stream& io, const Binding& binding) {
    switch (binding.kind) {
        case Binding::NONE: write(io, "none"); break;
        case Binding::FPREG:
        case Binding::GPREG: write(io, DefaultTarget::reg_name(binding.reg)); break;
        case Binding::STACK: write(io, "[frame - ", binding.offset, "]"); break;
    }
}

#endif