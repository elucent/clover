#ifndef BASIL_JASMINE_PASS_H
#define BASIL_JASMINE_PASS_H

#include "jasmine/insn.h"
#include "jasmine/arch.h"
#include "jasmine/obj.h"

struct Binding {
    enum Kind : u8 {
        NONE, GPREG, FPREG, STACK
    };

    union {
        struct { Kind kind; mreg reg; };
        struct { Kind pad; i32 offset : 24; };
    };

    inline operator bool() const {
        return kind != NONE;
    }

    inline static Binding none() {
        Binding b;
        b.kind = NONE;
        return b;
    }

    inline static Binding gpreg(mreg reg) {
        Binding b;
        b.kind = GPREG;
        b.reg = reg;
        return b;
    }

    inline static Binding fpreg(mreg reg) {
        Binding b;
        b.kind = FPREG;
        b.reg = reg;
        return b;
    }

    inline static Binding stack(i32 offset) {
        Binding b;
        b.kind = STACK;
        b.offset = offset;
        return b;
    }
};

inline void write_impl(stream& io, const Binding& binding) {
    switch (binding.kind) {
        case Binding::NONE: write(io, "none"); break;
        case Binding::FPREG:
        case Binding::GPREG: write(io, DefaultTarget::reg_name(binding.reg)); break;
        case Binding::STACK: write(io, "[frame - ", binding.offset, "]"); break;
    }
}

struct GraphNode {
    vec<localidx, 4> edges;
    Binding binding, hint;
    RegSet gpregs, fpregs;

    inline GraphNode(): binding(Binding::none()), hint(Binding::none()) {}
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
    vec<Slot> slots;
    vec<vec<LiveInterval, 2>> live;
    vec<GraphNode> rg;
    vec<Succ> succ;
    vec<Pred> pred;
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

// Compute interference graph.
void irg(Function& fn, PassInfo& info);

// Allocate registers.
template<typename Target>
void regalloc(Function& fn, PassInfo& info) {
    live(fn, info);
    irg(fn, info);

    RegSet gpregs, fpregs;
    for (mreg r : Target::gpregs()) gpregs.add(r);
    for (mreg r : Target::fpregs()) fpregs.add(r);
    for (GraphNode& node : info.rg) node.gpregs = gpregs, node.fpregs = fpregs;

    i64 stack = 0;
    for (i32 i = 0; i < info.rg.size(); i ++) if (info.live[i].size()) {
        GraphNode& node = info.rg[i];
        if (node.gpregs) {
            mreg r = node.gpregs.next();
            node.binding = Binding::gpreg(r);
            for (i32 j : node.edges) info.rg[j].gpregs.remove(r);
        }
        else node.binding = Binding::stack(stack -= 8);
    }

    // for (i32 i = 0; i < info.live.size(); i ++) if (info.live[i].size()) {
    //     Binding b = info.rg[i].binding;
    //     println("Allocated local %", i, " to ", b);
    // }
}

template<typename Target>
void stackalloc(Function& fn, PassInfo& info) {
    const TypeTable& tab = fn.obj->types;
    typeidx ret = tab.types[fn.type].ret;
    UsageState usage(0, 0, 0);
    Placement retpos = Target::place_ret(tab, ret);
    usage += retpos.usage;
    for (localidx l = 0; l < fn.insns.size(); l ++) {
        const Insn& i = fn.insns[l];
        if (i.op == OP_ARG) {
            Placement p = Target::place_arg(tab, i.type, usage);
            usage += p.usage;
            info.slots.push(p.slot);
        }
        else if (i.has_output()) {
            typeidx t = i.result_type(tab);
            info.stack += tab.native_sizeof<Target>(t);
            info.slots.push(stack_slot(-info.stack));
        }
        else info.slots.push(empty_slot());
    }
}

template<typename Target>
void lower(Function& fn, Assembly& as) {

}

#endif