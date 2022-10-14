#ifndef BASIL_JASMINE_PASS_H
#define BASIL_JASMINE_PASS_H

#include "jasmine/insn.h"
#include "jasmine/arch.h"
#include "jasmine/obj.h"

struct Binding {
    enum Kind {
        NONE, GPREG, FPREG, STACK
    };

    Kind kind;
    union {
        mreg reg;
        i32 offset;
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

struct GraphNode {
    vec<localidx, 6> edges;
    Binding binding, hint;

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
void inlining(Function& fn, PassInfo& info);
void cfg(Function& fn, PassInfo& info);
void foldc(Function& fn, PassInfo& info);
void dce(Function& fn, PassInfo& info);
void live(Function& fn, PassInfo& info);

template<typename Target>
void regalloc(Function& fn, PassInfo& info) {
    live(fn, info);
    for (localidx l = 0; l < fn.insns.size(); l ++)
        info.rg.push(GraphNode());
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