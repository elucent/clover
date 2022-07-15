#include "jasmine/pass.h"
#include "jasmine/obj.h"
#include "lib/bits.h"

void foldc(Function& fn) {

}

void dce(Function& fn) {

}

void print_slots(const Function& fn, const Target& arch, stream& io) {
    fn.formatshort(io);
    write(io, '\n');
    for (localidx l = 0; l < fn.slots.size(); l ++) {
        if (fn.slots[l].type != SLOT_NONE) switch (fn.slots[l].type) {
            case SLOT_NONE: break;
            case SLOT_GPREG: write(io, "  %", l, " : ", arch.gpreg_name(fn.slots[l].reg), '\n'); break;
            case SLOT_FPREG: write(io, "  %", l, " : ", arch.fpreg_name(fn.slots[l].reg), '\n'); break;
            case SLOT_GPREG_PAIR: write(io, "  %", l, " : {", arch.gpreg_name(fn.slots[l].regpair[0]), ", ", arch.gpreg_name(fn.slots[l].regpair[1]), "}\n"); break;
            case SLOT_FPREG_PAIR: write(io, "  %", l, " : {", arch.fpreg_name(fn.slots[l].regpair[0]), ", ", arch.fpreg_name(fn.slots[l].regpair[1]), "}\n"); break;
            case SLOT_GPFPREG_PAIR: write(io, "  %", l, " : {", arch.gpreg_name(fn.slots[l].regpair[0]), ", ", arch.fpreg_name(fn.slots[l].regpair[1]), "}\n"); break;
            case SLOT_FPGPREG_PAIR: write(io, "  %", l, " : {", arch.fpreg_name(fn.slots[l].regpair[0]), ", ", arch.gpreg_name(fn.slots[l].regpair[1]), "}\n"); break;
            case SLOT_STACK: write(io, "  %", l, " : [sp", fn.slots[l].stack, "]\n"); break;
        }
    }
}

struct Interval {
    i32 start, end, hint, next;
};

void regalloc(Function& fn, const Target& arch) {
    fn.slots.clear();
    fn.format(stdout, 0);
    
    for (i64 i = 0; i < fn.labels.size(); i ++) print("# fn label ", i, " points to insn ", fn.labels[i], '\n');

    vec<Interval> intervals;
    vec<i32> cur_intervals;
    vec<vec<u32, 2>> preds;
    vec<bitset<>> live;
    for (u32 i = 0; i < fn.insns.size(); i ++) preds.push({}), live.push(bitset<>(fn.insns.size())), cur_intervals.push(-1);

    for (u32 i = 0; i < fn.insns.size(); i ++) {
        const auto& insn = fn.insns[i];
        if (insn.op == OP_RET) continue;
        else if (insn.op == OP_JUMP) {
            print("$ label = ", insn.args()[0].lbl, " with insn ", fn.labels[insn.args()[0].lbl], '\n');
            if (insn.params()[0] == P_LABEL) preds[fn.labels[insn.args()[0].lbl]].push(i);
        }
        else {
            if (insn.is_jump()) if (insn.params()[0] == P_LABEL) preds[fn.labels[insn.args()[0].lbl]].push(i);
            preds[i + 1].push(i);
        }
    }

    print("preds:");
    for (u32 i = 0; i < fn.insns.size(); i ++) {
        print("  ", i, ":");
        for (u32 p : preds[i]) print(' ', p);
        print('\n');
    }

    bitset<> newbits(fn.insns.size());
    for (i32 i = i32(fn.insns.size()) - 1; i >= 0; i --) {
        newbits.clear();
        newbits |= live[i];
        if (fn.insns[i].has_output()) if (newbits[i]) {
            if (cur_intervals[i] != -1) intervals[cur_intervals[i]].start = i;
            newbits.off(i);
        }
        for (u32 j = 0; j < fn.insns[i].params().n; j ++) {
            if (fn.insns[i].params()[j] == P_REG) {
                auto reg = fn.insns[i].args()[j].reg;
                if (!newbits[reg]) {
                    Interval v = {-1, i, -1, cur_intervals[reg]};
                    cur_intervals[reg] = intervals.size();
                    intervals.push(v);
                    newbits.on(reg);
                }
            }
        }
        for (u32 p : preds[i]) live[p] |= newbits;
    }

    for (u32 i = 0; i < fn.insns.size(); i ++) {
        if (fn.insns[i].has_output() && cur_intervals[i] != -1) {
            print("variable %", i, " live over ");
            i32 p = cur_intervals[i];
            while (p != -1) {
                const Interval& in = intervals[p];
                print('[', in.start, ", ", in.end, ']');
                p = in.next;
                if (p != -1) print(" -> ");
            }
            print('\n');
        }
    }

    print_slots(fn, arch, stdout);
}

void stackalloc(Function& fn, const Target& arch) {
    fn.slots.clear();

    const TypeTable& tab = fn.obj->types;
    typeidx ret = tab.types[fn.type].ret;
    UsageState usage(0, 0, 0);
    Placement retpos = arch.place_ret(tab, ret);
    usage += retpos.usage;
    for (localidx l = 0; l < fn.insns.size(); l ++) {
        const Insn& i = fn.insns[l];
        if (i.op == OP_PAR) {
            Placement p = arch.place_arg(tab, i.type, usage);
            usage += p.usage;
            fn.slots.push(p.slot);
        }
        else if (i.has_output()) {
            typeidx t = i.result_type(tab);
            fn.stack += tab.native_sizeof(arch, t);
            fn.slots.push(stack_slot(-fn.stack));
        }
        else fn.slots.push(empty_slot());
    }

    print_slots(fn, arch, stdout);
}