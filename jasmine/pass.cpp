#include "jasmine/pass.h"
#include "jasmine/obj.h"

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
            case SLOT_GPREG: write(io, "  %", l, " : ", arch.gpreg_name(fn.slots[l].payload), '\n'); break;
            case SLOT_FPREG: write(io, "  %", l, " : ", arch.fpreg_name(fn.slots[l].payload), '\n'); break;
            case SLOT_STACK: write(io, "  %", l, " : [sp - ", -fn.slots[l].payload, "]\n"); break;
        }
    }
}

void regalloc(Function& fn, const Target& arch) {
    
}

void stackalloc(Function& fn, const Target& arch) {
    fn.slots.clear();

    const TypeTable& tab = fn.obj->types;
    for (localidx l = 0; l < fn.insns.size(); l ++) {
        const Insn& i = fn.insns[l];
        if (i.op == OP_PAR) {
            
        }
        else if (i.has_output()) {
            typeidx t = i.result_type(tab);
            fn.stack += tab.native_sizeof(arch, t);
            fn.slots.push({ SLOT_STACK, -fn.stack });
        }
        else fn.slots.push({ SLOT_NONE, 0 });
    }

    print_slots(fn, arch, stdout);
}