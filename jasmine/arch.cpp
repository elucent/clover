#include "jasmine/arch.h"

MODULE(jasmine)

void LinkedAssembly::load() {
    iptr n_code = (data - code) / PAGESIZE;
    iptr n_data = (stat - data) / PAGESIZE;
    iptr n_static = pages.n - n_data - n_code;
    memory_tag({pages.ptr, n_code}, VM_READ | VM_EXEC);
    memory_tag({pages.ptr + n_code, n_data}, VM_READ);
    memory_tag({pages.ptr + n_code + n_data, n_static}, VM_READ | VM_WRITE);
}

inline iptr up_to_nearest_page(iptr p) {
    return p + PAGESIZE - 1 & ~(PAGESIZE - 1);
}

LinkedAssembly Assembly::link() {
    iptr codestart = 0;
    iptr datastart = codestart + up_to_nearest_page(code.size());
    iptr staticstart = datastart + up_to_nearest_page(data.size());
    iptr totalsize = staticstart + up_to_nearest_page(stat.size());
    
    LinkedAssembly linked;
    linked.pages = memory_map(totalsize / PAGESIZE);
    linked.code = (i8*)linked.pages.ptr + codestart;
    linked.data = (i8*)linked.pages.ptr + datastart;
    linked.stat = (i8*)linked.pages.ptr + staticstart;

    code.read(linked.code, code.size());
    data.read(linked.data, data.size());
    stat.read(linked.stat, stat.size());

    for (const Def& def : defs) {
        iptr base;
        switch (def.section) {
            case CODE_SECTION: base = (iptr)linked.code; break;
            case DATA_SECTION: base = (iptr)linked.data; break;
            case STATIC_SECTION: base = (iptr)linked.stat; break;
        }
        linked.defs.put(def.sym, base + def.offset);
    }

    for (const Reloc& ref : relocs) {
        iptr base;
        switch (ref.section) {
            case CODE_SECTION: base = (iptr)linked.code; break;
            case DATA_SECTION: base = (iptr)linked.data; break;
            case STATIC_SECTION: base = (iptr)linked.stat; break;
        }
        iptr reloc = base + ref.offset;
        auto it = linked.defs.find(ref.sym);
        if (it == linked.defs.end())
            fatal("Undefined symbol!");
        iptr sym = it->value;

        iptr diff = sym - reloc;
        switch (ref.kind) {
            case Reloc::REL8:
                if (diff < -128 || diff > 127)
                    fatal("Difference is too big for 8-bit relative relocation!");
                ((i8*)reloc)[-1] = i8(diff);
                break;
            case Reloc::REL16_LE:
                if (diff < -32768 || diff > 32767)
                    fatal("Difference is too big for 16-bit relative relocation!");
                ((i16*)reloc)[-1] = little_endian<i16>(diff);
                break;
            case Reloc::REL32_LE:
                if (diff < -0x80000000l || diff > 0xffffffffl)
                    fatal("Difference is too big for 32-bit relative relocation!");
                ((i32*)reloc)[-1] = little_endian<i32>(diff);
                break;
            case Reloc::REL64_LE:
                ((i64*)reloc)[-1] = little_endian<i64>(diff);
                break;
            case Reloc::REL16_BE:
                if (diff < -32768 || diff > 32767)
                    fatal("Difference is too big for 16-bit relative relocation!");
                ((i16*)reloc)[-1] = big_endian<i16>(diff);
                break;
            case Reloc::REL32_BE:
                if (diff < -0x80000000l || diff > 0xffffffffl)
                    fatal("Difference is too big for 32-bit relative relocation!");
                ((i32*)reloc)[-1] = big_endian<i32>(diff);
                break;
            case Reloc::REL64_BE:
                ((i64*)reloc)[-1] = big_endian<i64>(diff);
                break;
        }
    }

    return linked;
}

ENDMODULE()
