#include "asm/arch.h"
#include "util/config.h"
#include "util/io.h"
#include "util/hash.h"
#include "util/math.h"

void LinkedAssembly::load() {
    auto pagesize = memory::pagesize();
    iword n_code = roundUpToNearest<u64>(codesize, pagesize);
    iword n_data = roundUpToNearest<u64>(datasize, pagesize);
    iword n_static = roundUpToNearest<u64>(statsize, pagesize);
    memory::tag({pages.data(), n_code}, memory::READ | memory::EXEC);
    memory::tag({pages.data() + n_code, n_data}, memory::READ);
    memory::tag({pages.data() + n_code + n_data, n_static}, memory::READ | memory::WRITE);
}

void linkReloc(iptr reloc, iptr sym, Reloc::Kind kind) {
    iptr diff = sym - reloc;
    switch (kind) {
        #if INCLUDE_ARCH_AMD64
            case Reloc::REL32_LE_J_AMD64:
            case Reloc::REL32_LE_M_AMD64:
                if (diff < -0x80000000l || diff > 0x7fffffffl)
                    panic("Difference is too big for 32-bit relative relocation!");
                store<i32>(little_endian<i32>(diff), (i32*)reloc - 1);
                break;
        #endif

        #if INCLUDE_ARCH_RISCV64
            case Reloc::REL20_12_JALR_RV64:
            case Reloc::REL20_12_JCC_RV64:
            case Reloc::REL20_12_JR_RV64: {
                if (diff < -0x80000000l || diff > 0x7fffffffl)
                    panic("Difference is too big for 32-bit relative relocation!");
                i32 bccOffset = kind == Reloc::REL20_12_JCC_RV64 ? 1 : 0;

                i32* first = (i32*)reloc + bccOffset;
                i32* second = (i32*)reloc + bccOffset + 1;
                i32 auipc = from_little_endian<i32>(load<i32>(first));
                i32 jalr = from_little_endian<i32>(load<i32>(second));

                assert(auipc >> 12 == 0);
                assert((jalr >> 20 & 0xfff) == 0);
                auipc |= (diff >> 12) << 12;
                jalr |= (diff & 0xfff) << 20;
                store(little_endian<i32>(auipc), first);
                store(little_endian<i32>(jalr), second);

                if (kind == Reloc::REL20_12_JCC_RV64) {
                    // Also need to finish up the preceding conditional branch.
                    i32& bcc = ((i32*)reloc)[0];
                    i32 bccw = from_little_endian<i32>(bcc);
                    bccw |= 4 << 7; // It's always two instructions.
                    bcc = little_endian<i32>(bccw);
                }
                break;
            }

            case Reloc::REL20_12_LD_RV64:
            case Reloc::REL20_12_LDI_RV64: {
                if (diff < -0x80000000l || diff > 0x7fffffffl)
                    panic("Difference is too big for 32-bit relative relocation!");
                i32* first = (i32*)reloc;
                i32* second = (i32*)reloc + (kind == Reloc::REL20_12_LDI_RV64 ? 2 : 1);
                i32 auipc = from_little_endian<i32>(load<i32>(first));
                i32 ld = from_little_endian<i32>(load<i32>(second));

                assert(auipc >> 12 == 0);
                assert((ld >> 20 & 0xfff) == 0);
                auipc |= (diff >> 12) << 12;
                ld |= (diff & 0xfff) << 20;
                store(little_endian<i32>(auipc), first);
                store(little_endian<i32>(ld), second);
                break;
            }

            case Reloc::REL20_12_ST_RV64:
            case Reloc::REL20_12_STI_RV64: {
                if (diff < -0x80000000l || diff > 0x7fffffffl)
                    panic("Difference is too big for 32-bit relative relocation!");
                i32* first = (i32*)reloc;
                i32* second = (i32*)reloc + (kind == Reloc::REL20_12_STI_RV64 ? 2 : 1);
                i32 auipc = from_little_endian<i32>(load<i32>(first));
                i32 st = from_little_endian<i32>(load<i32>(second));

                assert(auipc >> 12 == 0);
                assert((st >> 7 & 0b11111) == 0);
                assert((st >> 25 & 0b1111111) == 0);
                auipc |= (diff >> 12) << 12;
                st |= (diff & 0b11111) << 7;
                st |= (diff & 0b1111111) << 25;
                store(little_endian<i32>(auipc), first);
                store(little_endian<i32>(st), second);
                break;
            }
        #endif

        case Reloc::REL8:
            if (diff < -0x80l || diff > 0x7fl)
                panic("Difference is too big for 8-bit relative relocation!");
            store(i8(diff), (i8*)reloc - 1);
            break;
        case Reloc::REL16_LE:
            if (diff < -0x8000l || diff > 0x7fffl)
                panic("Difference is too big for 16-bit relative relocation!");
            store(little_endian<i16>(diff), (i16*)reloc - 1);
            break;
        case Reloc::REL32_LE:
            if (diff < -0x80000000l || diff > 0x7fffffffl)
                panic("Difference is too big for 32-bit relative relocation!");
            store(little_endian<i32>(diff), (i32*)reloc - 1);
            break;
        case Reloc::REL64_LE:
            store(little_endian<i64>(diff), (i64*)reloc - 1);
            break;
        case Reloc::REL16_BE:
            if (diff < -0x8000l || diff > 0x7fffl)
                panic("Difference is too big for 16-bit relative relocation!");
            store(big_endian<i16>(diff), (i16*)reloc - 1);
            break;
        case Reloc::REL32_BE:
            if (diff < -0x80000000l || diff > 0x7fffffffl)
                panic("Difference is too big for 32-bit relative relocation!");
            store(big_endian<i32>(diff), (i32*)reloc - 1);
            break;
        case Reloc::REL64_BE:
            store(big_endian<i64>(diff), (i64*)reloc - 1);
            break;

        case Reloc::ABS64_LE:
            store(little_endian<i64>(sym), (i64*)reloc - 1);
            break;

        case Reloc::ABS64_BE:
            store(big_endian<i64>(sym), (i64*)reloc - 1);
            break;
    }
}

void Assembly::defaultRelocator(AssemblyView& view, vec<Reloc, 16>& relocs) {
    for (const Reloc& ref : relocs) {
        iptr base;
        switch (ref.section) {
            case CODE_SECTION: base = (iptr)view.code; break;
            case DATA_SECTION: base = (iptr)view.data; break;
            case STATIC_SECTION: base = (iptr)view.stat; break;
        }
        iptr reloc = base + ref.offset;
        iptr sym;
        if (ref.hasSym) {
            auto it = view.defmap->find(ref.sym);
            if (it == view.defmap->end())
                panic("Undefined symbol ", (*view.symtab)[ref.sym]);
            sym = (*view.defs)[it->value];
        } else
            sym = (*view.defs)[ref.sym]; // ref.sym encodes symbol index directly.

        linkReloc(reloc, sym, ref.kind);
    }
}

void Assembly::linkInternally(RelocationFunction relocator) {
    code.rebase();
    data.rebase();
    stat.rebase();
    vec<iptr, 16> defs;
    map<Symbol, u32> defmap;

    for (const Def& def : this->defs) {
        iptr base;
        switch (def.section) {
            case CODE_SECTION: base = (iptr)code._data; break;
            case DATA_SECTION: base = (iptr)data._data; break;
            case STATIC_SECTION: base = (iptr)stat._data; break;
        }
        if (def.hasSym)
            defmap.put(def.sym, defs.size());
        defs.push(base + def.offset);
    }

    vec<Reloc, 16> internalRelocs;
    relocs.removeIf([&](const Reloc& reloc) -> bool {
        if (!reloc.hasSym && this->defs[reloc.sym].section == reloc.section) {
            internalRelocs.push(reloc);
            return true;
        }
        return false;
    });

    AssemblyView view;
    view.code = code._data;
    view.data = data._data;
    view.stat = stat._data;

    i32 codesize = code.size(), datasize = data.size(), statsize = stat.size();
    view.codesize = &codesize;
    view.datasize = &datasize;
    view.statsize = &statsize;

    code._end = codesize;
    data._end = datasize;
    stat._end = statsize;

    view.defs = &defs;
    view.defmap = &defmap;
    view.symtab = &symtab;
    relocator(view, internalRelocs);
}

void Assembly::linkInto(LinkedAssembly& linked, RelocationFunction relocator) {
    u64 codestart = 0;
    u64 pagesize = memory::pagesize();
    u64 datastart = codestart + roundUpToNearest<u64>(code.size(), pagesize);
    u64 staticstart = datastart + roundUpToNearest<u64>(data.size(), pagesize);
    u64 totalsize = staticstart + roundUpToNearest<u64>(stat.size(), pagesize);

    linked.codesize = code.size();
    linked.datasize = data.size();
    linked.statsize = stat.size();
    linked.pages = memory::map(totalsize);
    linked.code = (i8*)linked.pages.data() + codestart;
    linked.data = (i8*)linked.pages.data() + datastart;
    linked.stat = (i8*)linked.pages.data() + staticstart;
    linked.symtab = &symtab;

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
        if (def.hasSym)
            linked.defmap.put(def.sym, linked.defs.size());
        linked.defs.push(base + def.offset);
    }

    auto view = linkedAssemblyView(linked);
    relocator(view, relocs);

    if (config::printMachineCode) {
        println("Loaded ", linked.codesize, " bytes at ", hex(linked.code));
        for (i8 i : const_slice<i8>{ linked.code, linked.codesize })
            print(hex((u64)(u8)i, 2));
        println("");
    }
}

#if INCLUDE_ARCH_AMD64
    #include "asm/arch/amd64.h"
#endif

#if INCLUDE_ARCH_ARM64
    #include "asm/arch/arm64.h"
#endif

#if INCLUDE_ARCH_RISCV64
    #include "asm/arch/riscv64.h"
#endif

RelocationFunction relocatorFor(Arch arch, OS os) {
    switch (arch) {
        #if INCLUDE_ARCH_AMD64
            case ARCH_AMD64:
                return compactingRelocator<AMD64Assembler>;
        #endif

        #if INCLUDE_ARCH_ARM64
            case ARCH_ARM64:
                return compactingRelocator<ARM64Assembler>;
        #endif

        #if INCLUDE_ARCH_RISCV64
            case ARCH_RISCV64:
                return compactingRelocator<RISCV64Assembler>;
        #endif

        default:
            unreachable("Couldn't construct relocator for arch ", ARCH_NAMES[arch], " and os ", OS_NAMES[os]);
    }
}

struct ELFSymbolInfo {
    Symbol sym;
    u32 index;
    u32 nameOffset;
    u32 offset; // -1 if undefined
    Section section;
    DefType type;
};

void Assembly::writeELFObject(fd file) {
    // Overall ELF relocatable object header

    bytebuf objectHeader;
    objectHeader.reserve(64);

    objectHeader.writeUnchecked("\x7f" "ELF", 4); // elf magic

    constexpr u8 ELFCLASSNONE = 0, ELFCLASS32 = 1, ELFCLASS64 = 2;
    #ifdef RT_64
    objectHeader.write<u8>(ELFCLASS64); // elf class. we assume 64-bit due to host
    #elif defined(RT_32)
    objectHeader.write<u8>(ELFCLASS32); // elf class. we assume 32-bit due to host
    #else
    #error "Can't generate ELF binaries for non-32-bit, non-64-bit platform."
    #endif

    constexpr u8 ELFDATANONE = 0, ELFDATALSB = 1, ELFDATAMSB = 2;
    objectHeader.writeUnchecked<u8>(ELFDATALSB); // elf data format. we always write little-endian for now

    objectHeader.writeLEUnchecked<u8>(1); // elf version. always 1 (current version)

    objectHeader.writeLEUnchecked<u8>(0); // elf os abi. 0 for now, until it becomes important

    objectHeader.writeLEUnchecked<u8>(0); // elf abi version. 0 for now as well

    objectHeader.writeUnchecked("\0\0\0\0\0\0\0", 7); // elf padding. the identifying header should be 16 bytes
    assert(objectHeader.size() == 16);

    constexpr u16 ET_NONE = 0, ET_REL = 1, ET_EXEC = 2, ET_DYN = 3;
    objectHeader.writeLEUnchecked<u16>(ET_REL); // e_type : u16

    constexpr u16 EM_NONE = 0, EM_X86_64 = 62, EM_AARCH64 = 183, EM_RISCV = 243;
    #ifdef RT_AMD64
    objectHeader.writeLEUnchecked<u16>(EM_X86_64); // e_machine : u16
    #elif defined(RT_ARM64)
    objectHeader.writeLEUnchecked<u16>(EM_AARCH64); // e_machine : u16
    #elif defined(RT_RISCV64)
    objectHeader.writeLEUnchecked<u16>(EM_RISCV); // e_machine : u16
    #else
    #error "Can't generate ELF binaries for this machine."
    #endif

    objectHeader.writeLEUnchecked<u32>(1); // e_version : u32 = 1 (current version)
    objectHeader.writeLEUnchecked<uptr>(0); // e_entry : uptr = 0x0 (since we're relocatable)
    objectHeader.writeLEUnchecked<u64>(0); // e_phoff : u64 = 0x0 (again, since we're relocatable)
    objectHeader.writeLEUnchecked<u64>(64); // e_shoff : u64 = 64 (right after this header)
    objectHeader.writeLEUnchecked<u32>(0); // e_flags : u32 = 0x0 (no arch-specific flags yet)
    objectHeader.writeLEUnchecked<u16>(64); // e_ehsize : u16 = 64 (size of ELF header, always the same)
    objectHeader.writeLEUnchecked<u16>(0); // e_phentsize : u16 = 0 (size of program header table entries, we don't have any)
    objectHeader.writeLEUnchecked<u16>(0); // e_phnum : u16 = 0 (number of program header table entries, again we don't have any)
    objectHeader.writeLEUnchecked<u16>(64); // e_shentsize : u16 = 64 (size of section header table entries, fixed for 64-bit binaries)
    objectHeader.writeLEUnchecked<u16>(10); // e_shnum : u16 = 10 (number of section header table entries, for us that's <null>, .shstrtab, .text, .data, .rodata, .strtab, .symtab, .rel.text, .rel.rodata, .rel.data)
    objectHeader.writeLEUnchecked<u16>(1); // e_shstrndx : u16 = 1 (index of .shstrtab)
    assert(objectHeader.size() == 64);

    // Section header table. Really we just define it here, and add entries
    // onto it as we generate subsequent sections.

    bytebuf sectionHeaderTable;
    sectionHeaderTable.reserve(640); // It should be 640 bytes exactly at this point.

    constexpr u32 SHT_NULL = 0, SHT_PROGBITS = 1, SHT_SYMTAB = 2, SHT_STRTAB = 3, SHT_RELA = 4, SHT_REL = 9;
    constexpr u32 SHF_WRITE = 1, SHF_ALLOC = 2, SHF_EXECINSTR = 4, SHF_MERGE = 16, SHF_STRINGS = 32, SHF_INFO_LINK = 64, SHF_TLS = 1024;
    constexpr u32 SHN_UNDEF = 0;

    constexpr u32 offsetOfFirstSection = 64 + 10 * 64; // e_ehsize + e_shnum * e_shentsize

    // Null section

    sectionHeaderTable.writeLEUnchecked<u32>(0); // sh_name : u32 = 0
    sectionHeaderTable.writeLEUnchecked<u32>(SHT_NULL); // sh_type : u32 = SHT_NULL
    sectionHeaderTable.writeLEUnchecked<u64>(0); // sh_flags : u64 = 0
    sectionHeaderTable.writeLEUnchecked<uptr>(0); // sh_addr : uptr = 0
    sectionHeaderTable.writeLEUnchecked<u64>(0); // sh_offset : u64 (we are the first section)
    sectionHeaderTable.writeLEUnchecked<u64>(0); // sh_size : u64
    sectionHeaderTable.writeLEUnchecked<u32>(SHN_UNDEF); // sh_link : u32 = 0
    sectionHeaderTable.writeLEUnchecked<u32>(0); // sh_info : u32 = 0
    sectionHeaderTable.writeLEUnchecked<u64>(0); // sh_addralign : u64 = 0
    sectionHeaderTable.writeLEUnchecked<u64>(0); // sh_entsize : u64 = 0
    assert(sectionHeaderTable.size() == 64);

    // Section header string table

    bytebuf sectionHeaderStringTable;
    u32 sectionHeaderStringTableOffset = offsetOfFirstSection;

    sectionHeaderStringTable.write("", 1); // string 0 must be the empty string

    u32 sectionHeaderStringTableNameOffset = sectionHeaderStringTable.size();
    sectionHeaderStringTable.write(".shstrtab", 10); // data for the shstrtab - just some fixed string literals
    u32 textSectionNameOffset = sectionHeaderStringTable.size();
    sectionHeaderStringTable.write(".text", 6);
    u32 dataSectionNameOffset = sectionHeaderStringTable.size();
    sectionHeaderStringTable.write(".rodata", 8);
    u32 staticSectionNameOffset = sectionHeaderStringTable.size();
    sectionHeaderStringTable.write(".data", 6);
    u32 stringTableNameOffset = sectionHeaderStringTable.size();
    sectionHeaderStringTable.write(".strtab", 8);
    u32 symbolTableNameOffset = sectionHeaderStringTable.size();
    sectionHeaderStringTable.write(".symtab", 8);
    u32 textRelocationTableNameOffset = sectionHeaderStringTable.size();
    sectionHeaderStringTable.write(".rela.text", 11);
    u32 dataRelocationTableNameOffset = sectionHeaderStringTable.size();
    sectionHeaderStringTable.write(".rela.rodata", 13);
    u32 staticRelocationTableNameOffset = sectionHeaderStringTable.size();
    sectionHeaderStringTable.write(".rela.data", 11);

    while (sectionHeaderStringTable.size() % 64) // pad to multiple of 64 bytes
        sectionHeaderStringTable.write<u8>(0);

    sectionHeaderTable.writeLEUnchecked<u32>(sectionHeaderStringTableNameOffset); // sh_name : u32 = 1 (.shstrtab)
    sectionHeaderTable.writeLEUnchecked<u32>(SHT_STRTAB); // sh_type : u32 = SHT_STRTAB (shstrtab is a string table)
    sectionHeaderTable.writeLEUnchecked<u64>(SHF_STRINGS | SHF_MERGE); // sh_flags : u64 = SHF_STRINGS | SHF_MERGE
    sectionHeaderTable.writeLEUnchecked<uptr>(0); // sh_addr : uptr = 0 (we're relocatable; someone will fill this in later)
    sectionHeaderTable.writeLEUnchecked<u64>(sectionHeaderStringTableOffset); // sh_offset : u64 (we are the first section)
    sectionHeaderTable.writeLEUnchecked<u64>(sectionHeaderStringTable.size()); // sh_size : u64
    sectionHeaderTable.writeLEUnchecked<u32>(SHN_UNDEF); // sh_link : u32 = 0
    sectionHeaderTable.writeLEUnchecked<u32>(0); // sh_info : u32 = 0
    sectionHeaderTable.writeLEUnchecked<u64>(0); // sh_addralign : u64 = 0 (we don't care about alignment)
    sectionHeaderTable.writeLEUnchecked<u64>(0); // sh_entsize : u64 = 0 (we don't have any entries with regular size)
    assert(sectionHeaderTable.size() == 128);

    // Text section

    bytebuf textSection;
    textSection.reserve(roundUpToNearest<iptr>(this->code.size(), 64));

    u32 textSectionOffset = sectionHeaderStringTableOffset + sectionHeaderStringTable.size();
    assert(textSectionOffset % 64 == 0);

    textSection.writeUnchecked(this->code); // copy in our binary
    while (textSection.size() % 64) // pad to multiple of 64 bytes
        textSection.writeUnchecked<u8>(0);

    sectionHeaderTable.writeLEUnchecked<u32>(textSectionNameOffset); // sh_name : u32 (.text in the section header string table)
    sectionHeaderTable.writeLEUnchecked<u32>(SHT_PROGBITS); // sh_type : u32 = SHT_PROGBITS (code is program data)
    sectionHeaderTable.writeLEUnchecked<u64>(SHF_EXECINSTR | SHF_ALLOC); // sh_flags : u64 (code is executable and must be allocated at load time)
    sectionHeaderTable.writeLEUnchecked<uptr>(0); // sh_addr : uptr = 0 (we're relocatable; someone will fill this in later)
    sectionHeaderTable.writeLEUnchecked<u64>(textSectionOffset); // sh_offset : u64 (text comes after the section header table)
    sectionHeaderTable.writeLEUnchecked<u64>(textSection.size()); // sh_size : u64
    sectionHeaderTable.writeLEUnchecked<u32>(SHN_UNDEF); // sh_link : u32 = 0
    sectionHeaderTable.writeLEUnchecked<u32>(0); // sh_info : u32 = 0
    sectionHeaderTable.writeLEUnchecked<u64>(16); // sh_addralign : u64 = 16 (reasonable code alignment requirement)
    sectionHeaderTable.writeLEUnchecked<u64>(0); // sh_entsize : u64 = 0 (we don't have any entries with regular size)
    assert(sectionHeaderTable.size() == 192);

    // Data section

    bytebuf dataSection;
    dataSection.reserve(roundUpToNearest<iptr>(this->data.size(), 64));

    u32 dataSectionOffset = textSectionOffset + textSection.size();
    assert(dataSectionOffset % 64 == 0);
    dataSection.writeUnchecked(this->data); // copy in our binary
    while (dataSection.size() % 64) // pad to multiple of 64 bytes
        dataSection.writeUnchecked<u8>(0);

    sectionHeaderTable.writeLEUnchecked<u32>(dataSectionNameOffset); // sh_name : u32 (.rodata in the section header string table)
    sectionHeaderTable.writeLEUnchecked<u32>(SHT_PROGBITS); // sh_type : u32 = SHT_PROGBITS (data is program data)
    sectionHeaderTable.writeLEUnchecked<u64>(SHF_ALLOC); // sh_flags : u64 (must be allocated, but not writable or executable)
    sectionHeaderTable.writeLEUnchecked<uptr>(0); // sh_addr : uptr = 0 (we're relocatable; someone will fill this in later)
    sectionHeaderTable.writeLEUnchecked<u64>(dataSectionOffset); // sh_offset : u64 (text comes after the section header table)
    sectionHeaderTable.writeLEUnchecked<u64>(dataSection.size()); // sh_size : u64
    sectionHeaderTable.writeLEUnchecked<u32>(SHN_UNDEF); // sh_link : u32 = 0
    sectionHeaderTable.writeLEUnchecked<u32>(0); // sh_info : u32 = 0
    sectionHeaderTable.writeLEUnchecked<u64>(16); // sh_addralign : u64 = 16 (minimal data alignment requirement)
    sectionHeaderTable.writeLEUnchecked<u64>(0); // sh_entsize : u64 = 0 (we don't have any entries with regular size)
    assert(sectionHeaderTable.size() == 256);

    // Static section

    bytebuf staticSection;
    staticSection.reserve(roundUpToNearest<iptr>(this->stat.size(), 64));

    u32 staticSectionOffset = dataSectionOffset + dataSection.size();
    assert(staticSectionOffset % 64 == 0);
    staticSection.writeUnchecked(this->stat); // copy in our binary
    while (staticSection.size() % 64) // pad to multiple of 64 bytes
        staticSection.writeUnchecked<u8>(0);

    sectionHeaderTable.writeLEUnchecked<u32>(staticSectionNameOffset); // sh_name : u32 (.data in the section header string table)
    sectionHeaderTable.writeLEUnchecked<u32>(SHT_PROGBITS); // sh_type : u32 = SHT_PROGBITS (static data is program data)
    sectionHeaderTable.writeLEUnchecked<u64>(SHF_WRITE | SHF_ALLOC); // sh_flags : u64 (must be allocated and writable, but not executable)
    sectionHeaderTable.writeLEUnchecked<uptr>(0); // sh_addr : uptr = 0 (we're relocatable; someone will fill this in later)
    sectionHeaderTable.writeLEUnchecked<u64>(staticSectionOffset); // sh_offset : u64 (text comes after the section header table)
    sectionHeaderTable.writeLEUnchecked<u64>(staticSection.size()); // sh_size : u64
    sectionHeaderTable.writeLEUnchecked<u32>(SHN_UNDEF); // sh_link : u32 = 0
    sectionHeaderTable.writeLEUnchecked<u32>(0); // sh_info : u32 = 0
    sectionHeaderTable.writeLEUnchecked<u64>(16); // sh_addralign : u64 = 16 (minimal data alignment requirement)
    sectionHeaderTable.writeLEUnchecked<u64>(0); // sh_entsize : u64 = 0 (we don't have any entries with regular size)
    assert(sectionHeaderTable.size() == 320);

    // String table

    bytebuf stringTable;
    u32 stringTableOffset = staticSectionOffset + staticSection.size();
    assert(stringTableOffset % 64 == 0);
    stringTable.write<u8>(0); // First string is always empty.

    vec<i32> symbols;
    vec<ELFSymbolInfo> symbolInfo;
    symbols.expandTo(symtab.strings.size(), -1);
    for (Def def : defs) if (def.sym >= symbols.size() || symbols[def.sym] == -1) {
        assert(def.hasSym); // Otherwise, it shouldn't have made it this far.
        symbols[def.sym] = symbolInfo.size();
        symbolInfo.push({ def.sym, 0, 0, (u32)def.offset, def.section, def.type });
    }
    for (Reloc reloc : relocs) if (symbols[reloc.sym] == -1) {
        assert(reloc.hasSym); // Otherwise, it shouldn't have made it this far.
        symbols[reloc.sym] = symbolInfo.size();
        symbolInfo.push({ reloc.sym, 0, 0, 0xffffffffu, CODE_SECTION, DEF_GLOBAL }); // If a symbol is referenced only in relocations, it must not be defined locally, so it must be global.
    }
    u32 cumulativeOffset = 1;
    u32 symbolIndex = 1;

    auto writeSymbolString = [&](ELFSymbolInfo& info) {
        auto str = symtab[info.sym];
        info.nameOffset = cumulativeOffset;
        info.index = symbolIndex ++;
        stringTable.write(str.data(), str.size());
        cumulativeOffset += str.size();
        if (str.last() != '\0')
            stringTable.write<u8>(0), cumulativeOffset ++;
    };
    for (auto& info : symbolInfo) if (info.type != DEF_GLOBAL)
        writeSymbolString(info);
    for (auto& info : symbolInfo) if (info.type == DEF_GLOBAL)
        writeSymbolString(info);
    while (stringTable.size() % 64) // pad to multiple of 64 bytes
        stringTable.write<u8>(0);

    sectionHeaderTable.writeLEUnchecked<u32>(stringTableNameOffset); // sh_name : u32 (.strtab in the section header string table)
    sectionHeaderTable.writeLEUnchecked<u32>(SHT_STRTAB); // sh_type : u32 = SHT_STRTAB
    sectionHeaderTable.writeLEUnchecked<u64>(SHF_MERGE | SHF_STRINGS); // sh_flags : u64 (it's a string table, and it's fine to merge it)
    sectionHeaderTable.writeLEUnchecked<uptr>(0); // sh_addr : uptr = 0 (we're relocatable; someone will fill this in later)
    sectionHeaderTable.writeLEUnchecked<u64>(stringTableOffset); // sh_offset : u64 (text comes after the section header table)
    sectionHeaderTable.writeLEUnchecked<u64>(stringTable.size()); // sh_size : u64
    sectionHeaderTable.writeLEUnchecked<u32>(SHN_UNDEF); // sh_link : u32 = 0
    sectionHeaderTable.writeLEUnchecked<u32>(0); // sh_info : u32 = 0
    sectionHeaderTable.writeLEUnchecked<u64>(0); // sh_addralign : u64 = 0 (we don't care about alignment)
    sectionHeaderTable.writeLEUnchecked<u64>(0); // sh_entsize : u64 = 0 (we don't have any entries with regular size)
    assert(sectionHeaderTable.size() == 384);

    // Symbol table

    bytebuf symbolTable;
    symbolTable.reserve(roundUpToNearest<iptr>(24 * (symbolInfo.size() + 1), 64));

    u32 symbolTableOffset = stringTableOffset + stringTable.size();
    assert(symbolTableOffset % 64 == 0);

    constexpr u8 STB_LOCAL = 0, STB_GLOBAL = 1, STB_WEAK = 2;
    constexpr u8 STT_NOTYPE = 0, STT_OBJECT = 1, STT_FUNC = 2, STT_TLS = 6;
    constexpr u8 STV_DEFAULT = 0, STV_INTERNAL = 1, STV_HIDDEN = 2, STV_EXPORTED = 4, STV_SINGLETON = 5;
    auto encodeSymbolInfo = [&](u8 binding, u8 type) -> u8 {
        return binding << 4 | type;
    };

    auto encodeDefType = [&](DefType type) -> u8 {
        switch (type) {
            case DEF_GLOBAL:
                return STB_GLOBAL;
            case DEF_LOCAL:
                return STB_LOCAL;
            case DEF_WEAK:
                return STB_WEAK;
        }
    };

    symbolTable.writeLEUnchecked<u32>(0); // st_name : u32, for the first symbol this is undefined
    symbolTable.writeUnchecked<u8>(encodeSymbolInfo(STB_LOCAL, STT_NOTYPE)); // st_info : u8, doesn't matter since this is a placeholder symbol
    symbolTable.writeUnchecked<u8>(STV_DEFAULT); // st_other : u8, again doesn't matter since this is a placeholder symbol
    symbolTable.writeLEUnchecked<u16>(0); // st_shndx : u16
    symbolTable.writeLEUnchecked<uptr>(0); // st_value : uptr = 0
    symbolTable.writeLEUnchecked<u64>(0); // st_size : u64 = 0

    auto generateSymbol = [&](ELFSymbolInfo& info) {
        u16 shndx;
        switch (info.section) {
            case CODE_SECTION: shndx = 2; break; // .text
            case DATA_SECTION: shndx = 3; break; // .rodata
            case STATIC_SECTION: shndx = 4; break; // .data
            default:
                unreachable("Shouldn't be able to define a symbol in any other section.");
        }
        if (info.offset == 0xffffffffu)
            shndx = SHN_UNDEF;

        symbolTable.writeLEUnchecked<u32>(info.nameOffset); // st_name : u32 (we generated strings in definition order, so st_name can just be the cumulative offset)
        symbolTable.writeUnchecked<u8>(encodeSymbolInfo(encodeDefType(info.type), STT_NOTYPE)); // st_info : u8
        symbolTable.writeUnchecked<u8>(STV_DEFAULT); // st_other : u8, we just use default visibility
        symbolTable.writeLEUnchecked<u16>(shndx);
        symbolTable.writeLEUnchecked<uptr>(info.offset == 0xffffffffu ? 0 : info.offset); // st_value : uptr (since it's relative to the start of the section, we can use the same offset)
        symbolTable.writeLEUnchecked<u64>(0); // st_size : u64 = 0
    };

    u32 firstGlobalSymbol = 1;
    for (auto& info : symbolInfo) if (info.type != DEF_GLOBAL)
        generateSymbol(info), firstGlobalSymbol ++;
    for (auto& info : symbolInfo) if (info.type == DEF_GLOBAL)
        generateSymbol(info);

    while (symbolTable.size() % 64) // pad to multiple of 64 bytes
        symbolTable.writeUnchecked<u8>(0);

    sectionHeaderTable.writeLEUnchecked<u32>(symbolTableNameOffset); // sh_name : u32 (.symtab in the section header string table)
    sectionHeaderTable.writeLEUnchecked<u32>(SHT_SYMTAB); // sh_type : u32 = SHT_SYMTAB
    sectionHeaderTable.writeLEUnchecked<u64>(SHF_MERGE | SHF_ALLOC); // sh_flags : u64 (it should be fine to merge our symbol table with others)
    sectionHeaderTable.writeLEUnchecked<uptr>(0); // sh_addr : uptr = 0 (we're relocatable; someone will fill this in later)
    sectionHeaderTable.writeLEUnchecked<u64>(symbolTableOffset); // sh_offset : u64 (text comes after the section header table)
    sectionHeaderTable.writeLEUnchecked<u64>(24 * (symbolInfo.size() + 1)); // sh_size : u64
    sectionHeaderTable.writeLEUnchecked<u32>(5); // sh_link : u32 = 5 (.strtab)
    sectionHeaderTable.writeLEUnchecked<u32>(firstGlobalSymbol); // sh_info : u32 = 2 (currently all symbols but the first are global; in the future this should be used to delineate local symbols)
    sectionHeaderTable.writeLEUnchecked<u64>(0); // sh_addralign : u64 = 8 (minimum alignment for entry type)
    sectionHeaderTable.writeLEUnchecked<u64>(24); // sh_entsize : u64 = 24 (size of each symbol entry)
    assert(sectionHeaderTable.size() == 448);

    // Relocation tables

    bytebuf textRelocationTable, dataRelocationTable, staticRelocationTable;
    u32 numTextRelocs = 0, numDataRelocs = 0, numStaticRelocs = 0;
    for (Reloc reloc : relocs) {
        bytebuf* buf;
        switch (reloc.section) {
            case CODE_SECTION: buf = &textRelocationTable; numTextRelocs ++; break;
            case DATA_SECTION: buf = &dataRelocationTable; numDataRelocs ++; break;
            case STATIC_SECTION: buf = &staticRelocationTable; numStaticRelocs ++; break;
            default:
                unreachable("Shouldn't have relocations to any other section.");
        }
        uptr offset = reloc.offset;
        u8 type = 0;
        u64 addend = 0;
        #ifdef RT_AMD64
            constexpr u8 R_AMD64_PC16 = 13, R_AMD64_PC32 = 2, R_AMD64_PC8 = 15, R_AMD64_PC64 = 24;
            constexpr u8 R_AMD64_16 = 12, R_AMD64_32 = 10, R_AMD64_8 = 14, R_AMD64_64 = 1;
            switch (reloc.kind) {
                case Reloc::REL32_LE_J_AMD64:
                case Reloc::REL32_LE_M_AMD64:
                    offset -= 4;
                    type = R_AMD64_PC32;
                    addend = -4;
                    break;
                default:
                    unreachable("Invalid relocation kind for AMD64");
            }
        #elif defined(RT_RISCV64)
            unreachable("TODO: Implement ELF relocations for RISC-V");
        #else
            #error "Unsupported architecture for ELF relocations."
        #endif
        buf->reserve(24);
        buf->writeLEUnchecked<uptr>(offset); // r_offset : uptr
        buf->writeLEUnchecked<u64>(u64(symbolInfo[symbols[reloc.sym]].index) << 32 | type); // r_info : u64
        buf->writeLEUnchecked<u64>(addend); // r_addend : u64
    }
    while (textRelocationTable.size() % 64) // pad to multiple of 64 bytes
        textRelocationTable.write<u8>(0);
    while (dataRelocationTable.size() % 64)
        dataRelocationTable.write<u8>(0);
    while (staticRelocationTable.size() % 64)
        staticRelocationTable.write<u8>(0);
    u32 textRelocationTableOffset = symbolTableOffset + symbolTable.size();
    u32 dataRelocationTableOffset = textRelocationTableOffset + textRelocationTable.size();
    u32 staticRelocationTableOffset = dataRelocationTableOffset + dataRelocationTable.size();
    assert(textRelocationTableOffset % 64 == 0);
    assert(dataRelocationTableOffset % 64 == 0);
    assert(staticRelocationTableOffset % 64 == 0);

    sectionHeaderTable.writeLEUnchecked<u32>(textRelocationTableNameOffset); // sh_name : u32 (.rela.text in the section header string table)
    sectionHeaderTable.writeLEUnchecked<u32>(SHT_RELA); // sh_type : u32 = SHT_RELA
    sectionHeaderTable.writeLEUnchecked<u64>(SHF_MERGE | SHF_INFO_LINK); // sh_flags : u64 (it should be fine to merge our relocations with others)
    sectionHeaderTable.writeLEUnchecked<uptr>(0); // sh_addr : uptr = 0 (we're relocatable; someone will fill this in later)
    sectionHeaderTable.writeLEUnchecked<u64>(textRelocationTableOffset); // sh_offset : u64
    sectionHeaderTable.writeLEUnchecked<u64>(24 * numTextRelocs); // sh_size : u64
    sectionHeaderTable.writeLEUnchecked<u32>(6); // sh_link : u32 = 6 (.symtab)
    sectionHeaderTable.writeLEUnchecked<u32>(2); // sh_info : u32 = 2 (relocations apply to .text section)
    sectionHeaderTable.writeLEUnchecked<u64>(0); // sh_addralign : u64 = 8 (minimum alignment for relocation type)
    sectionHeaderTable.writeLEUnchecked<u64>(24); // sh_entsize : u64 = 24 (size of each relocation entry)
    assert(sectionHeaderTable.size() == 512);

    sectionHeaderTable.writeLEUnchecked<u32>(dataRelocationTableNameOffset); // sh_name : u32 (.rela.rodata in the section header string table)
    sectionHeaderTable.writeLEUnchecked<u32>(SHT_RELA); // sh_type : u32 = SHT_RELA
    sectionHeaderTable.writeLEUnchecked<u64>(SHF_MERGE | SHF_INFO_LINK); // sh_flags : u64 (it should be fine to merge our relocations with others)
    sectionHeaderTable.writeLEUnchecked<uptr>(0); // sh_addr : uptr = 0 (we're relocatable; someone will fill this in later)
    sectionHeaderTable.writeLEUnchecked<u64>(dataRelocationTableOffset); // sh_offset : u64
    sectionHeaderTable.writeLEUnchecked<u64>(24 * numDataRelocs); // sh_size : u64
    sectionHeaderTable.writeLEUnchecked<u32>(6); // sh_link : u32 = 6 (.symtab)
    sectionHeaderTable.writeLEUnchecked<u32>(3); // sh_info : u32 = 3 (relocations apply to .rodata section)
    sectionHeaderTable.writeLEUnchecked<u64>(0); // sh_addralign : u64 = 8 (minimum alignment for relocation type)
    sectionHeaderTable.writeLEUnchecked<u64>(24); // sh_entsize : u64 = 24 (size of each relocation entry)
    assert(sectionHeaderTable.size() == 576);

    sectionHeaderTable.writeLEUnchecked<u32>(staticRelocationTableNameOffset); // sh_name : u32 (.rela.data in the section header string table)
    sectionHeaderTable.writeLEUnchecked<u32>(SHT_RELA); // sh_type : u32 = SHT_RELA
    sectionHeaderTable.writeLEUnchecked<u64>(SHF_MERGE | SHF_INFO_LINK); // sh_flags : u64 (it should be fine to merge our relocations with others)
    sectionHeaderTable.writeLEUnchecked<uptr>(0); // sh_addr : uptr = 0 (we're relocatable; someone will fill this in later)
    sectionHeaderTable.writeLEUnchecked<u64>(staticRelocationTableOffset); // sh_offset : u64
    sectionHeaderTable.writeLEUnchecked<u64>(24 * numStaticRelocs); // sh_size : u64
    sectionHeaderTable.writeLEUnchecked<u32>(6); // sh_link : u32 = 6 (.symtab)
    sectionHeaderTable.writeLEUnchecked<u32>(4); // sh_info : u32 = 2 (relocations apply to .data section)
    sectionHeaderTable.writeLEUnchecked<u64>(0); // sh_addralign : u64 = 8 (minimum alignment for relocation type)
    sectionHeaderTable.writeLEUnchecked<u64>(24); // sh_entsize : u64 = 24 (size of each relocation entry)
    assert(sectionHeaderTable.size() == 640);

    write(file, objectHeader);
    write(file, sectionHeaderTable);
    write(file, sectionHeaderStringTable);
    write(file, textSection);
    write(file, dataSection);
    write(file, staticSection);
    write(file, stringTable);
    write(file, symbolTable);
    write(file, textRelocationTable);
    write(file, dataRelocationTable);
    write(file, staticRelocationTable);
}

void LinkedAssembly::writeELFExecutable(fd file) {
    unreachable("TODO: Executable generation");
}
