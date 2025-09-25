#include "util/test/harness.h"
#include "asm/arch/amd64.h"
#include "util/io.h"

TEST(assembly_serialize_hello_world_to_elf) {
    SymbolTable table;
    Assembly as(table);
    using ASM = AMD64LinuxAssembler;

    ASM::global(as, Label::fromSym(as.symtab["hello"]));
    ASM::mov64(as, GP(ASM::RDI), Imm(1)); // stdout
    ASM::la(as, GP(ASM::RSI), LocalLabel(as.symtab["msg"]));
    ASM::mov64(as, GP(ASM::RDX), Imm(12));
    ASM::mov64(as, GP(ASM::RAX), Imm(1));
    as.code.write<u8>(0x0f);
    as.code.write<u8>(0x05);
    ASM::ret(as);

    as.def(DATA_SECTION, DEF_GLOBAL, Label::fromSym(as.symtab["msg"]));
    as.data.write("hello world\n", 12);

    file::fd output = file::open(cstring("bin/hello.o"), file::WRITE);
    as.writeELFObject(output);
    file::close(output);
}