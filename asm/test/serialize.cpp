#include "util/test/harness.h"
#include "asm/arch/amd64.h"
#include "util/io.h"

TEST(assembly_round_trip_to_file) {
    SymbolTable table;
    Assembly as(table);
    using ASM = AMD64LinuxAssembler;

    ASM::global(as, as.symtab["add"]);
    ASM::enter(as);
    ASM::add64(as, GP(ASM::RAX), GP(ASM::RDI), GP(ASM::RSI));
    ASM::leave(as);
    ASM::ret(as);

    as.def(DATA_SECTION, DEF_GLOBAL, as.symtab["nums"]);
    as.data.writeLE<double>(1.0);
    as.data.writeLE<double>(2.0);
    as.data.writeLE<double>(3.0);

    file::fd output = file::open(cstring("bin/test.as"), file::WRITE);
    as.serialize(output);
    file::close(output);

    file::fd input = file::open(cstring("bin/test.as"), file::READ);
    as.deserialize(input);
    file::close(input);

    auto linked = as.link();
    linked.load();
    auto add = linked.lookup<i32(i32, i32)>("add");
    ASSERT_EQUAL(add(1, 2), 3);
}

TEST(assembly_serialize_hello_world) {
    SymbolTable table;
    Assembly as(table);
    using ASM = AMD64LinuxAssembler;

    ASM::global(as, as.symtab["hello"]);
    ASM::mov64(as, GP(ASM::RDI), Imm(1)); // stdout
    ASM::la(as, GP(ASM::RSI), Label(as.symtab["msg"]));
    ASM::mov64(as, GP(ASM::RDX), Imm(12));
    ASM::mov64(as, GP(ASM::RAX), Imm(1));
    as.code.write<u8>(0x0f);
    as.code.write<u8>(0x05);
    ASM::ret(as);

    as.def(DATA_SECTION, DEF_GLOBAL, as.symtab["msg"]);
    as.data.write("hello world\n", 12);

    file::fd output = file::open(cstring("bin/hello.as"), file::WRITE);
    as.serialize(output);
    file::close(output);

    file::fd input = file::open(cstring("bin/hello.as"), file::READ);
    as.deserialize(input);
    file::close(input);

    auto linked = as.link();
    linked.load();
    auto hello = linked.lookup<void()>("hello");
    hello();
}