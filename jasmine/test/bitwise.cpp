#include "jasmine/test/common.h"

FOR_EACH_INT(DEFINE_TEST_OP, and_constants, 5, and, AND,
    auto bb0 = fn.addBlock();
    bb0.addNode(opcode, type, fn.variable("x"), fn.intConst(13), fn.intConst(7));
    bb0.addNode(Opcode::RET, type, fn.variable("x"));
)

FOR_EACH_INT(DEFINE_TEST_OP, and_variables, 5, and, AND,
    auto bb0 = fn.addBlock();
    bb0.addNode(Opcode::MOV, type, fn.variable("x"), fn.intConst(13));
    bb0.addNode(Opcode::MOV, type, fn.variable("y"), fn.intConst(7));
    bb0.addNode(opcode, type, fn.variable("x"), fn.variable("x"), fn.variable("y"));
    bb0.addNode(Opcode::RET, type, fn.variable("x"));
)

FOR_EACH_INT(DEFINE_TEST_OP, xor_constants, 8, xor, XOR,
    auto bb0 = fn.addBlock();
    bb0.addNode(opcode, type, fn.variable("x"), fn.intConst(7), fn.intConst(15));
    bb0.addNode(Opcode::RET, type, fn.variable("x"));
)

FOR_EACH_INT(DEFINE_TEST_OP, xor_variables, 8, xor, XOR,
    auto bb0 = fn.addBlock();
    bb0.addNode(Opcode::MOV, type, fn.variable("x"), fn.intConst(7));
    bb0.addNode(Opcode::MOV, type, fn.variable("y"), fn.intConst(15));
    bb0.addNode(opcode, type, fn.variable("x"), fn.variable("x"), fn.variable("y"));
    bb0.addNode(Opcode::RET, type, fn.variable("x"));
)

FOR_EACH_INT(DEFINE_TEST_OP, or_constants, 53, or, OR,
    auto bb0 = fn.addBlock();
    bb0.addNode(opcode, type, fn.variable("x"), fn.intConst(21), fn.intConst(48));
    bb0.addNode(Opcode::RET, type, fn.variable("x"));
)

FOR_EACH_INT(DEFINE_TEST_OP, or_variables, 53, or, OR,
    auto bb0 = fn.addBlock();
    bb0.addNode(Opcode::MOV, type, fn.variable("x"), fn.intConst(21));
    bb0.addNode(Opcode::MOV, type, fn.variable("y"), fn.intConst(48));
    bb0.addNode(opcode, type, fn.variable("x"), fn.variable("x"), fn.variable("y"));
    bb0.addNode(Opcode::RET, type, fn.variable("x"));
)

FOR_EACH_INT(DEFINE_TEST_OP, shl_constants, 120, shl, SHL,
    auto bb0 = fn.addBlock();
    bb0.addNode(opcode, type, fn.variable("x"), fn.intConst(15), fn.intConst(3));
    bb0.addNode(Opcode::RET, type, fn.variable("x"));
)

FOR_EACH_INT(DEFINE_TEST_OP, shl_variables, 120, shl, SHL,
    auto bb0 = fn.addBlock();
    bb0.addNode(Opcode::MOV, type, fn.variable("x"), fn.intConst(15));
    bb0.addNode(Opcode::MOV, type, fn.variable("y"), fn.intConst(3));
    bb0.addNode(opcode, type, fn.variable("x"), fn.variable("x"), fn.variable("y"));
    bb0.addNode(Opcode::RET, type, fn.variable("x"));
)

FOR_EACH_INT(DEFINE_TEST_OP, shr_constants, 26, shr, SHR,
    auto bb0 = fn.addBlock();
    bb0.addNode(opcode, type, fn.variable("x"), fn.intConst(104), fn.intConst(2));
    bb0.addNode(Opcode::RET, type, fn.variable("x"));
)

FOR_EACH_INT(DEFINE_TEST_OP, shr_variables, 26, shr, SHR,
    auto bb0 = fn.addBlock();
    bb0.addNode(Opcode::MOV, type, fn.variable("x"), fn.intConst(104));
    bb0.addNode(Opcode::MOV, type, fn.variable("y"), fn.intConst(2));
    bb0.addNode(opcode, type, fn.variable("x"), fn.variable("x"), fn.variable("y"));
    bb0.addNode(Opcode::RET, type, fn.variable("x"));
)

DEFINE_TEST_OP(shr_top_bit, 0x20, u8, U8, shr, SHR,
    auto bb0 = fn.addBlock();
    bb0.addNode(Opcode::MOV, type, fn.variable("x"), fn.intConst(0x80));
    bb0.addNode(Opcode::MOV, type, fn.variable("y"), fn.intConst(2));
    bb0.addNode(opcode, type, fn.variable("x"), fn.variable("x"), fn.variable("y"));
    bb0.addNode(Opcode::RET, type, fn.variable("x"));
)

DEFINE_TEST_OP(shr_top_bit, 0x2000, u16, U16, shr, SHR,
    auto bb0 = fn.addBlock();
    bb0.addNode(Opcode::MOV, type, fn.variable("x"), fn.intConst(0x8000));
    bb0.addNode(Opcode::MOV, type, fn.variable("y"), fn.intConst(2));
    bb0.addNode(opcode, type, fn.variable("x"), fn.variable("x"), fn.variable("y"));
    bb0.addNode(Opcode::RET, type, fn.variable("x"));
)

DEFINE_TEST_OP(shr_top_bit, 0x20000000, u32, U32, shr, SHR,
    auto bb0 = fn.addBlock();
    bb0.addNode(Opcode::MOV, type, fn.variable("x"), fn.intConst(0x80000000));
    bb0.addNode(Opcode::MOV, type, fn.variable("y"), fn.intConst(2));
    bb0.addNode(opcode, type, fn.variable("x"), fn.variable("x"), fn.variable("y"));
    bb0.addNode(Opcode::RET, type, fn.variable("x"));
)

DEFINE_TEST_OP(shr_top_bit, 0x2000000000000000ull, u64, U64, shr, SHR,
    auto bb0 = fn.addBlock();
    bb0.addNode(Opcode::MOV, type, fn.variable("x"), fn.intConst(0x8000000000000000ull));
    bb0.addNode(Opcode::MOV, type, fn.variable("y"), fn.intConst(2));
    bb0.addNode(opcode, type, fn.variable("x"), fn.variable("x"), fn.variable("y"));
    bb0.addNode(Opcode::RET, type, fn.variable("x"));
)

DEFINE_TEST_OP(shr_top_bit, -0x20, i8, I8, shr, SHR,
    auto bb0 = fn.addBlock();
    bb0.addNode(Opcode::MOV, type, fn.variable("x"), fn.intConst(-0x80));
    bb0.addNode(Opcode::MOV, type, fn.variable("y"), fn.intConst(2));
    bb0.addNode(opcode, type, fn.variable("x"), fn.variable("x"), fn.variable("y"));
    bb0.addNode(Opcode::RET, type, fn.variable("x"));
)

DEFINE_TEST_OP(shr_top_bit, -0x2000, i16, I16, shr, SHR,
    auto bb0 = fn.addBlock();
    bb0.addNode(Opcode::MOV, type, fn.variable("x"), fn.intConst(-0x8000));
    bb0.addNode(Opcode::MOV, type, fn.variable("y"), fn.intConst(2));
    bb0.addNode(opcode, type, fn.variable("x"), fn.variable("x"), fn.variable("y"));
    bb0.addNode(Opcode::RET, type, fn.variable("x"));
)

DEFINE_TEST_OP(shr_top_bit, -0x20000000l, i32, I32, shr, SHR,
    auto bb0 = fn.addBlock();
    bb0.addNode(Opcode::MOV, type, fn.variable("x"), fn.intConst(-0x80000000l));
    bb0.addNode(Opcode::MOV, type, fn.variable("y"), fn.intConst(2));
    bb0.addNode(opcode, type, fn.variable("x"), fn.variable("x"), fn.variable("y"));
    bb0.addNode(Opcode::RET, type, fn.variable("x"));
)

DEFINE_TEST_OP(shr_top_bit, -0x2000000000000000ll, i64, I64, shr, SHR,
    auto bb0 = fn.addBlock();
    bb0.addNode(Opcode::MOV, type, fn.variable("x"), fn.intConst(-0x8000000000000000ll));
    bb0.addNode(Opcode::MOV, type, fn.variable("y"), fn.intConst(2));
    bb0.addNode(opcode, type, fn.variable("x"), fn.variable("x"), fn.variable("y"));
    bb0.addNode(Opcode::RET, type, fn.variable("x"));
)