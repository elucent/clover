#include "jasmine/test/common.h"

FOR_EACH_INT(DEFINE_TEST_OP, add_constants, 3, add, ADD,
    auto bb0 = fn.addBlock();
    bb0.addNode(opcode, type, fn.variable("x"), fn.intConst(1), fn.intConst(2));
    bb0.addNode(Opcode::RET, type, fn.variable("x"));
)

FOR_EACH_INT(DEFINE_TEST_OP, add_variables, 3, add, ADD,
    auto bb0 = fn.addBlock();
    bb0.addNode(Opcode::MOV, type, fn.variable("x"), fn.intConst(1));
    bb0.addNode(Opcode::MOV, type, fn.variable("y"), fn.intConst(2));
    bb0.addNode(opcode, type, fn.variable("x"), fn.variable("x"), fn.variable("y"));
    bb0.addNode(Opcode::RET, type, fn.variable("x"));
)

FOR_EACH_UINT(DEFINE_TEST_OP, sub_constants, 8, sub, SUB,
    auto bb0 = fn.addBlock();
    bb0.addNode(opcode, type, fn.variable("x"), fn.intConst(17), fn.intConst(9));
    bb0.addNode(Opcode::RET, type, fn.variable("x"));
)

FOR_EACH_UINT(DEFINE_TEST_OP, sub_variables, 8, sub, SUB,
    auto bb0 = fn.addBlock();
    bb0.addNode(Opcode::MOV, type, fn.variable("x"), fn.intConst(17));
    bb0.addNode(Opcode::MOV, type, fn.variable("y"), fn.intConst(9));
    bb0.addNode(opcode, type, fn.variable("x"), fn.variable("x"), fn.variable("y"));
    bb0.addNode(Opcode::RET, type, fn.variable("x"));
)

FOR_EACH_SINT(DEFINE_TEST_OP, sub_constants, -8, sub, SUB,
    auto bb0 = fn.addBlock();
    bb0.addNode(opcode, type, fn.variable("x"), fn.intConst(9), fn.intConst(17));
    bb0.addNode(Opcode::RET, type, fn.variable("x"));
)

FOR_EACH_SINT(DEFINE_TEST_OP, sub_variables, -8, sub, SUB,
    auto bb0 = fn.addBlock();
    bb0.addNode(Opcode::MOV, type, fn.variable("x"), fn.intConst(9));
    bb0.addNode(Opcode::MOV, type, fn.variable("y"), fn.intConst(17));
    bb0.addNode(opcode, type, fn.variable("x"), fn.variable("x"), fn.variable("y"));
    bb0.addNode(Opcode::RET, type, fn.variable("x"));
)

FOR_EACH_UINT(DEFINE_TEST_OP, mul_constants, 153, mul, MUL,
    auto bb0 = fn.addBlock();
    bb0.addNode(opcode, type, fn.variable("x"), fn.intConst(17), fn.intConst(9));
    bb0.addNode(Opcode::RET, type, fn.variable("x"));
)

FOR_EACH_UINT(DEFINE_TEST_OP, mul_variables, 153, mul, MUL,
    auto bb0 = fn.addBlock();
    bb0.addNode(Opcode::MOV, type, fn.variable("x"), fn.intConst(17));
    bb0.addNode(Opcode::MOV, type, fn.variable("y"), fn.intConst(9));
    bb0.addNode(opcode, type, fn.variable("x"), fn.variable("x"), fn.variable("y"));
    bb0.addNode(Opcode::RET, type, fn.variable("x"));
)

FOR_EACH_SINT(DEFINE_TEST_OP, mul_constants, -36, mul, MUL,
    auto bb0 = fn.addBlock();
    bb0.addNode(opcode, type, fn.variable("x"), fn.intConst(9), fn.intConst(-4));
    bb0.addNode(Opcode::RET, type, fn.variable("x"));
)

FOR_EACH_SINT(DEFINE_TEST_OP, mul_variables, -36, mul, MUL,
    auto bb0 = fn.addBlock();
    bb0.addNode(Opcode::MOV, type, fn.variable("x"), fn.intConst(-9));
    bb0.addNode(Opcode::MOV, type, fn.variable("y"), fn.intConst(4));
    bb0.addNode(opcode, type, fn.variable("x"), fn.variable("x"), fn.variable("y"));
    bb0.addNode(Opcode::RET, type, fn.variable("x"));
)

FOR_EACH_UINT(DEFINE_TEST_OP, div_constants, 15, div, DIV,
    auto bb0 = fn.addBlock();
    bb0.addNode(opcode, type, fn.variable("x"), fn.intConst(173), fn.intConst(11));
    bb0.addNode(Opcode::RET, type, fn.variable("x"));
)

FOR_EACH_UINT(DEFINE_TEST_OP, div_variables, 15, div, DIV,
    auto bb0 = fn.addBlock();
    bb0.addNode(Opcode::MOV, type, fn.variable("x"), fn.intConst(173));
    bb0.addNode(Opcode::MOV, type, fn.variable("y"), fn.intConst(11));
    bb0.addNode(opcode, type, fn.variable("x"), fn.variable("x"), fn.variable("y"));
    bb0.addNode(Opcode::RET, type, fn.variable("x"));
)

FOR_EACH_UINT(DEFINE_TEST_OP, div_constants_po2, 5, div, DIV,
    auto bb0 = fn.addBlock();
    bb0.addNode(opcode, type, fn.variable("x"), fn.intConst(169), fn.intConst(32));
    bb0.addNode(Opcode::RET, type, fn.variable("x"));
)

FOR_EACH_UINT(DEFINE_TEST_OP, div_variables_po2, 5, div, DIV,
    auto bb0 = fn.addBlock();
    bb0.addNode(Opcode::MOV, type, fn.variable("x"), fn.intConst(169));
    bb0.addNode(Opcode::MOV, type, fn.variable("y"), fn.intConst(32));
    bb0.addNode(opcode, type, fn.variable("x"), fn.variable("x"), fn.variable("y"));
    bb0.addNode(Opcode::RET, type, fn.variable("x"));
)

FOR_EACH_SINT(DEFINE_TEST_OP, div_constants, 3, div, DIV,
    auto bb0 = fn.addBlock();
    bb0.addNode(opcode, type, fn.variable("x"), fn.intConst(-28), fn.intConst(-8));
    bb0.addNode(Opcode::RET, type, fn.variable("x"));
)

FOR_EACH_SINT(DEFINE_TEST_OP, div_variables, 3, div, DIV,
    auto bb0 = fn.addBlock();
    bb0.addNode(Opcode::MOV, type, fn.variable("x"), fn.intConst(-28));
    bb0.addNode(Opcode::MOV, type, fn.variable("y"), fn.intConst(-8));
    bb0.addNode(opcode, type, fn.variable("x"), fn.variable("x"), fn.variable("y"));
    bb0.addNode(Opcode::RET, type, fn.variable("x"));
)

FOR_EACH_UINT(DEFINE_TEST_OP, rem_constants, 18, rem, REM,
    auto bb0 = fn.addBlock();
    bb0.addNode(opcode, type, fn.variable("x"), fn.intConst(138), fn.intConst(24));
    bb0.addNode(Opcode::RET, type, fn.variable("x"));
)

FOR_EACH_UINT(DEFINE_TEST_OP, rem_variables, 18, rem, REM,
    auto bb0 = fn.addBlock();
    bb0.addNode(Opcode::MOV, type, fn.variable("x"), fn.intConst(138));
    bb0.addNode(Opcode::MOV, type, fn.variable("y"), fn.intConst(24));
    bb0.addNode(opcode, type, fn.variable("x"), fn.variable("x"), fn.variable("y"));
    bb0.addNode(Opcode::RET, type, fn.variable("x"));
)

FOR_EACH_UINT(DEFINE_TEST_OP, rem_constants_po2, 9, rem, REM,
    auto bb0 = fn.addBlock();
    bb0.addNode(opcode, type, fn.variable("x"), fn.intConst(233), fn.intConst(16));
    bb0.addNode(Opcode::RET, type, fn.variable("x"));
)

FOR_EACH_UINT(DEFINE_TEST_OP, rem_variables_po2, 9, rem, REM,
    auto bb0 = fn.addBlock();
    bb0.addNode(Opcode::MOV, type, fn.variable("x"), fn.intConst(233));
    bb0.addNode(Opcode::MOV, type, fn.variable("y"), fn.intConst(16));
    bb0.addNode(opcode, type, fn.variable("x"), fn.variable("x"), fn.variable("y"));
    bb0.addNode(Opcode::RET, type, fn.variable("x"));
)

FOR_EACH_SINT(DEFINE_TEST_OP, rem_constants, -5, rem, REM,
    auto bb0 = fn.addBlock();
    bb0.addNode(opcode, type, fn.variable("x"), fn.intConst(-97), fn.intConst(23));
    bb0.addNode(Opcode::RET, type, fn.variable("x"));
)

FOR_EACH_SINT(DEFINE_TEST_OP, rem_variables, -5, rem, REM,
    auto bb0 = fn.addBlock();
    bb0.addNode(Opcode::MOV, type, fn.variable("x"), fn.intConst(-97));
    bb0.addNode(Opcode::MOV, type, fn.variable("y"), fn.intConst(23));
    bb0.addNode(opcode, type, fn.variable("x"), fn.variable("x"), fn.variable("y"));
    bb0.addNode(Opcode::RET, type, fn.variable("x"));
)

FOR_EACH_SINT(DEFINE_TEST_OP, neg_positive, -87, neg, NEG,
    auto bb0 = fn.addBlock();
    bb0.addNode(Opcode::MOV, type, fn.variable("x"), fn.intConst(87));
    bb0.addNode(opcode, type, fn.variable("x"), fn.variable("x"));
    bb0.addNode(Opcode::RET, type, fn.variable("x"));
)

FOR_EACH_SINT(DEFINE_TEST_OP, neg_negative, 87, neg, NEG,
    auto bb0 = fn.addBlock();
    bb0.addNode(Opcode::MOV, type, fn.variable("x"), fn.intConst(-87));
    bb0.addNode(opcode, type, fn.variable("x"), fn.variable("x"));
    bb0.addNode(Opcode::RET, type, fn.variable("x"));
)