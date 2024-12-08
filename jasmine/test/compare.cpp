#include "jasmine/test/common.h"

FOR_EACH_INT(DEFINE_TEST_BOOL_OP, lt_constants, 1, is_lt, IS_LT,
    auto bb0 = fn.addBlock();
    bb0.addNode(opcode, type, fn.variable("z"), fn.intConst(42), fn.intConst(67));
    bb0.addNode(Opcode::RET, BOOL, fn.variable("z"));
)

FOR_EACH_INT(DEFINE_TEST_BOOL_OP, lt_variables, 1, is_lt, IS_LT,
    auto bb0 = fn.addBlock();
    bb0.addNode(Opcode::MOV, type, fn.variable("x"), fn.intConst(42));
    bb0.addNode(Opcode::MOV, type, fn.variable("y"), fn.intConst(57));
    bb0.addNode(opcode, type, fn.variable("z"), fn.variable("x"), fn.variable("y"));
    bb0.addNode(Opcode::RET, BOOL, fn.variable("z"));
)

FOR_EACH_INT(DEFINE_TEST_BOOL_OP, not_lt_constants, 0, is_lt, IS_LT,
    auto bb0 = fn.addBlock();
    bb0.addNode(opcode, type, fn.variable("z"), fn.intConst(45), fn.intConst(13));
    bb0.addNode(Opcode::RET, BOOL, fn.variable("z"));
)

FOR_EACH_INT(DEFINE_TEST_BOOL_OP, not_lt_variables, 0, is_lt, IS_LT,
    auto bb0 = fn.addBlock();
    bb0.addNode(Opcode::MOV, type, fn.variable("x"), fn.intConst(45));
    bb0.addNode(Opcode::MOV, type, fn.variable("y"), fn.intConst(13));
    bb0.addNode(opcode, type, fn.variable("z"), fn.variable("x"), fn.variable("y"));
    bb0.addNode(Opcode::RET, BOOL, fn.variable("z"));
)

FOR_EACH_INT(DEFINE_TEST_BOOL_OP, le_constants, 1, is_le, IS_LE,
    auto bb0 = fn.addBlock();
    bb0.addNode(opcode, type, fn.variable("x"), fn.intConst(42), fn.intConst(67));
    bb0.addNode(Opcode::RET, BOOL, fn.variable("x"));
)

FOR_EACH_INT(DEFINE_TEST_BOOL_OP, le_variables, 1, is_le, IS_LE,
    auto bb0 = fn.addBlock();
    bb0.addNode(Opcode::MOV, type, fn.variable("x"), fn.intConst(42));
    bb0.addNode(Opcode::MOV, type, fn.variable("y"), fn.intConst(57));
    bb0.addNode(opcode, type, fn.variable("z"), fn.variable("x"), fn.variable("y"));
    bb0.addNode(Opcode::RET, BOOL, fn.variable("z"));
)

FOR_EACH_INT(DEFINE_TEST_BOOL_OP, not_le_constants, 0, is_le, IS_LE,
    auto bb0 = fn.addBlock();
    bb0.addNode(opcode, type, fn.variable("z"), fn.intConst(45), fn.intConst(13));
    bb0.addNode(Opcode::RET, BOOL, fn.variable("z"));
)

FOR_EACH_INT(DEFINE_TEST_BOOL_OP, not_le_variables, 0, is_le, IS_LE,
    auto bb0 = fn.addBlock();
    bb0.addNode(Opcode::MOV, type, fn.variable("x"), fn.intConst(45));
    bb0.addNode(Opcode::MOV, type, fn.variable("y"), fn.intConst(13));
    bb0.addNode(opcode, type, fn.variable("z"), fn.variable("x"), fn.variable("y"));
    bb0.addNode(Opcode::RET, BOOL, fn.variable("z"));
)

FOR_EACH_INT(DEFINE_TEST_BOOL_OP, equal_le_constants, 1, is_le, IS_LE,
    auto bb0 = fn.addBlock();
    bb0.addNode(opcode, type, fn.variable("z"), fn.intConst(24), fn.intConst(24));
    bb0.addNode(Opcode::RET, BOOL, fn.variable("z"));
)

FOR_EACH_INT(DEFINE_TEST_BOOL_OP, equal_le_variables, 1, is_le, IS_LE,
    auto bb0 = fn.addBlock();
    bb0.addNode(Opcode::MOV, type, fn.variable("x"), fn.intConst(24));
    bb0.addNode(Opcode::MOV, type, fn.variable("y"), fn.intConst(24));
    bb0.addNode(opcode, type, fn.variable("z"), fn.variable("x"), fn.variable("y"));
    bb0.addNode(Opcode::RET, BOOL, fn.variable("z"));
)

FOR_EACH_INT(DEFINE_TEST_BOOL_OP, gt_constants, 1, is_gt, IS_GT,
    auto bb0 = fn.addBlock();
    bb0.addNode(opcode, type, fn.variable("z"), fn.intConst(9), fn.intConst(6));
    bb0.addNode(Opcode::RET, BOOL, fn.variable("z"));
)

FOR_EACH_INT(DEFINE_TEST_BOOL_OP, gt_variables, 1, is_gt, IS_GT,
    auto bb0 = fn.addBlock();
    bb0.addNode(Opcode::MOV, type, fn.variable("x"), fn.intConst(9));
    bb0.addNode(Opcode::MOV, type, fn.variable("y"), fn.intConst(6));
    bb0.addNode(opcode, type, fn.variable("z"), fn.variable("x"), fn.variable("y"));
    bb0.addNode(Opcode::RET, BOOL, fn.variable("z"));
)

FOR_EACH_INT(DEFINE_TEST_BOOL_OP, not_gt_constants, 0, is_gt, IS_GT,
    auto bb0 = fn.addBlock();
    bb0.addNode(opcode, type, fn.variable("z"), fn.intConst(13), fn.intConst(17));
    bb0.addNode(Opcode::RET, BOOL, fn.variable("z"));
)

FOR_EACH_INT(DEFINE_TEST_BOOL_OP, not_gt_variables, 0, is_gt, IS_GT,
    auto bb0 = fn.addBlock();
    bb0.addNode(Opcode::MOV, type, fn.variable("x"), fn.intConst(13));
    bb0.addNode(Opcode::MOV, type, fn.variable("y"), fn.intConst(17));
    bb0.addNode(opcode, type, fn.variable("z"), fn.variable("x"), fn.variable("y"));
    bb0.addNode(Opcode::RET, BOOL, fn.variable("z"));
)

FOR_EACH_INT(DEFINE_TEST_BOOL_OP, ge_constants, 1, is_ge, IS_GE,
    auto bb0 = fn.addBlock();
    bb0.addNode(opcode, type, fn.variable("x"), fn.intConst(109), fn.intConst(90));
    bb0.addNode(Opcode::RET, BOOL, fn.variable("x"));
)

FOR_EACH_INT(DEFINE_TEST_BOOL_OP, ge_variables, 1, is_ge, IS_GE,
    auto bb0 = fn.addBlock();
    bb0.addNode(Opcode::MOV, type, fn.variable("x"), fn.intConst(109));
    bb0.addNode(Opcode::MOV, type, fn.variable("y"), fn.intConst(90));
    bb0.addNode(opcode, type, fn.variable("z"), fn.variable("x"), fn.variable("y"));
    bb0.addNode(Opcode::RET, BOOL, fn.variable("z"));
)

FOR_EACH_INT(DEFINE_TEST_BOOL_OP, not_ge_constants, 0, is_ge, IS_GE,
    auto bb0 = fn.addBlock();
    bb0.addNode(opcode, type, fn.variable("z"), fn.intConst(9), fn.intConst(80));
    bb0.addNode(Opcode::RET, BOOL, fn.variable("z"));
)

FOR_EACH_INT(DEFINE_TEST_BOOL_OP, not_ge_variables, 0, is_ge, IS_GE,
    auto bb0 = fn.addBlock();
    bb0.addNode(Opcode::MOV, type, fn.variable("x"), fn.intConst(9));
    bb0.addNode(Opcode::MOV, type, fn.variable("y"), fn.intConst(80));
    bb0.addNode(opcode, type, fn.variable("z"), fn.variable("x"), fn.variable("y"));
    bb0.addNode(Opcode::RET, BOOL, fn.variable("z"));
)

FOR_EACH_INT(DEFINE_TEST_BOOL_OP, equal_ge_constants, 1, is_ge, IS_GE,
    auto bb0 = fn.addBlock();
    bb0.addNode(opcode, type, fn.variable("z"), fn.intConst(112), fn.intConst(112));
    bb0.addNode(Opcode::RET, BOOL, fn.variable("z"));
)

FOR_EACH_INT(DEFINE_TEST_BOOL_OP, equal_ge_variables, 1, is_ge, IS_GE,
    auto bb0 = fn.addBlock();
    bb0.addNode(Opcode::MOV, type, fn.variable("x"), fn.intConst(112));
    bb0.addNode(Opcode::MOV, type, fn.variable("y"), fn.intConst(112));
    bb0.addNode(opcode, type, fn.variable("z"), fn.variable("x"), fn.variable("y"));
    bb0.addNode(Opcode::RET, BOOL, fn.variable("z"));
)

FOR_EACH_INT(DEFINE_TEST_BOOL_OP, eq_constants, 1, is_eq, IS_EQ,
    auto bb0 = fn.addBlock();
    bb0.addNode(opcode, type, fn.variable("z"), fn.intConst(63), fn.intConst(63));
    bb0.addNode(Opcode::RET, BOOL, fn.variable("z"));
)

FOR_EACH_INT(DEFINE_TEST_BOOL_OP, eq_variables, 1, is_eq, IS_EQ,
    auto bb0 = fn.addBlock();
    bb0.addNode(Opcode::MOV, type, fn.variable("x"), fn.intConst(63));
    bb0.addNode(Opcode::MOV, type, fn.variable("y"), fn.intConst(63));
    bb0.addNode(opcode, type, fn.variable("z"), fn.variable("x"), fn.variable("y"));
    bb0.addNode(Opcode::RET, BOOL, fn.variable("z"));
)

FOR_EACH_INT(DEFINE_TEST_BOOL_OP, not_eq_constants, 0, is_eq, IS_EQ,
    auto bb0 = fn.addBlock();
    bb0.addNode(opcode, type, fn.variable("z"), fn.intConst(17), fn.intConst(18));
    bb0.addNode(Opcode::RET, BOOL, fn.variable("z"));
)

FOR_EACH_INT(DEFINE_TEST_BOOL_OP, not_eq_variables, 0, is_eq, IS_EQ,
    auto bb0 = fn.addBlock();
    bb0.addNode(Opcode::MOV, type, fn.variable("x"), fn.intConst(17));
    bb0.addNode(Opcode::MOV, type, fn.variable("y"), fn.intConst(18));
    bb0.addNode(opcode, type, fn.variable("z"), fn.variable("x"), fn.variable("y"));
    bb0.addNode(Opcode::RET, BOOL, fn.variable("z"));
)

FOR_EACH_INT(DEFINE_TEST_BOOL_OP, ne_constants, 1, is_ne, IS_NE,
    auto bb0 = fn.addBlock();
    bb0.addNode(opcode, type, fn.variable("z"), fn.intConst(83), fn.intConst(42));
    bb0.addNode(Opcode::RET, BOOL, fn.variable("z"));
)

FOR_EACH_INT(DEFINE_TEST_BOOL_OP, ne_variables, 1, is_ne, IS_NE,
    auto bb0 = fn.addBlock();
    bb0.addNode(Opcode::MOV, type, fn.variable("x"), fn.intConst(83));
    bb0.addNode(Opcode::MOV, type, fn.variable("y"), fn.intConst(42));
    bb0.addNode(opcode, type, fn.variable("z"), fn.variable("x"), fn.variable("y"));
    bb0.addNode(Opcode::RET, BOOL, fn.variable("z"));
)

FOR_EACH_INT(DEFINE_TEST_BOOL_OP, not_ne_constants, 0, is_ne, IS_NE,
    auto bb0 = fn.addBlock();
    bb0.addNode(opcode, type, fn.variable("z"), fn.intConst(3), fn.intConst(3));
    bb0.addNode(Opcode::RET, BOOL, fn.variable("z"));
)

FOR_EACH_INT(DEFINE_TEST_BOOL_OP, not_ne_variables, 0, is_ne, IS_NE,
    auto bb0 = fn.addBlock();
    bb0.addNode(Opcode::MOV, type, fn.variable("x"), fn.intConst(3));
    bb0.addNode(Opcode::MOV, type, fn.variable("y"), fn.intConst(3));
    bb0.addNode(opcode, type, fn.variable("z"), fn.variable("x"), fn.variable("y"));
    bb0.addNode(Opcode::RET, BOOL, fn.variable("z"));
)