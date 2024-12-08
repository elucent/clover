#include "jasmine/test/common.h"

FOR_EACH_INT(DEFINE_TEST, ssa_trivial, 42,
    auto bb0 = fn.addBlock();
    bb0.addNode(Opcode::MOV, type, fn.variable("x"), fn.intConst(21));
    bb0.addNode(Opcode::MOV, type, fn.variable("y"), fn.intConst(21));
    bb0.addNode(Opcode::ADD, type, fn.variable("z"), fn.variable("x"), fn.variable("y"));
    bb0.addNode(Opcode::RET, type, fn.variable("z"));
);

FOR_EACH_INT(DEFINE_TEST, ssa_straight_line_single_block, 42,
    auto bb0 = fn.addBlock();
    bb0.addNode(Opcode::MOV, type, fn.variable("x"), fn.intConst(41));
    bb0.addNode(Opcode::MOV, type, fn.variable("x"), fn.intConst(14));
    bb0.addNode(Opcode::ADD, type, fn.variable("x"), fn.variable("x"), fn.intConst(14));
    bb0.addNode(Opcode::ADD, type, fn.variable("x"), fn.variable("x"), fn.intConst(14));
    bb0.addNode(Opcode::RET, type, fn.variable("x"));
);

FOR_EACH_INT(DEFINE_TEST, ssa_straight_line_multi_block, 42,
    auto bb0 = fn.addBlock(), bb1 = fn.addBlock(), bb2 = fn.addBlock();
    bb0.addNode(Opcode::MOV, type, fn.variable("x"), fn.intConst(7));
    bb0.addNode(Opcode::ADD, type, fn.variable("x"), fn.variable("x"), fn.intConst(7));
    bb0.addNode(Opcode::BR, VOID, fn.branch(bb0, bb1));
    bb1.addNode(Opcode::ADD, type, fn.variable("x"), fn.variable("x"), fn.intConst(7));
    bb1.addNode(Opcode::ADD, type, fn.variable("x"), fn.variable("x"), fn.intConst(7));
    bb1.addNode(Opcode::BR, VOID, fn.branch(bb1, bb2));
    bb2.addNode(Opcode::ADD, type, fn.variable("x"), fn.variable("x"), fn.intConst(7));
    bb2.addNode(Opcode::ADD, type, fn.variable("x"), fn.variable("x"), fn.intConst(7));
    bb2.addNode(Opcode::RET, type, fn.variable("x"));
);

FOR_EACH_INT(DEFINE_TEST, ssa_if_simple, 42,
    auto bb0 = fn.addBlock(), bb1 = fn.addBlock(), bb2 = fn.addBlock();
    bb0.addNode(Opcode::MOV, BOOL, fn.variable("x"), fn.intConst(1));
    bb0.addNode(Opcode::MOV, type, fn.variable("y"), fn.intConst(41));
    bb0.addNode(Opcode::BR_IF, BOOL, fn.variable("x"), fn.branch(bb0, bb1), fn.branch(bb0, bb2));
    bb1.addNode(Opcode::MOV, type, fn.variable("y"), fn.intConst(42));
    bb1.addNode(Opcode::BR, VOID, fn.branch(bb1, bb2));
    bb2.addNode(Opcode::RET, type, fn.variable("y"));
);

FOR_EACH_INT(DEFINE_TEST, ssa_if_else_simple, 42,
    auto bb0 = fn.addBlock(), bb1 = fn.addBlock(), bb2 = fn.addBlock(), bb3 = fn.addBlock();
    bb0.addNode(Opcode::MOV, BOOL, fn.variable("x"), fn.intConst(1));
    bb0.addNode(Opcode::MOV, type, fn.variable("y"), fn.intConst(21));
    bb0.addNode(Opcode::BR_IF, BOOL, fn.variable("x"), fn.branch(bb0, bb1), fn.branch(bb0, bb2));
    bb1.addNode(Opcode::ADD, type, fn.variable("y"), fn.variable("y"), fn.intConst(21));
    bb1.addNode(Opcode::BR, VOID, fn.branch(bb1, bb3));
    bb2.addNode(Opcode::ADD, type, fn.variable("y"), fn.variable("y"), fn.intConst(20));
    bb2.addNode(Opcode::BR, VOID, fn.branch(bb2, bb3));
    bb3.addNode(Opcode::RET, type, fn.variable("y"));
);

FOR_EACH_INT(DEFINE_TEST, ssa_loop_simple, 42,
    auto bb0 = fn.addBlock(), bb1 = fn.addBlock(), bb2 = fn.addBlock(), bb3 = fn.addBlock();
    bb0.addNode(Opcode::MOV, type, fn.variable("i"), fn.intConst(0));
    bb0.addNode(Opcode::MOV, type, fn.variable("x"), fn.intConst(0));
    bb0.addNode(Opcode::BR, VOID, fn.branch(bb0, bb1));
    bb1.addNode(Opcode::BR_LT, type, fn.variable("i"), fn.intConst(21), fn.branch(bb1, bb2), fn.branch(bb1, bb3));
    bb2.addNode(Opcode::ADD, type, fn.variable("i"), fn.variable("i"), fn.intConst(1));
    bb2.addNode(Opcode::ADD, type, fn.variable("x"), fn.variable("x"), fn.intConst(2));
    bb2.addNode(Opcode::BR, VOID, fn.branch(bb2, bb1));
    bb3.addNode(Opcode::RET, type, fn.variable("x"));
);