#include "jasmine/test/common.h"

DEFINE_TEST(never_taken_loop, 42, i32, I32, 
    auto bb0 = fn.addBlock(), bb1 = fn.addBlock(), bb2 = fn.addBlock();
    bb0.addNode(Opcode::MOV, BOOL, fn.variable("x"), fn.intConst(0));
    bb0.addNode(Opcode::MOV, I32, fn.variable("y"), fn.intConst(42));
    bb0.addNode(Opcode::BR_IF, BOOL, fn.variable("x"), fn.branch(fn.addEdge(bb0, bb1)), fn.branch(fn.addEdge(bb0, bb2)));
    bb1.addNode(Opcode::BR, VOID, fn.branch(fn.addEdge(bb1, bb1)));
    bb2.addNode(Opcode::RET, I32, fn.variable("y"));
);

DEFINE_TEST(count_up_loop, 42, i32, I32,
    auto bb0 = fn.addBlock(), bb1 = fn.addBlock(), bb2 = fn.addBlock(), bb3 = fn.addBlock();
    bb0.addNode(Opcode::MOV, I32, fn.variable("x"), fn.intConst(0));
    bb0.addNode(Opcode::BR, VOID, fn.branch(fn.addEdge(bb0, bb1)));
    bb1.addNode(Opcode::IS_LT, I32, fn.variable("y"), fn.variable("x"), fn.intConst(42));
    bb1.addNode(Opcode::BR_IF, BOOL, fn.variable("y"), fn.branch(fn.addEdge(bb1, bb2)), fn.branch(fn.addEdge(bb1, bb3)));
    bb2.addNode(Opcode::ADD, I32, fn.variable("x"), fn.variable("x"), fn.intConst(1));
    bb2.addNode(Opcode::BR, VOID, fn.branch(fn.addEdge(bb2, bb1)));
    bb3.addNode(Opcode::RET, I32, fn.variable("x"));
);

DEFINE_TEST(branch_in_loop, 14, i32, I32,
    auto bb0 = fn.addBlock(), bb1 = fn.addBlock(), bb2 = fn.addBlock(), bb3 = fn.addBlock(), bb4 = fn.addBlock(), bb5 = fn.addBlock();
    bb0.addNode(Opcode::MOV, I32, fn.variable("x"), fn.intConst(0));
    bb0.addNode(Opcode::MOV, I32, fn.variable("y"), fn.intConst(0));
    bb0.addNode(Opcode::BR, VOID, fn.branch(fn.addEdge(bb0, bb1)));
    bb1.addNode(Opcode::IS_LT, I32, fn.variable("z"), fn.variable("x"), fn.intConst(42));
    bb1.addNode(Opcode::BR_IF, BOOL, fn.variable("z"), fn.branch(fn.addEdge(bb1, bb2)), fn.branch(fn.addEdge(bb1, bb5)));
    bb2.addNode(Opcode::ADD, I32, fn.variable("x"), fn.variable("x"), fn.intConst(1));
    bb2.addNode(Opcode::REM, I32, fn.variable("w"), fn.variable("x"), fn.intConst(3));
    bb2.addNode(Opcode::IS_EQ, I32, fn.variable("z"), fn.variable("w"), fn.intConst(0));
    bb2.addNode(Opcode::BR_IF, BOOL, fn.variable("z"), fn.branch(fn.addEdge(bb2, bb3)), fn.branch(fn.addEdge(bb2, bb4)));
    bb3.addNode(Opcode::ADD, I32, fn.variable("y"), fn.variable("y"), fn.intConst(1));
    bb3.addNode(Opcode::BR, VOID, fn.branch(fn.addEdge(bb3, bb4)));
    bb4.addNode(Opcode::BR, VOID, fn.branch(fn.addEdge(bb4, bb1)));
    bb5.addNode(Opcode::RET, I32, fn.variable("y"));
);

DEFINE_TEST(nested_loop, 100, i32, I32,
    auto bb0 = fn.addBlock(), bb1 = fn.addBlock(), bb2 = fn.addBlock(), bb3 = fn.addBlock(), bb4 = fn.addBlock();
    bb0.addNode(Opcode::MOV, I32, fn.variable("x"), fn.intConst(0));
    bb0.addNode(Opcode::MOV, I32, fn.variable("i"), fn.intConst(0));
    bb0.addNode(Opcode::BR, VOID, fn.branch(bb0, bb1));
    bb1.addNode(Opcode::MOV, I32, fn.variable("j"), fn.intConst(0));
    bb1.addNode(Opcode::BR, VOID, fn.branch(bb1, bb2));
    bb2.addNode(Opcode::ADD, I32, fn.variable("j"), fn.variable("j"), fn.intConst(1));
    bb2.addNode(Opcode::ADD, I32, fn.variable("x"), fn.variable("x"), fn.intConst(1));
    bb2.addNode(Opcode::BR_LT, I32, fn.variable("j"), fn.intConst(10), fn.branch(bb2, bb2), fn.branch(bb2, bb3));
    bb3.addNode(Opcode::ADD, I32, fn.variable("i"), fn.variable("i"), fn.intConst(1));
    bb3.addNode(Opcode::BR_LT, I32, fn.variable("i"), fn.intConst(10), fn.branch(bb3, bb1), fn.branch(bb3, bb4));
    bb4.addNode(Opcode::RET, I32, fn.variable("x"));
);