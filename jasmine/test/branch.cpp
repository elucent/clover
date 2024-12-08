#include "jasmine/test/common.h"

DEFINE_TEST(simple_direct, 3, i32, I32, 
    auto bb0 = fn.addBlock(), bb1 = fn.addBlock(), bb2 = fn.addBlock();
    bb0.addNode(Opcode::MOV, I32, fn.variable("x"), fn.intConst(1));
    bb0.addNode(Opcode::BR, VOID, fn.branch(fn.addEdge(bb0, bb2)));
    bb1.addNode(Opcode::MOV, I32, fn.variable("x"), fn.intConst(2));
    bb1.addNode(Opcode::RET, I32, fn.variable("x"));
    bb2.addNode(Opcode::MOV, I32, fn.variable("x"), fn.intConst(3));
    bb2.addNode(Opcode::RET, I32, fn.variable("x"));
);

DEFINE_TEST(trivial_jumps, 3, i32, I32, 
    auto bb0 = fn.addBlock(), bb1 = fn.addBlock(), bb2 = fn.addBlock();
    bb0.addNode(Opcode::MOV, I32, fn.variable("x"), fn.intConst(1));
    bb0.addNode(Opcode::BR, VOID, fn.branch(fn.addEdge(bb0, bb1)));
    bb1.addNode(Opcode::ADD, I32, fn.variable("x"), fn.variable("x"), fn.intConst(1));
    bb1.addNode(Opcode::BR, VOID, fn.branch(fn.addEdge(bb1, bb2)));
    bb2.addNode(Opcode::ADD, I32, fn.variable("x"), fn.variable("x"), fn.intConst(1));
    bb2.addNode(Opcode::RET, I32, fn.variable("x"));
);

DEFINE_TEST(branch_onesided_taken, 42, i32, I32,
    auto bb0 = fn.addBlock(), bb1 = fn.addBlock(), bb2 = fn.addBlock();
    bb0.addNode(Opcode::MOV, BOOL, fn.variable("x"), fn.intConst(1));
    bb0.addNode(Opcode::MOV, I32, fn.variable("y"), fn.intConst(41));
    bb0.addNode(Opcode::BR_IF, BOOL, fn.variable("x"), fn.branch(fn.addEdge(bb0, bb1)), fn.branch(fn.addEdge(bb0, bb2)));
    bb1.addNode(Opcode::MOV, I32, fn.variable("y"), fn.intConst(42));
    bb1.addNode(Opcode::BR, VOID, fn.branch(fn.addEdge(bb1, bb2)));
    bb2.addNode(Opcode::RET, I32, fn.variable("y"));
);

DEFINE_TEST(branch_onesided_not_taken, 42, i32, I32,
    auto bb0 = fn.addBlock(), bb1 = fn.addBlock(), bb2 = fn.addBlock();
    bb0.addNode(Opcode::MOV, BOOL, fn.variable("x"), fn.intConst(0));
    bb0.addNode(Opcode::MOV, I32, fn.variable("y"), fn.intConst(42));
    bb0.addNode(Opcode::BR_IF, BOOL, fn.variable("x"), fn.branch(fn.addEdge(bb0, bb1)), fn.branch(fn.addEdge(bb0, bb2)));
    bb1.addNode(Opcode::MOV, I32, fn.variable("y"), fn.intConst(41));
    bb1.addNode(Opcode::BR, VOID, fn.branch(fn.addEdge(bb1, bb2)));
    bb2.addNode(Opcode::RET, I32, fn.variable("y"));
);

DEFINE_TEST(branch_twosided_taken, 42, i32, I32,
    auto bb0 = fn.addBlock(), bb1 = fn.addBlock(), bb2 = fn.addBlock(), bb3 = fn.addBlock();
    bb0.addNode(Opcode::MOV, BOOL, fn.variable("x"), fn.intConst(1));
    bb0.addNode(Opcode::MOV, I32, fn.variable("y"), fn.intConst(41));
    bb0.addNode(Opcode::BR_IF, BOOL, fn.variable("x"), fn.branch(fn.addEdge(bb0, bb1)), fn.branch(fn.addEdge(bb0, bb2)));
    bb1.addNode(Opcode::ADD, I32, fn.variable("y"), fn.variable("y"), fn.intConst(1));
    bb1.addNode(Opcode::BR, VOID, fn.branch(fn.addEdge(bb1, bb3)));
    bb2.addNode(Opcode::ADD, I32, fn.variable("y"), fn.variable("y"), fn.intConst(2));
    bb2.addNode(Opcode::BR, VOID, fn.branch(fn.addEdge(bb2, bb3)));
    bb3.addNode(Opcode::RET, I32, fn.variable("y"));
);

DEFINE_TEST(branch_twosided_not_taken, 42, i32, I32,
    auto bb0 = fn.addBlock(), bb1 = fn.addBlock(), bb2 = fn.addBlock(), bb3 = fn.addBlock();
    bb0.addNode(Opcode::MOV, BOOL, fn.variable("x"), fn.intConst(0));
    bb0.addNode(Opcode::MOV, I32, fn.variable("y"), fn.intConst(41));
    bb0.addNode(Opcode::BR_IF, BOOL, fn.variable("x"), fn.branch(fn.addEdge(bb0, bb1)), fn.branch(fn.addEdge(bb0, bb2)));
    bb1.addNode(Opcode::ADD, I32, fn.variable("y"), fn.variable("y"), fn.intConst(2));
    bb1.addNode(Opcode::BR, VOID, fn.branch(fn.addEdge(bb1, bb3)));
    bb2.addNode(Opcode::ADD, I32, fn.variable("y"), fn.variable("y"), fn.intConst(1));
    bb2.addNode(Opcode::BR, VOID, fn.branch(fn.addEdge(bb2, bb3)));
    bb3.addNode(Opcode::RET, I32, fn.variable("y"));
);

DEFINE_TEST(branch_twosided_into_op, 42, i32, I32,
    auto bb0 = fn.addBlock(), bb1 = fn.addBlock(), bb2 = fn.addBlock(), bb3 = fn.addBlock();
    bb0.addNode(Opcode::MOV, BOOL, fn.variable("x"), fn.intConst(1));
    bb0.addNode(Opcode::MOV, I32, fn.variable("y"), fn.intConst(40));
    bb0.addNode(Opcode::BR_IF, BOOL, fn.variable("x"), fn.branch(fn.addEdge(bb0, bb1)), fn.branch(fn.addEdge(bb0, bb2)));
    bb1.addNode(Opcode::ADD, I32, fn.variable("y"), fn.variable("y"), fn.intConst(1));
    bb1.addNode(Opcode::BR, VOID, fn.branch(fn.addEdge(bb1, bb3)));
    bb2.addNode(Opcode::ADD, I32, fn.variable("y"), fn.variable("y"), fn.intConst(2));
    bb2.addNode(Opcode::BR, VOID, fn.branch(fn.addEdge(bb2, bb3)));
    bb3.addNode(Opcode::ADD, I32, fn.variable("y"), fn.variable("y"), fn.intConst(1));
    bb3.addNode(Opcode::RET, I32, fn.variable("y"));
);