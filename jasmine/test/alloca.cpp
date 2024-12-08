#include "jasmine/test/common.h"

DEFINE_TEST(alloca_constant, 42, i32, I32,
    auto bb0 = fn.addBlock();
    bb0.addNode(Opcode::ALLOCA, U32, fn.variable("x"), fn.intConst(4));
    bb0.addNode(Opcode::STORE, I32, fn.variable("x"), fn.intConst(42));
    bb0.addNode(Opcode::LOAD, I32, fn.variable("y"), fn.variable("x"));
    bb0.addNode(Opcode::RET, I32, fn.variable("y"));
);

DEFINE_TEST(alloca_variable, 42, i32, I32,
    auto bb0 = fn.addBlock();
    bb0.addNode(Opcode::MOV, U32, fn.variable("n"), fn.intConst(4));
    bb0.addNode(Opcode::ALLOCA, U32, fn.variable("x"), fn.variable("n"));
    bb0.addNode(Opcode::STORE, I32, fn.variable("x"), fn.intConst(42));
    bb0.addNode(Opcode::LOAD, I32, fn.variable("y"), fn.variable("x"));
    bb0.addNode(Opcode::RET, I32, fn.variable("y"));
);