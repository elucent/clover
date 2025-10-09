#include "jasmine/test/common.h"

TypeIndex addLaunder(Module& module, TypeIndex type) {
    // Used to hide constant folding.

    auto& fn = module.defineFunction(type, cstring("launder"));
    fn.addParameter(type, cstring("x"));

    auto bb0 = fn.addBlock();
    bb0.addNode(Opcode::RET, type, fn.variable("x"));

    return FunctionBuilder(type, type).build(module.typeContext());
}

DEFINE_TEST(hoist_simple_arithmetic, 42, i32, I32,
    auto bb0 = fn.addBlock(), bb1 = fn.addBlock(), bb2 = fn.addBlock();

    auto launderType = addLaunder(mod, I32);

    bb0.addNode(Opcode::MOV, I32, fn.variable("i"), fn.intConst(0));
    bb0.addNode(Opcode::MOV, I32, fn.variable("x"), fn.intConst(0));
    bb0.addNode(Opcode::CALL, launderType, fn.variable("x"), fn.func("launder"), fn.variable("x"));
    bb0.addNode(Opcode::BR, VOID, fn.branch(bb0, bb1));
    bb1.addNode(Opcode::ADD, I32, fn.variable("y"), fn.variable("x"), fn.intConst(1));
    bb1.addNode(Opcode::ADD, I32, fn.variable("i"), fn.variable("i"), fn.variable("y"));
    bb1.addNode(Opcode::BR_GE, I32, fn.variable("i"), fn.intConst(42), fn.branch(bb1, bb2), fn.branch(bb1, bb1));
    bb2.addNode(Opcode::RET, I32, fn.variable("i"));
);

DEFINE_TEST(hoist_nested_loop_index, 51, i32, I32,
    auto bb0 = fn.addBlock(), bb1 = fn.addBlock(), bb2 = fn.addBlock(), bb3 = fn.addBlock(), bb4 = fn.addBlock();

    auto launderType = addLaunder(mod, I32);

    bb0.addNode(Opcode::MOV, I32, fn.variable("i"), fn.intConst(0));
    bb0.addNode(Opcode::MOV, I32, fn.variable("x"), fn.intConst(41));
    bb0.addNode(Opcode::CALL, launderType, fn.variable("x"), fn.func("launder"), fn.variable("x"));
    bb0.addNode(Opcode::BR, VOID, fn.branch(bb0, bb1));
    bb1.addNode(Opcode::ADD, I32, fn.variable("i"), fn.variable("i"), fn.intConst(1));
    bb1.addNode(Opcode::MOV, I32, fn.variable("j"), fn.intConst(0));
    bb1.addNode(Opcode::BR, VOID, fn.branch(bb1, bb2));
    bb2.addNode(Opcode::ADD, I32, fn.variable("y"), fn.variable("x"), fn.intConst(1));
    bb2.addNode(Opcode::ADD, I32, fn.variable("z"), fn.variable("i"), fn.intConst(-1));
    bb2.addNode(Opcode::ADD, I32, fn.variable("w"), fn.variable("z"), fn.variable("y"));
    bb2.addNode(Opcode::ADD, I32, fn.variable("j"), fn.variable("j"), fn.intConst(1));
    bb2.addNode(Opcode::BR_GE, I32, fn.variable("j"), fn.variable("w"), fn.branch(bb2, bb3), fn.branch(bb2, bb2));
    bb3.addNode(Opcode::BR_GE, I32, fn.variable("i"), fn.intConst(10), fn.branch(bb3, bb4), fn.branch(bb3, bb1));
    bb4.addNode(Opcode::RET, I32, fn.variable("j"));
)
