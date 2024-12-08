#include "jasmine/test/common.h"

DEFINE_SIMPLE_TEST(unreachable_block, 0, i32, I32,
    Module mod("test");
    Function& fn = mod.defineFunction(type, "test");
    auto bb0 = fn.addBlock(), bb1 = fn.addBlock(), bb2 = fn.addBlock();
    bb0.addNode(Opcode::MOV, I32, fn.variable("x"), fn.intConst(1));
    bb0.addNode(Opcode::BR, VOID, fn.branch(fn.addEdge(bb0, bb2)));
    bb1.addNode(Opcode::MOV, I32, fn.variable("x"), fn.intConst(2));
    bb1.addNode(Opcode::BR, VOID, fn.branch(fn.addEdge(bb1, bb2)));
    bb2.addNode(Opcode::RET, I32, fn.variable("x"));

    mod.passContext().reset();
    typecheck(mod.passContext(), fn);
    cleanup(mod.passContext(), fn);
    ASSERT(fn.blockList.size() == 2);
    ASSERT(fn.edgeList.size() == 1);
);

DEFINE_SIMPLE_TEST(starts_with_nop, 0, i32, I32,
    Module mod("test");
    Function& fn = mod.defineFunction(type, "test");
    auto bb0 = fn.addBlock();
    bb0.addNode(Opcode::NOP, VOID);
    bb0.addNode(Opcode::MOV, I32, fn.variable("x"), fn.intConst(1));
    bb0.addNode(Opcode::RET, I32, fn.variable("x"));

    mod.passContext().reset();
    typecheck(mod.passContext(), fn);
    cleanup(mod.passContext(), fn);
    ASSERT(fn.nodeList.size() == 2);
    ASSERT(fn.blockList.size() == 1);
);

TEST(ends_with_nop_i32) {
    Module mod("test");
    Function& fn = mod.defineFunction(I32, "test");
    auto bb0 = fn.addBlock();
    bb0.addNode(Opcode::MOV, I32, fn.variable("x"), fn.intConst(1));
    bb0.addNode(Opcode::NOP, VOID);
    bb0.addNode(Opcode::RET, I32, fn.variable("x"));

    mod.passContext().reset();
    typecheck(mod.passContext(), fn);
    cleanup(mod.passContext(), fn);
    ASSERT(fn.nodeList.size() == 2);
    ASSERT(fn.blockList.size() == 1);
}

DEFINE_SIMPLE_TEST(nops_interspersed, 0, i32, I32,
    Module mod("test");
    Function& fn = mod.defineFunction(type, "test");
    auto bb0 = fn.addBlock();
    bb0.addNode(Opcode::MOV, I32, fn.variable("x"), fn.intConst(1));
    bb0.addNode(Opcode::NOP, VOID);
    bb0.addNode(Opcode::ADD, I32, fn.variable("x"), fn.variable("x"), fn.intConst(1));
    bb0.addNode(Opcode::NOP, VOID);
    bb0.addNode(Opcode::RET, I32, fn.variable("x"));

    mod.passContext().reset();
    typecheck(mod.passContext(), fn);
    cleanup(mod.passContext(), fn);
    ASSERT(fn.nodeList.size() == 3);
    ASSERT(fn.blockList.size() == 1);
);

DEFINE_SIMPLE_TEST(many_nops, 0, i32, I32,
    Module mod("test");
    Function& fn = mod.defineFunction(type, "test");
    auto bb0 = fn.addBlock();
    bb0.addNode(Opcode::NOP, VOID);
    bb0.addNode(Opcode::NOP, VOID);
    bb0.addNode(Opcode::NOP, VOID);
    bb0.addNode(Opcode::MOV, I32, fn.variable("x"), fn.intConst(1));
    bb0.addNode(Opcode::NOP, VOID);
    bb0.addNode(Opcode::NOP, VOID);
    bb0.addNode(Opcode::NOP, VOID);
    bb0.addNode(Opcode::ADD, I32, fn.variable("x"), fn.variable("x"), fn.intConst(1));
    bb0.addNode(Opcode::NOP, VOID);
    bb0.addNode(Opcode::NOP, VOID);
    bb0.addNode(Opcode::NOP, VOID);
    bb0.addNode(Opcode::RET, I32, fn.variable("x"));

    mod.passContext().reset();
    typecheck(mod.passContext(), fn);
    cleanup(mod.passContext(), fn);
    ASSERT(fn.nodeList.size() == 3);
    ASSERT(fn.blockList.size() == 1);
);