#include "jasmine/test/common.h"

DEFINE_TEST(load_after_store, 42, i32, I32,
    auto bb0 = fn.addBlock();
    bb0.addNode(Opcode::MOV, I32, fn.variable("x"), fn.intConst(20));
    bb0.addNode(Opcode::ADDR, PTR, fn.variable("p"), fn.variable("x"));
    bb0.addNode(Opcode::LOAD, I32, fn.variable("y"), fn.variable("p"));
    bb0.addNode(Opcode::ADD, I32, fn.variable("w"), fn.variable("y"), fn.intConst(2));
    bb0.addNode(Opcode::STORE, I32, fn.variable("p"), fn.variable("w"));
    bb0.addNode(Opcode::LOAD, I32, fn.variable("z"), fn.variable("p"));
    bb0.addNode(Opcode::ADD, I32, fn.variable("y"), fn.variable("y"), fn.variable("z"));
    bb0.addNode(Opcode::RET, I32, fn.variable("y"));
);

DEFINE_TEST(reference_local_struct, 126, i32, I32,
    TypeIndex tripleType = StructBuilder(I32, I32, I32).build(fn.typeContext());
    auto bb0 = fn.addBlock();
    bb0.addNode(Opcode::VAR, tripleType, fn.variable("v"));
    bb0.addNode(Opcode::ADDR_FIELD, tripleType, fn.variable("p0"), fn.variable("v"), fn.intConst(0));
    bb0.addNode(Opcode::ADDR_FIELD, tripleType, fn.variable("p1"), fn.variable("v"), fn.intConst(1));
    bb0.addNode(Opcode::ADDR_FIELD, tripleType, fn.variable("p2"), fn.variable("v"), fn.intConst(2));
    bb0.addNode(Opcode::STORE, I32, fn.variable("p0"), fn.intConst(42));
    bb0.addNode(Opcode::STORE, I32, fn.variable("p1"), fn.intConst(42));
    bb0.addNode(Opcode::STORE, I32, fn.variable("p2"), fn.intConst(42));
    bb0.addNode(Opcode::GET_FIELD, tripleType, fn.variable("x"), fn.variable("v"), fn.intConst(0));
    bb0.addNode(Opcode::GET_FIELD, tripleType, fn.variable("y"), fn.variable("v"), fn.intConst(0));
    bb0.addNode(Opcode::GET_FIELD, tripleType, fn.variable("z"), fn.variable("v"), fn.intConst(0));
    bb0.addNode(Opcode::ADD, I32, fn.variable("x"), fn.variable("x"), fn.variable("y"));
    bb0.addNode(Opcode::ADD, I32, fn.variable("x"), fn.variable("x"), fn.variable("z"));
    bb0.addNode(Opcode::RET, I32, fn.variable("x"));
);

DEFINE_TEST(reference_struct_across_branch, 42, i32, I32,
    TypeIndex pairType = StructBuilder(I32, I32).build(fn.typeContext());
    auto bb0 = fn.addBlock(), bb1 = fn.addBlock(), bb2 = fn.addBlock(), bb3 = fn.addBlock();
    bb0.addNode(Opcode::VAR, pairType, fn.variable("p"));
    bb0.addNode(Opcode::SET_FIELD, pairType, fn.variable("p"), fn.intConst(0), fn.intConst(0));
    bb0.addNode(Opcode::SET_FIELD, pairType, fn.variable("p"), fn.intConst(1), fn.intConst(0));
    bb0.addNode(Opcode::BR_IF, I32, fn.intConst(1), fn.branch(bb0, bb1), fn.branch(bb0, bb2));
    bb1.addNode(Opcode::ADDR_FIELD, pairType, fn.variable("a"), fn.variable("p"), fn.intConst(0));
    bb1.addNode(Opcode::BR, VOID, fn.branch(bb1, bb3));
    bb2.addNode(Opcode::ADDR_FIELD, pairType, fn.variable("a"), fn.variable("p"), fn.intConst(1));
    bb2.addNode(Opcode::BR, VOID, fn.branch(bb2, bb3));
    bb3.addNode(Opcode::STORE, I32, fn.variable("a"), fn.intConst(42));
    bb3.addNode(Opcode::GET_FIELD, pairType, fn.variable("x"), fn.variable("p"), fn.intConst(0));
    bb3.addNode(Opcode::RET, I32, fn.variable("x"));
);