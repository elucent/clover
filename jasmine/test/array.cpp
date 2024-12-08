#include "jasmine/test/common.h"

FOR_EACH_INT(DEFINE_TEST, array_sum_fixed, 55,
    auto bb0 = fn.addBlock();
    auto bb1 = fn.addBlock();
    auto bb2 = fn.addBlock();
    TypeIndex arrayType = ArrayBuilder(type, 10).build(fn.typeContext());
    bb0.addNode(Opcode::VAR, arrayType, fn.variable("arr"));
    bb0.addNode(Opcode::SET_INDEX, type, fn.variable("arr"), fn.typeOperand(PTR), fn.intConst(0), fn.intConst(1));
    bb0.addNode(Opcode::SET_INDEX, type, fn.variable("arr"), fn.typeOperand(PTR), fn.intConst(1), fn.intConst(2));
    bb0.addNode(Opcode::SET_INDEX, type, fn.variable("arr"), fn.typeOperand(PTR), fn.intConst(2), fn.intConst(3));
    bb0.addNode(Opcode::SET_INDEX, type, fn.variable("arr"), fn.typeOperand(PTR), fn.intConst(3), fn.intConst(4));
    bb0.addNode(Opcode::SET_INDEX, type, fn.variable("arr"), fn.typeOperand(PTR), fn.intConst(4), fn.intConst(5));
    bb0.addNode(Opcode::SET_INDEX, type, fn.variable("arr"), fn.typeOperand(PTR), fn.intConst(5), fn.intConst(6));
    bb0.addNode(Opcode::SET_INDEX, type, fn.variable("arr"), fn.typeOperand(PTR), fn.intConst(6), fn.intConst(7));
    bb0.addNode(Opcode::SET_INDEX, type, fn.variable("arr"), fn.typeOperand(PTR), fn.intConst(7), fn.intConst(8));
    bb0.addNode(Opcode::SET_INDEX, type, fn.variable("arr"), fn.typeOperand(PTR), fn.intConst(8), fn.intConst(9));
    bb0.addNode(Opcode::SET_INDEX, type, fn.variable("arr"), fn.typeOperand(PTR), fn.intConst(9), fn.intConst(10));
    bb0.addNode(Opcode::MOV, type, fn.variable("x"), fn.intConst(0));
    bb0.addNode(Opcode::MOV, type, fn.variable("i"), fn.intConst(0));
    bb0.addNode(Opcode::BR_GE, type, fn.variable("i"), fn.intConst(10), fn.branch(bb0, bb2), fn.branch(bb0, bb1));
    bb1.addNode(Opcode::GET_INDEX, type, fn.variable("y"), fn.variable("arr"), fn.typeOperand(type), fn.variable("i"));
    bb1.addNode(Opcode::ADD, type, fn.variable("x"), fn.variable("x"), fn.variable("y"));
    bb1.addNode(Opcode::ADD, type, fn.variable("i"), fn.variable("i"), fn.intConst(1));
    bb1.addNode(Opcode::BR_GE, type, fn.variable("i"), fn.intConst(10), fn.branch(bb1, bb2), fn.branch(bb1, bb1));
    bb2.addNode(Opcode::RET, type, fn.variable("x"));
)