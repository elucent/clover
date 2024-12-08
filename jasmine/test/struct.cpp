#include "jasmine/test/common.h"

FOR_EACH_INT(DEFINE_TEST, pair_set_and_get, 3,
    auto bb0 = fn.addBlock();
    TypeIndex structType = StructBuilder(type, type).build(fn.typeContext());
    bb0.addNode(Opcode::VAR, structType, fn.variable("pair"));
    bb0.addNode(Opcode::SET_FIELD, structType, fn.variable("pair"), fn.intConst(0), fn.intConst(1));
    bb0.addNode(Opcode::SET_FIELD, structType, fn.variable("pair"), fn.intConst(1), fn.intConst(2));
    bb0.addNode(Opcode::GET_FIELD, structType, fn.variable("x"), fn.variable("pair"), fn.intConst(0));
    bb0.addNode(Opcode::GET_FIELD, structType, fn.variable("y"), fn.variable("pair"), fn.intConst(1));
    bb0.addNode(Opcode::ADD, type, fn.variable("x"), fn.variable("x"), fn.variable("y"));
    bb0.addNode(Opcode::RET, type, fn.variable("x"));
)

FOR_EACH_INT(DEFINE_TEST, move_pair_value, 3,
    auto bb0 = fn.addBlock();
    TypeIndex structType = StructBuilder(type, type).build(fn.typeContext());
    bb0.addNode(Opcode::VAR, structType, fn.variable("pair"));
    bb0.addNode(Opcode::SET_FIELD, structType, fn.variable("pair"), fn.intConst(0), fn.intConst(1));
    bb0.addNode(Opcode::SET_FIELD, structType, fn.variable("pair"), fn.intConst(1), fn.intConst(2));
    bb0.addNode(Opcode::MOV, structType, fn.variable("pair2"), fn.variable("pair"));
    bb0.addNode(Opcode::GET_FIELD, structType, fn.variable("x"), fn.variable("pair2"), fn.intConst(0));
    bb0.addNode(Opcode::GET_FIELD, structType, fn.variable("y"), fn.variable("pair2"), fn.intConst(1));
    bb0.addNode(Opcode::ADD, type, fn.variable("x"), fn.variable("x"), fn.variable("y"));
    bb0.addNode(Opcode::RET, type, fn.variable("x"));
)

FOR_EACH_INT(DEFINE_TEST, pair_fields_by_value, 16,
    auto bb0 = fn.addBlock();
    TypeIndex pairType = StructBuilder(type, type).build(fn.typeContext());
    TypeIndex pairOfPairs = StructBuilder(pairType, pairType).build(fn.typeContext());
    bb0.addNode(Opcode::VAR, pairOfPairs, fn.variable("pairs"));
    bb0.addNode(Opcode::VAR, pairType, fn.variable("left"));
    bb0.addNode(Opcode::VAR, pairType, fn.variable("right"));
    bb0.addNode(Opcode::SET_FIELD, pairType, fn.variable("left"), fn.intConst(0), fn.intConst(4));
    bb0.addNode(Opcode::SET_FIELD, pairType, fn.variable("left"), fn.intConst(1), fn.intConst(4));
    bb0.addNode(Opcode::SET_FIELD, pairType, fn.variable("right"), fn.intConst(0), fn.intConst(4));
    bb0.addNode(Opcode::SET_FIELD, pairType, fn.variable("right"), fn.intConst(1), fn.intConst(4));
    bb0.addNode(Opcode::SET_FIELD, pairOfPairs, fn.variable("pairs"), fn.intConst(0), fn.variable("left"));
    bb0.addNode(Opcode::SET_FIELD, pairOfPairs, fn.variable("pairs"), fn.intConst(1), fn.variable("right"));
    bb0.addNode(Opcode::GET_FIELD, pairOfPairs, fn.variable("first"), fn.variable("pairs"), fn.intConst(0));
    bb0.addNode(Opcode::GET_FIELD, pairOfPairs, fn.variable("second"), fn.variable("pairs"), fn.intConst(1));
    bb0.addNode(Opcode::GET_FIELD, pairType, fn.variable("x"), fn.variable("first"), fn.intConst(0));
    bb0.addNode(Opcode::GET_FIELD, pairType, fn.variable("y"), fn.variable("first"), fn.intConst(1));
    bb0.addNode(Opcode::GET_FIELD, pairType, fn.variable("z"), fn.variable("second"), fn.intConst(0));
    bb0.addNode(Opcode::GET_FIELD, pairType, fn.variable("w"), fn.variable("second"), fn.intConst(1));
    bb0.addNode(Opcode::ADD, type, fn.variable("x"), fn.variable("x"), fn.variable("y"));
    bb0.addNode(Opcode::ADD, type, fn.variable("x"), fn.variable("x"), fn.variable("z"));
    bb0.addNode(Opcode::ADD, type, fn.variable("x"), fn.variable("x"), fn.variable("w"));
    bb0.addNode(Opcode::RET, type, fn.variable("x"));
)

FOR_EACH_INT(DEFINE_TEST, move_quad_value, 10,
    auto bb0 = fn.addBlock();
    TypeIndex structType = StructBuilder(type, type, type, type).build(fn.typeContext());
    bb0.addNode(Opcode::VAR, structType, fn.variable("pair"));
    bb0.addNode(Opcode::SET_FIELD, structType, fn.variable("pair"), fn.intConst(0), fn.intConst(1));
    bb0.addNode(Opcode::SET_FIELD, structType, fn.variable("pair"), fn.intConst(1), fn.intConst(2));
    bb0.addNode(Opcode::SET_FIELD, structType, fn.variable("pair"), fn.intConst(2), fn.intConst(3));
    bb0.addNode(Opcode::SET_FIELD, structType, fn.variable("pair"), fn.intConst(3), fn.intConst(4));
    bb0.addNode(Opcode::MOV, structType, fn.variable("pair2"), fn.variable("pair"));
    bb0.addNode(Opcode::GET_FIELD, structType, fn.variable("x"), fn.variable("pair2"), fn.intConst(0));
    bb0.addNode(Opcode::GET_FIELD, structType, fn.variable("y"), fn.variable("pair2"), fn.intConst(1));
    bb0.addNode(Opcode::GET_FIELD, structType, fn.variable("z"), fn.variable("pair2"), fn.intConst(2));
    bb0.addNode(Opcode::GET_FIELD, structType, fn.variable("w"), fn.variable("pair2"), fn.intConst(3));
    bb0.addNode(Opcode::ADD, type, fn.variable("x"), fn.variable("x"), fn.variable("y"));
    bb0.addNode(Opcode::ADD, type, fn.variable("x"), fn.variable("x"), fn.variable("z"));
    bb0.addNode(Opcode::ADD, type, fn.variable("x"), fn.variable("x"), fn.variable("w"));
    bb0.addNode(Opcode::RET, type, fn.variable("x"));
)

FOR_EACH_INT(DEFINE_TEST, pair_load_store_fields, 42,
    auto bb0 = fn.addBlock();
    auto pairType = StructBuilder(type, type).build(fn.typeContext());
    bb0.addNode(Opcode::VAR, pairType, fn.variable("pair"));
    bb0.addNode(Opcode::VAR, pairType, fn.variable("pair2"));
    bb0.addNode(Opcode::SET_FIELD, pairType, fn.variable("pair"), fn.intConst(0), fn.intConst(1));
    bb0.addNode(Opcode::SET_FIELD, pairType, fn.variable("pair"), fn.intConst(1), fn.intConst(2));
    bb0.addNode(Opcode::ADDR, PTR, fn.variable("ptr"), fn.variable("pair"));
    bb0.addNode(Opcode::LOAD, pairType, fn.variable("pair2"), fn.variable("ptr"));
    bb0.addNode(Opcode::ADDR, PTR, fn.variable("ptr2"), fn.variable("pair2"));
    bb0.addNode(Opcode::LOAD_FIELD, pairType, fn.variable("x"), fn.variable("ptr2"), fn.intConst(0));
    bb0.addNode(Opcode::LOAD_FIELD, pairType, fn.variable("y"), fn.variable("ptr2"), fn.intConst(1));
    bb0.addNode(Opcode::ADD, type, fn.variable("x"), fn.variable("x"), fn.intConst(5));
    bb0.addNode(Opcode::ADD, type, fn.variable("y"), fn.variable("y"), fn.intConst(5));
    bb0.addNode(Opcode::STORE_FIELD, pairType, fn.variable("ptr2"), fn.intConst(0), fn.variable("x"));
    bb0.addNode(Opcode::STORE_FIELD, pairType, fn.variable("ptr2"), fn.intConst(1), fn.variable("y"));
    bb0.addNode(Opcode::LOAD, pairType, fn.variable("pair"), fn.variable("ptr2"));
    bb0.addNode(Opcode::LOAD_FIELD, pairType, fn.variable("x"), fn.variable("ptr"), fn.intConst(0));
    bb0.addNode(Opcode::LOAD_FIELD, pairType, fn.variable("y"), fn.variable("ptr"), fn.intConst(1));
    bb0.addNode(Opcode::MUL, type, fn.variable("x"), fn.variable("x"), fn.variable("y"));
    bb0.addNode(Opcode::RET, type, fn.variable("x"));
)