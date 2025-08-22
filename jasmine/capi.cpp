#include "jasmine/ir.h"
#include "jasmine/mod.h"
#include "jasmine/pass.h"
#include "jasmine/capi.h"

using namespace jasmine;

#define M(...) (*((Module*)((__VA_ARGS__).handle)))
#define F(...) (*((Function*)((__VA_ARGS__).handle)))
#define O(...) (Operand { .bits = (__VA_ARGS__).bits })
#define V(...) (Value { .bits = (__VA_ARGS__).bits })

/*
 * Modules
 */

JASMINE_EXPORT JasmineModule jasmine_create_module(const char* name, size_t name_length) {
    return JasmineModule { .handle = new jasmine::Module(const_slice<i8>{ name, (iword)name_length }) };
}

JASMINE_EXPORT void jasmine_destroy_module(JasmineModule module) {
    delete (jasmine::Module*)module.handle;
}

/*
 * Types
 */

JASMINE_EXPORT JasmineType jasmine_make_struct_type(JasmineModule module, const JasmineType* types, size_t count) {
    auto builder = M(module).structBuilder();
    for (size_t i = 0; i < count; i ++)
        builder.addField(types[i].index);
    return JasmineType { .index = builder.build(M(module).typeContext()) };
}

JASMINE_EXPORT JasmineType jasmine_make_union_type(JasmineModule module, const JasmineType* types, size_t count) {
    auto builder = M(module).unionBuilder();
    for (size_t i = 0; i < count; i ++)
        builder.addCase(types[i].index);
    return JasmineType { .index = builder.build(M(module).typeContext()) };
}

JASMINE_EXPORT JasmineType jasmine_make_array_type(JasmineModule module, JasmineType element, size_t length) {
    return JasmineType { .index = M(module).arrayType(element.index, length) };
}

JASMINE_EXPORT JasmineType jasmine_make_function_type(JasmineModule module, const JasmineType* parameter_types, size_t parameter_count, JasmineType return_type) {
    auto builder = M(module).functionBuilder(return_type.index);
    for (size_t i = 0; i < parameter_count; i ++)
        builder.addArgument(parameter_types[i].index);
    return JasmineType { .index = builder.build(M(module).typeContext()) };
}

/*
 * Functions
 */

JASMINE_EXPORT JasmineFunction jasmine_create_function(JasmineModule module, const char* name, size_t name_length, JasmineType return_type) {
    return JasmineFunction { .handle = &M(module).defineFunction((TypeIndex)return_type.index, const_slice<i8>{ name, (iword)name_length }) };
}

/*
 * Blocks
 */

JASMINE_EXPORT JasmineBlock jasmine_add_block(JasmineFunction function) {
    return JasmineBlock { .index = (int32_t)F(function).addBlock().index() };
}

JASMINE_EXPORT JasmineOperand jasmine_add_parameter(JasmineFunction function, JasmineType type, const char* name, size_t name_length) {
    return JasmineOperand { .bits = F(function).addParameter((TypeIndex)type.index, const_slice<i8>{ name, (iword)name_length }).bits };
}

JASMINE_EXPORT void jasmine_add_node(JasmineFunction function, JasmineBlock block, JasmineNode node) {
    F(function).block(block.index).addNode(F(function).node(node.index));
}

JASMINE_EXPORT void jasmine_set_entrypoint(JasmineFunction function, JasmineBlock block) {
    F(function).setEntrypoint(F(function).block(block.index));
}

/*
 * Edges
 */

JASMINE_EXPORT JasmineEdge jasmine_add_edge(JasmineFunction function, JasmineBlock src, JasmineBlock dest) {
    return JasmineEdge { .index = (int32_t)F(function).addEdge(src.index, dest.index).index() };
}

/*
 * IR Generation
 */

JASMINE_EXPORT JasmineOperand jasmine_imm(JasmineFunction function, int64_t immediate) {
    return JasmineOperand { .bits = F(function).intConst(immediate).bits };
}

JASMINE_EXPORT JasmineOperand jasmine_sizeof(JasmineFunction function, JasmineType type) {
    return JasmineOperand { .bits = F(function).sizeOf(type.index).bits };
}

JASMINE_EXPORT JasmineOperand jasmine_f32(JasmineFunction function, float immediate) {
    return JasmineOperand { .bits = F(function).f32Const(immediate).bits };
}

JASMINE_EXPORT JasmineOperand jasmine_f64(JasmineFunction function, double immediate) {
    return JasmineOperand { .bits = F(function).f64Const(immediate).bits };
}

JASMINE_EXPORT JasmineOperand jasmine_known_function_ref(JasmineFunction function, JasmineFunction ref) {
    return JasmineOperand { .bits = F(function).func(F(ref).sym).bits };
}

JASMINE_EXPORT JasmineOperand jasmine_function_ref(JasmineFunction function, const char* name, size_t name_length) {
    return JasmineOperand { .bits = F(function).func(const_slice<i8>{ name, (iword)name_length }).bits };
}

JASMINE_EXPORT JasmineOperand jasmine_static_ref(JasmineFunction function, const char* name, size_t name_length) {
    return JasmineOperand { .bits = F(function).stat(const_slice<i8>{ name, (iword)name_length }).bits };
}

JASMINE_EXPORT JasmineOperand jasmine_data_ref(JasmineFunction function, const char* name, size_t name_length) {
    return JasmineOperand { .bits = F(function).data(const_slice<i8>{ name, (iword)name_length }).bits };
}

JASMINE_EXPORT JasmineOperand jasmine_variable(JasmineFunction function) {
    return JasmineOperand { .bits = F(function).variable().bits };
}

JASMINE_EXPORT JasmineOperand jasmine_variable_with_name(JasmineFunction function, const char* name, size_t name_length) {
    return JasmineOperand { .bits = F(function).variable({ name, (iword)name_length }).bits };
}

JASMINE_EXPORT JasmineOperand jasmine_parameter(JasmineFunction function, size_t index) {
    return JasmineOperand { .bits = F(function).parameters[index].operand.bits };
}

JASMINE_EXPORT jasmine_bool_t jasmine_is_imm(JasmineOperand operand) {
    return O(operand).kind == Operand::IntConst;
}

JASMINE_EXPORT jasmine_bool_t jasmine_is_f32(JasmineOperand operand) {
    return O(operand).kind == Operand::F32Const;
}

JASMINE_EXPORT jasmine_bool_t jasmine_is_f64(JasmineOperand operand) {
    return O(operand).kind == Operand::F64Const;
}

JASMINE_EXPORT jasmine_bool_t jasmine_is_sizeof(JasmineOperand operand) {
    return O(operand).kind == Operand::Sizeof;
}

JASMINE_EXPORT jasmine_bool_t jasmine_is_function_ref(JasmineOperand operand) {
    return O(operand).kind == Operand::Func;
}

JASMINE_EXPORT jasmine_bool_t jasmine_is_static_ref(JasmineOperand operand) {
    return O(operand).kind == Operand::Static;
}

JASMINE_EXPORT jasmine_bool_t jasmine_is_data_ref(JasmineOperand operand) {
    return O(operand).kind == Operand::Data;
}

JASMINE_EXPORT jasmine_bool_t jasmine_is_variable(JasmineOperand operand) {
    return O(operand).kind == Operand::Var;
}

JASMINE_EXPORT jasmine_bool_t jasmine_is_valid_operand(JasmineOperand operand) {
    return O(operand).kind != Operand::Invalid;
}

JASMINE_EXPORT int64_t jasmine_value_of_imm(JasmineFunction function, JasmineOperand operand) {
    return F(function).intValueOf(O(operand));
}

JASMINE_EXPORT float jasmine_value_of_f32(JasmineFunction function, JasmineOperand operand) {
    return F(function).f32ValueOf(O(operand));
}

JASMINE_EXPORT double jasmine_value_of_f64(JasmineFunction function, JasmineOperand operand) {
    return F(function).f64ValueOf(O(operand));
}

JASMINE_EXPORT JasmineNode jasmine_create_nop(JasmineFunction function) {
    return JasmineNode { .index = (int32_t)F(function).addNode(Opcode::NOP, VOID).index() };
}

JASMINE_EXPORT JasmineNode jasmine_create_var(JasmineFunction function, JasmineType type, JasmineOperand dest) {
    return JasmineNode { .index = (int32_t)F(function).addNode(Opcode::VAR, type.index, O(dest)).index() };
}

JASMINE_EXPORT JasmineNode jasmine_create_mov(JasmineFunction function, JasmineType type, JasmineOperand dest, JasmineOperand src) {
    return JasmineNode { .index = (int32_t)F(function).addNode(Opcode::MOV, type.index, O(dest), O(src)).index() };
}

JASMINE_EXPORT JasmineNode jasmine_create_pack(JasmineFunction function, JasmineType type, JasmineOperand dest, JasmineOperand* fields, size_t field_count) {
    return JasmineNode { .index = (int32_t)F(function).addNode(Opcode::PACK, type.index, O(dest), const_slice<Operand>{ (const Operand*)fields, (iword)field_count }).index() };
}

JASMINE_EXPORT JasmineNode jasmine_create_unpack(JasmineFunction function, JasmineType type, JasmineOperand* outputs, size_t output_count, JasmineOperand src) {
    return JasmineNode { .index = (int32_t)F(function).addNode(Opcode::UNPACK, type.index, const_slice<Operand>{ (const Operand*)outputs, (iword)output_count }, O(src)).index() };
}

JASMINE_EXPORT JasmineNode jasmine_create_new(JasmineFunction function, JasmineType type, JasmineOperand dest, JasmineOperand src) {
    return JasmineNode { .index = (int32_t)F(function).addNode(Opcode::NEW, type.index, O(dest), O(src)).index() };
}

JASMINE_EXPORT JasmineNode jasmine_create_new_struct(JasmineFunction function, JasmineType type, JasmineOperand dest, JasmineOperand* fields, size_t field_count) {
    return JasmineNode { .index = (int32_t)F(function).addNode(Opcode::NEW_STRUCT, type.index, O(dest), const_slice<Operand>{ (const Operand*)fields, (iword)field_count }).index() };
}

JASMINE_EXPORT JasmineNode jasmine_create_new_array(JasmineFunction function, JasmineType type, JasmineOperand dest, JasmineOperand* elements, size_t element_count) {
    return JasmineNode { .index = (int32_t)F(function).addNode(Opcode::NEW_ARRAY, type.index, O(dest), const_slice<Operand>{ (const Operand*)elements, (iword)element_count }).index() };
}

#define UNARY(opcode) { \
    return JasmineNode { .index = (int32_t)F(function).addNode(Opcode:: opcode, type.index, O(dest), O(src)).index() }; \
}

#define BINARY(opcode) { \
    return JasmineNode { .index = (int32_t)F(function).addNode(Opcode:: opcode, type.index, O(dest), O(left), O(right)).index() }; \
}

JASMINE_EXPORT JasmineNode jasmine_create_add(JasmineFunction function, JasmineType type, JasmineOperand dest, JasmineOperand left, JasmineOperand right) BINARY(ADD)
JASMINE_EXPORT JasmineNode jasmine_create_sub(JasmineFunction function, JasmineType type, JasmineOperand dest, JasmineOperand left, JasmineOperand right) BINARY(SUB)
JASMINE_EXPORT JasmineNode jasmine_create_mul(JasmineFunction function, JasmineType type, JasmineOperand dest, JasmineOperand left, JasmineOperand right) BINARY(MUL)
JASMINE_EXPORT JasmineNode jasmine_create_div(JasmineFunction function, JasmineType type, JasmineOperand dest, JasmineOperand left, JasmineOperand right) BINARY(DIV)
JASMINE_EXPORT JasmineNode jasmine_create_rem(JasmineFunction function, JasmineType type, JasmineOperand dest, JasmineOperand left, JasmineOperand right) BINARY(REM)
JASMINE_EXPORT JasmineNode jasmine_create_neg(JasmineFunction function, JasmineType type, JasmineOperand dest, JasmineOperand src) UNARY(NEG)
JASMINE_EXPORT JasmineNode jasmine_create_abs(JasmineFunction function, JasmineType type, JasmineOperand dest, JasmineOperand src) UNARY(ABS)
JASMINE_EXPORT JasmineNode jasmine_create_min(JasmineFunction function, JasmineType type, JasmineOperand dest, JasmineOperand left, JasmineOperand right) BINARY(MIN)
JASMINE_EXPORT JasmineNode jasmine_create_max(JasmineFunction function, JasmineType type, JasmineOperand dest, JasmineOperand left, JasmineOperand right) BINARY(MAX)
JASMINE_EXPORT JasmineNode jasmine_create_sqrt(JasmineFunction function, JasmineType type, JasmineOperand dest, JasmineOperand src) UNARY(SQRT)
JASMINE_EXPORT JasmineNode jasmine_create_round(JasmineFunction function, JasmineType type, JasmineOperand dest, JasmineOperand src) UNARY(ROUND)
JASMINE_EXPORT JasmineNode jasmine_create_floor(JasmineFunction function, JasmineType type, JasmineOperand dest, JasmineOperand src) UNARY(FLOOR)
JASMINE_EXPORT JasmineNode jasmine_create_ceil(JasmineFunction function, JasmineType type, JasmineOperand dest, JasmineOperand src) UNARY(CEIL)
JASMINE_EXPORT JasmineNode jasmine_create_and(JasmineFunction function, JasmineType type, JasmineOperand dest, JasmineOperand left, JasmineOperand right) BINARY(AND)
JASMINE_EXPORT JasmineNode jasmine_create_or(JasmineFunction function, JasmineType type, JasmineOperand dest, JasmineOperand left, JasmineOperand right) BINARY(OR)
JASMINE_EXPORT JasmineNode jasmine_create_xor(JasmineFunction function, JasmineType type, JasmineOperand dest, JasmineOperand left, JasmineOperand right) BINARY(XOR)
JASMINE_EXPORT JasmineNode jasmine_create_not(JasmineFunction function, JasmineType type, JasmineOperand dest, JasmineOperand src) UNARY(NOT)
JASMINE_EXPORT JasmineNode jasmine_create_shl(JasmineFunction function, JasmineType type, JasmineOperand dest, JasmineOperand left, JasmineOperand right) BINARY(SHL)
JASMINE_EXPORT JasmineNode jasmine_create_shr(JasmineFunction function, JasmineType type, JasmineOperand dest, JasmineOperand left, JasmineOperand right) BINARY(SHR)
JASMINE_EXPORT JasmineNode jasmine_create_rol(JasmineFunction function, JasmineType type, JasmineOperand dest, JasmineOperand left, JasmineOperand right) BINARY(ROL)
JASMINE_EXPORT JasmineNode jasmine_create_ror(JasmineFunction function, JasmineType type, JasmineOperand dest, JasmineOperand left, JasmineOperand right) BINARY(ROR)
JASMINE_EXPORT JasmineNode jasmine_create_lzcnt(JasmineFunction function, JasmineType type, JasmineOperand dest, JasmineOperand src) UNARY(LZCNT)
JASMINE_EXPORT JasmineNode jasmine_create_tzcnt(JasmineFunction function, JasmineType type, JasmineOperand dest, JasmineOperand src) UNARY(TZCNT)
JASMINE_EXPORT JasmineNode jasmine_create_popcnt(JasmineFunction function, JasmineType type, JasmineOperand dest, JasmineOperand src) UNARY(POPCNT)
JASMINE_EXPORT JasmineNode jasmine_create_is_lt(JasmineFunction function, JasmineType type, JasmineOperand dest, JasmineOperand left, JasmineOperand right) BINARY(IS_LT)
JASMINE_EXPORT JasmineNode jasmine_create_is_le(JasmineFunction function, JasmineType type, JasmineOperand dest, JasmineOperand left, JasmineOperand right) BINARY(IS_LE)
JASMINE_EXPORT JasmineNode jasmine_create_is_gt(JasmineFunction function, JasmineType type, JasmineOperand dest, JasmineOperand left, JasmineOperand right) BINARY(IS_GT)
JASMINE_EXPORT JasmineNode jasmine_create_is_ge(JasmineFunction function, JasmineType type, JasmineOperand dest, JasmineOperand left, JasmineOperand right) BINARY(IS_GE)
JASMINE_EXPORT JasmineNode jasmine_create_is_eq(JasmineFunction function, JasmineType type, JasmineOperand dest, JasmineOperand left, JasmineOperand right) BINARY(IS_EQ)
JASMINE_EXPORT JasmineNode jasmine_create_is_ne(JasmineFunction function, JasmineType type, JasmineOperand dest, JasmineOperand left, JasmineOperand right) BINARY(IS_NE)

JASMINE_EXPORT JasmineNode jasmine_create_get_field(JasmineFunction function, JasmineType type, JasmineOperand dest, JasmineOperand src, uint32_t field_index) {
    return JasmineNode { .index = (int32_t)F(function).addNode(Opcode::GET_FIELD, type.index, O(dest), O(src), F(function).intConst(field_index)).index() };
}

JASMINE_EXPORT JasmineNode jasmine_create_get_index(JasmineFunction function, JasmineType type, JasmineOperand dest, JasmineOperand src, JasmineType index_type, JasmineOperand index) {
    return JasmineNode { .index = (int32_t)F(function).addNode(Opcode::GET_INDEX, type.index, O(dest), O(src), F(function).typeOperand(index_type.index), O(index)).index() };
}

JASMINE_EXPORT JasmineNode jasmine_create_set_field(JasmineFunction function, JasmineType type, JasmineOperand dest, uint32_t field_index, JasmineOperand src) {
    return JasmineNode { .index = (int32_t)F(function).addNode(Opcode::SET_FIELD, type.index, O(dest), F(function).intConst(field_index), O(src)).index() };
}

JASMINE_EXPORT JasmineNode jasmine_create_set_index(JasmineFunction function, JasmineType type, JasmineOperand dest, JasmineType index_type, JasmineOperand index, JasmineOperand src) {
    return JasmineNode { .index = (int32_t)F(function).addNode(Opcode::SET_INDEX, type.index, O(dest), F(function).typeOperand(index_type.index), O(index), O(src)).index() };
}

JASMINE_EXPORT JasmineNode jasmine_create_addr(JasmineFunction function, JasmineType type, JasmineOperand dest, JasmineOperand base) {
    return JasmineNode { .index = (int32_t)F(function).addNode(Opcode::ADDR, type.index, O(dest), O(base)).index() };
}

JASMINE_EXPORT JasmineNode jasmine_create_addr_field(JasmineFunction function, JasmineType type, JasmineOperand dest, JasmineOperand base, uint32_t field_index) {
    return JasmineNode { .index = (int32_t)F(function).addNode(Opcode::ADDR_FIELD, type.index, O(dest), O(base), F(function).intConst(field_index)).index() };
}

JASMINE_EXPORT JasmineNode jasmine_create_addr_index(JasmineFunction function, JasmineType type, JasmineOperand dest, JasmineOperand base, JasmineType index_type, JasmineOperand index) {
    return JasmineNode { .index = (int32_t)F(function).addNode(Opcode::ADDR_INDEX, type.index, O(dest), O(base), F(function).typeOperand(index_type.index), O(index)).index() };
}

JASMINE_EXPORT JasmineNode jasmine_create_load(JasmineFunction function, JasmineType type, JasmineOperand dest, JasmineOperand src) {
    return JasmineNode { .index = (int32_t)F(function).addNode(Opcode::LOAD, type.index, O(dest), O(src)).index() };
}

JASMINE_EXPORT JasmineNode jasmine_create_load_field(JasmineFunction function, JasmineType type, JasmineOperand dest, JasmineOperand ptr, uint32_t field_index) {
    return JasmineNode { .index = (int32_t)F(function).addNode(Opcode::LOAD_FIELD, type.index, O(dest), O(ptr), F(function).intConst(field_index)).index() };
}

JASMINE_EXPORT JasmineNode jasmine_create_load_index(JasmineFunction function, JasmineType type, JasmineOperand dest, JasmineOperand ptr, JasmineType index_type, JasmineOperand index) {
    return JasmineNode { .index = (int32_t)F(function).addNode(Opcode::LOAD_INDEX, type.index, O(dest), O(ptr), F(function).typeOperand(index_type.index), O(index)).index() };
}

JASMINE_EXPORT JasmineNode jasmine_create_store(JasmineFunction function, JasmineType type, JasmineOperand dest, JasmineOperand src) {
    return JasmineNode { .index = (int32_t)F(function).addNode(Opcode::STORE, type.index, O(dest), O(src)).index() };
}

JASMINE_EXPORT JasmineNode jasmine_create_store_field(JasmineFunction function, JasmineType type, JasmineOperand ptr, uint32_t field_index, JasmineOperand src) {
    return JasmineNode { .index = (int32_t)F(function).addNode(Opcode::STORE_FIELD, type.index, O(ptr), F(function).intConst(field_index), O(src)).index() };
}

JASMINE_EXPORT JasmineNode jasmine_create_store_index(JasmineFunction function, JasmineType type, JasmineOperand ptr, JasmineType index_type, JasmineOperand index, JasmineOperand src) {
    return JasmineNode { .index = (int32_t)F(function).addNode(Opcode::STORE_INDEX, type.index, O(ptr), F(function).typeOperand(index_type.index), O(index), O(src)).index() };
}

JASMINE_EXPORT JasmineNode jasmine_create_offset_field(JasmineFunction function, JasmineType type, JasmineOperand dest, JasmineOperand ptr, uint32_t field_index) {
    return JasmineNode { .index = (int32_t)F(function).addNode(Opcode::OFFSET_FIELD, type.index, O(dest), O(ptr), F(function).intConst(field_index)).index() };
}

JASMINE_EXPORT JasmineNode jasmine_create_offset_index(JasmineFunction function, JasmineType type, JasmineOperand dest, JasmineOperand ptr, JasmineType index_type, JasmineOperand index) {
    return JasmineNode { .index = (int32_t)F(function).addNode(Opcode::OFFSET_INDEX, type.index, O(dest), O(ptr), F(function).typeOperand(index_type.index), O(index)).index() };
}

JASMINE_EXPORT JasmineNode jasmine_create_br(JasmineFunction function, JasmineEdge dest) {
    return JasmineNode { .index = (int32_t)F(function).addNode(Opcode::BR, VOID, F(function).branch(dest.index)).index() };
}

JASMINE_EXPORT JasmineNode jasmine_create_br_if(JasmineFunction function, JasmineType type, JasmineOperand src, JasmineEdge ifTrue, JasmineEdge ifFalse) {
    return JasmineNode { .index = (int32_t)F(function).addNode(Opcode::BR_IF, type.index, O(src), F(function).branch(ifTrue.index), F(function).branch(ifFalse.index)).index() };
}

JASMINE_EXPORT JasmineNode jasmine_create_br_if_not(JasmineFunction function, JasmineType type, JasmineOperand src, JasmineEdge ifTrue, JasmineEdge ifFalse) {
    return JasmineNode { .index = (int32_t)F(function).addNode(Opcode::BR_IF_NOT, type.index, O(src), F(function).branch(ifTrue.index), F(function).branch(ifFalse.index)).index() };
}

JASMINE_EXPORT JasmineNode jasmine_create_br_lt(JasmineFunction function, JasmineType type, JasmineOperand left, JasmineOperand right, JasmineEdge ifTrue, JasmineEdge ifFalse) {
    return JasmineNode { .index = (int32_t)F(function).addNode(Opcode::BR_LT, type.index, O(left), O(right), F(function).branch(ifTrue.index), F(function).branch(ifFalse.index)).index() };
}

JASMINE_EXPORT JasmineNode jasmine_create_br_le(JasmineFunction function, JasmineType type, JasmineOperand left, JasmineOperand right, JasmineEdge ifTrue, JasmineEdge ifFalse) {
    return JasmineNode { .index = (int32_t)F(function).addNode(Opcode::BR_LE, type.index, O(left), O(right), F(function).branch(ifTrue.index), F(function).branch(ifFalse.index)).index() };
}

JASMINE_EXPORT JasmineNode jasmine_create_br_gt(JasmineFunction function, JasmineType type, JasmineOperand left, JasmineOperand right, JasmineEdge ifTrue, JasmineEdge ifFalse) {
    return JasmineNode { .index = (int32_t)F(function).addNode(Opcode::BR_GT, type.index, O(left), O(right), F(function).branch(ifTrue.index), F(function).branch(ifFalse.index)).index() };
}

JASMINE_EXPORT JasmineNode jasmine_create_br_ge(JasmineFunction function, JasmineType type, JasmineOperand left, JasmineOperand right, JasmineEdge ifTrue, JasmineEdge ifFalse) {
    return JasmineNode { .index = (int32_t)F(function).addNode(Opcode::BR_GE, type.index, O(left), O(right), F(function).branch(ifTrue.index), F(function).branch(ifFalse.index)).index() };
}

JASMINE_EXPORT JasmineNode jasmine_create_br_eq(JasmineFunction function, JasmineType type, JasmineOperand left, JasmineOperand right, JasmineEdge ifTrue, JasmineEdge ifFalse) {
    return JasmineNode { .index = (int32_t)F(function).addNode(Opcode::BR_EQ, type.index, O(left), O(right), F(function).branch(ifTrue.index), F(function).branch(ifFalse.index)).index() };
}

JASMINE_EXPORT JasmineNode jasmine_create_br_ne(JasmineFunction function, JasmineType type, JasmineOperand left, JasmineOperand right, JasmineEdge ifTrue, JasmineEdge ifFalse) {
    return JasmineNode { .index = (int32_t)F(function).addNode(Opcode::BR_NE, type.index, O(left), O(right), F(function).branch(ifTrue.index), F(function).branch(ifFalse.index)).index() };
}

JASMINE_EXPORT JasmineNode jasmine_create_call(JasmineFunction function, JasmineType type, JasmineOperand dest, JasmineOperand callee, JasmineOperand* arguments, size_t argument_count) {
    return JasmineNode { .index = (int32_t)F(function).addNode(Opcode::CALL, type.index, O(dest), O(callee), const_slice<Operand>{ (Operand*)arguments, (iword)argument_count }).index() };
}

JASMINE_EXPORT JasmineNode jasmine_create_call_void(JasmineFunction function, JasmineType type, JasmineOperand callee, JasmineOperand* arguments, size_t argument_count) {
    return JasmineNode { .index = (int32_t)F(function).addNode(Opcode::CALL_VOID, type.index, O(callee), const_slice<Operand>{ (Operand*)arguments, (iword)argument_count }).index() };
}

JASMINE_EXPORT JasmineNode jasmine_create_ret(JasmineFunction function, JasmineType type, JasmineOperand return_value) {
    return JasmineNode { .index = (int32_t)F(function).addNode(Opcode::RET, type.index, O(return_value)).index() };
}

JASMINE_EXPORT JasmineNode jasmine_create_ret_void(JasmineFunction function) {
    return JasmineNode { .index = (int32_t)F(function).addNode(Opcode::RET, VOID).index() };
}

JASMINE_EXPORT JasmineNode jasmine_create_convert(JasmineFunction function, JasmineType destType, JasmineOperand dest, JasmineType srcType, JasmineOperand src) {
    return JasmineNode { .index = (int32_t)F(function).addNode(Opcode::CONVERT, destType.index, O(dest), F(function).typeOperand(srcType.index), O(src)).index() };
}

JASMINE_EXPORT JasmineNode jasmine_create_bitcast(JasmineFunction function, JasmineType destType, JasmineOperand dest, JasmineType srcType, JasmineOperand src) {
    return JasmineNode { .index = (int32_t)F(function).addNode(Opcode::BITCAST, destType.index, O(dest), F(function).typeOperand(srcType.index), O(src)).index() };
}

JASMINE_EXPORT JasmineNode jasmine_create_trap(JasmineFunction function) {
    return JasmineNode { .index = (int32_t)F(function).addNode(Opcode::TRAP, VOID).index() };
}

JASMINE_EXPORT JasmineNode jasmine_create_alloca(JasmineFunction function, JasmineType type, JasmineOperand dest, JasmineOperand size) {
    return JasmineNode { .index = (int32_t)F(function).addNode(Opcode::ALLOCA, type.index, O(dest), O(size)).index() };
}

/*
 * Global data
 */

JASMINE_EXPORT JasmineValue jasmine_integer_value(JasmineModule module, JasmineType type, int64_t constant) {
    return JasmineValue { .bits = M(module).values.makeInt((TypeKind)type.index, constant).bits };
}

JASMINE_EXPORT JasmineValue jasmine_f32_value(JasmineModule module, float constant) {
    return JasmineValue { .bits = M(module).values.makeF32(constant).bits };
}

JASMINE_EXPORT JasmineValue jasmine_f64_value(JasmineModule module, double constant) {
    return JasmineValue { .bits = M(module).values.makeF64(constant).bits };
}

JASMINE_EXPORT int64_t jasmine_get_integer_value(JasmineModule module, JasmineValue value) {
    return V(value).intValue(&M(module).constants);
}

JASMINE_EXPORT float jasmine_get_f32_value(JasmineModule module, JasmineValue value) {
    return V(value).f32Value(&M(module).constants);
}

JASMINE_EXPORT double jasmine_get_f64_value(JasmineModule module, JasmineValue value) {
    return V(value).f64Value(&M(module).constants);
}

JASMINE_EXPORT JasmineValue jasmine_function_ref_value(JasmineModule module, const char* name, size_t name_length) {
    return JasmineValue { .bits = M(module).values.makeFuncref(M(module).syms[{name, (iword)name_length}]).bits };
}

JASMINE_EXPORT JasmineValue jasmine_data_ref_value(JasmineModule module, const char* name, size_t name_length) {
    return JasmineValue { .bits = M(module).values.makeDataref(M(module).syms[{name, (iword)name_length}]).bits };
}

JASMINE_EXPORT JasmineValue jasmine_static_ref_value(JasmineModule module, const char* name, size_t name_length) {
    return JasmineValue { .bits = M(module).values.makeStaticref(M(module).syms[{name, (iword)name_length}]).bits };
}

JASMINE_EXPORT JasmineValue jasmine_struct_value(JasmineModule module, JasmineType type, const JasmineValue* fields, size_t field_count) {
    return JasmineValue { .bits = M(module).values.makeStruct(type.index, { (Value*)fields, (iword)field_count }).bits };
}

JASMINE_EXPORT JasmineValue jasmine_i8_array_value(JasmineModule module, const int8_t* elements, size_t element_count) {
    return { .bits = M(module).values.makeI8Array(M(module).arrayType(I8, element_count), { (const i8*)elements, (iword)element_count }).bits };
}

JASMINE_EXPORT JasmineValue jasmine_u8_array_value(JasmineModule module, const uint8_t* elements, size_t element_count) {
    return { .bits = M(module).values.makeU8Array(M(module).arrayType(U8, element_count), { elements, (iword)element_count }).bits };
}

JASMINE_EXPORT JasmineValue jasmine_i16_array_value(JasmineModule module, const int16_t* elements, size_t element_count) {
    return { .bits = M(module).values.makeI16Array(M(module).arrayType(I16, element_count), { elements, (iword)element_count }).bits };
}

JASMINE_EXPORT JasmineValue jasmine_u16_array_value(JasmineModule module, const uint16_t* elements, size_t element_count) {
    return { .bits = M(module).values.makeU16Array(M(module).arrayType(U16, element_count), { elements, (iword)element_count }).bits };
}

JASMINE_EXPORT JasmineValue jasmine_i32_array_value(JasmineModule module, const int32_t* elements, size_t element_count) {
    return { .bits = M(module).values.makeI32Array(M(module).arrayType(I32, element_count), { elements, (iword)element_count }).bits };
}

JASMINE_EXPORT JasmineValue jasmine_u32_array_value(JasmineModule module, const uint32_t* elements, size_t element_count) {
    return { .bits = M(module).values.makeU32Array(M(module).arrayType(U32, element_count), { elements, (iword)element_count }).bits };
}

JASMINE_EXPORT JasmineValue jasmine_i64_array_value(JasmineModule module, const int64_t* elements, size_t element_count) {
    return { .bits = M(module).values.makeI64Array(M(module).arrayType(I64, element_count), { elements, (iword)element_count }).bits };
}

JASMINE_EXPORT JasmineValue jasmine_u64_array_value(JasmineModule module, const uint64_t* elements, size_t element_count) {
    return { .bits = M(module).values.makeU64Array(M(module).arrayType(U64, element_count), { elements, (iword)element_count }).bits };
}

JASMINE_EXPORT JasmineValue jasmine_f32_array_value(JasmineModule module, const float* elements, size_t element_count) {
    return { .bits = M(module).values.makeF32Array(M(module).arrayType(jasmine::F32, element_count), { elements, (iword)element_count }).bits };
}

JASMINE_EXPORT JasmineValue jasmine_f64_array_value(JasmineModule module, const double* elements, size_t element_count) {
    return { .bits = M(module).values.makeF64Array(M(module).arrayType(jasmine::F64, element_count), { elements, (iword)element_count }).bits };
}

JASMINE_EXPORT JasmineValue jasmine_value_array_value(JasmineModule module, JasmineType elementType, const JasmineValue* elements, size_t element_count) {
    return { .bits = M(module).values.makeValueArray(M(module).arrayType(elementType.index, element_count), { (Value*)elements, (iword)element_count }).bits };
}

JASMINE_EXPORT void jasmine_define_data(JasmineModule module, JasmineType type, const char* name, size_t name_length, JasmineValue value) {
    M(module).values.defData(M(module).syms[{name, (iword)name_length}], V(value));
}

JASMINE_EXPORT void jasmine_define_static(JasmineModule module, JasmineType type, const char* name, size_t name_length, JasmineValue value) {
    M(module).values.defStatic(M(module).syms[{name, (iword)name_length}], V(value));
}

JASMINE_EXPORT void jasmine_define_static_uninit(JasmineModule module, JasmineType type, const char* name, size_t name_length) {
    M(module).values.defStaticUninit(M(module).syms[{name, (iword)name_length}], type.index);
}

/*
 * IO
 */

JASMINE_EXPORT size_t jasmine_format_module(int8_t* buffer, size_t buffer_size, JasmineModule module) {
    slice<i8> buffer_slice = { (i8*)buffer, (iword)buffer_size };
    auto result_slice = format(buffer_slice, M(module));
    return buffer_slice.size() - result_slice.size();
}

JASMINE_EXPORT size_t jasmine_format_type(int8_t* buffer, size_t buffer_size, JasmineModule module, JasmineType type) {
    slice<i8> buffer_slice = { (i8*)buffer, (iword)buffer_size };
    auto result_slice = format(buffer_slice, TypeLogger { M(module), type.index });
    return buffer_slice.size() - result_slice.size();
}

JASMINE_EXPORT size_t jasmine_format_function(int8_t* buffer, size_t buffer_size, JasmineFunction function) {
    slice<i8> buffer_slice = { (i8*)buffer, (iword)buffer_size };
    auto result_slice = format(buffer_slice, F(function));
    return buffer_slice.size() - result_slice.size();
}

JASMINE_EXPORT size_t jasmine_format_block(int8_t* buffer, size_t buffer_size, JasmineFunction function, JasmineBlock block) {
    slice<i8> buffer_slice = { (i8*)buffer, (iword)buffer_size };
    auto result_slice = format(buffer_slice, F(function).block(block.index));
    return buffer_slice.size() - result_slice.size();
}

JASMINE_EXPORT size_t jasmine_format_edge(int8_t* buffer, size_t buffer_size, JasmineFunction function, JasmineEdge edge) {
    slice<i8> buffer_slice = { (i8*)buffer, (iword)buffer_size };
    auto result_slice = format(buffer_slice, F(function).edge(edge.index));
    return buffer_slice.size() - result_slice.size();
}

JASMINE_EXPORT size_t jasmine_format_node(int8_t* buffer, size_t buffer_size, JasmineFunction function, JasmineNode node) {
    slice<i8> buffer_slice = { (i8*)buffer, (iword)buffer_size };
    auto result_slice = format(buffer_slice, F(function).node(node.index));
    return buffer_slice.size() - result_slice.size();
}

JASMINE_EXPORT size_t jasmine_format_operand(int8_t* buffer, size_t buffer_size, JasmineFunction function, JasmineOperand operand) {
    slice<i8> buffer_slice = { (i8*)buffer, (iword)buffer_size };
    auto result_slice = format(buffer_slice, OperandLogger { F(function), O(operand) });
    return buffer_slice.size() - result_slice.size();
}

JASMINE_EXPORT void jasmine_print_module(JasmineOutputStream stream, JasmineModule module) {
    write(stream == JasmineStdout ? io_stdout : io_stderr, M(module));
}

JASMINE_EXPORT void jasmine_print_function(JasmineOutputStream stream, JasmineFunction function) {
    write(stream == JasmineStdout ? io_stdout : io_stderr, F(function));
}

JASMINE_EXPORT void jasmine_print_block(JasmineOutputStream stream, JasmineFunction function, JasmineBlock block) {
    write(stream == JasmineStdout ? io_stdout : io_stderr, F(function).block(block.index));
}

JASMINE_EXPORT void jasmine_print_edge(JasmineOutputStream stream, JasmineFunction function, JasmineEdge edge) {
    write(stream == JasmineStdout ? io_stdout : io_stderr, F(function).edge(edge.index));
}

JASMINE_EXPORT void jasmine_print_type(JasmineOutputStream stream, JasmineModule module, JasmineType type) {
    write(stream == JasmineStdout ? io_stdout : io_stderr, TypeLogger { M(module), type.index });
}

JASMINE_EXPORT void jasmine_print_node(JasmineOutputStream stream, JasmineFunction function, JasmineNode node) {
    write(stream == JasmineStdout ? io_stdout : io_stderr, F(function).node(node.index));
}

JASMINE_EXPORT void jasmine_print_operand(JasmineOutputStream stream, JasmineFunction function, JasmineOperand operand) {
    write(stream == JasmineStdout ? io_stdout : io_stderr, OperandLogger { F(function), O(operand) });
}

JASMINE_EXPORT void jasmine_encode_module(FILE* file, JasmineModule module);

JASMINE_EXPORT JasmineModule jasmine_decode_module(FILE* file);

/*
 * Passes
 */

#define PC(...) (*(PassContext*)((__VA_ARGS__).handle))

JASMINE_EXPORT JasminePassContext jasmine_create_pass_context(JasmineFunction function) {
    auto& pc = F(function).mod->passContext();
    pc.reset();
    return JasminePassContext { .handle = &pc };
}

JASMINE_EXPORT void jasmine_destroy_pass_context(JasminePassContext context) {
    // No-op, because we reset the pass context when we create it.
}

JASMINE_EXPORT void jasmine_perform_typechecking(JasminePassContext context, JasmineFunction function) {
    typecheck(PC(context), F(function));
}

JASMINE_EXPORT void jasmine_perform_type_propagation(JasminePassContext context, JasmineFunction function) {
    assignTypesUnchecked(PC(context), F(function));
}

JASMINE_EXPORT void jasmine_validate_after_lowering(JasminePassContext context, JasmineFunction function) {
    validateAfterLowering(PC(context), F(function));
}

JASMINE_EXPORT void jasmine_ensure_ssa(JasminePassContext context, JasmineFunction function) {
    enforceSSA(PC(context), F(function));
}

JASMINE_EXPORT void jasmine_perform_constant_folding(JasminePassContext context, JasmineFunction function) {
    foldConstants(PC(context), F(function));
}

JASMINE_EXPORT void jasmine_perform_dead_code_elimination(JasminePassContext context, JasmineFunction function) {
    simplify(PC(context), F(function));
}

JASMINE_EXPORT void jasmine_perform_inlining(JasminePassContext context, JasmineFunction function) {
    inlineCalls(PC(context), F(function));
}

JASMINE_EXPORT void jasmine_perform_loop_unrolling(JasminePassContext context, JasmineFunction function) {
    unrollLoops(PC(context), F(function));
}

JASMINE_EXPORT void jasmine_perform_strength_reduction(JasminePassContext context, JasmineFunction function) {
    reduceStrength(PC(context), F(function));
}

JASMINE_EXPORT void jasmine_perform_scalar_replacement(JasminePassContext context, JasmineFunction function) {
    // scalarReplacementOfAggregate(PC(context), F(function));
}

JASMINE_EXPORT void jasmine_perform_register_allocation(JasminePassContext context, JasmineFunction function, JasmineRegisterAllocationMode mode) {
    PC(context).targetSpecificPasses->allocateRegisters(PC(context), F(function), (jasmine::RegisterAllocationMode)mode);
}

JASMINE_EXPORT void jasmine_perform_stack_allocation(JasminePassContext context, JasmineFunction function) {
    PC(context).targetSpecificPasses->allocateStackOnly(PC(context), F(function));
}

JASMINE_EXPORT void jasmine_finalize_cfg(JasminePassContext context, JasmineFunction function) {
    finalizeCFG(PC(context), F(function));
}

JASMINE_EXPORT JasmineAssembly jasmine_create_assembly(JasmineModule mod) {
    Assembly* as = new Assembly(M(mod).syms);
    return JasmineAssembly { .handle = as };
}

JASMINE_EXPORT void jasmine_compile(JasmineAssembly as, JasmineModule mod, JasmineOptimizationLevel optLevel, jasmine_bool_t validate) {
    compileWithOptimizations(&M(mod), *(Assembly*)as.handle, optLevel, validate);
}

JASMINE_EXPORT void jasmine_compile_function_only(JasmineAssembly as, JasmineFunction func, JasmineOptimizationLevel optLevel, jasmine_bool_t validate) {
    compileFunctionOnly(&F(func), *(Assembly*)as.handle, optLevel, validate);
}

JASMINE_EXPORT void jasmine_compile_module_only(JasmineAssembly as, JasmineModule mod, JasmineOptimizationLevel optLevel, jasmine_bool_t validate) {
    compileModuleOnly(&M(mod), *(Assembly*)as.handle, optLevel, validate);
}

/*
 * Lowering
 */

JASMINE_EXPORT JasmineAssembly jasmine_generate_assembly(JasminePassContext context, JasmineFunction function) {
    Assembly* as = new Assembly(F(function).syms());
    PC(context).targetSpecificPasses->generateAssembly(PC(context), F(function), *as);
    return JasmineAssembly { .handle = as };
}

JASMINE_EXPORT void jasmine_destroy_assembly(JasmineAssembly assembly) {
    delete (Assembly*)assembly.handle;
}

JASMINE_EXPORT void jasmine_write_relocatable_elf_object(JasmineAssembly assembly, const char* path, size_t path_length) {
    auto f = file::open({ path, (iword)path_length }, file::WRITE);
    auto as = (Assembly*)assembly.handle;
    as->writeELFObject(f);
    file::close(f);
}

/*
 * Linking and loading
 */

JASMINE_EXPORT JasmineAssembly jasmine_join_assemblies(JasmineAssembly* assemblies, size_t assembly_count) {
    Assembly* result = new Assembly(((Assembly*)assemblies[0].handle)->symtab);
    const_slice<const Assembly*> sources = { (const Assembly**)assemblies, (iword)assembly_count };
    join(*result, sources);
    return JasmineAssembly { .handle = result };
}

size_t totalSize(const Assembly* as) {
    return as->code.size() + as->data.size() + as->stat.size();
}

JASMINE_EXPORT JasmineAssembly jasmine_join_assemblies_in_place(JasmineAssembly* assemblies, size_t assembly_count) {
    slice<Assembly*> sources = { (Assembly**)assemblies, (iword)assembly_count };
    sort(sources, [](const Assembly* a, const Assembly* b) -> bool { return totalSize(a) < totalSize(b); });
    const_slice<const Assembly*> remaining_sources = { sources.data() + 1, sources.size() - 1 };
    join(*sources[0], remaining_sources);
    for (auto p : remaining_sources)
        delete p;
    return JasmineAssembly { .handle = sources[0] };
}

JASMINE_EXPORT JasmineExecutable jasmine_link_assemblies(JasmineAssembly* assemblies, size_t assembly_count) {
    const_slice<const Assembly*> sources = { (const Assembly**)assemblies, (iword)assembly_count };
    Assembly joined(sources[0]->symtab);
    join(joined, sources);
    LinkedAssembly* linked = new LinkedAssembly();
    joined.linkInto(*linked);
    return JasmineExecutable { .handle = linked };
}

JASMINE_EXPORT void jasmine_load_executable(JasmineExecutable executable) {
    ((LinkedAssembly*)executable.handle)->load();
}

JASMINE_EXPORT void* jasmine_lookup_symbol(JasmineExecutable executable, const char* symbol, size_t symbol_length) {
    LinkedAssembly& assembly = *(LinkedAssembly*)executable.handle;
    return assembly.lookup<void>(const_slice<i8>{ symbol, (iword)symbol_length });
}

JASMINE_EXPORT void jasmine_destroy_executable(JasmineExecutable executable) {
    delete (LinkedAssembly*)executable.handle;
}

struct Builder {
    Module* module;
    Function* function;
    BlockIndex block;

    Builder(Module* module_in):
        module(module_in) {}

    JasmineFunction func() {
        return { .handle = function };
    }

    void add(JasmineNode node) {
        function->block(block).addNode(function->node(node.index));
    }
};

#define B(...) (*(Builder*) (__VA_ARGS__).handle)

JASMINE_EXPORT JasmineBuilder jasmine_create_builder(JasmineModule module) {
    return { .handle = new Builder(&M(module)) };
}

JASMINE_EXPORT void jasmine_destroy_builder(JasmineBuilder builder) {
    delete (Builder*)builder.handle;
}

JASMINE_EXPORT void jasmine_builder_set_function(JasmineBuilder builder, JasmineFunction function) {
    B(builder).function = &F(function);
}

JASMINE_EXPORT void jasmine_builder_set_block(JasmineBuilder builder, JasmineBlock block) {
    B(builder).block = block.index;
}

JASMINE_EXPORT JasmineFunction jasmine_current_function(JasmineBuilder builder) {
    return B(builder).func();
}

JASMINE_EXPORT JasmineBlock jasmine_current_block(JasmineBuilder builder) {
    return { .index = (int32_t)B(builder).block };
}

JASMINE_EXPORT void jasmine_append_nop(JasmineBuilder builder) { B(builder).add(jasmine_create_nop(B(builder).func())); }
JASMINE_EXPORT void jasmine_append_var(JasmineBuilder builder, JasmineType type, JasmineOperand dest) { B(builder).add(jasmine_create_var(B(builder).func(), type, dest)); }
JASMINE_EXPORT void jasmine_append_mov(JasmineBuilder builder, JasmineType type, JasmineOperand dest, JasmineOperand src) { B(builder).add(jasmine_create_mov(B(builder).func(), type, dest, src)); }
JASMINE_EXPORT void jasmine_append_pack(JasmineBuilder builder, JasmineType type, JasmineOperand dest, JasmineOperand* fields, size_t field_count) { B(builder).add(jasmine_create_pack(B(builder).func(), type, dest, fields, field_count)); }
JASMINE_EXPORT void jasmine_append_unpack(JasmineBuilder builder, JasmineType type, JasmineOperand* outputs, size_t output_count, JasmineOperand src) { B(builder).add(jasmine_create_unpack(B(builder).func(), type, outputs, output_count, src)); }
JASMINE_EXPORT void jasmine_append_new(JasmineBuilder builder, JasmineType type, JasmineOperand dest, JasmineOperand src) { B(builder).add(jasmine_create_new(B(builder).func(), type, dest, src)); }
JASMINE_EXPORT void jasmine_append_new_struct(JasmineBuilder builder, JasmineType type, JasmineOperand dest, JasmineOperand* fields, size_t field_count) { B(builder).add(jasmine_create_new_struct(B(builder).func(), type, dest, fields, field_count)); }
JASMINE_EXPORT void jasmine_append_new_array(JasmineBuilder builder, JasmineType type, JasmineOperand dest, JasmineOperand* elements, size_t element_count) { B(builder).add(jasmine_create_new_array(B(builder).func(), type, dest, elements, element_count)); }
JASMINE_EXPORT void jasmine_append_add(JasmineBuilder builder, JasmineType type, JasmineOperand dest, JasmineOperand left, JasmineOperand right) { B(builder).add(jasmine_create_add(B(builder).func(), type, dest, left, right)); }
JASMINE_EXPORT void jasmine_append_sub(JasmineBuilder builder, JasmineType type, JasmineOperand dest, JasmineOperand left, JasmineOperand right) { B(builder).add(jasmine_create_sub(B(builder).func(), type, dest, left, right)); }
JASMINE_EXPORT void jasmine_append_mul(JasmineBuilder builder, JasmineType type, JasmineOperand dest, JasmineOperand left, JasmineOperand right) { B(builder).add(jasmine_create_mul(B(builder).func(), type, dest, left, right)); }
JASMINE_EXPORT void jasmine_append_div(JasmineBuilder builder, JasmineType type, JasmineOperand dest, JasmineOperand left, JasmineOperand right) { B(builder).add(jasmine_create_div(B(builder).func(), type, dest, left, right)); }
JASMINE_EXPORT void jasmine_append_rem(JasmineBuilder builder, JasmineType type, JasmineOperand dest, JasmineOperand left, JasmineOperand right) { B(builder).add(jasmine_create_rem(B(builder).func(), type, dest, left, right)); }
JASMINE_EXPORT void jasmine_append_neg(JasmineBuilder builder, JasmineType type, JasmineOperand dest, JasmineOperand src) { B(builder).add(jasmine_create_neg(B(builder).func(), type, dest, src)); }
JASMINE_EXPORT void jasmine_append_abs(JasmineBuilder builder, JasmineType type, JasmineOperand dest, JasmineOperand src) { B(builder).add(jasmine_create_abs(B(builder).func(), type, dest, src)); }
JASMINE_EXPORT void jasmine_append_min(JasmineBuilder builder, JasmineType type, JasmineOperand dest, JasmineOperand left, JasmineOperand right) { B(builder).add(jasmine_create_min(B(builder).func(), type, dest, left, right)); }
JASMINE_EXPORT void jasmine_append_max(JasmineBuilder builder, JasmineType type, JasmineOperand dest, JasmineOperand left, JasmineOperand right) { B(builder).add(jasmine_create_max(B(builder).func(), type, dest, left, right)); }
JASMINE_EXPORT void jasmine_append_sqrt(JasmineBuilder builder, JasmineType type, JasmineOperand dest, JasmineOperand src) { B(builder).add(jasmine_create_sqrt(B(builder).func(), type, dest, src)); }
JASMINE_EXPORT void jasmine_append_round(JasmineBuilder builder, JasmineType type, JasmineOperand dest, JasmineOperand src) { B(builder).add(jasmine_create_round(B(builder).func(), type, dest, src)); }
JASMINE_EXPORT void jasmine_append_floor(JasmineBuilder builder, JasmineType type, JasmineOperand dest, JasmineOperand src) { B(builder).add(jasmine_create_floor(B(builder).func(), type, dest, src)); }
JASMINE_EXPORT void jasmine_append_ceil(JasmineBuilder builder, JasmineType type, JasmineOperand dest, JasmineOperand src) { B(builder).add(jasmine_create_ceil(B(builder).func(), type, dest, src)); }
JASMINE_EXPORT void jasmine_append_and(JasmineBuilder builder, JasmineType type, JasmineOperand dest, JasmineOperand left, JasmineOperand right) { B(builder).add(jasmine_create_and(B(builder).func(), type, dest, left, right)); }
JASMINE_EXPORT void jasmine_append_or(JasmineBuilder builder, JasmineType type, JasmineOperand dest, JasmineOperand left, JasmineOperand right) { B(builder).add(jasmine_create_or(B(builder).func(), type, dest, left, right)); }
JASMINE_EXPORT void jasmine_append_xor(JasmineBuilder builder, JasmineType type, JasmineOperand dest, JasmineOperand left, JasmineOperand right) { B(builder).add(jasmine_create_xor(B(builder).func(), type, dest, left, right)); }
JASMINE_EXPORT void jasmine_append_not(JasmineBuilder builder, JasmineType type, JasmineOperand dest, JasmineOperand src) { B(builder).add(jasmine_create_not(B(builder).func(), type, dest, src)); }
JASMINE_EXPORT void jasmine_append_shl(JasmineBuilder builder, JasmineType type, JasmineOperand dest, JasmineOperand left, JasmineOperand right) { B(builder).add(jasmine_create_shl(B(builder).func(), type, dest, left, right)); }
JASMINE_EXPORT void jasmine_append_shr(JasmineBuilder builder, JasmineType type, JasmineOperand dest, JasmineOperand left, JasmineOperand right) { B(builder).add(jasmine_create_shr(B(builder).func(), type, dest, left, right)); }
JASMINE_EXPORT void jasmine_append_rol(JasmineBuilder builder, JasmineType type, JasmineOperand dest, JasmineOperand left, JasmineOperand right) { B(builder).add(jasmine_create_rol(B(builder).func(), type, dest, left, right)); }
JASMINE_EXPORT void jasmine_append_ror(JasmineBuilder builder, JasmineType type, JasmineOperand dest, JasmineOperand left, JasmineOperand right) { B(builder).add(jasmine_create_ror(B(builder).func(), type, dest, left, right)); }
JASMINE_EXPORT void jasmine_append_lzcnt(JasmineBuilder builder, JasmineType type, JasmineOperand dest, JasmineOperand src) { B(builder).add(jasmine_create_lzcnt(B(builder).func(), type, dest, src)); }
JASMINE_EXPORT void jasmine_append_tzcnt(JasmineBuilder builder, JasmineType type, JasmineOperand dest, JasmineOperand src) { B(builder).add(jasmine_create_tzcnt(B(builder).func(), type, dest, src)); }
JASMINE_EXPORT void jasmine_append_popcnt(JasmineBuilder builder, JasmineType type, JasmineOperand dest, JasmineOperand src) { B(builder).add(jasmine_create_popcnt(B(builder).func(), type, dest, src)); }
JASMINE_EXPORT void jasmine_append_is_lt(JasmineBuilder builder, JasmineType type, JasmineOperand dest, JasmineOperand left, JasmineOperand right) { B(builder).add(jasmine_create_is_lt(B(builder).func(), type, dest, left, right)); }
JASMINE_EXPORT void jasmine_append_is_le(JasmineBuilder builder, JasmineType type, JasmineOperand dest, JasmineOperand left, JasmineOperand right) { B(builder).add(jasmine_create_is_le(B(builder).func(), type, dest, left, right)); }
JASMINE_EXPORT void jasmine_append_is_gt(JasmineBuilder builder, JasmineType type, JasmineOperand dest, JasmineOperand left, JasmineOperand right) { B(builder).add(jasmine_create_is_gt(B(builder).func(), type, dest, left, right)); }
JASMINE_EXPORT void jasmine_append_is_ge(JasmineBuilder builder, JasmineType type, JasmineOperand dest, JasmineOperand left, JasmineOperand right) { B(builder).add(jasmine_create_is_ge(B(builder).func(), type, dest, left, right)); }
JASMINE_EXPORT void jasmine_append_is_eq(JasmineBuilder builder, JasmineType type, JasmineOperand dest, JasmineOperand left, JasmineOperand right) { B(builder).add(jasmine_create_is_eq(B(builder).func(), type, dest, left, right)); }
JASMINE_EXPORT void jasmine_append_is_ne(JasmineBuilder builder, JasmineType type, JasmineOperand dest, JasmineOperand left, JasmineOperand right) { B(builder).add(jasmine_create_is_ne(B(builder).func(), type, dest, left, right)); }
JASMINE_EXPORT void jasmine_append_get_field(JasmineBuilder builder, JasmineType type, JasmineOperand dest, JasmineOperand src, uint32_t field_index) { B(builder).add(jasmine_create_get_field(B(builder).func(), type, dest, src, field_index)); }
JASMINE_EXPORT void jasmine_append_get_index(JasmineBuilder builder, JasmineType type, JasmineOperand dest, JasmineOperand src, JasmineType index_type, JasmineOperand index) { B(builder).add(jasmine_create_get_index(B(builder).func(), type, dest, src, index_type, index)); }
JASMINE_EXPORT void jasmine_append_set_field(JasmineBuilder builder, JasmineType type, JasmineOperand dest, uint32_t field_index, JasmineOperand src) { B(builder).add(jasmine_create_set_field(B(builder).func(), type, dest, field_index, src)); }
JASMINE_EXPORT void jasmine_append_set_index(JasmineBuilder builder, JasmineType type, JasmineOperand dest, JasmineType index_type, JasmineOperand index, JasmineOperand src) { B(builder).add(jasmine_create_set_index(B(builder).func(), type, dest, index_type, index, src)); }
JASMINE_EXPORT void jasmine_append_addr(JasmineBuilder builder, JasmineType type, JasmineOperand dest, JasmineOperand src) { B(builder).add(jasmine_create_addr(B(builder).func(), type, dest, src)); }
JASMINE_EXPORT void jasmine_append_addr_field(JasmineBuilder builder, JasmineType type, JasmineOperand dest, JasmineOperand src, uint32_t field_index) { B(builder).add(jasmine_create_addr_field(B(builder).func(), type, dest, src, field_index)); }
JASMINE_EXPORT void jasmine_append_addr_index(JasmineBuilder builder, JasmineType type, JasmineOperand dest, JasmineOperand src, JasmineType index_type, JasmineOperand index) { B(builder).add(jasmine_create_addr_index(B(builder).func(), type, dest, src, index_type, index)); }
JASMINE_EXPORT void jasmine_append_load(JasmineBuilder builder, JasmineType type, JasmineOperand dest, JasmineOperand ptr) { B(builder).add(jasmine_create_load(B(builder).func(), type, dest, ptr)); }
JASMINE_EXPORT void jasmine_append_load_field(JasmineBuilder builder, JasmineType type, JasmineOperand dest, JasmineOperand ptr, uint32_t field_index) { B(builder).add(jasmine_create_load_field(B(builder).func(), type, dest, ptr, field_index)); }
JASMINE_EXPORT void jasmine_append_load_index(JasmineBuilder builder, JasmineType type, JasmineOperand dest, JasmineOperand ptr, JasmineType index_type, JasmineOperand index) { B(builder).add(jasmine_create_load_index(B(builder).func(), type, dest, ptr, index_type, index)); }
JASMINE_EXPORT void jasmine_append_store(JasmineBuilder builder, JasmineType type, JasmineOperand ptr, JasmineOperand src) { B(builder).add(jasmine_create_store(B(builder).func(), type, ptr, src)); }
JASMINE_EXPORT void jasmine_append_store_field(JasmineBuilder builder, JasmineType type, JasmineOperand ptr, uint32_t field_index, JasmineOperand src) { B(builder).add(jasmine_create_store_field(B(builder).func(), type, ptr, field_index, src)); }
JASMINE_EXPORT void jasmine_append_store_index(JasmineBuilder builder, JasmineType type, JasmineOperand ptr, JasmineType index_type, JasmineOperand index, JasmineOperand src) { B(builder).add(jasmine_create_store_index(B(builder).func(), type, ptr, index_type, index, src)); }
JASMINE_EXPORT void jasmine_append_offset_field(JasmineBuilder builder, JasmineType type, JasmineOperand dest, JasmineOperand src, uint32_t field_index) { B(builder).add(jasmine_create_offset_field(B(builder).func(), type, dest, src, field_index)); }
JASMINE_EXPORT void jasmine_append_offset_index(JasmineBuilder builder, JasmineType type, JasmineOperand dest, JasmineOperand src, JasmineType index_type, JasmineOperand index) { B(builder).add(jasmine_create_offset_index(B(builder).func(), type, dest, src, index_type, index)); }
JASMINE_EXPORT void jasmine_append_br(JasmineBuilder builder, JasmineEdge dest) { B(builder).add(jasmine_create_br(B(builder).func(), dest)); }
JASMINE_EXPORT void jasmine_append_br_if(JasmineBuilder builder, JasmineType type, JasmineOperand src, JasmineEdge ifTrue, JasmineEdge ifFalse) { B(builder).add(jasmine_create_br_if(B(builder).func(), type, src, ifTrue, ifFalse)); }
JASMINE_EXPORT void jasmine_append_br_if_not(JasmineBuilder builder, JasmineType type, JasmineOperand src, JasmineEdge ifTrue, JasmineEdge ifFalse) { B(builder).add(jasmine_create_br_if_not(B(builder).func(), type, src, ifTrue, ifFalse)); }
JASMINE_EXPORT void jasmine_append_br_lt(JasmineBuilder builder, JasmineType type, JasmineOperand left, JasmineOperand right, JasmineEdge ifTrue, JasmineEdge ifFalse) { B(builder).add(jasmine_create_br_lt(B(builder).func(), type, left, right, ifTrue, ifFalse)); }
JASMINE_EXPORT void jasmine_append_br_le(JasmineBuilder builder, JasmineType type, JasmineOperand left, JasmineOperand right, JasmineEdge ifTrue, JasmineEdge ifFalse) { B(builder).add(jasmine_create_br_le(B(builder).func(), type, left, right, ifTrue, ifFalse)); }
JASMINE_EXPORT void jasmine_append_br_gt(JasmineBuilder builder, JasmineType type, JasmineOperand left, JasmineOperand right, JasmineEdge ifTrue, JasmineEdge ifFalse) { B(builder).add(jasmine_create_br_gt(B(builder).func(), type, left, right, ifTrue, ifFalse)); }
JASMINE_EXPORT void jasmine_append_br_ge(JasmineBuilder builder, JasmineType type, JasmineOperand left, JasmineOperand right, JasmineEdge ifTrue, JasmineEdge ifFalse) { B(builder).add(jasmine_create_br_ge(B(builder).func(), type, left, right, ifTrue, ifFalse)); }
JASMINE_EXPORT void jasmine_append_br_eq(JasmineBuilder builder, JasmineType type, JasmineOperand left, JasmineOperand right, JasmineEdge ifTrue, JasmineEdge ifFalse) { B(builder).add(jasmine_create_br_eq(B(builder).func(), type, left, right, ifTrue, ifFalse)); }
JASMINE_EXPORT void jasmine_append_br_ne(JasmineBuilder builder, JasmineType type, JasmineOperand left, JasmineOperand right, JasmineEdge ifTrue, JasmineEdge ifFalse) { B(builder).add(jasmine_create_br_ne(B(builder).func(), type, left, right, ifTrue, ifFalse)); }
JASMINE_EXPORT void jasmine_append_call(JasmineBuilder builder, JasmineType type, JasmineOperand dest, JasmineOperand callee, JasmineOperand* arguments, size_t argument_count) { B(builder).add(jasmine_create_call(B(builder).func(), type, dest, callee, arguments, argument_count)); }
JASMINE_EXPORT void jasmine_append_call_void(JasmineBuilder builder, JasmineType type, JasmineOperand callee, JasmineOperand* arguments, size_t argument_count) { B(builder).add(jasmine_create_call_void(B(builder).func(), type, callee, arguments, argument_count)); }
JASMINE_EXPORT void jasmine_append_ret(JasmineBuilder builder, JasmineType type, JasmineOperand return_value) { B(builder).add(jasmine_create_ret(B(builder).func(), type, return_value)); }
JASMINE_EXPORT void jasmine_append_ret_void(JasmineBuilder builder) { B(builder).add(jasmine_create_ret_void(B(builder).func())); }
JASMINE_EXPORT void jasmine_append_convert(JasmineBuilder builder, JasmineType destType, JasmineOperand dest, JasmineType srcType, JasmineOperand src) { B(builder).add(jasmine_create_convert(B(builder).func(), destType, dest, srcType, src)); }
JASMINE_EXPORT void jasmine_append_bitcast(JasmineBuilder builder, JasmineType destType, JasmineOperand dest, JasmineType srcType, JasmineOperand src) { B(builder).add(jasmine_create_bitcast(B(builder).func(), destType, dest, srcType, src)); }
JASMINE_EXPORT void jasmine_append_trap(JasmineBuilder builder) { B(builder).add(jasmine_create_trap(B(builder).func())); }
JASMINE_EXPORT void jasmine_append_alloca(JasmineBuilder builder, JasmineType type, JasmineOperand dest, JasmineOperand size) { B(builder).add(jasmine_create_alloca(B(builder).func(), type, dest, size)); }

