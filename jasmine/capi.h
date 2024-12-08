#ifndef JASMINE_CAPI_H
#define JASMINE_CAPI_H

#include "stdint.h"
#include "stddef.h"

typedef int jasmine_bool_t;
typedef struct { int32_t index; } JasmineType;
typedef struct { int32_t index; } JasmineNode;
typedef struct { int32_t index; } JasmineBlock;
typedef struct { int32_t index; } JasmineEdge;
typedef struct { uint32_t bits; } JasmineOperand;
typedef struct { uint32_t bits; } JasmineValue;
typedef struct { void* handle; } JasmineFunction;
typedef struct { void* handle; } JasmineModule;

typedef int32_t JasmineOptimizationLevel;

#ifdef __cplusplus
#define JASMINE_EXPORT extern "C"
#else
#define JASMINE_EXPORT
#endif

// Holds the error message for the most recent error, or NULL if no error has
// yet occurred.
extern const char* JASMINE_ERROR_MESSAGE;

// Type indices for primitive types. Compound types (structs, arrays, and
// functions) have positive type indices, while primitives have negative ones.
const JasmineType
    JASMINE_TYPE_I8 = { .index = -1 },
    JASMINE_TYPE_I16 = { .index = -2 },
    JASMINE_TYPE_I32 = { .index = -3 },
    JASMINE_TYPE_I64 = { .index = -4 },
    JASMINE_TYPE_U8 = { .index = -5 },
    JASMINE_TYPE_U16 = { .index = -6 },
    JASMINE_TYPE_U32 = { .index = -7 },
    JASMINE_TYPE_U64 = { .index = -8 },
    JASMINE_TYPE_PTR = { .index = -9 },
    JASMINE_TYPE_REF = { .index = -10 },
    JASMINE_TYPE_F32 = { .index = -11 },
    JASMINE_TYPE_F64 = { .index = -12 },
    JASMINE_TYPE_VOID = { .index = -13 },
    JASMINE_TYPE_BOOL = { .index = -15 };

// These invalid node/function/module indices are used to indicate that an
// error occurred when constructing their respective types. Whenever a function
// returns an invalid value, it should also set JASMINE_ERROR_MESSAGE to a
// description of the error's cause. You can either check for these values
// yourself, if you want to choose how to respond to them; otherwise, passing
// any invalid value to a Jasmine API function will result in a crash, printing
// the latest error message to stderr.
const JasmineType
    JASMINE_INVALID_TYPE = { .index = -14 };
const JasmineNode
    JASMINE_INVALID_NODE = { .index = -1 };
const JasmineBlock
    JASMINE_INVALID_BLOCK = { .index = -1 };
const JasmineOperand
    JASMINE_INVALID_OPERAND = { .bits = 0xffffffffu };
const JasmineValue
    JASMINE_INVALID_VALUE = { .bits = 0xffffffffu };
const JasmineFunction
    JASMINE_INVALID_FUNCTION = { .handle = NULL };
const JasmineModule
    JASMINE_INVALID_MODULE = { .handle = NULL };

/*
 * Modules
 */

// Creates a new empty Jasmine module with the provided name.
JASMINE_EXPORT JasmineModule jasmine_create_module(const char* name, size_t name_length);

// Cleans up the memory of a Jasmine module.
JASMINE_EXPORT void jasmine_destroy_module(JasmineModule module);

/*
 * Types
 */

// Returns a handle to a struct type with the given types as its fields.
JASMINE_EXPORT JasmineType jasmine_make_struct_type(JasmineModule module, const JasmineType* types, size_t count);

// Returns a handle to a union type with the given types as its cases.
JASMINE_EXPORT JasmineType jasmine_make_union_type(JasmineModule module, const JasmineType* types, size_t count);

// Returns a handle to an array type with the given element type and the
// provided length.
JASMINE_EXPORT JasmineType jasmine_make_array_type(JasmineModule module, JasmineType element, size_t length);

// Returns a handle to a function type with the given parameter list and
// return type.
JASMINE_EXPORT JasmineType jasmine_make_function_type(JasmineModule module, const JasmineType* parameter_types, size_t parameter_count, JasmineType return_type);

/*
 * Functions
 */

// Defines a function with the provided name and return type. Parameters
// should be added afterwards, using jasmine_add_parameter. Returns a
// handle to the new function, or JASMINE_INVALID_FUNCTION if a function
// with the same name already exists in the module.
JASMINE_EXPORT JasmineFunction jasmine_create_function(JasmineModule module, const char* name, size_t name_length, JasmineType return_type);

/*
 * Blocks
 */

// Creates a new, empty, disconnected basic block in the provided Jasmine
// function.
JASMINE_EXPORT JasmineBlock jasmine_add_block(JasmineFunction function);

// Appends a parameter to the provided function with the specified type and name.
JASMINE_EXPORT JasmineOperand jasmine_add_parameter(JasmineFunction function, JasmineType type, const char* name, size_t name_length);

// Appends a node to the end of the provided block.
JASMINE_EXPORT void jasmine_add_node(JasmineFunction function, JasmineBlock block, JasmineNode node);

// Sets the entrypoint block of a function.
JASMINE_EXPORT void jasmine_set_entrypoint(JasmineFunction function, JasmineBlock block);

/*
 * Edges
 */

// Creates a control flow edge between two blocks.
JASMINE_EXPORT JasmineEdge jasmine_add_edge(JasmineFunction function, JasmineBlock src, JasmineBlock dest);

/*
 * IR Generation
 */

// Returns an encoded operand representing a constant integer.
JASMINE_EXPORT JasmineOperand jasmine_imm(JasmineFunction function, int64_t immediate);

// Returns an encoded operand representing a constant integer equal to the size of the specified type.
JASMINE_EXPORT JasmineOperand jasmine_sizeof(JasmineFunction function, JasmineType type);

// Returns an encoded operand representing a constant 32-bit float.
JASMINE_EXPORT JasmineOperand jasmine_f32(JasmineFunction function, float immediate);

// Returns an encoded operand representing a constant 64-bit float.
JASMINE_EXPORT JasmineOperand jasmine_f64(JasmineFunction function, double immediate);

// Returns an encoded operand referencing a known Jasmine function.
JASMINE_EXPORT JasmineOperand jasmine_known_function_ref(JasmineFunction function, JasmineFunction ref);

// Returns an encoded operand referencing an unknown function by symbol name.
JASMINE_EXPORT JasmineOperand jasmine_function_ref(JasmineFunction function, const char* name, size_t name_length);

// Returns an encoded operand referencing a global mutable variable by name.
JASMINE_EXPORT JasmineOperand jasmine_static_ref(JasmineFunction function, const char* name, size_t name_length);

// Returns an encoded operand referencing a global constant value by name.
JASMINE_EXPORT JasmineOperand jasmine_data_ref(JasmineFunction function, const char* name, size_t name_length);

// Defines a new virtual register within the provided function and returns it
// as an encoded operand.
JASMINE_EXPORT JasmineOperand jasmine_variable(JasmineFunction function);

// Accesses a virtual register by name within the provided function and returns
// it as an encoded operand. Calling this function repeatedly with the same name
// for the same function will return the same operand each time.
JASMINE_EXPORT JasmineOperand jasmine_variable_with_name(JasmineFunction function, const char* name, size_t name_length);

// Returns an encoded operand representing the function parameter variable
// at the provided index.
JASMINE_EXPORT JasmineOperand jasmine_parameter(JasmineFunction function, size_t index);

// Predicates to inspect the type of a JasmineOperand.
JASMINE_EXPORT jasmine_bool_t jasmine_is_imm(JasmineOperand operand);
JASMINE_EXPORT jasmine_bool_t jasmine_is_f32(JasmineOperand operand);
JASMINE_EXPORT jasmine_bool_t jasmine_is_f64(JasmineOperand operand);
JASMINE_EXPORT jasmine_bool_t jasmine_is_sizeof(JasmineOperand operand);
JASMINE_EXPORT jasmine_bool_t jasmine_is_function_ref(JasmineOperand operand);
JASMINE_EXPORT jasmine_bool_t jasmine_is_static_ref(JasmineOperand operand);
JASMINE_EXPORT jasmine_bool_t jasmine_is_data_ref(JasmineOperand operand);
JASMINE_EXPORT jasmine_bool_t jasmine_is_variable(JasmineOperand operand);
JASMINE_EXPORT jasmine_bool_t jasmine_is_valid_operand(JasmineOperand operand);

// Accessors to see the contents of a constant JasmineOperand.
JASMINE_EXPORT int64_t jasmine_value_of_imm(JasmineFunction function, JasmineOperand operand);
JASMINE_EXPORT float jasmine_value_of_f32(JasmineFunction function, JasmineOperand operand);
JASMINE_EXPORT double jasmine_value_of_f64(JasmineFunction function, JasmineOperand operand);

// All of the following construct various Jasmine nodes from operands. Nodes
// are associated with functions, not modules, so the node handles these
// functions return are meaningless outside the function they were created for.
// Each of these functions will return the handle to the created node, or
// JASMINE_INVALID_NODE if some error occurred.
JASMINE_EXPORT JasmineNode jasmine_create_nop(JasmineFunction function);
JASMINE_EXPORT JasmineNode jasmine_create_var(JasmineFunction function, JasmineType type, JasmineOperand dest);
JASMINE_EXPORT JasmineNode jasmine_create_mov(JasmineFunction function, JasmineType type, JasmineOperand dest, JasmineOperand src);
JASMINE_EXPORT JasmineNode jasmine_create_pack(JasmineFunction function, JasmineType type, JasmineOperand dest, JasmineOperand* fields, size_t field_count);
JASMINE_EXPORT JasmineNode jasmine_create_unpack(JasmineFunction function, JasmineType type, JasmineOperand* outputs, size_t output_count, JasmineOperand src);
JASMINE_EXPORT JasmineNode jasmine_create_new(JasmineFunction function, JasmineType type, JasmineOperand dest, JasmineOperand src);
JASMINE_EXPORT JasmineNode jasmine_create_new_struct(JasmineFunction function, JasmineType type, JasmineOperand dest, JasmineOperand* fields, size_t field_count);
JASMINE_EXPORT JasmineNode jasmine_create_new_array(JasmineFunction function, JasmineType type, JasmineOperand dest, JasmineOperand* elements, size_t element_count);
JASMINE_EXPORT JasmineNode jasmine_create_add(JasmineFunction function, JasmineType type, JasmineOperand dest, JasmineOperand left, JasmineOperand right);
JASMINE_EXPORT JasmineNode jasmine_create_sub(JasmineFunction function, JasmineType type, JasmineOperand dest, JasmineOperand left, JasmineOperand right);
JASMINE_EXPORT JasmineNode jasmine_create_mul(JasmineFunction function, JasmineType type, JasmineOperand dest, JasmineOperand left, JasmineOperand right);
JASMINE_EXPORT JasmineNode jasmine_create_div(JasmineFunction function, JasmineType type, JasmineOperand dest, JasmineOperand left, JasmineOperand right);
JASMINE_EXPORT JasmineNode jasmine_create_rem(JasmineFunction function, JasmineType type, JasmineOperand dest, JasmineOperand left, JasmineOperand right);
JASMINE_EXPORT JasmineNode jasmine_create_neg(JasmineFunction function, JasmineType type, JasmineOperand dest, JasmineOperand src);
JASMINE_EXPORT JasmineNode jasmine_create_abs(JasmineFunction function, JasmineType type, JasmineOperand dest, JasmineOperand src);
JASMINE_EXPORT JasmineNode jasmine_create_min(JasmineFunction function, JasmineType type, JasmineOperand dest, JasmineOperand left, JasmineOperand right);
JASMINE_EXPORT JasmineNode jasmine_create_max(JasmineFunction function, JasmineType type, JasmineOperand dest, JasmineOperand left, JasmineOperand right);
JASMINE_EXPORT JasmineNode jasmine_create_sqrt(JasmineFunction function, JasmineType type, JasmineOperand dest, JasmineOperand src);
JASMINE_EXPORT JasmineNode jasmine_create_round(JasmineFunction function, JasmineType type, JasmineOperand dest, JasmineOperand src);
JASMINE_EXPORT JasmineNode jasmine_create_floor(JasmineFunction function, JasmineType type, JasmineOperand dest, JasmineOperand src);
JASMINE_EXPORT JasmineNode jasmine_create_ceil(JasmineFunction function, JasmineType type, JasmineOperand dest, JasmineOperand src);
JASMINE_EXPORT JasmineNode jasmine_create_and(JasmineFunction function, JasmineType type, JasmineOperand dest, JasmineOperand left, JasmineOperand right);
JASMINE_EXPORT JasmineNode jasmine_create_or(JasmineFunction function, JasmineType type, JasmineOperand dest, JasmineOperand left, JasmineOperand right);
JASMINE_EXPORT JasmineNode jasmine_create_xor(JasmineFunction function, JasmineType type, JasmineOperand dest, JasmineOperand left, JasmineOperand right);
JASMINE_EXPORT JasmineNode jasmine_create_not(JasmineFunction function, JasmineType type, JasmineOperand dest, JasmineOperand src);
JASMINE_EXPORT JasmineNode jasmine_create_shl(JasmineFunction function, JasmineType type, JasmineOperand dest, JasmineOperand left, JasmineOperand right);
JASMINE_EXPORT JasmineNode jasmine_create_shr(JasmineFunction function, JasmineType type, JasmineOperand dest, JasmineOperand left, JasmineOperand right);
JASMINE_EXPORT JasmineNode jasmine_create_rol(JasmineFunction function, JasmineType type, JasmineOperand dest, JasmineOperand left, JasmineOperand right);
JASMINE_EXPORT JasmineNode jasmine_create_ror(JasmineFunction function, JasmineType type, JasmineOperand dest, JasmineOperand left, JasmineOperand right);
JASMINE_EXPORT JasmineNode jasmine_create_lzc(JasmineFunction function, JasmineType type, JasmineOperand dest, JasmineOperand src);
JASMINE_EXPORT JasmineNode jasmine_create_tzc(JasmineFunction function, JasmineType type, JasmineOperand dest, JasmineOperand src);
JASMINE_EXPORT JasmineNode jasmine_create_popc(JasmineFunction function, JasmineType type, JasmineOperand dest, JasmineOperand src);
JASMINE_EXPORT JasmineNode jasmine_create_is_lt(JasmineFunction function, JasmineType type, JasmineOperand dest, JasmineOperand left, JasmineOperand right);
JASMINE_EXPORT JasmineNode jasmine_create_is_le(JasmineFunction function, JasmineType type, JasmineOperand dest, JasmineOperand left, JasmineOperand right);
JASMINE_EXPORT JasmineNode jasmine_create_is_gt(JasmineFunction function, JasmineType type, JasmineOperand dest, JasmineOperand left, JasmineOperand right);
JASMINE_EXPORT JasmineNode jasmine_create_is_ge(JasmineFunction function, JasmineType type, JasmineOperand dest, JasmineOperand left, JasmineOperand right);
JASMINE_EXPORT JasmineNode jasmine_create_is_eq(JasmineFunction function, JasmineType type, JasmineOperand dest, JasmineOperand left, JasmineOperand right);
JASMINE_EXPORT JasmineNode jasmine_create_is_ne(JasmineFunction function, JasmineType type, JasmineOperand dest, JasmineOperand left, JasmineOperand right);
JASMINE_EXPORT JasmineNode jasmine_create_get_field(JasmineFunction function, JasmineType type, JasmineOperand dest, JasmineOperand src, uint32_t field_index);
JASMINE_EXPORT JasmineNode jasmine_create_get_index(JasmineFunction function, JasmineType type, JasmineOperand dest, JasmineOperand src, JasmineType index_type, JasmineOperand index);
JASMINE_EXPORT JasmineNode jasmine_create_set_field(JasmineFunction function, JasmineType type, JasmineOperand dest, uint32_t field_index, JasmineOperand src);
JASMINE_EXPORT JasmineNode jasmine_create_set_index(JasmineFunction function, JasmineType type, JasmineOperand dest, JasmineType index_type, JasmineOperand index, JasmineOperand src);
JASMINE_EXPORT JasmineNode jasmine_create_addr(JasmineFunction function, JasmineType type, JasmineOperand dest, JasmineOperand base);
JASMINE_EXPORT JasmineNode jasmine_create_addr_field(JasmineFunction function, JasmineType type, JasmineOperand dest, JasmineOperand base, uint32_t field_index);
JASMINE_EXPORT JasmineNode jasmine_create_addr_index(JasmineFunction function, JasmineType type, JasmineOperand dest, JasmineOperand base, JasmineType index_type, JasmineOperand index);
JASMINE_EXPORT JasmineNode jasmine_create_load(JasmineFunction function, JasmineType type, JasmineOperand dest, JasmineOperand ptr);
JASMINE_EXPORT JasmineNode jasmine_create_load_field(JasmineFunction function, JasmineType type, JasmineOperand dest, JasmineOperand ptr, uint32_t field_index);
JASMINE_EXPORT JasmineNode jasmine_create_load_index(JasmineFunction function, JasmineType type, JasmineOperand dest, JasmineOperand ptr, JasmineType index_type, JasmineOperand index);
JASMINE_EXPORT JasmineNode jasmine_create_store(JasmineFunction function, JasmineType type, JasmineOperand ptr, JasmineOperand src);
JASMINE_EXPORT JasmineNode jasmine_create_store_field(JasmineFunction function, JasmineType type, JasmineOperand ptr, uint32_t field_index, JasmineOperand src);
JASMINE_EXPORT JasmineNode jasmine_create_store_index(JasmineFunction function, JasmineType type, JasmineOperand ptr, JasmineType index_type, JasmineOperand index, JasmineOperand src);
JASMINE_EXPORT JasmineNode jasmine_create_offset_field(JasmineFunction function, JasmineType type, JasmineOperand dest, JasmineOperand ptr, uint32_t field_index);
JASMINE_EXPORT JasmineNode jasmine_create_offset_index(JasmineFunction function, JasmineType type, JasmineOperand dest, JasmineOperand ptr, JasmineType index_type, JasmineOperand index);
JASMINE_EXPORT JasmineNode jasmine_create_br(JasmineFunction function, JasmineEdge dest);
JASMINE_EXPORT JasmineNode jasmine_create_br_if(JasmineFunction function, JasmineType type, JasmineOperand cond, JasmineEdge ifTrue, JasmineEdge ifFalse);
JASMINE_EXPORT JasmineNode jasmine_create_br_if_not(JasmineFunction function, JasmineType type, JasmineOperand cond, JasmineEdge ifTrue, JasmineEdge ifFalse);
JASMINE_EXPORT JasmineNode jasmine_create_br_lt(JasmineFunction function, JasmineType type, JasmineOperand left, JasmineOperand right, JasmineEdge ifTrue, JasmineEdge ifFalse);
JASMINE_EXPORT JasmineNode jasmine_create_br_le(JasmineFunction function, JasmineType type, JasmineOperand left, JasmineOperand right, JasmineEdge ifTrue, JasmineEdge ifFalse);
JASMINE_EXPORT JasmineNode jasmine_create_br_gt(JasmineFunction function, JasmineType type, JasmineOperand left, JasmineOperand right, JasmineEdge ifTrue, JasmineEdge ifFalse);
JASMINE_EXPORT JasmineNode jasmine_create_br_ge(JasmineFunction function, JasmineType type, JasmineOperand left, JasmineOperand right, JasmineEdge ifTrue, JasmineEdge ifFalse);
JASMINE_EXPORT JasmineNode jasmine_create_br_eq(JasmineFunction function, JasmineType type, JasmineOperand left, JasmineOperand right, JasmineEdge ifTrue, JasmineEdge ifFalse);
JASMINE_EXPORT JasmineNode jasmine_create_br_ne(JasmineFunction function, JasmineType type, JasmineOperand left, JasmineOperand right, JasmineEdge ifTrue, JasmineEdge ifFalse);
JASMINE_EXPORT JasmineNode jasmine_create_call(JasmineFunction function, JasmineType type, JasmineOperand dest, JasmineOperand callee, JasmineOperand* arguments, size_t argument_count);
JASMINE_EXPORT JasmineNode jasmine_create_call_void(JasmineFunction function, JasmineType type, JasmineOperand callee, JasmineOperand* arguments, size_t argument_count);
JASMINE_EXPORT JasmineNode jasmine_create_ret(JasmineFunction function, JasmineType type, JasmineOperand return_value);
JASMINE_EXPORT JasmineNode jasmine_create_ret_void(JasmineFunction function);
JASMINE_EXPORT JasmineNode jasmine_create_convert(JasmineFunction function, JasmineType destType, JasmineOperand dest, JasmineType srcType, JasmineOperand src);
JASMINE_EXPORT JasmineNode jasmine_create_bitcast(JasmineFunction function, JasmineType destType, JasmineOperand dest, JasmineType srcType, JasmineOperand src);
JASMINE_EXPORT JasmineNode jasmine_create_trap(JasmineFunction function);
JASMINE_EXPORT JasmineNode jasmine_create_alloca(JasmineFunction function, JasmineType type, JasmineOperand dest, JasmineOperand size);

/*
 * Global data
 */

// Creates a new integer constant value.
JASMINE_EXPORT JasmineValue jasmine_integer_value(JasmineModule module, JasmineType type, int64_t constant);

// Creates a 32-bit floating-point value.
JASMINE_EXPORT JasmineValue jasmine_f32_value(JasmineModule module, float constant);

// Creates a 64-bit floating-point value.
JASMINE_EXPORT JasmineValue jasmine_f64_value(JasmineModule module, double constant);

// Gets the integer value of a Jasmine value.
JASMINE_EXPORT int64_t jasmine_get_integer_value(JasmineModule module, JasmineValue value);

// Gets the 32-bit floating point value of a Jasmine value.
JASMINE_EXPORT float jasmine_get_f32_value(JasmineModule module, JasmineValue value);

// Gets the 64-bit floating point value of a Jasmine value.
JASMINE_EXPORT double jasmine_get_f64_value(JasmineModule module, JasmineValue value);

// Creates a pointer value that references a function.
JASMINE_EXPORT JasmineValue jasmine_function_ref_value(JasmineModule module, const char* name, size_t name_length);

// Creates a pointer value that references a label in the data section.
JASMINE_EXPORT JasmineValue jasmine_data_ref_value(JasmineModule module, const char* name, size_t name_length);

// Creates a pointer value that references a label in the static section.
JASMINE_EXPORT JasmineValue jasmine_static_ref_value(JasmineModule module, const char* name, size_t name_length);

// Creates a new struct value from the provided field values.
JASMINE_EXPORT JasmineValue jasmine_struct_value(JasmineModule module, JasmineType type, const JasmineValue* fields, size_t field_count);

// Creates a new array value from the provided element values.
JASMINE_EXPORT JasmineValue jasmine_i8_array_value(JasmineModule module, const int8_t* elements, size_t element_count);
JASMINE_EXPORT JasmineValue jasmine_u8_array_value(JasmineModule module, const uint8_t* elements, size_t element_count);
JASMINE_EXPORT JasmineValue jasmine_i16_array_value(JasmineModule module, const int16_t* elements, size_t element_count);
JASMINE_EXPORT JasmineValue jasmine_u16_array_value(JasmineModule module, const uint16_t* elements, size_t element_count);
JASMINE_EXPORT JasmineValue jasmine_i32_array_value(JasmineModule module, const int32_t* elements, size_t element_count);
JASMINE_EXPORT JasmineValue jasmine_u32_array_value(JasmineModule module, const uint32_t* elements, size_t element_count);
JASMINE_EXPORT JasmineValue jasmine_i64_array_value(JasmineModule module, const int64_t* elements, size_t element_count);
JASMINE_EXPORT JasmineValue jasmine_u64_array_value(JasmineModule module, const uint64_t* elements, size_t element_count);
JASMINE_EXPORT JasmineValue jasmine_f32_array_value(JasmineModule module, const float* elements, size_t element_count);
JASMINE_EXPORT JasmineValue jasmine_f64_array_value(JasmineModule module, const double* elements, size_t element_count);
JASMINE_EXPORT JasmineValue jasmine_value_array_value(JasmineModule module, JasmineType elementType, const JasmineValue* elements, size_t element_count);

// Defines a new data memory slot with the specified name initialized with the
// provided data value.
JASMINE_EXPORT void jasmine_define_data(JasmineModule module, JasmineType type, const char* name, size_t name_length, JasmineValue value);

// Defines a new static memory slot with the specified name initialized with
// the provided data value.
JASMINE_EXPORT void jasmine_define_static(JasmineModule module, JasmineType type, const char* name, size_t name_length, JasmineValue value);

// Defines a new uninitialized static memory slot with the specified name.
JASMINE_EXPORT void jasmine_define_static_uninit(JasmineModule module, JasmineType type, const char* name, size_t name_length);

/*
 * IO
 */

struct FILE;

typedef enum { JasmineStdout, JasmineStderr } JasmineOutputStream;

// These format various Jasmine types to an in-memory buffer, returning the number of bytes written.
JASMINE_EXPORT size_t jasmine_format_module(int8_t* buffer, size_t buffer_size, JasmineModule module);
JASMINE_EXPORT size_t jasmine_format_type(int8_t* buffer, size_t buffer_size, JasmineModule module, JasmineType type);
JASMINE_EXPORT size_t jasmine_format_function(int8_t* buffer, size_t buffer_size, JasmineFunction function);
JASMINE_EXPORT size_t jasmine_format_block(int8_t* buffer, size_t buffer_size, JasmineFunction function, JasmineBlock block);
JASMINE_EXPORT size_t jasmine_format_edge(int8_t* buffer, size_t buffer_size, JasmineFunction function, JasmineEdge edge);
JASMINE_EXPORT size_t jasmine_format_node(int8_t* buffer, size_t buffer_size, JasmineFunction function, JasmineNode node);
JASMINE_EXPORT size_t jasmine_format_operand(int8_t* buffer, size_t buffer_size, JasmineFunction function, JasmineOperand operand);

// These print various Jasmine types to either stdout or stderr.
JASMINE_EXPORT void jasmine_print_module(JasmineOutputStream stream, JasmineModule module);
JASMINE_EXPORT void jasmine_print_type(JasmineOutputStream stream, JasmineModule module, JasmineType type);
JASMINE_EXPORT void jasmine_print_function(JasmineOutputStream stream, JasmineFunction function);
JASMINE_EXPORT void jasmine_print_block(JasmineOutputStream stream, JasmineFunction function, JasmineBlock block);
JASMINE_EXPORT void jasmine_print_edge(JasmineOutputStream stream, JasmineFunction function, JasmineEdge edge);
JASMINE_EXPORT void jasmine_print_node(JasmineOutputStream stream, JasmineFunction function, JasmineNode node);
JASMINE_EXPORT void jasmine_print_operand(JasmineOutputStream stream, JasmineFunction function, JasmineOperand operand);

// Serializes a full Jasmine module to the provided binary file.
JASMINE_EXPORT void jasmine_encode_module(FILE* file, JasmineModule module);

// Loads a full Jasmine module from the provided binary file.
JASMINE_EXPORT JasmineModule jasmine_decode_module(FILE* file);

/*
 * Passes
 */

typedef struct { void* handle; } JasminePassContext;

// Returns a handle to a newly-created pass context that stores metadata for
// all the subsequent optimization passes to use.
JASMINE_EXPORT JasminePassContext jasmine_create_pass_context(JasmineFunction function);

// Releases resources held by the pass context.
JASMINE_EXPORT void jasmine_destroy_pass_context(JasminePassContext context);

// Ensures every Jasmine instruction and variable is typed by propagating type
// information through the function.
JASMINE_EXPORT void jasmine_perform_type_propagation(JasminePassContext context, JasmineFunction function);

// Propagates types through the function and checks that each instruction is
// well-typed, crashing and printing a list of errors on failure.
JASMINE_EXPORT void jasmine_perform_type_propagation(JasminePassContext context, JasmineFunction function);

// Validates Jasmine instructions after stack or register allocation to ensure
// that their operands have valid kinds.
JASMINE_EXPORT void jasmine_validate_after_lowering(JasminePassContext context, JasmineFunction function);

// Transforms the provided function into SSA form.
JASMINE_EXPORT void jasmine_ensure_ssa(JasminePassContext context, JasmineFunction function);

// Tries to optimize the provided function by constant folding. Requires SSA
// form.
JASMINE_EXPORT void jasmine_perform_constant_folding(JasminePassContext context, JasmineFunction function);

// Tries to remove dead code from the provided function. Requires SSA form.
JASMINE_EXPORT void jasmine_perform_dead_code_elimination(JasminePassContext context, JasmineFunction function);

// Attempts to inline calls to known Jasmine functions. Requires SSA form.
JASMINE_EXPORT void jasmine_perform_inlining(JasminePassContext context, JasmineFunction function);

// Attempts to detect and unroll loops in the provided function. Requires SSA
// form.
JASMINE_EXPORT void jasmine_perform_loop_unrolling(JasminePassContext context, JasmineFunction function);

// Transforms various instruction and control flow patterns into cheaper
// alternatives. Requires SSA form.
JASMINE_EXPORT void jasmine_perform_strength_reduction(JasminePassContext context, JasmineFunction function);

// Attempts to split small struct and array variables into multiple scalar
// variables. Requires SSA form.
JASMINE_EXPORT void jasmine_perform_scalar_replacement(JasminePassContext context, JasmineFunction function);

// Finalizes basic block order before generating assembly.
JASMINE_EXPORT void jasmine_finalize_cfg(JasminePassContext context, JasmineFunction function);

// Assigns stack slots to each variable within the function and allocates
// scratch registers. This is the worst quality stack/register allocation
// method, keeping all variables in unique memory locations, and making all
// operations load/store their parameters.
JASMINE_EXPORT void jasmine_perform_stack_allocation(JasminePassContext context, JasmineFunction function);

typedef enum {
    // Selects the single-pass register allocator. Doesn't require liveness
    // analysis or an interference graph, and has generally the lowest quality
    // in exchange for the fastest compilation time.
    JasmineSinglePassRegisterAllocator,

    // Selects the linear register allocator. Relies on liveness analysis and
    // interference, and should generally yield good results, but is still
    // linear complexity and doesn't aggressively optimize assignments.
    JasmineLinearRegisterAllocator,

    // Selects the advanced register allocator. Largely similar to the linear
    // allocator, but performs more passes and optimizes more aggressively,
    // in exchange for potentially longer compilation times.
    JasmineAdvancedRegisterAllocator
} JasmineRegisterAllocationMode;

// Assigns machine registers to each variable within the function and replaces
// variable uses with register uses in the function's nodes. Requires SSA form.
JASMINE_EXPORT void jasmine_perform_register_allocation(JasminePassContext context, JasmineFunction function, JasmineRegisterAllocationMode mode);

typedef struct { void* handle; } JasmineAssembly;

// Allocates a new empty assembly object tied to a Jasmine module.
JASMINE_EXPORT JasmineAssembly jasmine_create_assembly(JasmineModule mod);

// Compiles a whole module with a default set of optimizations based on the
// optimization level.
//  -O0: No optimizations, stack allocation.
//  -O1: Basic optimizations (dead code elimination, constant folding, control
//       flow simplification) and low-quality register allocation.
//  -O2: All mid-level optimizations and high-level register allocation.
//  -O3: Aggressive optimizations, higher inlining and unrolling factors.
JASMINE_EXPORT void jasmine_compile(JasmineAssembly as, JasmineModule mod, JasmineOptimizationLevel optLevel, jasmine_bool_t validate);

// Compiles just a single function into a Jasmine assembly. This mode doesn't
// support interprocedural optimizations, and destroys the function after it's
// complete.
JASMINE_EXPORT void jasmine_compile_function_only(JasmineAssembly as, JasmineFunction func, JasmineOptimizationLevel optLevel, jasmine_bool_t validate);

// Compiles just the module-global components of a module into a Jasmine
// assembly. Used with compiling individual functions to ensure required module
// properties are included in the generated binary.
JASMINE_EXPORT void jasmine_compile_module_only(JasmineAssembly as, JasmineModule mod, JasmineOptimizationLevel optLevel, jasmine_bool_t validate);

/*
 * Lowering
 */

// Compiles the provided function into relocatable machine code.
JASMINE_EXPORT JasmineAssembly jasmine_generate_assembly(JasminePassContext context, JasmineFunction function);

// Cleans up the memory of a Jasmine assembly.
JASMINE_EXPORT void jasmine_destroy_assembly(JasmineAssembly assembly);

// Writes a Jasmine assembly to the provided file path as a relocatable object in the ELF format.
JASMINE_EXPORT void jasmine_write_relocatable_elf_object(JasmineAssembly assembly, const char* path, size_t path_length);

/*
 * Linking and loading
 */

typedef struct { void* handle; } JasmineExecutable;

// Concatenates multiple relocatable assemblies into a single relocatable
// assembly containing all the definitions of the others.
JASMINE_EXPORT JasmineAssembly jasmine_join_assemblies(JasmineAssembly* assemblies, size_t assembly_count);

// Concatenates multiple relocatable assemblies in-place, with one of the
// provided assemblies used as the result and the rest destroyed.
JASMINE_EXPORT JasmineAssembly jasmine_join_assemblies_in_place(JasmineAssembly* assemblies, size_t assembly_count);

// Links together one or more relocatable assemblies into a linked executable
// binary.
JASMINE_EXPORT JasmineExecutable jasmine_link_assemblies(JasmineAssembly* assemblies, size_t assembly_count);

// Loads a linked executable into executable memory.
JASMINE_EXPORT void jasmine_load_executable(JasmineExecutable executable);

// Returns the address of the provided symbol in a loaded executable. Can be
// cast to a function pointer if the symbol represents a compiled function, and
// called.
JASMINE_EXPORT void* jasmine_lookup_symbol(JasmineExecutable executable, const char* symbol, size_t name_length);

// Cleans up the memory of a Jasmine executable.
JASMINE_EXPORT void jasmine_destroy_executable(JasmineExecutable executable);

/*
 * Builder utility
 */

typedef struct { void* handle; } JasmineBuilder;

// Constructs a new Jasmine builder.
JASMINE_EXPORT JasmineBuilder jasmine_create_builder(JasmineModule module);

// Deallocates a Jasmine builder.
JASMINE_EXPORT void jasmine_destroy_builder(JasmineBuilder builder);

// Tells the builder to write subsequent instructions to the specified function.
JASMINE_EXPORT void jasmine_builder_set_function(JasmineBuilder builder, JasmineFunction function);

// Tells the builder to write subsequent instructions to the specified block.
JASMINE_EXPORT void jasmine_builder_set_block(JasmineBuilder builder, JasmineBlock block);

// Gets the current target function a builder is writing to.
JASMINE_EXPORT JasmineFunction jasmine_current_function(JasmineBuilder builder);

// Gets the current target block a builder is writing to.
JASMINE_EXPORT JasmineBlock jasmine_current_block(JasmineBuilder builder);

// Helpers to add different kinds of nodes to the builder.
JASMINE_EXPORT void jasmine_append_nop(JasmineBuilder builder);
JASMINE_EXPORT void jasmine_append_var(JasmineBuilder builder, JasmineType type, JasmineOperand dest);
JASMINE_EXPORT void jasmine_append_mov(JasmineBuilder builder, JasmineType type, JasmineOperand dest, JasmineOperand src);
JASMINE_EXPORT void jasmine_append_pack(JasmineBuilder builder, JasmineType type, JasmineOperand dest, JasmineOperand* fields, size_t field_count);
JASMINE_EXPORT void jasmine_append_unpack(JasmineBuilder builder, JasmineType type, JasmineOperand* outputs, size_t output_count, JasmineOperand src);
JASMINE_EXPORT void jasmine_append_new(JasmineBuilder builder, JasmineType type, JasmineOperand dest, JasmineOperand src);
JASMINE_EXPORT void jasmine_append_new_struct(JasmineBuilder builder, JasmineType type, JasmineOperand dest, JasmineOperand* fields, size_t field_count);
JASMINE_EXPORT void jasmine_append_new_array(JasmineBuilder builder, JasmineType type, JasmineOperand dest, JasmineOperand* elements, size_t element_count);
JASMINE_EXPORT void jasmine_append_add(JasmineBuilder builder, JasmineType type, JasmineOperand dest, JasmineOperand left, JasmineOperand right);
JASMINE_EXPORT void jasmine_append_sub(JasmineBuilder builder, JasmineType type, JasmineOperand dest, JasmineOperand left, JasmineOperand right);
JASMINE_EXPORT void jasmine_append_mul(JasmineBuilder builder, JasmineType type, JasmineOperand dest, JasmineOperand left, JasmineOperand right);
JASMINE_EXPORT void jasmine_append_div(JasmineBuilder builder, JasmineType type, JasmineOperand dest, JasmineOperand left, JasmineOperand right);
JASMINE_EXPORT void jasmine_append_rem(JasmineBuilder builder, JasmineType type, JasmineOperand dest, JasmineOperand left, JasmineOperand right);
JASMINE_EXPORT void jasmine_append_neg(JasmineBuilder builder, JasmineType type, JasmineOperand dest, JasmineOperand src);
JASMINE_EXPORT void jasmine_append_abs(JasmineBuilder builder, JasmineType type, JasmineOperand dest, JasmineOperand src);
JASMINE_EXPORT void jasmine_append_min(JasmineBuilder builder, JasmineType type, JasmineOperand dest, JasmineOperand left, JasmineOperand right);
JASMINE_EXPORT void jasmine_append_max(JasmineBuilder builder, JasmineType type, JasmineOperand dest, JasmineOperand left, JasmineOperand right);
JASMINE_EXPORT void jasmine_append_sqrt(JasmineBuilder builder, JasmineType type, JasmineOperand dest, JasmineOperand src);
JASMINE_EXPORT void jasmine_append_round(JasmineBuilder builder, JasmineType type, JasmineOperand dest, JasmineOperand src);
JASMINE_EXPORT void jasmine_append_floor(JasmineBuilder builder, JasmineType type, JasmineOperand dest, JasmineOperand src);
JASMINE_EXPORT void jasmine_append_ceil(JasmineBuilder builder, JasmineType type, JasmineOperand dest, JasmineOperand src);
JASMINE_EXPORT void jasmine_append_and(JasmineBuilder builder, JasmineType type, JasmineOperand dest, JasmineOperand left, JasmineOperand right);
JASMINE_EXPORT void jasmine_append_or(JasmineBuilder builder, JasmineType type, JasmineOperand dest, JasmineOperand left, JasmineOperand right);
JASMINE_EXPORT void jasmine_append_xor(JasmineBuilder builder, JasmineType type, JasmineOperand dest, JasmineOperand left, JasmineOperand right);
JASMINE_EXPORT void jasmine_append_not(JasmineBuilder builder, JasmineType type, JasmineOperand dest, JasmineOperand src);
JASMINE_EXPORT void jasmine_append_shl(JasmineBuilder builder, JasmineType type, JasmineOperand dest, JasmineOperand left, JasmineOperand right);
JASMINE_EXPORT void jasmine_append_shr(JasmineBuilder builder, JasmineType type, JasmineOperand dest, JasmineOperand left, JasmineOperand right);
JASMINE_EXPORT void jasmine_append_rol(JasmineBuilder builder, JasmineType type, JasmineOperand dest, JasmineOperand left, JasmineOperand right);
JASMINE_EXPORT void jasmine_append_ror(JasmineBuilder builder, JasmineType type, JasmineOperand dest, JasmineOperand left, JasmineOperand right);
JASMINE_EXPORT void jasmine_append_lzc(JasmineBuilder builder, JasmineType type, JasmineOperand dest, JasmineOperand src);
JASMINE_EXPORT void jasmine_append_tzc(JasmineBuilder builder, JasmineType type, JasmineOperand dest, JasmineOperand src);
JASMINE_EXPORT void jasmine_append_popc(JasmineBuilder builder, JasmineType type, JasmineOperand dest, JasmineOperand src);
JASMINE_EXPORT void jasmine_append_is_lt(JasmineBuilder builder, JasmineType type, JasmineOperand dest, JasmineOperand left, JasmineOperand right);
JASMINE_EXPORT void jasmine_append_is_le(JasmineBuilder builder, JasmineType type, JasmineOperand dest, JasmineOperand left, JasmineOperand right);
JASMINE_EXPORT void jasmine_append_is_gt(JasmineBuilder builder, JasmineType type, JasmineOperand dest, JasmineOperand left, JasmineOperand right);
JASMINE_EXPORT void jasmine_append_is_ge(JasmineBuilder builder, JasmineType type, JasmineOperand dest, JasmineOperand left, JasmineOperand right);
JASMINE_EXPORT void jasmine_append_is_eq(JasmineBuilder builder, JasmineType type, JasmineOperand dest, JasmineOperand left, JasmineOperand right);
JASMINE_EXPORT void jasmine_append_is_ne(JasmineBuilder builder, JasmineType type, JasmineOperand dest, JasmineOperand left, JasmineOperand right);
JASMINE_EXPORT void jasmine_append_get_field(JasmineBuilder builder, JasmineType type, JasmineOperand dest, JasmineOperand src, uint32_t field_index);
JASMINE_EXPORT void jasmine_append_get_index(JasmineBuilder builder, JasmineType type, JasmineOperand dest, JasmineOperand src, JasmineType index_type, JasmineOperand index);
JASMINE_EXPORT void jasmine_append_set_field(JasmineBuilder builder, JasmineType type, JasmineOperand dest, uint32_t field_index, JasmineOperand src);
JASMINE_EXPORT void jasmine_append_set_index(JasmineBuilder builder, JasmineType type, JasmineOperand dest, JasmineType index_type, JasmineOperand index, JasmineOperand src);
JASMINE_EXPORT void jasmine_append_addr(JasmineBuilder builder, JasmineType type, JasmineOperand dest, JasmineOperand base);
JASMINE_EXPORT void jasmine_append_addr_field(JasmineBuilder builder, JasmineType type, JasmineOperand dest, JasmineOperand base, uint32_t field_index);
JASMINE_EXPORT void jasmine_append_addr_index(JasmineBuilder builder, JasmineType type, JasmineOperand dest, JasmineOperand base, JasmineType index_type, JasmineOperand index);
JASMINE_EXPORT void jasmine_append_load(JasmineBuilder builder, JasmineType type, JasmineOperand dest, JasmineOperand ptr);
JASMINE_EXPORT void jasmine_append_load_field(JasmineBuilder builder, JasmineType type, JasmineOperand dest, JasmineOperand ptr, uint32_t field_index);
JASMINE_EXPORT void jasmine_append_load_index(JasmineBuilder builder, JasmineType type, JasmineOperand dest, JasmineOperand ptr, JasmineType index_type, JasmineOperand index);
JASMINE_EXPORT void jasmine_append_store(JasmineBuilder builder, JasmineType type, JasmineOperand ptr, JasmineOperand src);
JASMINE_EXPORT void jasmine_append_store_field(JasmineBuilder builder, JasmineType type, JasmineOperand ptr, uint32_t field_index, JasmineOperand src);
JASMINE_EXPORT void jasmine_append_store_index(JasmineBuilder builder, JasmineType type, JasmineOperand ptr, JasmineType index_type, JasmineOperand index, JasmineOperand src);
JASMINE_EXPORT void jasmine_append_offset_field(JasmineBuilder builder, JasmineType type, JasmineOperand dest, JasmineOperand ptr, uint32_t field_index);
JASMINE_EXPORT void jasmine_append_offset_index(JasmineBuilder builder, JasmineType type, JasmineOperand dest, JasmineOperand ptr, JasmineType index_type, JasmineOperand index);
JASMINE_EXPORT void jasmine_append_br(JasmineBuilder builder, JasmineEdge dest);
JASMINE_EXPORT void jasmine_append_br_if(JasmineBuilder builder, JasmineType type, JasmineOperand src, JasmineEdge ifTrue, JasmineEdge ifFalse);
JASMINE_EXPORT void jasmine_append_br_if_not(JasmineBuilder builder, JasmineType type, JasmineOperand src, JasmineEdge ifTrue, JasmineEdge ifFalse);
JASMINE_EXPORT void jasmine_append_br_lt(JasmineBuilder builder, JasmineType type, JasmineOperand left, JasmineOperand right, JasmineEdge ifTrue, JasmineEdge ifFalse);
JASMINE_EXPORT void jasmine_append_br_le(JasmineBuilder builder, JasmineType type, JasmineOperand left, JasmineOperand right, JasmineEdge ifTrue, JasmineEdge ifFalse);
JASMINE_EXPORT void jasmine_append_br_gt(JasmineBuilder builder, JasmineType type, JasmineOperand left, JasmineOperand right, JasmineEdge ifTrue, JasmineEdge ifFalse);
JASMINE_EXPORT void jasmine_append_br_ge(JasmineBuilder builder, JasmineType type, JasmineOperand left, JasmineOperand right, JasmineEdge ifTrue, JasmineEdge ifFalse);
JASMINE_EXPORT void jasmine_append_br_eq(JasmineBuilder builder, JasmineType type, JasmineOperand left, JasmineOperand right, JasmineEdge ifTrue, JasmineEdge ifFalse);
JASMINE_EXPORT void jasmine_append_br_ne(JasmineBuilder builder, JasmineType type, JasmineOperand left, JasmineOperand right, JasmineEdge ifTrue, JasmineEdge ifFalse);
JASMINE_EXPORT void jasmine_append_call(JasmineBuilder builder, JasmineType type, JasmineOperand dest, JasmineOperand callee, JasmineOperand* arguments, size_t argument_count);
JASMINE_EXPORT void jasmine_append_call_void(JasmineBuilder builder, JasmineType type, JasmineOperand callee, JasmineOperand* arguments, size_t argument_count);
JASMINE_EXPORT void jasmine_append_ret(JasmineBuilder builder, JasmineType type, JasmineOperand return_value);
JASMINE_EXPORT void jasmine_append_ret_void(JasmineBuilder builder);
JASMINE_EXPORT void jasmine_append_convert(JasmineBuilder builder, JasmineType destType, JasmineOperand dest, JasmineType srcType, JasmineOperand src);
JASMINE_EXPORT void jasmine_append_bitcast(JasmineBuilder builder, JasmineType destType, JasmineOperand dest, JasmineType srcType, JasmineOperand src);
JASMINE_EXPORT void jasmine_append_trap(JasmineBuilder builder);
JASMINE_EXPORT void jasmine_append_alloca(JasmineBuilder builder, JasmineType type, JasmineOperand dest, JasmineOperand size);

#endif