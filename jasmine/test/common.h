#ifndef JASMINE_TEST_HELPERS_H
#define JASMINE_TEST_HELPERS_H

#include "jasmine/mod.h"
#include "util/test/harness.h"

using namespace jasmine;

inline LinkedAssembly compile(Assembly& as, Function& fn) {
    PassContext& ctx = fn.mod->passContext();
    ctx.targetSpecificPasses->generateAssembly(ctx, fn, as);
    auto linked = as.link(Assembly::defaultRelocator);
    linked.load();
    return linked;
}

struct FunctionAddressifier {
    Function* ptr;
    inline FunctionAddressifier(Function& fn):
        ptr(&fn) {}
};

#define COMPILE(artifact) \
    Assembly as(mod.syms); \
    compileWithOptimizations(&mod, as, 2, true); \
    auto artifact = as.link(Assembly::defaultRelocator); \
    artifact .load();

#define DEFINE_SIMPLE_TEST(name, result, type_lower, type_upper, ...) \
TEST(name ## _ ## type_lower) { \
    using ctype = type_lower; \
    TypeIndex type = type_upper; \
    __VA_ARGS__ \
}

#define DEFINE_TEST(name, result, type_lower, type_upper, ...) \
TEST(name ## _ ## type_lower) { \
    Module mod("test"); \
    Function& fn = mod.defineFunction(type_upper, #name); \
    TypeIndex type = type_upper; \
    __VA_ARGS__ \
    COMPILE(linked); \
    auto name = linked.lookup<type_lower()>(#name); \
    ASSERT(name() == result); \
}

#define DEFINE_TEST_OP(name, result, type_lower, type_upper, opcode_lower, opcode_upper, ...) \
TEST(name ## _ ## opcode_lower ## _ ## type_lower) { \
    Module mod("test"); \
    Function& fn = mod.defineFunction(type_upper, #name); \
    Opcode opcode = Opcode::opcode_upper; \
    TypeIndex type = type_upper; \
    __VA_ARGS__ \
    COMPILE(linked); \
    auto name = linked.lookup<type_lower()>(#name); \
    ASSERT(name() == result); \
}

#define DEFINE_TEST_BOOL_OP(name, result, type_lower, type_upper, opcode_lower, opcode_upper, ...) \
TEST(name ## _ ## opcode_lower ## _ ## type_lower) { \
    Module mod("test"); \
    Function& fn = mod.defineFunction(type_upper, #name); \
    Opcode opcode = Opcode::opcode_upper; \
    TypeIndex type = type_upper; \
    __VA_ARGS__ \
    COMPILE(linked); \
    auto name = linked.lookup<bool()>(#name); \
    ASSERT(name() == result); \
}

#define FOR_EACH_NON_PTR_UINT(macro, name, result, ...) \
macro(name, result, u8, TypeKind::U8, __VA_ARGS__) \
macro(name, result, u16, TypeKind::U16, __VA_ARGS__) \
macro(name, result, u32, TypeKind::U32, __VA_ARGS__) \
macro(name, result, u64, TypeKind::U64, __VA_ARGS__)

#define FOR_EACH_UINT(macro, name, result, ...) \
FOR_EACH_NON_PTR_UINT(macro, name, result, __VA_ARGS__) \
macro(name, result, uptr, TypeKind::PTR, __VA_ARGS__)

#define FOR_EACH_SINT(macro, name, result, ...) \
macro(name, result, i8, TypeKind::I8, __VA_ARGS__) \
macro(name, result, i16, TypeKind::I16, __VA_ARGS__) \
macro(name, result, i32, TypeKind::I32, __VA_ARGS__) \
macro(name, result, i64, TypeKind::I64, __VA_ARGS__)

#define FOR_EACH_FLOAT(macro, name, result, ...) \
macro(name, result, f32, TypeKind::F32, __VA_ARGS__) \
macro(name, result, f64, TypeKind::F64, __VA_ARGS__)

#define FOR_EACH_NON_PTR_INT(macro, name, result, ...) \
FOR_EACH_NON_PTR_UINT(macro, name, result, __VA_ARGS__) \
FOR_EACH_SINT(macro, name, result, __VA_ARGS__)

#define FOR_EACH_INT(macro, name, result, ...) \
FOR_EACH_UINT(macro, name, result, __VA_ARGS__) \
FOR_EACH_SINT(macro, name, result, __VA_ARGS__)

#define FOR_EACH_NUMERIC(macro, name, result, ...) \
FOR_EACH_INT(macro, name, result, __VA_ARGS__) \
FOR_EACH_FLOAT(macro, name, result, __VA_ARGS__)

#endif