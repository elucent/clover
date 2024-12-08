#include "jasmine/test/common.h"

FOR_EACH_INT(DEFINE_SIMPLE_TEST, identity, 0,
    Module mod("test");
    Function& fn = mod.defineFunction(type, "identity");
    fn.addParameters(type, "x");
    auto bb0 = fn.addBlock();
    bb0.addNode(Opcode::RET, type, fn.variable("x"));
    COMPILE(linked);
    auto func = linked.lookup<ctype(ctype)>("identity");
    ASSERT(func(42) == 42);
);

FOR_EACH_INT(DEFINE_SIMPLE_TEST, add_parameters, 0,
    Module mod("test");
    Function& fn = mod.defineFunction(type, "add");
    fn.addParameters(type, "x", type, "y");
    auto bb0 = fn.addBlock();
    bb0.addNode(Opcode::ADD, type, fn.variable("x"), fn.variable("x"), fn.variable("y"));
    bb0.addNode(Opcode::RET, type, fn.variable("x"));
    COMPILE(linked);
    auto func = linked.lookup<ctype(ctype, ctype)>("add");
    ASSERT(func(21, 21) == 42);
);

FOR_EACH_INT(DEFINE_SIMPLE_TEST, branch_on_parameter, 0,
    Module mod("test");
    Function& fn = mod.defineFunction(type, "br_param");
    fn.addParameters(type, "x");
    auto bb0 = fn.addBlock(), bb1 = fn.addBlock(), bb2 = fn.addBlock(), bb3 = fn.addBlock();
    bb0.addNode(Opcode::BR_NE, type, fn.variable("x"), fn.intConst(0), fn.branch(bb0, bb1), fn.branch(bb0, bb2));
    bb1.addNode(Opcode::BR, VOID, fn.branch(bb1, bb3));
    bb2.addNode(Opcode::MOV, type, fn.variable("x"), fn.intConst(42));
    bb2.addNode(Opcode::BR, VOID, fn.branch(bb2, bb3));
    bb3.addNode(Opcode::RET, type, fn.variable("x"));
    COMPILE(linked);
    auto func = linked.lookup<ctype(ctype)>("br_param");
    ASSERT(func(0) == 42);
    ASSERT(func(1) == 1);
    ASSERT(func(2) == 2);
    ASSERT(func(3) == 3);
);

#define FOR_1_TO_15(macro, ...) \
    macro(1, __VA_ARGS__) macro(2, __VA_ARGS__) macro(3, __VA_ARGS__) macro(4, __VA_ARGS__) macro(5, __VA_ARGS__) \
    macro(6, __VA_ARGS__) macro(7, __VA_ARGS__) macro(8, __VA_ARGS__) macro(9, __VA_ARGS__) macro(10, __VA_ARGS__) \
    macro(11, __VA_ARGS__) macro(12, __VA_ARGS__) macro(13, __VA_ARGS__) macro(14, __VA_ARGS__) macro(15, __VA_ARGS__)
#define DEFINE_AGGREGATE_PARAMETER_TEST(sz, name, ...) \
TEST(name ## _ ## sz) { \
    constexpr size_t size = (sz); \
    __VA_ARGS__ \
}

FOR_1_TO_15(DEFINE_AGGREGATE_PARAMETER_TEST, take_small_aggregate_parameter,
    Module mod("test");
    Function& fn = mod.defineFunction(I8, "takes_aggregate");
    auto arrayType = mod.arrayType(I8, size);
    fn.addParameters(arrayType, "xs");
    auto bb0 = fn.addBlock();
    bb0.addNode(Opcode::GET_INDEX, I8, fn.variable("x"), fn.variable("xs"), fn.typeOperand(PTR), fn.intConst(size - 1));
    bb0.addNode(Opcode::RET, I8, fn.variable("x"));

    COMPILE(linked);
    auto func = linked.lookup<i8(array<i8, size>)>("takes_aggregate");
    array<i8, size> nums;
    for (i32 i = 0; i < size; i ++) nums[i] = i;
    ASSERT_EQUAL(func(nums), size - 1);
)

FOR_1_TO_15(DEFINE_AGGREGATE_PARAMETER_TEST, pass_small_aggregate_parameter,
    Module mod("test");
    auto arrayType = mod.arrayType(I8, size);
    {
        Function& fn = mod.defineFunction(I8, "takes_aggregate");
        fn.addParameters(arrayType, "xs");
        auto bb0 = fn.addBlock();
        bb0.addNode(Opcode::GET_INDEX, I8, fn.variable("x"), fn.variable("xs"), fn.typeOperand(PTR), fn.intConst(size - 1));
        bb0.addNode(Opcode::RET, I8, fn.variable("x"));
    }
    {
        Function& fn = mod.defineFunction(I8, "passes_aggregate");
        auto bb0 = fn.addBlock();
        bb0.addNode(Opcode::VAR, arrayType, fn.variable("xs"));
        for (u32 i = 0; i < size; i ++)
            bb0.addNode(Opcode::SET_INDEX, I8, fn.variable("xs"), fn.typeOperand(PTR), fn.intConst(i), fn.intConst(i));
        bb0.addNode(Opcode::CALL, mod.functionType(I8, arrayType), fn.variable("x"), fn.func("takes_aggregate"), fn.variable("xs"));
        bb0.addNode(Opcode::RET, I8, fn.variable("x"));
    }

    COMPILE(linked);
    auto func = linked.lookup<i8()>("passes_aggregate");
    ASSERT_EQUAL(func(), size - 1);
)

FOR_1_TO_15(DEFINE_AGGREGATE_PARAMETER_TEST, return_small_aggregate_parameter,
    Module mod("test");
    auto arrayType = mod.arrayType(I8, size);
    Function& fn = mod.defineFunction(arrayType, "returns_aggregate");
    auto bb0 = fn.addBlock();
    bb0.addNode(Opcode::VAR, arrayType, fn.variable("xs"));
    for (u32 i = 0; i < size; i ++)
        bb0.addNode(Opcode::SET_INDEX, I8, fn.variable("xs"), fn.typeOperand(PTR), fn.intConst(i), fn.intConst(i));
    bb0.addNode(Opcode::RET, arrayType, fn.variable("xs"));

    COMPILE(linked);
    auto func = linked.lookup<array<i8, size>()>("returns_aggregate");
    auto arr = func();
    for (i32 i = 0; i < size; i ++)
        ASSERT_EQUAL(arr[i], i);
)

TEST(pass_huge_aggregate_parameter) {
    Module mod("test");
    auto arrayType = mod.arrayType(I64, 16);
    {
        Function& fn = mod.defineFunction(I64, "takes_aggregate");
        fn.addParameters(arrayType, "xs");
        auto bb0 = fn.addBlock();
        bb0.addNode(Opcode::GET_INDEX, I64, fn.variable("x"), fn.variable("xs"), fn.typeOperand(PTR), fn.intConst(15));
        bb0.addNode(Opcode::RET, I64, fn.variable("x"));
    }
    {
        Function& fn = mod.defineFunction(I64, "passes_aggregate");
        auto bb0 = fn.addBlock();
        bb0.addNode(Opcode::VAR, arrayType, fn.variable("xs"));
        for (u32 i = 0; i < 16; i ++)
            bb0.addNode(Opcode::SET_INDEX, I64, fn.variable("xs"), fn.typeOperand(PTR), fn.intConst(i), fn.intConst(i));
        bb0.addNode(Opcode::CALL, mod.functionType(I64, arrayType), fn.variable("x"), fn.func("takes_aggregate"), fn.variable("xs"));
        bb0.addNode(Opcode::RET, I64, fn.variable("x"));
    }

    COMPILE(linked);
    auto func = linked.lookup<i64()>("passes_aggregate");
    ASSERT_EQUAL(func(), 15);
}