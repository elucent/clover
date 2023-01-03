#include "jasmine/arch.h"
#include "jasmine/obj.h"
#include "test/harness.h"

using namespace jasmine;

template<typename T>
T calleeSavedCall(T(*func)()) {
    auto saveFrame = (T(*)(T(*)()))pushrframe;
    return saveFrame(func);
}

void dumpAssembly(i32 codeSize, LinkedAssembly& linked) {
    for (i8 i : const_slice<i8>(linked.code, codeSize))
        write_hex(file_stdout, (u8)i), print(' ');
    println();
}

#define COMPILE(T, ...) [&]() -> T {                \
    SymbolTable syms;                               \
    Assembly as(&syms);                             \
    auto func_label = syms.intern(4, "test");       \
    ASM::global(as, func_label);                    \
    __VA_ARGS__;                                    \
    ASM::ret(as);                                   \
    i32 codeSize = as.code.size();                  \
    LinkedAssembly linked = as.link();              \
    linked.load();                                  \
    auto test = linked.lookup<T()>(func_label);     \
    auto result = calleeSavedCall(test);            \
    linked.unload();                                \
    return result;                                  \
}()

#define COMPILETEST(expected, ...) do {                     \
    auto exp = (expected);                                  \
    using resultType = decltype(exp);                       \
    auto result = COMPILE(resultType, __VA_ARGS__);         \
    ASSERT(exp == result, "Unexpected result");             \
} while (0);

#define DEBUGCOMPILE(T, ...) [&]() -> T {           \
    SymbolTable syms;                               \
    Assembly as(&syms);                             \
    auto func_label = syms.intern(4, "test");       \
    ASM::global(as, func_label);                    \
    __VA_ARGS__;                                    \
    ASM::ret(as);                                   \
    i32 codeSize = as.code.size();                  \
    LinkedAssembly linked = as.link();              \
    linked.load();                                  \
    dumpAssembly(codeSize, linked);                 \
    auto test = linked.lookup<T()>(func_label);     \
    auto result = calleeSavedCall(test);            \
    linked.unload();                                \
    return result;                                  \
}()

#define DEBUGCOMPILETEST(expected, ...) do {                \
    auto exp = (expected);                                  \
    using resultType = decltype(exp);                       \
    auto result = DEBUGCOMPILE(resultType, __VA_ARGS__);    \
    ASSERT(exp == result, "Unexpected result");             \
} while (0);
using ASM = DefaultTarget;

#ifdef LIBCORE_AMD64

#define TEST_UNARY_REG_CUSTOM(name, type, bits, bank, ret, regctor, immctor, input, expected, ...)  \
    TEST(name##_register_amd64) {                                               \
        for (mreg r : bank) {                                     \
            [&](ASMVal dst, ASMVal r, ASMVal input, auto expected) {                        \
                    COMPILETEST(type(expected),                                 \
                        __VA_ARGS__);                                           \
                }(regctor(ret), regctor(r), immctor(input), expected);            \
            }                                                                   \
        }                                                                       \
    }

#define TEST_UNARY_REG(name, type, bits, bank, ret, regctor, immctor, op, input, expected)                  \
    TEST(name##_register_amd64) {                                                       \
        for (mreg r : bank) {                                              \
                COMPILETEST(type(expected),                                             \
                    ASM::mov##bits(as, regctor(r), immctor(input));          \
                    ASM:: op##bits(as, regctor(ret), regctor(r)));  \
        }                                                                               \
    }

#define TEST_UNARY_REG_IMM(name, type, bits, bank, ret, regctor, immctor, op, input, expected)          \
    TEST_UNARY_REG(name, type, bits, bank, ret, regctor, immctor, op, input, expected)                           \
    TEST(name##_immediate_amd64) {                                                  \
        for (mreg r : bank) {                                          \
            COMPILETEST(type(expected),                                             \
                ASM:: op##bits(as, regctor(r), immctor(input));        \
                ASM::mov##bits(as, regctor(ret), regctor(r)));  \
        }                                                                           \
    }

#define TEST_BINARY_REG_CUSTOM(name, type, bits, bank, ret, regctor, immctor, left, right, expected, ...)                                                           \
    TEST(name##_register_amd64) {                                                                                                               \
        for (mreg r1 : bank) {                                                                                                     \
            for (mreg r2 : bank) {                                                                                                 \
                if (r1 == r2) continue;                                                                                                         \
                [&](ASMVal dst, ASMVal r1, ASMVal r2, ASMVal left, ASMVal right, auto expected) {                                               \
                    COMPILETEST(type(expected),                                                                                                 \
                        __VA_ARGS__);                                                                                                           \
                }(regctor(ret), regctor(r1), regctor(r2), immctor(left), immctor(right), expected);   \
            }                                                                                                                                   \
        }                                                                                                                                       \
    }

#define TEST_BINARY_REG(name, type, bits, bank, ret, regctor, immctor, op, left, right, expected)                                       \
    TEST(name##_register_amd64) {                                                                                   \
        for (mreg r1 : bank) {                                                                         \
            for (mreg r2 : bank) {                                                                     \
                if (r1 == r2) continue;                                                                             \
                COMPILETEST(type(expected),                                                                         \
                    ASM::mov##bits(as, regctor(r1), immctor(left));                                     \
                    ASM::mov##bits(as, regctor(r2), immctor(right));                                    \
                    ASM:: op##bits(as, regctor(ret), regctor(r1), regctor(r2)));  \
            }                                                                                                       \
        }                                                                                                           \
    }

#define TEST_BINARY_REG_IMM(name, type, bits, bank, ret, regctor, immctor, op, left, right, expected)                       \
    TEST_BINARY_REG(name, type, bits, bank, ret, regctor, immctor, op, left, right, expected)                                        \
    TEST(name##_left_immediate_amd64) {                                                                 \
        for (mreg r : bank) {                                                              \
            COMPILETEST(type(expected),                                                                 \
                ASM::mov##bits(as, regctor(r), immctor(right));                             \
                ASM:: op##bits(as, regctor(ret), immctor(left), regctor(r)));  \
        }                                                                                               \
    }                                                                                                   \
    TEST(name##_right_immediate_amd64) {                                                                \
        for (mreg r : bank) {                                                              \
            COMPILETEST(type(expected),                                                                 \
                ASM::mov##bits(as, regctor(r), immctor(left));                                          \
                ASM:: op##bits(as, regctor(ret), regctor(r), immctor(right)));                \
        }                                                                                               \
    }

// GP registers for testing integer division and remainder, basically ASM::gps() minus rax, rcx and rdx
const mreg DIV_GPS[] = {
    ASM::RBX, ASM::RSI, ASM::RDI, 
    ASM::R8, ASM::R9, ASM::R10, ASM::R11,
    ASM::R12, ASM::R13, ASM::R14, ASM::R15
};

const_slice<mreg> DIV_GP_SLICE = {DIV_GPS, 11};

// GP registers for testing integer shifts, basically ASM::gps() minus rcx
const mreg SHIFT_GPS[] = {
    ASM::RAX, ASM::RDX, ASM::RBX, ASM::RSI, ASM::RDI, 
    ASM::R8, ASM::R9, ASM::R10, ASM::R11,
    ASM::R12, ASM::R13, ASM::R14, ASM::R15
};

const_slice<mreg> SHIFT_GP_SLICE = {SHIFT_GPS, 13};

#define FOR_INT8(name, macro, ...)                                   \
    macro(name##_int8, i8, 8, ASM::gps(), ASM::RAX, ASMVal::gp, ASMVal::imm, __VA_ARGS__)

#define FOR_INT16(name, macro, ...)                                   \
    macro(name##_int16, i16, 16, ASM::gps(), ASM::RAX, ASMVal::gp, ASMVal::imm, __VA_ARGS__)

#define FOR_INT32(name, macro, ...)                                   \
    macro(name##_int32, i32, 32, ASM::gps(), ASM::RAX, ASMVal::gp, ASMVal::imm, __VA_ARGS__)

#define FOR_INT64(name, macro, ...)                                   \
    macro(name##_int64, i64, 64, ASM::gps(), ASM::RAX, ASMVal::gp, ASMVal::imm, __VA_ARGS__)

#define FOR_INT16_INT32_INT64(name, macro, ...)                               \
    macro(name##_int16, i16, 16, ASM::gps(), ASM::RAX, ASMVal::gp, ASMVal::imm, __VA_ARGS__)   \
    macro(name##_int32, i32, 32, ASM::gps(), ASM::RAX, ASMVal::gp, ASMVal::imm, __VA_ARGS__)   \
    macro(name##_int64, i64, 64, ASM::gps(), ASM::RAX, ASMVal::gp, ASMVal::imm, __VA_ARGS__)

#define FOR_INT32_INT64(name, macro, ...)                                     \
    macro(name##_int32, i32, 32, ASM::gps(), ASM::RAX, ASMVal::gp, ASMVal::imm, __VA_ARGS__)   \
    macro(name##_int64, i64, 64, ASM::gps(), ASM::RAX, ASMVal::gp, ASMVal::imm, __VA_ARGS__)

#define FOR_EACH_INT(name, macro, ...)                                   \
    macro(name##_int8, i8, 8, ASM::gps(), ASM::RAX, ASMVal::gp, ASMVal::imm, __VA_ARGS__)      \
    macro(name##_int16, i16, 16, ASM::gps(), ASM::RAX, ASMVal::gp, ASMVal::imm, __VA_ARGS__)   \
    macro(name##_int32, i32, 32, ASM::gps(), ASM::RAX, ASMVal::gp, ASMVal::imm, __VA_ARGS__)   \
    macro(name##_int64, i64, 64, ASM::gps(), ASM::RAX, ASMVal::gp, ASMVal::imm, __VA_ARGS__)

#define FOR_INT8_DIV(name, macro, ...)                                   \
    macro(name##_int8, i8, 8, DIV_GP_SLICE, ASM::RAX, ASMVal::gp, ASMVal::imm, __VA_ARGS__)

#define FOR_INT16_DIV(name, macro, ...)                                   \
    macro(name##_int16, i16, 16, DIV_GP_SLICE, ASM::RAX, ASMVal::gp, ASMVal::imm, __VA_ARGS__)

#define FOR_INT32_DIV(name, macro, ...)                                   \
    macro(name##_int32, i32, 32, DIV_GP_SLICE, ASM::RAX, ASMVal::gp, ASMVal::imm, __VA_ARGS__)

#define FOR_INT64_DIV(name, macro, ...)                                   \
    macro(name##_int64, i64, 64, DIV_GP_SLICE, ASM::RAX, ASMVal::gp, ASMVal::imm, __VA_ARGS__)

#define FOR_EACH_INT_DIV(name, macro, ...)                                   \
    macro(name##_int8, i8, 8, DIV_GP_SLICE, ASM::RAX, ASMVal::gp, ASMVal::imm, __VA_ARGS__)      \
    macro(name##_int16, i16, 16, DIV_GP_SLICE, ASM::RAX, ASMVal::gp, ASMVal::imm, __VA_ARGS__)   \
    macro(name##_int32, i32, 32, DIV_GP_SLICE, ASM::RAX, ASMVal::gp, ASMVal::imm, __VA_ARGS__)   \
    macro(name##_int64, i64, 64, DIV_GP_SLICE, ASM::RAX, ASMVal::gp, ASMVal::imm, __VA_ARGS__)

#define FOR_INT8_SHIFT(name, macro, ...)                                   \
    macro(name##_int8, i8, 8, SHIFT_GP_SLICE, ASM::RAX, ASMVal::gp, ASMVal::imm, __VA_ARGS__)

#define FOR_INT16_SHIFT(name, macro, ...)                                   \
    macro(name##_int16, i16, 16, SHIFT_GP_SLICE, ASM::RAX, ASMVal::gp, ASMVal::imm, __VA_ARGS__)

#define FOR_INT32_SHIFT(name, macro, ...)                                   \
    macro(name##_int32, i32, 32, SHIFT_GP_SLICE, ASM::RAX, ASMVal::gp, ASMVal::imm, __VA_ARGS__)

#define FOR_INT64_SHIFT(name, macro, ...)                                   \
    macro(name##_int64, i64, 64, SHIFT_GP_SLICE, ASM::RAX, ASMVal::gp, ASMVal::imm, __VA_ARGS__)

#define FOR_EACH_INT_SHIFT(name, macro, ...)                                   \
    macro(name##_int8, i8, 8, SHIFT_GP_SLICE, ASM::RAX, ASMVal::gp, ASMVal::imm, __VA_ARGS__)      \
    macro(name##_int16, i16, 16, SHIFT_GP_SLICE, ASM::RAX, ASMVal::gp, ASMVal::imm, __VA_ARGS__)   \
    macro(name##_int32, i32, 32, SHIFT_GP_SLICE, ASM::RAX, ASMVal::gp, ASMVal::imm, __VA_ARGS__)   \
    macro(name##_int64, i64, 64, SHIFT_GP_SLICE, ASM::RAX, ASMVal::gp, ASMVal::imm, __VA_ARGS__)

#define FOR_F32(name, macro, ...)                                 \
    macro(name##_float32, f32, 32, ASM::fps(), ASM::XMM0, ASMVal::fp, ASMVal::f32, __VA_ARGS__)

#define FOR_F64(name, macro, ...)                                 \
    macro(name##_float64, f64, 64, ASM::fps(), ASM::XMM0, ASMVal::fp, ASMVal::f64, __VA_ARGS__)

#define FOR_EACH_FLOAT(name, macro, ...)                                 \
    macro(name##_float32, f32, 32, ASM::fps(), ASM::XMM0, ASMVal::fp, ASMVal::f32, __VA_ARGS__) \
    macro(name##_float64, f64, 64, ASM::fps(), ASM::XMM0, ASMVal::fp, ASMVal::f64, __VA_ARGS__)

#define TEST_INTEGER_UNARY(op, name, input, expected) FOR_EACH_INT(op##_##name, TEST_UNARY_REG_IMM, op, input, expected)
#define TEST_INTEGER_BINARY(op, name, left, right, expected) FOR_EACH_INT(op##_##name, TEST_BINARY_REG_IMM, op, left, right, expected)
#define TEST_INTEGER_DIV_REM(op, name, left, right, expected) FOR_EACH_INT_DIV(op##_##name, TEST_BINARY_REG_IMM, op, left, right, expected)

// Moves
TEST_INTEGER_UNARY(mov, constant, 42, 42);
FOR_INT16_INT32_INT64(mov_constant16, TEST_UNARY_REG_IMM, mov, 9000, 9000);
FOR_INT16_INT32_INT64(mov_negative16, TEST_UNARY_REG_IMM, mov, -9000, -9000);
FOR_INT32_INT64(mov_constant32, TEST_UNARY_REG_IMM, mov, 524288, 524288);
FOR_INT32_INT64(mov_negative32, TEST_UNARY_REG_IMM, mov, -524288, -524288);

// Arithmetic
TEST_INTEGER_BINARY(add, zeroes, 0, 0, 0);
TEST_INTEGER_BINARY(add, numbers, 29, 13, 42);
TEST_INTEGER_BINARY(add, complement, -42, 42, 0);

TEST_INTEGER_BINARY(sub, zero, 42, 0, 42);
TEST_INTEGER_BINARY(sub, negative, 1, 4, -3);
TEST_INTEGER_BINARY(sub, self, 42, 42, 0);

TEST_INTEGER_BINARY(mul, zero, 42, 0, 0);
TEST_INTEGER_BINARY(mul, negative_zero, -42, 0, 0);
TEST_INTEGER_BINARY(mul, one, 42, 1, 42);
TEST_INTEGER_BINARY(mul, numbers, 7, 13, 91);
TEST_INTEGER_BINARY(mul, one_negative, -13, 7, -91);
TEST_INTEGER_BINARY(mul, two_negative, -4, -4, 16);

TEST_INTEGER_DIV_REM(sdiv, zero, 0, 42, 0);
TEST_INTEGER_DIV_REM(sdiv, one, 42, 1, 42);
TEST_INTEGER_DIV_REM(sdiv, self, 42, 42, 1);
TEST_INTEGER_DIV_REM(sdiv, different_sign, 42, -42, -1);
TEST_INTEGER_DIV_REM(sdiv, same_sign, -42, -3, 14);

FOR_INT8_DIV(sdiv_check_signed, TEST_BINARY_REG_IMM, sdiv, 0xc0, 16, -0x4);
FOR_INT16_DIV(sdiv_check_signed, TEST_BINARY_REG_IMM, sdiv, 0xc000, 16, -0x400);
FOR_INT32_DIV(sdiv_check_signed, TEST_BINARY_REG_IMM, sdiv, 0xc0000000, 16, -0x4000000);

TEST_INTEGER_DIV_REM(udiv, zero, 0, 42, 0);
TEST_INTEGER_DIV_REM(udiv, one, 42, 1, 42);
TEST_INTEGER_DIV_REM(udiv, self, 42, 42, 1);

FOR_INT8_DIV(udiv_check_unsigned, TEST_BINARY_REG_IMM, udiv, 0xc0, 16, 0xc);
FOR_INT16_DIV(udiv_check_unsigned, TEST_BINARY_REG_IMM, udiv, 0xc000, 16, 0xc00);
FOR_INT32_DIV(udiv_check_unsigned, TEST_BINARY_REG_IMM, udiv, 0xc0000000, 16, 0xc000000);

// Bitwise operations
TEST_INTEGER_BINARY(and, different, 1, 2, 0);
TEST_INTEGER_BINARY(and, self, 42, 42, 42);
TEST_INTEGER_BINARY(and, mask, 15, 3, 3);
TEST_INTEGER_BINARY(and, zero, 42, 0, 0);

TEST_INTEGER_BINARY(or, different, 1, 2, 3);
TEST_INTEGER_BINARY(or, self, 42, 42, 42);
TEST_INTEGER_BINARY(or, mask, 15, 3, 15);
TEST_INTEGER_BINARY(or, zero, 42, 0, 42);

TEST_INTEGER_BINARY(xor, different, 1, 2, 3);
TEST_INTEGER_BINARY(xor, mixed, 5, 3, 6);
TEST_INTEGER_BINARY(xor, self, 42, 42, 0);
TEST_INTEGER_BINARY(xor, mask, 15, 3, 12);
TEST_INTEGER_BINARY(xor, zero, 42, 0, 42);
TEST_INTEGER_BINARY(xor, minus, 42, -1, -43);

TEST_INTEGER_UNARY(not, zero, 0, -1);
TEST_INTEGER_UNARY(not, one, 1, -2);
TEST_INTEGER_UNARY(not, minus_one, -1, 0);
TEST_INTEGER_UNARY(not, number, 42, -43);

FOR_EACH_INT_SHIFT(shl_zero, TEST_BINARY_REG_IMM, shl, 42, 0, 42);
FOR_EACH_INT_SHIFT(shl_one, TEST_BINARY_REG_IMM, shl, 42, 1, 84);

FOR_EACH_INT_SHIFT(shr_zero, TEST_BINARY_REG_IMM, shr, 42, 0, 42);
FOR_EACH_INT_SHIFT(shr_one, TEST_BINARY_REG_IMM, shr, 42, 1, 21);
FOR_EACH_INT_SHIFT(shr_check_unsigned, TEST_BINARY_REG_IMM, shr, 255, 1, 127);

FOR_EACH_INT_SHIFT(sar_zero, TEST_BINARY_REG_IMM, sar, 42, 0, 42);
FOR_EACH_INT_SHIFT(sar_one, TEST_BINARY_REG_IMM, sar, 42, 1, 21);
FOR_EACH_INT_SHIFT(sar_check_signed, TEST_BINARY_REG_IMM, sar, -1, 1, -1);
FOR_EACH_INT_SHIFT(sar_signed_half, TEST_BINARY_REG_IMM, sar, -42, 1, -21);

FOR_EACH_INT_SHIFT(rol_zero, TEST_BINARY_REG_IMM, rol, 42, 0, 42);
FOR_EACH_INT_SHIFT(rol_one, TEST_BINARY_REG_IMM, rol, 42, 1, 84);
FOR_INT8_SHIFT(rol_overflow, TEST_BINARY_REG_IMM, rol, 0x40, 1, 0x80);
FOR_INT16_SHIFT(rol_overflow, TEST_BINARY_REG_IMM, rol, 0x4000, 1, 0x8000);
FOR_INT32_SHIFT(rol_overflow, TEST_BINARY_REG_IMM, rol, 0x40000000, 1, 0x80000000);
FOR_INT8_SHIFT(rol_around, TEST_BINARY_REG_IMM, rol, 0x80, 1, 1);
FOR_INT16_SHIFT(rol_around, TEST_BINARY_REG_IMM, rol, 0x8000, 1, 1);
FOR_INT32_SHIFT(rol_around, TEST_BINARY_REG_IMM, rol, 0x80000000, 1, 1);

FOR_EACH_INT_SHIFT(ror_zero, TEST_BINARY_REG_IMM, ror, 42, 0, 42);
FOR_EACH_INT_SHIFT(ror_one, TEST_BINARY_REG_IMM, ror, 42, 1, 21);
FOR_INT8_SHIFT(ror_underflow, TEST_BINARY_REG_IMM, ror, 0x80, 1, 0x40);
FOR_INT16_SHIFT(ror_underflow, TEST_BINARY_REG_IMM, ror, 0x8000, 1, 0x4000);
FOR_INT32_SHIFT(ror_underflow, TEST_BINARY_REG_IMM, ror, 0x80000000, 1, 0x40000000);
FOR_INT8_SHIFT(ror_around, TEST_BINARY_REG_IMM, ror, 1, 1, 0x80);
FOR_INT16_SHIFT(ror_around, TEST_BINARY_REG_IMM, ror, 1, 1, 0x8000);
FOR_INT32_SHIFT(ror_around, TEST_BINARY_REG_IMM, ror, 1, 1, 0x80000000);

FOR_EACH_INT(bitc_zero, TEST_UNARY_REG, bitc, 0, 0);
FOR_EACH_INT(bitc_one, TEST_UNARY_REG, bitc, 1, 1);
FOR_EACH_INT(bitc_power_of_two, TEST_UNARY_REG, bitc, 64, 1);
FOR_EACH_INT(bitc_number, TEST_UNARY_REG, bitc, 39, 4);
FOR_INT8(bitc_minus_one, TEST_UNARY_REG, bitc, -1, 8);
FOR_INT16(bitc_minus_one, TEST_UNARY_REG, bitc, -1, 16);
FOR_INT32(bitc_minus_one, TEST_UNARY_REG, bitc, -1, 32);

FOR_EACH_INT(tzc_one, TEST_UNARY_REG, tzc, 1, 0);
FOR_EACH_INT(tzc_two, TEST_UNARY_REG, tzc, 2, 1);
FOR_EACH_INT(tzc_number, TEST_UNARY_REG, tzc, 28, 2);
FOR_INT8(tzc_zero, TEST_UNARY_REG, tzc, 0, 8);
FOR_INT16(tzc_zero, TEST_UNARY_REG, tzc, 0, 16);
FOR_INT32(tzc_zero, TEST_UNARY_REG, tzc, 0, 32);

FOR_EACH_INT(lzc_minus_one, TEST_UNARY_REG, lzc, -1, 0);
FOR_INT8(lzc_zero, TEST_UNARY_REG, lzc, 0, 8);
FOR_INT16(lzc_zero, TEST_UNARY_REG, lzc, 0, 16);
FOR_INT32(lzc_zero, TEST_UNARY_REG, lzc, 0, 32);
FOR_INT8(lzc_one, TEST_UNARY_REG, lzc, 1, 7);
FOR_INT16(lzc_one, TEST_UNARY_REG, lzc, 1, 15);
FOR_INT32(lzc_one, TEST_UNARY_REG, lzc, 1, 31);
FOR_INT8(lzc_number, TEST_UNARY_REG, lzc, 28, 3);
FOR_INT16(lzc_number, TEST_UNARY_REG, lzc, 28, 11);
FOR_INT32(lzc_number, TEST_UNARY_REG, lzc, 28, 27);

#endif