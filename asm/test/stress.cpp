#include "asm/arch.h"
#include "util/test/harness.h"

#if defined(RT_AMD64) && defined(RT_LINUX)
    #include "asm/arch/amd64.h"
    using Assembler = AMD64LinuxAssembler;
    constexpr mreg returnRegister = Assembler::RAX;
    constexpr mreg floatReturnRegister = Assembler::XMM0;
#else
    #error Unsupported target for assembler tests.
#endif

static u32 anonIndex = 0;
static Symbol anon(Assembly& as) {
    array<i8, 64> buf;
    slice<i8> io = buf;
    io = format(io, ".test", anonIndex ++);
    return as.symtab[const_slice<i8>{buf.begin(), 64 - io.size()}];
}

static i32 allocated_bytes;

struct TestContext {
    vec<Symbol> funcList;
    SymbolTable table;
    Assembly as;

    inline TestContext(): as(table) {
        Printer<AMD64LinuxAssembler>::write_to(io_stdout);
    }
};

#define RUN(results, returntype, ret) do { \
    LinkedAssembly linked = ctx.as.link(); \
    linked.load(); \
    allocated_bytes += linked.pages.size() * memory::PAGESIZE; \
    for (Symbol sym : ctx.funcList) { \
        returntype(*fun)() = linked.lookup<returntype()>(sym); \
        using Ret = decltype(fun()); \
        if (!bits_equal(fun(), Ret(ret))) { \
            u8* start = (u8*)fun; \
            println(); \
            println("  Test returned ", fun(), ", correct answer was ", Ret(ret)); \
            print("  "); \
            while (*start != 0xc3) print(hex(*start ++, 2), ' '); \
            println(hex(*start, 2), ' '); \
        } \
        ASSERT(bits_equal(fun(), Ret(ret))); \
    } \
} while(false); 

template<typename MoveType, typename OpType>
struct TernaryFuncs {
    ASMOpcode opcode;
    MoveType move;
    OpType op;

    inline TernaryFuncs(ASMOpcode opcode_in, MoveType move_in, OpType op_in):
        opcode(opcode_in), move(move_in), op(op_in) {}
};

#define TERNARY_FUNC(upper, mov, op) TernaryFuncs { ASMOpcode:: upper, Assembler:: mov, Assembler:: op }

template<typename MoveType, typename OpType>
struct BinaryFuncs {
    ASMOpcode opcode;
    MoveType move;
    OpType op;

    inline BinaryFuncs(ASMOpcode opcode_in, MoveType move_in, OpType op_in):
        opcode(opcode_in), move(move_in), op(op_in) {}
};

#define BINARY_FUNC(upper, mov, op) BinaryFuncs { ASMOpcode:: upper, Assembler:: mov, Assembler:: op }

void save_callee_saved_gps(Assembly& as, RegSet r) {
    r &= Assembler::callee_saved_gps();
    for (mreg gp : r)
        Assembler::push64(as, GP(gp));
}

void restore_callee_saved_gps(Assembly& as, RegSet r) {
    r &= Assembler::callee_saved_gps();
    vec<mreg> toSave;
    for (mreg gp : r)
        toSave.push(gp);
    for (i32 i = toSave.size() - 1; i >= 0; i --)
        Assembler::pop64(as, GP(toSave[i]));
}

template<typename... Args>
void save_callee_saved_gps(Assembly& as, Args... args) {
    return save_callee_saved_gps(as, RegSet(args...));
}

template<typename... Args>
void restore_callee_saved_gps(Assembly& as, Args... args) {
    return restore_callee_saved_gps(as, RegSet(args...));
}

void save_callee_saved_fps(Assembly& as, RegSet r) {
    r &= Assembler::callee_saved_fps();
    for (mreg fp : r)
        Assembler::fpush64(as, FP(fp));
}

void restore_callee_saved_fps(Assembly& as, RegSet r) {
    r &= Assembler::callee_saved_fps();
    vec<mreg> toSave;
    for (mreg fp : r)
        toSave.push(fp);
    for (i32 i = toSave.size() - 1; i >= 0; i --)
        Assembler::fpop64(as, FP(toSave[i]));
}

template<typename... Args>
void save_callee_saved_fps(Assembly& as, Args... args) {
    return save_callee_saved_fps(as, RegSet(args...));
}

template<typename... Args>
void restore_callee_saved_fps(Assembly& as, Args... args) {
    return restore_callee_saved_fps(as, RegSet(args...));
}

template<typename MoveType, typename OpType>
void gen_binary_int(TestContext& ctx, BinaryFuncs<MoveType, OpType> funcs, ASMVal operand) {
    Assembly& as = ctx.as;
    RegSet allowed = Assembler::gps() - Assembler::clobbers(funcs.opcode);

    for (mreg l : allowed) {
        for (mreg d : allowed) {
            Symbol sym = anon(as);
            ctx.funcList.push(sym);
            Assembler::global(as, sym);
            save_callee_saved_gps(as, l, d);
            funcs.move(as, GP(l), operand);
            funcs.op(as, GP(d), GP(l));
            funcs.move(as, GP(returnRegister), GP(d));
            restore_callee_saved_gps(as, l, d);
            Assembler::ret(as);
        }
    }
}

template<typename MoveType, typename OpType>
void gen_ternary_int(TestContext& ctx, TernaryFuncs<MoveType, OpType> funcs, ASMVal left, ASMVal right) {
    Assembly& as = ctx.as;
    RegSet allowed = Assembler::gps() - Assembler::clobbers(funcs.opcode);
    
    // Both in registers.
    for (mreg l : allowed) {
        for (mreg r : allowed) {
            for (mreg d : allowed) {
                if (l == r)
                    continue;
                Symbol sym = anon(as);
                ctx.funcList.push(sym);
                Assembler::global(as, sym);
                save_callee_saved_gps(as, l, r, d);
                funcs.move(as, GP(l), left);
                funcs.move(as, GP(r), right);
                funcs.op(as, GP(d), GP(l), GP(r));
                funcs.move(as, GP(returnRegister), GP(d));
                restore_callee_saved_gps(as, l, r, d);
                Assembler::ret(as);
            }
        }
    }

    // Left is immediate.
    for (mreg r : allowed) {
        for (mreg d : allowed) {
            Symbol sym = anon(as);
            ctx.funcList.push(sym);
            Assembler::global(as, sym);
            save_callee_saved_gps(as, r, d);
            funcs.move(as, GP(r), right);
            funcs.op(as, GP(d), left, GP(r));
            funcs.move(as, GP(returnRegister), GP(d));
            restore_callee_saved_gps(as, r, d);
            Assembler::ret(as);
        }
    }

    // Right is immediate.
    for (mreg l : allowed) {
        for (mreg d : allowed) {
            Symbol sym = anon(as);
            ctx.funcList.push(sym);
            Assembler::global(as, sym);
            save_callee_saved_gps(as, l, d);
            funcs.move(as, GP(l), left);
            funcs.op(as, GP(d), GP(l), right);
            funcs.move(as, GP(returnRegister), GP(d));
            restore_callee_saved_gps(as, l, d);
            Assembler::ret(as);
        }
    }
}

template<typename MoveType, typename OpType>
void gen_binary_float(TestContext& ctx, BinaryFuncs<MoveType, OpType> funcs, ASMVal operand) {
    Assembly& as = ctx.as;
    RegSet allowed = Assembler::fps() - Assembler::clobbers(funcs.opcode);

    for (mreg l : allowed) {
        for (mreg d : allowed) {
            Symbol sym = anon(as);
            ctx.funcList.push(sym);
            Assembler::global(as, sym);
            save_callee_saved_fps(as, l, d);
            funcs.move(as, FP(l), operand);
            funcs.op(as, FP(d), FP(l));
            funcs.move(as, FP(returnRegister), FP(d));
            restore_callee_saved_fps(as, l, d);
            Assembler::ret(as);
        }
    }
}

template<typename MoveType, typename OpType>
void gen_ternary_float(TestContext& ctx, TernaryFuncs<MoveType, OpType> funcs, ASMVal left, ASMVal right) {
    Assembly& as = ctx.as;
    RegSet allowed = Assembler::fps() - Assembler::clobbers(funcs.opcode);
    
    // Both in registers.
    for (mreg l : allowed) {
        for (mreg r : allowed) {
            for (mreg d : allowed) {
                if (l == r)
                    continue;
                Symbol sym = anon(as);
                ctx.funcList.push(sym);
                Assembler::global(as, sym);
                save_callee_saved_fps(as, l, r, d);
                funcs.move(as, FP(l), left);
                funcs.move(as, FP(r), right);
                funcs.op(as, FP(d), FP(l), FP(r));
                funcs.move(as, FP(floatReturnRegister), FP(d));
                restore_callee_saved_fps(as, l, r, d);
                Assembler::ret(as);
            }
        }
    }

    // Left is immediate.
    for (mreg r : allowed) {
        for (mreg d : allowed) {
            Symbol sym = anon(as);
            ctx.funcList.push(sym);
            Assembler::global(as, sym);
            save_callee_saved_fps(as, r, d);
            funcs.move(as, FP(r), right);
            funcs.op(as, FP(d), left, FP(r));
            funcs.move(as, FP(floatReturnRegister), FP(d));
            restore_callee_saved_fps(as, r, d);
            Assembler::ret(as);
        }
    }

    // Right is immediate.
    for (mreg l : allowed) {
        for (mreg d : allowed) {
            Symbol sym = anon(as);
            ctx.funcList.push(sym);
            Assembler::global(as, sym);
            save_callee_saved_fps(as, l, d);
            funcs.move(as, FP(l), left);
            funcs.op(as, FP(d), FP(l), right);
            funcs.move(as, FP(floatReturnRegister), FP(d));
            restore_callee_saved_fps(as, l, d);
            Assembler::ret(as);
        }
    }
}

#define MAKE_TERNARY_INT_TESTS_FOR_EACH_WIDTH(upper, lower, test_name, left, right, result) \
MAKE_TERNARY_INT_TEST_FOR_WIDTH(upper, lower, 8, test_name, left, right, result) \
MAKE_TERNARY_INT_TEST_FOR_WIDTH(upper, lower, 16, test_name, left, right, result) \
MAKE_TERNARY_INT_TEST_FOR_WIDTH(upper, lower, 32, test_name, left, right, result) \
MAKE_TERNARY_INT_TEST_FOR_WIDTH(upper, lower, 64, test_name, left, right, result)

#define MAKE_TERNARY_INT_TEST_FOR_WIDTH(upper, lower, width, test_name, left, right, result) \
TEST(asm_##lower##width##_##test_name) { \
    TestContext ctx; \
    gen_ternary_int(ctx, TERNARY_FUNC(upper ## width, mov ## width, lower ## width), Imm(left), Imm(right)); \
    RUN(results, i ## width, result); \
}

#define MAKE_BINARY_INT_TESTS_FOR_EACH_WIDTH(upper, lower, test_name, operand, result) \
MAKE_BINARY_INT_TEST_FOR_WIDTH(upper, lower, 8, test_name, operand, result) \
MAKE_BINARY_INT_TEST_FOR_WIDTH(upper, lower, 16, test_name, operand, result) \
MAKE_BINARY_INT_TEST_FOR_WIDTH(upper, lower, 32, test_name, operand, result) \
MAKE_BINARY_INT_TEST_FOR_WIDTH(upper, lower, 64, test_name, operand, result)

#define MAKE_BINARY_INT_TEST_FOR_WIDTH(upper, lower, width, test_name, operand, result) \
TEST(asm_##lower##width##_##test_name) { \
    TestContext ctx; \
    gen_binary_int(ctx, BINARY_FUNC(upper ## width, mov ## width, lower ## width), Imm(operand)); \
    RUN(results, i ## width, result); \
}

#define MAKE_TERNARY_FLOAT_TESTS_FOR_EACH_WIDTH(upper, lower, test_name, left, right, result) \
MAKE_TERNARY_FLOAT_TEST_FOR_WIDTH(upper, lower, 32, test_name, left, right, result) \
MAKE_TERNARY_FLOAT_TEST_FOR_WIDTH(upper, lower, 64, test_name, left, right, result)

#define MAKE_TERNARY_FLOAT_TEST_FOR_WIDTH(upper, lower, width, test_name, left, right, result) \
TEST(asm_##lower##width##_##test_name) { \
    TestContext ctx; \
    gen_ternary_float(ctx, TERNARY_FUNC(upper ## width, fmov ## width, lower ## width), F ## width (left), F ## width (right)); \
    RUN(results, f ## width, result); \
}

#define MAKE_BINARY_FLOAT_TESTS_FOR_EACH_WIDTH(upper, lower, test_name, operand, result) \
MAKE_BINARY_FLOAT_TEST_FOR_WIDTH(upper, lower, 32, test_name, operand, result) \
MAKE_BINARY_FLOAT_TEST_FOR_WIDTH(upper, lower, 64, test_name, operand, result)

#define MAKE_BINARY_FLOAT_TEST_FOR_WIDTH(upper, lower, width, test_name, operand, result) \
TEST(asm_##lower##width##_##test_name) { \
    TestContext ctx; \
    gen_binary_float(ctx, BINARY_FUNC(upper ## width, fmov ## width, lower ## width), F ## width (operand)); \
    RUN(results, f ## width, result); \
}

// Arithmetic instructions.

MAKE_TERNARY_INT_TESTS_FOR_EACH_WIDTH(ADD, add, one_plus_two, 1, 2, 3);
MAKE_TERNARY_INT_TESTS_FOR_EACH_WIDTH(ADD, add, zero_plus_zero, 0, 0, 0);
MAKE_TERNARY_INT_TESTS_FOR_EACH_WIDTH(ADD, add, one_plus_minus_one, 1, -1, 0);
MAKE_TERNARY_INT_TESTS_FOR_EACH_WIDTH(ADD, add, numbers, 97, -33, 64);
MAKE_TERNARY_INT_TEST_FOR_WIDTH(ADD, add, 8, overflow, 0x7f, 1, -0x80);
MAKE_TERNARY_INT_TEST_FOR_WIDTH(ADD, add, 16, overflow, 0x7fff, 1, -0x8000);
MAKE_TERNARY_INT_TEST_FOR_WIDTH(ADD, add, 32, overflow, 0x7fffffff, 1, -0x80000000);
MAKE_TERNARY_INT_TEST_FOR_WIDTH(ADD, add, 8, underflow, -0x80, -1, 0x7f);
MAKE_TERNARY_INT_TEST_FOR_WIDTH(ADD, add, 16, underflow, -0x8000, -1, 0x7fff);
MAKE_TERNARY_INT_TEST_FOR_WIDTH(ADD, add, 32, underflow, -0x80000000, -1, 0x7fffffff);

MAKE_TERNARY_INT_TESTS_FOR_EACH_WIDTH(SUB, sub, two_minus_one, 2, 1, 1)
MAKE_TERNARY_INT_TESTS_FOR_EACH_WIDTH(SUB, sub, zero_minus_zero, 0, 0, 0)
MAKE_TERNARY_INT_TESTS_FOR_EACH_WIDTH(SUB, sub, four_minus_negative_eight, 4, -8, 12)
MAKE_TERNARY_INT_TESTS_FOR_EACH_WIDTH(SUB, sub, numbers, 17, 81, -64)
MAKE_TERNARY_INT_TEST_FOR_WIDTH(SUB, sub, 8, overflow, 0x7f, -1, -0x80);
MAKE_TERNARY_INT_TEST_FOR_WIDTH(SUB, sub, 16, overflow, 0x7fff, -1, -0x8000);
MAKE_TERNARY_INT_TEST_FOR_WIDTH(SUB, sub, 32, overflow, 0x7fffffff, -1, -0x80000000);
MAKE_TERNARY_INT_TEST_FOR_WIDTH(SUB, sub, 8, underflow, -0x80, 1, 0x7f);
MAKE_TERNARY_INT_TEST_FOR_WIDTH(SUB, sub, 16, underflow, -0x8000, 1, 0x7fff);
MAKE_TERNARY_INT_TEST_FOR_WIDTH(SUB, sub, 32, underflow, -0x80000000, 1, 0x7fffffff);

MAKE_TERNARY_INT_TESTS_FOR_EACH_WIDTH(MUL, mul, one_times_one, 1, 1, 1);
MAKE_TERNARY_INT_TESTS_FOR_EACH_WIDTH(MUL, mul, one_times_number, 1, 81, 81);
MAKE_TERNARY_INT_TESTS_FOR_EACH_WIDTH(MUL, mul, minus_one_times_number, -1, 42, -42);
MAKE_TERNARY_INT_TESTS_FOR_EACH_WIDTH(MUL, mul, both_negative, -17, -5, 85);
MAKE_TERNARY_INT_TESTS_FOR_EACH_WIDTH(MUL, mul, both_positive, 23, 4, 92);
MAKE_TERNARY_INT_TESTS_FOR_EACH_WIDTH(MUL, mul, zero_times_zero, 0, 0, 0);
MAKE_TERNARY_INT_TESTS_FOR_EACH_WIDTH(MUL, mul, negative_times_zero, -41, 0, 0);
MAKE_TERNARY_INT_TESTS_FOR_EACH_WIDTH(MUL, mul, positive_times_zero, 102, 0, 0);
MAKE_TERNARY_INT_TEST_FOR_WIDTH(MUL, mul, 8, overflow, 13, 13, -87);
MAKE_TERNARY_INT_TEST_FOR_WIDTH(MUL, mul, 16, overflow, 410, 573, -27214);
MAKE_TERNARY_INT_TEST_FOR_WIDTH(MUL, mul, 32, overflow, 78857, 90715, -1436421837);
MAKE_TERNARY_INT_TEST_FOR_WIDTH(MUL, mul, 8, underflow, 21, -42, -114);
MAKE_TERNARY_INT_TEST_FOR_WIDTH(MUL, mul, 16, underflow, 793, -232, 12632);
MAKE_TERNARY_INT_TEST_FOR_WIDTH(MUL, mul, 32, underflow, 1340452, -88703, 1356970532);

MAKE_TERNARY_INT_TESTS_FOR_EACH_WIDTH(SDIV, sdiv, two_divided_by_one, 2, 1, 2);
MAKE_TERNARY_INT_TESTS_FOR_EACH_WIDTH(SDIV, sdiv, zero_divided_by_number, 0, 42, 0);
MAKE_TERNARY_INT_TESTS_FOR_EACH_WIDTH(SDIV, sdiv, zero_divided_by_negative_number, 0, -42, 0);
MAKE_TERNARY_INT_TESTS_FOR_EACH_WIDTH(SDIV, sdiv, number_divided_by_one, 71, 1, 71);
MAKE_TERNARY_INT_TESTS_FOR_EACH_WIDTH(SDIV, sdiv, number_divided_by_minus_one, 71, -1, -71);
MAKE_TERNARY_INT_TESTS_FOR_EACH_WIDTH(SDIV, sdiv, negative_divided_by_negative, -57, -19, 3);
MAKE_TERNARY_INT_TESTS_FOR_EACH_WIDTH(SDIV, sdiv, bigger_denominator, 14, 70, 0);
MAKE_TERNARY_INT_TESTS_FOR_EACH_WIDTH(SDIV, sdiv, bigger_negative_denominator, 14, -70, 0);
MAKE_TERNARY_INT_TESTS_FOR_EACH_WIDTH(SDIV, sdiv, round_positive_down, 47, 12, 3);
MAKE_TERNARY_INT_TESTS_FOR_EACH_WIDTH(SDIV, sdiv, round_negative_up, -59, 12, -4);
MAKE_TERNARY_INT_TESTS_FOR_EACH_WIDTH(SDIV, sdiv, number_divided_by_po2, 83, 16, 5);
MAKE_TERNARY_INT_TESTS_FOR_EACH_WIDTH(SDIV, sdiv, negative_number_divided_by_po2, -83, 16, -5);
MAKE_TERNARY_INT_TEST_FOR_WIDTH(SDIV, sdiv, 8, check_signed, 0xc0, 16, -0x4);
MAKE_TERNARY_INT_TEST_FOR_WIDTH(SDIV, sdiv, 16, check_signed, 0xc000, 16, -0x400);
MAKE_TERNARY_INT_TEST_FOR_WIDTH(SDIV, sdiv, 32, check_signed, 0xc0000000, 16, -0x4000000);

MAKE_TERNARY_INT_TESTS_FOR_EACH_WIDTH(UDIV, udiv, two_divided_by_one, 2, 1, 2);
MAKE_TERNARY_INT_TESTS_FOR_EACH_WIDTH(UDIV, udiv, zero_divided_by_number, 0, 42, 0);
MAKE_TERNARY_INT_TESTS_FOR_EACH_WIDTH(UDIV, udiv, number_divided_by_one, 71, 1, 71);
MAKE_TERNARY_INT_TESTS_FOR_EACH_WIDTH(UDIV, udiv, bigger_denominator, 14, 70, 0);
MAKE_TERNARY_INT_TESTS_FOR_EACH_WIDTH(UDIV, udiv, round_positive_down, 47, 12, 3);
MAKE_TERNARY_INT_TESTS_FOR_EACH_WIDTH(UDIV, udiv, number_divided_by_po2, 83, 16, 5);
MAKE_TERNARY_INT_TEST_FOR_WIDTH(UDIV, udiv, 8, check_unsigned, 0xc0, 16, 0xc);
MAKE_TERNARY_INT_TEST_FOR_WIDTH(UDIV, udiv, 16, check_unsigned, 0xc000, 16, 0xc00);
MAKE_TERNARY_INT_TEST_FOR_WIDTH(UDIV, udiv, 32, check_unsigned, 0xc0000000, 16, 0xc000000);

MAKE_TERNARY_INT_TESTS_FOR_EACH_WIDTH(SREM, srem, positive_over_one, 51, 1, 0);
MAKE_TERNARY_INT_TESTS_FOR_EACH_WIDTH(SREM, srem, negative_over_one, -51, 1, 0);
MAKE_TERNARY_INT_TESTS_FOR_EACH_WIDTH(SREM, srem, positive_over_po2, 73, 16, 9);
MAKE_TERNARY_INT_TESTS_FOR_EACH_WIDTH(SREM, srem, negative_over_po2, -91, 16, -11);
MAKE_TERNARY_INT_TESTS_FOR_EACH_WIDTH(SREM, srem, zero_over_number, 0, 73, 0);
MAKE_TERNARY_INT_TESTS_FOR_EACH_WIDTH(SREM, srem, zero_over_negative_number, 0, -17, 0);
MAKE_TERNARY_INT_TESTS_FOR_EACH_WIDTH(SREM, srem, bigger_denominator, 15, 19, 15);
MAKE_TERNARY_INT_TESTS_FOR_EACH_WIDTH(SREM, srem, bigger_negative_denominator, 15, -19, 15);
MAKE_TERNARY_INT_TEST_FOR_WIDTH(SREM, srem, 8, check_signed, 0xbf, 16, -0x1);
MAKE_TERNARY_INT_TEST_FOR_WIDTH(SREM, srem, 16, check_signed, 0xbfff, 16, -0x1);
MAKE_TERNARY_INT_TEST_FOR_WIDTH(SREM, srem, 32, check_signed, 0xbfffffff, 16, -0x1);

MAKE_TERNARY_INT_TESTS_FOR_EACH_WIDTH(UREM, urem, positive_over_one, 51, 1, 0);
MAKE_TERNARY_INT_TESTS_FOR_EACH_WIDTH(UREM, urem, positive_over_po2, 73, 16, 9);
MAKE_TERNARY_INT_TESTS_FOR_EACH_WIDTH(UREM, urem, zero_over_number, 0, 73, 0);
MAKE_TERNARY_INT_TESTS_FOR_EACH_WIDTH(UREM, urem, bigger_denominator, 15, 19, 15);
MAKE_TERNARY_INT_TEST_FOR_WIDTH(UREM, urem, 8, check_unsigned, 0xbf, 16, 0xf);
MAKE_TERNARY_INT_TEST_FOR_WIDTH(UREM, urem, 16, check_unsigned, 0xbfff, 16, 0xf);
MAKE_TERNARY_INT_TEST_FOR_WIDTH(UREM, urem, 32, check_unsigned, 0xbfffffff, 16, 0xf);

MAKE_BINARY_INT_TESTS_FOR_EACH_WIDTH(NEG, neg, positive_number, 48, -48);
MAKE_BINARY_INT_TESTS_FOR_EACH_WIDTH(NEG, neg, negative_number, -48, 48);
MAKE_BINARY_INT_TESTS_FOR_EACH_WIDTH(NEG, neg, zero, 0, 0);
MAKE_BINARY_INT_TEST_FOR_WIDTH(NEG, neg, 8, overflow, -0x80, -0x80);
MAKE_BINARY_INT_TEST_FOR_WIDTH(NEG, neg, 16, overflow, -0x8000, -0x8000);
MAKE_BINARY_INT_TEST_FOR_WIDTH(NEG, neg, 32, overflow, -0x80000000, -0x80000000);

// Floating-point instructions.

MAKE_TERNARY_FLOAT_TESTS_FOR_EACH_WIDTH(FADD, fadd, one_plus_two, 1.0, 2.0, 1.0 + 2.0);
MAKE_TERNARY_FLOAT_TESTS_FOR_EACH_WIDTH(FADD, fadd, zero_plus_zero, 0.0, 0.0, 0.0);
MAKE_TERNARY_FLOAT_TESTS_FOR_EACH_WIDTH(FADD, fadd, zero_plus_negative_zero, 0.0, -0.0, 0.0);
MAKE_TERNARY_FLOAT_TESTS_FOR_EACH_WIDTH(FADD, fadd, negative_zero_plus_zero, -0.0, 0.0, 0.0);
MAKE_TERNARY_FLOAT_TESTS_FOR_EACH_WIDTH(FADD, fadd, negative_zero_plus_negative_zero, -0.0, -0.0, -0.0);
MAKE_TERNARY_FLOAT_TESTS_FOR_EACH_WIDTH(FADD, fadd, number_plus_zero, 521.0, 0.0, 521.0);
MAKE_TERNARY_FLOAT_TESTS_FOR_EACH_WIDTH(FADD, fadd, positive_plus_small_negative, 31.0, -16.0, 31.0 + -16.0);
MAKE_TERNARY_FLOAT_TESTS_FOR_EACH_WIDTH(FADD, fadd, positive_plus_big_negative, 31.0, -55.0, -24.0);
MAKE_TERNARY_FLOAT_TESTS_FOR_EACH_WIDTH(FADD, fadd, infinity_plus_positive, Infinity, 42.0, Infinity);
MAKE_TERNARY_FLOAT_TESTS_FOR_EACH_WIDTH(FADD, fadd, infinity_plus_negitive, Infinity, -42.0, Infinity);
MAKE_TERNARY_FLOAT_TESTS_FOR_EACH_WIDTH(FADD, fadd, minus_infinity_plus_positive, -Infinity, 42.0, -Infinity);
MAKE_TERNARY_FLOAT_TESTS_FOR_EACH_WIDTH(FADD, fadd, minus_infinity_plus_negitive, -Infinity, -42.0, -Infinity);
MAKE_TERNARY_FLOAT_TESTS_FOR_EACH_WIDTH(FADD, fadd, infinity_plus_infinity, Infinity, Infinity, Infinity);
MAKE_TERNARY_FLOAT_TESTS_FOR_EACH_WIDTH(FADD, fadd, infinity_plus_minus_infinity, Infinity, -Infinity, -NaN);
MAKE_TERNARY_FLOAT_TESTS_FOR_EACH_WIDTH(FADD, fadd, nan_plus_anything, NaN, 42.0, NaN);
MAKE_TERNARY_FLOAT_TESTS_FOR_EACH_WIDTH(FADD, fadd, nan_plus_nan, NaN, NaN, NaN);
MAKE_TERNARY_FLOAT_TESTS_FOR_EACH_WIDTH(FADD, fadd, nan_plus_infinity, NaN, Infinity, NaN);

MAKE_TERNARY_FLOAT_TESTS_FOR_EACH_WIDTH(FSUB, fsub, two_minus_one, 2.0, 1.0, 1.0);
MAKE_TERNARY_FLOAT_TESTS_FOR_EACH_WIDTH(FSUB, fsub, one_minus_two, 1.0, 2.0, -1.0);
MAKE_TERNARY_FLOAT_TESTS_FOR_EACH_WIDTH(FSUB, fsub, zero_minus_zero, 0.0, 0.0, 0.0);
MAKE_TERNARY_FLOAT_TESTS_FOR_EACH_WIDTH(FSUB, fsub, zero_minus_negative_zero, 0.0, -0.0, 0.0);
MAKE_TERNARY_FLOAT_TESTS_FOR_EACH_WIDTH(FSUB, fsub, negative_zero_minus_zero, -0.0, 0.0, -0.0);
MAKE_TERNARY_FLOAT_TESTS_FOR_EACH_WIDTH(FSUB, fsub, negative_zero_minus_negative_zero, -0.0, -0.0, 0.0);
MAKE_TERNARY_FLOAT_TESTS_FOR_EACH_WIDTH(FSUB, fsub, number_minus_zero, 521.0, 0.0, 521.0);
MAKE_TERNARY_FLOAT_TESTS_FOR_EACH_WIDTH(FSUB, fsub, positive_minus_small_negative, 31.0, -16.0, 31.0 - -16.0);
MAKE_TERNARY_FLOAT_TESTS_FOR_EACH_WIDTH(FSUB, fsub, positive_minus_big_negative, 31.0, -55.0, 86.0);
MAKE_TERNARY_FLOAT_TESTS_FOR_EACH_WIDTH(FSUB, fsub, infinity_minus_positive, Infinity, 42.0, Infinity);
MAKE_TERNARY_FLOAT_TESTS_FOR_EACH_WIDTH(FSUB, fsub, infinity_minus_negitive, Infinity, -42.0, Infinity);
MAKE_TERNARY_FLOAT_TESTS_FOR_EACH_WIDTH(FSUB, fsub, minus_infinity_minus_positive, -Infinity, 42.0, -Infinity);
MAKE_TERNARY_FLOAT_TESTS_FOR_EACH_WIDTH(FSUB, fsub, minus_infinity_minus_negitive, -Infinity, -42.0, -Infinity);
MAKE_TERNARY_FLOAT_TESTS_FOR_EACH_WIDTH(FSUB, fsub, infinity_minus_infinity, Infinity, Infinity, -NaN);
MAKE_TERNARY_FLOAT_TESTS_FOR_EACH_WIDTH(FSUB, fsub, infinity_minus_minus_infinity, Infinity, -Infinity, Infinity);
MAKE_TERNARY_FLOAT_TESTS_FOR_EACH_WIDTH(FSUB, fsub, nan_minus_anything, NaN, 42.0, NaN);
MAKE_TERNARY_FLOAT_TESTS_FOR_EACH_WIDTH(FSUB, fsub, nan_minus_infinity, NaN, Infinity, NaN);

MAKE_TERNARY_FLOAT_TESTS_FOR_EACH_WIDTH(FMUL, fmul, numbers, 42.0, 1.5, 63.0);
MAKE_TERNARY_FLOAT_TESTS_FOR_EACH_WIDTH(FMUL, fmul, multiply_by_negative, 42.0, -0.5, -21.0);
MAKE_TERNARY_FLOAT_TESTS_FOR_EACH_WIDTH(FMUL, fmul, number_times_one, 71.0, 1.0, 71.0);
MAKE_TERNARY_FLOAT_TESTS_FOR_EACH_WIDTH(FMUL, fmul, number_times_minus_one, 71.0, -1.0, -71.0);
MAKE_TERNARY_FLOAT_TESTS_FOR_EACH_WIDTH(FMUL, fmul, number_times_zero, 71.0, 0.0, 0.0);
MAKE_TERNARY_FLOAT_TESTS_FOR_EACH_WIDTH(FMUL, fmul, number_times_minus_zero, 71.0, -0.0, -0.0);
MAKE_TERNARY_FLOAT_TESTS_FOR_EACH_WIDTH(FMUL, fmul, zero_times_zero, 0.0, 0.0, 0.0);
MAKE_TERNARY_FLOAT_TESTS_FOR_EACH_WIDTH(FMUL, fmul, zero_times_minus_zero, 0.0, -0.0, -0.0);
MAKE_TERNARY_FLOAT_TESTS_FOR_EACH_WIDTH(FMUL, fmul, minus_zero_times_minus_zero, -0.0, -0.0, 0.0);
MAKE_TERNARY_FLOAT_TESTS_FOR_EACH_WIDTH(FMUL, fmul, infinity_times_anything, Infinity, 42.0, Infinity);
MAKE_TERNARY_FLOAT_TESTS_FOR_EACH_WIDTH(FMUL, fmul, infinity_times_negative, Infinity, -42.0, -Infinity);
MAKE_TERNARY_FLOAT_TESTS_FOR_EACH_WIDTH(FMUL, fmul, infinity_times_infinity, Infinity, Infinity, Infinity);
MAKE_TERNARY_FLOAT_TESTS_FOR_EACH_WIDTH(FMUL, fmul, infinity_times_minus_infinity, Infinity, -Infinity, -Infinity);
MAKE_TERNARY_FLOAT_TESTS_FOR_EACH_WIDTH(FMUL, fmul, minus_infinity_times_minus_infinity, -Infinity, -Infinity, Infinity);
MAKE_TERNARY_FLOAT_TESTS_FOR_EACH_WIDTH(FMUL, fmul, infinity_times_zero, Infinity, 0.0, -NaN);
MAKE_TERNARY_FLOAT_TESTS_FOR_EACH_WIDTH(FMUL, fmul, nan_times_anything, NaN, 17.0, NaN);
MAKE_TERNARY_FLOAT_TESTS_FOR_EACH_WIDTH(FMUL, fmul, nan_times_zero, NaN, 0.0, NaN);

MAKE_TERNARY_FLOAT_TESTS_FOR_EACH_WIDTH(FDIV, fdiv, numbers, 80.0, 4.0, 20.0);
MAKE_TERNARY_FLOAT_TESTS_FOR_EACH_WIDTH(FDIV, fdiv, bigger_denominator, 1.0, 2.0, 0.5);
MAKE_TERNARY_FLOAT_TESTS_FOR_EACH_WIDTH(FDIV, fdiv, divide_by_one, 93.0, 1.0, 93.0);
MAKE_TERNARY_FLOAT_TESTS_FOR_EACH_WIDTH(FDIV, fdiv, divide_by_minus_one, 93.0, -1.0, -93.0);
MAKE_TERNARY_FLOAT_TESTS_FOR_EACH_WIDTH(FDIV, fdiv, reciprocal, 1.0, 10.0, 0.1);
MAKE_TERNARY_FLOAT_TESTS_FOR_EACH_WIDTH(FDIV, fdiv, zero_numerator, 0.0, 193.0, 0.0);
MAKE_TERNARY_FLOAT_TESTS_FOR_EACH_WIDTH(FDIV, fdiv, minus_zero_numerator, -0.0, 193.0, -0.0);
MAKE_TERNARY_FLOAT_TESTS_FOR_EACH_WIDTH(FDIV, fdiv, divide_by_zero, 1.0, 0.0, Infinity);
MAKE_TERNARY_FLOAT_TESTS_FOR_EACH_WIDTH(FDIV, fdiv, divide_by_minus_zero, 1.0, -0.0, -Infinity);
MAKE_TERNARY_FLOAT_TESTS_FOR_EACH_WIDTH(FDIV, fdiv, divide_negative_by_zero, -1.0, 0.0, -Infinity);
MAKE_TERNARY_FLOAT_TESTS_FOR_EACH_WIDTH(FDIV, fdiv, zero_over_zero, 0.0, 0.0, -NaN);
MAKE_TERNARY_FLOAT_TESTS_FOR_EACH_WIDTH(FDIV, fdiv, anything_over_infinity, 93.0, Infinity, 0.0);
MAKE_TERNARY_FLOAT_TESTS_FOR_EACH_WIDTH(FDIV, fdiv, infinity_over_anything, Infinity, 17.0, Infinity);
MAKE_TERNARY_FLOAT_TESTS_FOR_EACH_WIDTH(FDIV, fdiv, infinity_over_infinity, Infinity, Infinity, -NaN);
MAKE_TERNARY_FLOAT_TESTS_FOR_EACH_WIDTH(FDIV, fdiv, divide_nan_by_anything, NaN, 173.0, NaN);
MAKE_TERNARY_FLOAT_TESTS_FOR_EACH_WIDTH(FDIV, fdiv, divide_nan_by_nan, NaN, NaN, NaN);
MAKE_TERNARY_FLOAT_TESTS_FOR_EACH_WIDTH(FDIV, fdiv, divide_zero_by_minus_zero, 0.0, -0.0, -NaN);

// Bitwise instructions.