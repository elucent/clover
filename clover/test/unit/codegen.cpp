#include "clover/test/unit/helpers.h"

template<typename T>
T* lookup(const i8* name, JasmineExecutable exec) {
    void* ptr = jasmine_lookup_symbol(exec, name, findc(name, 0));
    return (T*)ptr;
}

#define FOR_EACH_INT(macro, ...) \
    macro(I8, i8, __VA_ARGS__) \
    macro(I16, i16, __VA_ARGS__) \
    macro(I32, i32, __VA_ARGS__) \
    macro(I64, i64, __VA_ARGS__)

#define FOR_EACH_UINT(macro, ...) \
    macro(U8, u8, __VA_ARGS__) \
    macro(U16, u16, __VA_ARGS__) \
    macro(U32, u32, __VA_ARGS__) \
    macro(U64, u64, __VA_ARGS__)

#define FOR_EACH_FLOAT(macro, ...) \
    macro(F32, f32, __VA_ARGS__) \
    macro(F64, f64, __VA_ARGS__)

#define FOR_EACH_NUMBER(macro, ...) \
    FOR_EACH_INT(macro, __VA_ARGS__) \
    FOR_EACH_UINT(macro, __VA_ARGS__) \
    FOR_EACH_FLOAT(macro, __VA_ARGS__)

struct Substitutions {
    map<const_slice<i8>, const_slice<i8>> substitutions;

    bool isdelim(i8 ch) {
        return ch == ' ' || ch == '\t' || ch == '\n' || ch == ',' || ch == ':' || ch == '(' || ch == ')'
            || ch == '{' || ch == '}' || ch == '[' || ch == ']' || ch == '.' || ch == '*';
    }

    const_slice<i8> substitute(const_slice<i8> src) {
        i32 i = 0;
        vec<i8> result;
        i8 buffer[64];
        while (i < src.size()) {
            if (src[i] == '$') {
                i ++;
                u32 j = 0;
                while (i < src.size() && !isdelim(src[i]))
                    buffer[j ++] = src[i ++];
                auto it = substitutions.find({ buffer, iword(j) });
                assert(it != substitutions.end());
                for (u32 j = 0; j < it->value.size(); j ++)
                    result.push(it->value[j]);
                continue;
            }
            result.push(src[i ++]);
        }
        // println("Substituted ", src, " into ", result);
        return result.take_slice();
    }
};

#define DEFINE_TEST(type_upper, type_lower, name, ...) \
    TEST(name ## _ ## type_lower) { \
        using type = type_lower; \
        Substitutions substitutions; \
        substitutions.substitutions.put(cstring("T"), cstring(#type_lower)); \
        __VA_ARGS__ \
    }

#define COMPILE_SUBST(src) \
    OwnedArtifact([&]() -> Artifact* { \
        Compilation* compilation = new Compilation(); \
        Artifact* source = addSourceString(compilation, cstring("test"), substitutions.substitute(cstring(src))); \
        source = lex(source); \
        source = clover::parse(source); \
        source = clover::computeScopes(source); \
        source = clover::resolveNamesAndTypes(source); \
        source = clover::inferAndCheckTypes(source); \
        source->update(ArtifactKind::FinalizedAST, source->as<Module>()); \
        source->as<Module>()->noMangling = true; \
        source = clover::generateJasmine(source, config::forceOptLevel == -1 ? 0 : config::forceOptLevel); \
        if (source->kind != ArtifactKind::Assembly) \
            source = clover::emitAssembly(source, config::forceOptLevel == -1 ? 0 : config::forceOptLevel); \
        return source; \
    }())

FOR_EACH_INT(DEFINE_TEST, codegen_arithmetic_simple,
    auto instance = COMPILE_SUBST(R"(
$T add($T a, $T b):
    a + b
$T sub($T a, $T b):
    a - b
$T mul($T a, $T b):
    a * b
$T div($T a, $T b):
    a / b
$T rem($T a, $T b):
    a % b
$T plus($T a):
    +a
$T minus($T a):
    -a
)");
    auto exec = load(instance.artifact);
    auto add = lookup<type(type, type)>("add", exec);
    auto sub = lookup<type(type, type)>("sub", exec);
    auto mul = lookup<type(type, type)>("mul", exec);
    auto div = lookup<type(type, type)>("div", exec);
    auto rem = lookup<type(type, type)>("rem", exec);
    auto plus = lookup<type(type)>("plus", exec);
    auto minus = lookup<type(type)>("minus", exec);

    ASSERT_EQUAL(add(15, 27), 42);
    ASSERT_EQUAL(sub(60, 18), 42);
    ASSERT_EQUAL(mul(6, 7), 42);
    ASSERT_EQUAL(div(127, 3), 42);
    ASSERT_EQUAL(rem(102, 60), 42);
    ASSERT_EQUAL(plus(42), 42);
    ASSERT_EQUAL(minus(-42), 42);
)

FOR_EACH_INT(DEFINE_TEST, codegen_bitwise_simple,
    auto instance = COMPILE_SUBST(R"(
$T bitand($T a, $T b):
    a & b
$T bitor($T a, $T b):
    a | b
$T bitxor($T a, $T b):
    a ^ b
$T bitnot($T a):
    ~a
$T shl($T a, $T b):
    a << b
$T shr($T a, $T b):
    a >> b
$T rol($T a, $T b):
    a /< b
$T ror($T a, $T b):
    a /> b
)");
    auto exec = load(instance.artifact);
    auto band = lookup<type(type, type)>("bitand", exec);
    auto bor = lookup<type(type, type)>("bitor", exec);
    auto bxor = lookup<type(type, type)>("bitxor", exec);
    auto bnot = lookup<type(type)>("bitnot", exec);
    auto shl = lookup<type(type, type)>("shl", exec);
    auto shr = lookup<type(type, type)>("shr", exec);
    auto rol = lookup<type(type, type)>("rol", exec);
    auto ror = lookup<type(type, type)>("ror", exec);

    ASSERT_EQUAL(band(0b1110, 0b1001), 0b1000);
    ASSERT_EQUAL(bor(0b000101, 0b101100), 0b101101);
    ASSERT_EQUAL(bxor(0b1011, 0b1101), 0b0110);
    ASSERT_EQUAL(bnot(-1), 0);
    ASSERT_EQUAL(shl(5, 2), 20);
    ASSERT_EQUAL(shr(48, 3), 6);
    ASSERT_EQUAL(rol(7, 3), ::rol<type>(7, 3));
    ASSERT_EQUAL(ror(-42, 3), ::ror<type>(-42, 3));
)

FOR_EACH_INT(DEFINE_TEST, codegen_compare_simple,
    auto instance = COMPILE_SUBST(R"(
bool less($T a, $T b):
    a < b
bool lequal($T a, $T b):
    a <= b
bool greater($T a, $T b):
    a > b
bool gequal($T a, $T b):
    a >= b
bool equal($T a, $T b):
    a == b
bool inequal($T a, $T b):
    a != b
)");
    auto exec = load(instance.artifact);
    auto less = lookup<bool(type, type)>("less", exec);
    auto lequal = lookup<bool(type, type)>("lequal", exec);
    auto greater = lookup<bool(type, type)>("greater", exec);
    auto gequal = lookup<bool(type, type)>("gequal", exec);
    auto equal = lookup<bool(type, type)>("equal", exec);
    auto inequal = lookup<bool(type, type)>("inequal", exec);

    ASSERT_EQUAL(less(43, 42), false);
    ASSERT_EQUAL(less(42, 43), true);
    ASSERT_EQUAL(lequal(42, 41), false);
    ASSERT_EQUAL(lequal(41, 42), true);
    ASSERT_EQUAL(greater(-1, 0), false);
    ASSERT_EQUAL(greater(-42, -43), true);
    ASSERT_EQUAL(gequal(-43, -42), false);
    ASSERT_EQUAL(gequal(-42, -42), true);
    ASSERT_EQUAL(equal(42, 43), false);
    ASSERT_EQUAL(equal(-42, -42), true);
    ASSERT_EQUAL(inequal(42, 42), false);
    ASSERT_EQUAL(inequal(-42, 42), true);
)

FOR_EACH_INT(DEFINE_TEST, codegen_compare_branch_simple,
    auto instance = COMPILE_SUBST(R"(
bool less($T a, $T b):
    true if a < b else false
bool lequal($T a, $T b):
    true if a <= b else false
bool greater($T a, $T b):
    true if a > b else false
bool gequal($T a, $T b):
    true if a >= b else false
bool equal($T a, $T b):
    true if a == b else false
bool inequal($T a, $T b):
    true if a != b else false
)");
    auto exec = load(instance.artifact);
    auto less = lookup<bool(type, type)>("less", exec);
    auto lequal = lookup<bool(type, type)>("lequal", exec);
    auto greater = lookup<bool(type, type)>("greater", exec);
    auto gequal = lookup<bool(type, type)>("gequal", exec);
    auto equal = lookup<bool(type, type)>("equal", exec);
    auto inequal = lookup<bool(type, type)>("inequal", exec);

    ASSERT_EQUAL(less(43, 42), false);
    ASSERT_EQUAL(less(42, 43), true);
    ASSERT_EQUAL(lequal(42, 41), false);
    ASSERT_EQUAL(lequal(41, 42), true);
    ASSERT_EQUAL(greater(-1, 0), false);
    ASSERT_EQUAL(greater(-42, -43), true);
    ASSERT_EQUAL(gequal(-43, -42), false);
    ASSERT_EQUAL(gequal(-42, -42), true);
    ASSERT_EQUAL(equal(42, 43), false);
    ASSERT_EQUAL(equal(-42, -42), true);
    ASSERT_EQUAL(inequal(42, 42), false);
    ASSERT_EQUAL(inequal(-42, 42), true);
)

FOR_EACH_INT(DEFINE_TEST, codegen_loop_break,
    auto instance = COMPILE_SUBST(R"(
$T break_always():
    var i: 0
    while i < 10:
        i = i + 1
        break
    return i

$T break_conditionally($T cond):
    var result: 0
    while result < 42:
        if cond == 0:
            result = 43
            break
        result = result + 1
    return result

$T break_both_branches($T cond):
    var result: 0
    while true:
        if cond == 0:
            result = 42
            break
        else:
            result = 43
            break
    return result

$T break_nested_loop():
    var result: 0
    until false:
        break if result >= 42
        while true:
            break if result > 20
            until false:
                break if result > 10
                result = result + 1
            result = result + 1
        result = result + 1
    return result
)");

    auto exec = load(instance.artifact);
    auto break_always = lookup<type()>("break_always", exec);
    auto break_conditionally = lookup<type(type)>("break_conditionally", exec);
    auto break_both_branches = lookup<type(type)>("break_both_branches", exec);
    auto break_nested_loop = lookup<type()>("break_nested_loop", exec);

    ASSERT_EQUAL(break_always(), 1);
    ASSERT_EQUAL(break_conditionally(0), 43);
    ASSERT_EQUAL(break_conditionally(1), 42);
    ASSERT_EQUAL(break_both_branches(0), 42);
    ASSERT_EQUAL(break_both_branches(1), 43);
    ASSERT_EQUAL(break_nested_loop(), 42);
)

FOR_EACH_INT(DEFINE_TEST, codegen_loop_forwards_backwards,
    auto instance = COMPILE_SUBST(R"(
$T while_forward($T i, $T n):
    $T result: 0
    while i < n:
        i = i + 1
        result = result + i
    return result

$T while_backward($T i, $T n):
    $T result: 0
    while i >= n:
        result = result + i
        i = i - 1
    return result

$T until_forward($T i, $T n):
    $T result: 0
    until i >= n:
        i = i + 1
        result = result + i
    return result

$T until_backward($T i, $T n):
    $T result: 0
    until i < n:
        result = result + i
        i = i - 1
    return result
)");
    auto exec = load(instance.artifact);
    auto while_forward = lookup<type(type, type)>("while_forward", exec);
    auto while_backward = lookup<type(type, type)>("while_backward", exec);
    auto until_forward = lookup<type(type, type)>("until_forward", exec);
    auto until_backward = lookup<type(type, type)>("until_backward", exec);
    ASSERT_EQUAL(while_forward(0, 10), 55);
    ASSERT_EQUAL(while_backward(10, 0), 55);
    ASSERT_EQUAL(until_forward(0, 10), 55);
    ASSERT_EQUAL(until_backward(10, 0), 55);
)

FOR_EACH_INT(DEFINE_TEST, codegen_get_index,
    auto instance = COMPILE_SUBST(R"(
$T sum_slice($T[] xs):
    $T sum: 0
    u32 i: 0
    while i < |xs|:
        sum = sum + xs[i]
        i = i + 1
    return sum

$T sum_array($T[8] xs, u32 i, u32 n):
    $T sum: 0
    while i < n and i < |xs|:
        sum = sum + xs[i]
        i = i + 1
    return sum
)");
    auto exec = load(instance.artifact);
    auto sum_slice = lookup<type(const_slice<type>)>("sum_slice", exec);
    auto sum_array = lookup<type(array<type, 8>, u32, u32)>("sum_array", exec);

    array<type, 8> nums;
    for (u32 i = 0; i < 8; i ++)
        nums[i] = i;
    ASSERT_EQUAL(sum_slice(nums[{0, 0}]), 0);
    ASSERT_EQUAL(sum_slice(nums[{1, 5}]), 10);
    ASSERT_EQUAL(sum_slice(nums), 28);

    ASSERT_EQUAL(sum_array(nums, 0, 0), 0);
    ASSERT_EQUAL(sum_array(nums, 1, 5), 10);
    ASSERT_EQUAL(sum_array(nums, 0, 8), 28);
)

FOR_EACH_INT(DEFINE_TEST, codegen_swap,
    auto instance = COMPILE_SUBST(R"(
void swap($T* x, $T* y):
    var t: *x
    *x = *y
    *y = t

void xorswap($T* x, $T* y):
    *x = *x ^ *y
    *y = *x ^ *y
    *x = *x ^ *y
)");
    auto exec = load(instance.artifact);
    auto swap = lookup<void(type*, type*)>("swap", exec);
    auto xorswap = lookup<void(type*, type*)>("xorswap", exec);
    type x = 42, y = 43;
    swap(&x, &y);
    ASSERT_EQUAL(x, 43);
    ASSERT_EQUAL(y, 42);

    xorswap(&x, &y);
    ASSERT_EQUAL(x, 42);
    ASSERT_EQUAL(y, 43);
)

FOR_EACH_INT(DEFINE_TEST, codegen_set_index,
    auto instance = COMPILE_SUBST(R"(
void copy_slice($T[] dst, i32 i, $T[] src, i32 j, i32 n):
    var k: 0
    while k < n:
        dst[i + k] = src[j + k]
        k = k + 1

void transpose($T[4][4]* mat):
    var i: 0
    while i < 4:
        var j: 0
        while j < i:
            var tmp: mat[i][j]
            mat[i][j] = mat[j][i]
            mat[j][i] = tmp
            j = j + 1
        i = i + 1
)");
    auto exec = load(instance.artifact);
    auto copy_slice = lookup<void(slice<type>, i32, slice<type>, i32, i32)>("copy_slice", exec);
    auto transpose = lookup<void(array<array<type, 4>, 4>*)>("transpose", exec);

    type srcNums[4] = { 1, 2, 3, 4 };
    type dstNums[4];
    copy_slice({ dstNums, 4 }, 0, { srcNums, 4 }, 2, 2);
    copy_slice({ dstNums, 4 }, 2, { srcNums, 4 }, 0, 2);
    ASSERT_EQUAL(dstNums[0], 3);
    ASSERT_EQUAL(dstNums[1], 4);
    ASSERT_EQUAL(dstNums[2], 1);
    ASSERT_EQUAL(dstNums[3], 2);

    array<array<type, 4>, 4> matrix;
    for (u32 i = 0; i < 4; i ++) for (u32 j = 0; j < 4; j ++)
        matrix[i][j] = 0;
    for (u32 i = 0; i < 4; i ++)
        matrix[i][i] = 1;

    transpose(&matrix);
    ASSERT_EQUAL(matrix[0][0], 1);
    ASSERT_EQUAL(matrix[1][1], 1);
    ASSERT_EQUAL(matrix[2][2], 1);
    ASSERT_EQUAL(matrix[3][3], 1);

    matrix[3][0] = 42;
    matrix[0][3] = 43;
    transpose(&matrix);
    ASSERT_EQUAL(matrix[0][3], 42);
    ASSERT_EQUAL(matrix[3][0], 43);
)

FOR_EACH_INT(DEFINE_TEST, codegen_get_slice,
    auto instance = COMPILE_SUBST(R"(
$T[] as_slice($T[8]* xs):
    return xs[:]

$T[] take_arr($T[8]* xs, u32 n):
    return xs[:n]

$T[] drop_arr($T[8]* xs, u32 n):
    return xs[n:]

$T[] slice_arr($T[8]* xs, u32 i, u32 j):
    return xs[i:j]

$T[] identity($T[] xs):
    return xs[:]

$T[] take($T[] xs, u32 n):
    return xs[:n]

$T[] drop($T[] xs, u32 n):
    return xs[n:]

$T[] slice($T[] xs, u32 i, u32 j):
    return xs[i:j]
)");
    auto exec = load(instance.artifact);
    auto as_slice = lookup<slice<type>(array<type, 8>*)>("as_slice", exec);
    auto take_arr = lookup<slice<type>(array<type, 8>*, u32)>("take_arr", exec);
    auto drop_arr = lookup<slice<type>(array<type, 8>*, u32)>("drop_arr", exec);
    auto slice_arr = lookup<slice<type>(array<type, 8>*, u32, u32)>("slice_arr", exec);

    auto identity = lookup<slice<type>(slice<type>)>("identity", exec);
    auto take = lookup<slice<type>(slice<type>, u32)>("take", exec);
    auto drop = lookup<slice<type>(slice<type>, u32)>("drop", exec);
    auto slice_fn = lookup<slice<type>(slice<type>, u32, u32)>("slice", exec);

    array<type, 8> nums;
    for (u32 i = 0; i < 8; i ++)
        nums[i] = i;
    slice<type> slice = nums[{0, 8}];

    ASSERT_EQUAL(as_slice(&nums).size(), 8);
    ASSERT_EQUAL(as_slice(&nums)[4], 4);

    ASSERT_EQUAL(take_arr(&nums, 3).size(), 3);
    ASSERT_EQUAL(take_arr(&nums, 3)[2], 2);

    ASSERT_EQUAL(drop_arr(&nums, 5).size(), 3);
    ASSERT_EQUAL(drop_arr(&nums, 5)[0], 5);

    ASSERT_EQUAL(slice_arr(&nums, 2, 7).size(), 5);
    ASSERT_EQUAL(slice_arr(&nums, 2, 7)[0], 2);
    ASSERT_EQUAL(slice_arr(&nums, 2, 7)[4], 6);

    ASSERT_EQUAL(identity(slice).size(), 8);
    ASSERT_EQUAL(identity(slice)[4], 4);

    ASSERT_EQUAL(take(slice, 3).size(), 3);
    ASSERT_EQUAL(take(slice, 3)[2], 2);

    ASSERT_EQUAL(drop(slice, 5).size(), 3);
    ASSERT_EQUAL(drop(slice, 5)[0], 5);

    ASSERT_EQUAL(slice_fn(slice, 2, 7).size(), 5);
    ASSERT_EQUAL(slice_fn(slice, 2, 7)[0], 2);
    ASSERT_EQUAL(slice_fn(slice, 2, 7)[4], 6);
)

FOR_EACH_INT(DEFINE_TEST, codegen_global_scalar,
    auto instance = COMPILE_SUBST(R"(
$T global: 42

$T get_global():
    return global

void set_global($T val):
    global = val

void inc_global():
    global = global + 1

$T* addr_global():
    return &global
)");
    auto exec = load(instance.artifact);
    auto get_global = lookup<i64()>("get_global", exec);
    auto set_global = lookup<void(type)>("set_global", exec);
    auto inc_global = lookup<void()>("inc_global", exec);
    auto addr_global = lookup<type*()>("addr_global", exec);

    ASSERT_EQUAL(get_global(), 42);
    set_global(43);
    ASSERT_EQUAL(get_global(), 43);
    inc_global();
    ASSERT_EQUAL(get_global(), 44);
    type* ptr = addr_global();
    ASSERT_EQUAL(*ptr, 44);
    (*ptr) ++;
    ASSERT_EQUAL(get_global(), 45);
)

FOR_EACH_INT(DEFINE_TEST, codegen_global_array,
    auto instance = COMPILE_SUBST(R"(
$T[7] numbers: [1, 2, 3, 4, 5, 6, 7]

$T get_number(i32 i):
    return numbers[i]
void set_number(i32 i, $T n):
    numbers[i] = n
$T* addr_number(i32 i):
    return &numbers[i]
)");

    auto exec = load(instance.artifact);
    auto get_number = lookup<i64(i32)>("get_number", exec);
    auto set_number = lookup<void(i32, type)>("set_number", exec);
    auto addr_number = lookup<type*(i32)>("addr_number", exec);

    for (u32 i = 0; i < 7; i ++)
        set_number(i, get_number(i) + 1);
    for (u32 i = 0; i < 7; i ++)
        (*addr_number(i)) += 1;
    type sum = 0;
    for (u32 i = 0; i < 7; i ++)
        sum += get_number(i);
    ASSERT_EQUAL(sum, 42);
)

FOR_EACH_INT(DEFINE_TEST, codegen_global_slice,
    auto instance = COMPILE_SUBST(R"(
$T[] numbers: [1, 2, 3, 4, 5, 6, 7]

$T get_number(i32 i):
    return numbers[i]
void set_number(i32 i, $T n):
    numbers[i] = n
$T[] sub_slice(i32 i, i32 j):
    return numbers[i:j]
)");

    auto exec = load(instance.artifact);
    auto get_number = lookup<i64(i32)>("get_number", exec);
    auto set_number = lookup<void(i32, type)>("set_number", exec);
    auto sub_slice = lookup<slice<type>(i32, i32)>("sub_slice", exec);

    for (u32 i = 0; i < 7; i ++) {
        ASSERT_EQUAL(get_number(i), i + 1);
        set_number(i, get_number(i) + 1);
    }

    auto first_half = sub_slice(0, 4);
    ASSERT_EQUAL(first_half.size(), 4);
    for (u32 i = 0; i < first_half.size(); i ++)
        first_half[i] ++;

    auto second_half = sub_slice(4, 7);
    ASSERT_EQUAL(second_half.size(), 3);
    for (u32 i = 0; i < second_half.size(); i ++)
        second_half[i] ++;

    type sum = 0;
    auto full_slice = sub_slice(0, 7);
    for (u32 i = 0; i < full_slice.size(); i ++)
        sum += full_slice[i];
    ASSERT_EQUAL(sum, 42);
)

FOR_EACH_INT(DEFINE_TEST, codegen_apply_func_to_array,
    auto instance = COMPILE_SUBST(R"(
void map($T($T) f, $T[] input, $T[] out):
    var i: 0
    while i < |input| and i < |out|:
        out[i] = f(input[i])
        i = i + 1

void map_in_place($T($T) f, $T[] inout):
    var i: 0
    while i < |inout|:
        inout[i] = f(inout[i])
        i = i + 1

$T fold($T($T, $T) f, $T[] input, $T x):
    var i: 0
    while i < |input|:
        x = f(x, input[i])
        i = i + 1
    return x

$T add($T x, $T y):
    x + y

$T sum($T[] input):
    return fold(add, input, 0)
)");

    auto exec = load(instance.artifact);
    auto map = lookup<void(type(type), const_slice<type>, slice<type>)>("map", exec);
    auto map_in_place = lookup<void(type(type), slice<type>)>("map_in_place", exec);
    auto fold = lookup<type(type(type, type), const_slice<type>, type)>("fold", exec);
    auto sum = lookup<type(const_slice<type>)>("sum", exec);

    auto inc = [](type x) -> type { return x + 1; };
    auto mul = [](type x, type y) -> type { return x * y; };

    array<type, 8> input;
    for (u32 i = 0; i < 8; i ++)
        input[i] = i;

    array<type, 8> output;
    map(inc, input, output);
    for (u32 i = 0; i < 8; i ++)
        ASSERT_EQUAL(output[i], i + 1);
    map_in_place(inc, output[{0, 4}]);
    for (u32 i = 0; i < 4; i ++)
        ASSERT_EQUAL(output[i], i + 2);
    for (u32 i = 4; i < 8; i ++)
        ASSERT_EQUAL(output[i], i + 1);

    ASSERT_EQUAL(fold(mul, input[{1, 2}], 1), 1);
    ASSERT_EQUAL(fold(mul, input[{1, 5}], 1), 24);
    ASSERT_EQUAL(fold(mul, input[{6, 8}], 1), 42);

    ASSERT_EQUAL(sum(input[{0, 5}]), 10);
    ASSERT_EQUAL(sum(input[{3, 8}]), 25);
)

FOR_EACH_INT(DEFINE_TEST, codegen_arith_assignment,
    auto instance = COMPILE_SUBST(R"(
void add_eq($T* x, $T y):
    *x += y
void sub_eq($T* x, $T y):
    *x -= y
void mul_eq($T* x, $T y):
    *x *= y
void div_eq($T* x, $T y):
    *x /= y
void rem_eq($T* x, $T y):
    *x %= y
)");
    auto exec = load(instance.artifact);
    auto add_eq = lookup<void(type*, type)>("add_eq", exec);
    auto sub_eq = lookup<void(type*, type)>("sub_eq", exec);
    auto mul_eq = lookup<void(type*, type)>("mul_eq", exec);
    auto div_eq = lookup<void(type*, type)>("div_eq", exec);
    auto rem_eq = lookup<void(type*, type)>("rem_eq", exec);

    type val = 42;
    add_eq(&val, 1);
    ASSERT_EQUAL(val, 43);

    sub_eq(&val, 1);
    ASSERT_EQUAL(val, 42);

    mul_eq(&val, 2);
    ASSERT_EQUAL(val, 84);

    div_eq(&val, 3);
    ASSERT_EQUAL(val, 28);

    rem_eq(&val, 5);
    ASSERT_EQUAL(val, 3);
)

FOR_EACH_INT(DEFINE_TEST, codegen_compound_assignment_local,
    auto instance = COMPILE_SUBST(R"(
$T add_eq($T x, $T y):
    x += y
    return x
$T sub_eq($T x, $T y):
    x -= y
    return x
$T mul_eq($T x, $T y):
    x *= y
    return x
$T div_eq($T x, $T y):
    x /= y
    return x
$T rem_eq($T x, $T y):
    x %= y
    return x
)");
    auto exec = load(instance.artifact);
    auto add_eq = lookup<type(type, type)>("add_eq", exec);
    auto sub_eq = lookup<type(type, type)>("sub_eq", exec);
    auto mul_eq = lookup<type(type, type)>("mul_eq", exec);
    auto div_eq = lookup<type(type, type)>("div_eq", exec);
    auto rem_eq = lookup<type(type, type)>("rem_eq", exec);

    ASSERT_EQUAL(add_eq(6, 7), 13);
    ASSERT_EQUAL(sub_eq(13, 6), 7);
    ASSERT_EQUAL(mul_eq(6, 7), 42);
    ASSERT_EQUAL(div_eq(42, 6), 7);
    ASSERT_EQUAL(rem_eq(42, 5), 2);
)

FOR_EACH_INT(DEFINE_TEST, codegen_bitwise_assignment,
    auto instance = COMPILE_SUBST(R"(
void bitand_eq($T* x, $T y):
    *x &= y
void bitor_eq($T* x, $T y):
    *x |= y
void bitxor_eq($T* x, $T y):
    *x ^= y
void bitshl_eq($T* x, $T y):
    *x <<= y
void bitshr_eq($T* x, $T y):
    *x >>= y
void bitrol_eq($T* x, $T y):
    *x /<= y
void bitror_eq($T* x, $T y):
    *x />= y
)");
    auto exec = load(instance.artifact);
    auto bitand_eq = lookup<void(type*, type)>("bitand_eq", exec);
    auto bitor_eq = lookup<void(type*, type)>("bitor_eq", exec);
    auto bitxor_eq = lookup<void(type*, type)>("bitxor_eq", exec);
    auto bitshl_eq = lookup<void(type*, type)>("bitshl_eq", exec);
    auto bitshr_eq = lookup<void(type*, type)>("bitshr_eq", exec);
    auto bitrol_eq = lookup<void(type*, type)>("bitrol_eq", exec);
    auto bitror_eq = lookup<void(type*, type)>("bitror_eq", exec);

    type val = 0b1010101;
    bitand_eq(&val, 0b11111);
    ASSERT_EQUAL(val, 0b10101);

    bitor_eq(&val, 0b00010);
    ASSERT_EQUAL(val, 0b10111);

    bitxor_eq(&val, 0b01110);
    ASSERT_EQUAL(val, 0b11001);

    bitshl_eq(&val, 2);
    ASSERT_EQUAL(val, 0b01100100);

    bitshr_eq(&val, 3);
    ASSERT_EQUAL(val, 0b01100);

    bitrol_eq(&val, 7);
    bitror_eq(&val, 7);
    ASSERT_EQUAL(val, 0b01100);
)

FOR_EACH_INT(DEFINE_TEST, codegen_incr_decr,
    auto instance = COMPILE_SUBST(R"(
$T pre_incr($T* x):
    return ++ *x
$T post_incr($T* x):
    return *x ++
$T pre_decr($T* x):
    return -- *x
$T post_decr($T* x):
    return *x --
)");
    auto exec = load(instance.artifact);
    auto pre_incr = lookup<type(type*)>("pre_incr", exec);
    auto post_incr = lookup<type(type*)>("post_incr", exec);
    auto pre_decr = lookup<type(type*)>("pre_decr", exec);
    auto post_decr = lookup<type(type*)>("post_decr", exec);

    type val = 42;
    ASSERT_EQUAL(pre_incr(&val), 43);
    ASSERT_EQUAL(val, 43);

    ASSERT_EQUAL(post_incr(&val), 43);
    ASSERT_EQUAL(val, 44);

    ASSERT_EQUAL(pre_decr(&val), 43);
    ASSERT_EQUAL(val, 43);

    ASSERT_EQUAL(post_decr(&val), 43);
    ASSERT_EQUAL(val, 42);
)

FOR_EACH_INT(DEFINE_TEST, codegen_incr_decr_local,
    auto instance = COMPILE_SUBST(R"(
$T pre_incr($T x):
    return ++ x
$T post_incr($T x):
    return x ++
$T pre_decr($T x):
    return -- x
$T post_decr($T x):
    return x --
)");
    auto exec = load(instance.artifact);
    auto pre_incr = lookup<type(type)>("pre_incr", exec);
    auto post_incr = lookup<type(type)>("post_incr", exec);
    auto pre_decr = lookup<type(type)>("pre_decr", exec);
    auto post_decr = lookup<type(type)>("post_decr", exec);

    ASSERT_EQUAL(pre_incr(42), 43);
    ASSERT_EQUAL(post_incr(42), 42);
    ASSERT_EQUAL(pre_decr(43), 42);
    ASSERT_EQUAL(post_decr(43), 43);
)

FOR_EACH_INT(DEFINE_TEST, codegen_hailstone_two_ways,
    auto instance = COMPILE_SUBST(R"(
$T hail_iter($T x):
    while x != 1:
        if x % 2 == 0:
            x /= 2
        else:
            x = 3x + 1
    return x

$T hail_rec($T x):
    return x if x == 1
    return hail_rec(x / 2) if x % 2 == 0
    return hail_rec(3x + 1)
)");
    auto exec = load(instance.artifact);
    auto hail_iter = lookup<type(type)>("hail_iter", exec);
    auto hail_rec = lookup<type(type)>("hail_rec", exec);

    ASSERT_EQUAL(hail_iter(8), 1);
    ASSERT_EQUAL(hail_rec(8), 1);
    ASSERT_EQUAL(hail_iter(11), 1);
    ASSERT_EQUAL(hail_rec(11), 1);
    ASSERT_EQUAL(hail_iter(37), 1);
    ASSERT_EQUAL(hail_rec(37), 1);
)

FOR_EACH_INT(DEFINE_TEST, codegen_if_else_with_result,
    auto instance = COMPILE_SUBST(R"(
$T pick($T x, $T y, bool b):
    return x if b else y

$T pick4($T a, $T b, $T c, $T d, bool cond1, bool cond2):
    return (a if cond2 else b) if cond1 else (c if cond2 else d)
)");

    auto exec = load(instance.artifact);
    auto pick = lookup<type(type, type, bool)>("pick", exec);
    auto pick4 = lookup<type(type, type, type, type, bool, bool)>("pick4", exec);

    ASSERT_EQUAL(pick(42, 43, true), 42);
    ASSERT_EQUAL(pick(42, 43, false), 43);

    ASSERT_EQUAL(pick4(42, 43, 44, 45, true, true), 42);
    ASSERT_EQUAL(pick4(42, 43, 44, 45, true, false), 43);
    ASSERT_EQUAL(pick4(42, 43, 44, 45, false, true), 44);
    ASSERT_EQUAL(pick4(42, 43, 44, 45, false, false), 45);
)

FOR_EACH_INT(DEFINE_TEST, codegen_get_local_field,
    auto instance = COMPILE_SUBST(R"(
type Foo:
    $T x, y, z
type Bar:
    $T a, b
    Foo foo

$T get_field(i32 i):
    var foo: Foo(1, 2, 3)
    return foo.x if i == 0
    return foo.y if i == 1
    return foo.z if i == 2
    return -1

$T get_nested_field(i32 i):
    var bar: Bar(1, 2, Foo(3, 4, 5))
    return bar.a if i == 0
    return bar.b if i == 1
    return bar.foo.x if i == 2
    return bar.foo.y if i == 3
    return bar.foo.z if i == 4
    return -1
)");

    auto exec = load(instance.artifact);
    auto get_field = lookup<type(i32)>("get_field", exec);
    auto get_nested_field = lookup<type(i32)>("get_nested_field", exec);

    ASSERT_EQUAL(get_field(0), 1);
    ASSERT_EQUAL(get_field(1), 2);
    ASSERT_EQUAL(get_field(2), 3);
    ASSERT_EQUAL(get_field(3), -1);

    ASSERT_EQUAL(get_nested_field(0), 1);
    ASSERT_EQUAL(get_nested_field(1), 2);
    ASSERT_EQUAL(get_nested_field(2), 3);
    ASSERT_EQUAL(get_nested_field(3), 4);
    ASSERT_EQUAL(get_nested_field(4), 5);
    ASSERT_EQUAL(get_nested_field(5), -1);
)

FOR_EACH_INT(DEFINE_TEST, codegen_static_linked_list,
    auto instance = COMPILE_SUBST(R"(
type List:
    $T item
    List* next

var last: List(3, &last)
var middle: List(2, &last)
var first: List(1, &middle)

List* get_list(): &first

$T sum_list(List* list):
    if list.next == list:
        return list.item
    return list.item + sum_list(list.next)

$T get_sum():
    return first.sum_list()
)");

    auto exec = load(instance.artifact);
    auto get_list = lookup<void*()>("get_list", exec);
    auto sum_list = lookup<type(void*)>("sum_list", exec);
    auto get_sum = lookup<type()>("get_sum", exec);

    void* list = get_list();
    ASSERT_EQUAL(sum_list(list), 6);

    ASSERT_EQUAL(get_sum(), 6);
);

FOR_EACH_INT(DEFINE_TEST, codegen_quick_sort,
    auto instance = COMPILE_SUBST(R"(
void swap($T* a, $T* b):
  $T t: *a
  *a = *b
  *b = t

void cmpswap($T* a, $T* b):
    swap(a, b) if *b < *a

void insort($T[] xs):
    var i: 1
    while i < |xs|:
        var j: i ++
        swap(&xs[j - 1], &xs[j --]) until j == 0 or xs[j - 1] < xs[j]

void qsort($T[] xs):
    var n: |xs|
    return if n <= 1
    if n <= 2:
        cmpswap(&xs[0], &xs[1])
    else if n <= 3:
        cmpswap(&xs[0], &xs[2])
        cmpswap(&xs[0], &xs[1])
        cmpswap(&xs[1], &xs[2])
    else if n <= 4:
        cmpswap(&xs[0], &xs[2])
        cmpswap(&xs[1], &xs[3])
        cmpswap(&xs[0], &xs[1])
        cmpswap(&xs[2], &xs[3])
        cmpswap(&xs[1], &xs[2])
    else if n <= 5:
        cmpswap(&xs[0], &xs[3])
        cmpswap(&xs[1], &xs[4])
        cmpswap(&xs[0], &xs[2])
        cmpswap(&xs[1], &xs[3])
        cmpswap(&xs[0], &xs[1])
        cmpswap(&xs[2], &xs[4])
        cmpswap(&xs[1], &xs[2])
        cmpswap(&xs[3], &xs[4])
        cmpswap(&xs[2], &xs[3])
    else if n <= 32:
        insort(xs)
    else:
        var mid: n / 2
        swap(&xs[0], &xs[mid]) if xs[mid] < xs[0]
        swap(&xs[0], &xs[n - 1]) if xs[n - 1] < xs[0]
        swap(&xs[mid], &xs[n - 1]) if xs[n - 1] < xs[mid]
        var item: xs[mid], pivot: 0
        while true:
            var a: 0, b: n - 1
            a ++ while xs[a] < item
            b -- while item < xs[b]
            if a >= b:
                pivot = b then break
            swap(&xs[a], &xs[b])
        qsort(xs[0:pivot + 1])
        qsort(xs[pivot + 1:n])
)");

    auto exec = load(instance.artifact);
    auto insort = lookup<void(slice<type>)>("insort", exec);
    auto qsort = lookup<void(slice<type>)>("qsort", exec);

    {
        array<type, 100> numbers;
        for (type i = 0; i < 100; i ++)
            numbers[i] = 100 - i;

        insort(numbers);

        for (type i = 0; i < 100; i ++)
            ASSERT_EQUAL(numbers[i], i + 1);
    }
    {
        array<type, 100> numbers;
        for (type i = 0; i < 100; i ++)
            numbers[i] = 100 - i;

        qsort(numbers);

        for (type i = 0; i < 100; i ++)
            ASSERT_EQUAL(numbers[i], i + 1);
    }
)

FOR_EACH_INT(DEFINE_TEST, codegen_exp,
    auto instance = COMPILE_SUBST(R"(
$T square($T x):
    x ** 2

$T pow($T x, $T p):
    x ** p

$T pow_eq($T x, $T p):
    x **= p
    return x
)");

    auto exec = load(instance.artifact);
    auto square = lookup<type(type)>("square", exec);
    auto pow = lookup<type(type, type)>("pow", exec);
    auto pow_eq = lookup<type(type, type)>("pow_eq", exec);

    ASSERT_EQUAL(square(2), 4);
    ASSERT_EQUAL(square(9), 81);
    ASSERT_EQUAL(square(-8), 64);

    ASSERT_EQUAL(pow(-3, 3), -27);
    ASSERT_EQUAL(pow(5, 3), 125);

    ASSERT_EQUAL(pow_eq(-3, 3), -27);
    ASSERT_EQUAL(pow_eq(5, 3), 125);
)

FOR_EACH_UINT(DEFINE_TEST, codegen_exp,
    auto instance = COMPILE_SUBST(R"(
$T square($T x):
    x ** 2

$T pow($T x, $T p):
    x ** p

$T pow_eq($T x, $T p):
    x **= p
    return x
)");

    auto exec = load(instance.artifact);
    auto square = lookup<type(type)>("square", exec);
    auto pow = lookup<type(type, type)>("pow", exec);
    auto pow_eq = lookup<type(type, type)>("pow_eq", exec);

    ASSERT_EQUAL(square(2), 4);
    ASSERT_EQUAL(square(9), 81);
    ASSERT_EQUAL(square(12), 144);

    ASSERT_EQUAL(pow(3, 3), 27);
    ASSERT_EQUAL(pow(6, 3), 216);

    ASSERT_EQUAL(pow_eq(3, 3), 27);
    ASSERT_EQUAL(pow_eq(6, 3), 216);
)

FOR_EACH_FLOAT(DEFINE_TEST, codegen_exp,
    auto instance = COMPILE_SUBST(R"(
$T square($T x):
    x ** 2

$T pow($T x, $T p):
    x ** p

$T pow_eq($T x, $T p):
    x **= p
    return x
)");

    auto exec = load(instance.artifact);
    auto square = lookup<type(type)>("square", exec);
    auto pow = lookup<type(type, type)>("pow", exec);
    auto pow_eq = lookup<type(type, type)>("pow_eq", exec);

    ASSERT_FLOAT_EQUAL(square(2.0), 4.0);
    ASSERT_FLOAT_EQUAL(square(9.0), 81.0);
    ASSERT_FLOAT_EQUAL(square(0.5), 0.25);

    ASSERT_FLOAT_EQUAL(pow(3.0, 3.0), 27.0);
    ASSERT_FLOAT_EQUAL(pow(144.0, 0.5), 12.0);

    ASSERT_FLOAT_EQUAL(pow_eq(144.0, 0.5), 12.0);
    ASSERT_FLOAT_EQUAL(pow_eq(3.0, 3.0), 27.0);
)

FOR_EACH_INT(DEFINE_TEST, codegen_new,
    auto instance = COMPILE_SUBST(R"(
own $T* makeInt($T x):
    new x

type Pair:
    $T x, y
own Pair* makePair($T x, $T y):
    new Pair(x, y)
)");

    auto exec = load(instance.artifact);
    auto makeInt = lookup<type*(type)>("makeInt", exec);
    auto makePair = lookup<pair<type, type>*(type, type)>("makePair", exec);

    auto* i = makeInt(42);
    ASSERT_EQUAL(*i, 42);
    delete i;

    auto* p = makePair(42, 43);
    ASSERT_EQUAL(p->first, 42);
    ASSERT_EQUAL(p->second, 43);
    delete p;
)

FOR_EACH_INT(DEFINE_TEST, codegen_new_array,
    auto instance = COMPILE_SUBST(R"(
own $T[] makeNumbers($T n):
    var nums: new $T[n]
    var i: 0
    while i < |nums|:
        nums[i] = i
        i ++
    return own $T[](nums)
)");

    auto exec = load(instance.artifact);
    auto makeNumbers = lookup<slice<type>(type)>("makeNumbers", exec);

    auto a = makeNumbers(1);
    ASSERT_EQUAL(a.size(), 1);
    ASSERT_EQUAL(a[0], 0);
    delete a.data();

    auto b = makeNumbers(8);
    ASSERT_EQUAL(b.size(), 8);
    for (u32 i = 0; i < 8; i ++)
        ASSERT_EQUAL(b[i], i);
    delete b.data();

    auto c = makeNumbers(100);
    ASSERT_EQUAL(c.size(), 100);
    for (u32 i = 0; i < 100; i ++)
        ASSERT_EQUAL(c[i], i);
    delete c.data();
)

FOR_EACH_INT(DEFINE_TEST, codegen_integer_vector,
    auto instance = COMPILE_SUBST(R"(
type IntVector:
    own $T[] buffer
    $T size

own IntVector* makeVector():
    var buffer: new $T[4]
    $T i: 0
    while i < |buffer|:
        buffer[i ++] = 0
    new IntVector(own $T[](buffer), 0)

void grow(IntVector* vec):
    var new_buffer: new $T[|vec.buffer| * 2]
    $T i: 0
    while i < vec.size:
        new_buffer[i] = vec.buffer[i ++]
    vec.buffer = own $T[](new_buffer)

void push(IntVector* vec, $T i):
    if vec.size >= |vec.buffer|:
        vec.grow()
    vec.buffer[vec.size ++] = i

$T length(IntVector* vec):
    vec.size

$T at(IntVector* vec, $T i):
    vec.buffer[i]
)");

    auto exec = load(instance.artifact);
    auto makeVector = lookup<void*()>("makeVector", exec);
    auto grow = lookup<void(void*)>("grow", exec);
    auto push = lookup<void(void*, type)>("push", exec);
    auto at = lookup<type(void*, type)>("at", exec);
    auto length = lookup<type(void*)>("length", exec);

    auto v = makeVector();
    ASSERT_EQUAL(length(v), 0);

    push(v, 42);
    ASSERT_EQUAL(length(v), 1);
    ASSERT_EQUAL(at(v, 0), 42);

    push(v, 43);
    push(v, 44);
    push(v, 45);
    ASSERT_EQUAL(length(v), 4);
    ASSERT_EQUAL(at(v, 0), 42);
    ASSERT_EQUAL(at(v, 1), 43);
    ASSERT_EQUAL(at(v, 2), 44);
    ASSERT_EQUAL(at(v, 3), 45);

    push(v, 46);
    ASSERT_EQUAL(length(v), 5);
    ASSERT_EQUAL(at(v, 0), 42);
    ASSERT_EQUAL(at(v, 1), 43);
    ASSERT_EQUAL(at(v, 2), 44);
    ASSERT_EQUAL(at(v, 3), 45);
    ASSERT_EQUAL(at(v, 4), 46);
)

TEST(codegen_for_loop) {
    auto instance = COMPILE(R"(
var nums: [1, 2, 3, 4, 5, 6, 7, 8]
for i < |nums|, |nums| > j until j <= i:
    var t: nums[i]
    nums[i] = nums[j]
    nums[j] = t
)");
}

TEST(codegen_match_int) {
    auto instance = COMPILE(R"(
i32 fibonacci(i32 n):
    match n:
        case 0:
            0
        case 1:
            1
        else:
            fibonacci(n - 1) + fibonacci(n - 2)
)");

    auto exec = load(instance.artifact);
    auto fibonacci = lookup<i32(i32)>("fibonacci(i32)", exec);
    ASSERT_EQUAL(fibonacci(0), 0);
    ASSERT_EQUAL(fibonacci(1), 1);
    ASSERT_EQUAL(fibonacci(10), 55);
}

TEST(codegen_match_union) {
    auto instance = COMPILE(R"(
type List:
    case Cons:
        i32 value
        List* next
    case Nil
var nil: List.Nil()
var three: List.Cons(3, &nil)
var two: List.Cons(2, &three)
var one: List.Cons(1, &two)

i32 sum(List list):
    match list:
        case List.Cons(x, xs):
            x + sum(*xs)
        case List.Nil:
            0
)");

    auto exec = load(instance.artifact);
    auto list = lookup<triple<u32, i32, void*>>("one", exec);
    auto sum = lookup<i64(triple<u32, i32, void*>)>("sum(List)", exec);

    ASSERT_EQUAL(sum(*list), 6);
}

TEST(codegen_append_list) {
    auto instance = COMPILE(R"(
type List:
    case Cons:
        i32 value
        List* next
    case Nil

use List.*

own List* append(List* a, own List* b):
    match a:
        case Cons(x, xs):
            new Cons(x, xs.append(b))
        case Nil:
            b

own List* cons(i32 x, List* xs):
    new Cons(x, xs)

i32 car(List* l):
    match l:
        case Cons(x, xs): x
        case Nil: 0

List* cdr(List* l):
    match l:
        case Cons(x, xs): xs
        case Nil: Nil

var nil: Nil
)");

    auto exec = load(instance.artifact);
    auto nil = lookup<i32>("nil", exec);
    auto cons = lookup<void*(i32, void*)>("cons(i32,List*)", exec);
    auto car = lookup<i32(void*)>("car(List*)", exec);
    auto cdr = lookup<void*(void*)>("cdr(List*)", exec);
    auto append = lookup<void*(void*, void*)>("append(List*,List*)", exec);

    void* a = nil;
    for (i32 i = 20; i > 10; i --)
        a = cons(i, a);
    void* b = nil;
    for (i32 i = 10; i > 0; i --)
        b = cons(i, b);

    void* combined = append(b, a);
    for (i32 i = 1; i <= 20; i ++) {
        i32 x = car(combined);
        combined = cdr(combined);
        ASSERT_EQUAL(x, i);
    }

    ASSERT_EQUAL(combined, nil);
}

TEST(codegen_match_slice) {
    auto instance = COMPILE(R"(
i32 matchPrefix(i32[] numbers) match numbers:
    case [1, 2, 3, x, y]:
        x + y
    else:
        42

i32 sum(i32[] numbers):
    var result: 0
    result += numbers[i] for i < |numbers|
    return result

i32 matchSplat(i32[] numbers) match numbers:
    case [1, 2, 3, xs...]:
        sum(xs)
    else:
        42
)");

    auto exec = load(instance.artifact);
    auto matchPrefix = lookup<i32(slice<i32>)>("matchPrefix(i32[])", exec);
    auto matchSplat = lookup<i32(slice<i32>)>("matchSplat(i32[])", exec);
    auto matchInternal = lookup<i32(slice<i32>)>("matchInternal(i32[])", exec);

    array<i32, 5> arrayMatch;
    for (i32 i = 1; i <= 5; i ++)
        arrayMatch[i - 1] = i;
    array<i32, 5> arrayDoesntMatch = arrayMatch;
    arrayDoesntMatch[2] = 42;

    ASSERT_EQUAL(matchPrefix(arrayMatch), 9);
    ASSERT_EQUAL(matchPrefix(arrayDoesntMatch), 42);

    array<i32, 10> arrayToSum;
    for (i32 i = 1; i <= 10; i ++)
        arrayToSum[i - 1] = i;

    ASSERT_EQUAL(matchSplat(arrayMatch), 9);
    ASSERT_EQUAL(matchSplat(arrayDoesntMatch), 42);
    ASSERT_EQUAL(matchSplat(arrayToSum), 49);
}

TEST(codegen_match_tuple) {
    auto instance = COMPILE(R"(
type Vec3:
    i32 x, y, z

type Axis:
    case X
    case Y
    case Z

i32 abs(i32 x):
    x if x >= 0 else -x

Axis primaryAxis(Vec3 v) match v.xyz:
    case (1, 0, 0):
        Axis.X
    case (0, 1, 0):
        Axis.Y
    case (0, 0, 1):
        Axis.Z
    case (x, y, z):
        return Axis.X if abs(x) >= abs(y) and abs(x) >= abs(z)
        return Axis.Y if abs(y) >= abs(z)
        return Axis.Z

var xAxis: Axis.X
var yAxis: Axis.Y
var zAxis: Axis.Z
)");

    auto exec = load(instance.artifact);
    auto primaryAxis = lookup<i32(array<i32, 3>)>("primaryAxis(Vec3)", exec);
    auto xAxis = *lookup<i32>("xAxis", exec);
    auto yAxis = *lookup<i32>("yAxis", exec);
    auto zAxis = *lookup<i32>("zAxis", exec);

    array<i32, 3> xVec;
    xVec[0] = 1;
    xVec[1] = xVec[2] = 0;
    ASSERT_EQUAL(primaryAxis(xVec), xAxis);

    array<i32, 3> yVec;
    yVec[0] = yVec[2] = 0;
    yVec[1] = 1;
    ASSERT_EQUAL(primaryAxis(yVec), yAxis);

    array<i32, 3> zVec;
    zVec[0] = zVec[1] = 0;
    zVec[2] = 1;
    ASSERT_EQUAL(primaryAxis(zVec), zAxis);

    array<i32, 3> mystery;
    mystery[0] = 3;
    mystery[1] = -4;
    mystery[2] = 2;
    ASSERT_EQUAL(primaryAxis(mystery), yAxis);
}

TEST(codegen_bf_interpreter) {
    return;
    auto instance = COMPILE(R"(
i8[256] input: uninit, output: uninit
i32 inputIndex: 0, outputIndex: 0

i8 getc():
    input[inputIndex ++]

void putc(i8 b):
    output[outputIndex ++] = b

void bf(char[] program, i8[] memory, i32* pointer):
    while |program| > 0 match program[0]:
        case '>':
            *pointer ++
            program = program[1:]
        case '<':
            *pointer --
            program = program[1:]
        case '+':
            memory[*pointer] ++
            program = program[1:]
        case '-':
            memory[*pointer] --
            program = program[1:]
        case '.':
            putc(memory[*pointer])
            program = program[1:]
        case ',':
            memory[*pointer] = getc()
            program = program[1:]
        case '[':
            var nesting: 0, subprogram: program
            for i < |program|:
                if program[i] == '[':
                    nesting ++
                else if program[i] == ']' and -- nesting == 0:
                    subprogram = program[1:i]
                    break
            bf(subprogram, memory, pointer) while memory[*pointer] != 0
            program = program[2 + |subprogram|:]
)");

    auto exec = load(instance.artifact);
    auto& input = *lookup<array<i8, 256>>("input", exec);
    auto& output = *lookup<array<i8, 256>>("output", exec);
    auto bf = lookup<void(slice<rune>, slice<i8>, i32*)>("bf(char[],i8[],i32*)", exec);
    memory::fill(&input[0], 0, 256);
    memory::fill(&output[0], 0, 256);

    auto source = cstring("--[>--->->->++>-<<<<<-------]>--.>---------.>--..+++.>----.>+++++++++.<<.+++.------.<-.>>+.");
    array<rune, 256> program;
    u32 numRunes = utf8_decode(source.data(), source.size(), &program[0], 256).runes;
    ASSERT_EQUAL(numRunes, source.size());

    array<i8, 65536> memory;
    memory::fill(&memory[0], 0, 65536);
    i32 pointer = 0;
    bf({ &program[0], numRunes }, memory, &pointer);
    ASSERT_EQUAL(memory::compare(&output[0], "Hello world!", 12), 0);
}

TEST(codegen_call_overload) {
    auto instance = COMPILE(R"(
i32 foo(i32 x):
    x * 2
i32 foo(i32[] x):
    x[1]

i32[3] arr: [1, 2, 3]
i32 bar():
    arr.foo().foo()
)");

    auto exec = load(instance.artifact);
    auto bar = lookup<i64()>("bar()", exec);
    ASSERT_EQUAL(bar(), 4);
}

FOR_EACH_INT(DEFINE_TEST, codegen_local_array,
    auto instance = COMPILE_SUBST(R"(
$T[8] makeArray($T v):
    $T[8] arr
    arr[i] = v for i < 8
    return arr

$T accessArray($T v):
    var arr: makeArray(v)
    return arr[0] + arr[7]
)");

    auto exec = load(instance.artifact);
    auto accessArray = lookup<type(type)>("accessArray", exec);
    ASSERT_EQUAL(accessArray(2), 4);
)

TEST(codegen_string_literal) {
    auto instance = COMPILE(R"(
string global: "foo"
i8[3] local():
    "bar"

i32 strlen(string s):
    |s|

i8 maxbyte(string s):
    var max: 0
    for i < |s|:
        max = s[i] if s[i] > max
    return max

i32 test():
    strlen(local()) + strlen(global)
)");

    auto exec = load(instance.artifact);
    auto global = lookup<const_slice<i8>>("global", exec);
    auto local = lookup<array<i8, 3>()>("local()", exec);
    auto maxbyte = lookup<i8(const_slice<i8>)>("maxbyte(i8[])", exec);
    auto test = lookup<i32()>("test()", exec);

    ASSERT_EQUAL(maxbyte(*global), 'o');
    ASSERT_EQUAL(maxbyte(local()), 'r');
    ASSERT_EQUAL(test(), 6);
}

TEST(codegen_strcat_arrays) {
    auto instance = COMPILE(R"(
own i8[] strcat(i8[] a, i8[] b):
    var result: new i8[|a| + |b|]
    result[i] = a[i] for i < |a|
    result[|a| + i] = b[i] for i < |b|
    return own i8[](result)

own i8[] strcatSetSlice(i8[] a, i8[] b):
    var result: new i8[|a| + |b|]
    result[:|a|] = a
    result[|a|:] = b
    return own i8[](result)

own i8[] doStrcat():
    return strcat("cd", "efg")
)");

    auto exec = load(instance.artifact);
    auto strcat = lookup<const_slice<i8>(const_slice<i8>, const_slice<i8>)>("strcat(i8[],i8[])", exec);
    auto strcatSetSlice = lookup<const_slice<i8>(const_slice<i8>, const_slice<i8>)>("strcatSetSlice(i8[],i8[])", exec);
    auto doStrcat = lookup<const_slice<i8>()>("doStrcat()", exec);

    // ASSERT_EQUAL(strcat(cstring("a"), cstring("b")), cstring("ab"));
    ASSERT_EQUAL(strcatSetSlice(cstring("abc"), cstring("def")), cstring("abcdef"));
    // ASSERT_EQUAL(doStrcat(), cstring("cdefg"));
}

TEST(codegen_reverse_string) {
    auto instance = COMPILE(R"(
own i8[] reverse(i8[] xs):
    var result: new i8[|xs|]
    result[i] = xs[|xs| - i - 1] for i < |xs|
    return own i8[](result)
)");

    auto exec = load(instance.artifact);
    auto reverse = lookup<const_slice<i8>(const_slice<i8>)>("reverse(i8[])", exec);

    ASSERT_EQUAL(reverse(cstring("")), cstring(""));
    ASSERT_EQUAL(reverse(cstring("abc")), cstring("cba"));
    ASSERT_EQUAL(reverse(cstring("the quick brown fox")), cstring("xof nworb kciuq eht"));
}

TEST(codegen_string_indexof) {
    auto instance = COMPILE(R"(
i32 indexof(i8[] haystack, i8[] needle):
    return 0 if |needle| == 0
    for i <= |haystack| - |needle|:
        bool mismatched: false
        for j < |needle|:
            if haystack[i + j] != needle[j]:
                mismatched = true
                break
        if not mismatched:
            return i
    return -1
)");

    auto exec = load(instance.artifact);
    auto indexof = lookup<i32(const_slice<i8>, const_slice<i8>)>("indexof(i8[],i8[])", exec);

    ASSERT_EQUAL(indexof(cstring("abcdef"), cstring("abc")), 0);
    ASSERT_EQUAL(indexof(cstring("abcdef"), cstring("def")), 3);
    ASSERT_EQUAL(indexof(cstring("the quick brown fox"), cstring("brown")), 10);
    ASSERT_EQUAL(indexof(cstring("foo bar foo bar"), cstring("bar")), 4);
    ASSERT_EQUAL(indexof(cstring("the quick brown fox"), cstring("lazy dog")), -1);
    ASSERT_EQUAL(indexof(cstring(""), cstring("abc")), -1);
    ASSERT_EQUAL(indexof(cstring("abc"), cstring("")), 0);
    ASSERT_EQUAL(indexof(cstring(""), cstring("")), 0);
}

TEST(codegen_string_is_palindrome) {
    auto instance = COMPILE(R"(
bool palindrome?(i8[] str):
    for i < |str|/2:
        return false if str[i] != str[|str| - i - 1]
    return true
)");

    auto exec = load(instance.artifact);
    auto palindrome = lookup<bool(const_slice<i8>)>("palindrome?(i8[])", exec);

    ASSERT(!palindrome(cstring("abcdef")));
    ASSERT(palindrome(cstring("racecar")));
    ASSERT(palindrome(cstring("")));
    ASSERT(palindrome(cstring("rats live on no evil star")));
}

TEST(codegen_histogram) {
    auto instance = COMPILE(R"(
i32 min(i32[] nums):
    i32 m: nums[0]
    for 1 <= i < |nums|:
        m = nums[i] if nums[i] < m
    return m

i32 max(i32[] nums):
    i32 m: nums[0]
    for 1 <= i < |nums|:
        m = nums[i] if nums[i] > m
    return m

void histogram(i32[] nums, i32[] buckets):
    var l: min(nums), h: max(nums), interval: (h - l + 1) / |buckets|
    buckets[(nums[i] - l) / interval] ++ for i < |nums|
)");

    auto exec = load(instance.artifact);
    auto histogram = lookup<void(const_slice<i32>, slice<i32>)>("histogram(i32[],i32[])", exec);

    array<i32, 90> numbers;
    for (i32 i = 0; i < 80; i ++)
        numbers[i] = i;
    for (i32 i = 0; i < 10; i ++)
        numbers[80 + i] = i;

    array<i32, 8> h;
    histogram(numbers, h);

    ASSERT_EQUAL(h[0], 20);
    for (i32 i = 1; i < 8; i ++)
        ASSERT_EQUAL(h[i], 10);
}

TEST(codegen_count_subtrees) {
    return;
    // Adapted from an impromptu OCaml benchmark written by blueberrywren
    auto instance = COMPILE(R"(
type Tree:
    case Leaf
    case Branch:
        i32 item
        own Tree* left, right
use Tree.*

i32 x: 10
i32 rand():
    x = (x * 27527 + 27791) % 41231
    return x

own Tree* makeTree(i32 max, i32 depth):
    if depth <= 1:
        return Leaf
    new Branch(rand() % max, makeTree(max, depth - 1), makeTree(max, depth - 1))

bool isSub(Tree* t, Tree* k):
    match (t, k):
        case (Leaf, x): true
        case (Branch(x, a, b), Branch(y, q, w)):
            if x == y and isSub(a, q) and isSub(b, w):
                return true
            isSub(t, q) or isSub(t, w)
        else:
            false

i32 countSubtrees(Tree* a, Tree* t):
    match a:
        case Leaf: 1
        case Branch(i, x, y):
            (1 if isSub(a, t) else 0) + countSubtrees(x, t) + countSubtrees(y, t)

i32 benchmark(i32 x):
    var t: makeTree(10, x), f: makeTree(9, x)
    return countSubtrees(f, t)
)");

    auto exec = load(instance.artifact);
    auto benchmark = lookup<i32(i32)>("benchmark(i32)", exec);
    ASSERT_EQUAL(benchmark(11), 1848);
}

TEST(codegen_ball_bounce) {
    // Adapted from `bounce` Lua benchmark, viewed here: https://github.com/softdevteam/lua_benchmarking

    auto instance = COMPILE(R"(
u32 x: 74755

u32 rand():
    x = (x * 1309 + 13849) & 65535
    return x

type Ball:
    i32 x, y, vx, vy

void init(uninit Ball* ball):
    ball.x = rand() % 500
    ball.y = rand() % 500
    ball.vx = rand() % 300 - 150
    ball.vy = rand() % 300 - 150

bool bounce(Ball* ball):
    var xlim: 500, ylim: 500, bounced: false
    ball.x += ball.vx
    ball.y += ball.vy
    if ball.x > xlim:
        ball.x = xlim
        ball.vx *= -1
        bounced = true
    if ball.x < 0:
        ball.x = 0
        ball.vx *= -1
        bounced = true
    if ball.y > ylim:
        ball.y = ylim
        ball.vy *= -1
        bounced = true
    if ball.y < 0:
        ball.y = 0
        ball.vy *= -1
        bounced = true
    return bounced

u32 benchmark(u32 ticks):
    Ball[100] balls
    for i < 100:
        balls[i].init()
    var bounces: 0
    for i < ticks:
        for j < 100:
            bounces ++ if balls[j].bounce()
    return bounces
)");

    auto exec = load(instance.artifact);
    auto benchmark = lookup<i64(u32)>("benchmark(u32)", exec);

    ASSERT_EQUAL(benchmark(100), 2659);
}

FOR_EACH_INT(DEFINE_TEST, codegen_set_slice,
    auto instance = COMPILE_SUBST(R"(
$T[] getSlice($T[] a, u32 i, u32 j):
    a[i:j]
void setSlice($T[] a, u32 i, u32 j, $T[] b):
    a[i:j] = b
)");

    auto exec = load(instance.artifact);
    auto getSlice = lookup<slice<type>(slice<type>, u32, u32)>("getSlice", exec);
    auto setSlice = lookup<slice<type>(slice<type>, u32, u32, slice<type>)>("setSlice", exec);

    array<type, 10> arr;
    for (u32 i = 0; i < 10; i ++) arr[i] = i + 1;

    setSlice(arr, 1, 1, getSlice(arr, 5, 5)); // Zero-length, should be a no-op.
    for (u32 i = 0; i < 10; i ++)
        ASSERT_EQUAL(arr[i], i + 1);

    setSlice(arr, 9, 10, getSlice(arr, 8, 9)); // Set a single element, forward direction.
    setSlice(arr, 0, 1, getSlice(arr, 1, 2)); // Same in the backward direction.

    // Expect [2, 2, 3, 4, 5, 6, 7, 8, 9, 9]
    ASSERT_EQUAL(arr[0], 2);
    ASSERT_EQUAL(arr[9], 9);
    for (u32 i = 1; i < 9; i ++)
        ASSERT_EQUAL(arr[i], i + 1);

    setSlice(arr, 7, 10, getSlice(arr, 6, 9)); // Set multiple elements, overlapping, forward direction.
    setSlice(arr, 0, 3, getSlice(arr, 1, 4)); // Set multiple elements, overlapping, backward direction.

    // Expect [2, 3, 4, 4, 5, 6, 7, 7, 8, 9]
    array<type, 10> expected;
    expected[0] = 2;
    expected[1] = 3;
    expected[2] = 4;
    for (u32 i = 3; i < 7; i ++)
        expected[i] = i + 1;
    expected[7] = 7;
    expected[8] = 8;
    expected[9] = 9;
    for (u32 i = 0; i < 10; i ++)
        ASSERT_EQUAL(arr[i], expected[i]);

    setSlice(arr, 0, 10, getSlice(arr, 0, 10)); // Overwrite whole array, should be no-op.
    for (u32 i = 0; i < 10; i ++)
        ASSERT_EQUAL(arr[i], expected[i]);
)

FOR_EACH_INT(DEFINE_TEST, codegen_splat_list,
    return;
    auto instance = COMPILE(R"(
own $T[] append($T[] a, $T[] b):
    return new [a..., b...]

$T foldAppendLocal($T[] a, $T[] b, $T($T, $T) f, $T x):
    var arr: [a..., b...], acc: x
    acc = f(acc, arr[i]) for i < |arr|
    return acc

$T add($T x, $T y): x + y
$T mul($T x, $T y): x * y
)");

    auto exec = load(instance.artifact);
    auto append = lookup<const_slice<type>(const_slice<type>, const_slice<type>)>("append", exec);
    auto foldAppendLocal = lookup<type(const_slice<type>, const_slice<type>, type(*)(type, type), type)>("foldAppendLocal", exec);
    auto add = lookup<type(type, type)>("add", exec);
    auto mul = lookup<type(type, type)>("mul", exec);

    array<type, 1> a, b, c, d;
    a[0] = 1;
    b[0] = 2;
    c[0] = 3;
    d[0] = 4;

    auto ab = append(a, b);
    ASSERT_EQUAL(ab.size(), 2);
    ASSERT_EQUAL(ab[0], 1);
    ASSERT_EQUAL(ab[1], 2);

    auto cd = append(c, d);
    ASSERT_EQUAL(cd.size(), 2);
    ASSERT_EQUAL(cd[0], 3);
    ASSERT_EQUAL(cd[1], 4);

    ASSERT_EQUAL(foldAppendLocal(ab, cd, add, 0), 10);
    ASSERT_EQUAL(foldAppendLocal(ab, cd, mul, 1), 24);
)

TEST(codegen_sizeof) {
    auto instance = COMPILE(R"(
type Foo:
    i32 a, b
type Bar:
    i32 c
type Baz:
    f64 f
    i32 d

i8[64] array: uninit

i32* getA():
    var foo: &array[0] as Foo*
    return &foo.a

i32* getB():
    var foo: &array[0] as Foo*
    return &foo.b

i32* getC():
    var bar: &array[|Foo|] as Bar*
    return &bar.c

i32* getD():
    var baz: &array[|Foo| + |Bar|] as Baz*
    return &baz.d
)");

    auto exec = load(instance.artifact);
    auto getA = lookup<i32*()>("getA()", exec);
    auto getB = lookup<i32*()>("getB()", exec);
    auto getC = lookup<i32*()>("getC()", exec);
    auto getD = lookup<i32*()>("getD()", exec);

    *getA() = 1;
    *getB() = 2;
    *getC() = 3;
    *getD() = 4;

    ASSERT_EQUAL(*getA() + *getB() + *getC() + *getD(), 10);
}

TEST(codegen_is_simple) {
    auto instance = COMPILE(R"(
type Cases:
    case A
    case B
    case C: i32
use Cases.*

i32 mystery(Cases* obj):
    if obj is A:
        1
    else if obj is B:
        2
    else if obj is C(x):
        x + 21
    else: 42

own Cases* makeA(): A
own Cases* makeB(): B
own Cases* makeC(i32 x): new C(x)
)");

    auto exec = load(instance.artifact);
    auto mystery = lookup<i32(void*)>("mystery(Cases*)", exec);
    auto makeA = lookup<void*()>("makeA()", exec);
    auto makeB = lookup<void*()>("makeB()", exec);
    auto makeC = lookup<void*(i32)>("makeC(i32)", exec);

    auto a = makeA();
    auto b = makeB();
    auto c = makeC(21);

    ASSERT_EQUAL(mystery(a), 1);
    ASSERT_EQUAL(mystery(b), 2);
    ASSERT_EQUAL(mystery(c), 42);
}

TEST(codegen_var_destructuring) {
    auto instance = COMPILE(R"(
type Named: i32
type Pair: i32 x, y

i32 mystery():
    var a: Named(7)
    var b: Pair(7, 7)
    var c: [7, 7, 7]
    var Named(x): a
    var Pair(y, z): b
    var [w, etc...]: c
    return x + y + z + w + etc[0] + etc[1]
)");

    auto exec = load(instance.artifact);
    auto mystery = lookup<i32()>("mystery()", exec);
    ASSERT_EQUAL(mystery(), 42);
}