#include "clover/test/unit/helpers.h"

#define CONCRETE(type) \
    [&](Type t) -> Type { t.concretify(); return expand(t); }(type)

#define BOUNDS_OF(type) \
    [&]() -> pair<TypeIndex, TypeIndex> { \
        Type t = (type); \
        if (t.isVar()) \
            return { CONCRETE(t.asVar().lowerBound()).index, CONCRETE(t.asVar().upperBound()).index }; \
        if (t.isRange()) \
            return { t.asRange().lowerBoundIndex(), t.asRange().upperBoundIndex() }; \
        return { t.index, t.index }; \
    }()

inline Type testableType(Module* mod, Type t) {
    return expand(t);
}

inline Type testableType(Module* mod, TypeIndex t) {
    return expand(mod->types->get(t));
}

#define ASSERT_TYPE_EQUAL(a, b) do { \
        ASSERT_EQUAL(testableType(module, a), testableType(module, b)); \
    } while (false)

#define ASSERT_HAS_TYPE(src, typeExpr) do { \
        auto mod = TYPECHECK(src); \
        Module* module = mod.artifact->as<Module>(); \
        Type resultType = CONCRETE(module->getTopLevel().type()); \
        Type correctType = module-> typeExpr ; \
        ASSERT_TYPE_EQUAL(resultType, correctType); \
    } while (false)

#define ASSERT_HAS_LOWER_BOUND(src, lowerBound) do { \
        auto mod = TYPECHECK(src); \
        Module* module = mod.artifact->as<Module>(); \
        auto result = BOUNDS_OF(module->getTopLevel().type()); \
        Type correctLower = module-> lowerBound ; \
        ASSERT_TYPE_EQUAL(correctLower, correctLower.types->get(result.first)); \
    } while (false)

#define ASSERT_HAS_TYPE_BOUNDS(src, lowerBound, upperBound) do { \
        auto mod = TYPECHECK(src); \
        Module* module = mod.artifact->as<Module>(); \
        auto result = BOUNDS_OF(module->getTopLevel().type()); \
        Type correctLower = module-> lowerBound ; \
        Type correctUpper = module-> upperBound ; \
        ASSERT_TYPE_EQUAL(correctLower, correctLower.types->get(result.first)); \
        ASSERT_TYPE_EQUAL(correctUpper, correctLower.types->get(result.second)); \
    } while (false)

#define ASSERT_HAS_TYPE_BOUNDS(src, lowerBound, upperBound) do { \
        auto mod = TYPECHECK(src); \
        Module* module = mod.artifact->as<Module>(); \
        auto result = BOUNDS_OF(module->getTopLevel().type()); \
        Type correctLower = module-> lowerBound ; \
        Type correctUpper = module-> upperBound ; \
        ASSERT_TYPE_EQUAL(correctLower, correctLower.types->get(result.first)); \
        ASSERT_TYPE_EQUAL(correctUpper, correctLower.types->get(result.second)); \
    } while (false)

#define TYPEOF_GLOBAL(var) \
    [&]() -> Type { \
        Type t = module->types->get(module->globals[var .variable()].type); \
        t.concretify(); \
        return expand(t); \
    }()

TEST(typecheck_integer_constants) {
    ASSERT_HAS_TYPE("0", signedType(64));
    ASSERT_HAS_TYPE("1", signedType(64));
    ASSERT_HAS_TYPE("127", signedType(64));
    ASSERT_HAS_TYPE("128", signedType(64));
    ASSERT_HAS_TYPE("255", signedType(64));
    ASSERT_HAS_TYPE("256", signedType(64));
    ASSERT_HAS_TYPE("32767", signedType(64));
    ASSERT_HAS_TYPE("32768", signedType(64));
    ASSERT_HAS_TYPE("65535", signedType(64));
    ASSERT_HAS_TYPE("65536", signedType(64));
    ASSERT_HAS_TYPE("2147483647", signedType(64));
    ASSERT_HAS_TYPE("2147483648", signedType(64));
    ASSERT_HAS_TYPE("4294967295", signedType(64));
    ASSERT_HAS_TYPE("4294967296", signedType(64));
    ASSERT_HAS_TYPE("9223372036854775807", signedType(64));
    ASSERT_HAS_TYPE("9223372036854775808", unsignedType(64));
    ASSERT_HAS_TYPE("18446744073709551615", unsignedType(64));
}

TEST(typecheck_integer_plus_minus_constants) {
    ASSERT_HAS_TYPE("-0", signedType(64));
    ASSERT_HAS_TYPE("+0", signedType(64));
    ASSERT_HAS_TYPE("-1", signedType(64));
    ASSERT_HAS_TYPE("+1", signedType(64));
    ASSERT_HAS_TYPE("-128", signedType(64));
    ASSERT_HAS_TYPE("+128", signedType(64));
    ASSERT_HAS_TYPE("-255", signedType(64));
    ASSERT_HAS_TYPE("+255", signedType(64));
    ASSERT_HAS_TYPE("-256", signedType(64));
    ASSERT_HAS_TYPE("+256", signedType(64));
    ASSERT_HAS_TYPE("-32768", signedType(64));
    ASSERT_HAS_TYPE("+32768", signedType(64));
    ASSERT_HAS_TYPE("-65535", signedType(64));
    ASSERT_HAS_TYPE("+65535", signedType(64));
    ASSERT_HAS_TYPE("-65536", signedType(64));
    ASSERT_HAS_TYPE("+65536", signedType(64));
    ASSERT_HAS_TYPE("-2147483648", signedType(64));
    ASSERT_HAS_TYPE("+2147483648", signedType(64));
    ASSERT_HAS_TYPE("-4294967295", signedType(64));
    ASSERT_HAS_TYPE("+4294967295", signedType(64));
    ASSERT_HAS_TYPE("-4294967296", signedType(64));
    ASSERT_HAS_TYPE("+4294967296", signedType(64));
    ASSERT_HAS_TYPE("-9223372036854775808", signedType(64));
    ASSERT_HAS_TYPE("+9223372036854775808", unsignedType(64));
    ASSERT_HAS_TYPE("+18446744073709551615", unsignedType(64));

    // TODO: Check that we reject the following:
    // ASSERT_HAS_TYPE("+18446744073709551616", u64Type());
}

TEST(typecheck_add_constants) {
    ASSERT_HAS_TYPE("0 + 0", signedType(64));
    ASSERT_HAS_TYPE("0 + 1", signedType(64));
    ASSERT_HAS_TYPE("1 + -1", signedType(64));
    ASSERT_HAS_TYPE("500 + 1", signedType(64));
    ASSERT_HAS_TYPE("0.1 + 0.2", f32Type());
    ASSERT_HAS_TYPE("0.1 + 1", f32Type());
}

TEST(typecheck_string_literal) {
    ASSERT_HAS_LOWER_BOUND("\"\"", arrayType(I8, 0u));
    ASSERT_HAS_LOWER_BOUND("\"abc\"", arrayType(I8, 3u));
    ASSERT_HAS_LOWER_BOUND("\"abc def\"", arrayType(I8, 7u));

    auto instance = TYPECHECK(R"(
var str: "abc"
str = str[1:2]
)");
    Module* module = instance.as<Module>();
    auto topLevel = module->getTopLevel();

    AST str = topLevel.child(0);
    ASSERT(expand(str.type()).is<TypeKind::Slice>());
    ASSERT(expand(expand(str.type()).as<TypeKind::Slice>().elementType()) == I8);
}

TEST(typecheck_inferred_vardecl) {
    auto instance = TYPECHECK(R"(
var x: 1
var y: 2.0
var z: x + y
)");
    Module* module = instance.as<Module>();
    auto topLevel = module->getTopLevel();

    AST x = topLevel.child(0);
    ASSERT_TYPE_EQUAL(x.type(), module->i64Type());
    ASSERT_TYPE_EQUAL(TYPEOF_GLOBAL(x.child(1)), module->i64Type());

    AST y = topLevel.child(1);
    ASSERT_TYPE_EQUAL(y.type(), module->f32Type());
    ASSERT_TYPE_EQUAL(TYPEOF_GLOBAL(y.child(1)), module->f32Type());

    AST z = topLevel.child(2);
    ASSERT_TYPE_EQUAL(z.type(), module->f32Type());
    ASSERT_TYPE_EQUAL(TYPEOF_GLOBAL(z.child(1)), module->f32Type());
}

TEST(typecheck_chained_assignment) {
    auto instance = TYPECHECK(R"(
var x: 1
var y: 2
var z: 3
z = y
y = x
x = 256
)");
}

TEST(typecheck_simple_widen) {
    auto instance = TYPECHECK(R"(
var x: 1
x + 2.0
)");
}

TEST(typecheck_simple_cycle) {
    auto instance = TYPECHECK(R"(
var x: 1
var y: x
x = y
)");
}

TEST(typecheck_triple_cycle) {
    auto instance = TYPECHECK(R"(
var x: 1
var y: x
var z: y
x = z
)");
}

TEST(typecheck_quadruple_cycle) {
    auto instance = TYPECHECK(R"(
var x: 1
var y: x
var z: y
var w: z
x = w
)");
}

TEST(typecheck_type_diamond) {
    auto instance = TYPECHECK(R"(
var a: 1
var b: a
var c: a
b = c
var d: c
d = b
)");
}

TEST(typecheck_multiple_cycles) {
    auto instance = TYPECHECK(R"(
var a: 1
var b: a
var c: b
var d: b
var e: c
e = d
a = e
)");
}

TEST(typecheck_array_literal) {
    ASSERT_HAS_TYPE("[1]", arrayType(module->i64Type(), u32(1)));
    ASSERT_HAS_TYPE("[1, 2]", arrayType(module->i64Type(), u32(2)));
    ASSERT_HAS_TYPE("[1, 2.0]", arrayType(module->f32Type(), u32(2)));
    ASSERT_HAS_TYPE("i32 x: 1\n[x, 1]", arrayType(module->i64Type(), u32(2)));
}

TEST(typecheck_array_backpropagate) {
    ASSERT_HAS_TYPE(R"(
var x: 1
i32[2] arr: [x, 1]
x
)", i32Type());
}

TEST(typecheck_slice_backpropagate) {
    ASSERT_HAS_TYPE(R"(
var x: 1
i32[2] arr: [1, 2]
var slice: arr[0:1]
slice[0] = x
x
)", i32Type());
}

TEST(typecheck_paren_simple) {
    ASSERT_HAS_TYPE("(1)", signedType(64));
    ASSERT_HAS_TYPE("(1 + 2)", signedType(64));
    ASSERT_HAS_TYPE("(1 + 2.0)", f32Type());
    ASSERT_HAS_TYPE("i32 x\n(x)", i32Type());
    ASSERT_HAS_TYPE("i32 x\n(((x)))", i32Type());
}

TEST(typecheck_if_no_else) {
    ASSERT_HAS_TYPE("if true: 42", voidType());
    ASSERT_HAS_TYPE("if false: 42", voidType());
    ASSERT_HAS_TYPE("if 1 == 2: 42", voidType());
    ASSERT_HAS_TYPE("if 1 != 2: 42", voidType());
    ASSERT_HAS_TYPE("i32 x\nif x != 2: 42", voidType());
    ASSERT_HAS_TYPE("i32 x\nif 2 < x: 42", voidType());
}

TEST(typecheck_range_type_in_intermediate_result) {
    ASSERT_HAS_TYPE("i32 x\nx == 1", boolType());
    ASSERT_HAS_TYPE("i32 x\nx == 1 + 2", boolType());
    ASSERT_HAS_TYPE("i32 x\nx == 1 * 2 + 3", boolType());
    ASSERT_HAS_TYPE("i32 x\nx == 1 & 2 | 3", boolType());
    ASSERT_HAS_TYPE("i32 x\nx == 1 * (2 + 3)", boolType());
    ASSERT_HAS_TYPE("i32 x\nx == 1 & (2 | 3)", boolType());
    ASSERT_HAS_TYPE("i32 x\nx == (((1) + 2) + (3))", boolType());
}

TEST(typecheck_if_else_expr) {
    ASSERT_HAS_TYPE("1 if true else 2", i64Type());
    ASSERT_HAS_TYPE("1.0 if true else 2", f32Type());
    ASSERT_HAS_TYPE("1 if true else 2.0", f32Type());
    ASSERT_HAS_TYPE("true if true == false else false", boolType());
}

TEST(typecheck_if_else_stmt) {
    ASSERT_HAS_TYPE("if true: 1\nelse: 2", voidType());
}

TEST(typecheck_while_stmt) {
    ASSERT_HAS_TYPE("while true: 1", voidType());
    ASSERT_HAS_TYPE("var x: 1 then var y: 2 then x while x < y", voidType());
    ASSERT_HAS_TYPE("var x: 1 then var y: 2 then y while x != y", voidType());
}

TEST(typecheck_explicit_void_return_type) {
    ASSERT_HAS_TYPE(R"(
void foo():
    var x: 1
    x + 1
foo
)", funType(module->voidType()));
}

TEST(typecheck_implicit_return) {
    ASSERT_HAS_TYPE(R"(
i32 foo(i32 x):
    x + 1
foo
)", funType(module->i32Type(), module->i32Type()));
    ASSERT_HAS_TYPE(R"(
i64 foo():
    42
foo
)", funType(module->i64Type()));
}

TEST(typecheck_implicit_return_sequence) {
    ASSERT_HAS_TYPE(R"(
i32 foo(i32 x):
    var y: x
    var z: y
    var w: z * y * x
    while y < z:
        y = y + 1
    y + z + w
foo
)", funType(module->i32Type(), module->i32Type()));
}

TEST(typecheck_implicit_return_if_else) {
    ASSERT_HAS_TYPE(R"(
f32 foo(i32 x):
    if x < 10:
        x + 1
    else:
        x * 0.5
foo
)", funType(module->f32Type(), module->i32Type()));
    ASSERT_HAS_TYPE(R"(
f32 foo(i32 x):
    x + 1 if x < 10 else x * 0.5
foo
)", funType(module->f32Type(), module->i32Type()));
}

TEST(typecheck_get_field_struct) {
    auto instance = TYPECHECK(R"(
type Foo:
    i32 x
    f32 y
    Foo* z
Foo foo
foo.x
foo.y
foo.z
)");
    auto module = instance.artifact->as<Module>();
    auto topLevel = module->getTopLevel();
    auto FooType = topLevel.child(0).type();

    ASSERT_TYPE_EQUAL(topLevel.child(2).type(), I64);
    ASSERT_TYPE_EQUAL(topLevel.child(3).type(), F32);
    ASSERT_TYPE_EQUAL(topLevel.child(4).type(), module->ptrType(FooType));
}

TEST(typecheck_swizzle_struct_simple) {
    auto instance = TYPECHECK(R"(
type Vec3:
    f32 x, y, z
Vec3 v
v.xyz
)");
    auto module = instance.artifact->as<Module>();
    auto topLevel = module->getTopLevel();
    auto FooType = topLevel.child(0).type();

    ASSERT_TYPE_EQUAL(topLevel.child(2).type(), module->tupleType(F32, F32, F32));
}

TEST(typecheck_swizzle_struct) {
    auto instance = TYPECHECK(R"(
type Vec2:
    i32 x, y
type Vec3:
    f32 x, y, z
Vec2 v2
v2.x
v2.xy
v2.xyxy

Vec3 v3
v3.x
v3.yx
v3.xyz
v3.xxyyzz
)");
    auto module = instance.artifact->as<Module>();
    auto topLevel = module->getTopLevel();

    ASSERT_TYPE_EQUAL(topLevel.child(3).type(), I64);
    ASSERT_TYPE_EQUAL(topLevel.child(4).type(), module->tupleType(I32, I32));
    ASSERT_TYPE_EQUAL(topLevel.child(5).type(), module->tupleType(I32, I32, I32, I32));

    ASSERT_TYPE_EQUAL(topLevel.child(7).type(), F32);
    ASSERT_TYPE_EQUAL(topLevel.child(8).type(), module->tupleType(F32, F32));
    ASSERT_TYPE_EQUAL(topLevel.child(9).type(), module->tupleType(F32, F32, F32));
    ASSERT_TYPE_EQUAL(topLevel.child(10).type(), module->tupleType(F32, F32, F32, F32, F32, F32));
}

TEST(typecheck_call_unary) {
    auto instance = TYPECHECK(R"(
i32 id(i32 x): x

id(1)
var x: 2
var y: id(x)
id(y)
)");
    auto module = instance.artifact->as<Module>();
    auto topLevel = module->getTopLevel();

    ASSERT_TYPE_EQUAL(topLevel.child(0).type(), module->funType(I32, I32));
    ASSERT_TYPE_EQUAL(topLevel.child(1).type(), I64);
    ASSERT_TYPE_EQUAL(topLevel.child(2).type(), I32);
    ASSERT_TYPE_EQUAL(topLevel.child(3).type(), I32);
    ASSERT_TYPE_EQUAL(topLevel.child(4).type(), I64);
}

TEST(typecheck_call_binary) {
    auto instance = TYPECHECK(R"(
i32 add(i32 x, i32 y):
    x + y
add(1, 2)
i8 a: 42
u16 b: 46
add(add(a, 2), b)
1.add(2).add(3)
)");
}

TEST(typecheck_call_with_array) {
    auto instance = TYPECHECK(R"(
i32 intsum(i32[] xs):
    var i: 0, sum: 0
    while i < |xs|:
        sum = sum + xs[i]
    return sum
var nums: [1, 2, 3]
intsum(nums)
)");
}

TEST(typecheck_numeric_casts) {
    auto instance = TYPECHECK(R"(
var a: 42
var b: i8(a)
var c: f32(b)
var d: i16(c)
var e: u64(u8(i64(i8(d))))
var f: e.f32().f64().f32().u16().i16()
)");
}

TEST(typecheck_construct_named) {
    auto instance = TYPECHECK(R"(
type Foo: i32
type Bar: Foo
var x: Foo(42)
var y: x.Bar()
)");
}

TEST(typecheck_construct_tuple) {
    auto instance = TYPECHECK(R"(
var x: (i32, bool)(42, true)
var y: (u8, u16, f32, f64)(1, 2, 3, 4)
)");
}

TEST(typecheck_construct_struct) {
    auto instance = TYPECHECK(R"(
type Simple: i32 x, y
type Nested: Simple s, t
type Complex:
    i32 i
    (i32, i32) pair
    Nested nested
    f32 f
var simple: Simple(1, 2)
var nested: Nested(simple, Simple(3, 4))
var complex: Complex(42, (simple.x, simple.y), nested, 0.5)
)");
}

TEST(typecheck_dot_product) {
    auto instance = TYPECHECK(R"(
type Vec3:
    f32 x, y, z
f32 dot(Vec3 a, Vec3 b):
    a.x * b.x + a.y * b.y + a.z * b.z
Vec3(1, 0, 0).dot(Vec3(0, 1, 0))
)");
}

TEST(typecheck_call_hailstone) {
    auto instance = TYPECHECK(R"(
i32 hailRec(i32 x):
    if x == 1:
        x
    else if x % 2 == 0:
        hailRec(x / 2)
    else:
        hailRec(3x + 1)
i32 hailIter(i32 x):
    while x != 1:
        if x % 2 == 0:
            x = x / 2
        else:
            x = 3x + 1
    return x
hailRec(42)
hailIter(42)
)");
}

TEST(typecheck_bubble_sort_int) {
    auto instance = TYPECHECK(R"(
void swap(i32* a, i32* b):
    i32 t: *a
    *a = *b
    *b = t

void bsort(i32[] xs):
    var n: |xs|
    var i: 0
    while i < n - 1:
        var j: i + 1
        swap(&xs[j], &xs[j - 1]) if xs[j] < xs[j - 1] while j < n

var arr: [1, 2, 3]
bsort(arr)
)");
}

TEST(typecheck_set_field_simple) {
    auto instance = TYPECHECK(R"(
type Foo:
    i32 x, y, z
var foo: Foo(1, 2, 3)
foo.x = 42
foo.y = foo.z
)");
}

TEST(typecheck_set_field_chain) {
    auto instance = TYPECHECK(R"(
type Foo:
    i32 x, y
type Bar:
    Foo foo
var bar: Bar(Foo(1, 2))
bar.foo = Foo(3, 4)
bar.foo.x = 5
bar.foo.y = bar.foo.x + 1
)");
}

TEST(typecheck_addr_field_simple) {
    auto instance = TYPECHECK(R"(
type Foo:
    i32 x
var foo: Foo(42)
var ptr: &foo.x
*ptr = 43
)");
}

TEST(typecheck_addr_field_chain) {
    auto instance = TYPECHECK(R"(
type Foo:
    i32 x, y
type Bar:
    Foo foo
var bar: Bar(Foo(1, 2))
var ptr: &bar.foo.x
*ptr = 42
ptr = &bar.foo.y
*ptr = 43
)");
}

TEST(typecheck_set_index_simple) {
    auto instance = TYPECHECK(R"(
var arr: [1, 2, 3]
arr[0] = arr[1]
arr[1] = arr[2]
arr[2] = 4
)");
}

TEST(typecheck_set_index_chain) {
    auto instance = TYPECHECK(R"(
var arr: [[1, 2], [3, 4]]
arr[0][0] = arr[1][1]
)");
}

TEST(typecheck_addr_index_simple) {
    auto instance = TYPECHECK(R"(
var arr: [1, 2, 3]
var ptr: &arr[0]
var ptr2: &arr[2]
*ptr = *ptr2
)");
}

TEST(typecheck_addr_index_chain) {
    auto instance = TYPECHECK(R"(
var arr: [[1, 2], [3, 4]]
var ptr: &arr[0][0]
*ptr = *ptr + 1
)");
}

TEST(typecheck_addr_index_no_double_indirection) {
    auto instance = TYPECHECK(R"(
type Foo:
    i32 x, y
type Bar:
    Foo* foo
Bar*[16] bars

bars[0].foo.x
)");
    auto module = instance.artifact->as<Module>();
    auto topLevel = module->getTopLevel();

    auto access = topLevel.child(3);
    ASSERT_TYPE_EQUAL(access.type(), module->i64Type());
}

TEST(typecheck_new_expression) {
    auto instance = TYPECHECK(R"(
var p: new 42
var q: new 43
*q = 44
var r: new i32(42)
var t: new i32
)");

    auto module = instance.artifact->as<Module>();
    auto topLevel = module->getTopLevel();

    auto first = topLevel.child(0);
    ASSERT(first.type() == module->ptrType(Own, I64));
    ASSERT(first.child(2).type() == module->ptrType(Own, I64));

    auto second = topLevel.child(1);
    ASSERT(second.type() == module->ptrType(Own, I64));
    ASSERT(second.child(2).type() == module->ptrType(Own, I64));

    auto third = topLevel.child(3);
    ASSERT(third.type() == module->ptrType(Own, I32));
    ASSERT(third.child(2).type() == module->ptrType(Own, I32));

    auto fourth = topLevel.child(4);
    ASSERT(fourth.type() == module->ptrType(Own | Uninit, I32));
    ASSERT(fourth.child(2).type() == module->ptrType(Own | Uninit, I32));
}

TEST(typecheck_new_array_expression) {
    auto instance = TYPECHECK(R"(
var a: new i32[42]
var x: 42
var b: new i32[x]
)");

    auto module = instance.artifact->as<Module>();
    auto topLevel = module->getTopLevel();

    auto a = topLevel.child(0);
    ASSERT(a.type() == module->sliceType(Own | Uninit, I32));
    ASSERT(a.child(2).type() == module->sliceType(Own | Uninit, I32));

    auto x = topLevel.child(1);
    ASSERT(x.type() == module->i64Type());

    auto b = topLevel.child(2);
    ASSERT(b.type() == module->sliceType(Own | Uninit, I32));
    ASSERT(b.child(2).type() == module->sliceType(Own | Uninit, I32));
}

TEST(typecheck_union_members) {
    auto instance = TYPECHECK(R"(
type Foo:
    case Bar: i32
    case Baz

Foo foo: Foo.Bar(42)
Foo foo2: Foo.Baz()
)");
}

TEST(typecheck_union_lca) {
    auto instance = TYPECHECK(R"(
type Foo:
    case Bar: i32
    case Baz

var foo: Foo.Bar(42)
foo = Foo.Baz()
)");

    auto module = instance.artifact->as<Module>();
    auto topLevel = module->getTopLevel();

    auto foo = module->lookup(topLevel.scope(), module->sym("foo"));
    auto fooType = foo.type();
    ASSERT_TYPE_EQUAL(module->types->get(fooType), topLevel.child(0).type());
}

TEST(typecheck_union_lca_ptr) {
    auto instance = TYPECHECK(R"(
type Foo:
    case Bar: i32
    case Baz

var foo: new Foo.Bar(42)
foo = new Foo.Baz()
)");

    auto module = instance.artifact->as<Module>();
    auto topLevel = module->getTopLevel();
    auto foo = module->lookup(topLevel.scope(), module->sym("foo"));
    auto fooType = foo.type();
    auto Foo = topLevel.child(0).type();
    ASSERT_TYPE_EQUAL(module->types->get(fooType), module->ptrType(Own, Foo));
}

TEST(typecheck_match_scalar) {
    auto instance = TYPECHECK(R"(
var x: 42
match x:
    case 1: 1
    case 65536: 2
    case 0.5: 3
    else: 42
)");
}

TEST(typecheck_match_union) {
    auto instance = TYPECHECK(R"(
type Foo:
    case Bar
    case Baz: i32
    case Quux: i32 x, y

var x: Foo.Quux(1, 2)
match x:
    case Foo.Bar: 1
    case Foo.Baz(a): a + 1
    case Foo.Quux(a, b): a + b
)");
}

TEST(typecheck_match_array) {
    auto instance = TYPECHECK(R"(
var arr: [-1, 2, -3, 4]
match arr:
    case [1, 2, 3]: 5
    case [1, x, y]: x + y
    case [1, 2, xs...]: 42
    case [xs...]: 43
    else: 44
)");
}

TEST(typecheck_overloaded_call) {
    auto instance = TYPECHECK(R"(
i32 foo(i32 x):
    x + 1
i32[] foo(i32[] xs):
    xs[0:1]

foo(42)
foo([1, 2, 3])
)");
}

TEST(typecheck_match_doesnt_overfit) {
    auto instance = TYPECHECK(R"(
type Foo:
    case A
    case B
    case C

i32 mystery(Foo a, Foo b):
    match (a, b):
        case (Foo.A, x): 21
        case (Foo.B, Foo.C): 42
        else: 63
)");

    auto module = instance.artifact->as<Module>();
    auto topLevel = module->getTopLevel();

    auto FooType = topLevel.child(0).type();
    auto AType = topLevel.child(0).child(2).type();

    auto firstCase = topLevel.child(1).child(4).child(0).child(1);
    ASSERT_EQUAL(firstCase.kind(), ASTKind::Case);

    auto firstCaseType = firstCase.child(0).type();
    ASSERT(firstCaseType.is<TypeKind::Tuple>());
    ASSERT_TYPE_EQUAL(firstCaseType.as<TypeKind::Tuple>().fieldType(0), AType);
    ASSERT_TYPE_EQUAL(firstCaseType.as<TypeKind::Tuple>().fieldType(1), FooType);
}

TEST(typecheck_const_integer) {
    auto instance = TYPECHECK(R"(
const Foo: 41
var x: Foo + 1
)");

    auto module = instance.artifact->as<Module>();
    auto topLevel = module->getTopLevel();
    auto x = topLevel.child(1);
    ASSERT_EQUAL(x.child(2).kind(), ASTKind::Unsigned);
    ASSERT_EQUAL(x.child(2).uintConst(), 42);
}

TEST(typecheck_associated_const) {
    auto instance = TYPECHECK(R"(
type Foo:
    const x: 21
    const y: 21
var x: Foo.x + Foo.y
)");

    auto module = instance.artifact->as<Module>();
    auto topLevel = module->getTopLevel();
    auto x = topLevel.child(1);
    ASSERT_EQUAL(x.child(2).kind(), ASTKind::Unsigned);
    ASSERT_EQUAL(x.child(2).uintConst(), 42);
}

TEST(typecheck_use_associated_const) {
    auto instance = TYPECHECK(R"(
type Foo:
    const x: 21
use Foo.x
var y: x + x

type Bar:
    const y: x

i32 baz():
    use Bar.*
    var z: x + y
    z
)");

    auto module = instance.artifact->as<Module>();
    auto topLevel = module->getTopLevel();
    auto y = topLevel.child(2);
    ASSERT_EQUAL(y.child(2).kind(), ASTKind::Unsigned);
    ASSERT_EQUAL(y.child(2).uintConst(), 42);

    auto z = topLevel.child(4).child(4).child(1);
    ASSERT_EQUAL(z.child(2).kind(), ASTKind::Unsigned);
    ASSERT_EQUAL(z.child(2).uintConst(), 42);
}

TEST(typecheck_overload_by_return_type) {
    auto instance = TYPECHECK(R"(
i32[3] global: [1, 2, 3]
i32[] makeNum(): global
i32 makeNum(): 42

i32 x: makeNum()
i32[] y: makeNum()
)");

    auto module = instance.artifact->as<Module>();
    auto topLevel = module->getTopLevel();

    auto x = topLevel.child(3);
    ASSERT_TYPE_EQUAL(x.child(2).type(), module->i32Type());

    auto y = topLevel.child(4);
    ASSERT_TYPE_EQUAL(y.child(2).type(), module->sliceType(I32));
}

TEST(typecheck_call_generic) {
    auto instance = TYPECHECK(R"(
fun id(type T, T x): x

var i: id(42)
var j: id(42.0)
var k: i + j

i8 l: id(42)
)");

    auto module = instance.artifact->as<Module>();
    auto topLevel = module->getTopLevel();

    auto i = topLevel.child(1);
    ASSERT_TYPE_EQUAL(i.type(), module->i64Type());

    auto j = topLevel.child(2);
    ASSERT_TYPE_EQUAL(j.type(), module->f32Type());
}

TEST(typecheck_function_affects_global) {
    // Typechecking function bodies should influence the way global variables
    // are inferred. This goes for lexical function definitions and for generic
    // instantiations too.

    auto instance = TYPECHECK(R"(
var a: 1
fun foo():
    i32 x: a
    return x

var b: 2
fun bar(x):
    i32 y: x + b
    return y

foo()
bar(42)
)");

    auto module = instance.artifact->as<Module>();
    auto topLevel = module->getTopLevel();

    auto a = topLevel.child(0);
    ASSERT_TYPE_EQUAL(a.type(), module->i32Type());

    auto b = topLevel.child(2);
    ASSERT_TYPE_EQUAL(b.type(), module->i32Type());
}

TEST(typecheck_function_duplicate_instantiation) {
    auto instance = TYPECHECK(R"(
fun id(x): x + 1

var a: 1, b: 2, c: 3
id(a) + id(b) + id(c)
)");

    auto module = instance.artifact->as<Module>();
    auto topLevel = module->getTopLevel();

    auto expr = topLevel.child(2);
    auto ida = expr.child(0).child(0);
    auto idb = expr.child(0).child(1);
    auto idc = expr.child(1);
    ASSERT(ida.child(0).resolvedFunction() == idb.child(0).resolvedFunction());
    ASSERT(idb.child(0).resolvedFunction() == idc.child(0).resolvedFunction());
}

TEST(typecheck_generic_function_same_parameter) {
    auto instance = TYPECHECK(R"(
fun add(type T, T x, T y):
    return x + y

1.add(0.5)

add(1, 2)
)");
}

TEST(typecheck_generic_type_explicit) {
    auto instance = TYPECHECK(R"(
type Box(type T):
    T value

Box(i32) a: Box(i32)(42)
a.value

var b: Box(i8)(42)
b.value
)");

    auto module = instance.artifact->as<Module>();
    auto topLevel = module->getTopLevel();

    auto aval = topLevel.child(2);
    ASSERT_TYPE_EQUAL(aval.type(), module->i32Type());

    auto bval = topLevel.child(4);
    ASSERT_TYPE_EQUAL(bval.type(), module->i8Type());
}