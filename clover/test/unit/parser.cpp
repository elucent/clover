#include "clover/test/unit/helpers.h"

#define ASSERT_SAME_PARSE(src, sexp) do { \
        if UNLIKELY(config::printProducts) { \
            auto a = PARSE(src); \
            auto b = PARSE_SEXP(sexp); \
            a.artifact->print(), b.artifact->print(); \
        } \
        ASSERT(sameAST(PARSE(src), PARSE_SEXP(sexp), IgnorePos)); \
    } while (false)

TEST(parse_terminals) {
    auto artifact = PARSE("[42, 42.0, 0.0, true, false, \"abc\", `abc`, foo, Bar42, _123]");
    auto topLevel = artifact.as<Module>()->getTopLevel();
    auto list = topLevel.child(0);

    ASSERT(list.child(0).kind() == ASTKind::Unsigned);
    ASSERT_EQUAL(list.child(0).uintConst(), 42);

    ASSERT(list.child(1).kind() == ASTKind::Float);
    ASSERT_EQUAL(list.child(1).floatConst(), 42.0);

    ASSERT(list.child(2).kind() == ASTKind::Float);
    ASSERT_EQUAL(list.child(2).floatConst(), 0.0);

    ASSERT(list.child(3).kind() == ASTKind::Bool);
    ASSERT_EQUAL(list.child(3).boolConst(), true);

    ASSERT(list.child(4).kind() == ASTKind::Bool);
    ASSERT_EQUAL(list.child(4).boolConst(), false);

    ASSERT(list.child(5).kind() == ASTKind::String);
    ASSERT_EQUAL(STR(list.child(5).stringConst()), cstring("abc"));

    ASSERT(list.child(6).kind() == ASTKind::Ident);
    ASSERT_EQUAL(STR(list.child(6).symbol()), cstring("abc"));

    ASSERT(list.child(7).kind() == ASTKind::Ident);
    ASSERT_EQUAL(STR(list.child(7).symbol()), cstring("foo"));

    ASSERT(list.child(8).kind() == ASTKind::Ident);
    ASSERT_EQUAL(STR(list.child(8).symbol()), cstring("Bar42"));

    ASSERT(list.child(9).kind() == ASTKind::Ident);
    ASSERT_EQUAL(STR(list.child(9).symbol()), cstring("_123"));
}

TEST(parse_integer_constants) {
    ASSERT_SAME_PARSE("5", "5");
    ASSERT_SAME_PARSE("-5", "(minus 5)");
    ASSERT_SAME_PARSE("+5", "(plus 5)");
    ASSERT_SAME_PARSE("5000", "5000");
    ASSERT_SAME_PARSE("-5000", "(minus 5000)");
    ASSERT_SAME_PARSE("+5000", "(plus 5000)");
    ASSERT_SAME_PARSE("50000000", "50000000");
    ASSERT_SAME_PARSE("-50000000", "(minus 50000000)");
    ASSERT_SAME_PARSE("+50000000", "(plus 50000000)");
    ASSERT_SAME_PARSE("5000000000000000", "5000000000000000");
    ASSERT_SAME_PARSE("-5000000000000000", "(minus 5000000000000000)");
    ASSERT_SAME_PARSE("+5000000000000000", "(plus 5000000000000000)");

    ASSERT_SAME_PARSE("255", "255");
    ASSERT_SAME_PARSE("-255", "(minus 255)");
    ASSERT_SAME_PARSE("+255", "(plus 255)");
    ASSERT_SAME_PARSE("65535", "65535");
    ASSERT_SAME_PARSE("-65535", "(minus 65535)");
    ASSERT_SAME_PARSE("+65535", "(plus 65535)");
    ASSERT_SAME_PARSE("4294967295", "4294967295");
    ASSERT_SAME_PARSE("-4294967295", "(minus 4294967295)");
    ASSERT_SAME_PARSE("+4294967295", "(plus 4294967295)");
    ASSERT_SAME_PARSE("18446744073709551615", "18446744073709551615");
    ASSERT_SAME_PARSE("-18446744073709551615", "(minus 18446744073709551615)");
    ASSERT_SAME_PARSE("+18446744073709551615", "(plus 18446744073709551615)");
}

TEST(parse_binary_arithmetic) {
    // Associativity
    ASSERT_SAME_PARSE("1 + 2 + 3", "(+ (+ 1 2) 3)");
    ASSERT_SAME_PARSE("1 - 2 - 3", "(- (- 1 2) 3)");
    ASSERT_SAME_PARSE("1 * 2 * 3", "(stars (stars 1 2 1) 3 1)");
    ASSERT_SAME_PARSE("1 / 2 / 3", "(/ (/ 1 2) 3)");
    ASSERT_SAME_PARSE("1 % 2 % 3", "(% (% 1 2) 3)");
    ASSERT_SAME_PARSE("1 ** 2 ** 3", "(stars 1 (stars 2 3 2) 2)");

    // Same precedence
    ASSERT_SAME_PARSE("1 + 2 - 3", "(- (+ 1 2) 3)");
    ASSERT_SAME_PARSE("1 - 2 + 3", "(+ (- 1 2) 3)");
    ASSERT_SAME_PARSE("1 * 2 / 3", "(/ (stars 1 2 1) 3)");
    ASSERT_SAME_PARSE("1 / 2 * 3", "(stars (/ 1 2) 3 1)");
    ASSERT_SAME_PARSE("1 * 2 % 3", "(% (stars 1 2 1) 3)");
    ASSERT_SAME_PARSE("1 % 2 * 3", "(stars (% 1 2) 3 1)");
    ASSERT_SAME_PARSE("1 % 2 / 3", "(/ (% 1 2) 3)");
    ASSERT_SAME_PARSE("1 / 2 % 3", "(% (/ 1 2) 3)");

    // Different precedence
    ASSERT_SAME_PARSE("1 + 2 * 3", "(+ 1 (stars 2 3 1))");
    ASSERT_SAME_PARSE("1 * 2 + 3", "(+ (stars 1 2 1) 3)");
    ASSERT_SAME_PARSE("1 + 2 / 3", "(+ 1 (/ 2 3))");
    ASSERT_SAME_PARSE("1 / 2 + 3", "(+ (/ 1 2) 3)");
    ASSERT_SAME_PARSE("1 + 2 % 3", "(+ 1 (% 2 3))");
    ASSERT_SAME_PARSE("1 % 2 + 3", "(+ (% 1 2) 3)");
    ASSERT_SAME_PARSE("1 + 2 ** 3", "(+ 1 (stars 2 3 2))");
    ASSERT_SAME_PARSE("1 ** 2 + 3", "(+ (stars 1 2 2) 3)");
    ASSERT_SAME_PARSE("1 - 2 * 3", "(- 1 (stars 2 3 1))");
    ASSERT_SAME_PARSE("1 * 2 - 3", "(- (stars 1 2 1) 3)");
    ASSERT_SAME_PARSE("1 - 2 / 3", "(- 1 (/ 2 3))");
    ASSERT_SAME_PARSE("1 / 2 - 3", "(- (/ 1 2) 3)");
    ASSERT_SAME_PARSE("1 - 2 % 3", "(- 1 (% 2 3))");
    ASSERT_SAME_PARSE("1 % 2 - 3", "(- (% 1 2) 3)");
    ASSERT_SAME_PARSE("1 - 2 ** 3", "(- 1 (stars 2 3 2))");
    ASSERT_SAME_PARSE("1 ** 2 - 3", "(- (stars 1 2 2) 3)");
    ASSERT_SAME_PARSE("1 * 2 ** 3", "(stars 1 (stars 2 3 2) 1)");
    ASSERT_SAME_PARSE("1 ** 2 * 3", "(stars (stars 1 2 2) 3 1)");
    ASSERT_SAME_PARSE("1 / 2 ** 3", "(/ 1 (stars 2 3 2))");
    ASSERT_SAME_PARSE("1 ** 2 / 3", "(/ (stars 1 2 2) 3)");
    ASSERT_SAME_PARSE("1 % 2 ** 3", "(% 1 (stars 2 3 2))");
    ASSERT_SAME_PARSE("1 ** 2 % 3", "(% (stars 1 2 2) 3)");
}

TEST(parse_binary_bitwise) {
    // Associativity
    ASSERT_SAME_PARSE("1 & 2 & 3", "(& (& 1 2) 3)");
    ASSERT_SAME_PARSE("1 | 2 | 3", "(| (| 1 2) 3)");
    ASSERT_SAME_PARSE("1 ^ 2 ^ 3", "(^ (^ 1 2) 3)");
    ASSERT_SAME_PARSE("1 << 2 << 3", "(<< (<< 1 2) 3)");
    ASSERT_SAME_PARSE("1 >> 2 >> 3", "(>> (>> 1 2) 3)");
    ASSERT_SAME_PARSE("1 /< 2 /< 3", "(/< (/< 1 2) 3)");
    ASSERT_SAME_PARSE("1 /> 2 /> 3", "(/> (/> 1 2) 3)");

    // Same precedence
    ASSERT_SAME_PARSE("1 << 2 >> 3", "(>> (<< 1 2) 3)");
    ASSERT_SAME_PARSE("1 >> 2 << 3", "(<< (>> 1 2) 3)");
    ASSERT_SAME_PARSE("1 /< 2 >> 3", "(>> (/< 1 2) 3)");
    ASSERT_SAME_PARSE("1 >> 2 /< 3", "(/< (>> 1 2) 3)");
    ASSERT_SAME_PARSE("1 << 2 /> 3", "(/> (<< 1 2) 3)");
    ASSERT_SAME_PARSE("1 /> 2 << 3", "(<< (/> 1 2) 3)");
    ASSERT_SAME_PARSE("1 /< 2 /> 3", "(/> (/< 1 2) 3)");
    ASSERT_SAME_PARSE("1 /> 2 /< 3", "(/< (/> 1 2) 3)");

    // Different precedence
    ASSERT_SAME_PARSE("1 ^ 2 & 3", "(^ 1 (& 2 3))");
    ASSERT_SAME_PARSE("1 & 2 ^ 3", "(^ (& 1 2) 3)");
    ASSERT_SAME_PARSE("1 | 2 & 3", "(| 1 (& 2 3))");
    ASSERT_SAME_PARSE("1 & 2 | 3", "(| (& 1 2) 3)");
    ASSERT_SAME_PARSE("1 | 2 ^ 3", "(| 1 (^ 2 3))");
    ASSERT_SAME_PARSE("1 ^ 2 | 3", "(| (^ 1 2) 3)");
    ASSERT_SAME_PARSE("1 & 2 << 3", "(& 1 (<< 2 3))");
    ASSERT_SAME_PARSE("1 << 2 & 3", "(& (<< 1 2) 3)");
    ASSERT_SAME_PARSE("1 ^ 2 << 3", "(^ 1 (<< 2 3))");
    ASSERT_SAME_PARSE("1 << 2 ^ 3", "(^ (<< 1 2) 3)");
    ASSERT_SAME_PARSE("1 | 2 << 3", "(| 1 (<< 2 3))");
    ASSERT_SAME_PARSE("1 << 2 | 3", "(| (<< 1 2) 3)");
}

TEST(parse_if_statement) {
    ASSERT_SAME_PARSE("if foo: bar", "(if foo bar)");
    ASSERT_SAME_PARSE("if foo: bar\nelse: baz", "(if_else foo bar baz)");

    ASSERT_SAME_PARSE(R"(
if foo:
    bar
)", "(if foo (do bar))");

    ASSERT_SAME_PARSE(R"(
if foo:
    bar
    baz
)", "(if foo (do bar baz))");

    ASSERT_SAME_PARSE(R"(
if foo:
    bar
else:
    baz
)", "(if_else foo (do bar) (do baz))");

    ASSERT_SAME_PARSE(R"(
if foo:
    bar
else if baz:
    quux
)", "(if_else foo (do bar) (if baz (do quux)))");

    ASSERT_SAME_PARSE(R"(
if foo:
    bar
else:
    if baz:
        quux
)", "(if_else foo (do bar) (do (if baz (do quux))))");

    ASSERT_SAME_PARSE(R"(
if foo:
    bar
else: if baz:
    quux
else: xyzzy
)", "(if_else foo (do bar) (if_else baz (do quux) xyzzy))");

    ASSERT_SAME_PARSE(R"(
if foo:
    if bar:
        1
    else: 2
else if baz: 3
else:
    4
)", "(if_else foo (do (if_else bar (do 1) 2)) (if_else baz 3 (do 4)))");
}

TEST(parse_unless_statement) {
    ASSERT_SAME_PARSE("unless foo: bar", "(if (not foo) bar)");

    ASSERT_SAME_PARSE(R"(
unless foo:
    bar
    baz
)", "(if (not foo) (do bar baz))");

    ASSERT_SAME_PARSE(R"(
unless foo:
    unless bar:
        baz
)", "(if (not foo) (do (if (not bar) (do baz))))");
}

TEST(parse_while_statement) {
    ASSERT_SAME_PARSE("while foo: bar", "(while foo bar)");

    ASSERT_SAME_PARSE(R"(
while foo:
    bar
    baz
)", "(while foo (do bar baz))");

    ASSERT_SAME_PARSE(R"(
while foo:
    while bar:
        baz
)", "(while foo (do (while bar (do baz))))");
}

TEST(parse_until_statement) {
    ASSERT_SAME_PARSE("until foo: bar", "(while (not foo) bar)");

    ASSERT_SAME_PARSE(R"(
until foo:
    bar
    baz
)", "(while (not foo) (do bar baz))");

    ASSERT_SAME_PARSE(R"(
until foo:
    until bar:
        baz
)", "(while (not foo) (do (while (not bar) (do baz))))");
}

TEST(parse_inline_if_or_else) {
    ASSERT_SAME_PARSE("foo if bar", "(if bar foo)");
    ASSERT_SAME_PARSE("foo if bar else baz", "(ternary bar foo baz)");
    ASSERT_SAME_PARSE("foo if bar if baz", "(if baz (if bar foo))");
}

TEST(parse_inline_if_precedence) {
    ASSERT_SAME_PARSE("x = 1 if y", "(if y (= x 1))");
    ASSERT_SAME_PARSE("x = 1 if y else 2", "(= x (ternary y 1 2))");
}

TEST(parse_inline_unless) {
    ASSERT_SAME_PARSE("foo unless bar", "(if (not bar) foo)");
    ASSERT_SAME_PARSE("x = 1 unless foo", "(if (not foo) (= x 1))");
    ASSERT_SAME_PARSE("foo unless bar unless baz", "(if (not baz) (if (not bar) foo))");
}

TEST(parse_inline_while) {
    ASSERT_SAME_PARSE("foo while bar", "(while bar foo)");
    ASSERT_SAME_PARSE("x = 1 while bar", "(while bar (= x 1))");
    ASSERT_SAME_PARSE("foo while bar while baz", "(while baz (while bar foo))");
}

TEST(parse_inline_until) {
    ASSERT_SAME_PARSE("foo until bar", "(while (not bar) foo)");
    ASSERT_SAME_PARSE("x = 1 until bar", "(while (not bar) (= x 1))");
    ASSERT_SAME_PARSE("foo until bar until baz", "(while (not baz) (while (not bar) foo))");
}

TEST(parse_inline_hailstone) {
    ASSERT_SAME_PARSE("(x /= 2) if x % 2 == 0 else (x = 3 * x + 1) while x != 1", "(while (!= x 1) (ternary (== (% x 2) 0) (paren (/= x 2)) (paren (= x (+ (stars 3 x 1) 1)))))");
}

TEST(parse_typed_vardecl) {
    ASSERT_SAME_PARSE("int x: 1", "(var int x 1)");
    ASSERT_SAME_PARSE("int x", "(var int x missing)");
    ASSERT_SAME_PARSE("int x, y, z", "(do (var int x missing) (var int y missing) (var int z missing))");
    ASSERT_SAME_PARSE("int x, y: x, z, w: 2", "(do (var int x missing) (var int y x) (var int z missing) (var int w 2))");
}

TEST(parse_untyped_vardecl) {
    ASSERT_SAME_PARSE("var x: 1", "(var missing x 1)");
    ASSERT_SAME_PARSE("var x", "(var missing x missing)");
    ASSERT_SAME_PARSE("var x, y, z", "(do (var missing x missing) (var missing y missing) (var missing z missing))");
    ASSERT_SAME_PARSE("var x: 1, y: \"a\", z: foo", "(do (var missing x 1) (var missing y \"a\") (var missing z foo))");
}

TEST(parse_typed_fundecl) {
    ASSERT_SAME_PARSE("int f()", "(fun int f (tuple) missing missing)");
    ASSERT_SAME_PARSE("int f(): 42", "(fun int f (tuple) missing 42)");
    ASSERT_SAME_PARSE("int f(int x): x", "(fun int f (tuple (var int x missing)) missing x)");
    ASSERT_SAME_PARSE("int f(int x, int y): x + y", "(fun int f (tuple (var int x missing) (var int y missing)) missing (+ x y))");
    ASSERT_SAME_PARSE("int(f) f(int(int, int) x): f(f)", "(fun (call int f) f (tuple (var (call int int int) x missing)) missing (call f f))");
}

TEST(parse_untyped_fundecl) {
    ASSERT_SAME_PARSE("fun f()", "(fun missing f (tuple) missing missing)");
    ASSERT_SAME_PARSE("fun f(): 42", "(fun missing f (tuple) missing 42)");
    ASSERT_SAME_PARSE("fun f(x): x", "(fun missing f (tuple (var missing x missing)) missing x)");
    ASSERT_SAME_PARSE("fun f(int x, y): x + y", "(fun missing f (tuple (var int x missing) (var missing y missing)) missing (+ x y))");
    ASSERT_SAME_PARSE("fun f(x, int y): x + y", "(fun missing f (tuple (var missing x missing) (var int y missing)) missing (+ x y))");
}

TEST(parse_fundecl_default_parameters) {
    ASSERT_SAME_PARSE("fun f(int x: 42): x", "(fun missing f (tuple (var int x 42)) missing x)");
    ASSERT_SAME_PARSE("fun f(x: 42): x", "(fun missing f (tuple (var missing x 42)) missing x)");
    ASSERT_SAME_PARSE("fun f(int x: 42, int y: 43): x", "(fun missing f (tuple (var int x 42) (var int y 43)) missing x)");
    ASSERT_SAME_PARSE("fun f(int x, int y: 43): x", "(fun missing f (tuple (var int x missing) (var int y 43)) missing x)");
    ASSERT_SAME_PARSE("fun f(int x, int y): x", "(fun missing f (tuple (var int x missing) (var int y missing)) missing x)");
}

TEST(parse_named_typedecl) {
    ASSERT_SAME_PARSE("type Foo", "(named Foo missing)");
    ASSERT_SAME_PARSE("type Foo: int", "(named Foo int)");
    ASSERT_SAME_PARSE(R"(
type Foo:
    int
)", "(named Foo int)");
}

TEST(parse_ptr_type) {
    ASSERT_SAME_PARSE("T*", "(ptr_type T)");
    ASSERT_SAME_PARSE("T**", "(ptr_type (ptr_type T))");
    ASSERT_SAME_PARSE("T***", "(ptr_type (ptr_type (ptr_type T)))");
    ASSERT_SAME_PARSE("foo(T*)", "(call foo (ptr_type T))");
    ASSERT_SAME_PARSE("foo(T*, x, T**)", "(call foo (ptr_type T) x (ptr_type (ptr_type T)))");
    ASSERT_SAME_PARSE("T* * x + U*", "(+ (stars T x 1 1) (ptr_type U))");
    ASSERT_SAME_PARSE("T* * x", "(stars T x 1 1)");
    ASSERT_SAME_PARSE("T* * *U** ** *V", "(stars T (stars U V 2 2 1) 1 1 1)");
}

TEST(parse_get_index) {
    ASSERT_SAME_PARSE("x[0]", "(get_index x 0)");
    ASSERT_SAME_PARSE("x[0][1]", "(get_index (get_index x 0) 1)");
    ASSERT_SAME_PARSE("x[a][b][c]", "(get_index (get_index (get_index x a) b) c)");
}

TEST(parse_get_indices) {
    ASSERT_SAME_PARSE("x[0, 1, 2]", "(get_indices x 0 1 2)");
    ASSERT_SAME_PARSE("x[0, y[1]]", "(get_indices x 0 (get_index y 1))");
    ASSERT_SAME_PARSE("x[0, y[1, z[w]]]", "(get_indices x 0 (get_indices y 1 (get_index z w)))");
    ASSERT_SAME_PARSE("x[0][1, 2]", "(get_indices (get_index x 0) 1 2)");
    ASSERT_SAME_PARSE("x[0, 1][2]", "(get_index (get_indices x 0 1) 2)");
    ASSERT_SAME_PARSE("[0, 1, 2][0, 1, 2]", "(get_indices (list 0 1 2) 0 1 2)");
}

TEST(parse_get_field) {
    ASSERT_SAME_PARSE("x.y", "(get_field x y)");
    ASSERT_SAME_PARSE("x.y.z", "(get_field (get_field x y) z)");
}

TEST(parse_call_unnamed_parameters) {
    ASSERT_SAME_PARSE("f()", "(call f)");
    ASSERT_SAME_PARSE("f(x)", "(call f x)");
    ASSERT_SAME_PARSE("f(x, y)", "(call f x y)");
    ASSERT_SAME_PARSE("f(0, x, 1, y, f(z))", "(call f 0 x 1 y (call f z))");
}

TEST(parse_call_named_parameters) {
    ASSERT_SAME_PARSE("f(x: 1)", "(call f (named_parameter x 1))");
    ASSERT_SAME_PARSE("f(x: 1, y: 2)", "(call f (named_parameter x 1) (named_parameter y 2))");
    ASSERT_SAME_PARSE("f(x: 1, y: 2, z: 3)", "(call f (named_parameter x 1) (named_parameter y 2) (named_parameter z 3))");
    ASSERT_SAME_PARSE("f(1, y: 2, z: 3)", "(call f 1 (named_parameter y 2) (named_parameter z 3))");
    ASSERT_SAME_PARSE("f(x: 1, 2, z: 3)", "(call f (named_parameter x 1) 2 (named_parameter z 3))");
    ASSERT_SAME_PARSE("f(x: 1, y: 2, 3)", "(call f (named_parameter x 1) (named_parameter y 2) 3)");
    ASSERT_SAME_PARSE("f(x, y: 2, z: 3)", "(call f x (named_parameter y 2) (named_parameter z 3))");
    ASSERT_SAME_PARSE("f(x: 1, y, z: 3)", "(call f (named_parameter x 1) y (named_parameter z 3))");
    ASSERT_SAME_PARSE("f(x: 1, y: 2, z)", "(call f (named_parameter x 1) (named_parameter y 2) z)");
}

TEST(parse_call_method) {
    ASSERT_SAME_PARSE("x.f()", "(call_method f x)");
    ASSERT_SAME_PARSE("x.f(y)", "(call_method f x y)");
    ASSERT_SAME_PARSE("(42).f()", "(call_method f (paren 42))");
    ASSERT_SAME_PARSE("(x.f)()", "(call (paren (get_field x f)))");
    ASSERT_SAME_PARSE("x.f().g()", "(call_method g (call_method f x))");
    ASSERT_SAME_PARSE("x.f().g()(x, f, g)", "(call (call_method g (call_method f x)) x f g)");
    ASSERT_SAME_PARSE("f(x.g())", "(call f (call_method g x))");
}

TEST(parse_constructor_prefix) {
    ASSERT_SAME_PARSE("i32(x)", "(call i32 x)");
    ASSERT_SAME_PARSE("i32*(x)", "(stars i32 (paren x) 1)");
    ASSERT_SAME_PARSE("i32[](x)", "(call (slice_type i32) x)");
    ASSERT_SAME_PARSE("i32[4](x)", "(call (get_index i32 4) x)");
    ASSERT_SAME_PARSE("(i32)(x)", "(call (paren i32) x)");
    ASSERT_SAME_PARSE("(i32, i32)(x)", "(call (tuple i32 i32) x)");
    ASSERT_SAME_PARSE("i32(i32)(x)", "(call (call i32 i32) x)");
    ASSERT_SAME_PARSE("i32(i32, i32)(x)", "(call (call i32 i32 i32) x)");
}

TEST(parse_constructor_method) {
    ASSERT_SAME_PARSE("x.i32()", "(call_method i32 x)");
    ASSERT_SAME_PARSE("x.i32*()", "(stars (get_field x i32) (tuple) 1)");
    ASSERT_SAME_PARSE("x.i32(i32)()", "(call (call_method i32 x i32))");
}

TEST(parse_return_no_parameter) {
    ASSERT_SAME_PARSE("return", "(return missing)");
    ASSERT_SAME_PARSE("return if true", "(if true (return missing))");
    ASSERT_SAME_PARSE("return\n42", "(return missing) 42");
}

TEST(parse_return_stmt) {
    ASSERT_SAME_PARSE("return 1", "(return 1)");
    ASSERT_SAME_PARSE("return 1 then return x", "(then (return 1) (return x))");
    ASSERT_SAME_PARSE(R"(
var x: 1
x = x + 1
return x
)", "(var missing x 1) (= x (+ x 1)) (return x)");
}

TEST(parse_length_bitor) {
    ASSERT_SAME_PARSE("|x| | |y| | z | w", "(| (| (| (length x) (length y)) z) w)");
}

TEST(parse_coefficient_expr) {
    ASSERT_SAME_PARSE("3x", "(* 3 x)");
    ASSERT_SAME_PARSE("5x + 6y", "(+ (* 5 x) (* 6 y))");
    ASSERT_SAME_PARSE("0.5x + 0.6y", "(+ (* 0.5 x) (* 0.6 y))");
}

TEST(parse_incr_decr_precedence) {
    ASSERT_SAME_PARSE("++ x --", "(post_decr (pre_incr x))");
    ASSERT_SAME_PARSE("-- x ++", "(post_incr (pre_decr x))");
    ASSERT_SAME_PARSE("++ x[0]", "(pre_incr (get_index x 0))");
    ASSERT_SAME_PARSE("x[0] ++", "(post_incr (get_index x 0))");
    ASSERT_SAME_PARSE("-- x[0]", "(pre_decr (get_index x 0))");
    ASSERT_SAME_PARSE("x[0] --", "(post_decr (get_index x 0))");
    ASSERT_SAME_PARSE("++ x.y", "(pre_incr (get_field x y))");
    ASSERT_SAME_PARSE("x.y ++", "(post_incr (get_field x y))");
    ASSERT_SAME_PARSE("-- x.y", "(pre_decr (get_field x y))");
    ASSERT_SAME_PARSE("x.y --", "(post_decr (get_field x y))");
    ASSERT_SAME_PARSE("++ *x", "(pre_incr (deref x))");
    ASSERT_SAME_PARSE("*x ++", "(post_incr (deref x))");
    ASSERT_SAME_PARSE("-- *x", "(pre_decr (deref x))");
    ASSERT_SAME_PARSE("*x --", "(post_decr (deref x))");
}

TEST(parse_own_uninit_types) {
    ASSERT_SAME_PARSE("own i32*", "(own (ptr_type i32))");
    ASSERT_SAME_PARSE("own i32[]", "(own (slice_type i32))");
    ASSERT_SAME_PARSE("own (i32, i32)[4]*", "(own (ptr_type (get_index (tuple i32 i32) 4)))");
    ASSERT_SAME_PARSE("own i32*(i32)", "(own (stars i32 (paren i32) 1))");
    ASSERT_SAME_PARSE("uninit i32*", "(uninit (ptr_type i32))");
    ASSERT_SAME_PARSE("uninit i32[]", "(uninit (slice_type i32))");
    ASSERT_SAME_PARSE("uninit (i32, i32)[4]*", "(uninit (ptr_type (get_index (tuple i32 i32) 4)))");
    ASSERT_SAME_PARSE("uninit i32*(i32)", "(uninit (stars i32 (paren i32) 1))");
    ASSERT_SAME_PARSE("own uninit i32*", "(own (uninit (ptr_type i32)))");
    ASSERT_SAME_PARSE("uninit own i32*", "(uninit (own (ptr_type i32)))");
}

TEST(parse_new_expr) {
    ASSERT_SAME_PARSE("new 42", "(new 42)");
    ASSERT_SAME_PARSE("new (42)", "(new (paren 42))");
    ASSERT_SAME_PARSE("new i32(42)", "(new (call i32 42))");
    ASSERT_SAME_PARSE("new i32*(42)", "(new (stars i32 (paren 42) 1))");
    ASSERT_SAME_PARSE("new i32[42]", "(new (get_index i32 42))");
    ASSERT_SAME_PARSE("new i32[42](1, 2, 3)", "(new (call (get_index i32 42) 1 2 3))");
    ASSERT_SAME_PARSE("new i32*[42](&x, &y, &z)", "(new (stars i32 (call (list 42) (address_of x) (address_of y) (address_of z)) 1))");
}

TEST(parse_case_decl) {
    ASSERT_SAME_PARSE(R"(
type Foo:
    case Bar
    case Baz: i32 x
    case Quux:
        case A
        case B
)", "(union Foo (named_case Bar missing) (struct_case Baz (var i32 x missing)) (union_case Quux (named_case A missing) (named_case B missing)))");
}

TEST(parse_match) {
    ASSERT_SAME_PARSE(R"(
match x:
    case 1:
        1
    case Foo(x):
        x.print()
        x + 1
    case [1, 2, 3]: x - 1
    else: 42
)", "(match x (case 1 (do 1)) (case (call Foo x) (do (call_method print x) (+ x 1))) (case (list 1 2 3) (- x 1)) (case missing 42))");
}

TEST(parse_splat) {
    ASSERT_SAME_PARSE("...", "(... missing)");
    ASSERT_SAME_PARSE("xs...", "(... xs)");
    ASSERT_SAME_PARSE("[prefix..., 42, suffix...]", "(list (... prefix) 42 (... suffix))");
}

TEST(parse_statement_chaining) {
    ASSERT_SAME_PARSE("if a while b unless c until d: 42", "(if a (while b (if (not c) (while (not d) 42))))");
}

TEST(parse_use) {
    ASSERT_SAME_PARSE("use foo", "(use_module foo)");
    ASSERT_SAME_PARSE("use foo/bar", "(use_module (get_field foo bar))");
    ASSERT_SAME_PARSE("use foo.bar", "(use_type (get_field foo bar))");
    ASSERT_SAME_PARSE("use foo.bar as f", "(use_type (as (get_field foo bar) f))");
    ASSERT_SAME_PARSE("use foo.bar, foo/bar, baz as b", "(do (use_type (get_field foo bar)) (use_module (get_field foo bar)) (use_module (as baz b)))");
    ASSERT_SAME_PARSE("use foo.bar.*", "(use_type (get_field (get_field foo bar) wildcard))");
    ASSERT_SAME_PARSE("use foo/bar/*", "(use_module (get_field (get_field foo bar) wildcard))");
}

TEST(parse_export) {
    ASSERT_SAME_PARSE("export var x: 1", "(export (var missing x 1))");
    ASSERT_SAME_PARSE("export type Baz", "(export (named Baz missing))");
    ASSERT_SAME_PARSE("export type Foo: i32 x", "(export (struct Foo (var i32 x missing)))");
    ASSERT_SAME_PARSE("export alias Bar: Foo", "(export (alias Bar Foo))");
    ASSERT_SAME_PARSE("export i32 x", "(export (var i32 x missing))");
    ASSERT_SAME_PARSE("export i32 f(i32)", "(export (fun i32 f (tuple (var missing i32 missing)) missing missing))");
}

TEST(parse_generic_function) {
    ASSERT_SAME_PARSE("fun id(type T, T x): x", "(fun missing id (tuple (alias T missing) (var T x missing)) missing x)");
}

TEST(parse_binary_as) {
    ASSERT_SAME_PARSE("1 + 2 as i32", "(+ 1 (construct i32 2))");
}

TEST(parse_bad_escape_sequence) {
    EXPECT_ERRORS;

    auto badEscape = PARSE("\"abc\\m\"");
    ASSERT_DID_ERROR(badEscape);

    auto badHex = PARSE("\"abc\\x1g\"");
    ASSERT_DID_ERROR(badHex);

    auto badUnicode = PARSE("\"abc\\u12h3\"");
    ASSERT_DID_ERROR(badUnicode);

    auto badUnicodeScoped = PARSE("\"abc\\u{r12345}\"");
    ASSERT_DID_ERROR(badUnicodeScoped);
}

TEST(parse_bad_tuple) {
    EXPECT_ERRORS;

    auto badSep = PARSE("(a, b, c; d)");
    ASSERT_DID_ERROR(badSep);

    auto missingSep = PARSE("(a, b c)");
    ASSERT_DID_ERROR(missingSep);

    auto extraSep = PARSE("(a, b, c,)");
    ASSERT_DID_ERROR(extraSep);

    auto unterminated = PARSE("(a, b, c");
    ASSERT_DID_ERROR(unterminated);
}

TEST(parse_bad_list) {
    EXPECT_ERRORS;

    auto badSep = PARSE("[a, b, c; d]");
    ASSERT_DID_ERROR(badSep);

    auto missingSep = PARSE("[a b c]");
    ASSERT_DID_ERROR(missingSep);

    auto extraSep = PARSE("[a, b, c,]");
    ASSERT_DID_ERROR(extraSep);

    auto unterminated = PARSE("[a, b, c");
    ASSERT_DID_ERROR(unterminated);
}