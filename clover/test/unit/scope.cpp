#include "clover/test/unit/helpers.h"

TEST(scope_simple_function) {
    auto artifact = SCOPE("fun foo(int x): int y then y + 1");
    auto topLevel = artifact.as<Module>()->getTopLevel();
    auto func = topLevel.child(0);

    ASSERT(topLevel.scope()->find(SYM("foo")));
    ASSERT(func.scope()->find(SYM("x")));
    ASSERT(func.scope()->find(SYM("y")));
}

TEST(scope_nested_function) {
    auto artifact = SCOPE(R"(
fun foo(int x):
    int y
    fun bar(int z):
        x + y + z
)");
    auto topLevel = artifact.as<Module>()->getTopLevel();
    auto foo = topLevel.child(0);
    auto fooBody = foo.child(4);
    auto bar = fooBody.child(1);

    ASSERT(topLevel.scope()->find(SYM("foo")));
    ASSERT(foo.scope()->find(SYM("x")));
    ASSERT(fooBody.scope()->find(SYM("y")));
    ASSERT(fooBody.scope()->find(SYM("bar")));
    ASSERT(bar.scope()->find(SYM("x")));
    ASSERT(bar.scope()->find(SYM("y")));
    ASSERT(bar.scope()->find(SYM("z")));
    ASSERT(bar.scope()->find(SYM("foo")));
}

TEST(scope_overloaded_function) {
    auto artifact = SCOPE(R"(
fun foo(i32 x): x
fun foo(f32 x): x

fun bar():
    fun foo(i32[] xs): xs
    foo(42)
)");
}

TEST(scope_bad_duplicate_global) {
    EXPECT_ERRORS;
    auto dupVars = SCOPE(R"(
int x
int y
int x
int z
)");
    ASSERT_DID_ERROR(dupVars);

    auto dupTypes = SCOPE(R"(
type Foo
alias Foo: i32
type Foo: i32
type Bar: f32
)");
    ASSERT_DID_ERROR(dupTypes);
}

TEST(scope_bad_duplicate_local) {
    EXPECT_ERRORS;
    auto dupVars = SCOPE(R"(
fun foo():
    var x: 1, y: 2
    var x: 2
)");
    ASSERT_DID_ERROR(dupVars);

    auto dupVarsSameDecl = SCOPE(R"(
fun foo():
    var x: 1, y: 2, x: 3
)");
    ASSERT_DID_ERROR(dupVarsSameDecl);

    auto dupParams = SCOPE(R"(
fun foo(i32 x, i32 y, i32 z, i32 x):
    var bar
)");
    ASSERT_DID_ERROR(dupParams);

    auto dupVarAndParam = SCOPE(R"(
fun foo(i32 x):
    var x: 1
)");
    ASSERT_DID_ERROR(dupVarAndParam);

    auto dupFuncAndParam = SCOPE(R"(
fun foo(i32 foo):
    foo + 1
)");

    ASSERT_NO_ERRORS(dupFuncAndParam);
}