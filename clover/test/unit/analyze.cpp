#include "clover/test/unit/helpers.h"

TEST(analyze_destroy_toplevel_ptr) {
    auto instance = ANALYZE(R"(
var ptr: new 42
)");
}

TEST(analyze_destroy_in_expression) {
    auto instance = ANALYZE(R"(
(*new 42) + (*new 42)
)");
}

TEST(analyze_alloc_in_branch) {
    auto instance = ANALYZE(R"(
var ptr: new 42
var x: 0, y: 10
if x < y:
    ptr = new 43
*ptr
)");
}

TEST(analyze_freed_in_branch) {
    auto instance = ANALYZE(R"(
var ptr: new 42
var x: 0
if x < 10:
    own i32* local: ptr
    *local
)");
}

TEST(analyze_access_uninit) {
    auto instance = ANALYZE(R"(
i32 i: uninit
i = 42
i + 1
)");
}

TEST(analyze_bad_access_uninit) {
    EXPECT_ERRORS;
    auto instance = ANALYZE(R"(
i32 i: uninit
i + 1
)");
    ASSERT_DID_ERROR(instance);
}

TEST(analyze_access_uninit_after_if_else) {
    auto instance = ANALYZE(R"(
i32 i: uninit
i32 j: 0
if j < 10:
    i = 42
else:
    i = 43
i + 1
)");
}

TEST(analyze_bad_access_uninit_in_branch) {
    EXPECT_ERRORS;
    auto instance = ANALYZE(R"(
i32 i: uninit
i32 j: 0
if j < 10:
    i = 42
i + 1
)");
    ASSERT_DID_ERROR(instance);
}

TEST(analyze_bad_freed_in_branch) {
    EXPECT_ERRORS;
    auto instance = ANALYZE(R"(
var ptr: new 42
var x: 0
if x < 10:
    own i32* local: ptr
    *local
*ptr
)");
    ASSERT_DID_ERROR(instance);
}

TEST(analyze_bad_dangle_after_branch) {
    EXPECT_ERRORS;
    auto instance = ANALYZE(R"(
var ptr: new 42     # &own 'a = @1
i64* dangle: ptr    # & 'a
var x: 0, y: 10
if x < y:
    ptr = new 43    # &own
*dangle
)");
    ASSERT_DID_ERROR(instance);
}

TEST(analyze_bad_alloc_in_branch) {
    EXPECT_ERRORS;
    auto instance = ANALYZE(R"(
var ptr: uninit
var x: 0, y: 10
if x < y:
    ptr = new 43
*ptr
)");
    ASSERT_DID_ERROR(instance);
}

TEST(analyze_alloc_in_loop) {
    auto instance = ANALYZE(R"(
var ptr: new 42
var sum: 0
for i < 10:
    sum += *ptr
    ptr = new 42
)");
}

TEST(analyze_alloc_in_loop_uninit) {
    auto instance = ANALYZE(R"(
var ptr: uninit
var sum: 0
for i < 10:
    ptr = new i
    sum += *ptr
sum
)");
}

TEST(analyze_bad_alloc_in_loop_uninit) {
    EXPECT_ERRORS;
    auto instance = ANALYZE(R"(
var ptr: uninit
var sum: 0
for i < 10:
    ptr = new i
    sum += *ptr
sum + *ptr
)");
    ASSERT_DID_ERROR(instance);
}

TEST(analyze_make_linked_list) {
    auto instance = ANALYZE(R"(
type List:
    case Nil
    case Cons:
        i32 car
        own List* cdr
use List.Nil, List.Cons

own List* list: Nil
for i < 10:
    list = new Cons(i, list)
)");
}

// TEST(analyze_return_local_ref) {
//     auto instance = ANALYZE(R"(
// fun returnsLocal():
//     i32 x
//     return &x
// i32* p: returnsLocal()
// )");
//     ASSERT_DID_ERROR(instance);
// }

// TEST(analyze_store_local_ref_to_global) {
//     auto instance = ANALYZE(R"(
// i32* p
// fun storesLocal():
//     i32 x
//     p = &x
// storesLocal()
// )");
//     ASSERT_DID_ERROR(instance);
// }

// TEST(analyze_takes_owned_ref) {
//     auto instance = ANALYZE(R"(
// fun takesOwned(own i32* p):
//     return *p
// takesOwned(new 42)
// )");
//     ASSERT_NO_ERRORS(instance);
// }

// TEST(analyze_mutate_local_aggregate_owner) {
//     auto instance = ANALYZE(R"(
// type Box:
//     own i32* p
// var b: Box(new 42)
// b.p = new 43
// b.p = new 44
// )");
//     ASSERT_NO_ERRORS(instance);
// }

// TEST(analyze_mutate_nonlocal_aggregate_owner) {
//     auto instance = ANALYZE(R"(
// type Box:
//     own i32* p
// fun mutate(Box* b):
//     b.p = new 43
// var b: Box(new 42)
// b.mutate()
// )");
//     ASSERT_NO_ERRORS(instance);
// }

// TEST(analyze_nonlocal_create_dangling_pointer) {
//     auto instance = ANALYZE(R"(
// type Box:
//     own i32* p
// var b: Box(new 42)
// i32* r: b.p
// fun mutate(Box* b):
//     b.p = new 43
// b.mutate()
// *r
// )");
//     ASSERT_DID_ERROR(instance);
// }

// TEST(analyze_copy_aggregate_owner) {
//     auto instance = ANALYZE(R"(
// type Box:
//     own i32* p
// fun transform(Box b):
//     *b.p *= 2
//     return b
// var b: Box(new 42)
// b = b.transform()
// )");
//     ASSERT_NO_ERRORS(instance);
// }

// TEST(analyze_alloc_in_loop) {
//     auto instance = ANALYZE(R"(
// i32 foo():
//     var ptr: new 42, sum: 0
//     while sum < 420:
//         sum += *ptr
//         ptr = new 42
//     return sum
// )");
//     ASSERT_NO_ERRORS(instance);
// }