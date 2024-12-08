#include "clover/test/unit/helpers.h"

#undef ANALYZE
#define ANALYZE(...) OwnedArtifact(nullptr); return;

TEST(analyze_top_level) {
    auto instance = ANALYZE(R"(
type Foo:
    i32* a, b, c

var a: 42
var b: &a
var c: [1, 2, 3]
var d: c[:2]
var e: Foo(&a, &a, &a)
)");
}

TEST(analyze_return_local_ref) {
    auto instance = ANALYZE(R"(
fun returnsLocal():
    i32 x
    return &x
i32* p: returnsLocal()
)");
    ASSERT_DID_ERROR(instance);
}

TEST(analyze_store_local_ref_to_global) {
    auto instance = ANALYZE(R"(
i32* p
fun storesLocal():
    i32 x
    p = &x
storesLocal()
)");
    ASSERT_DID_ERROR(instance);
}

TEST(analyze_takes_owned_ref) {
    auto instance = ANALYZE(R"(
fun takesOwned(own i32* p):
    return *p
takesOwned(new 42)
)");
    ASSERT_NO_ERRORS(instance);
}

TEST(analyze_mutate_local_aggregate_owner) {
    auto instance = ANALYZE(R"(
type Box:
    own i32* p
var b: Box(new 42)
b.p = new 43
b.p = new 44
)");
    ASSERT_NO_ERRORS(instance);
}

TEST(analyze_mutate_nonlocal_aggregate_owner) {
    auto instance = ANALYZE(R"(
type Box:
    own i32* p
fun mutate(Box* b):
    b.p = new 43
var b: Box(new 42)
b.mutate()
)");
    ASSERT_NO_ERRORS(instance);
}

TEST(analyze_nonlocal_create_dangling_pointer) {
    auto instance = ANALYZE(R"(
type Box:
    own i32* p
var b: Box(new 42)
i32* r: b.p
fun mutate(Box* b):
    b.p = new 43
b.mutate()
*r
)");
    ASSERT_DID_ERROR(instance);
}

TEST(analyze_copy_aggregate_owner) {
    auto instance = ANALYZE(R"(
type Box:
    own i32* p
fun transform(Box b):
    *b.p *= 2
    return b
var b: Box(new 42)
b = b.transform()
)");
    ASSERT_NO_ERRORS(instance);
}