#include "core/sys.h"
#include "test/harness.h"
#include "core/sys.h"

TEST(memory_map) {
    auto pages = memory_map(1);
    *(i64*)pages.ptr = 42;
    ASSERT(*(i64*)pages.ptr == 42, "Page should still contain 42");
    memory_free(pages);
}