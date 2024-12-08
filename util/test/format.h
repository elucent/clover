#include "util/test/harness.h"

#define ASSERT_FORMAT_EQUAL(result, ...) do { \
    array<i8, sizeof(result) + 32> buffer; \
    ASSERT_EQUAL((prints(buffer, __VA_ARGS__)), cstring(result)); \
    ASSERT_EQUAL((buffer[{0, sizeof(result) - 1}]), cstring(result)); \
} while (false);