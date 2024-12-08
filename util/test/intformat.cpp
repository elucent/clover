#include "util/test/harness.h"
#include "util/io.h"
#include "util/str.h"
#include "util/test/format.h"

TEST(format_decimal_zero) {
    ASSERT_FORMAT_EQUAL("0", 0);
}

TEST(format_small_positive) {
    ASSERT_FORMAT_EQUAL("42", 42);
}

TEST(format_small_negative) {
    ASSERT_FORMAT_EQUAL("-42", -42);
}

TEST(format_large_positive) {
    ASSERT_FORMAT_EQUAL("1234567890", 1234567890);
}

TEST(format_large_negative) {
    ASSERT_FORMAT_EQUAL("-1234567890", -1234567890);
}

TEST(format_hex_zero) {
    ASSERT_FORMAT_EQUAL("00", hex(0));
}

TEST(format_hex_128) {
    ASSERT_FORMAT_EQUAL("80", hex(i8(0x80)));
}

TEST(format_hex_signed_128_padded) {
    ASSERT_FORMAT_EQUAL("ffffff80", hex(i8(0x80), 8));
}

TEST(format_hex_unsigned_128_padded) {
    ASSERT_FORMAT_EQUAL("00000080", hex(u8(0x80), 8));
}