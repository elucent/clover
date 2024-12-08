#include "util/test/harness.h"
#include "util/io.h"
#include "util/test/format.h"

TEST(format_float_zero) {
    ASSERT_FORMAT_EQUAL("0.0", 0.0);
}

TEST(format_float_fractional) {
    ASSERT_FORMAT_EQUAL("0.2", 0.2);
    ASSERT_FORMAT_EQUAL("0.01", 0.01);
    ASSERT_FORMAT_EQUAL("0.037501", 0.037501);
}

TEST(format_float_integral) {
    ASSERT_FORMAT_EQUAL("42.0", 42.0);
    ASSERT_FORMAT_EQUAL("40501.0", 40501.0);
    ASSERT_FORMAT_EQUAL("999888777.0", 999888777.0);
}

TEST(format_float_random) {
    ASSERT_FORMAT_EQUAL("10000.00001", 10000.00001);
    ASSERT_FORMAT_EQUAL("12.5", 12.5);
    ASSERT_FORMAT_EQUAL("1.020304", 1.020304);
    ASSERT_FORMAT_EQUAL("3.14159", 3.14159);
}

TEST(format_float_positive_rounding) {
    static_assert(FP_PRECISION == 7); // Assume we have exactly 7 digits of precision; otherwise we need to update this test.
    ASSERT_FORMAT_EQUAL("0.0", 0.0000000001);
    ASSERT_FORMAT_EQUAL("1.0000001", 1.0000000999);
    ASSERT_FORMAT_EQUAL("1.0", 1.00000000999);
    ASSERT_FORMAT_EQUAL("1.999999", 1.999999);
    ASSERT_FORMAT_EQUAL("1.9999999", 1.9999999);
    ASSERT_FORMAT_EQUAL("2.0", 1.99999999);
}

TEST(format_float_negative_rounding) {
    static_assert(FP_PRECISION == 7);
    ASSERT_FORMAT_EQUAL("-0.0", -0.0000000001);
    ASSERT_FORMAT_EQUAL("-1.0000001", -1.0000000999);
    ASSERT_FORMAT_EQUAL("-1.0", -1.00000000999);
    ASSERT_FORMAT_EQUAL("-1.999999", -1.999999);
    ASSERT_FORMAT_EQUAL("-1.9999999", -1.9999999);
    ASSERT_FORMAT_EQUAL("-2.0", -1.99999999);
}

TEST(format_nan) {
    ASSERT_FORMAT_EQUAL("NaN", NaN);
    ASSERT_FORMAT_EQUAL("NaN", NaNf);
}

TEST(format_negative_nan) {
    ASSERT_FORMAT_EQUAL("-NaN", -NaN);
    ASSERT_FORMAT_EQUAL("-NaN", -NaNf);
}

TEST(format_infinity) {
    ASSERT_FORMAT_EQUAL("Infinity", Infinity);
    ASSERT_FORMAT_EQUAL("Infinity", Infinityf);
}

TEST(format_negative_infinity) {
    ASSERT_FORMAT_EQUAL("-Infinity", -Infinity);
    ASSERT_FORMAT_EQUAL("-Infinity", -Infinityf);
}