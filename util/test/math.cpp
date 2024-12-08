#include "util/test/harness.h"
#include "util/io.h"
#include "util/math.h"

TEST(integer_reciprocal_division) {
    auto divisionBy3 = multiplierAndShiftForDiv(3);
    auto divisionBy5 = multiplierAndShiftForDiv(5);
    auto divisionBy39 = multiplierAndShiftForDiv(39);
    auto divisionBy48 = multiplierAndShiftForDiv(48);
    auto divisionBy1729 = multiplierAndShiftForDiv(1729);
    for (u32 i = 0; i < 1u << 31; i += 63) {
        ASSERT_EQUAL(divideUsingIntegerReciprocal(i, divisionBy3), i / 3);
        ASSERT_EQUAL(divideUsingIntegerReciprocal(i, divisionBy5), i / 5);
        ASSERT_EQUAL(divideUsingIntegerReciprocal(i, divisionBy39), i / 39);
        ASSERT_EQUAL(divideUsingIntegerReciprocal(i, divisionBy48), i / 48);
        ASSERT_EQUAL(divideUsingIntegerReciprocal(i, divisionBy1729), i / 1729);
    }
}