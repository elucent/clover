#include "util/test/harness.h"
#include "util/maybe.h"

TEST(maybe_none) {
    auto m = none<i32>();
    ASSERT_EQUAL(!!m, false);
}

TEST(maybe_some) {
    auto m = some<i32>(42);
    ASSERT_EQUAL(!!m, true);
    ASSERT_EQUAL(*m, 42);
}

struct Constructed {
    i32 x;
    Constructed(i32 a, i32 b): x(a + b) {}
};

TEST(maybe_some_ctor) {
    auto m = some<Constructed>(1, 2);
    ASSERT_EQUAL(!!m, true);
    ASSERT_EQUAL(m->x, 3);
}

struct Latch {
    i32* flag;

    Latch(i32* flag_in): flag(flag_in) {
        (*flag) ++;
    }

    PREVENT_COPYING(Latch);

    ~Latch() {
        if (flag)
            (*flag) --;
    }

    Latch(Latch&& other): flag(other.flag) {
        other.flag = nullptr;
    }
};

TEST(maybe_calls_dtor) {
    i32 flag = 0;
    {
        auto latch = some<Latch>(&flag);
        ASSERT_EQUAL(!!latch, true);
        ASSERT_EQUAL(latch->flag, &flag);
        ASSERT_EQUAL(flag, 1);
    }
    ASSERT_EQUAL(flag, 0);
}

TEST(maybe_calls_move_ctor) {
    i32 flag = 0;
    auto outer = none<Latch>();
    {
        auto inner = some<Latch>(&flag);
        ASSERT_EQUAL(!!inner, true);
        ASSERT_EQUAL(!!outer, false);
        ASSERT_EQUAL(inner->flag, &flag);
        ASSERT_EQUAL(flag, 1);
        outer = move(inner);
        ASSERT_EQUAL(!!inner, false);
        ASSERT_EQUAL(!!outer, true);
        ASSERT_EQUAL(outer->flag, &flag);
        ASSERT_EQUAL(flag, 1);
    }
    ASSERT_EQUAL(!!outer, true);
    ASSERT_EQUAL(outer->flag, &flag);
    ASSERT_EQUAL(flag, 1);
}
