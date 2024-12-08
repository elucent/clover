#include "util/test/harness.h"
#include "util/func.h"

TEST(boxed_func_identity) {
    func<i32(i32)> identity = [](i32 i) -> i32 { return i; };
    ASSERT_EQUAL(identity(1), 1);
    ASSERT_EQUAL(identity(42), 42);
}

TEST(boxed_func_copy) {
    func<i32(i32)> inc = [](i32 i) -> i32 { return i + 1; };
    func<i32(i32)> inc2 = inc;
    ASSERT_EQUAL(inc(2), 3);
    ASSERT_EQUAL(inc2(2), 3);
}

TEST(boxed_func_move) {
    func<i32(i32)> twice = [](i32 i) -> i32 { return i * 2; };
    func<i32(i32)> twice2 = move(twice);
    ASSERT_EQUAL(twice2(3), 6);
}

struct CounterWorker {
    func<i32()> counter;
};

TEST(boxed_func_shared_tally) {
    i32 count = 0;
    func<i32()> counter = [&]() -> i32 { return ++ count; };
    vec<CounterWorker> workers;
    for (u32 i = 0; i < 64; i ++)
        workers.push({ counter });
    for (u32 i = 0; i < 4096; i ++)
        workers[i % 64].counter();
    ASSERT_EQUAL(count, 4096);
}

TEST(boxed_func_currying) {
    func<func<i32(i32)>(i32)> add = [](i32 x) -> auto {
        return [=](i32 y) -> i32 {
            return x + y;
        };
    };

    ASSERT_EQUAL(add(1)(2), 3);

    auto identity = add(0);
    ASSERT_EQUAL(identity(1), 1);
    ASSERT_EQUAL(identity(2), 2);
    ASSERT_EQUAL(identity(3), 3);

    auto inc = add(1);
    ASSERT_EQUAL(inc(42), 43);
    ASSERT_EQUAL(inc(inc(1)), 3);
}
