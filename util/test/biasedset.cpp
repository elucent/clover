#include "util/bits.h"
#include "util/test/harness.h"

TEST(biasedset_add_increasing) {
    biasedset<128> b;
    for (u32 i = 0; i < 4096; i ++) {
        b.on(i);
        ASSERT(b[i]);
        ASSERT_EQUAL(*b.smallest(), 0);
        ASSERT_EQUAL(*b.largest(), i);
    }
    for (u32 i = 0; i < 4096; i ++)
        ASSERT(b[i]);
    u32 item = *b.begin();
    ASSERT_EQUAL(item, 0);

    bool first = true;
    for (u32 i : b) {
        if (!first)
            ASSERT_EQUAL(i, item + 1);
        first = false;
        item = i;
    }
}

TEST(biasedset_add_decreasing) {
    biasedset<128> b;
    for (u32 i = 0; i < 4096; i ++) {
        b.on(4095 - i);
        ASSERT(b[4095 - i]);
        ASSERT_EQUAL(*b.smallest(), 4095 - i);
        ASSERT_EQUAL(*b.largest(), 4095);
    }
    for (u32 i = 0; i < 4096; i ++)
        ASSERT(b[i]);
    u32 item = *b.begin();
    ASSERT_EQUAL(item, 0);

    bool first = true;
    for (u32 i : b) {
        if (!first)
            ASSERT_EQUAL(i, item + 1);
        first = false;
        item = i;
    }
}

TEST(biasedset_remove_all_from_the_left) {
    biasedset<128> b;
    for (u32 i = 0; i < 4096; i ++)
        b.on(i);
    for (i32 i = 0; i < 4096; i ++) {
        ASSERT(b[i]);
        ASSERT_EQUAL(*b.smallest(), i);
        b.off(i);
        ASSERT(!b[i]);
        if (i < 4095)
            ASSERT_EQUAL(*b.smallest(), i + 1);
    }
    ASSERT(b.begin() == b.end());
    ASSERT(!b.smallest());
    ASSERT(!b.largest());
}

TEST(biasedset_remove_all_from_the_right) {
    biasedset<128> b;
    for (u32 i = 0; i < 4096; i ++)
        b.on(i);
    for (i32 i = 4095; i >= 0; i --) {
        ASSERT(b[i]);
        ASSERT_EQUAL(*b.largest(), i);
        b.off(i);
        ASSERT(!b[i]);
        if (i > 0)
            ASSERT_EQUAL(*b.largest(), i - 1);
    }
    ASSERT(b.begin() == b.end());
    ASSERT(!b.smallest());
    ASSERT(!b.largest());
}

bool isprime(u32 n) {
    for (u32 i = 2; i < n / 64; i ++) if (n % i == 0)
        return false;
    return true;
}

TEST(biasedset_sieve) {
    biasedset<128> b;
    for (u32 i = 0; i < 16384; i ++)
        b.on(i);
    u32 primes[31] = {
        2, 3, 5, 7, 11, 13, 17, 19,
        23, 29, 31, 37, 41, 43, 47, 53,
        59, 61, 67, 71, 73, 79, 83, 89,
        97, 101, 103, 107, 109, 113, 127
    };
    for (i32 i = 0; i < 31; i ++) {
        u32 prime = primes[i];
        for (u32 j : b) if (j % prime == 0)
            b.off(j);
    }
    for (u32 i : b) ASSERT(isprime(i));
}