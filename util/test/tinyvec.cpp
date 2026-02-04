#include "util/test/harness.h"
#include "util/vec.h"

TEST(tinyvec_grow_shrink) {
    tinyvec<u8> bytes;
    for (u32 i = 0; i < 7; i ++)
        bytes.push(i);
    ASSERT(bytes.begin() == bytes.fixed);
    for (u32 i = 0; i < 7; i ++)
        ASSERT_EQUAL(i, bytes[i]);

    bytes.push(7);
    ASSERT(bytes.begin() != bytes.fixed);
    for (u32 i = 0; i < 8; i ++)
        ASSERT_EQUAL(i, bytes[i]);

    bytes.pop();
    ASSERT(bytes.begin() == bytes.fixed);
    for (u32 i = 0; i < 7; i ++)
        ASSERT_EQUAL(i, bytes[i]);
}

TEST(tinyvec_iterate) {
    tinyvec<u8> a;
    tinyvec<u16> b;
    tinyvec<u32> c;
    tinyvec<u64> d;
    for (u32 i = 0; i < 100; i ++) {
        a.push(i);
        b.push(i);
        c.push(i);
        d.push(i);
        for (u32 j = 0; j < i; j ++) {
            ASSERT_EQUAL(a[j], j);
            ASSERT_EQUAL(b[j], j);
            ASSERT_EQUAL(c[j], j);
            ASSERT_EQUAL(d[j], j);
        }
    }
}

TEST(tinyvec_remove_if) {
    tinyvec<u8> a;
    tinyvec<u16> b;
    tinyvec<u32> c;
    tinyvec<u64> d;
    for (u32 i = 0; i < 100; i ++) {
        a.push(i);
        b.push(i);
        c.push(i);
        d.push(i);
    }
    a.removeIf([&](u8 u) -> bool { return u % 2 == 0; });
    b.removeIf([&](u16 u) -> bool { return u % 3 == 0; });
    c.removeIf([&](u32 u) -> bool { return u % 5 == 0; });
    d.removeIf([&](u64 u) -> bool { return u % 7 == 0; });

    for (u8 u : a) ASSERT(u % 2 != 0);
    for (u16 u : b) ASSERT(u % 3 != 0);
    for (u32 u : c) ASSERT(u % 5 != 0);
    for (u64 u : d) ASSERT(u % 7 != 0);
}

struct TwoBytes {
    u8 first, second;
};

TEST(tinyvec_non_integer_element) {
    tinyvec<TwoBytes> v;
    for (u32 i = 0; i < 100; i ++)
        v.push({ u8(i), u8(i + 1) });
    for (auto [i, p] : enumerate(v)) {
        ASSERT(i == p.first);
        ASSERT(i + 1 == p.second);
    }
    while (v.size()) {
        auto p = v.pop();
        ASSERT(p.first == v.size());
        ASSERT(p.second == v.size() + 1);
    }
}