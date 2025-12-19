#ifndef UTILITY_BLOOM_H
#define UTILITY_BLOOM_H

#include "rt/def.h"

template<typename T, u64 K, u64 N = 256>
struct bloom {
    u64 bits[N / 64];

    bloom() {
        memory::fill(bits, 0, sizeof(bits));
    }

    inline bloom& operator|=(const bloom& other) {
        for (u32 i = 0; i < N / 64; i ++)
            bits[i] |= other.bits[i];
        return *this;
    }

    u64 hash1(u64 word) {
        return ((word >> 19) + 14208304000226097331ull) * 16072118377525667491ull * (word << 23) + 9981740664797991917ull;
    }

    u64 hash2(u64 word) {
        return ((word >> 37) + 17911635668274463517ull) * 13035240630932546849ull * (word << 7) + 14860081850123516429ull;
    }

    u64 hash3(u64 word) {
        return word ^ 12047805040268363351ull;
    }

    bool mayContain(T t) {
        if constexpr (K >= 1)
            if (test(hash1(t)))
                return true;
        if constexpr (K >= 2)
            if (test(hash2(t)))
                return true;
        if constexpr (K >= 3)
            if (test(hash3(t)))
                return true;
        return false;
    }

    void add(T t) {
        if constexpr (K >= 1)
            set(hash1(t));
        if constexpr (K >= 2)
            set(hash2(t));
        if constexpr (K >= 3)
            set(hash3(t));
    }

    bool test(u64 i) {
        i %= N;
        return bits[i / 64ull] & 1ull << (i % 64ull);
    }

    void set(u64 i) {
        i %= N;
        bits[i / 64ull] |= 1ull << (i % 64ull);
    }
};

#endif