#ifndef UTILITY_BLOOM_H
#define UTILITY_BLOOM_H

#include "rt/def.h"

template<typename T, u64 N = 256>
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
        return word ^ 12047805040268363351ull;
    }

    u64 hash2(u64 word) {
        return ((word >> 19) + 14208304000226097331ull) * 16072118377525667491ull * (word << 23) + 9981740664797991917ull;
    }

    u64 hash3(u64 word) {
        return ((word >> 37) + 17911635668274463517ull) * 13035240630932546849ull * (word << 7) + 14860081850123516429ull;
    }

    bool mayContain(T t) {
        return test(hash1(t)) && test(hash2(t)) && test(hash3(t));
    }
    
    void add(T t) {
        set(hash1(t));
        set(hash2(t));
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