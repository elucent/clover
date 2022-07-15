#ifndef BASIL_LIB_BITS_H
#define BASIL_LIB_BITS_H

#include "core/def.h"
#include "lib/malloc.h"

template<typename Alloc = allocator>
struct bitset {
    u64 n;
    Alloc* alloc = Alloc::instance;
    u64* bits;
    
    bitset(u64 n): bits(new(*alloc) u64[(n + 63) >> 6]) {
        mset(bits, 0, sizeof(u64) * ((n + 63) >> 6));
    }

    ~bitset() {
        alloc->free(bits);
    }

    struct accessor {
        bitset& b;
        u64 i;
        accessor(bitset& b_in, u64 i_in): b(b_in), i(i_in) {}

        accessor& operator=(bool b) {
            this->b.set(i, b);
            return *this;
        }

        operator bool() const {
            return ((const bitset&)b)[i];
        }
    };

    bool operator[](u64 i) const {
        return bits[i >> 6] & 1ul << (i & 63);
    }

    accessor operator[](u64 i) {
        return { *this, i };
    }

    void set(u64 i, bool b) {
        u64 mask = b << (i & 63);
        bits[i >> 6] &= ~mask;
        bits[i >> 6] |= mask;
    }

    void on(u64 i) {
        u64 mask = 1ul << (i & 63);
        bits[i >> 6] &= ~mask;
        bits[i >> 6] |= mask;
    }

    void off(u64 i) {
        u64 mask = 0ul << (i & 63);
        bits[i >> 6] &= ~mask;
        bits[i >> 6] |= mask;
    }

    void clear() {
        mset(bits, 0, sizeof(u64) * ((n + 63) >> 6));
    }

    bitset& operator|=(const bitset& other) {
        for (u64 i = 0; i < (n + 63) >> 6; i ++) {
            bits[i] |= other.bits[i];
        }
        return *this;
    }

    bitset& operator&=(const bitset& other) {
        for (u64 i = 0; i < (n + 63) >> 6; i ++) {
            bits[i] &= other.bits[i];
        }
        return *this;
    }

    bitset& operator^=(const bitset& other) {
        for (u64 i = 0; i < (n + 63) >> 6; i ++) {
            bits[i] ^= other.bits[i];
        }
        return *this;
    }
};

#endif