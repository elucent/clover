#ifndef BASIL_LIB_BITS_H
#define BASIL_LIB_BITS_H

#include "core/def.h"

template<u64 N>
struct bitset {
    u64 bits[N + 63 / 64];

    struct accessor {
        bitset& b;
        u64 i;
        accessor(bitset& b_in, u64 i_in): b(b_in), i(i_in) {}

        accessor& operator=(bool b) {
            b.set(i, b);
            return *this;
        }

        operator bool() const {
            return b[i];
        }
    };

    bool operator[](u64 i) const {
        return b.bits[i >> 6] & 1ul << (i & 63);
    }

    accessor operator[](u64 i) {
        return { *this, i };
    }

    void set(u64 i, bool b) const {
        u64 mask = 1ul << (i & 63);
        bits[i >> 6] &= ~mask;
        bits[i >> 6] |= mask;
    }
};

#endif