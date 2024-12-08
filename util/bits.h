#ifndef UTILITY_BITS_H
#define UTILITY_BITS_H

#include "rt/def.h"
#include "util/malloc.h"

template<u32 Bits>
struct bitset {
    static constexpr u32 N = (Bits + 63) / 64;
    u64 n;
    u64* bits;
    u64 fixed[N];

    inline bitset(u64 n_in = N * 64): n((n_in + 63) / 64 * 64), bits(n >> 6 > N ? new u64[n >> 6] : fixed) {
        memory::fill(bits, 0, sizeof(u64) * (n >> 6));
    }

    inline ~bitset() {
        if (bits != fixed) delete[] bits;
    }

    inline bitset(const bitset& other): n(other.n), bits(n >> 6 > N ? new u64[n >> 6] : fixed) {
        memory::copy(bits, other.bits, n >> 3);
    }

    inline bitset& operator=(const bitset& other) {
        if (this == &other) return *this;
        if (bits != fixed) delete[] bits;
        n = other.n;
        bits = n >> 6 > N ? new u64[n >> 6] : fixed;
        memory::copy(bits, other.bits, n >> 3);
        return *this;
    }

    struct accessor {
        bitset& b;
        u64 i;
        inline accessor(bitset& b_in, u64 i_in): b(b_in), i(i_in) {}

        inline accessor& operator=(bool b) {
            this->b.set(i, b);
            return *this;
        }

        inline operator bool() const {
            return ((const bitset&)b)[i];
        }
    };

    inline bool operator[](u64 i) const {
        if (i >= n) return false;
        return bits[i >> 6] & u64(1) << (i & 63);
    }

    inline accessor operator[](u64 i) {
        growto(i + 1);
        return { *this, i };
    }

    inline void fillto(u64 i) {
        fill();
        if (i <= n) return;
        u64 new_n = (i + 63) / 64 * 64;
        if (new_n >> 6 > n >> 6 && new_n >> 6 > N) {
            u64* new_bits = new u64[new_n >> 6];
            memory::fill(new_bits, 0xffu, new_n >> 3);
            memory::copy(new_bits, bits, n >> 3);
            if (bits != fixed) delete[] bits;
            bits = new_bits;
        }
        n = new_n;
    }

    inline void growto(u64 i) {
        if (i <= n) return;
        u64 new_n = (i + 63) / 64 * 64;
        if (new_n >> 6 > n >> 6 && new_n >> 6 > N) {
            u64* new_bits = new u64[new_n >> 6];
            memory::fill(new_bits, 0, new_n >> 3);
            memory::copy(new_bits, bits, n >> 3);
            if (bits != fixed) delete[] bits;
            bits = new_bits;
        }
        n = new_n;
    }

    inline void set(u64 i, bool b) {
        growto(i + 1);
        u64 mask = b << (i & 63);
        bits[i >> 6] &= ~mask;
        bits[i >> 6] |= mask;
    }

    inline bool add(u64 i) {
        growto(i + 1);
        bool contains = operator[](i);
        on(i);
        return !contains;
    }

    inline void on(u64 i) {
        growto(i + 1);
        u64 mask = u64(1) << (i & 63);
        bits[i >> 6] |= mask;
    }

    inline void off(u64 i) {
        growto(i + 1);
        u64 mask = u64(1) << (i & 63);
        bits[i >> 6] &= ~mask;
    }

    inline u64 count() const {
        u64 sum = 0;
        for (u64 i = 0; i < n >> 6; i ++)
            sum += popcount64(bits[i]);
        return sum;
    }

    inline void clear() {
        memory::fill(bits, 0, n >> 3);
    }

    inline void fill() {
        memory::fill(bits, 0xffu, n >> 3);
    }

    inline operator bool() const {
        for (u64 i = 0; i < n >> 6; i ++)
            if (bits[i]) return true;
        return false;
    }

    inline bool intersect(const bitset& other) {
        growto(other.n);
        bool changed = false;
        for (u64 i = 0; i < other.n >> 6; i ++) {
            if (other.bits[i] != bits[i]) { // If we are adding new bits.
                changed = true;
                bits[i] &= other.bits[i];
            }
        }
        return changed;
    }

    inline bool containsAll(const bitset& other) {
        growto(other.n);
        for (u64 i = 0; i < other.n >> 6; i ++) {
            if ((other.bits[i] | bits[i]) != bits[i])
                return false;
        }
        return true;
    }

    inline bool addAll(const bitset& other) {
        growto(other.n);
        bool changed = false;
        for (u64 i = 0; i < other.n >> 6; i ++) {
            if (other.bits[i] & ~bits[i]) { // If we are adding new bits.
                changed = true;
                bits[i] |= other.bits[i];
            }
        }
        return changed;
    }

    inline bitset& operator|=(const bitset& other) {
        growto(other.n);
        for (u64 i = 0; i < n >> 6; i ++) {
            bits[i] |= other.bits[i];
        }
        return *this;
    }

    inline bitset& operator&=(const bitset& other) {
        growto(other.n);
        for (u64 i = 0; i < n >> 6; i ++) {
            bits[i] &= other.bits[i];
        }
        return *this;
    }

    inline bitset& operator^=(const bitset& other) {
        growto(other.n);
        for (u64 i = 0; i < n >> 6; i ++) {
            bits[i] ^= other.bits[i];
        }
        return *this;
    }

    inline void removeAll(const bitset& other) {
        for (u64 i = 0; i < min(n, other.n) >> 6; i ++) {
            bits[i] &= ~other.bits[i];
        }
    }

    struct iterator {
        u64* blocks;
        u64* end;
        u32 i;
        u8 offset;

        inline u64 operator*() const {
            return i * 64 + offset;
        }

        inline bool operator==(const iterator& other) const {
            return blocks + i == other.blocks + other.i && offset == other.offset;
        }

        inline bool operator!=(const iterator& other) const {
            return blocks + i != other.blocks + other.i || offset != other.offset;
        }

        inline iterator& operator++() {
            while (blocks + i != end) {
                u64 mask = 0xffffffffffffffffull >> (63 - offset);
                if (offset == 65)
                    mask = 0;
                u64 word = blocks[i] & ~mask;
                if (!word) {
                    offset = 0;
                    i ++;
                    if ((blocks + i) == end || blocks[i] & 1)
                        return *this;
                } else {
                    offset = ctz64(word);
                    break;
                }
            }
            return *this;
        }

        inline iterator operator++(int) {
            iterator copy(*this);
            operator++();
            return copy;
        }
    };

    inline iterator begin() const {
        u64* word = bits;
        while (word < bits + n / 64 && !*word) word ++;
        return iterator{
            bits,
            bits + n / 64,
            u32(word - bits),
            word == bits + n / 64 ? (u8)0 : (u8)ctz64(*word)
        };
    }

    inline iterator end() const {
        return iterator{
            bits,
            bits + n / 64,
            (u32)n / 64,
            (u8)0
        };
    }
};

#endif