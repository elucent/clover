#ifndef UTILITY_BITS_H
#define UTILITY_BITS_H

#include "rt/def.h"
#include "util/malloc.h"
#include "util/maybe.h"

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

template<u64 N>
struct biasedset {
    u64* bits;
    u32 min, maxp1; // We store the max plus one so that it and min form an exclusive range.
    u64 capacity;

    static_assert(N % 64 == 0);

    constexpr static u32 Words = N / 64;
    u64 fixed[Words];

    inline biasedset():
        bits(fixed), min(0), maxp1(0), capacity(N) {
        for (u32 i = 0; i < Words; i ++)
            fixed[i] = 0;
    }

    inline ~biasedset() {
        if (bits != fixed)
            delete[] bits;
    }

    inline biasedset(const biasedset& other): bits(fixed), min(other.min), maxp1(other.maxp1), capacity(other.capacity) {
        if (capacity > N)
            bits = new u64[capacity / 64];
        memory::copy(bits, other.bits, capacity / 8);
    }

    inline biasedset(biasedset&& other): bits(other.bits), min(other.min), maxp1(other.maxp1), capacity(other.capacity) {
        if (bits == other.fixed) {
            bits = fixed;
            memory::copy(bits, other.bits, capacity / 8);
        } else {
            other.bits = other.fixed, other.min = other.maxp1 = 0, other.capacity = N;
            for (u32 i = 0; i < Words; i ++)
                other.fixed[i] = 0;
        }
    }

    inline biasedset& operator=(const biasedset& other) {
        if (this != &other) {
            if (bits != fixed)
                delete[] bits;
            min = other.min;
            maxp1 = other.maxp1;
            capacity = other.capacity;
            bits = capacity <= N ? fixed : new u64[capacity / 64];
            memory::copy(bits, other.bits, capacity / 8);
        }
        return *this;
    }

    inline biasedset& operator=(biasedset&& other) {
        if (this != &other) {
            if (bits != fixed)
                delete[] bits;
            min = other.min;
            maxp1 = other.maxp1;
            capacity = other.capacity;
            bits = other.bits;
            if (bits == other.fixed) {
                bits = fixed;
                memory::copy(bits, other.bits, capacity / 8);
            } else {
                other.bits = other.fixed, other.min = other.maxp1 = 0, other.capacity = N;
                for (u32 i = 0; i < Words; i ++)
                    other.fixed[i] = 0;
            }
            memory::copy(bits, other.bits, capacity / 8);
        }
        return *this;
    }

    inline NOINLINE void growto(u32 i) {
        u32 newcapacity = capacity;
        while (newcapacity <= i)
            newcapacity *= 2;
        assert(newcapacity > N); // Otherwise, why did we grow?

        auto old = bits;
        bits = new u64[newcapacity / 64];
        memory::copy(bits, old, capacity / 8);
        memory::fill(bits + capacity / 64, 0, (newcapacity - capacity) / 8);
        capacity = newcapacity;
        if (old != fixed)
            delete[] old;

        // Note that min doesn't change.
    }

    inline NOINLINE void lshift(u32 i) {
        // Expands the set by moving the minimum down and shifting existing
        // elements up. Note that this doesn't actually change min - the caller
        // is responsible for that. This function also assumes our capacity has
        // already been increased to fit the shifted elements.

        u32 wordDiff = i / 64;
        u32 bitDiff = i % 64;
        // First shift by words, then by sub-word amount.
        // TODO: these can probably be fused...idk if they should be.
        u32 size = (maxp1 - min + 63) / 64;
        assert((size + wordDiff) * 64 <= capacity);
        memory::move(bits + wordDiff, bits, size * 8);

        if (bitDiff) {
            u64 overflow = 0;
            for (u32 i = wordDiff; i < size + wordDiff; i ++) {
                u64 newOverflow = bits[i] >> (64 - bitDiff);
                bits[i] <<= bitDiff;
                bits[i] |= overflow;
                overflow = newOverflow;
            }
        }
    }

    inline NOINLINE void rshift(u32 i) {
        // Shrinks the set by moving the minimum up by moving existing elements
        // down. Note that this doesn't actually change min - the caller is
        // responsible for that.

        u32 wordDiff = i / 64;
        u32 bitDiff = i % 64;

        u32 size = (maxp1 - min + 63) / 64;
        assert(size * 64 <= capacity);
        assert(size >= wordDiff);
        memory::move(bits, bits + wordDiff, size * 8 - wordDiff * 8);

        if (bitDiff) {
            u64 underflow = 0;
            for (i32 i = i32(size - wordDiff) - 1; i >= 0; i --) {
                u64 newUnderflow = bits[i] << (64 - bitDiff);
                bits[i] >>= bitDiff;
                bits[i] |= underflow;
                underflow = newUnderflow;
            }
        }
    }

    inline void on(u32 i) {
        if (i < min || min == maxp1) {
            if (min != maxp1) {
                u32 diff = min - i;
                if (maxp1 + diff > capacity)
                    growto(maxp1 + diff);
                lshift(diff);
                maxp1 += diff;
            } else
                maxp1 = i + 1;
            min = i,
            bits[0] |= 1;
            return;
        }
        u32 adj = i - min;
        if (adj >= capacity)
            growto(adj);
        bits[adj / 64] |= 1ull << adj % 64;
        if (i >= maxp1)
            maxp1 = i + 1;
    }

    inline biasedset& operator|=(const biasedset& other) {
        if (other.min == other.maxp1)
            return *this;
        on(other.min);
        if (other.maxp1 > capacity)
            growto(other.maxp1);
        u64 shift = other.min - min;
        if (!shift) for (u64 i = 0; i < other.capacity / 64; i ++)
            bits[i] |= other.bits[i];
        else {
            u64 wordShift = shift / 64;
            shift = shift % 64;
            for (u64 i = 0; i < other.capacity / 64; i ++) {
                if (i > 0)
                    bits[i + wordShift] |= other.bits[i - 1] >> shift;
                bits[i + wordShift] |= other.bits[i] << shift;
            }
        }
        return *this;
    }

    inline maybe<u32> smallest() const {
        for (u32 i = 0; i < capacity / 64; i ++) {
            if (bits[i])
                return some<u32>(min + i * 64 + ctz64(bits[i]));
        }
        return none<u32>();
    }

    inline maybe<u32> largest() const {
        for (i32 i = i32(maxp1 - min + 63) / 64 - 1; i >= 0; i --) {
            if (bits[i])
                return some<u32>(min + i * 64 + 63 - clz64(bits[i]));
        }
        return none<u32>();
    }

    inline void off(u32 i) {
        if (i - min >= maxp1)
            return; // Underflow-based bounds check.
        u32 adj = i - min;
        bits[adj / 64] &= ~(1ull << adj % 64);
        if (i + 1 == maxp1) {
            auto newmax = largest();
            if (!newmax) {
                min = maxp1 = 0;
                return;
            }
            maxp1 = *newmax + 1;
        }
        if (i == min) {
            auto newmin = smallest();
            if (!newmin) {
                min = maxp1 = 0;
                return;
            }
            rshift(*newmin - min);
            min = *newmin;
        }
    }

    inline bool empty() const {
        return min == maxp1;
    }

    inline bool operator[](u32 i) const {
        if (i - min >= maxp1)
            return false;
        i -= min;
        return bits[i / 64] & (1ull << i % 64);
    }

    struct iterator {
        u64* bits;
        u32 idx, min, maxp1, numWords;

        inline bool operator==(const iterator& other) const {
            return bits == other.bits && idx == other.idx && min == other.min;
        }

        inline bool operator!=(const iterator& other) const {
            return bits != other.bits || idx != other.idx || min != other.min;
        }

        inline u32 operator*() const {
            return min + idx;
        }

        inline iterator& operator++() {
            u64 wordidx = idx / 64;
            u64 word = bits[wordidx];
            u64 mask = 0xffffffffffffffffull;
            u64 masked = word & ~(mask >> (63 - idx) % 64);
            if (masked) {
                idx += ctz64(masked) - (idx % 64);
                return *this;
            }
            wordidx ++;
            while (wordidx < numWords) {
                if (bits[wordidx]) {
                    idx = wordidx * 64 + ctz64(bits[wordidx]);
                    return *this;
                }
                wordidx ++;
            }
            idx = maxp1 - min;
            return *this;
        }
    };

    iterator begin() const {
        return { bits, 0, min, maxp1, u32(capacity / 64) };
    }

    iterator end() const {
        return { bits, maxp1 - min, min, maxp1, u32(capacity / 64) };
    }

    inline void clear() {
        memory::fill(bits, 0, capacity / 8);
        min = maxp1 = 0;
    }
};

#endif