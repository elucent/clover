#ifndef UTILITY_HASH_H
#define UTILITY_HASH_H

#include "rt/def.h"
#include "util/malloc.h"

#define COLLECT_HASH_INFO 0

#if COLLECT_HASH_INFO
    extern iptr lookups, scans;
#endif

// MurmurHash, 64-bit version, unaligned by Austin Appleby (https://sites.google.com/site/murmurhash/)
// The source has been slightly modified, using basil typedefs, and a fixed seed (a 64-bit prime).

inline u64 raw_hash(const void* input, u64 size){
	const u64 m = 0xc6a4a7935bd1e995;
	const u32 r = 47;

	u64 h = 7576351903513440497ul ^ (size * m);

	const u64* data = (const u64*)input;
	const u64* end = data + (size / 8);

	while(data != end) {
		u64 k = load<u64>(data ++);
		k *= m;
		k ^= k >> r;
		k *= m;
		h ^= k;
		h *= m;
	}

	const u8* data2 = (const u8*)data;
	switch(size & 7) {
	case 7: h ^= u64(data2[6]) << 48;
	case 6: h ^= u64(data2[5]) << 40;
	case 5: h ^= u64(data2[4]) << 32;
	case 4: h ^= u64(data2[3]) << 24;
	case 3: h ^= u64(data2[2]) << 16;
	case 2: h ^= u64(data2[1]) << 8;
	case 1: h ^= u64(data2[0]);
	        h *= m;
	};

	h ^= h >> r;
	h *= m;
	h ^= h >> r;

	return h;
}

inline u64 intHash(u64 x) {
    x = ((x >> 16) ^ x) * 0x45d9f3bull;
    x = ((x >> 16) ^ x) * 0x45d9f3bull;
    x = (x >> 16) ^ x;
    return x;
}

inline u64 hash(u32 i) {
    return intHash(i);
}

inline u64 hash(u64 i) {
    return intHash(i);
}

inline u64 hash(i32 i) {
    return intHash(i);
}

inline u64 hash(i64 i) {
    return intHash(i);
}

inline u64 hash(const i8* s) {
    u32 size = 0;
    const i8* sptr = s;
    while (*sptr) ++ sptr, ++ size;
    return raw_hash(s, size);
}

inline u64 hash(const_slice<i8> s) {
    return raw_hash(s.data(), s.size());
}

template<typename K, typename V>
inline u64 hash(const entry<K, V>& e) {
    return hash(e.key);
}

template<typename A, typename B>
inline u64 hash(const pair<A, B>& e) {
    return mixHash(hash(e.first), hash(e.second));
}

inline u64 mixHash(u64 a, u64 b) {
    return a ^ b + 0x517cc1b727220a95 + (a << 6) + (a >> 2);
}

// Open-addressed general-purpose hash table based on robin-hood hashing, with a
// fixed stack-allocated buffer of N elements.
template<typename T, i32 N = 8>
class set {
    enum bucket_status : u8 {
        FILLED, EMPTY, GHOST
    };

    constexpr static u32 SIZE_MUL = sizeof(bucket_status) + sizeof(u8) + sizeof(T);
    constexpr static u32 LinearSearchSpace = N * SIZE_MUL;
    constexpr static bool ShouldLinearSearch = LinearSearchSpace <= 256 && LinearSearchSpace / sizeof(T) <= 16;
    constexpr static u32 LinearSearchLimit = LinearSearchSpace / sizeof(T);

    struct memory {
        u8* bytes;
        u64 capacity;
        // status is at offset 0
        // hashbytes start at offset N
        // data starts at offset N * 2

        memory(u8* bytes_in, u64 capacity_in): bytes(bytes_in), capacity(capacity_in) {
            ::memory::fill(bytes, EMPTY, capacity * 2);
        }

        inline void free() {
            ::free(bytes);
        }

        inline bucket_status& status(u64 i) {
            return *(bucket_status*)(bytes + i * 2);
        }

        inline const bucket_status& status(u64 i) const {
            return *(const bucket_status*)(bytes + i * 2);
        }

        inline u8& hashbyte(u64 i) {
            return *(u8*)(bytes + i * 2 + 1);
        }

        inline const u8& hashbyte(u64 i) const {
            return *(const u8*)(bytes + i * 2 + 1);
        }

        inline T* linearValues() {
            assert(isLinearSearched());
            return (T*)bytes;
        }

        inline const T* linearValues() const {
            assert(isLinearSearched());
            return (const T*)bytes;
        }

        inline T* values() {
            assert(!isLinearSearched());
            return (T*)(bytes + capacity * 2);
        }

        inline const T* values() const {
            assert(!isLinearSearched());
            return (const T*)(bytes + capacity * 2);
        }

        void fill(u32 i, const T& t, u64 h) {
            if (status(i) == FILLED) values()[i] = t;
            else status(i) = FILLED, new(values() + i) T(t);
            hashbyte(i) = u8(h >> 24);
        }

        void evict(u32 i) {
            status(i) = GHOST;
            values()[i].~T();
        }

        void clear() {
            if (isLinearSearched()) {
                for (u32 i = 0; i < capacity; i ++)
                    linearValues()[i].~T();
                return;
            }
            for (u32 i = 0; i < capacity; i ++) if (status(i) == FILLED) evict(i);
            ::memory::fill(bytes, EMPTY, capacity * 2);
        }

        inline bool isLinearSearched() const {
            return ShouldLinearSearch && capacity <= LinearSearchLimit;
        }
    };
private:
    memory mem;
    u64 nelts;
    u8 fixed[N * SIZE_MUL];

    inline void init(u32 size) {
        nelts = 0, mem.capacity = size;
        if (ShouldLinearSearch && mem.capacity <= LinearSearchLimit)
            mem.bytes = fixed;
        else {
            mem.bytes = mem.capacity <= N ? fixed : new u8[mem.capacity * SIZE_MUL];
            ::memory::fill(mem.bytes, EMPTY, mem.capacity * 2);
        }
    }

    template<typename U>
    inline void swap(U& a, U& b) {
        U t = a;
        a = b;
        b = t;
    }

    inline void grow() {
        memory old = mem;

        if (old.isLinearSearched()) {
            u64 oldSize = LinearSearchLimit;
            u64 newSize = oldSize * 2;
            newSize = 1u << (64 - clz64(newSize)); // Round up to nearest power of two since we don't know that the linear search limit is one.
            init(newSize);
            for (u32 i = 0; i < old.capacity; ++ i)
                insert(old.linearValues()[i]);
            return;
        } else {
            init(old.capacity * 2);
            for (u32 i = 0; i < old.capacity; ++ i) {
                if (old.status(i) == FILLED) insert(old.values()[i]);
            }
            if (old.capacity > N) old.clear(), free(old.bytes);
        }
    }

public:
    set(): mem(fixed, ShouldLinearSearch ? LinearSearchLimit : N), nelts(0) {}

    ~set() {
        mem.clear();
        if (mem.bytes != fixed) free(mem.bytes);
    }

    set(const set& other): mem(other.mem.bytes == other.fixed ? fixed : (u8*)malloc(other.mem.capacity * SIZE_MUL), other.mem.capacity), nelts(other.nelts) {
        if (mem.isLinearSearched()) {
            for (u32 i = 0; i < nelts; i ++)
                new(mem.linearValues() + i) T(other.mem.linearValues()[i]);
        } else {
            ::memory::copy(mem.bytes, other.mem.bytes, mem.capacity * SIZE_MUL);
            for (u32 i = 0; i < mem.capacity; i ++) if (mem.status(i) == FILLED)
                new(mem.values() + i) T(other.mem.values()[i]);
        }
    }

    set& operator=(const set& other) {
        if (this != &other) {
            mem.clear();
            if (mem.bytes != fixed) free(mem.bytes);
            mem.capacity = other.mem.capacity;
            mem.bytes = other.mem.bytes == other.fixed ? fixed : (u8*)malloc(other.mem.capacity * SIZE_MUL);
            nelts = other.nelts;
            if (mem.isLinearSearched()) {
                for (u32 i = 0; i < nelts; i ++)
                    new(mem.linearValues() + i) T(other.mem.linearValues()[i]);
            } else {
                ::memory::copy(mem.bytes, other.mem.bytes, mem.capacity * SIZE_MUL);
                for (u32 i = 0; i < mem.capacity; i ++) if (mem.status(i) == FILLED)
                    new(mem.values() + i) T(other.mem.values()[i]);
            }
        }
        return *this;
    }

    class const_iterator {
        const memory* m;
        u32 ptr = 0, end = 0;
        friend class set;
    public:
        const_iterator(const memory* m_in, u32 ptr_in, u32 end_in):
            m(m_in), ptr(ptr_in), end(end_in) {
            //
        }

        const T& operator*() const {
            return (m->isLinearSearched() ? m->linearValues() : m->values())[ptr];
        }

        const T* operator->() const {
            return (m->isLinearSearched() ? m->linearValues() : m->values()) + ptr;
        }

        const_iterator& operator++() {
            if (ptr != end) ++ ptr;
            if (!m->isLinearSearched())
                while (ptr != end && m->status(ptr)) ++ ptr;
            return *this;
        }

        const_iterator operator++(int) {
            iterator it = *this;
            operator++();
            return it;
        }

        bool operator==(const const_iterator& other) const {
            return ptr == other.ptr;
        }

        bool operator!=(const const_iterator& other) const {
            return ptr != other.ptr;
        }
    };

    class iterator {
        memory* m;
        u32 ptr = 0, end = 0;
        friend class set;
    public:
        iterator(memory* m_in, u32 ptr_in, u32 end_in):
            m(m_in), ptr(ptr_in), end(end_in) {
            //
        }

        T& operator*() {
            return (m->isLinearSearched() ? m->linearValues() : m->values())[ptr];
        }

        T* operator->() {
            return (m->isLinearSearched() ? m->linearValues() : m->values()) + ptr;
        }

        iterator& operator++() {
            if (ptr != end) ++ ptr;
            if (!m->isLinearSearched())
                while (ptr != end && m->status(ptr)) ++ ptr;
            return *this;
        }

        iterator operator++(int) {
            iterator it = *this;
            operator++();
            return it;
        }

        bool operator==(const iterator& other) const {
            return ptr == other.ptr;
        }

        bool operator!=(const iterator& other) const {
            return ptr != other.ptr;
        }

        operator const_iterator() const {
            return const_iterator(ptr, end);
        }
    };

    iterator begin() {
        if (mem.isLinearSearched())
            return iterator(&mem, 0, nelts);
        u32 start = 0, end = mem.capacity;
        while (start != end && mem.status(start) != FILLED) ++ start;
        return iterator(&mem, start, end);
    }

    const_iterator begin() const {
        if (mem.isLinearSearched())
            return const_iterator(&mem, 0, nelts);
        u32 start = 0, end = mem.capacity;
        while (start != end && mem.status(start) != FILLED) ++ start;
        return const_iterator(&mem, start, end);
    }

    inline iterator end() {
        if (mem.isLinearSearched())
            return iterator(&mem, nelts, nelts);
        return iterator(&mem, mem.capacity, mem.capacity);
    }

    inline const_iterator end() const {
        if (mem.isLinearSearched())
            return const_iterator(&mem, nelts, nelts);
        return const_iterator(&mem, mem.capacity, mem.capacity);
    }

    void clear() {
        mem.clear();
        nelts = 0;
    }

    NOINLINE void insert(const T& t, u64 h) {
        if (mem.isLinearSearched()) {
            if (nelts >= mem.capacity)
                grow();
            else if (mem.isLinearSearched()) {
               new(mem.linearValues() + nelts ++) T(t);
               return;
            }
        }

        #if COLLECT_HASH_INFO
            lookups ++;
        #endif

        if (mem.capacity * 5 < (nelts + 1) * 10)
            grow();
        u64 dist = 0;
        u64 mask = mem.capacity - 1;
        u64 i = h & mask;
        T item = t;
        while (true) {
            #if COLLECT_HASH_INFO
                scans ++;
            #endif

            if (mem.status(i)) {
                mem.fill(i, item, h);
                ++ nelts;
                return;
            } else if (mem.hashbyte(i) == u8(h >> 24) && mem.values()[i] == item) {
                mem.fill(i, item, h); // potentially redundant assignment allows for overwriting existing values
                return;
            } else {
                u64 oh = hash(mem.values()[i]);
                u64 other_dist = (i - oh) & mask;
                if (other_dist < dist) {
                    swap(item, mem.values()[i]);
                    mem.hashbyte(i) = u8(h >> 24);
                    h = oh;
                    dist = other_dist;
                }
                i = (i + 1) & mask;
                ++ dist;
            }
        }
    }

    void insert(const T& t) {
        insert(t, hash(t));
    }

    NOINLINE void erase(const T& t) {
        if (mem.isLinearSearched()) {
            for (u32 i = 0; i < nelts; i ++) {
                if (mem.linearValues()[i] == t) {
                    swap(mem.linearValues()[i], mem.linearValues()[nelts - 1]);
                    mem.linearValues()[nelts - 1].~T();
                    nelts --;
                    break;
                }
            }
            return;
        }

        #if COLLECT_HASH_INFO
            lookups ++;
        #endif

        u64 h = hash(t);
        u64 mask = mem.capacity - 1;
        u64 i = h & mask;
        while (true) {
            #if COLLECT_HASH_INFO
                scans ++;
            #endif

            if (mem.status(i) == EMPTY) return;
            if (mem.status(i) == FILLED && mem.hashbyte(i) == u8(h >> 24) && mem.values()[i] == t) {
                mem.evict(i);
                -- nelts;
                return;
            }
            i = (i + 1) & mask;
        }
    }

    NOINLINE const_iterator find(const T& t) const {
        if (mem.isLinearSearched()) {
            for (u32 i = 0; i < nelts; i ++) {
                if (mem.linearValues()[i] == t)
                    return const_iterator(&mem, i, nelts);
            }
            return end();
        }

        #if COLLECT_HASH_INFO
            lookups ++;
        #endif

        u64 h = hash(t);
        u64 mask = mem.capacity - 1;
        u64 i = h & mask;
        while (true) {
            #if COLLECT_HASH_INFO
                scans ++;
            #endif

            switch (mem.status(i)) {
                case EMPTY:
                    return end();
                case GHOST: {
                    u64 dist = (i - h) & mask;
                    u64 oh = hash(mem.values()[i]);
                    u64 other_dist = i - oh & mask;
                    if (other_dist < dist) return end();
                    break;
                }
                case FILLED:
                    if (mem.hashbyte(i) == u8(h >> 24) && t == mem.values()[i])
                        return const_iterator(&mem, i, mem.capacity);
                    break;
            }
            i = i + 1 & mask;
        }
    }

    NOINLINE iterator find(const T& t) {
        if (mem.isLinearSearched()) {
            for (u32 i = 0; i < nelts; i ++) {
                if (mem.linearValues()[i] == t)
                    return iterator(&mem, i, nelts);
            }
            return end();
        }

        #if COLLECT_HASH_INFO
            lookups ++;
        #endif

        u64 h = hash(t);
        u64 mask = mem.capacity - 1;
        u64 i = h & mask;
        while (true) {
            #if COLLECT_HASH_INFO
                scans ++;
            #endif

            switch (mem.status(i)) {
                case EMPTY:
                    return end();
                case GHOST: {
                    u64 dist = (i - h) & mask;
                    u64 oh = hash(mem.values()[i]);
                    u64 other_dist = i - oh & mask;
                    if (other_dist < dist) return end();
                    break;
                }
                case FILLED:
                    if (mem.hashbyte(i) == u8(h >> 24) && t == mem.values()[i])
                        return iterator(&mem, i, mem.capacity);
                    break;
            }
            i = i + 1 & mask;
        }
    }

    bool contains(const T& t) const {
        return find(t) != end();
    }

    u32 size() const {
        return nelts;
    }

    u32 capacity() const {
        return mem.capacity;
    }
};

// Key-value map backed by a hashset.
template<typename K, typename V, i32 N = 4>
class map : public set<entry<K, V>, N> {
    static V default_value;
    using entryset = set<entry<K, V>, N>;
public:
    void put(const K& key, const V& value) {
        entryset::insert({ key, value });
    }

    void erase(const K& key) {
        entryset::erase({ key, default_value });
    }

    V& operator[](const K& key) {
        entry<K, V> dummy = { key, default_value };
        auto it = entryset::find(dummy);
        if (it == entryset::end()) {
            entryset::insert(dummy);
        }
        return entryset::find(dummy)->value;
    }

    const V& operator[](const K& key) const {
        entry<K, V> dummy = { key, default_value };
        auto it = entryset::find(dummy);
        if (it == entryset::end()) {
            panic("Tried to access nonexistent map key!");
        }
        return set<entry<K, V>, N>::find(dummy)->value;
    }

    typename entryset::const_iterator find(const K& key) const {
        return entryset::find({ key, default_value });
    }

    typename entryset::iterator find(const K& key) {
        return entryset::find({ key, default_value });
    }

    bool contains(const K& key) const {
        return entryset::contains({ key, default_value });
    }
};

template<typename K, typename V, i32 N>
V map<K, V, N>::default_value;

#endif
