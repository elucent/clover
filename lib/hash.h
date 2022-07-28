#ifndef BASIL_LIB_HASH_H
#define BASIL_LIB_HASH_H

#include "core/def.h"
#include "lib/malloc.h"

extern iptr lookups, scans;

// MurmurHash, 64-bit version, unaligned by Austin Appleby (https://sites.google.com/site/murmurhash/)
// The source has been slightly modified, using basil typedefs, and a fixed seed (a 64-bit prime).

inline u64 raw_hash(const void* input, u64 size){
	const u64 m = 0xc6a4a7935bd1e995;
	const u32 r = 47;

	u64 h = 7576351903513440497ul ^ (size * m);

	const u64* data = (const u64*)input;
	const u64* end = data + (size / 8);

	while(data != end) {
		u64 k = *data ++;
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

inline u64 hash(i32 i) { // Cheaty, potentially a little exploitable, but it seems to give good results in practice.
    return i;
}

inline u64 hash(const i8* s) {
    u32 size = 0;
    const i8* sptr = s;
    while (*sptr) ++ sptr, ++ size;
    return raw_hash(s, size);
}

inline u64 hash(const_slice<i8> s) {
    return raw_hash(s.ptr, s.n);
}

template<typename K, typename V>
struct entry {
    K key;
    V value;

    inline bool operator==(const entry& other) const {
        return key == other.key;
    }
};

template<typename K, typename V>
inline u64 hash(const entry<K, V>& e) {
    return hash(e.key);
}

// Open-addressed general-purpose hash table based on robin-hood hashing, with a 
// fixed stack-allocated buffer of N elements.
template<typename T, i32 N = 8, typename Alloc = allocator>
class set {
    enum bucket_status : u8 {
        FILLED, EMPTY, GHOST
    };

    static constexpr u32 SIZE_MUL = sizeof(bucket_status) + sizeof(u64) + sizeof(T); 

    struct memory {
        u8* bytes;
        u64 capacity;
        // status is at offset 0
        // hashes start at offset N
        // data starts at offset N * 9

        memory(u8* bytes_in, u64 capacity_in): bytes(bytes_in), capacity(capacity_in) {
            mset(bytes, EMPTY, capacity);
        }

        inline void free(Alloc* alloc) {
            alloc->free(bytes);
        }

        inline bucket_status* status() {
            return (bucket_status*)bytes;
        }

        inline const bucket_status* status() const {
            return (bucket_status*)bytes;
        }

        inline u64* hashes() {
            return (u64*)(bytes + capacity);
        }

        inline const u64* hashes() const {
            return (u64*)(bytes + capacity);
        }

        inline T* values() {
            return (T*)(bytes + capacity * 9);
        }

        inline const T* values() const {
            return (T*)(bytes + capacity * 9);
        }

        void fill(u32 i, const T& t, u64 h) {
            if (status()[i] == FILLED) values()[i] = t;
            else status()[i] = FILLED, new(values() + i) T(t);
            hashes()[i] = h;
        }

        void evict(u32 i) {
            status()[i] = GHOST;
            values()[i].~T();
        }

        void clear() {
            for (u32 i = 0; i < capacity; i ++) if (status()[i] == FILLED) evict(i);
            mset(status(), EMPTY, capacity);
        }
    };

    memory mem;
public:
    Alloc* alloc = Alloc::instance;
private:
    u64 nelts;
    u8 fixed[N * SIZE_MUL];

    inline void init(u32 size) {
        nelts = 0, mem.capacity = size;
        mem.bytes = mem.capacity <= N ? fixed : new(*alloc) u8[mem.capacity * SIZE_MUL];
        mset(mem.status(), EMPTY, mem.capacity);
    }

    template<typename U>
    inline void swap(U& a, U& b) {
        U t = a;
        a = b;
        b = t;
    }

    inline void grow() {
        memory old = mem;
        init(mem.capacity * 2);
        for (u32 i = 0; i < old.capacity; ++ i) {
            if (old.status()[i] == FILLED) insert(old.values()[i], old.hashes()[i]);
        }
        if (old.capacity > N) old.clear(), alloc->free(old.bytes);
    }

public:
    set(): mem(fixed, N), nelts(0) {}

    ~set() {
        mem.clear();
        if (mem.bytes != fixed) alloc->free(mem.bytes);
    }

    set(const set& other): mem(alloc->alloc(other.mem.capacity)), nelts(other.nelts) {
        mcpy(mem.bytes, other.mem.bytes, mem.capacity * 9);
        for (u32 i = 0; i < mem.capacity; i ++) if (mem.status()[i] == FILLED) 
            new(mem.values() + i) T(other.mem.values()[i]);
    }

    set& operator=(const set& other) {
        if (this != &other) {
            mem.clear();
            if (mem.bytes != fixed) alloc->free(mem.bytes);
            mem.bytes = alloc->alloc(other.mem.capacity);
            nelts = other.nelts;
            mcpy(mem.bytes, other.mem.bytes, mem.capacity * 9);
            for (u32 i = 0; i < mem.capacity; i ++) if (mem.status()[i] == FILLED) 
                new(mem.values() + i) T(other.mem.values()[i]);
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
            return m->values()[ptr];
        }

        const T* operator->() const {
            return m->values() + ptr;
        }

        const_iterator& operator++() {
            if (ptr != end) ++ ptr;
            while (ptr != end && m->status()[ptr]) ++ ptr;
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
            return m->values()[ptr];
        }

        T* operator->() {
            return m->values() + ptr;
        }

        iterator& operator++() {
            if (ptr != end) ++ ptr;
            while (ptr != end && m->status()[ptr]) ++ ptr;
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
        u32 start = 0, end = mem.capacity;
        while (start != end && mem.status()[start] != FILLED) ++ start;
        return iterator(&mem, start, end);
    }

    const_iterator begin() const {
        u32 start = 0, end = mem.capacity;
        while (start != end && mem.status()[start] != FILLED) ++ start;
        return const_iterator(&mem, start, end);
    }

    inline iterator end() {
        return iterator(&mem, mem.capacity, mem.capacity);
    }

    inline const_iterator end() const {
        return const_iterator(&mem, mem.capacity, mem.capacity);
    }

    void clear() {
        mem.clear();
        nelts = 0;
    }

    void insert(const T& t, u64 h) {
        if (mem.capacity * 8 < (nelts + 1) * 10) grow();
        u64 dist = 0;
        u64 mask = mem.capacity - 1;
        u64 i = h & mask;
        T item = t;
        while (true) {
            if (mem.status()[i]) {
                mem.fill(i, item, h);
                ++ nelts;
                return;
            }
            else if (mem.hashes()[i] == h && mem.values()[i] == item) {
                mem.fill(i, item, h); // potentially redundant assignment allows for overwriting existing values
                return;
            }
            else {
                u64 other_dist = i - mem.hashes()[i] & mask;
                if (other_dist < dist) {
                    swap(item, mem.values()[i]);
                    swap(h, mem.hashes()[i]);
                    dist = other_dist;
                }
                i = i + 1 & mask;
                ++ dist;
            }
        }
    }

    void insert(const T& t) {
        insert(t, hash(t));
    }

    void erase(const T& t) {
        u64 h = hash(t);
        u64 mask = mem.capacity - 1;
        u64 i = h & mask;
        while (true) {
            if (mem.status()[i] == EMPTY) return;
            if (mem.status()[i] == FILLED && mem.hashes()[i] == h && mem.values()[i] == t) {
                mem.evict(i);
                -- nelts;
                return;
            }
            i = i + 1 & mask;
        }
    }

    const_iterator find(const T& t) const {
        u64 h = hash(t);
        u64 mask = mem.capacity - 1;
        u64 i = h & mask;
        u64 dist = 0;
        while (true) {
            if (mem.status()[i] == EMPTY) return end();
            if (mem.status()[i] == GHOST) {
                u64 dist = (i - h) & mask;
                u64 oh = mem.hashes()[i];
                u64 other_dist = i - oh & mask;
                if (other_dist < dist) return end();
            }
            if (mem.status()[i] == FILLED && mem.hashes()[i] == h && t == mem.values()[i]) {
                return const_iterator(&mem, i, mem.capacity);
            }
            i = i + 1 & mask;
            dist ++;
        }
    }

    iterator find(const T& t) {
        u64 h = hash(t);
        u64 mask = mem.capacity - 1;
        u64 i = h & mask;
        u64 dist = 0;
        while (true) {
            if (mem.status()[i] == EMPTY) return end();
            if (mem.status()[i] == GHOST) {
                u64 dist = (i - h) & mask;
                u64 oh = mem.hashes()[i];
                u64 other_dist = i - oh & mask;
                if (other_dist < dist) return end();
            }
            if (mem.status()[i] == FILLED && mem.hashes()[i] == h && t == mem.values()[i]) {
                return iterator(&mem, i, mem.capacity);
            }
            i = i + 1 & mask;
            dist ++;
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
template<typename K, typename V, i32 N = 8, typename Alloc = allocator>
class map : public set<entry<K, V>, N, Alloc> {
    static V default_value;
    using entryset = set<entry<K, V>, N, Alloc>;
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
            fatal("Tried to access nonexistent map key!");
        }
        return set<entry<K, V>, N, Alloc>::find(dummy)->value;
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

template<typename K, typename V, i32 N, typename Alloc>
V map<K, V, N, Alloc>::default_value;

#endif
