#ifndef BASIL_LIB_HASH_H
#define BASIL_LIB_HASH_H

#include "core/def.h"
#include "lib/malloc.h"

extern iptr lookups, scans;

template<typename T>
struct prehash {
    u64 h;
    T t;
    prehash(const T& t_in): t(t_in), h(hash(t_in)) {}

    inline bool operator==(const prehash& other) const {
        return t == other.t;
    }

    inline operator T&() {
        return t;
    }

    inline operator const T&() const {
        return t;
    }
};

template<typename T>
inline u64 hash(const prehash<T>& p) {
    return p.h;
}

u64 raw_hash(const void* input, u64 size);

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
        EMPTY, GHOST, FILLED
    };

    struct bucket {
        u8 data[sizeof(T)];
        bucket_status status;

        inline bucket() {}

        inline ~bucket() {
            if (status == FILLED) (*(T*)data).~T();
        }

        inline bucket(const bucket& other): status(other.status) {
            if (status == FILLED) new(data) T(*(T*)other.data);
        }

        inline bucket& operator=(const bucket& other) {
            if (this != &other) {
                evict();
                status = other.status;
                if (status == FILLED) new(data) T(*(T*)other.data);
            }
            return *this;
        }

        inline const T& value() const {
            return *(const T*)data;
        }

        inline T& value() {
            return *(T*)data;
        }

        inline void fill(const T& value) {
            if (status != FILLED) {
                status = FILLED, new(data) T(value);
            }
            else this->value() = value;
        }

        inline void evict() {
            if (status == FILLED) {
                (*(T*)data).~T();
                status = GHOST;
            }
        }

        inline void clear() {
            if (status == FILLED) (*(T*)data).~T();
            status = EMPTY;
        }
    };

    bucket* data;
    u32 _size, _capacity;
    bucket fixed[N];

    inline void init(u32 size) {
        _size = 0, _capacity = size;
        data = _capacity <= N ? fixed : new(*alloc) bucket[size];
        for (u32 i = 0; i < _capacity; i ++) data[i].status = EMPTY;
    }

    inline void swap(T& a, T& b) {
        T t = a;
        a = b;
        b = t;
    }

    inline void free() {
        if (_capacity > N) alloc->free(data);
    }

    inline void copy(const bucket* bs) {
        for (u32 i = 0; i < _capacity; ++ i) {
            data[i] = bs[i];
        }
    }

    inline void grow() {
        bucket* old = data;
        u32 oldsize = _capacity;
        init(_capacity * 2);
        for (u32 i = 0; i < oldsize; ++ i) {
            if (old[i].status == FILLED) insert(old[i].value());
        }
        if (oldsize > N) alloc->free(old);
    }

public:
    Alloc* alloc = Alloc::instance;

    set() {
        init(N);
    }

    ~set() {
        free();
    }

    set(const set& other) {
        init(other._capacity);
        _size = other._size;
        copy(other.data);
    }

    set& operator=(const set& other) {
        if (this != &other) {
            free();
            init(other._capacity);
            _size = other._size;
            copy(other.data);
        }
        return *this;
    }

    class const_iterator {
        const bucket *ptr, *end;
        friend class set;
    public:
        const_iterator(const bucket* ptr_in, const bucket* end_in): 
            ptr(ptr_in), end(end_in) {
            //
        }

        const T& operator*() const {
            return ptr->value();
        }

        const T* operator->() const {
            return &(ptr->value());
        }

        const_iterator& operator++() {
            if (ptr != end) ++ ptr;
            while (ptr != end && ptr->status != FILLED) ++ ptr;
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
        bucket *ptr, *end;
        friend class set;
    public:
        iterator(bucket* ptr_in, bucket* end_in): ptr(ptr_in), end(end_in) {
            //
        }

        T& operator*() {
            return ptr->value();
        }

        T* operator->() {
            return &(ptr->value());
        }

        iterator& operator++() {
            if (ptr != end) ++ ptr;
            while (ptr != end && ptr->status != FILLED) ++ ptr;
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
        bucket *start = data, *end = data + _capacity;
        while (start != end && start->status != FILLED) ++ start;
        return iterator(start, end);
    }

    const_iterator begin() const {
        const bucket *start = data, *end = data + _capacity;
        while (start != end && start->status != FILLED) ++ start;
        return const_iterator(start, end);
    }

    inline iterator end() {
        return iterator(data + _capacity, data + _capacity);
    }

    inline const_iterator end() const {
        return const_iterator(data + _capacity, data + _capacity);
    }

    void clear() {
        for (u64 i = 0; i < _capacity; i ++) {
            if (data[i].status == FILLED) data[i].clear();
        }
        _size = 0;
    }

    void insert(const T& t) {
        if (double(_size + 1) / double(_capacity) > 0.625) grow();
        u64 h = hash(t);
        u64 dist = 0;
        u64 _mask = _capacity - 1;
        u64 i = h & _mask;
        T item = t;
        while (true) {
            if (data[i].status == EMPTY || data[i].status == GHOST) {
                data[i].fill(item);
                ++ _size;
                return;
            }

            if (data[i].status == FILLED && data[i].value() == item) {
                data[i].value() = t; // potentially redundant assignment allows for overwriting existing values
                return;
            }

            u64 other_dist = i - hash(data[i].value()) & _mask;
            if (other_dist < dist) {
                swap(item, data[i].value());
                dist = other_dist;
            }
            i = i + 1 & _mask;
            ++ dist;
        }
    }

    void erase(const T& t) {
        u64 h = hash(t);
        u64 _mask = _capacity - 1;
        u64 i = h & _mask;
        while (true) {
            if (data[i].status == EMPTY) return;
            if (data[i].status == FILLED && data[i].value() == t) {
                data[i].evict();
                -- _size;
                return;
            }
            i = i + 1 & _mask;
        }
    }

    const_iterator find(const T& t) const {
        lookups ++;
        u64 h = hash(t);
        u64 _mask = _capacity - 1;
        u64 i = h & _mask;
        u64 dist = 0;
        while (true) {
            scans ++;
            if (data[i].status == EMPTY) return end();
            if (data[i].status != GHOST) {
                u64 oh = hash(data[i].value());
                u64 other_dist = i - oh & _mask;
                if (other_dist < dist) return end();
            }
            if (data[i].status == FILLED && t == data[i].value()) {
                return const_iterator(data + i, data + _capacity);
            }
            i = i + 1 & _mask;
            dist ++;
        }
    }

    iterator find(const T& t) {
        lookups ++;
        u64 h = hash(t);
        u64 _mask = _capacity - 1;
        u64 i = h & _mask;
        u64 dist = 0;
        while (true) {
            scans ++;
            if (data[i].status == EMPTY) return end();
            if (data[i].status != GHOST) {
                u64 dist = (i - h) & _mask;
                u64 oh = hash(data[i].value());
                u64 other_dist = i - oh & _mask;
                if (other_dist < dist) return end();
            }
            if (data[i].status == FILLED && t == data[i].value()) {
                return iterator(data + i, data + _capacity);
            }
            i = i + 1 & _mask;
            dist ++;
        }
    }

    bool contains(const T& t) const {
        return find(t) != end();
    }

    u32 size() const {
        return _size;
    }

    u32 capacity() const {
        return _capacity;
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
        return set<entry<K, V>>::find(dummy)->value;
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
