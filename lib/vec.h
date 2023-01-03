#ifndef BASIL_LIB_VEC_H
#define BASIL_LIB_VEC_H

#include "core/def.h"
#include "lib/slice.h"
#include "lib/malloc.h"

template<typename T, u32 N = 8, typename Alloc = allocator>
struct vec {
    u8* data;
    Alloc* alloc = Alloc::instance;
    u32 _size, _capacity;
    u8 fixed[N * sizeof(T)];

    inline void free(u8* array) {
        if (array) {
            T* tptr = (T*)array;
            for (u32 i = 0; i < _size; i ++) tptr[i].~T();
            if (array != fixed) alloc->free(array);
        }
    }

    inline void init(u32 size) {
        _size = 0, _capacity = size;
        if (_capacity <= N) data = fixed;
        else data = new(*alloc) u8[_capacity * sizeof(T)];
    }

    inline void copy(const T* ts, u32 n) {
        _size = n;
        T* tptr = (T*)data;
        for (u32 i = 0; i < n; i ++) {
            new(tptr + i) T(ts[i]);
        }
    }

    inline void assign(const T* ts, u32 n) {
        _size = n;
        T* tptr = (T*)data;
        for (u32 i = 0; i < n; i ++) {
            new(tptr + i) T(ts[i]);
        }
    }

    inline void destruct(u32 i) {
        T* tptr = (T*)data;
        tptr[i].~T();
    }

    inline void grow() {
        u8* old = data;
        u32 oldsize = _size;
        init(_capacity * 2);
        copy((const T*)old, oldsize);
        if (old != fixed) free(old);
    }

    inline vec() {
        init(N);
    }

    inline vec(const T& item, i32 n): vec() {
        for (i32 i = 0; i < n; i ++) 
            push(item);
    }

    inline ~vec() {
        if (data != fixed) free(data);
    }

    inline vec(const vec& other) {
        init(other._capacity);
        copy((const T*)other.data, other._size);
    }

    inline vec(vec&& other): data(other.data), _size(other._size), _capacity(other._capacity) {
        if (other.data == other.fixed) {
            data = fixed;
            copy((const T*)other.data, _size);
        }
        other.data = nullptr;
    }

    inline vec& operator=(const vec& other) {
        if (this != &other) {
            free(data);
            init(other._capacity);
            assign((const T*)other.data, other._size);
        }
        return *this;
    }

    inline vec& operator=(vec&& other) {
        if (this != &other) {
            free(data);
            data = other.data;
            _size = other._size;
            _capacity = other._capacity;
            if (other.data == other.fixed) data = fixed, copy((const T*)other.data, _size);
            other.data = nullptr;
        }
        return *this;
    }

    inline void push(const T& t) {
        if (_size + 1 >= _capacity) grow();
        new((T*)data + _size ++) T(t);
    }
    
    inline T pop() {
        T last = static_cast<T&&>(back());
        -- _size;
        return last;
    }

    inline void clear() {
        for (u32 i = 0; i < _size; ++ i) destruct(i);
        _size = 0;
    }

    inline const T& operator[](u32 i) const {
        return ((T*)data)[i];
    }

    inline T& operator[](u32 i) {
        return ((T*)data)[i];
    }

    inline const T* begin() const {
        return (const T*)data;
    }

    inline T* begin() {
        return (T*)data;
    }

    inline const T* end() const {
        return (const T*)data + _size;
    }

    inline T* end() {
        return (T*)data + _size;
    }

    inline u32 size() const {
        return _size;
    }

    inline u32 capacity() const {
        return _capacity;
    }

    inline const T& front() const { 
        return *(const T*)data;
    }

    inline T& front() { 
        return *(T*)data;
    }

    inline const T& back() const { 
        return ((const T*)data)[_size - 1];
    }

    inline T& back() { 
        return ((T*)data)[_size - 1];
    }

    inline void trim(i32 n) {
        for (i32 i = 0; i < n; i ++) destruct(_size - i - 1);
        _size -= n;
    }
};

template<typename T, u32 N = 8, typename Alloc = allocator>
inline void vec_fill(vec<T, N, Alloc>& v) {}

template<typename T, u32 N = 8, typename Alloc = allocator, typename... Args>
inline void vec_fill(vec<T, N, Alloc>& v, const T& arg, const Args&... args) {
    v.push(arg);
    vec_fill(v, args...);
}

template<typename T, u32 N = 8, typename Alloc = allocator, typename... Args>
inline vec<T, N, Alloc> vec_of(const Args&... args) {
    vec<T, N, Alloc> v;
    vec_fill(v, args...);
    return v;
}

#endif