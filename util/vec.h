#ifndef UTILITY_VEC_H
#define UTILITY_VEC_H

#include "rt/def.h"
#include "util/malloc.h"
#include "util/ptrhax.h"

template<typename T, typename U = IsTriviallyCopyable<T>>
struct vec_copier;

template<typename T>
struct vec_copier<T, TrueType> {
    static ALWAYSINLINE void copy(T* dst, const T* src, u32 n) {
        memory::copy(dst, src, n * sizeof(T));
    }
};

template<typename T>
struct vec_copier<T, FalseType> {
    static ALWAYSINLINE void copy(T* dst, const T* src, u32 n) {
        for (u32 i = 0; i < n; i ++)
            new (dst + i) T(src[i]);
    }
};

template<typename T, typename U = IsTriviallyCopyable<T>>
struct vec_deleter;

template<typename T>
struct vec_deleter<T, TrueType> {
    static ALWAYSINLINE void free(T* src, u32 n) {
        ::free(src);
    }
};

template<typename T>
struct vec_deleter<T, FalseType> {
    static ALWAYSINLINE void free(T* src, u32 n) {
        for (u32 i = 0; i < n; i ++)
            src[i].~T();
    }
};

template<typename T, u32 N = 8>
struct vec {
    u8* _data;
    u32 _size, _capacity;
    u8 fixed[N * sizeof(T)];

    inline void free(u8* array) {
        if (array && array != fixed)
            vec_deleter<T>::free((T*)array, _size);
    }

    inline void init(u32 size) {
        _size = 0, _capacity = size;
        if (_capacity <= N) _data = fixed;
        else _data = new u8[_capacity * sizeof(T)];
    }

    inline void copy(const T* ts, u32 n) {
        _size = n;
        vec_copier<T>::copy((T*)_data, ts, n);
    }

    inline void destruct(u32 i) {
        T* tptr = (T*)_data;
        tptr[i].~T();
    }

    inline void grow() {
        u8* old = _data;
        u32 oldsize = _size;
        init(_capacity ? _capacity * 2 : 8);
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
        if (_data != fixed) free(_data);
    }

    inline vec(const vec& other) {
        init(other._capacity);
        copy((const T*)other._data, other._size);
    }

    inline vec(vec&& other): _data(other._data), _size(other._size), _capacity(other._capacity) {
        if (other._data == other.fixed) {
            _data = fixed;
            copy((const T*)other._data, _size);
        }
        other._data = nullptr;
    }

    inline vec& operator=(const vec& other) {
        if (this != &other) {
            free(_data);
            init(other._capacity);
            copy((const T*)other._data, other._size);
        }
        return *this;
    }

    inline vec& operator=(vec&& other) {
        if (this != &other) {
            free(_data);
            _data = other._data;
            _size = other._size;
            _capacity = other._capacity;
            if (other._data == other.fixed) _data = fixed, copy((const T*)other._data, _size);
            other._data = nullptr;
        }
        return *this;
    }

    inline void push(const T& t) {
        if UNLIKELY(_size + 1 > _capacity)
            grow();
        new((T*)_data + _size ++) T(t);
    }

    inline void pushUnchecked(const T& t) {
        new((T*)_data + _size ++) T(t);
    }

    inline void reserveTo(i64 n) {
        while (_capacity < n)
            grow();
    }

    inline void reserveBy(i64 n) {
        while (_capacity < _size + n)
            grow();
    }

    template<typename... Args>
    inline void expandTo(i64 n, Args&&... args) {
        if (n <= _size)
            return;
        while UNLIKELY(n > _capacity)
            grow();
        for (i64 i = _size; i < n; i ++)
            new (data() + _size ++) T(forward<Args>(args)...);
    }

    template<typename... Args>
    inline void expandBy(i64 n, Args&&... args) {
        if (n <= 0)
            return;
        while UNLIKELY(_size + n > _capacity)
            grow();
        for (i64 i = 0; i < n; i ++)
            new (data() + _size ++) T(forward<Args>(args)...);
    }

    inline void expandTo(i64 n) {
        if (n <= _size)
            return;
        while UNLIKELY(n > _capacity)
            grow();
        for (i64 i = _size; i < n; i ++)
            new (data() + _size ++) T();
    }

    inline void expandBy(i64 n) {
        if (n <= 0)
            return;
        while UNLIKELY(_size + n > _capacity)
            grow();
        for (i64 i = 0; i < n; i ++)
            new (data() + _size ++) T();
    }

    inline void shrinkBy(i64 n) {
        if (n > _size)
            n = _size;
        i64 prevSize = _size;
        _size -= n;
        for (i64 i = _size; i < prevSize; i ++)
            destruct(i);
    }

    template<typename... Args>
    inline void push(const T& t, const Args&... args) {
        push(t);
        push(args...);
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
        assert(i < _size);
        return ((T*)_data)[i];
    }

    inline T& operator[](u32 i) {
        assert(i < _size);
        return ((T*)_data)[i];
    }

    inline const_slice<T> operator[](pair<u32, u32> p) const {
        assert(p.first <= p.second);
        assert(p.first <= _size);
        assert(p.second <= _size);
        return { ((const T*)_data) + p.first, p.second - p.first };
    }

    inline slice<T> operator[](pair<u32, u32> p) {
        assert(p.first <= p.second);
        assert(p.first <= _size);
        assert(p.second <= _size);
        return { ((T*)_data) + p.first, p.second - p.first };
    }

    inline const T* begin() const {
        return (const T*)_data;
    }

    inline T* data() {
        return (T*)_data;
    }

    inline T* begin() {
        return (T*)_data;
    }

    inline const T* end() const {
        return (const T*)_data + _size;
    }

    inline T* end() {
        return (T*)_data + _size;
    }

    inline u32 size() const {
        return _size;
    }

    inline u32 capacity() const {
        return _capacity;
    }

    inline const T& front() const {
        return *(const T*)_data;
    }

    inline T& front() {
        return *(T*)_data;
    }

    inline const T& back() const {
        return ((const T*)_data)[_size - 1];
    }

    inline T& back() {
        return ((T*)_data)[_size - 1];
    }

    inline void trim(i32 n) {
        for (i32 i = 0; i < n; i ++) destruct(_size - i - 1);
        _size -= n;
    }

    inline operator const_slice<T>() const {
        return { begin(), size() };
    }

    inline operator slice<T>() {
        return { begin(), size() };
    }

    inline void insert(i32 index, const T& element) {
        if (_size + 1 > _capacity) grow();
        _size ++;
        for (i32 i = _size - 1; i > index; i --)
            ((T*)_data)[i] = ((T*)_data)[i - 1];
        ((T*)_data)[index] = element;
    }

    inline void append(const_slice<T> v) {
        while (_size + v.size() > _capacity)
            grow();
        for (const T& t : v)
            new ((T*)_data + _size ++) T(t);
    }

    template<u32 M>
    inline void append(const vec<T, M>& v) {
        while (_size + v.size() > _capacity)
            grow();
        for (const T& t : v)
            new ((T*)_data + _size ++) T(t);
    }

    template<typename Func>
    inline void removeIf(Func&& func) {
        T* writer = (T*)_data;
        T* reader = writer;

        for (u32 i = 0; i < _size; i ++) {
            if (!func(*reader))
                *writer ++ = *reader;
            ++ reader;
        }
        _size = writer - (T*)_data;
    }

    template<typename Func>
    inline void removeIfIndexed(Func&& func) {
        T* writer = (T*)_data;
        T* reader = writer;

        for (u32 i = 0; i < _size; i ++) {
            if (!func(i, *reader))
                *writer ++ = *reader;
            ++ reader;
        }
        _size = writer - (T*)_data;
    }

    inline T remove(i32 index) {
        T element = ((T*)_data)[index];
        for (i32 i = index; i < _size + 1; i ++)
            ((T*)_data)[i] = ((T*)_data)[i + 1];
        pop();
        return element;
    }

    inline slice<T> take_slice() {
        slice<T> sl;
        if (_data == fixed) {
            sl = { (T*)new i8[sizeof(T) * _size], _size };
            for (i32 i = 0; i < _size; i ++)
                new (&sl[i]) T(operator[](i));
        } else
            sl = { (T*)_data, _size };
        _data = nullptr;
        return sl;
    }

    inline const_slice<T> drop(u32 n) const {
        return ((const_slice<T>)*this).drop(n);
    }

    inline slice<T> drop(u32 n) {
        return ((slice<T>)*this).drop(n);
    }

    inline const_slice<T> take(u32 n) const {
        return ((const_slice<T>)*this).take(n);
    }

    inline slice<T> take(u32 n) {
        return ((slice<T>)*this).take(n);
    }

    inline void push() {}
};

template<typename T, u32 N = 8>
inline void vec_fill(vec<T, N>& v) {}

template<typename T, u32 N = 8, typename... Args>
inline void vec_fill(vec<T, N>& v, const T& arg, const Args&... args) {
    v.push(arg);
    vec_fill(v, args...);
}

template<typename T, u32 N = 8, typename... Args>
inline vec<T, N> vec_of(const Args&... args) {
    vec<T, N> v;
    vec_fill(v, args...);
    return v;
}

template<typename T, u32 N = max<i64>(3, i64(sizeof(void*)) - i64(sizeof(T)))>
struct tinyvec {
    using I = bit_int<T>;
    using U = bit_uint<T>;
    using P = packed_ptr<u8>;

    U sz;
    u8 fixed[N * sizeof(T)];

    constexpr static bool CanGoOutOfLine = sizeof(T) * N >= sizeof(P);

    inline tinyvec():
        sz(0) {}

    inline ~tinyvec() {
        if constexpr (CanGoOutOfLine) {
            if (sz > N)
                free(raw());
        }
    }

    inline u8* raw() {
        if (sz <= N)
            return fixed;
        return load<P>(fixed + sizeof(fixed) - sizeof(P)).unpack();
    }

    inline T* data() {
        return (T*)raw();
    }

    inline void free(const void* data) {
        u32 addedSpace = max<u32>(4, sizeof(T));
        delete[] ((u8*)data - addedSpace);
    }

    inline void setRaw(void* ptr) {
        auto p = P((T*)ptr);
        store<P>(p, fixed + sizeof(fixed) - sizeof(P));
    }

    inline u32 capacity(const void* data) {
        return load<u32>((u8*)data - 4);
    }

    inline void setCapacity(void* data, u32 capacity) {
        store<u32>(capacity, (u8*)data - 4);
    }

    inline void grow() {
        if constexpr (!CanGoOutOfLine)
            unreachable("Tried to grow tiny vector that doesn't have room to go out of line!");
        const T* old = data();
        u32 capacity = sz <= N ? (N > 4 ? 16 : 8) : this->capacity(old) * 2;

        u32 addedSpace = max<u32>(4, sizeof(T));
        u8* data = new u8[capacity * sizeof(T) + addedSpace];
        setCapacity(data + addedSpace, capacity);
        vec_copier<T>::copy((T*)(data + addedSpace), old, sz);
        setRaw(data + addedSpace);
        if (old != (const T*)fixed)
            free(old);
    }

    inline void push(const T& t) {
        if (sz == N || (sz > N && sz >= capacity(data())))
            grow();
        sz ++;
        new (data() + sz - 1) T(t);
    }

    inline void shrink(T* old) {
        vec_copier<T>::copy((T*)fixed, old, sz);
        free(old);
    }

    inline T pop() {
        T* dat = data();
        T result = dat[-- sz];
        if (sz == N)
            shrink(dat);
        return result;
    }

    inline const T& operator[](u32 i) const {
        return data()[i];
    }

    inline T& operator[](u32 i) {
        return data()[i];
    }

    inline const T* begin() const {
        return data();
    }

    inline T* begin() {
        return data();
    }

    inline const T* end() const {
        return data() + sz;
    }

    inline T* end() {
        return data() + sz;
    }

    inline const T& front() const {
        return data()[0];
    }

    inline T& front() {
        return data()[0];
    }

    inline const T& back() const {
        return data()[sz - 1];
    }

    inline T& back() {
        return data()[sz - 1];
    }

    inline u32 size() const {
        return sz;
    }

    template<typename Func>
    inline void removeIf(Func&& func) {
        T* begin = data();
        T* writer = begin;
        T* reader = writer;

        for (u32 i = 0; i < sz; i ++) {
            if (!func(*reader))
                *writer ++ = *reader;
            ++ reader;
        }
        sz = writer - begin;
        if (reader - begin > N && sz <= N)
            shrink(begin);
    }
};

#endif