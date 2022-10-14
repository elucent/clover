#ifndef BASIL_LIB_SLICE_H
#define BASIL_LIB_SLICE_H

#include "core/util.h"

/*
 * swap(lhs, rhs)
 *
 * Swaps the values beneath the two provided references.
 */
template<typename T>
void swap(T& a, T& b) {
    T t = a;
    a = b;
    b = t;
}

/*
 * const_slice<T>
 *
 * Immutable sized pointer type. Represents a valid sequence of values of length
 * n following the starting address ptr.
 */
template <typename T>
struct const_slice {
    const T* ptr;
    iptr n;

    const_slice() {}
    const_slice(const T* ptr_in, iptr n_in): ptr(ptr_in), n(n_in) {}
    const_slice(const T* start, const T* end): ptr(start), n(end - start) {}

    iptr size() const {
        return n;
    }

    bool empty() const {
        return n;
    }

    const T& operator[](iptr i) const {
        return ptr[i];
    }

    const T* begin() const {
        return ptr;
    }

    const T* end() const {
        return ptr + n;
    }

    bool operator==(const const_slice& other) const {
        if (n != other.n) return false;
        for (iptr i = 0; i < n; i ++) if (ptr[i] != other.ptr[i]) return false;
        return true;
    }
};


/*
 * slice<T>
 *
 * Mutable sized pointer type. Represents a valid sequence of values of length
 * n following the starting address ptr.
 */
template <typename T>
struct slice {
    T* ptr;
    iptr n;

    slice() {}
    slice(T* ptr_in, iptr n_in): ptr(ptr_in), n(n_in) {}
    slice(T* start, T* end): ptr(start), n(end - start) {}

    iptr size() const {
        return n;
    }

    bool empty() const {
        return n;
    }

    T& operator[](iptr i) {
        return ptr[i];
    }

    const T& operator[](iptr i) const {
        return ptr[i];
    }

    T* begin() {
        return ptr;
    }

    T* end() {
        return ptr + n;
    }

    const T* begin() const {
        return ptr;
    }

    const T* end() const {
        return ptr + n;
    }

    bool operator==(const slice& other) const {
        return ptr == other.ptr && n == other.n;
    }

    operator const_slice<T>() const {
        return { ptr, n };
    }
};

#endif