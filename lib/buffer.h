#ifndef BASIL_LIB_BUFFER_H
#define BASIL_LIB_BUFFER_H

#include "core/def.h"

template<typename T, uptr N>
struct buffer {
    T elts[N];
    iptr start = 0, end = 0;

    T& operator[](iptr i) {
        return elts[(start + i) & N - 1];
    }

    const T& operator[](iptr i) const {
        return elts[(start + i) & N - 1];
    }

    iptr size() const {
        return (end - start) & N - 1;
    }

    operator bool() const {
        return size();
    }

    T& peek() {
        return elts[start];
    }

    const T& peek() const {
        return elts[start];
    }

    T read() {
        T elt = peek();
        start = (start + 1) & N - 1;
        return elt;
    }

    void push(const T& t) {
        elts[end] = t;
        end = (end + 1) & N - 1;
    }

    bool full() {
        return size() == N - 1;
    }
};

#endif