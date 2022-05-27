#ifndef BASIL_LIB_ARRAY_H
#define BASIL_LIB_ARRAY_H

#include "core/def.h"

template<typename T, uptr N>
struct array {
    T elts[N];

    T& operator[](uptr i) {
        return elts[i];
    }

    const T& operator[](uptr i) const {
        return elts[i];
    }

    uptr size() const {
        return N;
    }

    T* begin() {
        return elts;
    }

    T* end() {
        return elts + N;
    }

    const T* begin() const {
        return elts;
    }

    const T* end() const {
        return elts + N;
    }
};

#endif