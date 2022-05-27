#ifndef BASIL_LIB_META_H
#define BASIL_LIB_META_H

#include "core/def.h"

template <typename T>
struct meta {
    struct Data {
        void(*dtor)(T* t) = [](T* t) { t->~T(); };
        uptr size = sizeof(T);
    };

    static Data data;
};

template<typename T>
typename meta<T>::Data meta<T>::data;

#endif