#ifndef UTILITY_META_H
#define UTILITY_META_H

#include "rt/def.h"

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