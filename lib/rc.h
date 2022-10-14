#ifndef BASIL_LIB_RC_H
#define BASIL_LIB_RC_H

#include "core/def.h"
#include "lib/malloc.h"

template<typename T>
struct rc {
    struct blob {
        i64 count;
        union blob_union {
            i8 data[sizeof(T)];
            T item;

            blob_union() {}
        };
    };

    blob* ptr;

    template<typename... Args>
    rc(Args... args): ptr(new blob) {
        ptr->count = 1;
        new (&ptr->item) T(args...);
    }

    rc(): ptr(nullptr) {}

    ~rc() {
        if (!--ptr->count) {
            ptr->item.~T(); 
            delete ptr;
        }
    }

    rc(const rc& other): ptr(other.ptr) {
        if (ptr) ptr->count ++;
    }

    rc& operator=(const rc& other) {
        if (this != &other) {
            if (!--ptr->count) {
                ptr->item.~T(); 
                delete ptr;
            }
            ptr = other.ptr;
            if (ptr) ptr->count ++;
        }
    }

    rc(rc&& other): ptr(other.ptr) {
        other.ptr = nullptr;
    }

    rc& operator=(rc&& other) {
        if (this != &other) {
            if (!--ptr->count) {
                ptr->item.~T(); 
                delete ptr;
            }
            ptr = other.ptr;
            other.ptr = nullptr;
        }
    }

    const T& operator*() const {
        return ptr->item;
    }

    T& operator*() {
        return ptr->item;
    }

    operator bool() const {
        return ptr;
    }

    const T* operator->() const {
        return &ptr->item;
    }

    T* operator->() {
        return &ptr->item;
    }
};

#endif