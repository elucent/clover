#ifndef BASIL_LIB_RC_H
#define BASIL_LIB_RC_H

#include "core/def.h"
#include "lib/malloc.h"

template<typename T>
struct rc {
    union pointee {
        i64 words[1];
        i8 data[sizeof(T)];
        T item;
    };

    struct blob {
        i64 count;
        pointee payload;
    };

    pointee* ptr;

    template<typename... Args>
    rc(Args... args): ptr(&(new blob)->payload) {
        ptr->words[-1] = 1;
        new (&ptr->item) T(args...);
    }

    rc(): ptr(nullptr) {}

    ~rc() {
        if (!--ptr->words[-1]) {
            ptr->item.~T(); 
            delete (ptr->words - 1);
        }
    }

    rc(const rc& other): ptr(other.ptr) {
        if (ptr) ptr->words[-1] ++;
    }

    rc& operator=(const rc& other) {
        if (this != &other) {
            if (!--ptr->words[-1]) {
                ptr->item.~T(); 
                delete (ptr->words - 1);
            }
            ptr = other.ptr;
            if (ptr) ptr->words[-1] ++;
        }
    }

    rc(rc&& other): ptr(other.ptr) {
        other.ptr = nullptr;
    }

    rc& operator=(rc&& other) {
        if (this != &other) {
            if (!--ptr->words[-1]) {
                ptr->item.~T(); 
                delete (ptr->words - 1);
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

    template<typename U>
    operator rc<U>() const {
        rc<U> newref;
        newref->ptr = static_cast<U*>(ptr);
        ptr->words[-1] ++;
        return newref;
    }
};

#endif