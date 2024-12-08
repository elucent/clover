#ifndef UTILITY_RC_H
#define UTILITY_RC_H

#include "rt/def.h"
#include "util/malloc.h"

template<typename T>
struct rc {
    union pointee {
        i64 words[0];
        i8 data[0];
    };

    template<typename U>
    union concrete_pointee {
        i8 data[sizeof(U)];
    };

    struct blob {
        i64 count;
        pointee payload;
    };

    template<typename U>
    struct concrete_blob {
        i64 count;
        concrete_pointee<U> payload;
    };

    pointee* ptr;

    rc(): ptr(nullptr) {}

    ~rc() {
        if (ptr && !--ptr->words[-1]) {
            operator->()->~T(); 
            delete (ptr->words - 1);
        }
    }

    rc(const rc& other): ptr(other.ptr) {
        if (ptr) ptr->words[-1] ++;
    }

    template<typename U>
    rc(const rc<U>& other): ptr((pointee*)other.ptr) {
        if (ptr) ptr->words[-1] ++;
    }

    template<typename... Args>
    rc(Args&&... args): ptr((pointee*)&(new concrete_blob<T>)->payload) {
        ptr->words[-1] = 1;
        new ((T*)ptr->data) T(forward<decltype(args)>(args)...);
    }

    rc& operator=(const rc& other) {
        if (this != &other) {
            if (!--ptr->words[-1]) {
                operator->()->~T(); 
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
                operator->()->~T(); 
                delete (ptr->words - 1);
            }
            ptr = other.ptr;
            other.ptr = nullptr;
        }
    }

    const T& operator*() const {
        return *(const T*)ptr->data;
    }

    T& operator*() {
        return*(T*)ptr->data;
    }

    operator bool() const {
        return ptr;
    }

    const T* operator->() const {
        return (const T*)ptr->data;
    }

    T* operator->() {
        return (T*)ptr->data;
    }

    template<typename U>
    operator rc<U>() const {
        rc<U> newref;
        static_cast<U*>((T*)ptr);
        newref.ptr = (typename rc<U>::pointee*)(ptr);
        ptr->words[-1] ++;
        return newref;
    }
};

#endif