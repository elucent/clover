#ifndef UTILITY_MAYBE_H
#define UTILITY_MAYBE_H

#include "rt/def.h"

template<typename T>
struct maybe {
    u8 bytes[sizeof(T)];
    bool exists;

    template<typename... Args>
    inline static maybe some(Args&&... args) {
        maybe m;
        new (m.bytes) T(forward<Args>(args)...);
        m.exists = true;
        return m;
    }

    inline static maybe none() {
        maybe m;
        m.exists = false;
        return m;
    }

    inline maybe(): exists(false) {}

    inline T& get() {
        return *(T*)bytes;
    }

    inline const T& get() const {
        return *(const T*)bytes;
    }

    inline T&& take() {
        return move(get());
    }

    inline ~maybe() {
        if (exists)
            get().~T();
    }

    inline maybe(const maybe& m):
        exists(m.exists) {
        if (exists)
            new (bytes) T(m.get());
    }

    inline maybe& operator=(const maybe& m) {
        if (&m != this) {
            if (exists)
                get().~T();
            exists = m.exists;
            if (exists)
                new (bytes) T(m.get());
        }
        return *this;
    }

    inline maybe(maybe&& m):
        exists(m.exists) {
        if (exists) {
            new (bytes) T(m.take());
            m.exists = false;
        }
    }

    inline maybe& operator=(maybe&& m) {
        if (&m != this) {
            if (exists)
                get().~T();
            exists = m.exists;
            if (exists) {
                new (bytes) T(m.take());
                m.exists = false;
            }
        }
        return *this;
    }

    inline operator bool() const { return exists; }
    inline T& operator*() { return get(); }
    inline const T& operator*() const { return get(); }
    inline T* operator->() { return &get(); }
    inline const T* operator->() const { return &get(); }
};

template<typename T, typename... Args>
inline maybe<T> some(Args&&... args) {
    return maybe<T>::some(forward<Args>(args)...);
}

template<typename T>
inline maybe<T> none() {
    return maybe<T>::none();
}

#endif