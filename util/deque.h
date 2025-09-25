#ifndef UTILITY_DEQUE_H
#define UTILITY_DEQUE_H

#include "rt/def.h"
#include "util/malloc.h"

template<typename T, uptr N = 8>
struct deque {
    T* data;
    u32 start = 0, end = 0, capacity = 0, pad = 0;
    i8 fixed[N * sizeof(T)];

    void init(u32 size) {
        if (size <= N)
            data = (T*)fixed;
        else
            data = (T*)new i8[size * sizeof(T)];
        capacity = size;
    }

    void free() {
        if ((i8*)data != fixed) {
            for (u32 i = start; i != end; i = (i + 1) & (capacity - 1))
                data[i].~T();
            delete[] (i8*)data;
        }
    }

    void copy(const deque& other) {
        init(other.capacity);
        start = end = 0;
        for (u32 i = other.start; i != other.end; i = (i + 1) & (capacity - 1))
            new (&data[end ++]) T(other.data[i]);
    }

    void grow() {
        // Hold onto old buffer
        iptr old_start = start, old_end = end, old_capacity = capacity;
        T* old_data = data;

        // Grow and clear buffer
        start = 0;
        end = 0;
        capacity *= 2;
        data = (T*)new i8[sizeof(T) * capacity];

        // Copy from old buffer
        while (old_start != old_end) {
            new (&data[end ++]) T(old_data[old_start]);
            old_data[old_start].~T();
            old_start = (old_start + 1) & (old_capacity - 1);
        }

        // Free old buffer
        if ((i8*)old_data != fixed)
            delete (i8*)old_data;
    }

    deque(u32 size = N) { init(N); }
    ~deque() { free(); }
    deque(const deque& other) { copy(other); }
    deque& operator=(const deque& other) {
        if (this != &other) {
            free();
            copy(other);
        }
    }

    T& operator[](iptr i) {
        return data[(start + i) & (capacity - 1)];
    }

    const T& operator[](iptr i) const {
        return data[(start + i) & (capacity - 1)];
    }

    u32 size() const {
        if (end >= start)
            return end - start;
        else
            return (capacity - start) + end;
    }

    operator bool() const {
        return size();
    }

    void clear() {
        for (u32 i = 0; i < size(); i ++)
            operator[](i).~T();
        end = start = 0;
    }

    T& front() {
        return data[start];
    }

    const T& front() const {
        return data[start];
    }

    T& back() {
        return data[end - 1 & (capacity - 1)];
    }

    const T& back() const {
        return data[end - 1 & (capacity - 1)];
    }

    T popl() {
        T elt = front();
        start = (start + 1) & (capacity - 1);
        return elt;
    }

    T popr() {
        T elt = back();
        end = (end - 1) & (capacity - 1);
        return elt;
    }

    void pushl(const T& t) {
        if (full()) grow();
        start = (start - 1) & (capacity - 1);
        data[start] = t;
    }

    void pushr(const T& t) {
        if (full()) grow();
        data[end] = t;
        end = (end + 1) & (capacity - 1);
    }

    bool full() {
        return size() == capacity - 1;
    }
};

#endif