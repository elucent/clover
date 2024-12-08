#ifndef UTILITY_HEAP_H
#define UTILITY_HEAP_H

#include "util/vec.h"
#include "util/io.h"

template<typename T>
struct LessThan {
    inline bool operator()(const T& left, const T& right) const {
        return left < right;
    }

    static LessThan instance;
};

template<typename T>
LessThan<T> LessThan<T>::instance;

template<typename T>
struct GreaterThan {
    inline bool operator()(const T& left, const T& right) const {
        return right < left;
    }
};

template<typename T, typename Func = LessThan<T>>
struct heap {
    vec<T> items;
    Func func;

    inline heap(Func&& func_in):
        func(move(func_in)) {
        items._size ++;
    }

    const T& front() const {
        return items[1];
    }

    u32 size() const {
        return items.size() - 1;
    }

    void push(T item) {
        i32 i = items.size();
        items.push(item);
        while (i > 1) {
            T& child = items[i];
            i /= 2;
            T& parent = items[i];
            if (func(child, parent))
                swap(child, parent);
        }
    }

    T pop() {
        swap(items.back(), items[1]);
        T result = items.pop();
        i32 i = 1;
        while (i < items.size()) {
            i32 left = i * 2, right = i * 2 + 1;
            i32 min = i;
            if (left < items.size() && func(items[left], items[min]))
                min = left;
            if (right < items.size() && func(items[right], items[min]))
                min = right;
            if (min == i)
                break;
            else
                swap(items[i], items[min]), i = min;
        }
        return result;
    }
};

#endif