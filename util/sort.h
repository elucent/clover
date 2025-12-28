#ifndef UTILITY_SORT_H
#define UTILITY_SORT_H

#include "util/heap.h"

template<typename Iter, typename T = valtype<decltype(*Iter())>, typename Func = LessThan<T>>
void heapsort(Iter begin, Iter end, Func&& func = LessThan<T>{}) {
    heap<T, Func> h(move(func));
    Iter scan = begin;
    while (scan != end) {
        h.push(*scan);
        ++ scan;
    }
    while (begin != end) {
        *begin = h.pop();
        ++ begin;
    }
}

template<typename Container, typename T = valtype<decltype(*declval<Container>().begin())>, typename Func = LessThan<T>>
void heapsort(Container& container, Func&& func = LessThan<T>{}) {
    heapsort(container.begin(), container.end(), move(func));
}

template<typename Iter1, typename Iter2, typename Func>
void merge(Iter1 destBegin, Iter1 destEnd, Iter2 srcBegin, Iter2 srcEnd, Func&& func) {
    // This function assumes that the range [destBegin:destEnd) is followed by enough
    // capacity to store all of [srcBegin:srcEnd).
    Iter1 out = destBegin + (destEnd - destBegin) + (srcEnd - srcBegin);
    -- srcEnd, -- destEnd, -- out;
    while (out >= destBegin) {
        if (srcEnd < srcBegin)
            *out -- = move(*destEnd --);
        else if (destEnd < destBegin)
            *out -- = move(*srcEnd --);
        else if (!func(*destEnd, *srcEnd)) // destEnd is not less than srcEnd; since we are writing in reverse, this means we use destEnd
            *out -- = move(*destEnd --);
        else
            *out -- = move(*srcEnd --);
    }
}

template<typename Iter, typename T = valtype<decltype(*Iter())>, typename Func = LessThan<T>>
void mergesort(Iter begin, Iter end, Func&& func = LessThan<T>{}) {
    u32 n = end - begin;
    T* storage = (T*)new i8[sizeof(T) * n / 2];

    i32 steps = 32 - clz32(n);
    u32 stride = 1;
    for (u32 i = 0; i < steps; i ++) {
        for (u32 j = 0; j < n / 2 && stride < n; j += stride) {
            Iter destBegin = begin + j * 2;
            Iter destEnd = begin + j * 2 + stride;
            if (destEnd >= end)
                continue;
            Iter sectionEnd = begin + j * 2 + stride * 2;
            if (sectionEnd > end)
                sectionEnd = end;
            auto srcBegin = storage + j;
            auto srcEnd = storage + j + (sectionEnd - destEnd);
            for (auto k = destEnd, l = srcBegin; k != sectionEnd; k ++, l ++)
                new(l) T(move(*k));
            merge(destBegin, destEnd, srcBegin, srcEnd, move(func));
        }
        stride *= 2;
    }
    delete[] (i8*)storage;
}

template<typename Container, typename T = valtype<decltype(*declval<Container>().begin())>, typename Func = LessThan<T>>
void mergesort(Container& container, Func&& func = LessThan<T>{}) {
    mergesort(container.begin(), container.end(), move(func));
}

template<typename Iter, typename T = valtype<decltype(*Iter())>, typename Func = LessThan<T>>
void bubblesort(Iter begin, Iter end, const Func& func = LessThan<T>{}) {
    bool swapped = true;
    while (swapped) {
        swapped = false;
        for (Iter a = begin + 1; a != end; ++ a) if (func(*a, *(a - 1)))
            swap(*a, *(a - 1)), swapped = true;
    }
}

template<typename Container, typename T = valtype<decltype(*declval<Container>().begin())>, typename Func = LessThan<T>>
void bubblesort(Container& container, const Func& func = LessThan<T>{}) {
    bubblesort(container.begin(), container.end(), move(func));
}

template<typename Iter, typename T = valtype<decltype(*Iter())>, typename Func = LessThan<T>>
void insort(Iter begin, Iter end, const Func& func = LessThan<T>{}) {
    for (Iter a = begin + 1; a != end; ++ a) {
        for (Iter b = a; b > begin && func(*b, *(b - 1)); -- b)
            swap(*b, *(b - 1));
    }
}

template<typename Container, typename T = valtype<decltype(*declval<Container>().begin())>, typename Func = LessThan<T>>
void insort(Container& container, const Func& func = LessThan<T>{}) {
    insort(container.begin(), container.end(), move(func));
}

template<typename Iter, typename Func>
ALWAYSINLINE void sort2(Iter a, Iter b, const Func& func) {
    if (func(*b, *a))
        swap(*a, *b);
}

template<typename Iter, typename T = valtype<decltype(*Iter())>, typename Func = LessThan<T>>
void quicksort(Iter begin, Iter end, const Func& func = LessThan<T>{}) {
    constexpr u32 insertionSortThreshold = 48;

    u32 n = end - begin;
    switch (n) {
        case 0 ... 1:
            return; // Trivially sorted.
        case 2:
            sort2(begin, begin + 1, func); // One swap will suffice.
            return;
        case 3:
            sort2(begin, begin + 2, func); // This and all sorts until N=6 use the optimal sorting network of swaps.
            sort2(begin, begin + 1, func);
            sort2(begin + 1, begin + 2, func);
            return;
        case 4:
            sort2(begin, begin + 2, func);
            sort2(begin + 1, begin + 3, func);
            sort2(begin, begin + 1, func);
            sort2(begin + 2, begin + 3, func);
            sort2(begin + 1, begin + 2, func);
            return;
        case 5:
            sort2(begin, begin + 3, func);
            sort2(begin + 1, begin + 4, func);
            sort2(begin, begin + 2, func);
            sort2(begin + 1, begin + 3, func);
            sort2(begin, begin + 1, func);
            sort2(begin + 2, begin + 4, func);
            sort2(begin + 1, begin + 2, func);
            sort2(begin + 3, begin + 4, func);
            sort2(begin + 2, begin + 3, func);
            return;
        case 6 ... insertionSortThreshold:
            insort(begin, end, func);
            return;
        default:
            break;
    }

    // Select pivot by median-of-three

    Iter mid = begin + n / 2;
    if (func(*mid, *begin))
        swap(*mid, *begin);
    if (func(*(end - 1), *begin))
        swap(*begin, *(end - 1));
    if (func(*(end - 1), *mid))
        swap(*mid, *(end - 1));
    auto item = *mid;

    // Partition using Hoare's method

    Iter a = begin, b = end - 1, pivot = a;
    while (true) {
        while (!func(item, *a)) ++ a;
        while (func(item, *b)) -- b;
        if (a >= b) {
            pivot = b;
            break;
        }
        swap(*a, *b);
    }

    // Recursively sort subarrays

    quicksort(begin, pivot + 1, func);
    quicksort(pivot + 1, end, func);
}

template<typename Container, typename T = valtype<decltype(*declval<Container>().begin())>, typename Func = LessThan<T>>
void quicksort(Container& container, const Func& func = LessThan<T>{}) {
    quicksort(container.begin(), container.end(), move(func));
}

template<typename Iter, typename T = valtype<decltype(*Iter())>>
void reverse(Iter begin, Iter end) {
    u32 n = (end - begin) / 2;
    -- end;
    for (u32 i = 0; i < n; i ++)
        swap(*begin ++, *end --);
}

template<typename Container>
void reverse(Container& container) {
    reverse(container.begin(), container.end());
}

template<typename Iter, typename T = valtype<decltype(*Iter())>, typename Func = LessThan<T>>
void sort(Iter begin, Iter end, Func&& func = LessThan<T>{}) {
    return quicksort(begin, end, move(func));
}

template<typename Container, typename T = valtype<decltype(*declval<Container>().begin())>, typename Func = LessThan<T>>
void sort(Container& container, Func&& func = LessThan<T>{}) {
    return quicksort(container, move(func));
}

#endif