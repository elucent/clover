#ifndef UTILITY_POOL_H
#define UTILITY_POOL_H

#include "util/vec.h"

template<typename T>
struct pool {
    constexpr static u32 InvalidKey = 0xffffffffu;

    struct Entry {
        T* item;
        u32 freeList;
    
        inline const T* operator->() const {
            return item;
        }
    
        inline T* operator->() {
            return item;
        }

        inline const T& operator*() const {
            return *item;
        }

        inline T& operator*() {
            return *item;
        }
    };

    vec<Entry, 8> items;
    u32 freeList = InvalidKey;

    ~pool() {
        for (u32 i : indices(items))
            delete items[i].item;
    }

    inline const T& operator[](u32 i) const {
        return *items[i];
    }

    inline T& operator[](u32 i) {
        return *items[i];
    }

    inline u32 size() const {
        return items.size();
    }

    inline u32 claim() {
        if (freeList == InvalidKey) {
            items.push({ new T(), InvalidKey });
            return items.size() - 1;
        }
        u32 result = freeList;
        freeList = items[result].freeList;
        return result;
    }

    inline void release(u32 i) {
        #ifndef RELEASE
            u32 key = freeList;
            while (freeList != InvalidKey) {
                assert(i != freeList);
                freeList = items[freeList].freeList;
            }
        #endif

        items[i].freeList = freeList;
        freeList = i;
    }
};

#endif