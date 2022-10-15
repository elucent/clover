#ifndef BASIL_LIB_TUPLE_H
#define BASIL_LIB_TUPLE_H

template<typename A, typename B>
struct pair {
    A first;
    B second;

    inline bool operator==(const pair& other) const {
        return first == other.first && second == other.second;
    }
};

#endif