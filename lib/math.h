#ifndef BASIL_LIB_MATH_H
#define BASIL_LIB_MATH_H

#include "core/def.h"

template<typename T>
inline T min(T a, T b) {
    return a < b ? a : b;
}

template<typename T>
inline T max(T a, T b) {
    return a < b ? b : a;
}

inline float frem(float a, float b) {
    float i = a / b;
    return i - i64(i);
}

inline double frem(double a, double b) {
    double i = a / b;
    return i - i64(i);
}

#endif