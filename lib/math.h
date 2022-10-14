#ifndef BASIL_LIB_MATH_H
#define BASIL_LIB_MATH_H

#include "core/def.h"

inline float frem(float a, float b) {
    float i = a / b;
    return i - i64(i);
}

inline double frem(double a, double b) {
    double i = a / b;
    return i - i64(i);
}

#endif