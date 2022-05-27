#include "lib/str.h"
#include "lib/utf.h"

i64 toint(const_slice<i8> str) {
    i64 acc = 0;
    i64 i = 0;
    i64 negative = 1;
    if (str[i] == '-') i ++, negative = -1;
    for (; i < str.n; i ++) {
        acc *= 10;
        i8 b = str[i];
        if (b < 0) { // UTF-8
            rune r;
            const i8* new_ptr = utf8_decode_forward(str.ptr, &r);
            i += new_ptr - (str.ptr + i) - 1;
            acc += utf8_digit_value(r);
        }
        else acc += str[i] - '0';
    }
    return acc * negative;
}

double tofloat(const_slice<i8> str) {
    i64 acc = 0;
    i64 i = 0;
    i64 negative = 1;
    if (str[i] == '-') i ++, negative = -1;
    for (; i < str.n; i ++) {
        i8 b = str[i];
        if (b < 0) { // UTF-8
            acc *= 10;
            rune r;
            const i8* new_ptr = utf8_decode_forward(str.ptr, &r);
            i += new_ptr - (str.ptr + i) - 1;
            acc += utf8_digit_value(r);
        }
        else if (str[i] == '.') {
            i ++;
            break;
        }
        else acc *= 10, acc += str[i] - '0';
    }
    double frac = 0, fpow = 0.1;
    for (; i < str.n; i ++) {
        i8 b = str[i];
        if (b < 0) { // UTF-8
            rune r;
            const i8* new_ptr = utf8_decode_forward(str.ptr, &r);
            i += new_ptr - (str.ptr + i) - 1;
            acc += utf8_digit_value(r) * fpow;
        }
        else frac += (str[i] - '0') * fpow;
        fpow /= 10;
    }
    return (acc + frac) * negative;
}