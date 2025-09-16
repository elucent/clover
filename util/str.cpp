#include "util/str.h"
#include "util/utf.h"

u64 touint(const_slice<i8> str) {
    u64 acc = 0;
    u64 i = 0;
    for (; i < str.size(); i ++) {
        acc *= 10;
        i8 b = str[i];
        if UNLIKELY(b < 0) { // UTF-8
            rune r;
            const i8* new_ptr = utf8_decode_forward(str.data(), &r);
            i += new_ptr - (str.data() + i) - 1;
            acc += utf8_digit_value(r);
        }
        else acc += str[i] - '0';
    }
    return acc;
}

u64 hextouint(const_slice<i8> str) {
    u64 acc = 0;
    u64 i = 0;

    for (; i < str.size(); i ++) {
        acc <<= 4;
        i8 b = str[i];
        if (b < 0) { // UTF-8
            rune r;
            const i8* new_ptr = utf8_decode_forward(str.data(), &r);
            i += new_ptr - (str.data() + i) - 1;
            acc += utf8_digit_value(r);
        }
        else if (b >= '0' && b <= '9')
            acc += b - '0';
        else if (b >= 'a' && b <= 'f')
            acc += b - 'a' + 10;
        else if (b >= 'A' && b <= 'F')
            acc += b - 'A' + 10;
    }
    return acc;
}

u64 octaltouint(const_slice<i8> str) {
    u64 acc = 0;
    u64 i = 0;

    for (; i < str.size(); i ++) {
        acc <<= 3;
        i8 b = str[i];
        if (b < 0) { // UTF-8
            rune r;
            const i8* new_ptr = utf8_decode_forward(str.data(), &r);
            i += new_ptr - (str.data() + i) - 1;
            acc += utf8_digit_value(r);
        }
        else if (b >= '0' && b <= '7')
            acc += b - '0';
    }
    return acc;
}

u64 binarytouint(const_slice<i8> str) {
    u64 acc = 0;
    for (i8 i : str) {
        acc <<= 1;
        if (i == '1') acc |= 1;
    }
    return acc;
}

i64 toint(const_slice<i8> str) {
    i64 acc = 0;
    i64 i = 0;
    i64 negative = 1;
    if (str[i] == '-') i ++, negative = -1;
    for (; i < str.size(); i ++) {
        acc *= 10;
        i8 b = str[i];
        if UNLIKELY(b < 0) { // UTF-8
            rune r;
            const i8* new_ptr = utf8_decode_forward(str.data(), &r);
            i += new_ptr - (str.data() + i) - 1;
            acc += utf8_digit_value(r);
        }
        else acc += str[i] - '0';
    }
    return acc * negative;
}

i64 hextoint(const_slice<i8> str) {
    i64 acc = 0;
    i64 i = 0;

    for (; i < str.size(); i ++) {
        acc <<= 4;
        i8 b = str[i];
        if (b < 0) { // UTF-8
            rune r;
            const i8* new_ptr = utf8_decode_forward(str.data(), &r);
            i += new_ptr - (str.data() + i) - 1;
            acc += utf8_digit_value(r);
        }
        else if (b >= '0' && b <= '9')
            acc += b - '0';
        else if (b >= 'a' && b <= 'f')
            acc += b - 'a' + 10;
        else if (b >= 'A' && b <= 'F')
            acc += b - 'A' + 10;
    }
    return acc;
}

i64 octaltoint(const_slice<i8> str) {
    i64 acc = 0;
    u64 i = 0;

    for (; i < str.size(); i ++) {
        acc <<= 3;
        i8 b = str[i];
        if (b < 0) { // UTF-8
            rune r;
            const i8* new_ptr = utf8_decode_forward(str.data(), &r);
            i += new_ptr - (str.data() + i) - 1;
            acc += utf8_digit_value(r);
        }
        else if (b >= '0' && b <= '7')
            acc += b - '0';
    }
    return acc;
}

i64 binarytoint(const_slice<i8> str) {
    i64 acc = 0;
    for (i8 i : str) {
        acc <<= 1;
        if (i == '1') acc |= 1;
    }
    return acc;
}

double tofloat(const_slice<i8> str) {
    i64 acc = 0;
    i64 i = 0;
    i64 negative = 1;
    if (str[i] == '-') i ++, negative = -1;
    for (; i < str.size(); i ++) {
        i8 b = str[i];
        if UNLIKELY(b < 0) { // UTF-8
            acc *= 10;
            rune r;
            const i8* new_ptr = utf8_decode_forward(str.data(), &r);
            i += new_ptr - (str.data() + i) - 1;
            acc += utf8_digit_value(r);
        }
        else if (str[i] == '.') {
            i ++;
            break;
        }
        else acc *= 10, acc += str[i] - '0';
    }
    double frac = 0, fpow = 0.1;
    for (; i < str.size(); i ++) {
        i8 b = str[i];
        if UNLIKELY(b < 0) { // UTF-8
            rune r;
            const i8* new_ptr = utf8_decode_forward(str.data(), &r);
            i += new_ptr - (str.data() + i) - 1;
            acc += utf8_digit_value(r) * fpow;
        }
        else frac += (str[i] - '0') * fpow;
        fpow /= 10;
    }
    return (acc + frac) * negative;
}