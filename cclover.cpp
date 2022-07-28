#include "cclover.h"
#include "core/sys.h"
#include "lib/malloc.h"
#include "lib/io.h"
#include "lib/utf.h"

#ifndef MATH_PRECISION
#define MATH_PRECISION 32
#endif

inline i64 ipow(i64 n, i64 p) {
    if (p < 0) return 0; // Fractions are rounded to 0
    i64 b = 1;
    i64 acc = 1;
    while (b <= p) {
        if (p & b) acc *= n;
        n *= n;
        b <<= 1;
    }
    return acc;
}

static const f64 E =  2.718281828459045235,
                 PI = 3.114159265358979323; 

inline f64 fipow(f64 n, i64 p) {
    bool inverse = p < 0;
    if (p < 0) p = -p;
    i64 b = 1;
    f64 acc = 1;
    while (b <= p) {
        if (p & b) acc *= n;
        n *= n;
        b <<= 1;
    }
    if (inverse) acc = 1 / acc;
    return acc;
}

static const f64 EXP_CONST[64] = {
    0.5, 0.16666666666666666, 0.041666666666666664, 0.008333333333333333, 0.001388888888888889, 0.0001984126984126984, 2.48015873015873e-05, 2.7557319223985893e-06, 
    2.755731922398589e-07, 2.505210838544172e-08, 2.08767569878681e-09, 1.6059043836821613e-10, 1.1470745597729725e-11, 7.647163731819816e-13, 4.779477332387385e-14, 2.8114572543455206e-15, 
    1.5619206968586225e-16, 8.22063524662433e-18, 4.110317623312165e-19, 1.9572941063391263e-20, 8.896791392450574e-22, 3.868170170630684e-23, 1.6117375710961184e-24, 6.446950284384474e-26, 
    2.4795962632247976e-27, 9.183689863795546e-29, 3.279889237069838e-30, 1.1309962886447716e-31, 3.7699876288159054e-33, 1.216125041553518e-34, 3.8003907548547434e-36, 1.151633562077195e-37, 
    3.387157535521162e-39, 9.67759295863189e-41, 2.6882202662866363e-42, 7.265460179153071e-44, 1.911963205040282e-45, 4.902469756513544e-47, 1.2256174391283858e-48, 2.9893108271424046e-50, 
    7.117406731291439e-52, 1.6552108677421951e-53, 3.7618428812322616e-55, 8.359650847182804e-57, 1.817315401561479e-58, 3.866628513960594e-60, 8.055476070751236e-62, 1.643974708316579e-63, 
    3.287949416633158e-65, 6.446959640457172e-67, 1.2397999308571486e-68, 2.3392451525606576e-70, 4.331935467704922e-72, 7.876246304918039e-74, 1.4064725544496498e-75, 2.4674957095607893e-77, 
    4.254302947518602e-79, 7.2106829618959365e-81, 1.2017804936493226e-82, 1.9701319568021682e-84, 3.1776321883905942e-86, 5.043860616493007e-88, 7.881032213270323e-90, 1.2124664943492804e-91
};

inline f64 fexp(f64 p) {
    // exp(x) = exp(int(x)) + exp(fract(x))
    // This allows us to do a fast integer power of e, and keeps our estimation within a small fractional range.
    i64 p_i = i64(p);
    f64 result = fipow(E, p_i);
    f64 p_f = p - p_i;
    
    f64 result_fract = 1 + p_f, x = p_f * p_f;
    // We use 64 iterations of the taylor series expansion for the fractional part:
    //      exp(x) = 1 + X + X^2/2 + X^3/6 + X^4/24 + ...
    for (i64 i = 0; i < MATH_PRECISION; i ++) {
        result_fract += x * EXP_CONST[i];
        x *= p_f;
    }
    return result * result_fract;
}

static const f64 LN2 = 0.693147180559945309,
                 INV_LN2 = 1 / LN2;

static const f64 LOG_CONST[64] = {
    -0.5, 0.3333333333333333, -0.25, 0.2, -0.16666666666666666, 0.14285714285714285, -0.125, 0.1111111111111111, 
    -0.1, 0.09090909090909091, -0.08333333333333333, 0.07692307692307693, -0.07142857142857142, 0.06666666666666667, -0.0625, 0.058823529411764705, 
    -0.05555555555555555, 0.05263157894736842, -0.05, 0.047619047619047616, -0.045454545454545456, 0.043478260869565216, -0.041666666666666664, 0.04, 
    -0.038461538461538464, 0.037037037037037035, -0.03571428571428571, 0.034482758620689655, -0.03333333333333333, 0.03225806451612903, -0.03125, 0.030303030303030304, 
    -0.029411764705882353, 0.02857142857142857, -0.027777777777777776, 0.02702702702702703, -0.02631578947368421, 0.02564102564102564, -0.025, 0.024390243902439025, 
    -0.023809523809523808, 0.023255813953488372, -0.022727272727272728, 0.022222222222222223, -0.021739130434782608, 0.02127659574468085, -0.020833333333333332, 0.02040816326530612, 
    -0.02, 0.0196078431372549, -0.019230769230769232, 0.018867924528301886, -0.018518518518518517, 0.01818181818181818, -0.017857142857142856, 0.017543859649122806, 
    -0.017241379310344827, 0.01694915254237288, -0.016666666666666666, 0.01639344262295082, -0.016129032258064516, 0.015873015873015872, -0.015625, 0.015384615384615385
};

inline f64 flog(f64 p) {
    if (p < 0) return 0; // Log not defined for negative numbers.
    union { i64 i; f64 f; } cvt;
    cvt.f = p;
    i64 exp = (cvt.i >> 52 & 2047) - 1023;   // Extract 11-bit exponent.
    cvt.i &= 4503599627370495;               // Clear exponent and sign bits.
    cvt.i |= 1023l << 52;                    // Set exponent to 0

    // Compute ln(x) for remaining fractional part using 64 iterations of taylor series.
    //      ln(x) = (x - 1) - (x - 1)^2 / 2 + (x - 1)^3 / 3 - ...
    f64 fdec = cvt.f - 1, x = fdec * fdec, result_fract = fdec;
    for (i64 i = 0; i < MATH_PRECISION; i ++) {
        result_fract += x * LOG_CONST[i];
        x *= fdec;
    }
    return LN2 * exp + result_fract;
}

inline f64 froot(f64 n, f64 r) {
    return fexp(r * flog(n));
}

inline f64 fpow(f64 n, f64 p) {
    i64 p_i = i64(p);
    f64 p_f = p - p_i;

    if (p_f == 0) return fipow(n, p_i);
    else if (p_i == 0) return froot(n, p_f);
    else return fipow(n, p_i) * froot(n, p_f);
}

extern "C" i8 i8$$pow(i8 n, i8 p) {
    return i8(ipow(n, p));
}

extern "C" i16 i16$$pow(i16 n, i16 p) {
    return i16(ipow(n, p));
}

extern "C" i32 i32$$pow(i32 n, i32 p) {
    return i32(ipow(n, p));
}

extern "C" i64 i64$$pow(i64 n, i64 p) {
    return i64(ipow(n, p));
}

extern "C" f32 f32$$pow(f32 n, f32 p) {
    return f32(fpow(n, p));
}

extern "C" f64 f64$$pow(f64 n, f64 p) {
    return f64(fpow(n, p));
}

extern "C" void int$print(iword i) {
    print(i, '\n');
}

extern "C" void float$print(double i) {
    print(i, '\n');
}

extern "C" void unit$print(unit i) {
    print("()\n");
}

extern "C" void bool$print(bool_t i) {
    print(i ? "true\n" : "false\n");
}

extern "C" void char$print(i32 i) {
    print(rune(i), '\n');
}

extern "C" void string$print(string s) {
    print(const_slice<i8>{s.data, s.size}, '\n');
}

extern "C" iptr $strlen(string s) {
    return utf8_length(s.data, s.size);
}

static allocator* alloc;

extern "C" void $core_init() {
    alloc = new((allocator*)mreq(1).ptr) allocator;
}

extern "C" void $core_deinit() {
    mfree({(page*)alloc, 1});
}

extern "C" iword $malloc(iword size) {
    return alloc->alloc(size);
}

extern "C" void $del(void* ptr) {
    return alloc->free(ptr);
}

// Libcore bindings.

extern CLINKAGE slice<page> memory$map(i64 n) {
    return mreq(n);
}

extern CLINKAGE void memory$unmap(slice<page> pages) {
    mfree(pages);
}

extern CLINKAGE void memory$tag(slice<page> pages, i8 flags) {
    mpermit(pages, flags);
}