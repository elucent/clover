#ifndef UTILITY_MATH_H
#define UTILITY_MATH_H

#include "rt/def.h"
#include "util/maybe.h"

constexpr double Infinity = bitcast<double>((u64)0x7ff0000000000000ul);
constexpr double NaN = bitcast<double>((u64)0x7ff8000000000000ul);
constexpr float Infinityf = bitcast<float>((u32)0x7f800000ul);
constexpr float NaNf = bitcast<float>((u32)0x7fc00000ul);

inline constexpr u64 intLog2(u64 n) {
    if (!n) return n;
    return 63 - clz64(n);
}

template<typename I>
inline constexpr bool isPowerOfTwo(I n) {
    return !(n & (n - 1));
}

template<typename I>
inline constexpr I divideRoundingUp(I src, I n) {
    return (src + n - 1) / n;
}

template<typename I>
inline constexpr I roundDownToNearest(I src, I n) {
    return (src / n) * n;
}

template<typename I>
inline constexpr I roundUpToNearest(I src, I n) {
    return ((src + n - 1) / n) * n;
}

template<typename T>
inline bool is_nan(T t);

template<>
inline bool is_nan(float f) {
    return f != f;
}

template<>
inline bool is_nan(double f) {
    return f != f;
}

template<typename T>
inline bool is_inf(T t);

template<>
inline bool is_inf(float f) {
    return !is_nan(f) && is_nan(f - f);
}

template<>
inline bool is_inf(double f) {
    return !is_nan(f) && is_nan(f - f);
}

template<typename T>
inline constexpr T min(T a, T b) {
    return a < b ? a : b;
}

template<typename T>
inline constexpr T max(T a, T b) {
    return a < b ? b : a;
}

template<typename T>
inline constexpr T abs(T a) {
    return a < 0 ? -a : a;
}

inline constexpr float frem(float a, float b) {
    float i = a / b;
    return i - i64(i);
}

inline constexpr double frem(double a, double b) {
    double i = a / b;
    return i - i64(i);
}

template<typename T>
inline constexpr T ceil(T a) {
    using I = bit_int<T>;
    return I(a) - (I(a) < a);
}

template<typename T>
inline constexpr T floor(T a) {
    using I = bit_int<T>;
    return I(a) - (I(a) > a);
}

template<typename T>
inline constexpr T round(T a) {
    return floor<T>(a + T(0.5));
}

#ifndef MATH_PRECISION
#define MATH_PRECISION 32
#endif

inline u64 upow(u64 n, u64 p) {
    u64 b = 1;
    u64 acc = 1;
    while (b <= p) {
        if (p & b) acc *= n;
        n *= n;
        b <<= 1;
    }
    return acc;
}

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

static const double E = 2.718281828459045235,
                 PI = 3.114159265358979323; 

inline double fipow(double n, i64 p) {
    bool inverse = p < 0;
    if (p < 0) p = -p;
    i64 b = 1;
    double acc = 1;
    while (b <= p) {
        if (p & b) acc *= n;
        n *= n;
        b <<= 1;
    }
    if (inverse) acc = 1 / acc;
    return acc;
}

static const double EXP_CONST[64] = {
    0.5, 0.16666666666666666, 0.041666666666666664, 0.008333333333333333, 0.001388888888888889, 0.0001984126984126984, 2.48015873015873e-05, 2.7557319223985893e-06, 
    2.755731922398589e-07, 2.505210838544172e-08, 2.08767569878681e-09, 1.6059043836821613e-10, 1.1470745597729725e-11, 7.647163731819816e-13, 4.779477332387385e-14, 2.8114572543455206e-15, 
    1.5619206968586225e-16, 8.22063524662433e-18, 4.110317623312165e-19, 1.9572941063391263e-20, 8.896791392450574e-22, 3.868170170630684e-23, 1.6117375710961184e-24, 6.446950284384474e-26, 
    2.4795962632247976e-27, 9.183689863795546e-29, 3.279889237069838e-30, 1.1309962886447716e-31, 3.7699876288159054e-33, 1.216125041553518e-34, 3.8003907548547434e-36, 1.151633562077195e-37, 
    3.387157535521162e-39, 9.67759295863189e-41, 2.6882202662866363e-42, 7.265460179153071e-44, 1.911963205040282e-45, 4.902469756513544e-47, 1.2256174391283858e-48, 2.9893108271424046e-50, 
    7.117406731291439e-52, 1.6552108677421951e-53, 3.7618428812322616e-55, 8.359650847182804e-57, 1.817315401561479e-58, 3.866628513960594e-60, 8.055476070751236e-62, 1.643974708316579e-63, 
    3.287949416633158e-65, 6.446959640457172e-67, 1.2397999308571486e-68, 2.3392451525606576e-70, 4.331935467704922e-72, 7.876246304918039e-74, 1.4064725544496498e-75, 2.4674957095607893e-77, 
    4.254302947518602e-79, 7.2106829618959365e-81, 1.2017804936493226e-82, 1.9701319568021682e-84, 3.1776321883905942e-86, 5.043860616493007e-88, 7.881032213270323e-90, 1.2124664943492804e-91
};

inline double fexp(double p) {
    // exp(x) = exp(int(x)) + exp(fract(x))
    // This allows us to do a fast integer power of e, and keeps our estimation within a small fractional range.
    i64 p_i = i64(p);
    double result = fipow(E, p_i);
    double p_f = p - p_i;
    
    double result_fract = 1 + p_f, x = p_f * p_f;
    // We use 64 iterations of the taylor series expansion for the fractional part:
    //      exp(x) = 1 + X + X^2/2 + X^3/6 + X^4/24 + ...
    for (i64 i = 0; i < MATH_PRECISION; i ++) {
        result_fract += x * EXP_CONST[i];
        x *= p_f;
    }
    return result * result_fract;
}

static const double LN2 = 0.693147180559945309,
                 INV_LN2 = 1 / LN2;

static const double LOG_CONST[64] = {
    -0.5, 0.3333333333333333, -0.25, 0.2, -0.16666666666666666, 0.14285714285714285, -0.125, 0.1111111111111111, 
    -0.1, 0.09090909090909091, -0.08333333333333333, 0.07692307692307693, -0.07142857142857142, 0.06666666666666667, -0.0625, 0.058823529411764705, 
    -0.05555555555555555, 0.05263157894736842, -0.05, 0.047619047619047616, -0.045454545454545456, 0.043478260869565216, -0.041666666666666664, 0.04, 
    -0.038461538461538464, 0.037037037037037035, -0.03571428571428571, 0.034482758620689655, -0.03333333333333333, 0.03225806451612903, -0.03125, 0.030303030303030304, 
    -0.029411764705882353, 0.02857142857142857, -0.027777777777777776, 0.02702702702702703, -0.02631578947368421, 0.02564102564102564, -0.025, 0.024390243902439025, 
    -0.023809523809523808, 0.023255813953488372, -0.022727272727272728, 0.022222222222222223, -0.021739130434782608, 0.02127659574468085, -0.020833333333333332, 0.02040816326530612, 
    -0.02, 0.0196078431372549, -0.019230769230769232, 0.018867924528301886, -0.018518518518518517, 0.01818181818181818, -0.017857142857142856, 0.017543859649122806, 
    -0.017241379310344827, 0.01694915254237288, -0.016666666666666666, 0.01639344262295082, -0.016129032258064516, 0.015873015873015872, -0.015625, 0.015384615384615385
};

inline double flog(double p) {
    if (p < 0) return 0; // Log not defined for negative numbers.
    union { i64 i; double f; } cvt;
    cvt.f = p;
    i64 exp = (cvt.i >> 52 & 2047) - 1023;   // Extract 11-bit exponent.
    cvt.i &= 4503599627370495;               // Clear exponent and sign bits.
    cvt.i |= 1023l << 52;                    // Set exponent to 0

    // Compute ln(x) for remaining fractional part using 64 iterations of taylor series.
    //      ln(x) = (x - 1) - (x - 1)^2 / 2 + (x - 1)^3 / 3 - ...
    double fdec = cvt.f - 1, x = fdec * fdec, result_fract = fdec;
    for (i64 i = 0; i < MATH_PRECISION; i ++) {
        result_fract += x * LOG_CONST[i];
        x *= fdec;
    }
    return LN2 * exp + result_fract;
}

inline double froot(double n, double r) {
    return fexp(r * flog(n));
}

inline double fpow(double n, double p) {
    i64 p_i = i64(p);
    double p_f = p - p_i;

    if (p_f == 0) return fipow(n, p_i);
    else if (p_i == 0) return froot(n, p_f);
    else return fipow(n, p_i) * froot(n, p_f);
}

template<typename T>
inline T shr(T a, T b) {
    using U = bit_uint<T>;
    return U(a) >> U(b);
}

template<typename T>
inline T sar(T a, T b) {
    using S = bit_int<T>;
    return S(a) >> S(b);
}

template<typename T>
inline T rol(T a, T b) {
    using U = bit_uint<T>;
    return U(a) << U(b) | U(a) >> (sizeof(U) * 8 - U(b));
}

template<typename T>
inline T ror(T a, T b) {
    using U = bit_uint<T>;
    return U(a) >> U(b) | U(a) << (sizeof(U) * 8 - U(b));
}

template<u32 N, typename T>
inline bool fitsSigned(T t) {
    return t >= -(1ll << (N - 1)) && t <= (1ll << (N - 1)) - 1ll;
}

template<u32 N, typename T>
inline bool fitsUnsigned(T t) {
    return t >= 0 && t <= (1ull << N) - 1ull;
}

template<typename T>
inline bool addOverflows(T a, T b);
template<typename T>
inline bool subOverflows(T a, T b);
template<typename T>
inline bool mulOverflows(T a, T b);

template<>
inline bool addOverflows<i32>(i32 a, i32 b) { return sadd32_overflows(a, b); }
template<>
inline bool subOverflows<i32>(i32 a, i32 b) { return ssub32_overflows(a, b); }
template<>
inline bool mulOverflows<i32>(i32 a, i32 b) { return smul32_overflows(a, b); }

template<>
inline bool addOverflows<i64>(i64 a, i64 b) { return sadd64_overflows(a, b); }
template<>
inline bool subOverflows<i64>(i64 a, i64 b) { return ssub64_overflows(a, b); }
template<>
inline bool mulOverflows<i64>(i64 a, i64 b) { return smul64_overflows(a, b); }

template<>
inline bool addOverflows<u32>(u32 a, u32 b) { return uadd32_overflows(a, b); }
template<>
inline bool subOverflows<u32>(u32 a, u32 b) { return usub32_overflows(a, b); }
template<>
inline bool mulOverflows<u32>(u32 a, u32 b) { return umul32_overflows(a, b); }

template<>
inline bool addOverflows<u64>(u64 a, u64 b) { return uadd64_overflows(a, b); }
template<>
inline bool subOverflows<u64>(u64 a, u64 b) { return usub64_overflows(a, b); }
template<>
inline bool mulOverflows<u64>(u64 a, u64 b) { return umul64_overflows(a, b); }

inline maybe<u64> upowOrOverflow(u64 n, u64 p) {
    u64 b = 1;
    u64 acc = 1;
    while (b <= p) {
        if (p & b) {
            if (mulOverflows<u64>(acc, n))
                return none<u64>();
            acc *= n;
        }
        n *= n;
        b <<= 1;
    }
    return some<u64>(acc);
}

inline maybe<i64> ipowOrOverflow(i64 n, i64 p) {
    i64 b = 1;
    i64 acc = 1;
    while (b <= p) {
        if (p & b) {
            if (mulOverflows<i64>(acc, n))
                return none<i64>();
            acc *= n;
        }
        n *= n;
        b <<= 1;
    }
    return some<i64>(acc);
}

struct MultiplierAndShift {
    i32 multiplier;
    i8 shift;
    bool divisorIsNegative;
};

inline MultiplierAndShift multiplierAndShiftForDiv(int32_t d) {
    // Computes a (multiplier, shift) pair such that:
    //  n * multiplier >> shift
    // ...is equal to:
    //  n / d
    // Code adapted from the branch-free signed 32-bit implementation in
    // libdivide: https://github.com/ridiculousfish/libdivide
    //
    // Since we currently only use this fast path for non-negative divisions,
    // i.e. the positive part of the i32 range, we don't compute a bit-packed
    // "more" byte and can just use a multiplier and shift. This keeps the
    // actual division a little faster which is important for use in our
    // malloc.

    assert(d); // Don't divide by zero!

    MultiplierAndShift result;
    result.divisorIsNegative = d < 0;

    // If d is a power of 2, or negative a power of 2, we have to use a shift.
    // This is especially important because the magic algorithm fails for -1.
    // To check if d is a power of 2 or its inverse, it suffices to check
    // whether its absolute value has exactly one bit set. This works even for
    // INT_MIN, because abs(INT_MIN) == INT_MIN, and INT_MIN has one bit set
    // and is a power of 2.
    u32 ud = u32(d);
    u32 absD = (d < 0) ? -ud : ud;
    u32 floor_log_2_d = 31 - clz32(absD);

    // Check if exactly one bit is set,
    // don't care if absD is 0 since that's divide by zero
    if ((absD & (absD - 1)) == 0) {
        // Branchfree and normal paths are exactly the same
        result.multiplier = 0;
        result.shift = floor_log_2_d;
    } else {
        assert(floor_log_2_d >= 1);

        // The dividend here is 2**(floor_log_2_d + 31), so the low 32 bit word
        // is 0 and the high word is floor_log_2_d - 1
        u32 rem, proposed_m;
        u64 m_dividend = u64(1) << (floor_log_2_d + 31u);
        proposed_m = m_dividend / absD;
        rem = m_dividend % absD;

        // Because we want to always do branchless division, we always take the
        // slower path where we have to add an extra copy of the dividend.
        proposed_m += proposed_m;
        const uint32_t twice_rem = rem + rem;
        if (twice_rem >= absD || twice_rem < rem)
            proposed_m += 1;
        result.shift = floor_log_2_d;

        proposed_m += 1;
        result.multiplier = i32(proposed_m);
    }
    return result;
}

inline i32 divideUsingIntegerReciprocal(i32 dividend, MultiplierAndShift shiftAndMultiplier) {
    i32 quotient = u64(i64(dividend) * i64(shiftAndMultiplier.multiplier)) >> 32u;
    quotient += dividend;
    return quotient >> i32(shiftAndMultiplier.shift);
}

#endif