#ifndef RT_DEF_H
#define RT_DEF_H

#include "stdint.h"
#include "stddef.h"

/*
 * Useful Macros
 *
 * Macro definitions for built-in attributes, functions, and more.
 */

#ifdef RT_GCC_COMPATIBLE
#ifdef __wasm__
#define ASMLABEL(name) __attribute__((export_name(name)))
#else
#define ASMLABEL(name) asm(name)
#endif
#define USED __attribute__((used))
#define NOINLINE __attribute__((noinline))
#define ALWAYSINLINE __attribute__((always_inline))
#define LIKELY(...) (__builtin_expect((__VA_ARGS__), 1))
#define UNLIKELY(...) (__builtin_expect((__VA_ARGS__), 0))
#define EXPECT(v, ...) (__builtin_expect((__VA_ARGS__), (v)))
#define CURRENT_FUNCTION __func__
#define fallthrough [[fallthrough]]
#else // Non-GCC-compatible compiler
#define ASMLABEL(name)
#define NOINLINE
#define ALWAYSINLINE inline
#define LIKELY(...) (__VA_ARGS__)
#define UNLIKELY(...) (__VA_ARGS__)
#define EXPECT(v, ...) (__VA_ARGS__)
#define CURRENT_FUNCTION __FUNCTION__
#define fallthrough
#endif

#define STRINGIFY(x) #x
#define TOSTRING(x) STRINGIFY(x)

#if defined(RT_AMD64)
#define RT_48BIT_ADDRESS // Platform should only have 48 significant bits in pointers.
#endif

// Breakable block.
#define do_block(...) do { __VA_ARGS__ } while (false);

// Color codes

#ifndef RT_WINDOWS

// Ubuntu and Mac color codes, from
// https://stackoverflow.com/questions/9158150/colored-output-in-c/
#define RESET   "\033[0m"
#define BOLD    "\033[1m"
#define ITALIC  "\033[3m"
#define BLACK   "\033[30m"      /* Black */
#define GRAY   "\033[1;30m"      /* Black */
#define RED     "\033[31m"      /* Red */
#define GREEN   "\033[32m"      /* Green */
#define YELLOW  "\033[33m"      /* Yellow */
#define BLUE    "\033[34m"      /* Blue */
#define MAGENTA "\033[35m"      /* Magenta */
#define CYAN    "\033[36m"      /* Cyan */
#define WHITE   "\033[37m"      /* White */
#define BOLDBLACK   "\033[1m\033[30m"      /* Bold Black */
#define BOLDGRAY    "\033[1m\033[0;30m"      /* Bold Gray */
#define BOLDRED     "\033[1m\033[31m"      /* Bold Red */
#define BOLDGREEN   "\033[1m\033[32m"      /* Bold Green */
#define BOLDYELLOW  "\033[1m\033[33m"      /* Bold Yellow */
#define BOLDBLUE    "\033[1m\033[34m"      /* Bold Blue */
#define BOLDMAGENTA "\033[1m\033[35m"      /* Bold Magenta */
#define BOLDCYAN    "\033[1m\033[36m"      /* Bold Cyan */
#define BOLDWHITE   "\033[1m\033[37m"      /* Bold White */
#define ITALICBLACK     "\033[3m\033[30m"      /* Italic Black */
#define ITALICRED       "\033[3m\033[31m"      /* Italic Red */
#define ITALICGREEN     "\033[3m\033[32m"      /* Italic Green */
#define ITALICYELLOW    "\033[3m\033[33m"      /* Italic Yellow */
#define ITALICBLUE      "\033[3m\033[34m"      /* Italic Blue */
#define ITALICMAGENT    "\033[3m\033[35m"      /* Italic Magenta */
#define ITALICCYAN      "\033[3m\033[36m"      /* Italic Cyan */
#define ITALICWHITE     "\033[3m\033[37m"      /* Italic White */

#else

#define RESET ""
#define BOLD ""
#define ITALIC ""
#define BLACK ""
#define GRAY ""
#define RED ""
#define GREEN ""
#define YELLOW ""
#define BLUE ""
#define MAGENTA ""
#define CYAN ""
#define WHITE ""
#define BOLDBLACK ""
#define BOLDRED ""
#define BOLDGREEN ""
#define BOLDYELLOW ""
#define BOLDBLUE ""
#define BOLDMAGENTA ""
#define BOLDCYAN ""
#define BOLDWHITE ""
#define ITALICBLACK ""
#define ITALICRED ""
#define ITALICGREEN ""
#define ITALICYELLOW ""
#define ITALICBLUE ""
#define ITALICMAGENT ""
#define ITALICCYAN ""
#define ITALICWHITE ""

#endif

#define PREVENT_COPYING(type) \
public: \
    inline type(const type&) = delete; \
    inline type& operator=(const type&) = delete; \
    using __ ## type ## __forbidCopying = int

#define PREVENT_MOVING(type) \
    inline type(type&&) = delete; \
    inline type& operator=(type&&) = delete; \
    using __ ## type ## __forbidMoving = int

/*
 * Intrinsics
 *
 * Special functions for hardware instructions that aren't standard C.
 */

#define intrinsic(insn, ...) insn##_intrinsic(__VA_ARGS__)

/*
 * Types Section
 *
 * These are the types over which we define the ABI of the Basil runtime.
 */

namespace types {

    // Primitive numeric types
    typedef char i8;
    typedef int16_t i16;
    typedef int32_t i32;
    typedef int64_t i64;

    typedef unsigned char u8;
    typedef uint16_t u16;
    typedef uint32_t u32;
    typedef uint64_t u64;

    typedef intptr_t iptr;
    typedef uintptr_t uptr;

    typedef float f32;
    typedef double f64;

#ifdef RT_64
    typedef i64 iword;
    typedef u64 uword;
#else
    typedef i32 iword;
    typedef u32 uword;
#endif
}

namespace std {
    template<typename T, T K>
    struct integral_constant {
      static constexpr T value = K;

      using value_type = T;
      using type = integral_constant;
      constexpr operator T() const noexcept { return value; }
      constexpr value_type operator()() const noexcept { return value; }
    };

    template<typename T>
    struct tuple_size {};

    template<int I, typename T>
    struct tuple_element {};
}

namespace utilities {
    /*
     * swap(a, b)
     *
     * Swaps the values referenced by the two parameters.
     */
    template<typename T>
    inline void swap(T& a, T& b) {
        T t = a;
        a = b;
        b = t;
    }

    /*
     * move(value)
     *
     * Attempts to move value into a new location.
     */
    template<typename T>
    T&& move(T& t) {
        return static_cast<T&&>(t);
    }
}

namespace types {
    template<typename T>
    struct IsSigned { constexpr static bool value = false; };

    template<> struct IsSigned<i8> { constexpr static bool value = true; };
    template<> struct IsSigned<i16> { constexpr static bool value = true; };
    template<> struct IsSigned<i32> { constexpr static bool value = true; };
    template<> struct IsSigned<i64> { constexpr static bool value = true; };

    template<typename T>
    constexpr bool isSigned = IsSigned<T>::value;

    template<typename A, typename B, bool C>
    struct Choice;

    template<typename A, typename B>
    struct Choice<A, B, true> {
        using Type = A;
    };

    template<typename A, typename B>
    struct Choice<A, B, false> {
        using Type = B;
    };

    template<typename A, typename B, bool C>
    using choice = typename Choice<A, B, C>::Type;

    template<typename T>
    struct RemoveReference { using Type = T; };

    template<typename T>
    struct RemoveReference<T&> { using Type = T; };

    template<typename T>
    struct RemoveReference<const T&> { using Type = T; };

    template<typename T>
    using remove_reference = typename RemoveReference<T>::Type;

    // Pairs
    template<typename A, typename B>
    struct pair {
        A first;
        B second;
    };

    template<typename P, int I>
    struct const_pair_getter {};
    template<typename P, int I>
    struct pair_getter {};

    template<typename P>
    struct const_pair_getter<P, 0> {
        const P& p;
        auto get() const -> const decltype(p.first)& { return p.first; }
    };

    template<typename P>
    struct const_pair_getter<P, 1> {
        const P& p;
        auto get() const -> const decltype(p.second)& { return p.second; }
    };

    template<typename P>
    struct pair_getter<P, 0> {
        P& p;
        auto get() -> decltype(p.first)& { return p.first; }
    };

    template<typename P>
    struct pair_getter<P, 1> {
        P& p;
        auto get() -> decltype(p.second)& { return p.second; }
    };

    template<i32 I, typename A, typename B>
    auto get(const pair<A, B>& p) -> const typename std::tuple_element<I, pair<A, B>>::type& {
        return const_pair_getter<pair<A, B>, I>{p}.get();
    }

    template<i32 I, typename A, typename B>
    auto get(pair<A, B>& p) -> typename std::tuple_element<I, pair<A, B>>::type& {
        return pair_getter<pair<A, B>, I>{p}.get();
    }

    // Triples
    template<typename A, typename B, typename C>
    struct triple {
        A first;
        B second;
        C third;
    };

    template<typename P, int I>
    struct const_triple_getter {};
    template<typename P, int I>
    struct triple_getter {};

    template<typename P>
    struct const_triple_getter<P, 0> {
        const P& p;
        auto get() const -> const decltype(p.first)& { return p.first; }
    };

    template<typename P>
    struct const_triple_getter<P, 1> {
        const P& p;
        auto get() const -> const decltype(p.second)& { return p.second; }
    };

    template<typename P>
    struct const_triple_getter<P, 2> {
        const P& p;
        auto get() const -> const decltype(p.third)& { return p.third; }
    };

    template<typename P>
    struct triple_getter<P, 0> {
        P& p;
        auto get() -> decltype(p.first)& { return p.first; }
    };

    template<typename P>
    struct triple_getter<P, 1> {
        P& p;
        auto get() -> decltype(p.second)& { return p.second; }
    };

    template<typename P>
    struct triple_getter<P, 2> {
        P& p;
        auto get() -> decltype(p.third)& { return p.third; }
    };

    template<i32 I, typename A, typename B, typename C>
    auto get(const triple<A, B, C>& p) -> const typename std::tuple_element<I, triple<A, B, C>>::type& {
        return const_triple_getter<triple<A, B, C>, I>{p}.get();
    }

    template<i32 I, typename A, typename B, typename C>
    auto get(triple<A, B, C>& p) -> typename std::tuple_element<I, triple<A, B, C>>::type& {
        return triple_getter<triple<A, B, C>, I>{p}.get();
    }
}

namespace std {
    template<typename A, typename B>
    struct tuple_size<const types::pair<A, B>> : public integral_constant<types::iword, 2> {};
    template<typename A, typename B>
    struct tuple_size<types::pair<A, B>> : public integral_constant<types::iword, 2> {};

    template<typename K, typename V>
    struct tuple_element<0, const types::pair<K, V>> {
        using type = const K;
    };

    template<typename K, typename V>
    struct tuple_element<1, const types::pair<K, V>> {
        using type = const V;
    };

    template<typename K, typename V>
    struct tuple_element<0, types::pair<K, V>> {
        using type = K;
    };

    template<typename K, typename V>
    struct tuple_element<1, types::pair<K, V>> {
        using type = V;
    };

    template<typename A, typename B, typename C>
    struct tuple_size<const types::triple<A, B, C>> : public integral_constant<types::iword, 3> {};
    template<typename A, typename B, typename C>
    struct tuple_size<types::triple<A, B, C>> : public integral_constant<types::iword, 3> {};

    template<typename A, typename B, typename C>
    struct tuple_element<0, const types::triple<A, B, C>> {
        using type = const A;
    };

    template<typename A, typename B, typename C>
    struct tuple_element<1, const types::triple<A, B, C>> {
        using type = const B;
    };

    template<typename A, typename B, typename C>
    struct tuple_element<2, const types::triple<A, B, C>> {
        using type = const C;
    };

    template<typename A, typename B, typename C>
    struct tuple_element<0, types::triple<A, B, C>> {
        using type = A;
    };

    template<typename A, typename B, typename C>
    struct tuple_element<1, types::triple<A, B, C>> {
        using type = B;
    };

    template<typename A, typename B, typename C>
    struct tuple_element<2, types::triple<A, B, C>> {
        using type = C;
    };
}

namespace utilities {
    template<typename T>
    inline void swap(T& a, T& b);

    inline void out_of_line_assert(bool);
}

namespace types {
    using utilities::out_of_line_assert;

    // Slices
    template<typename T>
    struct slice;

    template<typename T>
    struct const_slice {
        const T* items;
        iword length;

        inline constexpr const_slice():
            items(nullptr), length(0) {}

        inline constexpr const_slice(const T* items_in, iword length_in):
            items(items_in), length(length_in) {}

        inline const T& operator[](iword i) const {
            out_of_line_assert(i >= 0 && i < length);
            return items[i];
        }

        inline const_slice<T> operator[](pair<iword, iword> span) const {
            iword start = span.first;
            iword end = span.second;
            out_of_line_assert(end >= start);
            out_of_line_assert(start >= 0 && start <= length);
            out_of_line_assert(end >= 0 && end <= length);
            return const_slice<T>{ items + start, end - start };
        }

        constexpr const T* data() const {
            return items;
        }

        constexpr iword size() const {
            return length;
        }

        inline constexpr const T& first() const {
            return operator[](0);
        }

        inline constexpr const T& last() const {
            return operator[](size() - 1);
        }

        inline constexpr const T* begin() const {
            return items;
        }

        inline constexpr const T* end() const {
            return items + length;
        }

        const_slice<T> drop(iptr i) const {
            return { items + i, length - i };
        }

        const_slice<T> take(iptr i) const {
            return { items, i };
        }

        template<typename U>
        inline const_slice<U> as_slice() const {
            return { (const U*)items, iword(length * sizeof(T) / sizeof(U)) };
        }

        inline slice<T> dup() const;
    };

    template<typename T>
    struct slice {
        T* items;
        iword length;

        inline constexpr slice():
            items(nullptr), length(0) {}

        inline constexpr slice(T* items_in, iword length_in):
            items(items_in), length(length_in) {}

        inline const T& operator[](iword i) const {
            out_of_line_assert(i >= 0 && i < length);
            return items[i];
        }

        inline T& operator[](iword i) {
            out_of_line_assert(i >= 0 && i < length);
            return items[i];
        }

        inline const_slice<T> operator[](pair<iword, iword> span) const {
            iword start = span.first;
            iword end = span.second;
            out_of_line_assert(end >= start);
            out_of_line_assert(start >= 0 && start <= length);
            out_of_line_assert(end >= 0 && end <= length);
            return const_slice<T>{ items + start, end - start };
        }

        inline slice<T> operator[](pair<iword, iword> span) {
            iword start = span.first;
            iword end = span.second;
            out_of_line_assert(end >= start);
            out_of_line_assert(start >= 0 && start <= length);
            out_of_line_assert(end >= 0 && end <= length);
            return slice<T>{ items + start, end - start };
        }

        constexpr const T* data() const {
            return items;
        }

        constexpr T* data() {
            return items;
        }

        constexpr iword size() const {
            return length;
        }

        inline constexpr const T& first() const {
            return operator[](0);
        }

        inline constexpr T& first() {
            return operator[](0);
        }

        inline constexpr const T& last() const {
            return operator[](size() - 1);
        }

        inline constexpr T& last() {
            return operator[](size() - 1);
        }

        inline constexpr const T* begin() const {
            return items;
        }

        inline constexpr const T* end() const {
            return items + length;
        }

        inline constexpr T* begin() {
            return items;
        }

        inline constexpr T* end() {
            return items + length;
        }

        inline operator const_slice<T>() const {
            return { items, length };
        }

        slice<T> drop(iptr i) const {
            return { items + i, length - i };
        }

        slice<T> take(iptr i) const {
            return { items, i };
        }

        template<typename U>
        inline slice<U> as_slice() const {
            return { (U*)items, iword(length * sizeof(T) / sizeof(U)) };
        }

        inline slice<T> dup() const;
    };

    // Fixed-size arrays
    template<typename T, iword N>
    struct array {
        T items[N];

        template<typename... Args>
        array(const Args&... args): items{ args... } {}

        inline constexpr const T& operator[](iword i) const {
            if (i < 0) return items[N + i];
            else return items[i];
        }

        inline constexpr T& operator[](iword i) {
            if (i < 0) return items[N + i];
            else return items[i];
        }

        inline constexpr const_slice<T> operator[](pair<iword, iword> span) const {
            iword start = span.first >= 0 ? span.first : N + span.first;
            iword end = span.second >= 0 ? span.second : N + span.second;
            if (end < start)
                utilities::swap(start, end);
            return const_slice<T>{ items + start, end - start };
        }

        inline constexpr slice<T> operator[](pair<iword, iword> span) {
            iword start = span.first >= 0 ? span.first : N + span.first;
            iword end = span.second >= 0 ? span.second : N + span.second;
            if (end < start)
                utilities::swap(start, end);
            return slice<T>{ items + start, end - start };
        }

        inline constexpr operator const_slice<T>() const {
            return const_slice<T>{ items, N };
        }

        inline constexpr operator slice<T>() {
            return slice<T>{ items, N };
        }

        constexpr iword size() const {
            return N;
        }

        inline constexpr const T& first() const {
            return operator[](0);
        }

        inline constexpr T& first() {
            return operator[](0);
        }

        inline constexpr const T& last() const {
            return operator[](-1);
        }

        inline constexpr T& last() {
            return operator[](-1);
        }

        inline constexpr const T* begin() const {
            return items;
        }

        inline constexpr const T* end() const {
            return items + N;
        }

        inline constexpr T* begin() {
            return items;
        }

        inline constexpr T* end() {
            return items + N;
        }
    };

    template<typename T, iword N>
    inline constexpr bool operator!=(const array<T, N>& a, const array<T, N>& b) {
        for (iword i = 0; i < N; i ++)
            if (a[i] != b[i])
                return true;
        return false;
    }

    template<typename T, iword N>
    inline constexpr bool operator==(const array<T, N>& a, const array<T, N>& b) {
        for (iword i = 0; i < N; i ++)
            if (a[i] != b[i])
                return false;
        return true;
    }

    template<class T> struct valtypeStruct { using type = T; };
    template<class T> struct valtypeStruct<T&> { using type = T; };
    template<class T> struct valtypeStruct<T&&> { using type = T; };

    template<class T> using valtype = typename valtypeStruct<T>::type;

    template<int N>
    struct BitInt;

    template<>
    struct BitInt<1> {
        using type = i8;
        using unsigned_type = u8;
    };
    template<>
    struct BitInt<2> {
        using type = i16;
        using unsigned_type = u16;
    };
    template<>
    struct BitInt<4> {
        using type = i32;
        using unsigned_type = u32;
    };
    template<>
    struct BitInt<8> {
        using type = i64;
        using unsigned_type = u64;
    };

    template<typename T>
    using bit_int = typename BitInt<sizeof(T)>::type;

    template<typename T>
    using bit_uint = typename BitInt<sizeof(T)>::unsigned_type;

    template<typename T>
    T&& forward(valtype<T>& ref) {
        return static_cast<T&&>(ref);
    }

    template<typename T>
    T&& forward(valtype<T>&& ref) {
        return static_cast<T&&>(ref);
    }

    template<typename T>
    auto declval() -> T&& { }

    template<typename T, typename... Args>
    struct is_constructible_impl { constexpr static bool value = false; };

    template<typename T, typename... Args>
    struct is_constructible_impl<decltype((new T(declval<Args>()...), void())), T, Args...> { constexpr static bool value = true; };

    template<typename T, typename... Args>
    constexpr bool is_constructible() { return is_constructible_impl<T, Args...>::value; }
}

using namespace types;
using namespace std;

/*
 * Constants
 *
 * Useful constants and enumerations.
 */
namespace constants {
    enum class StrictOrder : i8 {
        Below,
        LessThan = Below,
        Equal,
        Above,
        GreaterThan = Above
    };

    enum class TopologicalMark : i8 {
        Unmarked,
        TemporaryMark,
        Marked
    };

    enum class Associativity : i8 {
        Left,
        Right
    };
}

using namespace constants;

/*
 * Utilities
 *
 * Core critical functions used in the Basil runtime.
 */

namespace memory {
    /*
     * copy(dst, src, size)
     *
     * Copies size bytes from the block pointed to by src to the block pointed
     * to by dst. Assumes both pointers are pointing to a valid space of at
     * least size bytes.
     */
    inline constexpr void* copy(void* dst, const void* src, iword size) {
#ifdef RT_GCC_COMPATIBLE
        return __builtin_memcpy(dst, src, size);
#else
        const i8* src8 = (const i8*)src;
        i8* dst8 = (i8*)dst;
        for (iword i = 0; i < size; i ++)
            dst8[i] = src8[i];
        return dst8 + size;
#endif
    }

    /*
     * move(dst, src, size)
     *
     * Copies size bytes from the block pointed to by src to the block pointed
     * to by dst. Assumes both pointers are pointing to a valid space of at
     * least size bytes.
     */
    inline constexpr void* move(void* dst, const void* src, iword size) {
#ifdef RT_GCC_COMPATIBLE
        return __builtin_memmove(dst, src, size);
#else
        const i8* src8 = (const i8*)src;
        i8* dst8 = (i8*)dst;
        if (src8 < dst8)
            for (iword i = size - 1; i >= 0; i --)
                dst8[i] = src8[i];
        else for (iword i = 0; i < size; i ++)
            dst8[i] = src8[i];
        return dst8 + size;
#endif
    }

    /*
     * fill(dst, val, size)
     *
     * Writes the byte val to each of size bytes pointed to by dst. Assumes dst is
     * pointing to a valid space of at least size bytes.
     */
    inline void* fill(void* dst, u8 val, iword size) {
#ifdef RT_GCC_COMPATIBLE
        return __builtin_memset(dst, val, size);
#else
        i8* dst8 = (i8*)dst;
        for (iword i = 0; i < size; i ++)
            dst8[i] = val;
        return dst8 + size;
#endif
    }

    /*
     * compare(a, b, size)
     *
     * Performs a bytewise lexicographic comparison between the memory regions
     * pointed to by a and b of length size. Assumes a and b point to valid spaces
     * of at least size bytes. Returns a positive number if a is greater than b,
     * negative if a is less than b, or zero if they are equal.
     */
    inline constexpr iword compare(const void* a, const void* b, iword size) {
#ifdef RT_GCC_COMPATIBLE
        return __builtin_memcmp(a, b, size);
#else
        const i8* src8 = (const i8*)a;
        const i8* dst8 = (const i8*)b;
        for (iword i = 0; i < size; i ++)
            if (src8[i] != dst8[i])
                return dst8[i] - src8[i];
        return 0;
#endif
    }
}

namespace utilities {
    /*
     * bitcast<T>(a)
     *
     * Reinterprets the bits of a as type T.
     */
    template<typename T, typename U>
    inline constexpr T bitcast(U u) {
#ifdef RT_GCC_COMPATIBLE
        return __builtin_bit_cast(T, u);
#else
        static_assert(sizeof(T) == sizeof(U));
        types::i8 bytes[sizeof(T)];
        memory::copy(bytes, &u, sizeof(U));
        return reinterpret_cast<T*>(bytes);
#endif
    }

    /*
     * load<T>(p)
     *
     * Loads a value of type T from pointer p, regardless of alignment or
     * static type.
     */
    template<typename T>
    inline T load(const void* p) {
        types::array<types::u8, sizeof(T)> bytes;
        memory::copy(&bytes, p, sizeof(T));
        return bitcast<T>(bytes);
    }

    /*
     * store(a, p)
     *
     * Stores a at the memory pointed to by p, regardless of alignment or
     * static type.
     */
    template<typename T>
    inline void store(const T& t, void* p) {
        memory::copy(p, &t, sizeof(T));
    }

    /*
     * findc(string, char)
     *
     * Returns the index of the provided character in the string, or -1 if not present.
     */
    inline constexpr iword findc(const i8* string, i8 c) {
        for (iword i = 0; string[i] != '\0' || c == '\0'; i ++) {
            if (string[i] == c)
                return i;
        }
        return -1;
    }

    inline const_slice<i8> cstring(const i8* str) {
        return const_slice<i8> { str, findc(str, 0) };
    }

    template<typename T>
    inline slice<T> dup(const_slice<T> s) {
        T* data = new T[s.size()];
        for (u32 i = 0; i < s.size(); i ++)
            data[i] = s[i];
        return { data, s.size() };
    }
}

namespace types {
    template<typename T, typename U>
    bool bits_equal(T t, U u) {
        static_assert(sizeof(T) == sizeof(U));
        return utilities::bitcast<bit_int<T>>(t) == utilities::bitcast<bit_int<U>>(u);
    }

    template<typename T>
    slice<T> const_slice<T>::dup() const {
        return utilities::dup(*this);
    }

    template<typename T>
    slice<T> slice<T>::dup() const {
        return utilities::dup(*this);
    }

    // Entries
    template<typename K, typename V>
    struct entry {
        K key;
        V value;

        inline bool operator==(const entry& other) const {
            return key == other.key;
        }
    };

    inline bool operator==(const_slice<i8> a, const_slice<i8> b) {
        return a.size() == b.size() && !memory::compare(a.data(), b.data(), a.size());
    }

    template<typename V>
    struct entry<const i8*, V> {
        const i8* key;
        V value;

        inline bool operator==(const entry& other) const {
            return utilities::findc(key, 0) == utilities::findc(other.key, 0) && !memory::compare(key, other.key, utilities::findc(key, 0));
        }
    };

    template<typename P, int I>
    struct const_entry_getter {};
    template<typename P, int I>
    struct entry_getter {};

    template<typename P>
    struct const_entry_getter<P, 0> {
        const P& p;
        auto get() const -> const decltype(p.key)& { return p.key; }
    };

    template<typename P>
    struct const_entry_getter<P, 1> {
        const P& p;
        auto get() const -> const decltype(p.value)& { return p.value; }
    };

    template<typename P>
    struct entry_getter<P, 0> {
        P& p;
        auto get() -> decltype(p.key)& { return p.key; }
    };

    template<typename P>
    struct entry_getter<P, 1> {
        P& p;
        auto get() -> decltype(p.value)& { return p.value; }
    };

    template<i32 I, typename A, typename B>
    auto get(const entry<A, B>& p) -> const typename std::tuple_element<I, entry<A, B>>::type& {
        return const_entry_getter<entry<A, B>, I>{p}.get();
    }

    template<i32 I, typename A, typename B>
    auto get(entry<A, B>& p) -> typename std::tuple_element<I, entry<A, B>>::type& {
        return entry_getter<entry<A, B>, I>{p}.get();
    }
}

namespace std {
    template<typename K, typename V>
    struct tuple_size<const types::entry<K, V>> : public integral_constant<types::iword, 2> {};
    template<typename K, typename V>
    struct tuple_size<types::entry<K, V>> : public integral_constant<types::iword, 2> {};

    template<typename K, typename V>
    struct tuple_element<0, const types::entry<K, V>> {
        using type = const K;
    };

    template<typename K, typename V>
    struct tuple_element<1, const types::entry<K, V>> {
        using type = const V;
    };

    template<typename K, typename V>
    struct tuple_element<0, types::entry<K, V>> {
        using type = K;
    };

    template<typename K, typename V>
    struct tuple_element<1, types::entry<K, V>> {
        using type = V;
    };
}

namespace utilities {
    /*
     * crash()
     *
     * Immediately halts the program with a nonzero exit code.
     */
    inline void crash() {
#ifdef RT_GCC_COMPATIBLE
        __builtin_trap();
#else
        volatile i32* ptr = nullptr;
        *ptr = 0xdead1eaf;
#endif
    }

    /*
     * panic(msg)
     *
     * Writes diagnostic information and a message to stderr then exits with a nonzero
     * code.
     */
    extern void printString(const_slice<i8> msg);
    extern void printUint(iword msg);

    template<typename... Args>
    inline void panicPrint(const Args&... args);

    inline void panicPrint(const i8* msg) {
        printString({ msg, findc(msg, '\0') });
    }

    template<typename... Args>
    inline void panicImpl(const i8* file, iword line, const Args&... args) {
        printString({ "[PANIC] ", 8 });
        printString({ file, findc(file, '\0') });
        printString({ ":", 1 });
        printUint(line);
        printString({ " ", 1 });
        panicPrint(args...);
        printString({ "\n", 1 });
        crash();
    }

    #define panic(...) panicImpl(__FILE__, __LINE__, __VA_ARGS__)

#if defined(RT_GCC_COMPATIBLE) && defined(RT_AMD64)
    #define RT_FAST_BITCOUNT
    inline u32 clz32(u32 x) {
        u32 result;
        asm (
            "lzcnt %1, %0\n\t"
            : "=r" (result)
            : "r" (x)
        );
        return result;
    }

    inline u64 clz64(u64 x) {
        u64 result;
        asm (
            "lzcnt %1, %0\n\t"
            : "=r" (result)
            : "r" (x)
        );
        return result;
    }

    inline u32 ctz32(u32 x) {
        u32 result;
        asm (
            "rep bsf %1, %0\n\t"
            : "=r" (result)
            : "r" (x)
        );
        return result;
    }

    inline u64 ctz64(u64 x) {
        u64 result;
        asm (
            "rep bsfq %1, %0\n\t"
            : "=r" (result)
            : "r" (x)
        );
        return result;
    }

    inline u32 popcount32(u32 x) {
        u32 result;
        asm (
            "popcntl %1, %0\n\t"
            : "=r" (result)
            : "r" (x)
        );
        return result;
    }

    inline u64 popcount64(u64 x) {
        u64 result;
        asm (
            "popcntq %1, %0\n\t"
            : "=r" (result)
            : "r" (x)
        );
        return result;
    }

    inline bool sadd32_overflows(i32 a, i32 b) {
        i8 result;
        i32 temp;
        asm (
            "movl %2, %1\n\t"
            "addl %3, %1\n\t"
            "seto %0\n\t"
            : "=r" (result), "=r" (temp)
            : "r" (a), "r" (b)
        );
        return result;
    }

    inline bool ssub32_overflows(i32 a, i32 b) {
        i8 result;
        i32 temp;
        asm (
            "movl %2, %1\n\t"
            "subl %3, %1\n\t"
            "seto %0\n\t"
            : "=r" (result), "=r" (temp)
            : "r" (a), "r" (b)
        );
        return result;
    }

    inline bool smul32_overflows(i32 a, i32 b) {
        i8 result;
        i32 temp;
        asm (
            "movl %2, %1\n\t"
            "imull %3, %1\n\t"
            "seto %0\n\t"
            : "=r" (result), "=r" (temp)
            : "r" (a), "r" (b)
        );
        return result;
    }

    inline bool uadd32_overflows(u32 a, u32 b) {
        i8 result;
        u32 temp;
        asm (
            "movl %2, %1\n\t"
            "addl %3, %1\n\t"
            "seto %0\n\t"
            : "=r" (result), "=r" (temp)
            : "r" (a), "r" (b)
        );
        return result;
    }

    inline bool usub32_overflows(u32 a, u32 b) {
        i8 result;
        u32 temp;
        asm (
            "movl %2, %1\n\t"
            "subl %3, %1\n\t"
            "seto %0\n\t"
            : "=r" (result), "=r" (temp)
            : "r" (a), "r" (b)
        );
        return result;
    }

    inline bool umul32_overflows(u32 a, u32 b) {
        i8 result;
        u32 temp;
        asm (
            "movl %2, %1\n\t"
            "imullW %3, %1\n\t"
            "seto %0\n\t"
            : "=r" (result), "=r" (temp)
            : "r" (a), "r" (b)
        );
        return result;
    }

    inline bool sadd64_overflows(i64 a, i64 b) {
        i8 result;
        i64 temp;
        asm (
            "movq %2, %1\n\t"
            "addq %3, %1\n\t"
            "seto %0\n\t"
            : "=r" (result), "=r" (temp)
            : "r" (a), "r" (b)
        );
        return result;
    }

    inline bool ssub64_overflows(i64 a, i64 b) {
        i8 result;
        i64 temp;
        asm (
            "movq %2, %1\n\t"
            "subq %3, %1\n\t"
            "seto %0\n\t"
            : "=r" (result), "=r" (temp)
            : "r" (a), "r" (b)
        );
        return result;
    }

    inline bool smul64_overflows(i64 a, i64 b) {
        i8 result;
        i64 temp;
        asm (
            "movq %2, %1\n\t"
            "imulq %3, %1\n\t"
            "seto %0\n\t"
            : "=r" (result), "=r" (temp)
            : "r" (a), "r" (b)
        );
        return result;
    }

    inline bool uadd64_overflows(u64 a, u64 b) {
        i8 result;
        u64 temp;
        asm (
            "movq %2, %1\n\t"
            "addq %3, %1\n\t"
            "seto %0\n\t"
            : "=r" (result), "=r" (temp)
            : "r" (a), "r" (b)
        );
        return result;
    }

    inline bool usub64_overflows(u64 a, u64 b) {
        i8 result;
        u64 temp;
        asm (
            "movq %2, %1\n\t"
            "subq %3, %1\n\t"
            "seto %0\n\t"
            : "=r" (result), "=r" (temp)
            : "r" (a), "r" (b)
        );
        return result;
    }

    inline bool umul64_overflows(u64 a, u64 b) {
        i8 result;
        u64 temp;
        asm (
            "movq %2, %1\n\t"
            "imulq %3, %1\n\t"
            "seto %0\n\t"
            : "=r" (result), "=r" (temp)
            : "r" (a), "r" (b)
        );
        return result;
    }

    inline u8 bswap8(u8 x) {
        return x;
    }

    inline u16 bswap16(u16 x) {
        u16 low = u8(x);
        return x >> 8 | low << 8;
    }

    inline u32 bswap32(u32 x) {
        asm (
            "bswapl %0"
            : "=r" (x)
        );
        return x;
    }

    inline u64 bswap64(u64 x) {
        asm (
            "bswapq %0"
            : "=r" (x)
        );
        return x;
    }
#elif defined(RT_GCC_COMPATIBLE)
    #define RT_FAST_BITCOUNT

    inline unsigned builtin_clz(unsigned x) {
        return __builtin_clz(x);
    }

    inline unsigned long builtin_clz(unsigned long x) {
        return __builtin_clzl(x);
    }

    inline unsigned long long builtin_clz(unsigned long long x) {
        return __builtin_clzll(x);
    }

    inline unsigned builtin_ctz(unsigned x) {
        return __builtin_ctz(x);
    }

    inline unsigned long builtin_ctz(unsigned long x) {
        return __builtin_ctzl(x);
    }

    inline unsigned long long builtin_ctz(unsigned long long x) {
        return __builtin_ctzll(x);
    }

    inline unsigned builtin_popcount(unsigned x) {
        return __builtin_popcount(x);
    }

    inline unsigned long builtin_popcount(unsigned long x) {
        return __builtin_popcountl(x);
    }

    inline unsigned long long builtin_popcount(unsigned long long x) {
        return __builtin_popcountll(x);
    }

    inline u32 clz32(u32 x) {
        if (!x)
            return 32;
        return builtin_clz(x);
    }

    inline u64 clz64(u64 x) {
        if (!x)
            return 64;
        return builtin_clz(x);
    }

    inline u32 ctz32(u32 x) {
        if (!x)
            return 32;
        return builtin_ctz(x);
    }

    inline u64 ctz64(u64 x) {
        if (!x)
            return 64;
        return builtin_ctz(x);
    }

    inline u32 popcount32(u32 x) {
        return builtin_popcount(x);
    }

    inline u64 popcount64(u64 x) {
        return builtin_popcount(x);
    }

    inline int builtin_sadd_overflow(int x, int y) {
        int r;
        return __builtin_sadd_overflow(x, y, &r);
    }

    inline long builtin_sadd_overflow(long x, long y) {
        long r;
        return __builtin_saddl_overflow(x, y, &r);
    }

    inline long long builtin_sadd_overflow(long long x, long long y) {
        long long r;
        return __builtin_saddll_overflow(x, y, &r);
    }

    inline int builtin_ssub_overflow(int x, int y) {
        int r;
        return __builtin_ssub_overflow(x, y, &r);
    }

    inline long builtin_ssub_overflow(long x, long y) {
        long r;
        return __builtin_ssubl_overflow(x, y, &r);
    }

    inline long long builtin_ssub_overflow(long long x, long long y) {
        long long r;
        return __builtin_ssubll_overflow(x, y, &r);
    }

    inline int builtin_smul_overflow(int x, int y) {
        int r;
        return __builtin_smul_overflow(x, y, &r);
    }

    inline long builtin_smul_overflow(long x, long y) {
        long r;
        return __builtin_smull_overflow(x, y, &r);
    }

    inline long long builtin_smul_overflow(long long x, long long y) {
        long long r;
        return __builtin_smulll_overflow(x, y, &r);
    }

    inline unsigned builtin_uadd_overflow(unsigned x, unsigned y) {
        unsigned r;
        return __builtin_uadd_overflow(x, y, &r);
    }

    inline unsigned long builtin_uadd_overflow(unsigned long x, unsigned long y) {
        unsigned long r;
        return __builtin_uaddl_overflow(x, y, &r);
    }

    inline unsigned long long builtin_uadd_overflow(unsigned long long x, unsigned long long y) {
        unsigned long long r;
        return __builtin_uaddll_overflow(x, y, &r);
    }

    inline unsigned builtin_usub_overflow(unsigned x, unsigned y) {
        unsigned r;
        return __builtin_usub_overflow(x, y, &r);
    }

    inline unsigned long builtin_usub_overflow(unsigned long x, unsigned long y) {
        unsigned long r;
        return __builtin_usubl_overflow(x, y, &r);
    }

    inline unsigned long long builtin_usub_overflow(unsigned long long x, unsigned long long y) {
        unsigned long long r;
        return __builtin_usubll_overflow(x, y, &r);
    }

    inline unsigned builtin_umul_overflow(unsigned x, unsigned y) {
        unsigned r;
        return __builtin_umul_overflow(x, y, &r);
    }

    inline unsigned long builtin_umul_overflow(unsigned long x, unsigned long y) {
        unsigned long r;
        return __builtin_umull_overflow(x, y, &r);
    }

    inline unsigned long long builtin_umul_overflow(unsigned long long x, unsigned long long y) {
        unsigned long long r;
        return __builtin_umulll_overflow(x, y, &r);
    }

    inline bool sadd32_overflows(i32 a, i32 b) {
        return builtin_sadd_overflow(a, b);
    }

    inline bool ssub32_overflows(i32 a, i32 b) {
        return builtin_ssub_overflow(a, b);
    }

    inline bool smul32_overflows(i32 a, i32 b) {
        return builtin_smul_overflow(a, b);
    }

    inline bool uadd32_overflows(u32 a, u32 b) {
        return builtin_uadd_overflow(a, b);
    }

    inline bool usub32_overflows(u32 a, u32 b) {
        return builtin_usub_overflow(a, b);
    }

    inline bool umul32_overflows(u32 a, u32 b) {
        return builtin_umul_overflow(a, b);
    }

    inline bool sadd64_overflows(i64 a, i64 b) {
        return builtin_sadd_overflow(a, b);
    }

    inline bool ssub64_overflows(i64 a, i64 b) {
        return builtin_ssub_overflow(a, b);
    }

    inline bool smul64_overflows(i64 a, i64 b) {
        return builtin_smul_overflow(a, b);
    }

    inline bool uadd64_overflows(u64 a, u64 b) {
        return builtin_uadd_overflow(a, b);
    }

    inline bool usub64_overflows(u64 a, u64 b) {
        return builtin_usub_overflow(a, b);
    }

    inline bool umul64_overflows(u64 a, u64 b) {
        return builtin_umul_overflow(a, b);
    }

    inline u8 bswap8(u8 x) {
        return x;
    }

    inline u16 bswap16(u16 x) {
        u16 low = u8(x);
        return x >> 8 | low << 8;
    }

    inline u32 bswap32(u32 x) {
        return __builtin_bswap32(x);
    }

    inline u64 bswap64(u64 x) {
        return __builtin_bswap64(x);
    }
#else
    inline u32 clz32(u32 x) {
        u32 count = 0;
        while (x < 0x80000000u) count ++, x <<= 1, x |= 1;
        return count;
    }

    inline u64 clz64(u64 x) {
        u64 count = 0;
        while (x < 0x8000000000000000ull) count ++, x <<= 1;
        return count;
    }

    inline u32 ctz32(u32 x) {
        u32 count = 0;
        while (x) count ++, x >>= 1;
        return count;
    }

    inline u64 ctz64(u64 x) {
        u64 count = 0;
        while (x) count ++, x >>= 1;
        return count;
    }

    inline u32 popcount32(u32 x) {
        u32 count = 0;
        for (u32 i = 0; i < 32; i ++)
            count += !!(x >> i & 1);
        return count;
    }

    inline u64 popcount64(u64 x) {
        u64 count = 0;
        for (u32 i = 0; i < 64; i ++)
            count += !!(x >> i & 1);
        return count;
    }
#endif

#if defined(RT_GCC_COMPATIBLE)

#endif

#ifndef RELEASE
    /*
     * assert(condition)
     *
     * Checks that a boolean condition is true, panics with an optional message otherwise.
     */
    #define assert(...) do { if (!(__VA_ARGS__)) panic("Assertion failed: " TOSTRING(__VA_ARGS__)); } while (false)
#else
    #define assert(...) void()
#endif

    /*
     * unreachable(condition)
     *
     * Alias for panic() indicating unreachability.
     */
#ifdef RT_GCC_COMPATIBLE
    #define unreachable(...) do { panic(__VA_ARGS__); __builtin_unreachable(); } while (false)
#else
    #define unreachable(...) panic(__VA_ARGS__)
#endif


    inline void out_of_line_assert(bool cond) {
        assert(cond);
    }

    template<u32 N>
    inline typename BitInt<N>::unsigned_type ctzn(typename BitInt<N>::unsigned_type t);

    template<>
    inline u32 ctzn<4>(u32 t) {
        return ctz32(t);
    }

    template<>
    inline u64 ctzn<8>(u64 t) {
        return ctz64(t);
    }

    template<typename T>
    inline T ctz(T t) {
        using bits = bit_uint<T>;
        return utilities::bitcast<T>(ctzn<sizeof(T)>(utilities::bitcast<bits>(t)));
    }

    template<typename Iterator>
    struct Span {
        Iterator start, finish;

        Span(const Iterator& start_in, const Iterator& finish_in):
            start(start_in), finish(finish_in) {}

        Iterator begin() const {
            return start;
        }

        Iterator end() const {
            return finish;
        }
    };

    template<typename Int>
    struct IndexIterator {
        Int index;

        inline IndexIterator(Int i): index(i) {}

        inline bool operator==(const IndexIterator& other) const {
            return index == other.index;
        }

        inline bool operator!=(const IndexIterator& other) const {
            return index != other.index;
        }

        inline Int operator*() const {
            return index;
        }

        inline IndexIterator& operator++() {
            ++ index;
            return *this;
        }
    };

    template<typename Int>
    using Range = Span<IndexIterator<Int>>;

    template<typename Iterator>
    struct Indices : public Span<Iterator> {
        struct Wrapper {
            Iterator iterator;
            iword count = 0;

            Wrapper& operator++() {
                ++ iterator;
                ++ count;
                return *this;
            }

            bool operator==(const Wrapper& other) const {
                return iterator == other.iterator;
            }

            bool operator!=(const Wrapper& other) const {
                return iterator != other.iterator;
            }

            iword operator*() const {
                return count;
            }
        };

        Indices(Iterator start, Iterator finish):
            Span<Iterator>(start, finish) {}

        Wrapper begin() const {
            return { Span<Iterator>::start };
        }

        Wrapper end() const {
            return { Span<Iterator>::finish };
        }
    };

    /*
     * span(start, finish)
     *
     * Returns an iterable range between two iterators.
     */
    template<typename Iterator>
    auto span(Iterator start, Iterator finish) {
        return Span<Iterator> { start, finish };
    }

    /*
     * span(container)
     *
     * Returns an iterable range from the beginning to end of a container.
     */
    template<typename Container>
    auto span(const Container& container) -> Span<decltype(container.begin())> {
        return Span<decltype(container.begin())> { container.begin(), container.end() };
    }

    /*
     * range(start, end)
     *
     * Returns an iterable integer sequence between start and end (exclusive).
     */
    template<typename Int>
    auto range(Int start, Int end) -> Range<Int> {
        return Range<Int> { start, end };
    }

    /*
     * indices(container)
     *
     * Returns an iterable of the indices of the elements of an iterable container.
     */
    template<typename T>
    auto indices(const T& container) -> Indices<decltype(container.begin())> {
        return Indices { container.begin(), container.end() };
    }

    template<typename Iterator>
    struct Reverser : public Span<Iterator> {
        struct Wrapper {
            Iterator iterator;

            Wrapper& operator++() {
                -- iterator;
                return *this;
            }

            bool operator==(const Wrapper& other) const {
                return iterator == other.iterator;
            }

            bool operator!=(const Wrapper& other) const {
                return iterator != other.iterator;
            }

            const decltype(*iterator) operator*() const {
                return *iterator;
            }
        };

        Reverser(Iterator start, Iterator finish):
            Span<Iterator>(-- finish, -- start) {}

        Wrapper begin() const {
            return { Span<Iterator>::start };
        }

        Wrapper end() const {
            return { Span<Iterator>::finish };
        }
    };

    /*
     * reversed(container)
     *
     * Returns an iterable of the items of the container in reversed order.
     */
    template<typename T>
    auto reversed(const T& container) -> Reverser<decltype(container.begin())> {
        return Reverser { container.begin(), container.end() };
    }

    template<typename T>
    auto reversed(T& container) -> Reverser<decltype(container.begin())> {
        return Reverser { container.begin(), container.end() };
    }

    template<typename Iterator>
    struct Enumerator : public Span<Iterator> {
        struct Wrapper {
            Iterator iterator;
            iword count = 0;

            Wrapper& operator++() {
                ++ iterator;
                ++ count;
                return *this;
            }

            bool operator==(const Wrapper& other) const {
                return iterator == other.iterator;
            }

            bool operator!=(const Wrapper& other) const {
                return iterator != other.iterator;
            }

            pair<const iword, const decltype(*iterator)> operator*() const {
                return { count, *iterator };
            }
        };

        Enumerator(Iterator start, Iterator finish):
            Span<Iterator>(start, finish) {}

        Wrapper begin() const {
            return { Span<Iterator>::start };
        }

        Wrapper end() const {
            return { Span<Iterator>::finish };
        }
    };

    /*
     * enumerate(container)
     *
     * Returns an iterable of (index, value) pairs for each element of an iterable container.
     */
    template<typename T>
    auto enumerate(const T& container) -> Enumerator<decltype(container.begin())> {
        return Enumerator { container.begin(), container.end() };
    }

    template<typename T>
    auto enumerate(T& container) -> Enumerator<decltype(container.begin())> {
        return Enumerator { container.begin(), container.end() };
    }

    template<typename LeftIterator, typename RightIterator>
    struct Zipper {
        Span<LeftIterator> left;
        Span<RightIterator> right;

        struct Wrapper {
            LeftIterator left;
            RightIterator right;

            Wrapper& operator++() {
                ++ left, ++ right;
                return *this;
            }

            bool operator==(const Wrapper& other) const {
                return left == other.left && right == other.right;
            }

            bool operator!=(const Wrapper& other) const {
                return left != other.left || right != other.right;
            }

            pair<decltype(*left), decltype(*right)> operator*() const {
                return { *left, *right };
            }
        };

        Zipper(Span<LeftIterator> left_in, Span<RightIterator> right_in):
            left(left_in), right(right_in) {}

        Wrapper begin() const {
            return { left.start, right.start };
        }

        Wrapper end() const {
            return { left.finish, right.finish };
        }
    };

    /*
     * zip(left, right)
     *
     * Returns an iterable of (left, right) pairs of elements at the same
     * positions in the provided containers.
     */
    template<typename T, typename U>
    auto zip(const T& left, const U& right) -> Zipper<decltype(left.begin()), decltype(right.begin())> {
        return Zipper { span(left), span(right) };
    }
}

inline void* operator new(size_t, void* ptr) noexcept {
    return ptr;
}

using namespace utilities;

#endif