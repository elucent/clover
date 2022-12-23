#ifndef BASIL_CORE_UTIL_H
#define BASIL_CORE_UTIL_H

#include "core/def.h"

/*
 * mcpy(dst, src, size)
 *
 * Copies size bytes from the block pointed to by src to the block pointed
 * to by dst. Assumes both pointers are pointing to a valid space of at
 * least size bytes.
 */
extern "C" void* mcpy(void* dst, const void* src, uptr size);

/*
 * mcmp(a, b, size)
 *
 * Performs a bytewise lexicographic comparison between the memory regions
 * pointed to by a and b of length size. Assumes a and b point to valid spaces
 * of at least size bytes. Returns a positive number if a is greater than b,
 * negative if a is less than b, or zero if they are equal.
 */
extern "C" iptr mcmp(const void* a, const void* b, uptr size);

/*
 * mset(dst, val, size)
 *
 * Writes the byte val to each of size bytes pointed to by dst. Assumes dst is
 * pointing to a valid space of at least size bytes.
 */
extern "C" void* mset(void* dst, u8 val, uptr size);

/*
 * panic(file, line, msg)
 *
 * Writes diagnostic information and a message to stderr then exits with a nonzero
 * code.
 */
extern "C" void panic(const i8* msg);

/*
 * cidx(str, val)
 *
 * Returns the index at which the byte val appears in the byte sequence pointed to
 * by str. Assumes the byte appears at least once within the bounds of the space
 * pointed to by str.
 */
extern "C" iptr cidx(const i8* str, i8 val);

#ifndef NDEBUG
#define assert(x) if (!(x)) panic("[ASSERT FAILED] " __FILE__ " line " TOSTRING(__LINE__) ": " #x)
#define unreachable(x) panic("[UNREACHABLE] " __FILE__ " line " TOSTRING(__LINE__) ": " x)
#define fatal(x) panic("[FATAL] " __FILE__ " line " TOSTRING(__LINE__) ": " x)
#elif defined(TEST)
#define unreachable(x) panic("[UNREACHABLE] " __FILE__ " line " TOSTRING(__LINE__) ": " x)
#define fatal(x) panic("[FATAL] " __FILE__ " line " TOSTRING(__LINE__) ": " x)
#else 
#define assert(x) void()
#define unreachable(x) void()
#define fatal(x) void()
#endif

#endif