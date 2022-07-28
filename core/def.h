#ifndef BASIL_CORE_DEF_H
#define BASIL_CORE_DEF_H
#include "stdint.h"
#include "stddef.h"

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

typedef i64 iword;
typedef u64 uword;

#ifdef __linux__
#define LIBCORE_LINUX
#elif defined(__APPLE__)
#define LIBCORE_OSX
#elif defined(_WIN64) || defined(_WIN32)
#define LIBCORE_WINDOWS
#elif defined(__wasm__)
#define LIBCORE_WASI
#endif

#ifdef __x86__
#define LIBCORE_X86
#elif defined(__amd64__) || defined(__x86_64__)
#define LIBCORE_AMD64
#elif defined(__arm64__) || defined(__aarch64__)
#define LIBCORE_ARM64
#elif defined(__wasm__)
#define LIBCORE_WASM
#endif

#endif