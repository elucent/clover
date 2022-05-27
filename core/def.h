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
#define CLOVER_LINUX
#elif defined(__APPLE__)
#define CLOVER_OSX
#elif defined(_WIN64) || defined(_WIN32)
#define CLOVER_WINDOWS
#endif

#endif