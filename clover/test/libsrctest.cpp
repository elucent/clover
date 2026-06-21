#include "util/io.h"
#include "util/utf.h"

#define DEFINE_LOG(ctype, type) \
    EXPORTED void log(ctype) ASMLABEL("test.log(" type ")void"); \
    EXPORTED void log(ctype value) { \
        println(value); \
    }

#define DEFINE_CUSTOM_LOG(ctype, type, ...) \
    EXPORTED void log(ctype) ASMLABEL("test.log(" type ")void"); \
    EXPORTED void log(ctype value) { \
        __VA_ARGS__; \
    }

DEFINE_CUSTOM_LOG(bool, "bool", println(value ? "true" : "false"))
DEFINE_LOG(rune, "char")
DEFINE_LOG(f32, "f32")
DEFINE_LOG(f64, "f64")
DEFINE_LOG(i8, "i8")
DEFINE_LOG(i64, "i64")
DEFINE_LOG(u64, "u64")
DEFINE_LOG(const_slice<i8>, "i8[]")
