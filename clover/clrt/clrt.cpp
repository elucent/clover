#include "rt/def.h"
#include "util/elumalloc.h"
#include "util/math.h"
#include "util/init.h"

extern u8* memory_alloc(u64) ASMLABEL("memory.alloc");
extern u8* memory_alloc(u64 bytes) {
    return bitcast<u8*>(elumalloc::allocate(bytes));
}

extern void memory_free(void*) ASMLABEL("memory.free");
extern void memory_free(void* ptr) {
    return elumalloc::free(ptr);
}

extern i64 math_ipow(i64, i64) ASMLABEL("math.pow(i64)");
extern i64 math_ipow(i64 i, i64 p) {
    return ipow(i, p);
}

extern u64 math_upow(u64, u64) ASMLABEL("math.pow(u64)");
extern u64 math_upow(u64 u, u64 p) {
    return upow(u, p);
}

extern f64 math_fpow(f64, f64) ASMLABEL("math.pow(f64)");
extern f64 math_fpow(f64 f, f64 p) {
    return fpow(f, p);
}

void setup_main_thread() ASMLABEL("process.setup_main_thread");

extern void clrt_init() ASMLABEL(".clrt.init");
extern void clrt_init() {
    setup_main_thread();
    lib_init();
}

extern void clrt_deinit() ASMLABEL(".clrt.deinit");
extern void clrt_deinit() {
    process::exit(0);
}

u64 clrt_argc ASMLABEL(".clrt.argc");
i8** clrt_argv ASMLABEL(".clrt.argv");

extern u64 process_argc() ASMLABEL("process.argc");
extern u64 process_argc() {
    return clrt_argc;
}

extern const_slice<i8> process_arg(u64) ASMLABEL("process.argv");
extern const_slice<i8> process_arg(u64 i) {
    return cstring(clrt_argv[i]);
}
