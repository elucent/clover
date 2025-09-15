#include "rt/sys.h"

namespace process {
    // Even though this function is totally implemented in assembly, we put it
    // in a C++ source to allow it to be inlined during LTO.

    #ifdef RT_LIBC_COMPATIBLE
        thread_local i8 rt_tls[65536];
    #endif

    void* tls() ASMLABEL("process.tls");
    void* tls() {
        #ifdef RT_LIBC_COMPATIBLE
            return rt_tls;
        #else
            void* result;
            asm (
                "movq %%fs:0x0, %0\n"
                : "=r"(result)
                :
                :
            );
            return result;
        #endif
    }
}
