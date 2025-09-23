#include "rt/sys.h"

#ifdef RT_LINUX
    #define time _time
    #include "time.h"
    #undef time
    #include "unistd.h"
#endif

namespace time {
    enum Syscall {
        ClockGetTime = 228
    };

    enum Clock {
        Monotonic = 1
    };

    u64 seconds() ASMLABEL("time.seconds");
    u64 seconds() {
        #ifdef RT_LINUX
            struct timespec tsp;
            clock_gettime(CLOCK_MONOTONIC, &tsp);
            return tsp.tv_sec;
        #else
            #error "Unimplemented syscall for platform: clock_gettime"
        #endif
    }

    u64 millis() ASMLABEL("time.millis");
    u64 millis() {
        #ifdef RT_LINUX
            struct timespec tsp;
            clock_gettime(CLOCK_MONOTONIC, &tsp);
            return tsp.tv_sec * 1000 + tsp.tv_nsec / 1000000;
        #else
            #error "Unimplemented syscall for platform: clock_gettime"
        #endif
    }

    u64 nanos() ASMLABEL("time.nanos");
    u64 nanos() {
        #ifdef RT_LINUX
            struct timespec tsp;
            clock_gettime(CLOCK_MONOTONIC, &tsp);
            return tsp.tv_sec * 1000000000ull + tsp.tv_nsec;
        #else
            #error "Unimplemented syscall for platform: clock_gettime"
        #endif
    }

    u64 ticks() ASMLABEL("time.ticks");
    u64 ticks() {
        #if defined(RT_AMD64)
            u64 result;
            asm volatile (
                "\trdtsc\n"
                "\tshlq 32, %%rdx\n"
                "\torq %%rdx, %0\n"
                : "=a"(result)
                :
                :
            );
            return result;
        #else
            return nanos();
        #endif
    }
}
