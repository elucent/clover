#include "rt/linux/common.h"

namespace time {
    enum Syscall {
        ClockGetTime = 228
    };

    enum Clock {
        Monotonic = 1
    };

    struct Timespec {
        u64 tv_sec, tv_nsec;
    };

    iword seconds() ASMLABEL("time.seconds");
    iword seconds() {
        Timespec tv;
        os_syscall(ClockGetTime, Monotonic, (iword)&tv);
        return tv.tv_sec;
    }

    iword millis() ASMLABEL("time.millis");
    iword millis() {
        Timespec tv;
        os_syscall(ClockGetTime, Monotonic, (iword)&tv);
        return tv.tv_sec * 1000 + tv.tv_nsec / 1000000;
    }

    iword nanos() ASMLABEL("time.nanos");
    iword nanos() {
        Timespec tv;
        os_syscall(ClockGetTime, Monotonic, (iword)&tv);
        return tv.tv_sec * 1000000000 + tv.tv_nsec;
    }
}
