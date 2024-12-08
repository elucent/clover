#ifndef RT_LINUX_COMMON_H
#define RT_LINUX_COMMON_H

#include "rt/def.h"
#include "rt/sys.h"

extern iword os_syscall(i32 id, iword a = 0, iword b = 0, iword c = 0, iword d = 0, iword e = 0, iword f = 0) ASMLABEL("os.syscall");

#define os_syscall_inline_2(result, syscall, arg1, arg2) \
    asm volatile ( \
        "syscall\n" \
        : "=a"(result) \
        : "a"(syscall), "D"(arg1), "S"(arg2) \
        : "memory", "rcx", "r11" \
    );

#endif