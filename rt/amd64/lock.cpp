#include "rt/sys.h"

namespace process {
    // Even though these functions are totally implemented in assembly, we put them
    // in a C++ source to allow them to be inlined during LTO.

    bool try_lock(i8* mutex) ASMLABEL("process.try_lock");
    bool try_lock(i8* mutex) {
        bool result;
        asm volatile (
            "\tlock btsq $0, 0(%1)\n"
            "\tsetnc %0\n"
            : "=r"(result)
            : "r"(mutex)
            :
        );
        return result;
    }

    void lock(i8* mutex) ASMLABEL("process.lock");
    void lock(i8* mutex) {
        asm volatile (
            "0:\n"
            "\tlock btsq $0, 0(%0)\n"
            "\tjc 1f\n"
            "\tjmp 2f\n"
            "1:\n"
            "\ttestq $1, 0(%0)\n"
            "\tjz 0b\n"
            "\tjmp 1b\n"
            "2:\n"
            : 
            : "r"(mutex)
            :
        );
    }

    void lock_slow() ASMLABEL("process.lock_slow");
    void lock_slow() {
        asm volatile (
            "\tpushq %%rdi\n"
            "\tpushq %%rsi\n"
            "\tpushq %%rax\n"
            "\tpushq %%r11\n"
            "\tpushq $500000\n"
            "\tpushq $0\n"
            "\tmovq %%rsp, %%rdi\n"
            "\tmovq $35, %%rax\n"   // NANOSLEEP
            "\txorq %%rsi, %%rsi\n"
            "\tsyscall\n"
            "\tpopq %%rax\n"
            "\tpopq %%rax\n"
            "\tpopq %%r11\n"
            "\tpopq %%rax\n"
            "\tpopq %%rsi\n"
            "\tpopq %%rdi\n"
            :
            :
            :
        );
    }

    void unlock(i8* mutex) ASMLABEL("process.unlock");
    void unlock(i8* mutex) {
        asm volatile (
            "\tmovb $0, 0(%0)\n"
            : 
            : "r"(mutex)
            :
        );
    }
}
