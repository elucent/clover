#include "rt/sys.h"

namespace atomics {
    void set_bit(u64* word, u64 index) ASMLABEL("atomics.set_bit");
    void set_bit(u64* word, u64 index) {
        index = 1ull << index;

        #ifdef RT_GCC_COMPATIBLE
            __atomic_or_fetch(word, index, __ATOMIC_SEQ_CST);
        #elif defined(RT_AMD64)
            asm volatile (
                "\tlock orq %1, 0(%0)\n"
                :
                : "r"(word), "r"(index)
                :
            );
        #else
            #error "Unimplemented syscall for platform: set_bit"
        #endif
    }

    void clear_bit(u64* word, u64 index) ASMLABEL("atomics.clear_bit");
    void clear_bit(u64* word, u64 index) {
        index = ~(1ull << index);

        #ifdef RT_GCC_COMPATIBLE
            __atomic_and_fetch(word, index, __ATOMIC_SEQ_CST);
        #elif defined(RT_AMD64)
            asm volatile (
                "\tlock andq %1, 0(%0)\n"
                :
                : "r"(word), "r"(index)
                :
            );
        #else
            #error "Unimplemented syscall for platform: set_bit"
        #endif
    }

    bool test_set_bit(u64* word, u64 index) ASMLABEL("atomics.test_set_bit");
    bool test_set_bit(u64* word, u64 index) {
        bool result;

        #ifdef RT_GCC_COMPATIBLE
            index = 1ull << index;
            result = __atomic_fetch_or(word, index, __ATOMIC_SEQ_CST) & index;
        #elif defined(RT_AMD64)
            asm volatile (
                "\tlock btsq %2, 0(%1)\n"
                "\tsetc %0\n"
                : "=r"(result)
                : "r"(word), "r"(index)
                :
            );
        #else
            #error "Unimplemented syscall for platform: test_set_bit"
        #endif

        return result;
    }

    bool test_clear_bit(u64* word, u64 index) ASMLABEL("atomics.test_clear_bit");
    bool test_clear_bit(u64* word, u64 index) {
        bool result;

        #ifdef RT_GCC_COMPATIBLE
            index = 1ull << index;
            result = __atomic_fetch_and(word, ~index, __ATOMIC_SEQ_CST) & index;
        #elif defined(RT_AMD64)
            asm volatile (
                "\tlock btrq %2, 0(%1)\n"
                "\tsetc %0\n"
                : "=r"(result)
                : "r"(word), "r"(index)
                :
            );
        #else
            #error "Unimplemented syscall for platform: test_clear_bit"
        #endif

        return result;
    }

    u64 swap(u64* word, u64 input) ASMLABEL("atomics.swap");
    u64 swap(u64* word, u64 input) {
        #ifdef RT_GCC_COMPATIBLE
            __atomic_exchange(word, &input, &input, __ATOMIC_SEQ_CST);
        #elif defined(RT_AMD64)
            asm volatile (
                "\tlock xchg %0, 0(%1)\n"
                : "+r"(input)
                : "r"(word)
                :
            );
        #else
            #error "Unimplemented syscall for platform: swap"
        #endif

        return input;
    }
}