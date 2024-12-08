#include "rt/sys.h"

namespace atomics {
    void set_bit(u64* word, u64 index) ASMLABEL("atomics.set_bit");
    void set_bit(u64* word, u64 index) {
        index = 1ull << index;
        asm volatile (
            "\tlock orq %1, 0(%0)\n"
            :
            : "r"(word), "r"(index)
            :
        );
    }

    void clear_bit(u64* word, u64 index) ASMLABEL("atomics.clear_bit");
    void clear_bit(u64* word, u64 index) {
        index = ~(1ull << index);
        asm volatile (
            "\tlock andq %1, 0(%0)\n"
            :
            : "r"(word), "r"(index)
            :
        );
    }

    bool test_set_bit(u64* word, u64 index) ASMLABEL("atomics.test_set_bit");
    bool test_set_bit(u64* word, u64 index) {
        bool result;
        asm volatile (
            "\tlock btsq %2, 0(%1)\n"
            "\tsetc %0\n"
            : "=r"(result)
            : "r"(word), "r"(index)
            :
        );
        return result;
    }

    bool test_clear_bit(u64* word, u64 index) ASMLABEL("atomics.test_clear_bit");
    bool test_clear_bit(u64* word, u64 index) {
        bool result;
        asm volatile (
            "\tlock btrq %2, 0(%1)\n"
            "\tsetc %0\n"
            : "=r"(result)
            : "r"(word), "r"(index)
            :
        );
        return result;
    }

    u64 swap(u64* word, u64 input) ASMLABEL("atomics.swap");
    u64 swap(u64* word, u64 input) {
        asm volatile (
            "\tlock xchg %0, 0(%1)\n"
            : "+r"(input)
            : "r"(word)
            :
        );
        return input;
    }
}