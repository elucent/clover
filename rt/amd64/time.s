# High precision timing

.intel_syntax noprefix

.text
.global time.ticks
time.ticks:
    rdtsc
    shl rdx, 32
    or rax, rdx
    ret