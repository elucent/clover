#
#   Memory helpers
#

.intel_syntax noprefix

.text

.global memcpy
memcpy:
.global memory.copy
memory.copy:
    mov rcx, rdx
    rep movsb
    mov rax, rdi
    ret

.global memmove
memmove:
.global memory.move
memory.move:
    cmp rdi, rsi
    jge .memory.move.direction_set
    std
.memory.move.direction_set:
    mov rcx, rdx
    rep movsb
    mov rax, rdi
    cld
    ret

.global memcmp
memcmp:
.global memory.compare
memory.compare:
    cld
    xor eax, eax
    mov rcx, rdx
    cmp rcx, rcx
    repe cmpsb
    setb al
    seta dl
    sub al, dl
    movsx rax, al
    ret

.global memset
memset:
.global memory.fill
memory.fill:
    mov al, sil
    mov rcx, rdx
    rep stosb
    mov rax, rdi
    ret