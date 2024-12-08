#
#   Memory helpers
#

.intel_syntax noprefix

.text

.global memory.sp
memory.sp:
    lea rax, [rsp + 8]
    ret

.global memory.flush
memory.flush:
    push rbx
    push rbp
    push r12
    push r13
    push r14
    push r15
    call rdi
    pop r15
    pop r14
    pop r13
    pop r12
    pop rbp
    pop rbx
    ret

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