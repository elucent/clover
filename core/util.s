.text
.global memcpy
memcpy:
.global mcpy
mcpy:
    mov rcx, rdx
    rep movsb
    mov rax, rdi
    ret

.global memcmp
memcmp:
.global mcmp
mcmp:
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
.global mset
mset:
    mov al, sil
    mov rcx, rdx
    rep stosb
    mov rax, rdi
    ret