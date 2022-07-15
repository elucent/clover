.intel_syntax noprefix

.data
.clobberable:
    .quad 0

.us_tspec:
    .quad 1
    .quad 0

.PATH:
    .quad 0

.shell:
    .asciz "/bin/sh"
.shell_name:
    .asciz "sh"
.shell_arg:
    .asciz "-c"

.global _initial_sp
_initial_sp:
    .quad 0

.text
.global ___stack_chk_fail
___stack_chk_fail:
.global ___stack_chk_guard
___stack_chk_guard:
    ret

.global start
start:
.global _start
_start:
    pop rdi
    mov rsi, rsp
    lea rdx, [rsp + rdi * 8 + 8]
    mov [rip + .PATH], rdx
    and rsp, -16
    sub rsp, 8
    mov [_initial_sp], rsp
    jmp __start_impl

.global _mreq
_mreq:
    push rdi
    mov rsi, rdi
    xor edi, edi
    shl rsi, 12
    mov rdx, 3          # read | write by default
    mov r10d, 4098      # anonymous | private
    mov r8d, -1
    xor r9d, r9d
    mov eax, 0x20000c5
    syscall
    pop rdx
    ret 

.global _mpermit
_mpermit:

.global _mfree
_mfree:
    mov eax, 0x2000049
    shl rsi, 12
    syscall
    ret

.global _fdopen
_fdopen:
    mov ecx, esi    # copy flags
    and ecx, 3      
    dec ecx         # ecx now holds read/write flags
    mov edx, esi
    and edx, 2      # create based on write bit
    jz .no_create
    or ecx, 1540    # O_CREAT | O_TRUNC | O_NONBLOCK
.no_create:
    and esi, 4      # append
    jz .no_append
    or ecx, 8
.no_append:
    mov eax, 0x2000005      # open
    mov esi, ecx
    mov edx, 0666
    syscall
    ret

.global _fdread
_fdread:
    mov eax, 0x2000003    # read 
    syscall
    ret

.global _fdwrite
_fdwrite:
    mov eax, 0x2000004      # write
    syscall
    ret

.global _fdclose
_fdclose:
    mov eax, 0x2000006      # close
    syscall
    ret

.global _pexit
_pexit:
    push rdi
    call _lib_deinit
    pop rdi
    mov eax, 0x2000001
    syscall

.global _pclone
_pclone:
    push r14
    push r15
    mov eax, 56
    mov r14, rdi        # store fn ptr
    mov r15, rdx        # store arg
    mov edi, 3477248    # CLONE_VM | CLONE_FS | CLONE_FILES | CLONE_SIGHAND | CLONE_THREAD | CLONE_SYSVSEM | CLONE_PARENT_SETTID | CLONE_CHILD_CLEARTID
    lea rdx, [rip + .clobberable]   # parent tid
    lea r10, [rip + .clobberable]   # 0 child tid
    syscall
    test eax, eax
    jnz .parent
    mov rax, r14    # get fn ptr
    mov rdi, r15    # get arg
    jmp rax
.parent:
    pop r15
    pop r14
    ret

.global _plock
.spin:
    test QWORD PTR [rdi], 1
    jnz .spin
_plock:
    lock bts QWORD PTR [rdi], 0
    jc .spin
    ret   

.global _punlock
_punlock:
    mov QWORD PTR [rdi], 0
    ret

.global _nanotime
_nanotime:
    mov eax, 0x2000074    # gettimeofday
    sub rsp, 24
    mov QWORD PTR [rsp + 16], 0
    lea rsi, [rsp + 16]
    mov rdi, rsp
    xor edx, edx
    syscall
    imul rax, [rsp], 1000000000  # tv_sec
    imul edx, [rsp + 8], 1000    # tv_usec
    add rax, rdx
    add rsp, 24
    ret

.global _sysrun
_sysrun:
    push rdi
    push rsi
    mov edi, 0x1200000
    xor esi, esi
    xor edx, edx
    mov r10, rsp
    xor r8, r8
    mov eax, 58     # clone()
    syscall
    test eax, eax
    jz .system_child
    pop rsi
    pop rdi
    mov edi, eax    # child pid
    sub rsp, 8
    lea rsi, [rsp]
    xor edx, edx
    xor r10d, r10d  # disregard usage
    mov eax, 61
    syscall         # wait4
    mov eax, [rsp]
    add rsp, 8
    ret
.system_child:
    pop rsi
    pop rdi
    xor eax, eax
    push rax 
    push rdi
    lea rax, [rip + .shell_arg]
    push rax 
    lea rax, [rip + .shell_name]
    push rax 
    lea rdi, [rip + .shell]
    mov rsi, rsp
    mov rdx, [rip + .PATH]
    mov eax, 59    # execve
    syscall
    pop rsi # rdi
    pop rsi # nullptr
    mov edi, eax
    mov eax, 60
    syscall # exit

.global _memcpy
_memcpy:
.global _mcpy
_mcpy:
    mov rcx, rdx
    rep movsb
    mov rax, rdi
    ret

.global _memcmp
_memcmp:
.global _mcmp
_mcmp:
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

.global _memset
_memset:
.global _mset
_mset:
    mov al, sil
    mov rcx, rdx
    rep stosb
    mov rax, rdi
    ret
