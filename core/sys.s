.data
.clobberable:
    .quad 0

.us_tspec:
    .quad 1
    .quad 0

.PATH:
    .quad 0

.rodata
.shell:
    .asciz "/bin/sh"
.shell_name:
    .asciz "sh"
.shell_arg:
    .asciz "-c"

.text
.global _start
_start:
    pop rdi
    mov rsi, rsp
    lea rdx, [rsp + rdi * 8 + 8]
    mov [rip + .PATH], rdx
    jmp _start_impl

.global mmap
mmap:
    push rdi
    mov rsi, rdi
    xor edi, edi
    shl rsi, 12
    mov rdx, 3      # read | write by default
    mov r10d, 34    # anonymous | private
    mov r8d, -1
    xor r9d, r9d
    mov eax, 9
    syscall
    pop rdx
    ret 

.global mpermit
mpermit:

.global munmap
munmap:
    mov eax, 11
    shl rsi, 12
    syscall
    ret

.global fdopen
fdopen:
    mov ecx, esi    # copy flags
    and ecx, 3      
    dec ecx         # ecx now holds read/write flags
    mov edx, esi
    and edx, 2      # create based on write bit
    jz .no_create
    or ecx, 576     # O_CREAT | O_TRUNC
.no_create:
    and esi, 4      # append
    jz .no_append
    or ecx, 1024
.no_append:
    mov eax, 2      # open
    mov esi, ecx
    mov edx, 0666
    syscall
    ret

.global fdread
fdread:
    xor eax, eax    # read 
    syscall
    ret

.global fdwrite
fdwrite:
    mov eax, 1      # write
    syscall
    ret

.global fdclose
fdclose:
    mov eax, 3      # close
    syscall
    ret

.global exit
exit:
    push rdi
    call lib_deinit
    pop rdi
    mov eax, 60
    syscall

.global clone
clone:
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

.global getpid
getpid:
    mov eax, 39
    syscall
    ret

.global gettid
gettid:
    mov eax, 186
    syscall
    ret

.global plock
.spin:
    test QWORD PTR [rdi], 1
    jnz .spin
plock:
    lock bts QWORD PTR [rdi], 0
    jc .spin
    ret   

.global punlock
punlock:
    mov QWORD PTR [rdi], 0
    ret

.global pyield
pyield:
    mov eax, 34
    syscall
    ret

.global _tgid
.global psignal
psignal:
    mov eax, 234
    mov rdx, rdi    # sig id in rdx
    mov rdi, QWORD PTR [rip + _tgid]
    syscall
    ret

.global pswitch
pswitch:
    ret

.global phandle
phandle:
    mov eax, 13
    xor edx, edx
    mov r10d, 32
    syscall
    ret

.global nanotime
nanotime:
    mov eax, 228    # clock_gettime
    mov edi, 1      # CLOCK_MONOTONIC
    sub rsp, 16
    mov rsi, rsp
    syscall
    imul rax, [rsp], 1000000000  # tv_sec
    add rax, [rsp + 8]           # tv_nsec
    add rsp, 16
    ret

.global system
system:
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