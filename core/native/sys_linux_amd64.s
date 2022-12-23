.intel_syntax noprefix

.data
.clobberable:
    .quad 0

.us_tspec:
    .quad 1
    .quad 0

.global .PATH
.PATH:
    .quad 0

.global _initial_sp
_initial_sp:
    .quad 0

.global file.stdin
file.stdin:
    .quad 0

.global file.stdout
file.stdout:
    .quad 1

.global file.stderr
file.stderr:
    .quad 2

.bss

.global file.fd_table
file.fd_table:
    .space 524288

.section .rodata
.shell:
    .asciz "/bin/sh"
.shell_name:
    .asciz "sh"
.shell_arg:
    .asciz "-c"

.text
.global memory.map
memory.map:
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

.global memory.tag
memory.tag:
    mov eax, 10
    syscall
    ret

.global memory.free
memory.free:
    mov eax, 11
    shl rsi, 12
    syscall
    ret

.global do_syscall
do_syscall:
    mov eax, edi
    mov rdi, rsi
    mov rsi, rdx
    mov rdx, rcx
    mov r10, r8
    mov r8, r9
    mov r10, rcx
    syscall
    ret

.global file.open
file.open:
    jmp libcore_file_open_impl

.global file.read
file.read:
    jmp libcore_file_read_impl

.global file.write
file.write:
    jmp libcore_file_write_impl

.global file.close
file.close:
    jmp libcore_file_close_impl

.global dir.open
dir.open:
    jmp libcore_dir_open_impl

.global dir.close
dir.close:
    jmp libcore_dir_close_impl

.global dir.read
dir.read:
    jmp libcore_dir_read_impl

.global dir.remove
dir.remove:
    jmp libcore_dir_remove_impl

.global net.open
net.open:
    jmp libcore_net_open_impl

.global net.serve
net.serve:
    jmp libcore_net_serve_impl

.global net.connect
net.connect:
    jmp libcore_net_connect_impl

.global net.accept
net.accept:
    jmp libcore_net_accept_impl

.global net.close
net.close:
    jmp libcore_net_close_impl

.global process.exit
process.exit:
    push rdi
    call lib_deinit
    pop rdi
    mov eax, 60
    syscall

# .global pclone
# pclone:
#     push r14
#     push r15
#     mov eax, 56
#     mov r14, rdi        # store fn ptr
#     mov r15, rdx        # store arg
#     mov edi, 3477248    # CLONE_VM | CLONE_FS | CLONE_FILES | CLONE_SIGHAND | CLONE_THREAD | CLONE_SYSVSEM | CLONE_PARENT_SETTID | CLONE_CHILD_CLEARTID
#     lea rdx, [rip + .clobberable]   # parent tid
#     lea r10, [rip + .clobberable]   # 0 child tid
#     syscall
#     test eax, eax
#     jnz .parent
#     mov rax, r14    # get fn ptr
#     mov rdi, r15    # get arg
#     jmp rax
# .parent:
#     pop r15
#     pop r14
#     ret

# .global getpid
# getpid:
#     mov eax, 39
#     syscall
#     ret

# .global gettid
# gettid:
#     mov eax, 186
#     syscall
#     ret

.global process.lock
.spin:
    test QWORD PTR [rdi], 1
    jnz .spin
process.lock:
    lock bts QWORD PTR [rdi], 0
    jc .spin
    ret   

.global process.unlock
process.unlock:
    mov QWORD PTR [rdi], 0
    ret

# .global pyield
# pyield:
#     mov eax, 34
#     syscall
#     ret

# .global _tgid
# .global psignal
# psignal:
#     mov eax, 234
#     mov rdx, rdi    # sig id in rdx
#     mov rdi, QWORD PTR [rip + _tgid]
#     syscall
#     ret

# .global pswitch
# pswitch:
#     ret

# .global phandle
# phandle:
#     mov eax, 13
#     xor edx, edx
#     mov r10d, 32
#     syscall
#     ret

.global time.seconds
time.seconds:
    mov eax, 228    # clock_gettime
    mov edi, 1      # CLOCK_MONOTONIC
    sub rsp, 16
    mov rsi, rsp
    syscall
    mov rax, [rsp]        # tv_sec
    add rsp, 16
    ret

.global time.millis
time.millis:
    mov eax, 228    # clock_gettime
    mov edi, 1      # CLOCK_MONOTONIC
    sub rsp, 16
    mov rsi, rsp
    syscall
    imul rdi, [rsp], 1000        # tv_sec
    mov rax, [rsp + 8]           # tv_nsec
    cqo
    mov rcx, 1000000
    idiv rcx
    add rax, rdi
    add rsp, 16
    ret

.global time.nanos
time.nanos:
    mov eax, 228    # clock_gettime
    mov edi, 1      # CLOCK_MONOTONIC
    sub rsp, 16
    mov rsi, rsp
    syscall
    imul rax, [rsp], 1000000000  # tv_sec
    add rax, [rsp + 8]           # tv_nsec
    add rsp, 16
    ret

.global sysrun
sysrun:
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

.global getsp
getsp:
    lea rax, [rsp + 8]
    ret

.global pushrframe
pushrframe:
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
