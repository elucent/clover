.intel_syntax noprefix

.data
.clobberable:
    .quad 0

.us_tspec:
    .quad 1
    .quad 0

.PATH:
    .quad 0

.global _initial_sp
_initial_sp:
    .quad 0

.section .rodata
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
    and rsp, -16
    sub rsp, 8
    mov [rip + _initial_sp], rsp
    jmp _start_impl

.global mreq
mreq:
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
    mov eax, 10
    syscall
    ret

.global mfree
mfree:
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
    or ecx, 2624    # O_CREAT | O_TRUNC | O_NONBLOCK
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

.nwsock:            # rdi = flags, rax = result
    mov esi, edi
    and edi, 15
    shr esi, 4
    and esi, 15
    test edi, edi   # ipv4 is 0
    jz .nwsock_ipv4
    mov edi, 10     # AF_INET6
    jmp .nwsock_postip
.nwsock_ipv4:
    mov edi, 2      # AF_INET
.nwsock_postip:
    inc esi         # this works currently, maps nicely onto linux SOCK_STREAM and etc
    or esi, 2048    # SOCK_NONBLOCK
    xor edx, edx
    mov eax, 41     # socket
    syscall
    ret

.htons:
    mov ax, di
    rol ax, 8
    ret

.hton_ipv4:
    bswap edi
    ret

.hton_ipv6:
    bswap rdi
    bswap rsi
    xchg rsi, rdi
    ret

.nwipv4:            # rdi = high, rsi = low, dx = port, rcx = dest
    mov WORD PTR [rcx], 2           # AF_INET
    mov WORD PTR [rcx + 2], dx      # port
    mov DWORD PTR [rcx + 4], esi      
    mov eax, 16
    ret

.nwipv6:
    mov WORD PTR [rcx], 10         # AF_INET6
    mov WORD PTR [rcx + 2], dx     # port
    mov DWORD PTR [rcx + 4], 0     # flow
    mov QWORD PTR [rcx + 8], rdi
    mov QWORD PTR [rcx + 16], rsi
    mov DWORD PTR [rcx + 24], 0 
    mov eax, 32
    ret

.global nwbind
nwbind:
    sub rsp, 64
    mov ebx, edx    # save flags
    call .htons     # and port
    mov r14w, ax
    mov r15, rsi    # and queue size
    mov edi, edx
    call .nwsock    # create socket in rax
    cmp eax, -1     # failure?
    je .nwbind_err
    mov edi, eax    # fd as first arg
    mov rcx, rsp    # address base
    push rdi        
    xor edi, edi    # INADDR_ANY
    xor esi, esi
    mov dx, r14w    # port
    mov eax, ebx
    and eax, 15
    test eax, eax
    jnz .nwbind_ipv6
    call .nwipv4
    jmp .nwbind_postip
.nwbind_ipv6:
    call .nwipv6
.nwbind_postip:
    mov edx, eax    # addrlen
    pop rdi
    mov rsi, rsp
    mov eax, 49     # bind
    push rdi 
    syscall
    pop rdi
    test eax, eax   # failure?
    jnz .nwbind_err
    mov rsi, r15
    mov eax, 50     # listen
    push rdi
    syscall
    pop rax         # return fd
.nwbind_err:
    add rsp, 64
    ret

.global nwconn
nwconn:
    sub rsp, 64
    mov ebx, ecx    # save flags
    xchg rdi, rdx 
    call .htons     # and port
    mov r13w, ax
    mov rdi, rdx
    mov r14, rdi    # and addr
    mov r15, rsi
    mov edi, ecx
    call .nwsock    # create socket in rax
    cmp eax, 0      # failure?
    jl .nwconn_err
    mov edi, eax    # fd as first arg
    mov rcx, rsp    # address base
    push rdi        
    mov rdi, r14
    mov rsi, r15
    mov dx, r13w    # port
    mov eax, ebx
    and eax, 15
    test eax, eax
    jnz .nwconn_ipv6
    call .hton_ipv4
    call .nwipv4
    jmp .nwconn_postip
.nwconn_ipv6:
    call .hton_ipv6
    call .nwipv6
.nwconn_postip:
    mov edx, eax    # addrlen
    pop rdi
    mov rsi, rsp
    mov eax, 42     # connect
    push rdi 
    syscall
    pop rdi
    test eax, eax   # failure?
    jnz .nwconn_err
    mov eax, edi    # return fd
.nwconn_end:
    add rsp, 64
    ret
.nwconn_err:
    mov rax, -1
    jmp .nwconn_end

.global nwaccept
nwaccept:
    mov rbx, rdi
    mov rdi, rsi
    sub rsp, 64
    mov rsi, rsp
    sub rsp, 8
    mov rdx, rsp
    push rsi
    push rdx
    mov r10d, 2048    # O_NONBLOCK
    mov eax, 288     # accept4
    syscall
    pop rdx
    pop rsi
    cmp eax, 0
    jl .nwaccept_err

    mov rdx, rsi
    cmp WORD PTR [rdx], 10  # ipv6
    je .nwaccept_ipv6
    xor edi, edi
    mov esi, DWORD PTR [rdx + 4]
    call .hton_ipv4
    mov QWORD PTR [rbx], rdi
    mov QWORD PTR [rbx + 8], rsi
    jmp .nwaccept_postip
.nwaccept_ipv6:
    mov rdi, QWORD PTR [rdx + 8]
    mov rsi, QWORD PTR [rdx + 16]
    call .hton_ipv6
    mov QWORD PTR [rbx], rdi
    mov QWORD PTR [rbx + 8], rsi
.nwaccept_postip:
    add rsp, 72
    mov QWORD PTR [rbx + 16], rax
    ret
.nwaccept_err:
    mov rax, -1
    jmp .nwaccept_postip

.global nwclose
nwclose:
    mov esi, 2      # RDWR
    mov eax, 48
    syscall         # shutdown
    ret

.global pexit
pexit:
    push rdi
    call lib_deinit
    pop rdi
    mov eax, 60
    syscall

.global pclone
pclone:
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
    push rax
    push rcx
    push rdx
    push rbx
    push rbp
    push rsi
    push rdi
    push r8
    push r9
    push r10
    push r11
    push r12
    push r13
    push r14
    push r15
    call rdi
    pop r15
    pop r14
    pop r13
    pop r12
    pop r11
    pop r10
    pop r9
    pop r8
    pop rdi
    pop rsi
    pop rbp
    pop rbx
    pop rdx
    pop rcx
    pop rax
    ret
