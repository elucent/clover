.intel_syntax noprefix

.data
.clobberable:
    .quad 0

.us_tspec:
    .quad 1
    .quad 0

.PATH:
    .quad 0

.fd_info_kinds:
    .space 131072

.fd_info_data:
    .space 262144

.fd_info_paths:
    .space 262144

.global _initial_sp
_initial_sp:
    .quad 0

.text
_init_fds:
    push rdi
    push rsi
    push rdx
    mov eax, 0

    // Initialize all fd structures.
    lea rcx, [rip + .fd_info_kinds]
    lea rdx, [rip + .fd_info_data]
._init_fds_loop:
    mov [rcx + rax * 4], 0  # kind = none
    mov [rdx + rax * 8], 0  # data = 0
    inc eax
    cmp eax, 32768
    jne ._init_fds_loop

    mov [rcx], 

    pop rdx
    pop rsi
    pop rdi

.global _start
_start:
    pop rdi
    mov rsi, rsp
    lea rdx, [rsp + rdi * 8 + 8]
    mov [rip + .PATH], rdx
    and rsp, -16
    sub rsp, 8
    mov [_initial_sp], rsp
    call _init_fds
    jmp _start_impl

#
# page[] memory.map(int pages:rdi)
#
.global core__memory__map
core__memory__map:
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

#
# void memory.tag(page[] pages:rdi,rsi; i8 flags:rdx)
#
.global core__memory__tag
core__memory__tag:
    mov eax, 10
    syscall
    ret

#
# void memory.unmap(page[] pages:rdi,rsi)
#
.global core__memory__unmap
core__memory__unmap:
    mov eax, 11
    shl rsi, 12
    syscall
    ret

#
# fd file.open(fd dir:rdi, path p:rsi, i8 flags:rdx)
#
.global core__file__open
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

#
# int file.read(fd file:rdi, i8[] buf:rsi,rdx)
#
.global core__file__read
core__file__read:
    xor eax, eax    # read 
    syscall
    ret

#
# int file.write(fd file:rdi, i8[] buf:rsi,rdx)
#
.global core__file__write
core__file__write:
    mov eax, 1      # write
    syscall
    ret

#
# void file.close(fd file:rdi)
#
.global core__file__close
core__file__close:
    mov eax, 3      # close
    syscall
    ret

#
# void dir.open(fd dir:rdi, path p:rsi)
#
.global core__dir__open
core__dir__open:
    ret

#
# void dir.close(fd dir:rdi)
#
.global core__dir__close
core__dir__close:
    ret

#
# int dir.read(fd dir:rdi, int start:rsi, entry[] entries:rdx,rcx)
#
.global core__dir__read
core__dir__read:
