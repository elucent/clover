#
#   Barebones entrypoint of a program on linux_amd64
#

.intel_syntax noprefix

.text
.global _start
_start:
    xor rbp, rbp
    pop rdi                         # load argc from stack into first argument
    mov rsi, rsp                    # load pointer to argv into second argument
    lea rdx, [rsp + rdi * 8 + 8]    # load pointer to envp into third argument

    lea rax, [rip + .ENVP]          # store envp in global memory
    mov [rax], rdx

    and rsp, -16
    lea rax, [rip + memory.initial_sp]
    mov [rax], rsp

    sub rsp, 8                      # needed to align stack
    push rdi
    push rsi
    push rdx
    call process.setup_main_thread
    call lib_init
    pop rdx
    pop rsi
    pop rdi
    add rsp, 8

    # all arguments should be as we expect, and stack should be 16-byte aligned, so enter
    call main

    mov edi, eax
    call process.exit

.global __cxa_atexit
__cxa_atexit:
    ret

.global __cxa_pure_virtual
__cxa_pure_virtual:
    ret

.global __dso_handle
__dso_handle:
    ret

.global __errno_location
__errno_location:
    lea rax, [rip + os.errno]
    ret

.global atexit
atexit:
    ret
