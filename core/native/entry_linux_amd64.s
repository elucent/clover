.intel_syntax noprefix
.data
libcore_errno:
    .quad 0
.text
.global _start
_start:
    pop rdi
    mov rsi, rsp
    lea rdx, [rsp + rdi * 8 + 8]
    mov [rip + .PATH], rdx
    and rsp, -16
    mov [rip + _initial_sp], rsp
    sub rsp, 8
    push rdi
    push rsi
    push rdx
    call lib_init
    pop rdx
    pop rsi
    pop rdi
    add rsp, 8
    call main
    mov edi, eax
    call process.exit

# Definitions for any C/C++ runtime functions we couldn't scrub out.

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
    lea rax, [rip + libcore_errno]
    ret
