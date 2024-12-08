#
#   General interface for performing a system call
#

.intel_syntax noprefix

.text

.global os.syscall
os.syscall:
    mov eax, edi
    mov rdi, rsi
    mov rsi, rdx
    mov rdx, rcx
    mov r10, r8
    mov r8, r9
    mov r9, [rsp + 8]
    syscall
    ret
