#
#   Implementation of clone3 syscall from process::spawn.
#

.intel_syntax noprefix

.text

.global process.init
process.init:
    lea rdx, [rsi + rdi * 8 + 8]
    lea rax, [rip + .ENVP]
    mov [rax], rdx
    lea rax, [rip + memory.initial_sp]
    mov [rax], rsp
    call process.setup_main_thread
    call lib_init
    ret

.process.handle_sig:
    ret

.global process.sig_return
process.sig_return:
    mov eax, 15
    syscall

.global process.clone_impl
process.clone_impl:             # process.clone_impl(clone_args:rdi, size:rsi, stack:rdx)
    push r15
    mov r15, rdx
    mov eax, 435                # CLONE3
    syscall
    test eax, eax
    jz .process.clone_impl.in_child
    pop r15
    ret

.process.clone_impl.in_child:
    mov rsp, r15

    sub rsp, 32
    mov edi, 33                 # SIGRT1
    lea rsi, [rip + .process.handle_sig]
    call process.sigaction
    add rsp, 8

    lea rax, [rip + .process.thread_cleanup]
    push rax                    # Push the address of the cleanup function as a return address.

    # Bottom 32 bytes on the stack are now our metadata + the cleanup function.
    # We can safely do one more call, to thread initialization:

    call thread_init

    # Returning back here, we load info and call the task.

    pop rax
    mov rcx, [rsp + 16]             # Get the task function pointer.
    mov rdi, [rsp + 8]             # ...and its argument.
    mov rsi, [rsp]             # ...and also our thread handle.
    and rsp, -16
    push rax
    jmp rcx

    # We will return to the cleanup function at the end.

.process.thread_cleanup:
    call process.exit                 # Release the thread table lock

.global process.exit_and_unmap_stack
process.exit_and_unmap_stack:
    mov r15, rdx
    mov eax, 11                 # MUNMAP
    syscall
    mov rdi, r15
    mov eax, 60                 # EXIT
    syscall

.bss

.global process.thread_table_lock
process.thread_table_lock:
    .byte 0

.global process.thread_table
process.thread_table:
    .space 524288

.global process.pid
process.pid:
    .int 0

.global process.main_thread
process.main_thread:
    .quad 0

.global process.thread_count
process.thread_count:
    .quad 0