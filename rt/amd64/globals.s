# Misc. global storage

.intel_syntax noprefix

.data
.global .ENVP
.ENVP:
    .quad 0

.global memory.initial_sp
memory.initial_sp:
    .quad 0

.global os.errno
os.errno:
    .quad 0
