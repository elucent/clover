use std/rt

alias fd: CLRTFd

alias FdBuffer: i8[65536]

FdBuffer*[65536] fdTable

const fd stdin: CLRTFileStdin, stdout: CLRTFileStdout

i8[] bufferFor(fd io):
    var fd(i): io
    fdTable[i]

fun flushOutput(fd io):


fun put(fd io, i8 ch):
    if io == 1 or io == 2:


fun reserve(fd io, u32 n):

