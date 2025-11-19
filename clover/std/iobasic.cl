use std/rt

void print(i8[] str):
    CLRTFileWrite(CLRTFd(1), str)
