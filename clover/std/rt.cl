
############
#  Memory  #
############

alias CLRTMemoryFlags: u32

u64 CLRTMemoryPagesize()
i8[] CLRTMemoryMap(u64 bytes)
void CLRTMemoryUnmap(i8[] pages)
void CLRTMemoryTag(i8[] pages, CLRTMemoryFlags)
void CLRTMemoryDecommit(i8[] pages)

###########
#  Files  #
###########

alias CLRTFileFlags: u32
alias CLRTFileKind: u8

type CLRTFd:
    i32 id

CLRTFd CLRTFileStdin, CLRTFileStdout

type CLRTFilePerm:
    const Read: 1, Write: 2, Append: 4

CLRTFd CLRTFileOpen(i8[] path, CLRTFileFlags)
u32 CLRTFileRead(CLRTFd, uninit i8[] output)
u32 CLRTFileWrite(CLRTFd, i8[] message)
void CLRTFileClose(CLRTFd)

type CLRTFileData:
    u32 size
    CLRTFileKind kind

CLRTFileData CLRTFileInfo(CLRTFd)
CLRTFileData CLRTFilePathInfo(i8[] path)

void CLRTFileRemove(i8[] path)
i8[] CLRTFileCwd(i8[] output)

#################
#  Directories  #
#################

CLRTFd CLRTDirOpen(i8[] path, CLRTFileFlags)
void CLRTDirClose(CLRTFd)
void CLRTDirRemove(i8[] path)

type CLRTDirEntry:
    i8* path
    u32 pathlen
    CLRTFileKind kind

u32 CLRTDirRead(CLRTFd, CLRTDirEntry[] output)

###############
#  Processes  #
###############

void CLRTProcessTrap()
void CLRTProcessExit(i32 exitCode)
i32 CLRTProcessExec(i8[] path, i8[][] argv)

##########
#  Time  #
##########

i64 CLRTTimeSeconds()
i64 CLRTTimeMillis()
i64 CLRTTimeNanos()
i64 CLRTTimeTicks()

#############
#  Atomics  #
#############

void CLRTAtomicSetBit(u64* word, u64 bit)
void CLRTAtomicClearBit(u64* word, u64 bit)
void CLRTAtomicTestSetBit(u64* word, u64 bit)
void CLRTAtomicTestClearBit(u64* word, u64 bit)

