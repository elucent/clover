#include "rt/def.h"
#include "util/elumalloc.h"
#include "util/math.h"
#include "util/init.h"

extern u8* memory_alloc(u64) ASMLABEL("memory.alloc");
extern u8* memory_alloc(u64 bytes) {
    return bitcast<u8*>(elumalloc::allocate(bytes));
}

extern void memory_free(void*) ASMLABEL("memory.free");
extern void memory_free(void* ptr) {
    return elumalloc::free(ptr);
}

extern i64 math_ipow(i64, i64) ASMLABEL("math.pow(i64)");
extern i64 math_ipow(i64 i, i64 p) {
    return ipow(i, p);
}

extern u64 math_upow(u64, u64) ASMLABEL("math.pow(u64)");
extern u64 math_upow(u64 u, u64 p) {
    return upow(u, p);
}

extern f64 math_fpow(f64, f64) ASMLABEL("math.pow(f64)");
extern f64 math_fpow(f64 f, f64 p) {
    return fpow(f, p);
}

extern void clrt_init(i32 argc, i8** argv, i8** envp) ASMLABEL(".clrt.init");
extern void clrt_init(i32 argc, i8** argv, i8** envp) {
    process::init(argc, argv, envp);
    lib_init();
}

extern void clrt_deinit() ASMLABEL(".clrt.deinit");
extern void clrt_deinit() {
    process::exit(0);
}

u64 clrt_argc ASMLABEL(".clrt.argc");
i8** clrt_argv ASMLABEL(".clrt.argv");

extern u64 process_argc() ASMLABEL("process.argc");
extern u64 process_argc() {
    return clrt_argc;
}

extern const_slice<i8> process_arg(u64) ASMLABEL("process.argv");
extern const_slice<i8> process_arg(u64 i) {
    return cstring(clrt_argv[i]);
}

// System definitions.

// alias CLRTMemoryFlags: u32
u32 CLRT_MAP_READ = 1, CLRT_MAP_WRITE = 2, CLRT_MAP_EXEC = 4;

// u64 CLRTMemoryPagesize()
u64 CLRTMemoryPagesize() ASMLABEL("CLRTMemoryPagesize()u64");
u64 CLRTMemoryPagesize() {
    return memory::pagesize();
}

// i8[] CLRTMemoryMap(u64 bytes)
slice<i8> CLRTMemoryMap(u64) ASMLABEL("CLRTMemoryMap(u64)i8[]");
slice<i8> CLRTMemoryMap(u64 bytes) {
    auto pages = memory::map(bytes);
    return { (i8*)pages.data(), (iword)pages.size() };
}

// void CLRTMemoryUnmap(i8[] pages)
void CLRTMemoryUnmap(slice<i8>) ASMLABEL("CLRTMemoryUnmap(i8[])void");
void CLRTMemoryUnmap(slice<i8> pages) {
    memory::unmap(pages);
}

// void CLRTMemoryTag(i8[] pages, CLRTMemoryFlags)
void CLRTMemoryTag(slice<i8>, u32) ASMLABEL("CLRTMemoryTag(i8[],u32)void");
void CLRTMemoryTag(slice<i8> pages, u32 flags) {
    memory::tag(pages, flags);
}

// void CLRTMemoryDecommit(i8[] pages)
void CLRTMemoryDecommit(slice<i8>) ASMLABEL("CLRTMemoryDecommit(i8[])void");
void CLRTMemoryDecommit(slice<i8> pages) {
    memory::decommit(pages);
}

u32 CLRTFileStdin ASMLABEL("CLRTFileStdin") = file::stdin;
u32 CLRTFileStdout ASMLABEL("CLRTFileStdout") = file::stdout;

// alias CLRTFileFlags: u32
u32 CLRT_FP_READ = 1, CLRT_FP_WRITE = 2, CLRT_FP_APPEND = 4;

// alias CLRTFileKind: i8
u8 CLRT_FK_NONE = 0, CLRT_FK_FILE = 1, CLRT_FK_DIR = 2, CLRT_FK_SOCKET = 3;

// CLRTFd CLRTFileOpen(i8[] path, CLRTFileFlags)
u32 CLRTFileOpen(const_slice<i8>, u32) ASMLABEL("CLRTFileOpen(i8[],u32)CLRTFd");
u32 CLRTFileOpen(const_slice<i8> path, u32 flags) {
    return file::open(path, flags);
}

// u32 CLRTFileRead(CLRTFd, i8[] output)
u32 CLRTFileRead(u32, slice<i8>) ASMLABEL("CLRTFileRead(CLRTFd,i8[])u32");
u32 CLRTFileRead(u32 fd, slice<i8> output) {
    return file::read(fd, output);
}

// u32 CLRTFileWrite(CLRTFd, i8[] message)
u32 CLRTFileWrite(u32, const_slice<i8>) ASMLABEL("CLRTFileWrite(CLRTFd,i8[])u32");
u32 CLRTFileWrite(u32 fd, const_slice<i8> message) {
    return file::write(fd, message);
}

// void CLRTFileClose(CLRTFd)
void CLRTFileClose(u32) ASMLABEL("CLRTFileClose(CLRTFd)void");
void CLRTFileClose(u32 fd) {
    return file::close(fd);
}

// CLRTFileData CLRTFileInfo(i8[] path)
file::FileInfo CLRTFileInfo(const_slice<i8>) ASMLABEL("CLRTFileInfo(i8[])CLRTFileData");
file::FileInfo CLRTFileInfo(const_slice<i8> path) {
    return file::info(path);
}

// void CLRTFileRemove(i8[] path)
void CLRTFileRemove(const_slice<i8>) ASMLABEL("CLRTFileRemove(i8[])void");
void CLRTFileRemove(const_slice<i8> path) {
    file::remove(path);
}

// i8[] CLRTFileCwd(i8[] output)
slice<i8> CLRTFileCwd(slice<i8>) ASMLABEL("CLRTFileCwd(i8[])i8[]");
slice<i8> CLRTFileCwd(slice<i8> output) {
    return file::cwd(output);
}

// CLRTFd CLRTDirOpen(i8[] path, CLRTFileFlags)
u32 CLRTDirOpen(const_slice<i8>, u32) ASMLABEL("CLRTDirOpen(i8[],u32)CLRTFd");
u32 CLRTDirOpen(const_slice<i8> path, u32 flags) {
    return dir::open(path, flags);
}

// void CLRTDirClose(CLRTFd)
void CLRTDirClose(u32) ASMLABEL("CLRTDirClose(CLRTFd)void");
void CLRTDirClose(u32 fd) {
    dir::close(fd);
}

// void CLRTDirRemove(i8[] path)
void CLRTDirRemove(const_slice<i8>) ASMLABEL("CLRTDirRemove(i8[])void");
void CLRTDirRemove(const_slice<i8> path) {
    dir::remove(path);
}

// u32 CLRTDirRead(CLRTFd, CLRTDirEntry[] output)
u32 CLRTDirRead(u32, slice<dir::entry>) ASMLABEL("CLRTDirRead(CLRTFd,CLRTDirEntry[])u32");
u32 CLRTDirRead(u32 fd, slice<dir::entry> entries) {
    return dir::read(fd, entries);
}

// void CLRTProcessExit(i32 exitCode)
void CLRTProcessExit(i32) ASMLABEL("CLRTProcessExit(i32)void");
void CLRTProcessExit(i32 exitCode) {
    process::exit(exitCode);
}

// i32 CLRTProcessExec(i8[] path, i8[][] argv)
i32 CLRTProcessExec(const_slice<i8>,const_slice<const_slice<i8>>) ASMLABEL("CLRTProcessExec(i8[],i8[][])i32");
i32 CLRTProcessExec(const_slice<i8> path, const_slice<const_slice<i8>> argv) {
    return process::exec(path, argv);
}

// i64 CLRTTimeSeconds()
i64 CLRTTimeSeconds() ASMLABEL("CLRTTimeSeconds()i64");
i64 CLRTTimeSeconds() {
    return time::seconds();
}

// i64 CLRTTimeMillis()
i64 CLRTTimeMillis() ASMLABEL("CLRTTimeMillis()i64");
i64 CLRTTimeMillis() {
    return time::millis();
}

// i64 CLRTTimeNanos()
i64 CLRTTimeNanos() ASMLABEL("CLRTTimeNanos()i64");
i64 CLRTTimeNanos() {
    return time::nanos();
}

// i64 CLRTTimeTicks()
i64 CLRTTimeTicks() ASMLABEL("CLRTTimeTicks()i64");
i64 CLRTTimeTicks() {
    return time::ticks();
}

// void CLRTAtomicSetBit(u64* word, u64 bit)
void CLRTAtomicSetBit(u64*, u64) ASMLABEL("CLRTAtomicSetBit(u64*,u64)void");
void CLRTAtomicSetBit(u64* word, u64 bit) {
    atomics::set_bit(word, bit);
}

// void CLRTAtomicClearBit(u64* word, u64 bit)
void CLRTAtomicClearBit(u64*, u64) ASMLABEL("CLRTAtomicClearBit(u64*,u64)void");
void CLRTAtomicClearBit(u64* word, u64 bit) {
    atomics::clear_bit(word, bit);
}

// void CLRTAtomicTestSetBit(u64* word, u64 bit)
void CLRTAtomicTestSetBit(u64*, u64) ASMLABEL("CLRTAtomicTestSetBit(u64*,u64)void");
void CLRTAtomicTestSetBit(u64* word, u64 bit) {
    atomics::test_set_bit(word, bit);
}

// void CLRTAtomicTestClearBit(u64* word, u64 bit)
void CLRTAtomicTestClearBit(u64*, u64) ASMLABEL("CLRTAtomicTestClearBit(u64*,u64)void");
void CLRTAtomicTestClearBit(u64* word, u64 bit) {
    atomics::test_clear_bit(word, bit);
}


