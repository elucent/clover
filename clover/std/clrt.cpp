#include "rt/def.h"
#include "util/elumalloc.h"
#include "util/math.h"
#include "util/init.h"

EXPORTED u8* memory_alloc(u64) ASMLABEL("memory.alloc");
EXPORTED u8* memory_alloc(u64 bytes) {
    return bitcast<u8*>(elumalloc::allocate(bytes));
}

EXPORTED void memory_free(void*) ASMLABEL("memory.free");
EXPORTED void memory_free(void* ptr) {
    return elumalloc::free(ptr);
}

EXPORTED i64 math_ipow(i64, i64) ASMLABEL("math.pow(i64)");
EXPORTED i64 math_ipow(i64 i, i64 p) {
    return ipow(i, p);
}

EXPORTED u64 math_upow(u64, u64) ASMLABEL("math.pow(u64)");
EXPORTED u64 math_upow(u64 u, u64 p) {
    return upow(u, p);
}

EXPORTED f64 math_fpow(f64, f64) ASMLABEL("math.pow(f64)");
EXPORTED f64 math_fpow(f64 f, f64 p) {
    return fpow(f, p);
}

EXPORTED void clrt_init(i32 argc, i8** argv, i8** envp) ASMLABEL("clrt.init");
EXPORTED void clrt_init(i32 argc, i8** argv, i8** envp) {
    process::init(argc, argv, envp);
    lib_init();
}

EXPORTED void clrt_deinit() ASMLABEL("clrt.deinit");
EXPORTED void clrt_deinit() {
    process::exit(0);
}

u64 clrt_argc ASMLABEL("clrt.argc");
i8** clrt_argv ASMLABEL("clrt.argv");

extern u64 process_argc() ASMLABEL("process.argc");
extern u64 process_argc() {
    return clrt_argc;
}

extern const_slice<i8> process_arg(u64) ASMLABEL("process.argv");
extern const_slice<i8> process_arg(u64 i) {
    return cstring(clrt_argv[i]);
}

// System definitions.

EXPORTED u64 CLRTMemoryPagesize() ASMLABEL("memory.pagesize()u64");
EXPORTED u64 CLRTMemoryPagesize() {
    return memory::pagesize();
}

EXPORTED slice<i8> CLRTMemoryMap(u64) ASMLABEL("memory.map(u64)i8[]");
EXPORTED slice<i8> CLRTMemoryMap(u64 bytes) {
    auto pages = memory::map(bytes);
    return { (i8*)pages.data(), (iword)pages.size() };
}

EXPORTED void CLRTMemoryUnmap(slice<i8>) ASMLABEL("memory.unmap(i8[])void");
EXPORTED void CLRTMemoryUnmap(slice<i8> pages) {
    memory::unmap(pages);
}

EXPORTED void CLRTMemoryTag(slice<i8>, u32) ASMLABEL("memory.tag(i8[],u32)void");
EXPORTED void CLRTMemoryTag(slice<i8> pages, u32 flags) {
    memory::tag(pages, flags);
}

EXPORTED void CLRTMemoryDecommit(slice<i8>) ASMLABEL("memory.decommit(i8[])void");
EXPORTED void CLRTMemoryDecommit(slice<i8> pages) {
    memory::decommit(pages);
}

EXPORTED u32 CLRTFileOpen(const_slice<i8>, u32) ASMLABEL("file.open(i8[],u32)file.fd");
EXPORTED u32 CLRTFileOpen(const_slice<i8> path, u32 flags) {
    return file::open(path, flags);
}

EXPORTED u32 CLRTFileRead(u32, slice<i8>) ASMLABEL("file.read(file.fd,i8[]uninit)u32");
EXPORTED u32 CLRTFileRead(u32 fd, slice<i8> output) {
    return file::read(fd, output);
}

EXPORTED u32 CLRTFileWrite(u32, const_slice<i8>) ASMLABEL("file.write(file.fd,i8[])u32");
EXPORTED u32 CLRTFileWrite(u32 fd, const_slice<i8> message) {
    return file::write(fd, message);
}

EXPORTED void CLRTFileClose(u32) ASMLABEL("file.close(file.fd)void");
EXPORTED void CLRTFileClose(u32 fd) {
    return file::close(fd);
}

EXPORTED file::FileInfo CLRTFileInfo(u32) ASMLABEL("file.info(file.fd)file.Info");
EXPORTED file::FileInfo CLRTFileInfo(u32 fd) {
    return file::info(fd);
}

EXPORTED file::FileInfo CLRTFilePathInfo(const_slice<i8>) ASMLABEL("file.pathinfo(i8[])file.Info");
EXPORTED file::FileInfo CLRTFilePathInfo(const_slice<i8> path) {
    return file::pathinfo(path);
}

EXPORTED void CLRTFileRemove(const_slice<i8>) ASMLABEL("file.remove(i8[])void");
EXPORTED void CLRTFileRemove(const_slice<i8> path) {
    file::remove(path);
}

EXPORTED slice<i8> CLRTFileCwd(slice<i8>) ASMLABEL("file.cwd(i8[])i8[]");
EXPORTED slice<i8> CLRTFileCwd(slice<i8> output) {
    return file::cwd(output);
}

EXPORTED u32 CLRTDirOpen(const_slice<i8>, u32) ASMLABEL("dir.open(i8[],u32)file.fd");
EXPORTED u32 CLRTDirOpen(const_slice<i8> path, u32 flags) {
    return dir::open(path, flags);
}

EXPORTED void CLRTDirClose(u32) ASMLABEL("dir.close(file.fd)void");
EXPORTED void CLRTDirClose(u32 fd) {
    dir::close(fd);
}

EXPORTED void CLRTDirRemove(const_slice<i8>) ASMLABEL("dir.remove(i8[])void");
EXPORTED void CLRTDirRemove(const_slice<i8> path) {
    dir::remove(path);
}

EXPORTED u32 CLRTDirRead(u32, slice<dir::entry>) ASMLABEL("dir.read(file.fd,dir.Entry[])u32");
EXPORTED u32 CLRTDirRead(u32 fd, slice<dir::entry> entries) {
    return dir::read(fd, entries);
}

EXPORTED void CLRTProcessTrap() ASMLABEL("process.trap()void");
EXPORTED void CLRTProcessTrap() {
    utilities::crash();
}

EXPORTED void CLRTProcessExit(i32) ASMLABEL("process.exit(i32)void");
EXPORTED void CLRTProcessExit(i32 exitCode) {
    process::exit(exitCode);
}

EXPORTED i32 CLRTProcessExec(const_slice<i8>,const_slice<const_slice<i8>>) ASMLABEL("process.exec(i8[],i8[][])i32");
EXPORTED i32 CLRTProcessExec(const_slice<i8> path, const_slice<const_slice<i8>> argv) {
    return process::exec(path, argv);
}

EXPORTED i64 CLRTTimeSeconds() ASMLABEL("time.seconds()i64");
EXPORTED i64 CLRTTimeSeconds() {
    return time::seconds();
}

EXPORTED i64 CLRTTimeMillis() ASMLABEL("time.millis()i64");
EXPORTED i64 CLRTTimeMillis() {
    return time::millis();
}

EXPORTED i64 CLRTTimeNanos() ASMLABEL("time.nanos()i64");
EXPORTED i64 CLRTTimeNanos() {
    return time::nanos();
}

EXPORTED i64 CLRTTimeTicks() ASMLABEL("time.ticks()i64");
EXPORTED i64 CLRTTimeTicks() {
    return time::ticks();
}

EXPORTED void CLRTAtomicSetBit(u64*, u64) ASMLABEL("atomic.setBit(u64*,u64)void");
EXPORTED void CLRTAtomicSetBit(u64* word, u64 bit) {
    atomics::set_bit(word, bit);
}

EXPORTED void CLRTAtomicClearBit(u64*, u64) ASMLABEL("atomic.clearBit(u64*,u64)void");
EXPORTED void CLRTAtomicClearBit(u64* word, u64 bit) {
    atomics::clear_bit(word, bit);
}

EXPORTED void CLRTAtomicTestSetBit(u64*, u64) ASMLABEL("atomic.testSetBit(u64*,u64)void");
EXPORTED void CLRTAtomicTestSetBit(u64* word, u64 bit) {
    atomics::test_set_bit(word, bit);
}

EXPORTED void CLRTAtomicTestClearBit(u64*, u64) ASMLABEL("atomic.testClearBit(u64*,u64)void");
EXPORTED void CLRTAtomicTestClearBit(u64* word, u64 bit) {
    atomics::test_clear_bit(word, bit);
}


