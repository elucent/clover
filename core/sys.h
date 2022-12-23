#ifndef BASIL_CORE_SYS_H
#define BASIL_CORE_SYS_H

#include "core/def.h"
#include "lib/slice.h"

/*
 * Assumed page size for Basil targets.
 */
constexpr iptr PAGESIZE = 4096;

/*
 * Abstract representation of a native file handle.
 */ 
typedef iptr fd;

/*
 * Storage type for a single memory page.
 */ 
typedef u8 page[PAGESIZE];

/*
 * memory.map(pages)
 * 
 * Requests the provided number of memory pages from the operating system
 * and returns a pointer to them. Returns 0 if the request fails.
 */
extern "C" slice<page> memory_map(iptr pages) ASM_LABEL("memory.map");

/*
 * memory.free(pages)
 *
 * Releases the provided contiguous block of pages from this process. 
 */
extern "C" void memory_free(slice<page> pages) ASM_LABEL("memory.free");

enum VMFLAGS {
    VM_READ = 1,    // Specifies that the memory pages should be readable.
    VM_WRITE = 2,   // Specifies that the memory pages should be writable.
    VM_EXEC = 4     // Specifies that the memory pages should be executable.
};

/*
 * memory.tag(pages)
 *
 * Sets the permissions for the provided contiguous block of pages.
 */
extern "C" void memory_tag(slice<page> pages, iptr flags) ASM_LABEL("memory.tag");

enum FDFLAGS {
    FP_READ = 1,    // Specifies that the file should be readable.
    FP_WRITE = 2,   // Specifies that the file should be writable.
    FP_APPEND = 4   // Specifies that the file should be appended to, not overwritten.
};

struct netaddr {
    i32 data[4];        // All of these are used to encode a single IPv6 address,
};                      // while IPv4 addresses are encoded using only 'data[0]'.

typedef u16 netport;

struct FileMeta {
    enum Kind : i8 {
        NONE, FILE, DIR, SOCKET
    };
    Kind kind;
    i32 sysfd;
    union {
        struct { i32 start, end; };
        struct { netaddr addr; netport port; i8 socket_flags; };
    };
};

struct FileInfo {
    FileMeta meta;
    i32 pathlen;
    i8 path[0];
};

constexpr u32 FDBUF_SIZE = 65536;
constexpr u32 MAX_FDS = 65536;

extern "C" FileInfo* fd_table[65536] ASM_LABEL("file.fd_table");
extern "C" fd file_stdin ASM_LABEL("file.stdin"), file_stdout ASM_LABEL("file.stdout"), file_stderr ASM_LABEL("file.stderr");

/*
 * file.open(path, flags)
 *
 * Attempts to open a file at the desired path with the given flags. Flags
 * may be any combination of the bits specified in the FDFLAGS enum, but at least
 * one of FP_READ or FP_WRITE must be set. If FP_WRITE is set, a file will be created
 * if it does not exist.
 * Returns -1 if the file could not be opened.
 */
extern "C" fd file_open(const_slice<i8> path, iptr flags) ASM_LABEL("file.open");

/*
 * file.read(file, buf)
 *
 * Attempts to read bytes from the file up to the size of the provided buffer. Returns
 * the number of bytes that were read.
 */
extern "C" iptr file_read(fd file, slice<i8> buf) ASM_LABEL("file.read");

/*
 * file.write(file, buf)
 *
 * Attempts to write the provided buffer's contents into the file. Returns the number
 * of bytes that were written.
 */
extern "C" iptr file_write(fd file, const_slice<i8> buf) ASM_LABEL("file.write");

/*
 * file.close(file)
 *
 * Closes the file associated with the given file descriptor. 
 */
extern "C" void file_close(fd file) ASM_LABEL("file.close");

/*
 * dir.open(path)
 *
 * Attempts to open the directory at the provided path.
 */
extern "C" fd dir_open(const_slice<i8> path, iptr flags) ASM_LABEL("dir.open");

/*
 * dir.close(dir)
 *
 * Closes the directory associated with the given file descriptor.
 */
extern "C" void dir_close(fd dir) ASM_LABEL("dir.close");

/*
 * dir.remove(path)
 *
 * Tries to delete the directory at the provided path.
 */
extern "C" void dir_remove(const_slice<i8> path) ASM_LABEL("dir.remove");

struct dir_entry {
    i8* path;
    FileMeta::Kind kind : 8;
    iptr pathlen : 56;
};

/*
 * dir.read(dir, entries)
 *
 * Attempts to read directory entries from the directory up to the size of the provided
 * buffer. Returns the number of entries that were read.
 */
 extern "C" iptr dir_read(fd dir, slice<dir_entry> entries) ASM_LABEL("dir.read");

enum IPFLAGS {
    IP_V4 = 0x00,       // Specifies that the socket should use the IPv4 protocol.
    IP_V6 = 0x01,       // Specifies that the socket should use the IPv6 protocol.
    IP_STREAM = 0x00,   // Creates a stream socket.
    IP_DGRAM = 0x10,    // Creates a datagram socket.
    IP_RAW = 0x20       // Creates a raw socket.
};

/*
 * net.open(addr, port, flags)
 *
 * Attempts to open a socket connecting to the provided address and port with properties 
 * given by the provided flags. Flags are comprised of a protocol and a socket type, 
 * specified by ORing two flags together.
 * Returns -1 if the socket could not be opened.
 */
extern "C" fd net_open(iptr flags) ASM_LABEL("net.open");

/*
 * net.serve(port, flags)
 *
 * Attempts to bind a server socket to a local port with properties given by the 
 * provided flags. Flags are comprised of a protocol and a socket type, specified
 * by ORing two flags together.
 * Returns -1 if the socket could not be opened.
 */
extern "C" fd net_serve(fd socket, netport port, iptr queue) ASM_LABEL("net.serve");

struct connected {
    bool failed, done;

    inline operator bool() const {
        return done;
    }
};

/*
 * net.connect(socket)
 *
 * Attempts to connect an opened socket to its destination.
 * Returns the state of the socket.
 */
extern "C" connected net_connect(fd socket, netaddr addr, netport port) ASM_LABEL("net.connect");

struct accepted {
    fd acceptedfd;
    bool failed;

    inline operator bool() const {
        return acceptedfd != -1;
    }
};

/*
 * net.accept(socket)
 *
 * Accepts the next pending connection on the provided socket, and returns its
 * address and file descriptor.
 * Returns -1 if there are no pending connections or if an error occurs.
 */
extern "C" accepted net_accept(fd socket) ASM_LABEL("net.accept");

/*
 * nwclose(socket)
 *
 * Closes the provided socket.
 */
extern "C" void net_close(fd socket) ASM_LABEL("net.close");

/*
 * process.exit(code)
 *
 * Exits the program with the given exit code.
 */
extern "C" void process_exit(iptr code) ASM_LABEL("process.exit");

/*
 * spawn(function, data)
 *
 * Spawns a thread to execute the provided function with some initial data.
 */
extern "C" void pspawn(void (*function)(void*), void* data);

struct chan;

/*
 * chopen()
 *
 * Creates a new channel to facilitate the exchange of data between threads.
 */
extern "C" chan* chopen();

/*
 * chrecv(ch)
 *
 * Waits until a value can be received from the provided channel, then returns it. 
 */
extern "C" void* chrecv(chan* ch);

/*
 * chsend(ch, msg)
 *
 * Sends the value msg to the provided channel ch.
 */
extern "C" void chsend(chan* ch, void* msg);

/*
 * chclose(ch)
 *
 * Closes the provided channel.    
 */
extern "C" void chclose(chan* ch);

/*
 * time.seconds()
 *
 * Returns a clock time in seconds.
 */
extern "C" iptr time_seconds() ASM_LABEL("time.seconds");

/*
 * time.millis()
 *
 * Returns a clock time in milliseconds.
 */
extern "C" iptr time_millis() ASM_LABEL("time.millis");

/*
 * time.nanos()
 *
 * Returns a clock time in nanoseconds, guaranteed to be at least millisecond-precise and usually more.
 */
extern "C" iptr time_nanos() ASM_LABEL("time.nanos");

/*
 * process.lock(ptr)
 *
 * Locks the value pointed to by ptr.
 */

extern "C" void process_lock(iptr* ptr) ASM_LABEL("process.lock");

/*
 * process.unlock(ptr)
 *
 * Unlocks the value pointed to by ptr.
 */
extern "C" void process_unlock(iptr* ptr) ASM_LABEL("process.unlock");

/*
 * sysrun(path, args)
 *
 * Executes the provided command. Returns the command's exit code.
 */
extern "C" iptr sysrun(const i8* cmd);

/*
 * getsp()
 *
 * Returns the current value of the stack pointer.
 */
extern "C" iptr getsp();

/*
 * rpushall()
 *
 * Push all registers to the stack.
 */
extern "C" void pushrframe(void(*func)());

extern "C" void __cxa_pure_virtual();

#endif