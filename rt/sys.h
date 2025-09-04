#ifndef RT_SYS_H
#define RT_SYS_H

#include "rt/def.h"

namespace memory {
    constexpr iword KiB = 1024;
    constexpr iword MiB = KiB * 1024;
    constexpr iword GiB = MiB * 1024;

    /*
     * Assumed page size for targets.
     */
    constexpr iword PAGESIZE = 4 * KiB;

    /*
     * Storage type for a single memory page.
     */
    using page = array<i8, PAGESIZE>;

    /*
     * memory.map(pages)
     *
     * Requests the provided number of memory pages from the operating system
     * and returns a pointer to them. Returns 0 if the request fails.
     */
    extern slice<page> map(iword pages) ASMLABEL("memory.map");

    /*
     * memory.unmap(pages)
     *
     * Releases the provided contiguous block of pages from this process.
     */
    extern void unmap(slice<page> pages) ASMLABEL("memory.unmap");

    /*
     * memory.tag(pages)
     *
     * Sets the permissions for the provided contiguous block of pages.
     */
    extern void tag(slice<page> pages, iword flags) ASMLABEL("memory.tag");

    /*
     * memory.decommit(pages)
     *
     * Does not unmap, but allows the decommitting of memory in the provided
     * page block.
     */
    extern void decommit(slice<page> pages) ASMLABEL("memory.decommit");

    /*
     * memory.sp()
     *
     * Returns the current value of the stack pointer.
     */
    extern iword sp() ASMLABEL("memory.sp");

    /*
     * memory.flush()
     *
     * Flush all registers to the stack and call.
     */
    extern iword flush(void* func) ASMLABEL("memory.flush");

    enum Tag {
        READ = 1,       // Specifies that the memory pages should be readable.
        WRITE = 2,      // Specifies that the memory pages should be writable.
        EXEC = 4        // Specifies that the memory pages should be executable.
    };
}

namespace net {
    struct addr {
        i32 data[4];    // All of these are used to encode a single IPv6 address,
    };                  // while IPv4 addresses are encoded using only 'data[0]'.

    typedef u16 port;
}

namespace file {
    /*
     * Abstract representation of a native file handle.
     */
    typedef i32 fd;

    enum Permission {
        READ = 1,       // Specifies that the file should be readable.
        WRITE = 2,      // Specifies that the file should be writable.
        APPEND = 4      // Specifies that the file should be appended to, not overwritten.
    };

    enum Kind : i8 {
        NONE, FILE, DIR, SOCKET
    };

    struct FileMeta {
        Kind kind;
        i8 mutex;
        i32 sysfd;
        union {
            struct { i32 start, end; };
            struct { net::addr addr; net::port port; i8 socket_flags; };
        };
    };

    struct FileStorage {
        FileMeta meta;
        i32 pathlen;
        i8 path[0];
    };

    constexpr u32 FDBUF_SIZE = 65536;
    constexpr u32 MAX_FDS = 65536;

    extern FileStorage* fd_table[65536] ASMLABEL("file.fd_table");
    extern fd stdin ASMLABEL("file.stdin"), stdout ASMLABEL("file.stdout"), stderr ASMLABEL("file.stderr");

    /*
     * file.open(path, flags)
     *
     * Attempts to open a file at the desired path with the given flags. Flags
     * may be any combination of the bits specified in the FDFLAGS enum, but at least
     * one of FP_READ or FP_WRITE must be set. If FP_WRITE is set, a file will be created
     * if it does not exist.
     * Returns -1 if the file could not be opened.
     */
    extern fd open(const_slice<i8> path, iword flags) ASMLABEL("file.open");

    /*
     * file.read(file, buf)
     *
     * Attempts to read bytes from the file up to the size of the provided buffer. Returns
     * the number of bytes that were read.
     */
    extern iword read(fd file, slice<i8> buf) ASMLABEL("file.read");

    /*
     * file.write(file, buf)
     *
     * Attempts to write the provided buffer's contents into the file. Returns the number
     * of bytes that were written.
     */
    extern iword write(fd file, const_slice<i8> buf) ASMLABEL("file.write");

    /*
     * file.remove(path)
     *
     * Deletes the file at the specified path.
     */
    extern void remove(const_slice<i8> path) ASMLABEL("file.remove");

    /*
     * file.close(file)
     *
     * Closes the file associated with the given file descriptor.
     */
    extern void close(fd file) ASMLABEL("file.close");

    struct FileInfo {
        u32 size;
        Kind kind : 8;
    };

    /*
     * file.info(path)
     *
     * Returns information about the file at the specified path.
     */
    extern FileInfo info(const_slice<i8> path) ASMLABEL("file.info");

    /*
     * file.kind(file)
     *
     * Returns the type of file represented by the given file descriptor.
     */
    extern Kind kind(fd file) ASMLABEL("file.kind");

    /*
     * file.cwd()
     *
     * Writes the working directory path of the current program into the output
     * buffer provided. Returns the subslice of the output buffer that was
     * written.
     */
    extern slice<i8> cwd(slice<i8> output) ASMLABEL("file.cwd");
}

namespace dir {
    using namespace file;

    /*
     * dir.open(path)
     *
     * Attempts to open the directory at the provided path.
     */
    extern fd open(const_slice<i8> path, iword flags) ASMLABEL("dir.open");

    /*
     * dir.close(dir)
     *
     * Closes the directory associated with the given file descriptor.
     */
    extern void close(fd dir) ASMLABEL("dir.close");

    /*
     * dir.remove(path)
     *
     * Tries to delete the directory at the provided path.
     */
    extern void remove(const_slice<i8> path) ASMLABEL("dir.remove");

    struct entry {
        i8* path;
        i32 pathlen;
        Kind kind;
    };

    /*
     * dir.read(dir, entries)
     *
     * Attempts to read directory entries from the directory up to the size of the provided
     * buffer. Returns the number of entries that were read.
     */
    extern iword read(fd dir, slice<entry> entries) ASMLABEL("dir.read");
}

namespace net {

    using file::fd;

    enum Flags {
        IPV4 = 0x00,        // Specifies that the socket should use the IPv4 protocol.
        IPV6 = 0x01,        // Specifies that the socket should use the IPv6 protocol.
        STREAM = 0x00,      // Creates a stream socket.
        DATAGRAM = 0x10,    // Creates a datagram socket.
        RAW = 0x20          // Creates a raw socket.
    };

    /*
    * net.open(addr, port, flags)
    *
    * Attempts to open a socket connecting to the provided address and port with properties
    * given by the provided flags. Flags are comprised of a protocol and a socket type,
    * specified by ORing two flags together.
    * Returns -1 if the socket could not be opened.
    */
    extern fd open(iword flags) ASMLABEL("net.open");

    /*
    * net.serve(port, flags)
    *
    * Attempts to bind a server socket to a local port with properties given by the
    * provided flags. Flags are comprised of a protocol and a socket type, specified
    * by ORing two flags together.
    * Returns -1 if the socket could not be opened.
    */
    extern fd serve(fd socket, net::port port, iword queue) ASMLABEL("net.serve");

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
    extern connected connect(fd socket, net::addr addr, net::port port) ASMLABEL("net.connect");

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
    extern accepted accept(fd socket) ASMLABEL("net.accept");

    /*
    * net.close(socket)
    *
    * Closes the provided socket.
    */
    extern void close(fd socket) ASMLABEL("net.close");
}

namespace process {
    /*
     * process.init(argc, argv)
     *
     * Initializes an already-started program. Used when this runtime is not the
     * provider of the entrypoint function.
     */
    extern void init(i32 argc, i8** argv) ASMLABEL("process.init");

    /*
     * process.exit(code)
     *
     * Exits the program with the given exit code.
     */
    extern void exit(iword code) ASMLABEL("process.exit");

    /*
     * process.try_lock(ptr)
     *
     * Tries to acquire the lock pointed to by ptr, returning true if successful
     * and false otherwise.
     */

    extern bool try_lock(i8* ptr) ASMLABEL("process.try_lock");

    /*
     * process.lock(ptr)
     *
     * Locks the value pointed to by ptr.
     */

    extern void lock(i8* ptr) ASMLABEL("process.lock");

    /*
     * process.unlock(ptr)
     *
     * Unlocks the value pointed to by ptr.
     */
    extern void unlock(i8* ptr) ASMLABEL("process.unlock");

    using thread = iword;
    constexpr u32 CHANNEL_BUFFER_SIZE = 512;

    /*
     * process.current()
     *
     * Returns the currently-executing thread.
     */
    extern thread current() ASMLABEL("process.current");

    /*
     * process.tls()
     *
     * Returns a pointer to the thread-local storage of the current thread.
     */
    extern void* tls() ASMLABEL("process.tls");

    /*
     * process.spawn(task, parameter, stack, channels)
     *
     * Spawns a new thread with the provided number of stack pages, the desired
     * number of channels, running the provided task function. Returns a handle
     * to the newly-created thread.
     */
    extern thread spawn(void (*task)(iword), iword parameter, iword stack, iword tls, iword channels) ASMLABEL("process.spawn");

    /*
     * process.send(thread, channel, message)
     *
     * Sends a message to the specified channel of the specified thread. Returns
     * whether sending the message succeeded.
     */
    extern bool send(thread t, iword channel, iword message) ASMLABEL("process.send");

    /*
     * process.isempty(channel)
     *
     * Returns whether the specified channel on the current thread is empty.
     */
    extern bool isempty(iword channel) ASMLABEL("process.isempty");

    /*
     * process.receive(channel)
     *
     * Waits to receive a message from the specified channel of the current
     * thread. Returns the message once one is received.
     */
    extern iword receive(iword channel) ASMLABEL("process.receive");

    /*
     * process.await(thread)
     *
     * Waits until the specified thread finishes.
     */
    extern void await(thread t) ASMLABEL("process.await");

    /*
     * process.sleep()
     *
     * Suspends the current thread until it's woken by another thread.
     */
    extern void sleep() ASMLABEL("process.sleep");

    /*
     * process.running(thread)
     *
     * Returns whether the specified thread is currently running.
     */
    extern bool running(thread t) ASMLABEL("process.running");

    /*
     * process.wake(thread)
     *
     * Wakes up a sleeping thread.
     */
    extern void wake(thread t) ASMLABEL("process.wake");

    /*
     * process.exec(path, argv)
     *
     * Runs the executable at the provided path with the specified arguments,
     * and returns its exit code.
     */
    extern i32 exec(const_slice<i8> path, const_slice<const_slice<i8>> argv) ASMLABEL("process.exec");
}

namespace time {
    /*
     * time.seconds()
     *
     * Returns a clock time in seconds.
     */
    extern iword seconds() ASMLABEL("time.seconds");

    /*
     * time.millis()
     *
     * Returns a clock time in milliseconds.
     */
    extern iword millis() ASMLABEL("time.millis");

    /*
     * time.nanos()
     *
     * Returns a clock time in nanoseconds, guaranteed to be at least millisecond-precise and usually more.
     */
    extern iword nanos() ASMLABEL("time.nanos");

    /*
     * time.ticks()
     *
     * Returns a clock time in ticks, based on the most precise available clock on the target hardware.
     */
    extern iword ticks() ASMLABEL("time.ticks");
}

namespace atomics {
    /*
     * atomics.set_bit(word, index)
     *
     * Atomically sets bit in the specified word.
     */
    extern void set_bit(u64* word, u64 index) ASMLABEL("atomics.set_bit");

    /*
     * atomics.clear_bit(word, index)
     *
     * Atomically clears bit in the specified word.
     */
    extern void clear_bit(u64* word, u64 index) ASMLABEL("atomics.clear_bit");

    /*
     * atomics.test_set_bit(word, index)
     *
     * Atomically sets bit in the specified word, returns previous bit value.
     */
    extern bool test_set_bit(u64* word, u64 index) ASMLABEL("atomics.test_set_bit");

    /*
     * atomics.test_clear_bit(word, index)
     *
     * Atomically clears bit in the specified word.
     */
    extern bool test_clear_bit(u64* word, u64 index) ASMLABEL("atomics.test_clear_bit");

    /*
     * atomics.swap(ptr, input)
     *
     * Swaps the provided input with the word pointed to by ptr, and returns
     * the previous word.
     */
    extern u64 swap(u64* ptr, u64 input) ASMLABEL("atomics.swap");
}

#endif
