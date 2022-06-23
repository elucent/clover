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
 * mreq(pages)
 * 
 * Requests the provided number of memory pages from the operating system
 * and returns a pointer to them. Returns 0 if the request fails.
 */
extern "C" slice<page> mreq(iptr pages);

/*
 * mfree(pages)
 *
 * Releases the provided contiguous block of pages from this process. 
 */
extern "C" void mfree(slice<page> pages);

enum VMFLAGS {
    VM_READ = 1,    // Specifies that the memory pages should be readable.
    VM_WRITE = 2,   // Specifies that the memory pages should be writable.
    VM_EXEC = 4     // Specifies that the memory pages should be executable.
};

/*
 * mpermit(pages)
 *
 * Sets the permissions for the provided contiguous block of pages.
 */
extern "C" void mpermit(slice<page> pages, iptr flags);

enum FDFLAGS {
    FP_READ = 1,    // Specifies that the file should be readable.
    FP_WRITE = 2,   // Specifies that the file should be writable.
    FP_APPEND = 4   // Specifies that the file should be appended to, not overwritten.
};

/*
 * fdopen(path, flags)
 *
 * Attempts to open a file at the desired path with the given flags. Flags
 * may be any combination of the bits specified in the FDFLAGS enum, but at least
 * one of FP_READ or FP_WRITE must be set. If FP_WRITE is set, a file will be created
 * if it does not exist.
 * Returns -1 if the file could not be opened.
 */
extern "C" fd fdopen(const i8* path, iptr flags);

/*
 * fdread(file, buf)
 *
 * Attempts to read bytes from the file up to the size of the provided buffer. Returns
 * the number of bytes that were read.
 */
extern "C" iptr fdread(fd file, slice<i8> buf);

/*
 * fdwrite(file, buf)
 *
 * Attempts to write the provided buffer's contents into the file. Returns the number
 * of bytes that were written.
 */
extern "C" iptr fdwrite(fd file, const_slice<i8> buf);

/*
 * fdclose(file)
 *
 * Closes the file associated with the given file descriptor. 
 */
extern "C" void fdclose(fd file);

enum IPFLAGS {
    IP_V4 = 0x00,       // Specifies that the socket should use the IPv4 protocol.
    IP_V6 = 0x01,       // Specifies that the socket should use the IPv6 protocol.
    IP_STREAM = 0x00,   // Creates a stream socket.
    IP_DGRAM = 0x10,    // Creates a datagram socket.
    IP_RAW = 0x20       // Creates a raw socket.
};

struct netaddr {
    iptr hi, low;       // Both of these are used to encode a single IPv6 address,
};                      // while IPv4 addresses are encoded using only 'low'.

typedef u16 netport;

/*
 * nwbind(port, flags)
 *
 * Attempts to bind a server socket to a local port with properties given by the 
 * provided flags. Flags are comprised of a protocol and a socket type, specified
 * by ORing two flags together.
 * Returns -1 if the socket could not be opened.
 */
extern "C" fd nwbind(netport port, iptr queue, iptr flags);

/*
 * nwconn(addr, port, flags)
 *
 * Attempts to open a socket connecting to the provided address and port with properties 
 * given by the provided flags. Flags are comprised of a protocol and a socket type, 
 * specified by ORing two flags together.
 * Returns -1 if the socket could not be opened.
 */
extern "C" fd nwconn(netaddr addr, netport port, iptr flags);

struct accepted {
    netaddr addr;
    fd acceptedfd;
};

/*
 * nwaccept(socket)
 *
 * Accepts the next pending connection on the provided socket, and returns its
 * address and file descriptor.
 * Returns -1 if there are no pending connections or if an error occurs.
 */
extern "C" accepted nwaccept(fd socket);

/*
 * nwclose(socket)
 *
 * Closes the provided socket.
 */
extern "C" void nwclose(fd socket);

/*
 * pexit(code)
 *
 * Exits the program with the given exit code.
 */
extern "C" void pexit(iptr code);

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
 * nanotime()
 *
 * Returns a clock time in nanoseconds, guaranteed to be at least millisecond-precise and usually more.
 */
extern "C" iptr nanotime();

/*
 * plock(ptr)
 *
 * Locks the value pointed to by ptr.
 */

extern "C" void plock(iptr* ptr);

/*
 * punlock(ptr)
 *
 * Unlocks the value pointed to by ptr.
 */
extern "C" void punlock(iptr* ptr);

/*
 * sysrun(path, args)
 *
 * Executes the provided command. Returns the command's exit code.
 */
extern "C" iptr sysrun(const i8* cmd);

extern "C" void __cxa_pure_virtual();

#endif