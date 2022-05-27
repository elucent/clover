# Basil Core

These files define the fundamental OS interface and basic data types upon which the rest of Basil and its compiler are defined.

## Types

| Type | Description |
|---|---|
|`i8`, `i16`, `i32`, `i64`| Fixed-width signed integer types. |
|`u8`, `u16`, `u32`, `u64`| Fixed-width unsigned integer types. |
|`uptr`| Unsigned integer type large enough to represent any memory address. |
|`iptr`| Signed integer type large enough to represent any difference in memory addresses. |
|`iword`| Machine-word-sized signed integer type. |
|`uword`| Machine-word-sized unsigned integer type. |

Generally speaking, we abide by the following guidelines:
 - Fixed-width integer types should be used for non-pointer structure members.
 - `uptr` and `iptr` should be used when dealing with pointers and pointer differences.
 - `uword` and `iword` are preferred for local and global variables where the represented range is not important.
 - Prefer the use of signed integers over unsigned.

## System Procedures

Generally shallow wrappers around OS system calls, the Basil core system procedures (defined primarily in `sys.h`) define the
API through which Basil programs and the compiler interface with the operating system. We try to keep this set of functions
small, so as to minimize the necessary features on any given system and make Basil as portable as possible. Most system
procedures are also defined pretty loosely, and can accommodate a variety of backing implementations as required by different
platforms.

#### Memory

| Symbol | Description |
|---|---|
| `PAGESIZE` | The size of a page in the Basil runtime, always equal to 4096 bytes. |
| `mmap(pages)` | Requests `pages` pages of memory from the operating system. Returns a `slice<page>`, containing a pointer to the desired number of pages on success, or with a null pointer in failure. |
| `munmap(pages)` | Tells the operating system to free the provided page slice. |
| `VM_READ`, `VM_WRITE`, `VM_EXEC` | Permission flags for protecting the readability, writability, and executability of a memory region. |
| `mpermit(pages, flags)` | Sets permissions of the provided page slice based on the bitset represented by `flags`. |

#### File System

| Symbol | Description |
|---|---|
| `FD_READ`, `FD_WRITE`, `FD_APPEND` | Permission flags determining the readability and writability of an opened file. |
| `fdopen(path, flags)` | Attempts to open a file at `path` with access permissions given by `flags`. Creates the file if it does not exist if `FD_WRITE` is set. Appends written bytes to the end of the file instead of overwriting it if `FD_APPEND` is set. Returns an abstract file descriptor for the opened file on success, or a negative file descriptor on failure. |
| `fdread(file, buf)` | Attempts to read bytes from the provided file up to the capacity of `buf`, returning the number of bytes read. |
| `fdwrite(file, buf)` | Attempts to write the entirety of `buf` to the file, returning the number of bytes written. |
| `fdclose(file)` | Closes the provided file. |

#### Processes

| Symbol | Description |
|---|---|
| `exit(code)` | Immediately exits the program with error code `code`. |
| `spawn(function, data)` | Starts a concurrent task that executes `function` on `data` and dies when the function returns. |
| `chopen()` | Creates a new channel on which tasks can communicate information. |
| `chrecv(chan)` | Waits for `chan` to have an available value, then returns it. |
| `chsend(chan, value)` | Sends a value to the provided channel. |
| `chclose(chan)` | Closes the provided channel. |