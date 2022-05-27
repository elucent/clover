#ifndef BASIL_CLOVER_LOAD_H
#define BASIL_CLOVER_LOAD_H

#include "lib/utf.h"
#include "clover/clover.h"

#define UNICODE_BUFSIZE 16384

/*
 * UnicodeBuf
 *
 * Fixed-length buffer of codepoints that can be repeatedly replenished from a byte source.
 * UnicodeBuf basically functions as a window into a decoded source file, decoding a bit
 * at a time and continuing to the lexer after each decoding action.
 */
struct UnicodeBuf {
    i32 length = 0, idx = 0, byteidx = 0;
    rune buf[UNICODE_BUFSIZE];

    inline rune peek() const {
        return buf[idx];
    }

    inline rune read() {
        return buf[idx ++];
    }

    inline operator bool() const {
        return idx < length;
    }
};

/*
 * read_bytes(path)
 *
 * Attempts to open the file located at the provided path, and read all of its contents
 * into memory. On success, returns a ByteSource object with a pointer to the file's
 * loaded contents and the length of the loaded file in bytes. On failure, returns a
 * ByteSource with a length of zero and a null text pointer.
 */
ByteSource read_bytes(const i8* path);

/*
 * advance_decoder(buf)
 *
 * Reads up to UNICODE_BUFSIZE codepoints from the provided byte source. This overwrites
 * any existing codepoints in the buffer, and sets the buffer's length to the number
 * of codepoints decoded.
 */
void advance_decoder(UnicodeBuf& buf, Module* mod);

#endif