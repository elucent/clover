#include "clover/load.h"
#include "lib/io.h"
#include "clover/err.h"

ByteSource read_bytes(const i8* path) {
    fd file = file_open({path, cidx(path, 0)}, FP_READ);
    if (file < 0) fatal("Couldn't open file.");

    i8 buffer[65536];
    iptr length = 0, bytes;
    do {
        bytes = file_read(file, {buffer, 65536});
        length += bytes;
    } while (bytes > 0);

    file_close(file);
    file = file_open({path, cidx(path, 0)}, FP_READ);

    i8* persistent_buf = new i8[length + 1];
    iptr actually_read = file_read(file, {persistent_buf, length});
    assert(actually_read == length);
    file_close(file);
    if (persistent_buf[length - 1] != '\n') persistent_buf[length ++] = '\n';
    return ByteSource{length, persistent_buf};
}

inline iptr min(iptr a, iptr b) {
    return a < b ? a : b;
}

void advance_decoder(UnicodeBuf& buf, Module* mod) {
    amounts decoded = utf8_decode(mod->bytes.text + buf.byteidx, min(mod->bytes.length - buf.byteidx, UNICODE_BUFSIZE * sizeof(rune)), buf.buf, UNICODE_BUFSIZE);
    buf.length = decoded.runes;
    buf.byteidx += decoded.bytes;
    buf.idx = 0;
    if (auto err = unicode_error()) utf8_format_error(mod, err, buf.byteidx);
}