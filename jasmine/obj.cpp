#include "jasmine/obj.h"
#include "jasmine/pass.h"

void JasmineModule::write(bytebuf<arena>& buf) {
    bytebuf<arena> strbuf(&modspace), metabuf(&modspace), typebuf(&modspace), databuf(&modspace), statbuf(&modspace), codebuf(&modspace);
    strings.write(strbuf);
    meta.write(metabuf);
    types.write(typebuf);
    data.write(databuf);
    stat.write(statbuf);

    codebuf.writeULEB(funcs.size());
    for (auto& f : funcs) f->write(codebuf);

    u32 size = 16 + strbuf.size() + metabuf.size() + typebuf.size() + databuf.size() + statbuf.size() + codebuf.size();

    buf.write("#!jasmine\n\0\0", 12);
    buf.writeLE<u32>(size);
    while (strbuf.size()) buf.write(strbuf.read());
    while (metabuf.size()) buf.write(metabuf.read());
    while (typebuf.size()) buf.write(typebuf.read());
    while (databuf.size()) buf.write(databuf.read());
    while (statbuf.size()) buf.write(statbuf.read());
    while (codebuf.size()) buf.write(codebuf.read());
}

void JasmineModule::read(bytebuf<arena>& buf) {
    i8 magic[12];
    buf.read(magic, 12);
    u32 sz = buf.readLE<u32>();
    strings.read(buf);
    meta.read(buf);
    types.read(buf);
    data.read(buf);
    stat.read(buf);

    u32 nfuncs = buf.readULEB();
    for (u32 i = 0; i < nfuncs; i ++) {
        Function* f = new(modspace) Function(this);
        funcs.push(f);
        funcs.back()->read(buf);
    }
}

/*
23 21 6a 61 73 6d 69 6e 65 0a 00 00 f4 00 00 00 06 03 66 69 62 0a 61 5f 63 6f 6e 73 74 61 6e 74 09 61 5f 
70 6f 69 6e 74 65 72 07 61 5f 74 75 70 6c 65 08 61 6e 5f 61 72 72 61 79 08 61 5f 73 74 72 69 6e 67 01 00 
00 01 00 00 01 07 63 6f 6d 6d 65 6e 74 29 74 68 69 73 20 69 73 20 61 20 74 65 73 74 20 6f 62 6a 65 63 74 
20 63 6f 6e 73 74 72 75 63 74 65 64 20 69 6e 20 63 6f 64 65 04 01 02 7c 79 00 7d 03 00 7f 0c 02 7c 01 7c 
02 01 7c 2a 02 7a 00 03 03 00 01 00 00 00 40 04 01 04 05 06 05 02 e8 00 e5 00 ec 00 ec 00 ef 00 20 f7 00 
ef 00 f2 00 ec 00 e4 00 21 01 00 03 00 0b 0c 7c 20 00 21 7c 12 00 02 23 7f 51 00 01 0b 7c 10 00 0e 7a 50 
00 12 7c 12 00 01 25 03 67 00 01 10 05 12 7c 12 00 02 25 03 67 00 01 10 07 11 7c 11 06 08 0b 7c 10 09 
*/

void JasmineModule::formatshort(stream& io) {
    ::write(io, strings.strings[meta.modname], ' ', meta.ver);
}

void JasmineModule::format(stream& io) {
    meta.format(io);
    strings.format(io);
    types.format(io);
    data.format(io);
    stat.format(io);

    ::write(io, "=== Function Table ===\n");
    for (const auto& func : funcs) func->format(io);
}

void JasmineModule::opt(OptLevel level) {
    for (Function* func : funcs) {
        if (level >= OPT_1) {
            foldc(*func);
            dce(*func);   
        }
    }
}

void JasmineModule::target(const Target& arch, OptLevel level) {
    types.compute_native_sizes(arch);
    for (Function* func : funcs) {
        if (level >= OPT_1) regalloc(*func, arch);
        else stackalloc(*func, arch);
    }
}

void JasmineObject::write(bytebuf<arena>& buf) {
    for (const auto& e : modules) e.value->write(buf);
}

void JasmineObject::read(bytebuf<arena>& buf) {
    while (buf.size()) {
        JasmineModule* m = new(objspace) JasmineModule(buf);
        modules.put(m->strings.strings[m->meta.modname], m);
        moduleseq.push(m);
    }
}

void JasmineObject::write(stream& io) {
    bytebuf<arena> buf(&objspace);
    write(buf);
    if (buf._start > buf._end) {
        ::write(io, const_slice<i8>(buf._data + buf._start, buf._capacity - buf._start));
        ::write(io, const_slice<i8>(buf._data, buf._end));
    }
    else ::write(io, const_slice<i8>{buf._data + buf._start, buf._end - buf._start});
}

void JasmineObject::read(stream& io) {
    i8 buf[65536];
    slice<i8> dest = {buf, 65536};
    bytebuf<arena> bytes(&objspace, 65536);

    while (iptr amt = fdread(io.fd, dest)) bytes.write(buf, amt);
    read(bytes);
}

void JasmineObject::format(stream& io) {
    for (const auto& mod : modules) mod.value->format(io);
}