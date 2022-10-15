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
    types.compute_native_sizes<DefaultTarget>();
    PassInfo* info = makepassinfo();
    for (Function* func : funcs) {
        print(" === before opts === \n");
        func->format(stdout), print('\n');
        if (level >= OPT_1) {
            inlining(*func, *info);
            cfg(*func, *info);

            // print(" === after inline === \n");
            // func->format(stdout), print('\n');
            // foldc(*func, *info);

            // print(" === after foldc === \n");
            // func->format(stdout), print('\n');
            // dce(*func, *info);   

            // print(" === after dce === \n");
            // func->format(stdout), print('\n');
        }
        if (level >= OPT_1) regalloc<DefaultTarget>(*func, *info);
        else stackalloc<DefaultTarget>(*func, *info);
    }
    delete info;
}

void JasmineModule::compile(Assembly& as) {
    for (Function* func : funcs) {
        lower<DefaultTarget>(*func, as);
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