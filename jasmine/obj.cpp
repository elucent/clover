#include "jasmine/obj.h"
#include "jasmine/pass.h"
#include "jasmine/pass_target.h"

MODULE(jasmine)

void JasmineModule::write(bytebuf<>& buf) {
    bytebuf<> strbuf, metabuf, typebuf, databuf, statbuf, codebuf;
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

void JasmineModule::read(bytebuf<>& buf) {
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
        Function* f = new Function(this, 0, 0);
        funcs.push(f);
        funcs.back()->read(buf);
    }
    format(io_stdout);
}

void JasmineModule::formatshort(fd io) {
    ::write(io, strings.strings[meta.modname], ' ', meta.ver);
}

void JasmineModule::format(fd io) {
    meta.format(io);
    strings.format(io);
    types.format(io);
    data.format(io);
    stat.format(io);

    ::write(io, " === Function Table === \n");
    for (const auto& func : funcs) func->format(io);
}

void JasmineModule::dumpDOT(fd io) {
    for (Function* fn : funcs) {
        fn->dumpDOT(io);
    }
}

void JasmineModule::dumpDOT(fd io, PassInfo& info) {
    for (Function* fn : funcs) {
        fn->dumpDOT(io, info);
    }
}

void JasmineModule::opt(PassInfo& info, OptLevel level) {
    types.compute_native_sizes<DefaultTarget>();
    for (Function* func : funcs) {
        cfg(info, *func);
        dce(info, *func);
        if (level >= OPT_1) {
            // inlining(*func, *info);

            // print(" === after inline === \n");
            // func->format(stdout), print('\n');
            // foldc(*func, *info);

            // print(" === after foldc === \n");
            // func->format(stdout), print('\n');
            // dce(*func, *info);   

            // print(" === after dce === \n");
            // func->format(stdout), print('\n');
        }
        renumber(info, *func); // Regularize node indices for the control flow graph.
        live(info, *func);
        irg(info, *func);
        regalloc<DefaultTarget>(info, *func); // Allocate registers to variables.
        // pie(info, *func); // Eliminate phi nodes.
        print(" === jasmine IR === \n");
        func->format(io_stdout), print('\n');
    }
}

void JasmineModule::compile(PassInfo& info, Assembly& as) {
    print(" === asm-level IR === \n");
    ASMPrinter<DefaultTarget>::write_to(io_stdout);
    for (Function* func : funcs) {
        lower<ASMPrinter<DefaultTarget>>(info, *func, as);
        lower<DefaultTarget>(info, *func, as);
    }
    println();

    print(" === machine code === \n");
    auto codesize = as.code.size();
    LinkedAssembly linkasm = as.link();
    for (i32 i = 0; i < codesize; i ++)
        write_hex(io_stdout, (u8)linkasm.code[i]), print(' ');
    println();
    println();
}

void JasmineObject::write(bytebuf<>& buf) {
    for (const auto& e : modules) e.value->write(buf);
}

void JasmineObject::read(bytebuf<>& buf) {
    while (buf.size()) {
        JasmineModule* m = new JasmineModule(buf);
        modules.put(m->strings.strings[m->meta.modname], m);
        moduleseq.push(m);
    }
}

void JasmineObject::write(fd io) {
    bytebuf<> buf;
    write(buf);
    if (buf._start > buf._end) {
        ::write(io, const_slice<i8>(buf._data + buf._start, buf._capacity - buf._start));
        ::write(io, const_slice<i8>(buf._data, buf._end));
    }
    else ::write(io, const_slice<i8>{buf._data + buf._start, buf._end - buf._start});
}

void JasmineObject::read(fd io) {
    i8 buf[65536];
    slice<i8> dest = {buf, 65536};
    bytebuf<> bytes;

    while (iptr amt = file_read(io, dest)) bytes.write(buf, amt);
    read(bytes);
}

void JasmineObject::format(fd io) {
    for (const auto& mod : modules) mod.value->format(io);
}

ENDMODULE()
