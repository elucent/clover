#include "core/sys.h"
#include "clover/clover.h"
#include "jasmine/jasmine.h"

#include "jasmine/arch/amd64.h"

using Verifier = ASMVerifier<AMD64LinuxTarget>;
using Printer = ASMPrinter<AMD64LinuxTarget>;
using ASM = ASMCompose<Verifier, ASMCompose<Printer, AMD64LinuxTarget>>;

i32 main(i32 argc, i8** argv) {
    return jasmine_main(argc, argv);
    // Printer::write_to(stdout);
    // SymbolTable syms;

    // Assembly assembly(&syms);
    // auto func_label = syms.intern(6, "floats");
    // ASM::global(assembly, func_label);
    //     ASM::fmov32(assembly, MVal::fpreg(ASM::XMM0), MVal::f32(0.1f));
    //     ASM::fmov32(assembly, MVal::fpreg(ASM::XMM1), MVal::f32(0.2f));
    //     ASM::fadd32(assembly, MVal::fpreg(ASM::XMM2), MVal::fpreg(ASM::XMM0), MVal::fpreg(ASM::XMM1));
    //     ASM::fmov32(assembly, MVal::fpreg(ASM::XMM0), MVal::fpreg(ASM::XMM2));
    //     ASM::ret(assembly);
    
    // auto code_size = assembly.code.size();
    // LinkedAssembly linked = assembly.link();
    // linked.load();

    // // print("code size = ", code_size, '\n');
    // // for (iptr i = 0; i < code_size; i ++) {
    // //     write_hex(stdout, (u8)linked.code[i]);
    // //     print(' ');
    // // }
    // // print('\n');

    // auto foo = linked.lookup<float()>(func_label);
    // print("func returned ", foo(), "\n");
    // return 0;
}
