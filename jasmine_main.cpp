#include "core/sys.h"
#include "jasmine/obj.h"

using namespace jasmine;

// #include "jasmine/arch/amd64.h"

// using Verifier = ASMVerifier<AMD64LinuxTarget>;
// using Printer = ASMPrinter<AMD64LinuxTarget>;
// using ASM = ASMCompose<Verifier, ASMCompose<Printer, AMD64LinuxTarget>>;

i32 main(i32 argc, i8** argv) {
    JasmineModule* mod = new JasmineModule(Version(0, 0, 1), const_slice<i8>{"test", 4});
    
    Function* fn = new Function(mod, t_fun(mod->types, T_I32, tvec(T_I32)), mod->strings.intern({"fib", 3}));
    {
        using namespace assembler;
        writeTo(*fn);
        auto arg = add(ARG, T_I32, Int(0));
        auto cmp = add(CLT, T_I32, Reg(arg), Int(2));
        auto ifLess = add(BRNZ, T_I32, Reg(cmp), Branch(), Branch());
        link(ifLess, 1);
        add(RET, T_I32, Reg(arg));
        auto minus1 = add(SUB, T_I32, Reg(arg), Int(1));
        auto call1 = add(CALL, t_fun(mod->types, T_I32, tvec(T_I32)), Func(fn->idx), Reg(minus1));
        auto minus2 = add(SUB, T_I32, Reg(arg), Int(2));
        auto call2 = add(CALL, t_fun(mod->types, T_I32, tvec(T_I32)), Func(fn->idx), Reg(minus2));
        auto sum = add(ADD, T_I32, Reg(call1), Reg(call2));
        add(RET, T_I32, Reg(sum));
    }

    fn->dumpDOT(file_stdout);

    return 0;
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
