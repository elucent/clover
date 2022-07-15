#include "core/sys.h"
#include "clover/clover.h"
#include "jasmine/jasmine.h"

i32 main(i32 argc, i8** argv) {
    JasmineObject o;
    
    // auto start = nanotime();
    // for (i32 i = 0; i < 1; i ++) {
    //     JasmineModule* obj = new(o.objspace) JasmineModule(Version(1, 0, 0), {"fib", 3});
    //     using namespace jasm;

    //     obj->data.def(
    //         T_I64,
    //         obj->strings.intern({ "a_constant", 10 }),
    //         42ll
    //     );
    //     obj->data.def(
    //         T_PTR,
    //         obj->strings.intern({ "a_pointer", 9 }),
    //         0ll
    //     );
    //     obj->stat.deftup(
    //         t_tuple(obj->types, tvec(obj->types, T_I64, T_F32)),
    //         obj->strings.intern({ "a_tuple", 7 }),
    //         1ll, 2.0f
    //     );
    //     obj->stat.defarr(
    //         t_array(obj->types, T_I32, 3),
    //         obj->strings.intern({ "an_array", 8 }),
    //         4, 5, 6
    //     );
    //     obj->stat.def(
    //         t_array(obj->types, T_I8, 12),
    //         obj->strings.intern({ "a_string", 8 }),
    //         const_slice<i8>{ "hello world!", 12 }
    //     );

    //     funcidx fib = beginfunc(*obj, obj->makestr(3, "fib"), t_fun(obj->types, T_I64, tvec(obj->types, T_I64)));
    //     labelidx bb0 = obj->funcs[fib]->label();
    //     localidx n = par(T_I64, 0);
    //     localidx cmp = lt(T_I64, reg(n), imm(2));
    //     jz(lbl(bb0), reg(cmp));
    //     ret(T_I64, reg(n));
    //     label(bb0);
    //     localidx nm1 = sub(T_I64, reg(n), imm(1));
    //     localidx a = begincall(obj->funcs[fib]->type, func(fib));
    //     callarg(reg(nm1));
    //     endcall();
    //     localidx nm2 = sub(T_I64, reg(n), imm(2));
    //     localidx b = begincall(obj->funcs[fib]->type, func(fib));
    //     callarg(reg(nm2));
    //     endcall();
    //     localidx sum = add(T_I64, reg(a), reg(b));
    //     ret(T_I64, reg(sum));
    //     endfunc();

    //     obj->meta.add(obj->makestr(7, "comment"), obj->makestr(41, "this is a test object constructed in code"));
        
    //     o.modules.put(obj->strings.strings[obj->meta.modname], obj);
    //     o.moduleseq.push(obj);
    // }

    // {
    //     JasmineModule* obj = new(o.objspace) JasmineModule(Version(1, 0, 0), {"loop", 4});
    //     using namespace jasm;

    //     funcidx loop = beginfunc(*obj, obj->makestr(4, "loop"), t_fun(obj->types, T_VOID, tvec(obj->types, T_I64)));
    //     labelidx bb0 = obj->funcs[loop]->label();
    //     labelidx bb1 = obj->funcs[loop]->label();
    //     localidx n = par(T_I64, 0);
    //     label(bb0);
    //     localidx np = phi(T_I64, reg(n), reg(5));
    //     localidx cmp = eq(T_I64, reg(np), imm(0));
    //     jnz(lbl(bb1), reg(cmp));
    //     localidx ndec = sub(T_I64, reg(np), imm(1));
    //     jump(lbl(bb0));
    //     label(bb1);
    //     ret(T_I64, reg(np));
    //     endfunc();
    //     o.modules.put(obj->strings.strings[obj->meta.modname], obj);
    //     o.moduleseq.push(obj);
    // }

    // stream* out = open("loop.o", FP_WRITE);
    // o.write(*out);
    // close(out);
    return jasmine_main(argc, argv);
}