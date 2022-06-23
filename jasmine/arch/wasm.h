#ifndef BASIL_JASMINE_WASM_H
#define BASIL_JASMINE_WASM_H

#include "lib/buffer.h"
#include "lib/malloc.h"
#include "lib/io.h"

struct WasmContext;

struct WasmModule {
    const_slice<i8> name;
    arena modspace; // Module-specific arena.

    void save(stream& io);
    void load(stream& io);
};

struct WasmContext {
    arena genspace; // General-purpose arena.

    const_slice<i8> makestr(i32 n);
    const_slice<i8> makestr(i32 n, const char* init);

    void load(const_slice<i8> path);
};

namespace wasm {
    using labelidx = i64;
    using funcidx = i64;
    using localidx = i64;
    using globalidx = i64;
    using memidx = i64;
    using typeidx = i64;
    using tableidx = i64;
    using dataidx = i64;
    using name = const_slice<i8>;
    using reftype = i8;
    using valtype = i8;
    using resulttype = const_slice<valtype>;
    struct functype {
        resulttype params, results;
    };
    struct memtype {
        u32 low, high;
    };
    struct memarg {
        u32 align, offset;
    };

    constexpr valtype 
        T_I32 = 0x7f,
        T_I64 = 0x7e,
        T_F32 = 0x7d,
        T_F64 = 0x7c,
        T_VEC = 0x7b,
        T_FUNCREF = 0x70,
        T_EXTERNREF = 0x6f;
    
    using mut = u8;
    constexpr mut
        MUT_CONST = 0x00,
        MUT_VAR = 0x01;

    struct Wasm;

    void writeto(Wasm& buf);

    memidx memory(memtype t);

    globalidx global(valtype t, mut m);

    void wasm_unreachable();
    void nop();

    void beginfunc();
    localidx local(valtype t);
    funcidx endfunc();

    void start(funcidx i);

    funcidx importfunc(name mod, name n, typeidx t);
    // void importtable(name mod, name n, tabletype t)
    memidx importmem(name mod, name n, memtype t);
    globalidx importglobal(name mod, name n, mut m, valtype t);

    void exportfunc(name n, funcidx i);
    void exporttable(name n, tableidx i);
    void exportmem(name n, memtype t);
    void exportglobal(name n, globalidx i);

    void beginblock();
    void endblock();
    void beginloop();
    void endloop();
    void beginif();
    void endif();
    void beginifelse();
    void beginelse();
    void endelse();
    void br(labelidx l);
    void br_if(labelidx l);
    void ret();
    void call(funcidx i);
    void call_indirect(typeidx t, tableidx tbl);

    void refnull(reftype t);
    void refisnull();
    void reffunc(funcidx f);

    void drop();
    void select(const_slice<valtype> vals);

    void localget(localidx i);
    void localset(localidx i);
    void localtee(localidx i);
    void globalget(globalidx i);
    void globalset(globalidx i);

    void i32load(memarg m);
    void i64load(memarg m);
    void f32load(memarg m);
    void f64load(memarg m);
    void i32load8s(memarg m);
    void i32load8u(memarg m);
    void i32load16s(memarg m);
    void i32load16u(memarg m);
    void i64load8s(memarg m);
    void i64load8u(memarg m);
    void i64load16s(memarg m);
    void i64load16u(memarg m);
    void i64load32s(memarg m);
    void i64load32u(memarg m);
    void i32store(memarg m);
    void i64store(memarg m);
    void f32store(memarg m);
    void f64store(memarg m);
    void i32store8(memarg m);
    void i32store16(memarg m);
    void i64store8(memarg m);
    void i64store16(memarg m);
    void i64store32(memarg m);

    void memorysize();
    void memorygrow();
    void memoryinit(dataidx i);
    void datadrop(dataidx i);
    void memorycopy();
    void memoryfill();

    void beginexpr();
    void endexpr();

    void i32const(i32 i);
    void i64const(i64 i);
    void f32const(float f);
    void f64const(double f);

    void i32eqz();
    void i32eq();
    void i32ne();
    void i32lts();
    void i32ltu();
    void i32gts();
    void i32gtu();
    void i32les();
    void i32leu();
    void i32ges();
    void i32geu();

    void i64eqz();
    void i64eq();
    void i64ne();
    void i64lts();
    void i64ltu();
    void i64gts();
    void i64gtu();
    void i64les();
    void i64leu();
    void i64ges();
    void i64geu();

    void f32eq();
    void f32ne();
    void f32lt();
    void f32gt();
    void f32le();
    void f32ge();

    void f64eq();
    void f64ne();
    void f64lt();
    void f64gt();
    void f64le();
    void f64ge();

    void i32clz();
    void i32ctz();
    void i32popcnt();
    void i32add();
    void i32sub();
    void i32mul();
    void i32divs();
    void i32divu();
    void i32rems();
    void i32remu();
    void i32and();
    void i32or();
    void i32xor();
    void i32shl();
    void i32shrs();
    void i32shru();
    void i32rotl();
    void i32rotr();

    void i64clz();
    void i64ctz();
    void i64popcnt();
    void i64add();
    void i64sub();
    void i64mul();
    void i64divs();
    void i64divu();
    void i64rems();
    void i64remu();
    void i64and();
    void i64or();
    void i64xor();
    void i64shl();
    void i64shrs();
    void i64shru();
    void i64rotl();
    void i64rotr();

    void f32abs();
    void f32neg();
    void f32ceil();
    void f32floor();
    void f32trunc();
    void f32nearest();
    void f32sqrt();
    void f32add();
    void f32sub();
    void f32mul();
    void f32div();
    void f32min();
    void f32max();
    void f32copysign();

    void f64abs();
    void f64neg();
    void f64ceil();
    void f64floor();
    void f64trunc();
    void f64nearest();
    void f64sqrt();
    void f64add();
    void f64sub();
    void f64mul();
    void f64div();
    void f64min();
    void f64max();
    void f64copysign();

    void i32wrapi64();
    void i32truncf32s();
    void i32truncf32u();
    void i32truncf64s();
    void i32truncf64u();
    void i64extendi32s();
    void i64extendi64u();
    void i64truncf32s();
    void i64truncf32u();
    void i64truncf64s();
    void i64truncf64u();
    void f32converti32s();
    void f32converti32u();
    void f32converti64s();
    void f32converti64u();
    void f32demotef64();
    void f64converti32s();
    void f64converti32u();
    void f64converti64s();
    void f64converti64u();
    void f64promotef32();
    void i32reinterpretf32();
    void i64reinterpretf64();
    void f32reinterpreti32();
    void f64reinterpreti64();

    void i32extend8s();
    void i32extend16s();
    void i64extend8s();
    void i64extend16s();
    void i64extend32s();

    void i32truncsatf32s();
    void i32truncsatf32u();
    void i32truncsatf64s();
    void i32truncsatf64u();
    void i64truncsatf32s();
    void i64truncsatf32u();
    void i64truncsatf64s();
    void i64truncsatf64u();
}

#endif