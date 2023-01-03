#ifndef BASIL_JASMINE_TARGET_H
#define BASIL_JASMINE_TARGET_H

#include "lib/buffer.h"
#include "lib/io.h"
#include "lib/tuple.h"
#include "jasmine/type.h"
#include "jasmine/tab.h"

MODULE(jasmine)

enum Arch : u8 {
    ARCH_X86,
    ARCH_AMD64,
    ARCH_ARM64,
    ARCH_WASM
};

constexpr const i8* ARCH_NAMES[] = {
    "x86", "amd64", "arm64", "wasm"
};

enum OS : u8 {
    OS_WINDOWS,
    OS_OSX,
    OS_LINUX,
    OS_BSD,
    OS_WASI
};

constexpr const i8* OS_NAMES[] = {
    "windows", "osx", "linux", "bsd", "wasi"
};

struct TargetDesc {
    OS os;
    Arch arch;
    inline TargetDesc(OS os_in, Arch arch_in): os(os_in), arch(arch_in) {}

    inline bool operator==(const TargetDesc& other) const {
        return arch == other.arch && os == other.os;
    }

    inline bool operator!=(const TargetDesc& other) const {
        return !(*this == other);
    }
};

using mreg = i8;   // Generic type of machine register.

inline void write_impl(fd io, const TargetDesc& target) {
    ::write(io, OS_NAMES[target.os], '-', ARCH_NAMES[target.arch]);
}

struct Function;
struct TypeTable;

struct RegSet {
    u64 regs;

    inline RegSet(): 
        regs(0) {}

    inline void add(mreg r) {
        regs |= u64(1) << r;
    }

    inline void remove(mreg r) {
        regs &= ~(u64(1) << r);
    }

    inline bool operator[](mreg r) const {
        return regs & u64(1) << r;
    }

    inline operator bool() const {
        return regs;
    }

    inline mreg next() const {
        return __builtin_ctz(regs);
    }
};

struct Binding {
    enum Kind : u8 {
        NONE, GP, FP, STACK
    };

    union {
        struct { Kind kind; mreg reg; };
        struct { Kind pad; i32 offset : 24; };
    };

    inline static Binding none() {
        Binding b;
        b.kind = NONE;
        return b;
    }

    inline static Binding gp(mreg reg) {
        Binding b;
        b.kind = GP;
        b.reg = reg;
        return b;
    }

    inline static Binding fp(mreg reg) {
        Binding b;
        b.kind = FP;
        b.reg = reg;
        return b;
    }

    inline static Binding stack(i32 offset) {
        Binding b;
        b.kind = STACK;
        b.offset = offset;
        return b;
    }
};

struct CallBindings {
    vec<Binding, 6> bindings;
    vec<i32, 6> param_indices;

    inline const_slice<Binding> param(i32 i) const {
        i32 idx = param_indices[i];
        i32 next = i >= i32(param_indices.size()) - 1 ? bindings.size() : param_indices[i + 1];
        return {&bindings[idx], next - idx};
    }

    inline const_slice<Binding> ret() const {
        return { &bindings[0], param_indices.size() > 0 ? param_indices[0] : bindings.size() };
    }

    inline void place_ret(Binding binding) {
        assert(bindings.size() == 0);
        bindings.push(binding);
    }

    inline void place_ret(Binding first, Binding second) {
        assert(bindings.size() == 0);
        bindings.push(first);
        bindings.push(second);
    }

    inline void add_param(Binding binding) {
        param_indices.push(bindings.size());
        bindings.push(binding);
    }

    inline void add_param(Binding first, Binding second) {
        param_indices.push(bindings.size());
        bindings.push(first);
        bindings.push(second);
    }
};

// Machine-level value.
union ASMVal {
    double dval;
    u64 uval;
    
    enum Kind : u16 { 
        F64 = 0xfff0,
        GP  = 0xfff9,
        FP  = 0xfffa,
        IMM = 0xfffb,
        F32 = 0xfffc,
        MEM = 0xfffd
    };

    enum MemKind : u8 {
        REG_OFFSET = 1,
        FUNC_LABEL = 2,
        LOCAL_LABEL = 3,
        DATA_LABEL = 4,
        STATIC_LABEL = 5
    };

    struct {
        union {
            i32 imm;
            float f32;
            mreg gp;
            mreg fp;
            i32 offset;
            i32 sym;
        };
        MemKind memkind; 
        mreg base;
        Kind kind;
    } payload;

    inline static ASMVal fp(mreg r) {
        ASMVal m;
        m.uval = 0;
        m.payload.kind = FP;
        m.payload.base = m.payload.memkind = MemKind(0);
        m.payload.fp = r;
        return m;
    }

    inline static ASMVal gp(mreg r) {
        ASMVal m;
        m.uval = 0;
        m.payload.kind = GP;
        m.payload.base = m.payload.memkind = MemKind(0);
        m.payload.gp = r;
        return m;
    }

    inline static ASMVal imm(i32 imm) {
        ASMVal m;
        m.uval = 0;
        m.payload.kind = IMM;
        m.payload.base = m.payload.memkind = MemKind(0);
        m.payload.imm = imm;
        return m;
    }

    inline static ASMVal f32(float imm) {
        ASMVal m;
        m.uval = 0;
        m.payload.kind = F32;
        m.payload.base = m.payload.memkind = MemKind(0);
        m.payload.f32 = imm;
        return m;
    }

    inline static ASMVal f64(double imm) {
        ASMVal m;
        if (*(u64*)&imm > 0xfff8000000000000ul) // Purify NaNs
            *(u64*)&imm = 0xfff8000000000000ul;
        m.dval = imm;
        return m;
    }

    inline static ASMVal mem(mreg base, i32 offset) {
        ASMVal m;
        m.uval = 0;
        m.payload.kind = MEM;
        m.payload.memkind = REG_OFFSET;
        m.payload.base = base;
        m.payload.offset = offset;
        return m;
    }

    inline static ASMVal func(stridx sym) {
        ASMVal m;
        m.uval = 0;
        m.payload.kind = MEM;
        m.payload.memkind = FUNC_LABEL;
        m.payload.base = 0;
        m.payload.sym = sym;
        return m;
    }

    inline static ASMVal label(stridx sym) {
        ASMVal m;
        m.uval = 0;
        m.payload.kind = MEM;
        m.payload.memkind = LOCAL_LABEL;
        m.payload.base = 0;
        m.payload.sym = sym;
        return m;
    }

    inline static ASMVal data(stridx sym) {
        ASMVal m;
        m.uval = 0;
        m.payload.kind = MEM;
        m.payload.memkind = DATA_LABEL;
        m.payload.base = 0;
        m.payload.sym = sym;
        return m;
    }

    inline static ASMVal stat(stridx sym) {
        ASMVal m;
        m.uval = 0;
        m.payload.kind = MEM;
        m.payload.memkind = STATIC_LABEL;
        m.payload.base = 0;
        m.payload.sym = sym;
        return m;
    }

    inline Kind kind() const {
        if (uval <= 0xfff8000000000000ul) return F64;
        else return payload.kind;
    }

    inline bool operator==(ASMVal other) const {
        return uval == other.uval;
    }
};

enum Section : u8 {
    CODE_SECTION, DATA_SECTION, STATIC_SECTION
};

// Abstract representation of a symbol location.
struct Def {

    i32 offset;
    stridx sym;
    Section section;

    inline Def(Section section_in, i32 offset_in, stridx sym_in):
        offset(offset_in), sym(sym_in), section(section_in) {}
};

// Abstract representation of a relocation.
struct Reloc : public Def {
    enum Kind : u8 {
        REL8, 
        REL16_LE, REL32_LE, REL64_LE,
        REL16_BE, REL32_BE, REL64_BE
    };

    Kind kind;

    inline Reloc(Section section_in, Kind kind_in, i32 offset_in, stridx sym_in):
        Def(section_in, offset_in, sym_in), kind(kind_in) {}
};

// Unified buffer representing fully-linked code.
struct LinkedAssembly {
    slice<page> pages;
    i8 *code, *data, *stat;
    map<stridx, iptr> defs;

    void load();

    inline void unload() {
        memory_free(pages);
    }

    template<typename T>
    T* lookup(stridx sym) {
        auto it = defs.find(sym);
        if (it == defs.end())
            fatal("Undefined symbol!");
        return (T*)it->value;
    }
};

// Collection of buffers for target-specific code.
struct Assembly {
    bytebuf<> code, data, stat;
    vec<Def, 16> defs;
    vec<Reloc, 16> relocs;
    SymbolTable* symtab;

    inline Assembly(SymbolTable* symtab_in): 
        symtab(symtab_in) {}

    inline void def(Section section, stridx sym) {
        bytebuf<>* ptr;
        switch (section) {
            case CODE_SECTION: ptr = &code; break;
            case DATA_SECTION: ptr = &data; break;
            case STATIC_SECTION: ptr = &stat; break;
        }
        defs.push(Def(section, ptr->size(), sym));
    }

    inline void ref(Section section, Reloc::Kind kind, stridx sym) {
        bytebuf<>* ptr;
        switch (section) {
            case CODE_SECTION: ptr = &code; break;
            case DATA_SECTION: ptr = &data; break;
            case STATIC_SECTION: ptr = &stat; break;
        }
        relocs.push(Reloc(section, kind, ptr->size(), sym));
    }

    LinkedAssembly link();
};

enum Condition {
    COND_EQ, COND_NEQ, COND_LT, COND_LEQ, COND_GT, COND_GEQ, COND_ABOVE, COND_AEQ, COND_BELOW, COND_BEQ
};

enum FloatCondition {
    FCOND_EQ, FCOND_NEQ, FCOND_LT, FCOND_LEQ, FCOND_GT, FCOND_GEQ
};

constexpr static const i8* ASM_CONDITION_NAMES[] = {
    "eq", "neq", "lt", "leq", "gt", "geq", "a", "aeq", "b", "beq"
};

enum class Size {
    BITS8 = 0, BITS16 = 1, BITS32 = 2, BITS64 = 3,
    BYTE = BITS8, 
    WORD = BITS16, DWORD = BITS32, QWORD = BITS64,
    HALF = BITS16, INT = BITS32, LONG = BITS64,
    FLOAT32 = 4, FLOAT64 = 5, FLOAT = FLOAT32, DOUBLE = FLOAT64,
    MEMORY = 6, OTHER = 7
};

constexpr static const i8* ASM_SIZE_NAMES[] = {
    "8", "16", "32", "64", "f32", "f64", "", ""
};

ENDMODULE()

inline void write_impl(fd io, jasmine::Size size) {
    write_impl(io, jasmine::ASM_SIZE_NAMES[(unsigned)size]);
}

MODULE(jasmine)

enum class ASMOpcode {
    ADD8, ADD16, ADD32, ADD64,
    SUB8, SUB16, SUB32, SUB64,
    MUL8, MUL16, MUL32, MUL64,
    SDIV8, SDIV16, SDIV32, SDIV64,
    UDIV8, UDIV16, UDIV32, UDIV64,
    SREM8, SREM16, SREM32, SREM64,
    UREM8, UREM16, UREM32, UREM64,
    NEG8, NEG16, NEG32, NEG64,

    FADD32, FADD64,
    FSUB32, FSUB64,
    FMUL32, FMUL64,
    FDIV32, FDIV64,
    FREM32, FREM64,
    FNEG32, FNEG64,
    FSQRT32, FSQRT64,
    FMIN32, FMIN64,
    FMAX32, FMAX64,
    FROUND32, FROUND64,
    FFLOOR32, FFLOOR64,
    FCEIL32, FCEIL64,
    FABS32, FABS64,

    AND8, AND16, AND32, AND64,
    XOR8, XOR16, XOR32, XOR64,
    OR8, OR16, OR32, OR64,
    NOT8, NOT16, NOT32, NOT64,
    SHL8, SHL16, SHL32, SHL64,
    SHR8, SHR16, SHR32, SHR64,
    SAR8, SAR16, SAR32, SAR64,
    ROL8, ROL16, ROL32, ROL64,
    ROR8, ROR16, ROR32, ROR64,
    BITC8, BITC16, BITC32, BITC64,
    LZC8, LZC16, LZC32, LZC64,
    TZC8, TZC16, TZC32, TZC64,

    ISZ, ISNZ,
    CCC8, CCC16, CCC32, CCC64,
    CFCC32, CFCC64,

    PUSH8, PUSH16, PUSH32, PUSH64,
    POP8, POP16, POP32, POP64,
    FPUSH32, FPUSH64,
    FPOP32, FPOP64,
    MOV8, MOV16, MOV32, MOV64,
    FMOV32, FMOV64,
    LD8, LD16, LD32, LD64,
    ST8, ST16, ST32, ST64,
    LDI8, LDI16, LDI32, LDI64,
    LDAI8, LDAI16, LDAI32, LDAI64,
    STI8, STI16, STI32, STI64,
    FLD32, FLD64,
    FST32, FST64,
    FLDI32, FLDI64,
    FSTI32, FSTI64,
    LDA, LDC,

    J, JZ, JNZ,
    JCC8, JCC16, JCC32, JCC64,
    JFCC8, JFCC16, JFCC32, JFCC64,

    ENTER, LEAVE,
    STACK, ALLOCA, UNSTACK,
    CALL, RET,

    SXT8, SXT16, SXT32,
    ZXT8, ZXT16, ZXT32,
    I8TOF32, I16TOF32, I32TOF32, I64TOF32,
    I8TOF64, I16TOF64, I32TOF64, I64TOF64,
    F32TOI8, F32TOI16, F32TOI32, F32TOI64,
    F64TOI8, F64TOI16, F64TOI32, F64TOI64,
    F32TOF64,
    F64TOF32,
    F32FROMBITS, F64FROMBITS,
    F32TOBITS, F64TOBITS,

    MCPY, MMOV, MSET, MCMP
};

constexpr static const i8* ASM_OPCODE_NAMES[] = {
    "add8", "add16", "add32", "add64",
    "sub8", "sub16", "sub32", "sub64",
    "mul8", "mul16", "mul32", "mul64",
    "sdiv8", "sdiv16", "sdiv32", "sdiv64",
    "udiv8", "udiv16", "udiv32", "udiv64",
    "srem8", "srem16", "srem32", "srem64",
    "urem8", "urem16", "urem32", "urem64",
    "neg8", "neg16", "neg32", "neg64",

    "fadd32", "fadd64",
    "fsub32", "fsub64",
    "fmul32", "fmul64",
    "fdiv32", "fdiv64",
    "frem32", "frem64",
    "fneg32", "fneg64",
    "fsqrt32", "fsqrt64",
    "fmin32", "fmin64",
    "fmax32", "fmax64",
    "fround32", "fround64",
    "ffloor32", "ffloor64",
    "fceil32", "fceil64",
    "fabs32", "fabs64",

    "and8", "and16", "and32", "and64",
    "xor8", "xor16", "xor32", "xor64",
    "or8", "or16", "or32", "or64",
    "not8", "not16", "not32", "not64",
    "shl8", "shl16", "shl32", "shl64",
    "shr8", "shr16", "shr32", "shr64",
    "sar8", "sar16", "sar32", "sar64",
    "rol8", "rol16", "rol32", "rol64",
    "ror8", "ror16", "ror32", "ror64",
    "bitc8", "bitc16", "bitc32", "bitc64",
    "lzc8", "lzc16", "lzc32", "lzc64",
    "tzc8", "tzc16", "tzc32", "tzc64",

    "isz", "isnz",
    "ccc8", "ccc16", "ccc32", "ccc64",
    "fccc32", "fccc64",

    "push8", "push16", "push32", "push64",
    "pop8", "pop16", "pop32", "pop64",
    "fpush32", "fpush64",
    "fpop32", "fpop64",
    "mov8", "mov16", "mov32", "mov64",
    "fmov32", "fmov64",
    "ld8", "ld16", "ld32", "ld64",
    "st8", "st16", "st32", "st64",
    "ldi8", "ldi16", "ldi32", "ldi64",
    "ldai8", "ldai16", "ldai32", "ldai64",
    "sti8", "sti16", "sti32", "sti64",
    "fld32", "fld64",
    "fst32", "fst64",
    "fldi32", "fldi64",
    "fsti32", "fsti64",
    "lda", "ldc",

    "j", "jz", "jnz",
    "jcc8", "jcc16", "jcc32", "jcc64",
    "fjcc8", "fjcc16", "fjcc32", "fjcc64",

    "enter", "leave",
    "stack", "alloca", "unstack",
    "call", "ret",

    "sxt8", "sxt16", "sxt32",
    "zxt8", "zxt16", "zxt32",
    "i8tof32", "i16tof32", "i32tof32", "i64tof32",
    "i8tof64", "i16tof64", "i32tof64", "i64tof64",
    "f32toi8", "f32toi16", "f32toi32", "f32toi64",
    "f64toi8", "f64toi16", "f64toi32", "f64toi64",
    "f32tof64",
    "f64tof32",
    "f32frombits", "f64frombits",
    "f32tobits", "f64tobits",

    "mcpy", "mmov", "mset", "mcmp"
};

constexpr static const i8* ASM_REGISTER_NAMES[] = {
    "r0", "r1", "r2", "r3", "r4", "r5", "r6", "r7",
    "r8", "r9", "r10", "r11", "r12", "r13", "r14", "r15",
    "r16", "r17", "r18", "r19", "r20", "r21", "r22", "r23",
    "r24", "r25", "r26", "r27", "r28", "r29", "r30", "r31",
    "f0", "f1", "f2", "f3", "f4", "f5", "f6", "f7",
    "f8", "f9", "f10", "f11", "f12", "f13", "f14", "f15",
    "f16", "f17", "f18", "f19", "f20", "f21", "f22", "f23",
    "f24", "f25", "f26", "f27", "f28", "f29", "f30", "f31",
};

struct Insn;

// Not a real target, represents the abstract target properties of the virtual instruction set. Implements calling convention
// and the register set for an abstract machine of 32 general-purpose and 32 floating-point registers. Can be used for platform-independent
// instruction set purposes, such as validation or formatting.
struct FakeTarget {
    constexpr static mreg GPREGS[] = { 
        0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15,
        16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31
    };
    constexpr static mreg FPREGS[] = { 
        32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 
        48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, 61, 62, 63
    };

    constexpr static mreg fp = 30;
    constexpr static mreg sp = 31;

    static inline const_slice<mreg> gps() {
        return const_slice<mreg>{GPREGS, 32};
    }

    static inline const_slice<mreg> fps() {
        return const_slice<mreg>{FPREGS, 32};
    }

    static inline bool is_gp(mreg r) {
        return r < 32;
    }

    static inline bool is_fp(mreg r) {
        return r >= 32 && r < 64;
    }

    static inline const_slice<i8> reg_name(mreg r) {
        return const_slice<i8>{ ASM_REGISTER_NAMES[r], cidx(ASM_REGISTER_NAMES[r], '\0') };
    }

    static inline u32 primsize(typeidx t) {
        return 0;
    }

    static inline u32 primalign(typeidx t) {
        return 0;
    }

    static inline mreg hint(const Insn& insn, typeidx t) {
        return -1;
    }

    static inline RegSet clobbers(ASMOpcode opcode) {
        return RegSet();
    }

    static inline void place_call(CallBindings& call, const TypeTable& typetab, typeidx fntype) {
        //
    }

    static inline Size word_size() {
        return Size::BITS64;
    }

    static inline Size ptr_size() {
        return Size::BITS64;
    }
};

// Used to mock up an existing backend, but instead write the instructions as textual representation to an output stream.
// You can use this as an example of the static interface of backend target types.
template<typename Target = FakeTarget>
struct ASMPrinter : public Target {
    static void write_mval(fd io, Assembly& as, const ASMVal& val) {
        switch (val.kind()) {
            case ASMVal::FP:
                ::write(io, Target::reg_name(val.payload.fp));
                break;
            case ASMVal::GP:
                ::write(io, Target::reg_name(val.payload.gp));
                break;
            case ASMVal::IMM:
                ::write(io, val.payload.imm);
                break;
            case ASMVal::F32:
                ::write(io, val.payload.f32);
                break;
            case ASMVal::F64:
                ::write(io, val.dval);
                break;
            case ASMVal::MEM: switch (val.payload.memkind) {
                case ASMVal::REG_OFFSET:
                    ::write(io, '[', Target::reg_name(val.payload.base), val.payload.offset < 0 ? " - " : " + ", val.payload.offset < 0 ? -val.payload.offset : val.payload.offset, ']');
                    break;
                case ASMVal::FUNC_LABEL:
                    ::write(io, "func ", as.symtab->str(val.payload.sym));
                    break;
                case ASMVal::LOCAL_LABEL:
                    ::write(io, '.', as.symtab->str(val.payload.sym));
                    break;
                case ASMVal::DATA_LABEL:
                    ::write(io, "data ", as.symtab->str(val.payload.sym));
                    break;
                case ASMVal::STATIC_LABEL:
                    ::write(io, "static ", as.symtab->str(val.payload.sym));
                    break;
            }
            break;
        }
    }

    static void write_label(fd io, Assembly& as, const i8* prefix, stridx label) {
        ::write(io, prefix, as.symtab->str(label), ":\n");
    }

    static void write_nullary(fd io, Assembly& as, ASMOpcode opcode) {
        ::write(io, "  ", ASM_OPCODE_NAMES[(unsigned)opcode], '\n');
    }

    static void write_unary(fd io, Assembly& as, ASMOpcode opcode, const ASMVal& dst) {
        ::write(io, "  ", ASM_OPCODE_NAMES[(unsigned)opcode], ' ');
        write_mval(io, as, dst);
        ::write(io, '\n');
    }

    static void write_binary(fd io, Assembly& as, ASMOpcode opcode, const ASMVal& dst, const ASMVal& src) {
        ::write(io, "  ", ASM_OPCODE_NAMES[(unsigned)opcode], ' ');
        write_mval(io, as, dst);
        ::write(io, ", ");
        write_mval(io, as, src);
        ::write(io, '\n');
    }

    static void write_ternary(fd io, Assembly& as, ASMOpcode opcode, const ASMVal& dst, const ASMVal& a, const ASMVal& b) {
        ::write(io, "  ", ASM_OPCODE_NAMES[(unsigned)opcode], ' ');
        write_mval(io, as, dst);
        ::write(io, ", ");
        write_mval(io, as, a);
        ::write(io, ", ");
        write_mval(io, as, b);
        ::write(io, '\n');
    }

    static void write_quaternary(fd io, Assembly& as, ASMOpcode opcode, const ASMVal& dst, const ASMVal& a, const ASMVal& b, const ASMVal& c) {
        ::write(io, "  ", ASM_OPCODE_NAMES[(unsigned)opcode], ' ');
        write_mval(io, as, dst);
        ::write(io, ", ");
        write_mval(io, as, a);
        ::write(io, ", ");
        write_mval(io, as, b);
        ::write(io, ", ");
        write_mval(io, as, c);
        ::write(io, '\n');
    }

    static void write_compare(fd io, Assembly& as, const i8* prefix, Condition condition, const i8* suffix, const ASMVal& dst, const ASMVal& a, const ASMVal& b) {
        ::write(io, "  ", prefix, ASM_CONDITION_NAMES[(unsigned)condition], suffix, ' ');
        write_mval(io, as, dst);
        ::write(io, ", ");
        write_mval(io, as, a);
        ::write(io, ", ");
        write_mval(io, as, b);
        ::write(io, '\n');
    }

    static fd output;

    static void write_to(fd output_in) {
        output = output_in;
    }

    #define NULLARY(opcode) { write_nullary(output, as, ASMOpcode::opcode); }
    #define UNARY(opcode) { write_unary(output, as, ASMOpcode::opcode, dst); }
    #define BINARY(opcode) { write_binary(output, as, ASMOpcode::opcode, dst, src); }
    #define TERNARY(opcode) { write_ternary(output, as, ASMOpcode::opcode, dst, a, b); }
    #define QUATERNARY(opcode) { write_quaternary(output, as, ASMOpcode::opcode, dst, a, b, c); }
    #define CONDITIONAL(p, s) { write_compare(output, as, #p, cc, #s, dst, a, b); }
    #define FCONDITIONAL(p, s) { write_compare(output, as, #p, (Condition)cc, #s, dst, a, b); }
    #define LABEL(p, label) { write_label(output, as, p, sym); }

    // Arithmetic

    static void add8(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) TERNARY(ADD8)
    static void add16(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) TERNARY(ADD16)
    static void add32(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) TERNARY(ADD32)
    static void add64(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) TERNARY(ADD64)

    static void sub8(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) TERNARY(SUB8)
    static void sub16(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) TERNARY(SUB16)
    static void sub32(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) TERNARY(SUB32)
    static void sub64(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) TERNARY(SUB64)

    static void mul8(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) TERNARY(MUL8)
    static void mul16(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) TERNARY(MUL16)
    static void mul32(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) TERNARY(MUL32)
    static void mul64(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) TERNARY(MUL64)

    static void sdiv8(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) TERNARY(SDIV8)
    static void sdiv16(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) TERNARY(SDIV16)
    static void sdiv32(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) TERNARY(SDIV32)
    static void sdiv64(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) TERNARY(SDIV64)

    static void udiv8(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) TERNARY(UDIV8)
    static void udiv16(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) TERNARY(UDIV16)
    static void udiv32(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) TERNARY(UDIV32)
    static void udiv64(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) TERNARY(UDIV64)

    static void srem8(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) TERNARY(SREM8)
    static void srem16(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) TERNARY(SREM16)
    static void srem32(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) TERNARY(SREM32)
    static void srem64(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) TERNARY(SREM64)

    static void urem8(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) TERNARY(UREM8)
    static void urem16(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) TERNARY(UREM16)
    static void urem32(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) TERNARY(UREM32)
    static void urem64(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) TERNARY(UREM64)

    static void neg8(Assembly& as, ASMVal dst, ASMVal src) BINARY(NEG8)
    static void neg16(Assembly& as, ASMVal dst, ASMVal src) BINARY(NEG16)
    static void neg32(Assembly& as, ASMVal dst, ASMVal src) BINARY(NEG32)
    static void neg64(Assembly& as, ASMVal dst, ASMVal src) BINARY(NEG64)

    // Floating-Point Arithmetic

    static void fadd32(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) TERNARY(FADD32)
    static void fadd64(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) TERNARY(FADD64)

    static void fsub32(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) TERNARY(FSUB32)
    static void fsub64(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) TERNARY(FSUB64)

    static void fmul32(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) TERNARY(FMUL32)
    static void fmul64(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) TERNARY(FMUL64)

    static void fdiv32(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) TERNARY(FDIV32)
    static void fdiv64(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) TERNARY(FDIV64)

    static void frem32(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) TERNARY(FREM32)
    static void frem64(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) TERNARY(FREM64)

    static void fneg32(Assembly& as, ASMVal dst, ASMVal src) BINARY(FNEG32)
    static void fneg64(Assembly& as, ASMVal dst, ASMVal src) BINARY(FNEG64)

    static void fsqrt32(Assembly& as, ASMVal dst, ASMVal src) BINARY(FSQRT32)
    static void fsqrt64(Assembly& as, ASMVal dst, ASMVal src) BINARY(FSQRT64)

    static void fround32(Assembly& as, ASMVal dst, ASMVal src) BINARY(FROUND32)
    static void fround64(Assembly& as, ASMVal dst, ASMVal src) BINARY(FROUND64)

    static void ffloor32(Assembly& as, ASMVal dst, ASMVal src) BINARY(FFLOOR32)
    static void ffloor64(Assembly& as, ASMVal dst, ASMVal src) BINARY(FFLOOR64)

    static void fceil32(Assembly& as, ASMVal dst, ASMVal src) BINARY(FCEIL32)
    static void fceil64(Assembly& as, ASMVal dst, ASMVal src) BINARY(FCEIL64)

    static void fmin32(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) TERNARY(FMIN32)
    static void fmin64(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) TERNARY(FMIN64)

    static void fmax32(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) TERNARY(FMIN64)
    static void fmax64(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) TERNARY(FMIN64)

    static void fabs32(Assembly& as, ASMVal dst, ASMVal src) BINARY(FABS32)
    static void fabs64(Assembly& as, ASMVal dst, ASMVal src) BINARY(FABS64)

    // Bitwise Operations

    static void and8(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) TERNARY(AND8)
    static void and16(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) TERNARY(AND16)
    static void and32(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) TERNARY(AND32)
    static void and64(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) TERNARY(AND64)

    static void xor8(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) TERNARY(XOR8)
    static void xor16(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) TERNARY(XOR16)
    static void xor32(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) TERNARY(XOR32)
    static void xor64(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) TERNARY(XOR64)

    static void or8(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) TERNARY(OR8)
    static void or16(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) TERNARY(OR16)
    static void or32(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) TERNARY(OR32)
    static void or64(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) TERNARY(OR64)

    static void not8(Assembly& as, ASMVal dst, ASMVal src) BINARY(NOT8)
    static void not16(Assembly& as, ASMVal dst, ASMVal src) BINARY(NOT16)
    static void not32(Assembly& as, ASMVal dst, ASMVal src) BINARY(NOT32)
    static void not64(Assembly& as, ASMVal dst, ASMVal src) BINARY(NOT64)

    static void shl8(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) TERNARY(SHL8)
    static void shl16(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) TERNARY(SHL16)
    static void shl32(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) TERNARY(SHL32)
    static void shl64(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) TERNARY(SHL64)

    static void shr8(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) TERNARY(SHR8)
    static void shr16(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) TERNARY(SHR16)
    static void shr32(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) TERNARY(SHR32)
    static void shr64(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) TERNARY(SHR64)

    static void sar8(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) TERNARY(SAR8)
    static void sar16(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) TERNARY(SAR16)
    static void sar32(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) TERNARY(SAR32)
    static void sar64(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) TERNARY(SAR64)

    static void rol8(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) TERNARY(ROL8)
    static void rol16(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) TERNARY(ROL16)
    static void rol32(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) TERNARY(ROL32)
    static void rol64(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) TERNARY(ROL64)

    static void ror8(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) TERNARY(ROR8)
    static void ror16(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) TERNARY(ROR16)
    static void ror32(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) TERNARY(ROR32)
    static void ror64(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) TERNARY(ROR64)

    static void bitc8(Assembly& as, ASMVal dst, ASMVal src) BINARY(BITC8)
    static void bitc16(Assembly& as, ASMVal dst, ASMVal src) BINARY(BITC16)
    static void bitc32(Assembly& as, ASMVal dst, ASMVal src) BINARY(BITC32)
    static void bitc64(Assembly& as, ASMVal dst, ASMVal src) BINARY(BITC64)

    static void lzc8(Assembly& as, ASMVal dst, ASMVal src) BINARY(LZC8)
    static void lzc16(Assembly& as, ASMVal dst, ASMVal src) BINARY(LZC16)
    static void lzc32(Assembly& as, ASMVal dst, ASMVal src) BINARY(LZC32)
    static void lzc64(Assembly& as, ASMVal dst, ASMVal src) BINARY(LZC64)

    static void tzc8(Assembly& as, ASMVal dst, ASMVal src) BINARY(TZC8)
    static void tzc16(Assembly& as, ASMVal dst, ASMVal src) BINARY(TZC16)
    static void tzc32(Assembly& as, ASMVal dst, ASMVal src) BINARY(TZC32)
    static void tzc64(Assembly& as, ASMVal dst, ASMVal src) BINARY(TZC64)

    // Comparisons

    static void isz(Assembly& as, ASMVal dst, ASMVal src) BINARY(ISZ)
    static void isnz(Assembly& as, ASMVal dst, ASMVal src) BINARY(ISNZ)

    static void ccc8(Assembly& as, Condition cc, ASMVal dst, ASMVal a, ASMVal b) CONDITIONAL(c, 8)
    static void ccc16(Assembly& as, Condition cc, ASMVal dst, ASMVal a, ASMVal b) CONDITIONAL(c, 16)
    static void ccc32(Assembly& as, Condition cc, ASMVal dst, ASMVal a, ASMVal b) CONDITIONAL(c, 32)
    static void ccc64(Assembly& as, Condition cc, ASMVal dst, ASMVal a, ASMVal b) CONDITIONAL(c, 64)

    static void fccc32(Assembly& as, FloatCondition cc, ASMVal dst, ASMVal a, ASMVal b) FCONDITIONAL(cf, 32)
    static void fccc64(Assembly& as, FloatCondition cc, ASMVal dst, ASMVal a, ASMVal b) FCONDITIONAL(cf, 64)

    // Memory

    static void push8(Assembly& as, ASMVal dst) UNARY(PUSH8)
    static void push16(Assembly& as, ASMVal dst) UNARY(PUSH16)
    static void push32(Assembly& as, ASMVal dst) UNARY(PUSH32)
    static void push64(Assembly& as, ASMVal dst) UNARY(PUSH64)

    static void pop8(Assembly& as, ASMVal dst) UNARY(POP8)
    static void pop16(Assembly& as, ASMVal dst) UNARY(POP16)
    static void pop32(Assembly& as, ASMVal dst) UNARY(POP32)
    static void pop64(Assembly& as, ASMVal dst) UNARY(POP64)

    static void fpush32(Assembly& as, ASMVal dst) UNARY(FPUSH32)
    static void fpush64(Assembly& as, ASMVal dst) UNARY(FPUSH64)

    static void fpop32(Assembly& as, ASMVal dst) UNARY(FPOP32)
    static void fpop64(Assembly& as, ASMVal dst) UNARY(FPOP64)

    static void mov8(Assembly& as, ASMVal dst, ASMVal src) BINARY(MOV8)
    static void mov16(Assembly& as, ASMVal dst, ASMVal src) BINARY(MOV16)
    static void mov32(Assembly& as, ASMVal dst, ASMVal src) BINARY(MOV32)
    static void mov64(Assembly& as, ASMVal dst, ASMVal src) BINARY(MOV64)

    static void fmov32(Assembly& as, ASMVal dst, ASMVal src) BINARY(FMOV32)
    static void fmov64(Assembly& as, ASMVal dst, ASMVal src) BINARY(FMOV64)

    static void ld8(Assembly& as, ASMVal dst, ASMVal src) BINARY(LD8)
    static void ld16(Assembly& as, ASMVal dst, ASMVal src) BINARY(LD16)
    static void ld32(Assembly& as, ASMVal dst, ASMVal src) BINARY(LD32)
    static void ld64(Assembly& as, ASMVal dst, ASMVal src) BINARY(LD64)

    static void st8(Assembly& as, ASMVal dst, ASMVal src) BINARY(ST8)
    static void st16(Assembly& as, ASMVal dst, ASMVal src) BINARY(ST16)
    static void st32(Assembly& as, ASMVal dst, ASMVal src) BINARY(ST32)
    static void st64(Assembly& as, ASMVal dst, ASMVal src) BINARY(ST64)
    
    static void ldi8(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) TERNARY(LDI8)
    static void ldi16(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) TERNARY(LDI16)
    static void ldi32(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) TERNARY(LDI32)
    static void ldi64(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) TERNARY(LDI64)
    
    static void ldai8(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) TERNARY(LDAI8)
    static void ldai16(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) TERNARY(LDAI16)
    static void ldai32(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) TERNARY(LDAI32)
    static void ldai64(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) TERNARY(LDAI64)
    
    static void sti8(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) TERNARY(STI8)
    static void sti16(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) TERNARY(STI16)
    static void sti32(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) TERNARY(STI32)
    static void sti64(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) TERNARY(STI64)

    static void fld32(Assembly& as, ASMVal dst, ASMVal src) BINARY(FLD32)
    static void fld64(Assembly& as, ASMVal dst, ASMVal src) BINARY(FLD64)

    static void fst32(Assembly& as, ASMVal dst, ASMVal src) BINARY(FST32)
    static void fst64(Assembly& as, ASMVal dst, ASMVal src) BINARY(FST64)

    static void fldi32(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) TERNARY(FLDI32)
    static void fldi64(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) TERNARY(FLDI64)

    static void fsti32(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) TERNARY(FSTI32)
    static void fsti64(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) TERNARY(FSTI64)
    
    static void lda(Assembly& as, ASMVal dst, ASMVal src) BINARY(LDA)

    static void ldc(Assembly& as, ASMVal dst, i64 imm) {
        ::write(output, "  ", ASM_OPCODE_NAMES[(unsigned)ASMOpcode::LDC], ' ', dst, ", ", imm, '\n');
    }

    // Labels

    static void global(Assembly& as, stridx sym) LABEL("", sym)
    static void local(Assembly& as, stridx sym) LABEL(".", sym)

    // Jumps

    static void j(Assembly& as, ASMVal dst) UNARY(J)

    static void jz(Assembly& as, ASMVal dst, ASMVal src) BINARY(JZ)
    static void jnz(Assembly& as, ASMVal dst, ASMVal src) BINARY(JNZ)

    static void jcc8(Assembly& as, Condition cc, ASMVal dst, ASMVal a, ASMVal b) CONDITIONAL(j, 8)
    static void jcc16(Assembly& as, Condition cc, ASMVal dst, ASMVal a, ASMVal b) CONDITIONAL(j, 16)
    static void jcc32(Assembly& as, Condition cc, ASMVal dst, ASMVal a, ASMVal b) CONDITIONAL(j, 32)
    static void jcc64(Assembly& as, Condition cc, ASMVal dst, ASMVal a, ASMVal b) CONDITIONAL(j, 64)
    
    static void fjcc32(Assembly& as, FloatCondition cc, ASMVal dst, ASMVal a, ASMVal b) FCONDITIONAL(jf, 32)
    static void fjcc64(Assembly& as, FloatCondition cc, ASMVal dst, ASMVal a, ASMVal b) FCONDITIONAL(jf, 64)

    // Functions

    static void enter(Assembly& as) NULLARY(ENTER)
    static void stack(Assembly& as, ASMVal dst) UNARY(STACK)
    static void alloca(Assembly& as, ASMVal dst, ASMVal src) BINARY(ALLOCA)
    static void unstack(Assembly& as, ASMVal dst) UNARY(UNSTACK)
    static void leave(Assembly& as) NULLARY(LEAVE)

    static void call(Assembly& as, ASMVal dst) UNARY(CALL)
    static void ret(Assembly& as) NULLARY(RET)

    // Conversions

    static void sxt8(Assembly& as, ASMVal dst, ASMVal src) BINARY(SXT8)
    static void sxt16(Assembly& as, ASMVal dst, ASMVal src) BINARY(SXT16)
    static void sxt32(Assembly& as, ASMVal dst, ASMVal src) BINARY(SXT32)

    static void zxt8(Assembly& as, ASMVal dst, ASMVal src) BINARY(ZXT8)
    static void zxt16(Assembly& as, ASMVal dst, ASMVal src) BINARY(ZXT16)
    static void zxt32(Assembly& as, ASMVal dst, ASMVal src) BINARY(ZXT32)

    static void i8tof32(Assembly& as, ASMVal dst, ASMVal src) BINARY(I8TOF32)
    static void i16tof32(Assembly& as, ASMVal dst, ASMVal src) BINARY(I16TOF32)
    static void i32tof32(Assembly& as, ASMVal dst, ASMVal src) BINARY(I32TOF32)
    static void i64tof32(Assembly& as, ASMVal dst, ASMVal src) BINARY(I64TOF32)

    static void i8tof64(Assembly& as, ASMVal dst, ASMVal src) BINARY(I8TOF64)
    static void i16tof64(Assembly& as, ASMVal dst, ASMVal src) BINARY(I16TOF64)
    static void i32tof64(Assembly& as, ASMVal dst, ASMVal src) BINARY(I32TOF64)
    static void i64tof64(Assembly& as, ASMVal dst, ASMVal src) BINARY(I64TOF64)

    static void f32toi8(Assembly& as, ASMVal dst, ASMVal src) BINARY(F32TOI8)
    static void f32toi16(Assembly& as, ASMVal dst, ASMVal src) BINARY(F32TOI16)
    static void f32toi32(Assembly& as, ASMVal dst, ASMVal src) BINARY(F32TOI32)
    static void f32toi64(Assembly& as, ASMVal dst, ASMVal src) BINARY(F32TOI64)

    static void f64toi8(Assembly& as, ASMVal dst, ASMVal src) BINARY(F64TOI8)
    static void f64toi16(Assembly& as, ASMVal dst, ASMVal src) BINARY(F64TOI16)
    static void f64toi32(Assembly& as, ASMVal dst, ASMVal src) BINARY(F64TOI32)
    static void f64toi64(Assembly& as, ASMVal dst, ASMVal src) BINARY(F64TOI64)

    static void f32tof64(Assembly& as, ASMVal dst, ASMVal src) BINARY(F32TOF64)
    static void f64tof32(Assembly& as, ASMVal dst, ASMVal src) BINARY(F64TOF32)

    static void f32frombits(Assembly& as, ASMVal dst, ASMVal src) BINARY(F32FROMBITS)
    static void f64frombits(Assembly& as, ASMVal dst, ASMVal src) BINARY(F64FROMBITS)
    static void f32tobits(Assembly& as, ASMVal dst, ASMVal src) BINARY(F32TOBITS)
    static void f64tobits(Assembly& as, ASMVal dst, ASMVal src) BINARY(F64TOBITS)

    // Memory
    
    static void mcpy(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) TERNARY(MCPY)
    static void mmov(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) TERNARY(MMOV)
    static void mset(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) TERNARY(MSET)
    static void mcmp(Assembly& as, ASMVal dst, ASMVal a, ASMVal b, ASMVal c) QUATERNARY(MCMP)

    #undef NULLARY
    #undef UNARY
    #undef BINARY
    #undef TERNARY
    #undef QUATERNARY
    #undef CONDITIONAL
    #undef FCONDITIONAL
    #undef LABEL
};

template<typename Target>
fd ASMPrinter<Target>::output = -1;

// Verifies instructions both independent of target and using target information if available.
template<typename Target = FakeTarget>
struct ASMVerifier : public Target {
    #define GP(x) assert(x.kind() == ASMVal::GP); assert(Target::is_gpreg(x.payload.gp))
    #define FP(x) assert(x.kind() == ASMVal::FP); assert(Target::is_fpreg(x.payload.fp))
    #define IMM(x) assert(x.kind() == ASMVal::IMM)
    #define GP_OR_IMM(x) assert(x.kind() == ASMVal::IMM || x.kind() == ASMVal::GP); if (x.kind() == ASMVal::GP) assert(Target::is_gp(x.payload.gp));
    #define FP_OR_F32(x) assert(x.kind() == ASMVal::F32 || x.kind() == ASMVal::FP); if (x.kind() == ASMVal::FP) assert(Target::is_fp(x.payload.fp));
    #define FP_OR_F64(x) assert(x.kind() == ASMVal::F64 || x.kind() == ASMVal::FP); if (x.kind() == ASMVal::FP) assert(Target::is_fp(x.payload.fp));
    #define ONLY_ONE_IMM(x, y) assert(x.kind() != ASMVal::IMM || y.kind() != ASMVal::IMM);
    #define ONLY_ONE_F32(x, y) assert(x.kind() != ASMVal::F32 || y.kind() != ASMVal::F32);
    #define ONLY_ONE_F64(x, y) assert(x.kind() != ASMVal::F64 || y.kind() != ASMVal::F64);
    #define MEM(x) assert(x.kind() == ASMVal::MEM); if (x.payload.memkind == ASMVal::REG_OFFSET) assert(Target::is_gpreg(x.payload.base));
    #define LABEL(x) assert(x.kind() == ASMVal::MEM && x.payload.memkind != ASMVal::REG_OFFSET)
    #define GP_OR_LABEL(x) assert(x.kind() == ASMVal::GP || (x.kind() == ASMVal::MEM && x.payload.memkind != ASMVal::REG_OFFSET))
    #define TRUE(a, b) (void)true

    #define NULLARY {}
    #define UNARY(DSTPRED) { DSTPRED(dst); }
    #define BINARY(DSTPRED, SRCPRED) { DSTPRED(dst); SRCPRED(src); }
    #define TERNARY(DSTPRED, APRED, BPRED, ABPRED) { DSTPRED(dst); APRED(a); BPRED(b); ABPRED(a, b); }
    #define QUATERNARY(DSTPRED, APRED, BPRED, CPRED) { DSTPRED(dst); APRED(a); BPRED(b); CPRED(c); }

    static void add8(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) TERNARY(GP, GP_OR_IMM, GP_OR_IMM, ONLY_ONE_IMM)
    static void add16(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) TERNARY(GP, GP_OR_IMM, GP_OR_IMM, ONLY_ONE_IMM)
    static void add32(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) TERNARY(GP, GP_OR_IMM, GP_OR_IMM, ONLY_ONE_IMM)
    static void add64(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) TERNARY(GP, GP_OR_IMM, GP_OR_IMM, ONLY_ONE_IMM)

    static void sub8(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) TERNARY(GP, GP_OR_IMM, GP_OR_IMM, ONLY_ONE_IMM)
    static void sub16(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) TERNARY(GP, GP_OR_IMM, GP_OR_IMM, ONLY_ONE_IMM)
    static void sub32(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) TERNARY(GP, GP_OR_IMM, GP_OR_IMM, ONLY_ONE_IMM)
    static void sub64(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) TERNARY(GP, GP_OR_IMM, GP_OR_IMM, ONLY_ONE_IMM)

    static void mul8(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) TERNARY(GP, GP_OR_IMM, GP_OR_IMM, ONLY_ONE_IMM)
    static void mul16(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) TERNARY(GP, GP_OR_IMM, GP_OR_IMM, ONLY_ONE_IMM)
    static void mul32(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) TERNARY(GP, GP_OR_IMM, GP_OR_IMM, ONLY_ONE_IMM)
    static void mul64(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) TERNARY(GP, GP_OR_IMM, GP_OR_IMM, ONLY_ONE_IMM)

    static void sdiv8(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) TERNARY(GP, GP_OR_IMM, GP_OR_IMM, ONLY_ONE_IMM)
    static void sdiv16(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) TERNARY(GP, GP_OR_IMM, GP_OR_IMM, ONLY_ONE_IMM)
    static void sdiv32(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) TERNARY(GP, GP_OR_IMM, GP_OR_IMM, ONLY_ONE_IMM)
    static void sdiv64(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) TERNARY(GP, GP_OR_IMM, GP_OR_IMM, ONLY_ONE_IMM)

    static void udiv8(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) TERNARY(GP, GP_OR_IMM, GP_OR_IMM, ONLY_ONE_IMM)
    static void udiv16(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) TERNARY(GP, GP_OR_IMM, GP_OR_IMM, ONLY_ONE_IMM)
    static void udiv32(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) TERNARY(GP, GP_OR_IMM, GP_OR_IMM, ONLY_ONE_IMM)
    static void udiv64(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) TERNARY(GP, GP_OR_IMM, GP_OR_IMM, ONLY_ONE_IMM)

    static void srem8(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) TERNARY(GP, GP_OR_IMM, GP_OR_IMM, ONLY_ONE_IMM)
    static void srem16(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) TERNARY(GP, GP_OR_IMM, GP_OR_IMM, ONLY_ONE_IMM)
    static void srem32(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) TERNARY(GP, GP_OR_IMM, GP_OR_IMM, ONLY_ONE_IMM)
    static void srem64(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) TERNARY(GP, GP_OR_IMM, GP_OR_IMM, ONLY_ONE_IMM)

    static void urem8(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) TERNARY(GP, GP_OR_IMM, GP_OR_IMM, ONLY_ONE_IMM)
    static void urem16(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) TERNARY(GP, GP_OR_IMM, GP_OR_IMM, ONLY_ONE_IMM)
    static void urem32(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) TERNARY(GP, GP_OR_IMM, GP_OR_IMM, ONLY_ONE_IMM)
    static void urem64(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) TERNARY(GP, GP_OR_IMM, GP_OR_IMM, ONLY_ONE_IMM)

    static void neg8(Assembly& as, ASMVal dst, ASMVal src) BINARY(GP, GP)
    static void neg16(Assembly& as, ASMVal dst, ASMVal src) BINARY(GP, GP)
    static void neg32(Assembly& as, ASMVal dst, ASMVal src) BINARY(GP, GP)
    static void neg64(Assembly& as, ASMVal dst, ASMVal src) BINARY(GP, GP)

    // Floating-Point Arithmetic

    static void fadd32(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) TERNARY(FP, FP_OR_F32, FP_OR_F32, ONLY_ONE_F32)
    static void fadd64(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) TERNARY(FP, FP_OR_F64, FP_OR_F64, ONLY_ONE_F64)

    static void fsub32(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) TERNARY(FP, FP_OR_F32, FP_OR_F32, ONLY_ONE_F32)
    static void fsub64(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) TERNARY(FP, FP_OR_F64, FP_OR_F64, ONLY_ONE_F64)

    static void fmul32(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) TERNARY(FP, FP_OR_F32, FP_OR_F32, ONLY_ONE_F32)
    static void fmul64(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) TERNARY(FP, FP_OR_F64, FP_OR_F64, ONLY_ONE_F64)

    static void fdiv32(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) TERNARY(FP, FP_OR_F32, FP_OR_F32, ONLY_ONE_F32)
    static void fdiv64(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) TERNARY(FP, FP_OR_F64, FP_OR_F64, ONLY_ONE_F64)

    static void frem32(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) TERNARY(FP, FP_OR_F32, FP_OR_F32, ONLY_ONE_F32)
    static void frem64(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) TERNARY(FP, FP_OR_F64, FP_OR_F64, ONLY_ONE_F64)

    static void fneg32(Assembly& as, ASMVal dst, ASMVal src) BINARY(FP, FP)
    static void fneg64(Assembly& as, ASMVal dst, ASMVal src) BINARY(FP, FP)

    static void fsqrt32(Assembly& as, ASMVal dst, ASMVal src) BINARY(FP, FP)
    static void fsqrt64(Assembly& as, ASMVal dst, ASMVal src) BINARY(FP, FP)

    static void fround32(Assembly& as, ASMVal dst, ASMVal src) BINARY(FP, FP)
    static void fround64(Assembly& as, ASMVal dst, ASMVal src) BINARY(FP, FP)

    static void ffloor32(Assembly& as, ASMVal dst, ASMVal src) BINARY(FP, FP)
    static void ffloor64(Assembly& as, ASMVal dst, ASMVal src) BINARY(FP, FP)

    static void fceil32(Assembly& as, ASMVal dst, ASMVal src) BINARY(FP, FP)
    static void fceil64(Assembly& as, ASMVal dst, ASMVal src) BINARY(FP, FP)

    static void fmin32(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) TERNARY(FP, FP_OR_F32, FP_OR_F32, ONLY_ONE_F32)
    static void fmin64(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) TERNARY(FP, FP_OR_F64, FP_OR_F64, ONLY_ONE_F64)

    static void fmax32(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) TERNARY(FP, FP_OR_F32, FP_OR_F32, ONLY_ONE_F32)
    static void fmax64(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) TERNARY(FP, FP_OR_F64, FP_OR_F64, ONLY_ONE_F64)

    static void fabs32(Assembly& as, ASMVal dst, ASMVal src) BINARY(FP, FP)
    static void fabs64(Assembly& as, ASMVal dst, ASMVal src) BINARY(FP, FP)

    // Bitwise Operations

    static void and8(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) TERNARY(GP, GP_OR_IMM, GP_OR_IMM, ONLY_ONE_IMM)
    static void and16(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) TERNARY(GP, GP_OR_IMM, GP_OR_IMM, ONLY_ONE_IMM)
    static void and32(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) TERNARY(GP, GP_OR_IMM, GP_OR_IMM, ONLY_ONE_IMM)
    static void and64(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) TERNARY(GP, GP_OR_IMM, GP_OR_IMM, ONLY_ONE_IMM)

    static void xor8(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) TERNARY(GP, GP_OR_IMM, GP_OR_IMM, ONLY_ONE_IMM)
    static void xor16(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) TERNARY(GP, GP_OR_IMM, GP_OR_IMM, ONLY_ONE_IMM)
    static void xor32(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) TERNARY(GP, GP_OR_IMM, GP_OR_IMM, ONLY_ONE_IMM)
    static void xor64(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) TERNARY(GP, GP_OR_IMM, GP_OR_IMM, ONLY_ONE_IMM)

    static void or8(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) TERNARY(GP, GP_OR_IMM, GP_OR_IMM, ONLY_ONE_IMM)
    static void or16(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) TERNARY(GP, GP_OR_IMM, GP_OR_IMM, ONLY_ONE_IMM)
    static void or32(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) TERNARY(GP, GP_OR_IMM, GP_OR_IMM, ONLY_ONE_IMM)
    static void or64(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) TERNARY(GP, GP_OR_IMM, GP_OR_IMM, ONLY_ONE_IMM)

    static void not8(Assembly& as, ASMVal dst, ASMVal src) BINARY(GP, GP)
    static void not16(Assembly& as, ASMVal dst, ASMVal src) BINARY(GP, GP)
    static void not32(Assembly& as, ASMVal dst, ASMVal src) BINARY(GP, GP)
    static void not64(Assembly& as, ASMVal dst, ASMVal src) BINARY(GP, GP)

    static void shl8(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) TERNARY(GP, GP_OR_IMM, GP_OR_IMM, ONLY_ONE_IMM)
    static void shl16(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) TERNARY(GP, GP_OR_IMM, GP_OR_IMM, ONLY_ONE_IMM)
    static void shl32(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) TERNARY(GP, GP_OR_IMM, GP_OR_IMM, ONLY_ONE_IMM)
    static void shl64(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) TERNARY(GP, GP_OR_IMM, GP_OR_IMM, ONLY_ONE_IMM)

    static void shr8(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) TERNARY(GP, GP_OR_IMM, GP_OR_IMM, ONLY_ONE_IMM)
    static void shr16(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) TERNARY(GP, GP_OR_IMM, GP_OR_IMM, ONLY_ONE_IMM)
    static void shr32(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) TERNARY(GP, GP_OR_IMM, GP_OR_IMM, ONLY_ONE_IMM)
    static void shr64(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) TERNARY(GP, GP_OR_IMM, GP_OR_IMM, ONLY_ONE_IMM)

    static void sar8(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) TERNARY(GP, GP_OR_IMM, GP_OR_IMM, ONLY_ONE_IMM)
    static void sar16(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) TERNARY(GP, GP_OR_IMM, GP_OR_IMM, ONLY_ONE_IMM)
    static void sar32(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) TERNARY(GP, GP_OR_IMM, GP_OR_IMM, ONLY_ONE_IMM)
    static void sar64(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) TERNARY(GP, GP_OR_IMM, GP_OR_IMM, ONLY_ONE_IMM)

    static void rol8(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) TERNARY(GP, GP_OR_IMM, GP_OR_IMM, ONLY_ONE_IMM)
    static void rol16(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) TERNARY(GP, GP_OR_IMM, GP_OR_IMM, ONLY_ONE_IMM)
    static void rol32(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) TERNARY(GP, GP_OR_IMM, GP_OR_IMM, ONLY_ONE_IMM)
    static void rol64(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) TERNARY(GP, GP_OR_IMM, GP_OR_IMM, ONLY_ONE_IMM)

    static void ror8(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) TERNARY(GP, GP_OR_IMM, GP_OR_IMM, ONLY_ONE_IMM)
    static void ror16(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) TERNARY(GP, GP_OR_IMM, GP_OR_IMM, ONLY_ONE_IMM)
    static void ror32(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) TERNARY(GP, GP_OR_IMM, GP_OR_IMM, ONLY_ONE_IMM)
    static void ror64(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) TERNARY(GP, GP_OR_IMM, GP_OR_IMM, ONLY_ONE_IMM)

    static void bitc8(Assembly& as, ASMVal dst, ASMVal src) BINARY(GP, GP)
    static void bitc16(Assembly& as, ASMVal dst, ASMVal src) BINARY(GP, GP)
    static void bitc32(Assembly& as, ASMVal dst, ASMVal src) BINARY(GP, GP)
    static void bitc64(Assembly& as, ASMVal dst, ASMVal src) BINARY(GP, GP)

    static void lzc8(Assembly& as, ASMVal dst, ASMVal src) BINARY(GP, GP)
    static void lzc16(Assembly& as, ASMVal dst, ASMVal src) BINARY(GP, GP)
    static void lzc32(Assembly& as, ASMVal dst, ASMVal src) BINARY(GP, GP)
    static void lzc64(Assembly& as, ASMVal dst, ASMVal src) BINARY(GP, GP)

    static void tzc8(Assembly& as, ASMVal dst, ASMVal src) BINARY(GP, GP)
    static void tzc16(Assembly& as, ASMVal dst, ASMVal src) BINARY(GP, GP)
    static void tzc32(Assembly& as, ASMVal dst, ASMVal src) BINARY(GP, GP)
    static void tzc64(Assembly& as, ASMVal dst, ASMVal src) BINARY(GP, GP)

    // Comparisons

    static void isz(Assembly& as, ASMVal dst, ASMVal src) BINARY(GP, GP)
    static void isnz(Assembly& as, ASMVal dst, ASMVal src) BINARY(GP, GP)

    static void ccc8(Assembly& as, Condition cc, ASMVal dst, ASMVal a, ASMVal b) TERNARY(GP, GP_OR_IMM, GP_OR_IMM, ONLY_ONE_IMM)
    static void ccc16(Assembly& as, Condition cc, ASMVal dst, ASMVal a, ASMVal b) TERNARY(GP, GP_OR_IMM, GP_OR_IMM, ONLY_ONE_IMM)
    static void ccc32(Assembly& as, Condition cc, ASMVal dst, ASMVal a, ASMVal b) TERNARY(GP, GP_OR_IMM, GP_OR_IMM, ONLY_ONE_IMM)
    static void ccc64(Assembly& as, Condition cc, ASMVal dst, ASMVal a, ASMVal b) TERNARY(GP, GP_OR_IMM, GP_OR_IMM, ONLY_ONE_IMM)

    static void fccc32(Assembly& as, FloatCondition cc, ASMVal dst, ASMVal a, ASMVal b) TERNARY(FP, FP_OR_F32, FP_OR_F32, ONLY_ONE_F32)
    static void fccc64(Assembly& as, FloatCondition cc, ASMVal dst, ASMVal a, ASMVal b) TERNARY(FP, FP_OR_F64, FP_OR_F64, ONLY_ONE_F64)

    // Memory

    static void push8(Assembly& as, ASMVal dst) UNARY(GP)
    static void push16(Assembly& as, ASMVal dst) UNARY(GP)
    static void push32(Assembly& as, ASMVal dst) UNARY(GP)
    static void push64(Assembly& as, ASMVal dst) UNARY(GP)

    static void pop8(Assembly& as, ASMVal dst) UNARY(GP)
    static void pop16(Assembly& as, ASMVal dst) UNARY(GP)
    static void pop32(Assembly& as, ASMVal dst) UNARY(GP)
    static void pop64(Assembly& as, ASMVal dst) UNARY(GP)

    static void fpush32(Assembly& as, ASMVal dst) UNARY(FP)
    static void fpush64(Assembly& as, ASMVal dst) UNARY(FP)

    static void fpop32(Assembly& as, ASMVal dst) UNARY(FP)
    static void fpop64(Assembly& as, ASMVal dst) UNARY(FP)

    static void mov8(Assembly& as, ASMVal dst, ASMVal src) BINARY(GP, GP_OR_IMM)
    static void mov16(Assembly& as, ASMVal dst, ASMVal src) BINARY(GP, GP_OR_IMM)
    static void mov32(Assembly& as, ASMVal dst, ASMVal src) BINARY(GP, GP_OR_IMM)
    static void mov64(Assembly& as, ASMVal dst, ASMVal src) BINARY(GP, GP_OR_IMM)

    static void fmov32(Assembly& as, ASMVal dst, ASMVal src) BINARY(FP, FP_OR_F32)
    static void fmov64(Assembly& as, ASMVal dst, ASMVal src) BINARY(FP, FP_OR_F64)

    static void ld8(Assembly& as, ASMVal dst, ASMVal src) BINARY(GP, MEM)
    static void ld16(Assembly& as, ASMVal dst, ASMVal src) BINARY(GP, MEM)
    static void ld32(Assembly& as, ASMVal dst, ASMVal src) BINARY(GP, MEM)
    static void ld64(Assembly& as, ASMVal dst, ASMVal src) BINARY(GP, MEM)

    static void st8(Assembly& as, ASMVal dst, ASMVal src) BINARY(MEM, GP_OR_IMM)
    static void st16(Assembly& as, ASMVal dst, ASMVal src) BINARY(MEM, GP_OR_IMM)
    static void st32(Assembly& as, ASMVal dst, ASMVal src) BINARY(MEM, GP_OR_IMM)
    static void st64(Assembly& as, ASMVal dst, ASMVal src) BINARY(MEM, GP_OR_IMM)
    
    static void ldi8(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) TERNARY(GP, MEM, GP, TRUE)
    static void ldi16(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) TERNARY(GP, MEM, GP, TRUE)
    static void ldi32(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) TERNARY(GP, MEM, GP, TRUE)
    static void ldi64(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) TERNARY(GP, MEM, GP, TRUE)
    
    static void ldai8(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) TERNARY(GP, MEM, GP, TRUE)
    static void ldai16(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) TERNARY(GP, MEM, GP, TRUE)
    static void ldai32(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) TERNARY(GP, MEM, GP, TRUE)
    static void ldai64(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) TERNARY(GP, MEM, GP, TRUE)
    
    static void sti8(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) TERNARY(MEM, GP, GP, TRUE)
    static void sti16(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) TERNARY(MEM, GP, GP, TRUE)
    static void sti32(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) TERNARY(MEM, GP, GP, TRUE)
    static void sti64(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) TERNARY(MEM, GP, GP, TRUE)

    static void fld32(Assembly& as, ASMVal dst, ASMVal src) BINARY(FP, MEM)
    static void fld64(Assembly& as, ASMVal dst, ASMVal src) BINARY(FP, MEM)

    static void fst32(Assembly& as, ASMVal dst, ASMVal src) BINARY(MEM, FP_OR_F32)
    static void fst64(Assembly& as, ASMVal dst, ASMVal src) BINARY(MEM, FP_OR_F64)

    static void fldi32(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) TERNARY(FP, MEM, GP, TRUE)
    static void fldi64(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) TERNARY(FP, MEM, GP, TRUE)

    static void fsti32(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) TERNARY(MEM, GP, FP, TRUE)
    static void fsti64(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) TERNARY(MEM, GP, FP, TRUE)
    
    static void lda(Assembly& as, ASMVal dst, ASMVal src) BINARY(GP, MEM)

    static void ldc(Assembly& as, ASMVal dst, i64 imm) UNARY(GP)

    // Labels

    static void global(Assembly& as, stridx sym) NULLARY
    static void local(Assembly& as, stridx sym) NULLARY

    // Jumps

    static void j(Assembly& as, ASMVal dst) UNARY(GP_OR_LABEL)

    static void jz(Assembly& as, ASMVal dst, ASMVal src) BINARY(GP_OR_LABEL, GP)
    static void jnz(Assembly& as, ASMVal dst, ASMVal src) BINARY(GP_OR_LABEL, GP)

    static void jcc8(Assembly& as, Condition cc, ASMVal dst, ASMVal a, ASMVal b) TERNARY(GP_OR_LABEL, GP_OR_IMM, GP_OR_IMM, ONLY_ONE_IMM)
    static void jcc16(Assembly& as, Condition cc, ASMVal dst, ASMVal a, ASMVal b) TERNARY(GP_OR_LABEL, GP_OR_IMM, GP_OR_IMM, ONLY_ONE_IMM)
    static void jcc32(Assembly& as, Condition cc, ASMVal dst, ASMVal a, ASMVal b) TERNARY(GP_OR_LABEL, GP_OR_IMM, GP_OR_IMM, ONLY_ONE_IMM)
    static void jcc64(Assembly& as, Condition cc, ASMVal dst, ASMVal a, ASMVal b) TERNARY(GP_OR_LABEL, GP_OR_IMM, GP_OR_IMM, ONLY_ONE_IMM)
    
    static void fjcc32(Assembly& as, FloatCondition cc, ASMVal dst, ASMVal a, ASMVal b) TERNARY(GP_OR_LABEL, FP_OR_F32, FP_OR_F32, ONLY_ONE_F32)
    static void fjcc64(Assembly& as, FloatCondition cc, ASMVal dst, ASMVal a, ASMVal b) TERNARY(GP_OR_LABEL, FP_OR_F64, FP_OR_F64, ONLY_ONE_F64)

    // Functions

    static void enter(Assembly& as) NULLARY
    static void stack(Assembly& as, ASMVal dst) UNARY(GP_OR_IMM)
    static void alloca(Assembly& as, ASMVal dst, ASMVal src) BINARY(GP, GP_OR_IMM)
    static void unstack(Assembly& as, ASMVal dst) UNARY(GP_OR_IMM)
    static void leave(Assembly& as) NULLARY

    static void call(Assembly& as, ASMVal dst) UNARY(GP_OR_LABEL)
    static void ret(Assembly& as) NULLARY

    // Conversions

    static void sxt8(Assembly& as, ASMVal dst, ASMVal src) BINARY(GP, GP)
    static void sxt16(Assembly& as, ASMVal dst, ASMVal src) BINARY(GP, GP)
    static void sxt32(Assembly& as, ASMVal dst, ASMVal src) BINARY(GP, GP)

    static void zxt8(Assembly& as, ASMVal dst, ASMVal src) BINARY(GP, GP)
    static void zxt16(Assembly& as, ASMVal dst, ASMVal src) BINARY(GP, GP)
    static void zxt32(Assembly& as, ASMVal dst, ASMVal src) BINARY(GP, GP)

    static void i8tof32(Assembly& as, ASMVal dst, ASMVal src) BINARY(FP, GP)
    static void i16tof32(Assembly& as, ASMVal dst, ASMVal src) BINARY(FP, GP)
    static void i32tof32(Assembly& as, ASMVal dst, ASMVal src) BINARY(FP, GP)
    static void i64tof32(Assembly& as, ASMVal dst, ASMVal src) BINARY(FP, GP)

    static void i8tof64(Assembly& as, ASMVal dst, ASMVal src) BINARY(FP, GP)
    static void i16tof64(Assembly& as, ASMVal dst, ASMVal src) BINARY(FP, GP)
    static void i32tof64(Assembly& as, ASMVal dst, ASMVal src) BINARY(FP, GP)
    static void i64tof64(Assembly& as, ASMVal dst, ASMVal src) BINARY(FP, GP)

    static void f32toi8(Assembly& as, ASMVal dst, ASMVal src) BINARY(GP, FP)
    static void f32toi16(Assembly& as, ASMVal dst, ASMVal src) BINARY(GP, FP)
    static void f32toi32(Assembly& as, ASMVal dst, ASMVal src) BINARY(GP, FP)
    static void f32toi64(Assembly& as, ASMVal dst, ASMVal src) BINARY(GP, FP)

    static void f64toi8(Assembly& as, ASMVal dst, ASMVal src) BINARY(GP, FP)
    static void f64toi16(Assembly& as, ASMVal dst, ASMVal src) BINARY(GP, FP)
    static void f64toi32(Assembly& as, ASMVal dst, ASMVal src) BINARY(GP, FP)
    static void f64toi64(Assembly& as, ASMVal dst, ASMVal src) BINARY(GP, FP)

    static void f32tof64(Assembly& as, ASMVal dst, ASMVal src) BINARY(FP, FP)
    static void f64tof32(Assembly& as, ASMVal dst, ASMVal src) BINARY(FP, FP)

    static void f32frombits(Assembly& as, ASMVal dst, ASMVal src) BINARY(FP, GP)
    static void f64frombits(Assembly& as, ASMVal dst, ASMVal src) BINARY(FP, GP)
    static void f32tobits(Assembly& as, ASMVal dst, ASMVal src) BINARY(GP, FP)
    static void f64tobits(Assembly& as, ASMVal dst, ASMVal src) BINARY(GP, FP)

    // Memory
    
    static void mcpy(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) TERNARY(MEM, MEM, GP_OR_IMM, TRUE)
    static void mmov(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) TERNARY(MEM, MEM, GP_OR_IMM, TRUE)
    static void mset(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) TERNARY(MEM, MEM, GP_OR_IMM, TRUE)
    static void mcmp(Assembly& as, ASMVal dst, ASMVal a, ASMVal b, ASMVal c) QUATERNARY(GP, MEM, MEM, GP_OR_IMM)
    
    #undef GP
    #undef FP
    #undef IMM
    #undef GP_OR_IMM
    #undef FP_OR_F32
    #undef FP_OR_F64
    #undef ONLY_ONE_IMM
    #undef ONLY_ONE_F32
    #undef ONLY_ONE_F64
    #undef MEM
    #undef LABEL
    #undef GP_OR_LABEL
    #undef TRUE

    #undef NULLARY
    #undef UNARY
    #undef BINARY
    #undef TERNARY
    #undef QUATERNARY
};

// Allows composition of multiple assembler phases, i.e. verification then printing.
template<typename A, typename B>
struct ASMCompose : public A, public B {
    #define NULLARY(fn) { A::fn(as); B::fn(as); }
    #define UNARY(fn) { A::fn(as, dst); B::fn(as, dst); }
    #define BINARY(fn) { A::fn(as, dst, src); B::fn(as, dst, src); }
    #define TERNARY(fn) { A::fn(as, dst, a, b); B::fn(as, dst, a, b); }
    #define CONDITIONAL(fn) { A::fn(as, cc, dst, a, b); B::fn(as, cc, dst, a, b); }
    #define QUATERNARY(fn) { A::fn(as, dst, a, b, c); B::fn(as, dst, a, b, c); }


    static void add8(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) TERNARY(add8)
    static void add16(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) TERNARY(add16)
    static void add32(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) TERNARY(add32)
    static void add64(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) TERNARY(add64)

    static void sub8(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) TERNARY(sub8)
    static void sub16(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) TERNARY(sub16)
    static void sub32(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) TERNARY(sub32)
    static void sub64(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) TERNARY(sub64)

    static void mul8(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) TERNARY(mul8)
    static void mul16(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) TERNARY(mul16)
    static void mul32(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) TERNARY(mul32)
    static void mul64(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) TERNARY(mul64)

    static void sdiv8(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) TERNARY(sdiv8)
    static void sdiv16(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) TERNARY(sdiv16)
    static void sdiv32(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) TERNARY(sdiv32)
    static void sdiv64(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) TERNARY(sdiv64)

    static void udiv8(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) TERNARY(udiv8)
    static void udiv16(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) TERNARY(udiv16)
    static void udiv32(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) TERNARY(udiv32)
    static void udiv64(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) TERNARY(udiv64)

    static void srem8(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) TERNARY(srem8)
    static void srem16(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) TERNARY(srem16)
    static void srem32(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) TERNARY(srem32)
    static void srem64(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) TERNARY(srem64)

    static void urem8(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) TERNARY(urem8)
    static void urem16(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) TERNARY(urem16)
    static void urem32(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) TERNARY(urem32)
    static void urem64(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) TERNARY(urem64)

    static void neg8(Assembly& as, ASMVal dst, ASMVal src) BINARY(neg8)
    static void neg16(Assembly& as, ASMVal dst, ASMVal src) BINARY(neg16)
    static void neg32(Assembly& as, ASMVal dst, ASMVal src) BINARY(neg32)
    static void neg64(Assembly& as, ASMVal dst, ASMVal src) BINARY(neg64)

    // Floating-Point Arithmetic

    static void fadd32(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) TERNARY(fadd32)
    static void fadd64(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) TERNARY(fadd64)

    static void fsub32(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) TERNARY(fsub32)
    static void fsub64(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) TERNARY(fsub64)

    static void fmul32(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) TERNARY(fmul32)
    static void fmul64(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) TERNARY(fmul64)

    static void fdiv32(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) TERNARY(fdiv32)
    static void fdiv64(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) TERNARY(fdiv64)

    static void frem32(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) TERNARY(frem32)
    static void frem64(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) TERNARY(frem64)

    static void fneg32(Assembly& as, ASMVal dst, ASMVal src) BINARY(fneg32)
    static void fneg64(Assembly& as, ASMVal dst, ASMVal src) BINARY(fneg64)

    static void fsqrt32(Assembly& as, ASMVal dst, ASMVal src) BINARY(fsqrt32)
    static void fsqrt64(Assembly& as, ASMVal dst, ASMVal src) BINARY(fsqrt64)

    static void fround32(Assembly& as, ASMVal dst, ASMVal src) BINARY(fround32)
    static void fround64(Assembly& as, ASMVal dst, ASMVal src) BINARY(fround64)

    static void ffloor32(Assembly& as, ASMVal dst, ASMVal src) BINARY(ffloor32)
    static void ffloor64(Assembly& as, ASMVal dst, ASMVal src) BINARY(ffloor64)

    static void fceil32(Assembly& as, ASMVal dst, ASMVal src) BINARY(fceil32)
    static void fceil64(Assembly& as, ASMVal dst, ASMVal src) BINARY(fceil64)

    static void fmin32(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) TERNARY(fmin32)
    static void fmin64(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) TERNARY(fmin64)

    static void fmax32(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) TERNARY(fmax32)
    static void fmax64(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) TERNARY(fmax64)

    static void fabs32(Assembly& as, ASMVal dst, ASMVal src) BINARY(fabs32)
    static void fabs64(Assembly& as, ASMVal dst, ASMVal src) BINARY(fabs64)

    // Bitwise Operations

    static void and8(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) TERNARY(and8)
    static void and16(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) TERNARY(and16)
    static void and32(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) TERNARY(and32)
    static void and64(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) TERNARY(and64)

    static void xor8(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) TERNARY(xor8)
    static void xor16(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) TERNARY(xor16)
    static void xor32(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) TERNARY(xor32)
    static void xor64(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) TERNARY(xor64)

    static void or8(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) TERNARY(or8)
    static void or16(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) TERNARY(or16)
    static void or32(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) TERNARY(or32)
    static void or64(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) TERNARY(or64)

    static void not8(Assembly& as, ASMVal dst, ASMVal src) BINARY(not8)
    static void not16(Assembly& as, ASMVal dst, ASMVal src) BINARY(not16)
    static void not32(Assembly& as, ASMVal dst, ASMVal src) BINARY(not32)
    static void not64(Assembly& as, ASMVal dst, ASMVal src) BINARY(not64)

    static void shl8(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) TERNARY(shl8)
    static void shl16(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) TERNARY(shl16)
    static void shl32(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) TERNARY(shl32)
    static void shl64(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) TERNARY(shl64)

    static void shr8(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) TERNARY(shr8)
    static void shr16(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) TERNARY(shr16)
    static void shr32(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) TERNARY(shr32)
    static void shr64(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) TERNARY(shr64)

    static void sar8(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) TERNARY(sar8)
    static void sar16(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) TERNARY(sar16)
    static void sar32(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) TERNARY(sar32)
    static void sar64(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) TERNARY(sar64)

    static void rol8(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) TERNARY(rol8)
    static void rol16(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) TERNARY(rol16)
    static void rol32(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) TERNARY(rol32)
    static void rol64(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) TERNARY(rol64)

    static void ror8(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) TERNARY(ror8)
    static void ror16(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) TERNARY(ror16)
    static void ror32(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) TERNARY(ror32)
    static void ror64(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) TERNARY(ror64)

    static void bitc8(Assembly& as, ASMVal dst, ASMVal src) BINARY(bitc8)
    static void bitc16(Assembly& as, ASMVal dst, ASMVal src) BINARY(bitc16)
    static void bitc32(Assembly& as, ASMVal dst, ASMVal src) BINARY(bitc32)
    static void bitc64(Assembly& as, ASMVal dst, ASMVal src) BINARY(bitc64)

    static void lzc8(Assembly& as, ASMVal dst, ASMVal src) BINARY(lzc8)
    static void lzc16(Assembly& as, ASMVal dst, ASMVal src) BINARY(lzc16)
    static void lzc32(Assembly& as, ASMVal dst, ASMVal src) BINARY(lzc32)
    static void lzc64(Assembly& as, ASMVal dst, ASMVal src) BINARY(lzc64)

    static void tzc8(Assembly& as, ASMVal dst, ASMVal src) BINARY(tzc8)
    static void tzc16(Assembly& as, ASMVal dst, ASMVal src) BINARY(tzc16)
    static void tzc32(Assembly& as, ASMVal dst, ASMVal src) BINARY(tzc32)
    static void tzc64(Assembly& as, ASMVal dst, ASMVal src) BINARY(tzc64)

    // Comparisons

    static void isz(Assembly& as, ASMVal dst, ASMVal src) BINARY(isz)
    static void isnz(Assembly& as, ASMVal dst, ASMVal src) BINARY(isnz)

    static void ccc8(Assembly& as, Condition cc, ASMVal dst, ASMVal a, ASMVal b) CONDITIONAL(ccc8)
    static void ccc16(Assembly& as, Condition cc, ASMVal dst, ASMVal a, ASMVal b) CONDITIONAL(ccc16)
    static void ccc32(Assembly& as, Condition cc, ASMVal dst, ASMVal a, ASMVal b) CONDITIONAL(ccc32)
    static void ccc64(Assembly& as, Condition cc, ASMVal dst, ASMVal a, ASMVal b) CONDITIONAL(ccc64)

    static void fccc32(Assembly& as, FloatCondition cc, ASMVal dst, ASMVal a, ASMVal b) CONDITIONAL(fccc32)
    static void fccc64(Assembly& as, FloatCondition cc, ASMVal dst, ASMVal a, ASMVal b) CONDITIONAL(fccc64)

    // Memory

    static void push8(Assembly& as, ASMVal dst) UNARY(push8)
    static void push16(Assembly& as, ASMVal dst) UNARY(push16)
    static void push32(Assembly& as, ASMVal dst) UNARY(push32)
    static void push64(Assembly& as, ASMVal dst) UNARY(push64)

    static void pop8(Assembly& as, ASMVal dst) UNARY(pop8)
    static void pop16(Assembly& as, ASMVal dst) UNARY(pop16)
    static void pop32(Assembly& as, ASMVal dst) UNARY(pop32)
    static void pop64(Assembly& as, ASMVal dst) UNARY(pop64)

    static void fpush32(Assembly& as, ASMVal dst) UNARY(fpush32)
    static void fpush64(Assembly& as, ASMVal dst) UNARY(fpush64)

    static void fpop32(Assembly& as, ASMVal dst) UNARY(fpop32)
    static void fpop64(Assembly& as, ASMVal dst) UNARY(fpop64)

    static void mov8(Assembly& as, ASMVal dst, ASMVal src) BINARY(mov8)
    static void mov16(Assembly& as, ASMVal dst, ASMVal src) BINARY(mov16)
    static void mov32(Assembly& as, ASMVal dst, ASMVal src) BINARY(mov32)
    static void mov64(Assembly& as, ASMVal dst, ASMVal src) BINARY(mov64)

    static void fmov32(Assembly& as, ASMVal dst, ASMVal src) BINARY(fmov32)
    static void fmov64(Assembly& as, ASMVal dst, ASMVal src) BINARY(fmov64)

    static void ld8(Assembly& as, ASMVal dst, ASMVal src) BINARY(ld8)
    static void ld16(Assembly& as, ASMVal dst, ASMVal src) BINARY(ld16)
    static void ld32(Assembly& as, ASMVal dst, ASMVal src) BINARY(ld32)
    static void ld64(Assembly& as, ASMVal dst, ASMVal src) BINARY(ld64)

    static void st8(Assembly& as, ASMVal dst, ASMVal src) BINARY(st8)
    static void st16(Assembly& as, ASMVal dst, ASMVal src) BINARY(st16)
    static void st32(Assembly& as, ASMVal dst, ASMVal src) BINARY(st32)
    static void st64(Assembly& as, ASMVal dst, ASMVal src) BINARY(st64)
    
    static void ldi8(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) TERNARY(ldi8)
    static void ldi16(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) TERNARY(ldi16)
    static void ldi32(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) TERNARY(ldi32)
    static void ldi64(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) TERNARY(ldi64)
    
    static void ldai8(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) TERNARY(ldai8)
    static void ldai16(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) TERNARY(ldai16)
    static void ldai32(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) TERNARY(ldai32)
    static void ldai64(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) TERNARY(ldai64)
    
    static void sti8(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) TERNARY(sti8)
    static void sti16(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) TERNARY(sti16)
    static void sti32(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) TERNARY(sti32)
    static void sti64(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) TERNARY(sti64)

    static void fld32(Assembly& as, ASMVal dst, ASMVal src) BINARY(fld32)
    static void fld64(Assembly& as, ASMVal dst, ASMVal src) BINARY(fld64)

    static void fst32(Assembly& as, ASMVal dst, ASMVal src) BINARY(fst32)
    static void fst64(Assembly& as, ASMVal dst, ASMVal src) BINARY(fst64)

    static void fldi32(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) TERNARY(fldi32)
    static void fldi64(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) TERNARY(fldi64)

    static void fsti32(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) TERNARY(fsti32)
    static void fsti64(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) TERNARY(fsti64)
    
    static void lda(Assembly& as, ASMVal dst, ASMVal src) BINARY(lda)

    static void ldc(Assembly& as, ASMVal dst, i64 imm) {
        A::ldc(as, dst, imm);
        B::ldc(as, dst, imm);
    }

    // Labels

    static void global(Assembly& as, stridx sym) {
        A::global(as, sym);
        B::global(as, sym);
    }

    static void local(Assembly& as, stridx sym) {
        A::local(as, sym);
        B::local(as, sym);
    }

    // Jumps

    static void j(Assembly& as, ASMVal dst) UNARY(j)

    static void jz(Assembly& as, ASMVal dst, ASMVal src) BINARY(jz)
    static void jnz(Assembly& as, ASMVal dst, ASMVal src) BINARY(jnz)

    static void jcc8(Assembly& as, Condition cc, ASMVal dst, ASMVal a, ASMVal b) CONDITIONAL(jcc8)
    static void jcc16(Assembly& as, Condition cc, ASMVal dst, ASMVal a, ASMVal b) CONDITIONAL(jcc16)
    static void jcc32(Assembly& as, Condition cc, ASMVal dst, ASMVal a, ASMVal b) CONDITIONAL(jcc32)
    static void jcc64(Assembly& as, Condition cc, ASMVal dst, ASMVal a, ASMVal b) CONDITIONAL(jcc64)
    
    static void fjcc32(Assembly& as, FloatCondition cc, ASMVal dst, ASMVal a, ASMVal b) CONDITIONAL(fjcc32)
    static void fjcc64(Assembly& as, FloatCondition cc, ASMVal dst, ASMVal a, ASMVal b) CONDITIONAL(fjcc64)

    // Functions

    static void enter(Assembly& as) NULLARY(enter)
    static void stack(Assembly& as, ASMVal dst) UNARY(stack)
    static void alloca(Assembly& as, ASMVal dst, ASMVal src) BINARY(alloca)
    static void unstack(Assembly& as, ASMVal dst) UNARY(unstack)
    static void leave(Assembly& as) NULLARY(leave)

    static void call(Assembly& as, ASMVal dst) UNARY(call)
    static void ret(Assembly& as) NULLARY(ret)

    // Conversions

    static void sxt8(Assembly& as, ASMVal dst, ASMVal src) BINARY(sxt8)
    static void sxt16(Assembly& as, ASMVal dst, ASMVal src) BINARY(sxt16)
    static void sxt32(Assembly& as, ASMVal dst, ASMVal src) BINARY(sxt32)

    static void zxt8(Assembly& as, ASMVal dst, ASMVal src) BINARY(zxt8)
    static void zxt16(Assembly& as, ASMVal dst, ASMVal src) BINARY(zxt16)
    static void zxt32(Assembly& as, ASMVal dst, ASMVal src) BINARY(zxt32)

    static void i8tof32(Assembly& as, ASMVal dst, ASMVal src) BINARY(i8tof32)
    static void i16tof32(Assembly& as, ASMVal dst, ASMVal src) BINARY(i16tof32)
    static void i32tof32(Assembly& as, ASMVal dst, ASMVal src) BINARY(i32tof32)
    static void i64tof32(Assembly& as, ASMVal dst, ASMVal src) BINARY(i64tof32)

    static void i8tof64(Assembly& as, ASMVal dst, ASMVal src) BINARY(i8tof64)
    static void i16tof64(Assembly& as, ASMVal dst, ASMVal src) BINARY(i16tof64)
    static void i32tof64(Assembly& as, ASMVal dst, ASMVal src) BINARY(i32tof64)
    static void i64tof64(Assembly& as, ASMVal dst, ASMVal src) BINARY(i64tof64)

    static void f32toi8(Assembly& as, ASMVal dst, ASMVal src) BINARY(f32toi8)
    static void f32toi16(Assembly& as, ASMVal dst, ASMVal src) BINARY(f32toi16)
    static void f32toi32(Assembly& as, ASMVal dst, ASMVal src) BINARY(f32toi32)
    static void f32toi64(Assembly& as, ASMVal dst, ASMVal src) BINARY(f32toi64)

    static void f64toi8(Assembly& as, ASMVal dst, ASMVal src) BINARY(f64toi8)
    static void f64toi16(Assembly& as, ASMVal dst, ASMVal src) BINARY(f64toi16)
    static void f64toi32(Assembly& as, ASMVal dst, ASMVal src) BINARY(f64toi32)
    static void f64toi64(Assembly& as, ASMVal dst, ASMVal src) BINARY(f64toi64)

    static void f32tof64(Assembly& as, ASMVal dst, ASMVal src) BINARY(f32tof64)
    static void f64tof32(Assembly& as, ASMVal dst, ASMVal src) BINARY(f64tof32)

    static void f32frombits(Assembly& as, ASMVal dst, ASMVal src) BINARY(f32frombits)
    static void f64frombits(Assembly& as, ASMVal dst, ASMVal src) BINARY(f64frombits)
    static void f32tobits(Assembly& as, ASMVal dst, ASMVal src) BINARY(f32tobits)
    static void f64tobits(Assembly& as, ASMVal dst, ASMVal src) BINARY(f64tobits)

    // Memory
    
    static void mcpy(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) TERNARY(mcpy)
    static void mmov(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) TERNARY(mmov)
    static void mset(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) TERNARY(mset)
    static void mcmp(Assembly& as, ASMVal dst, ASMVal a, ASMVal b, ASMVal c) QUATERNARY(mcmp)
};

void write_impl(fd io, const Binding&);

ENDMODULE()

#endif