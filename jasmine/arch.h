#ifndef BASIL_JASMINE_TARGET_H
#define BASIL_JASMINE_TARGET_H

#include "lib/buffer.h"
#include "lib/io.h"
#include "lib/tuple.h"
#include "jasmine/type.h"
#include "jasmine/tab.h"

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

using mreg = u8;   // Generic type of machine register.

inline void write(stream& io, const TargetDesc& target) {
    ::write(io, OS_NAMES[target.os], '_', ARCH_NAMES[target.arch]);
}

struct Function;
struct TypeTable;

enum SlotType {
    SLOT_NONE, SLOT_STACK, SLOT_GPREG, SLOT_FPREG, SLOT_ICONST
};

struct Slot {
    SlotType type = SLOT_NONE;

    union {
        mreg reg;
        mreg regpair[2];
        i64 stack;
    };
};

inline Slot gpreg_slot(mreg reg) {
    Slot s;
    s.type = SLOT_GPREG;
    s.reg = reg;
    return s;
}

inline Slot fpreg_slot(mreg reg) {
    Slot s;
    s.type = SLOT_FPREG;
    s.reg = reg;
    return s;
}

inline Slot stack_slot(i64 stack) {
    Slot s;
    s.type = SLOT_STACK;
    s.stack = stack;
    return s;
}

inline Slot empty_slot() {
    return Slot();
}

struct UsageState {
    u32 n_gpregs_used = 0, n_fpregs_used = 0, stack_used = 0;

    inline UsageState(u32 ngp, u32 nfp, u32 st): n_gpregs_used(ngp), n_fpregs_used(nfp), stack_used(st) {}

    inline UsageState& operator+=(const UsageState& other) {
        n_gpregs_used += other.n_gpregs_used;
        n_fpregs_used += other.n_fpregs_used;
        stack_used += other.stack_used;
        return *this;
    }
};

struct Placement {
    Slot slot;
    UsageState usage;
};

// Machine-level value.
union MVal {
    double dval;
    u64 uval;
    
    enum Kind : u16 { 
        F64 = 0xfff0,
        GPREG = 0xfff9,
        FPREG = 0xfffa,
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
            mreg gpreg;
            mreg fpreg;
            i32 offset;
            i32 sym;
        };
        MemKind memkind; 
        mreg base;
        Kind kind;
    } payload;

    inline static MVal fpreg(mreg r) {
        MVal m;
        m.uval = 0;
        m.payload.kind = FPREG;
        m.payload.base = m.payload.memkind = MemKind(0);
        m.payload.fpreg = r;
        return m;
    }

    inline static MVal gpreg(mreg r) {
        MVal m;
        m.uval = 0;
        m.payload.kind = GPREG;
        m.payload.base = m.payload.memkind = MemKind(0);
        m.payload.gpreg = r;
        return m;
    }

    inline static MVal imm(i32 imm) {
        MVal m;
        m.uval = 0;
        m.payload.kind = IMM;
        m.payload.base = m.payload.memkind = MemKind(0);
        m.payload.imm = imm;
        return m;
    }

    inline static MVal f32(float imm) {
        MVal m;
        m.uval = 0;
        m.payload.kind = F32;
        m.payload.base = m.payload.memkind = MemKind(0);
        m.payload.f32 = imm;
        return m;
    }

    inline static MVal f64(double imm) {
        MVal m;
        if (*(u64*)&imm > 0xfff8000000000000ul) // Purify NaNs
            *(u64*)&imm = 0xfff8000000000000ul;
        m.dval = imm;
        return m;
    }

    inline static MVal mem(mreg base, i32 offset) {
        MVal m;
        m.uval = 0;
        m.payload.kind = MEM;
        m.payload.memkind = REG_OFFSET;
        m.payload.base = base;
        m.payload.offset = offset;
        return m;
    }

    inline static MVal func(stridx sym) {
        MVal m;
        m.uval = 0;
        m.payload.kind = MEM;
        m.payload.memkind = FUNC_LABEL;
        m.payload.base = 0;
        m.payload.sym = sym;
        return m;
    }

    inline static MVal label(stridx sym) {
        MVal m;
        m.uval = 0;
        m.payload.kind = MEM;
        m.payload.memkind = LOCAL_LABEL;
        m.payload.base = 0;
        m.payload.sym = sym;
        return m;
    }

    inline static MVal data(stridx sym) {
        MVal m;
        m.uval = 0;
        m.payload.kind = MEM;
        m.payload.memkind = DATA_LABEL;
        m.payload.base = 0;
        m.payload.sym = sym;
        return m;
    }

    inline static MVal stat(stridx sym) {
        MVal m;
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

    inline bool operator==(MVal other) const {
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

enum class ASMOpcode {
    ADD8, ADD16, ADD32, ADD64,
    SUB8, SUB16, SUB32, SUB64,
    MUL8, MUL16, MUL32, MUL64,
    DIV8S, DIV16S, DIV32S, DIV64S,
    DIV8U, DIV16U, DIV32U, DIV64U,
    REM8S, REM16S, REM32S, REM64S,
    REM8U, REM16U, REM32U, REM64U,
    NEG8, NEG16, NEG32, NEG64,

    FADD32, FADD64,
    FSUB32, FSUB64,
    FMUL32, FMUL64,
    FDIV32, FDIV64,
    FREM32, FREM64,
    FNEG32, FNEG64,
    FSQRT32, FSQRT64,
    FSIN32, FSIN64,
    FCOS32, FCOS64,
    FTAN32, FTAN64,
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
    "div8s", "div16s", "div32s", "div64s",
    "div8u", "div16u", "div32u", "div64u",
    "rem8s", "rem16s", "rem32s", "rem64s",
    "rem8u", "rem16u", "rem32u", "rem64u",
    "neg8", "neg16", "neg32", "neg64",

    "fadd32", "fadd64",
    "fsub32", "fsub64",
    "fmul32", "fmul64",
    "fdiv32", "fdiv64",
    "frem32", "frem64",
    "fneg32", "fneg64",
    "fsqrt32", "fsqrt64",
    "fsin32", "fsin64",
    "fcos32", "fcos64",
    "ftan32", "ftan64",
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
    "cfcc32", "cfcc64",

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
    "jfcc8", "jfcc16", "jfcc32", "jfcc64",

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

    static inline const_slice<mreg> gpregs() {
        return const_slice<mreg>{GPREGS, 32};
    }

    static inline const_slice<mreg> fpregs() {
        return const_slice<mreg>{FPREGS, 32};
    }

    static inline bool is_gpreg(mreg r) {
        return r < 32;
    }

    static inline bool is_fpreg(mreg r) {
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

    static Placement place_ret(const TypeTable& tab, typeidx t) {
        return { {}, UsageState(0, 0, 0) };
    }

    static Placement place_arg(const TypeTable& tab, typeidx t, UsageState usage) {
        return { {}, UsageState(0, 0, 0) };
    }
};

// Used to mock up an existing backend, but instead write the instructions as textual representation to an output stream.
// You can use this as an example of the static interface of backend target types.
template<typename Target = FakeTarget>
struct ASMPrinter : public Target {
    static void write_mval(stream& io, Assembly& as, const MVal& val) {
        switch (val.kind()) {
            case MVal::FPREG:
                ::write(io, Target::reg_name(val.payload.fpreg));
                break;
            case MVal::GPREG:
                ::write(io, Target::reg_name(val.payload.gpreg));
                break;
            case MVal::IMM:
                ::write(io, val.payload.imm);
                break;
            case MVal::F32:
                ::write(io, val.payload.f32);
                break;
            case MVal::F64:
                ::write(io, val.dval);
                break;
            case MVal::MEM: switch (val.payload.memkind) {
                case MVal::REG_OFFSET:
                    ::write(io, '[', Target::reg_name(val.payload.base), val.payload.offset < 0 ? " - " : " + ", val.payload.offset < 0 ? -val.payload.offset : val.payload.offset, ']');
                    break;
                case MVal::FUNC_LABEL:
                    ::write(io, "func ", as.symtab->str(val.payload.sym));
                    break;
                case MVal::LOCAL_LABEL:
                    ::write(io, as.symtab->str(val.payload.sym));
                    break;
                case MVal::DATA_LABEL:
                    ::write(io, "data ", as.symtab->str(val.payload.sym));
                    break;
                case MVal::STATIC_LABEL:
                    ::write(io, "static ", as.symtab->str(val.payload.sym));
                    break;
            }
            break;
        }
    }

    static void write_label(stream& io, Assembly& as, const i8* prefix, stridx label) {
        ::write(io, prefix, as.symtab->str(label), ":\n");
    }

    static void write_nullary(stream& io, Assembly& as, ASMOpcode opcode) {
        ::write(io, "  ", ASM_OPCODE_NAMES[(unsigned)opcode], '\n');
    }

    static void write_unary(stream& io, Assembly& as, ASMOpcode opcode, const MVal& dst) {
        ::write(io, "  ", ASM_OPCODE_NAMES[(unsigned)opcode], ' ');
        write_mval(io, as, dst);
        ::write(io, '\n');
    }

    static void write_binary(stream& io, Assembly& as, ASMOpcode opcode, const MVal& dst, const MVal& src) {
        ::write(io, "  ", ASM_OPCODE_NAMES[(unsigned)opcode], ' ');
        write_mval(io, as, dst);
        ::write(io, ", ");
        write_mval(io, as, src);
        ::write(io, '\n');
    }

    static void write_ternary(stream& io, Assembly& as, ASMOpcode opcode, const MVal& dst, const MVal& a, const MVal& b) {
        ::write(io, "  ", ASM_OPCODE_NAMES[(unsigned)opcode], ' ');
        write_mval(io, as, dst);
        ::write(io, ", ");
        write_mval(io, as, a);
        ::write(io, ", ");
        write_mval(io, as, b);
        ::write(io, '\n');
    }

    static void write_quaternary(stream& io, Assembly& as, ASMOpcode opcode, const MVal& dst, const MVal& a, const MVal& b, const MVal& c) {
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

    static void write_compare(stream& io, Assembly& as, const i8* prefix, Condition condition, const i8* suffix, const MVal& dst, const MVal& a, const MVal& b) {
        ::write(io, "  ", prefix, ASM_CONDITION_NAMES[(unsigned)condition], suffix, ' ');
        write_mval(io, as, dst);
        ::write(io, ", ");
        write_mval(io, as, a);
        ::write(io, ", ");
        write_mval(io, as, b);
        ::write(io, '\n');
    }

    static stream* output;

    static void write_to(stream& output_in) {
        output = &output_in;
    }

    #define NULLARY(opcode) { write_nullary(*output, as, ASMOpcode::opcode); }
    #define UNARY(opcode) { write_unary(*output, as, ASMOpcode::opcode, dst); }
    #define BINARY(opcode) { write_binary(*output, as, ASMOpcode::opcode, dst, src); }
    #define TERNARY(opcode) { write_ternary(*output, as, ASMOpcode::opcode, dst, a, b); }
    #define QUATERNARY(opcode) { write_quaternary(*output, as, ASMOpcode::opcode, dst, a, b, c); }
    #define CONDITIONAL(p, s) { write_compare(*output, as, #p, cc, #s, dst, a, b); }
    #define FCONDITIONAL(p, s) { write_compare(*output, as, #p, (Condition)cc, #s, dst, a, b); }
    #define LABEL(p, label) { write_label(*output, as, p, sym); }

    // Arithmetic

    static void add8(Assembly& as, MVal dst, MVal a, MVal b) TERNARY(ADD8)
    static void add16(Assembly& as, MVal dst, MVal a, MVal b) TERNARY(ADD16)
    static void add32(Assembly& as, MVal dst, MVal a, MVal b) TERNARY(ADD32)
    static void add64(Assembly& as, MVal dst, MVal a, MVal b) TERNARY(ADD64)

    static void sub8(Assembly& as, MVal dst, MVal a, MVal b) TERNARY(SUB8)
    static void sub16(Assembly& as, MVal dst, MVal a, MVal b) TERNARY(SUB16)
    static void sub32(Assembly& as, MVal dst, MVal a, MVal b) TERNARY(SUB32)
    static void sub64(Assembly& as, MVal dst, MVal a, MVal b) TERNARY(SUB64)

    static void mul8(Assembly& as, MVal dst, MVal a, MVal b) TERNARY(MUL8)
    static void mul16(Assembly& as, MVal dst, MVal a, MVal b) TERNARY(MUL16)
    static void mul32(Assembly& as, MVal dst, MVal a, MVal b) TERNARY(MUL32)
    static void mul64(Assembly& as, MVal dst, MVal a, MVal b) TERNARY(MUL64)

    static void div8s(Assembly& as, MVal dst, MVal a, MVal b) TERNARY(DIV8S)
    static void div16s(Assembly& as, MVal dst, MVal a, MVal b) TERNARY(DIV16S)
    static void div32s(Assembly& as, MVal dst, MVal a, MVal b) TERNARY(DIV32S)
    static void div64s(Assembly& as, MVal dst, MVal a, MVal b) TERNARY(DIV64S)

    static void div8u(Assembly& as, MVal dst, MVal a, MVal b) TERNARY(DIV8U)
    static void div16u(Assembly& as, MVal dst, MVal a, MVal b) TERNARY(DIV16U)
    static void div32u(Assembly& as, MVal dst, MVal a, MVal b) TERNARY(DIV32U)
    static void div64u(Assembly& as, MVal dst, MVal a, MVal b) TERNARY(DIV64U)

    static void rem8s(Assembly& as, MVal dst, MVal a, MVal b) TERNARY(REM8S)
    static void rem16s(Assembly& as, MVal dst, MVal a, MVal b) TERNARY(REM16S)
    static void rem32s(Assembly& as, MVal dst, MVal a, MVal b) TERNARY(REM32S)
    static void rem64s(Assembly& as, MVal dst, MVal a, MVal b) TERNARY(REM64S)

    static void rem8u(Assembly& as, MVal dst, MVal a, MVal b) TERNARY(REM8U)
    static void rem16u(Assembly& as, MVal dst, MVal a, MVal b) TERNARY(REM16U)
    static void rem32u(Assembly& as, MVal dst, MVal a, MVal b) TERNARY(REM32U)
    static void rem64u(Assembly& as, MVal dst, MVal a, MVal b) TERNARY(REM64U)

    static void neg8(Assembly& as, MVal dst, MVal src) BINARY(NEG8)
    static void neg16(Assembly& as, MVal dst, MVal src) BINARY(NEG16)
    static void neg32(Assembly& as, MVal dst, MVal src) BINARY(NEG32)
    static void neg64(Assembly& as, MVal dst, MVal src) BINARY(NEG64)

    // Floating-Point Arithmetic

    static void fadd32(Assembly& as, MVal dst, MVal a, MVal b) TERNARY(FADD32)
    static void fadd64(Assembly& as, MVal dst, MVal a, MVal b) TERNARY(FADD64)

    static void fsub32(Assembly& as, MVal dst, MVal a, MVal b) TERNARY(FSUB32)
    static void fsub64(Assembly& as, MVal dst, MVal a, MVal b) TERNARY(FSUB64)

    static void fmul32(Assembly& as, MVal dst, MVal a, MVal b) TERNARY(FMUL32)
    static void fmul64(Assembly& as, MVal dst, MVal a, MVal b) TERNARY(FMUL64)

    static void fdiv32(Assembly& as, MVal dst, MVal a, MVal b) TERNARY(FDIV32)
    static void fdiv64(Assembly& as, MVal dst, MVal a, MVal b) TERNARY(FDIV64)

    static void frem32(Assembly& as, MVal dst, MVal a, MVal b) TERNARY(FREM32)
    static void frem64(Assembly& as, MVal dst, MVal a, MVal b) TERNARY(FREM64)

    static void fneg32(Assembly& as, MVal dst, MVal src) BINARY(FNEG32)
    static void fneg64(Assembly& as, MVal dst, MVal src) BINARY(FNEG64)

    static void fsqrt32(Assembly& as, MVal dst, MVal src) BINARY(FSQRT32)
    static void fsqrt64(Assembly& as, MVal dst, MVal src) BINARY(FSQRT64)

    static void fsin32(Assembly& as, MVal dst, MVal src) BINARY(FSIN32)
    static void fsin64(Assembly& as, MVal dst, MVal src) BINARY(FSIN64)

    static void fcos32(Assembly& as, MVal dst, MVal src) BINARY(FCOS32)
    static void fcos64(Assembly& as, MVal dst, MVal src) BINARY(FCOS64)

    static void ftan32(Assembly& as, MVal dst, MVal src) BINARY(FTAN32)
    static void ftan64(Assembly& as, MVal dst, MVal src) BINARY(FTAN64)

    static void fround32(Assembly& as, MVal dst, MVal src) BINARY(FROUND32)
    static void fround64(Assembly& as, MVal dst, MVal src) BINARY(FROUND64)

    static void ffloor32(Assembly& as, MVal dst, MVal src) BINARY(FFLOOR32)
    static void ffloor64(Assembly& as, MVal dst, MVal src) BINARY(FFLOOR64)

    static void fceil32(Assembly& as, MVal dst, MVal src) BINARY(FCEIL32)
    static void fceil64(Assembly& as, MVal dst, MVal src) BINARY(FCEIL64)

    static void fmin32(Assembly& as, MVal dst, MVal a, MVal b) TERNARY(FMIN32)
    static void fmin64(Assembly& as, MVal dst, MVal a, MVal b) TERNARY(FMIN64)

    static void fmax32(Assembly& as, MVal dst, MVal a, MVal b) TERNARY(FMIN64)
    static void fmax64(Assembly& as, MVal dst, MVal a, MVal b) TERNARY(FMIN64)

    static void fabs32(Assembly& as, MVal dst, MVal src) BINARY(FABS32)
    static void fabs64(Assembly& as, MVal dst, MVal src) BINARY(FABS64)

    // Bitwise Operations

    static void and8(Assembly& as, MVal dst, MVal a, MVal b) TERNARY(AND8)
    static void and16(Assembly& as, MVal dst, MVal a, MVal b) TERNARY(AND16)
    static void and32(Assembly& as, MVal dst, MVal a, MVal b) TERNARY(AND32)
    static void and64(Assembly& as, MVal dst, MVal a, MVal b) TERNARY(AND64)

    static void xor8(Assembly& as, MVal dst, MVal a, MVal b) TERNARY(XOR8)
    static void xor16(Assembly& as, MVal dst, MVal a, MVal b) TERNARY(XOR16)
    static void xor32(Assembly& as, MVal dst, MVal a, MVal b) TERNARY(XOR32)
    static void xor64(Assembly& as, MVal dst, MVal a, MVal b) TERNARY(XOR64)

    static void or8(Assembly& as, MVal dst, MVal a, MVal b) TERNARY(OR8)
    static void or16(Assembly& as, MVal dst, MVal a, MVal b) TERNARY(OR16)
    static void or32(Assembly& as, MVal dst, MVal a, MVal b) TERNARY(OR32)
    static void or64(Assembly& as, MVal dst, MVal a, MVal b) TERNARY(OR64)

    static void not8(Assembly& as, MVal dst, MVal src) BINARY(NOT8)
    static void not16(Assembly& as, MVal dst, MVal src) BINARY(NOT16)
    static void not32(Assembly& as, MVal dst, MVal src) BINARY(NOT32)
    static void not64(Assembly& as, MVal dst, MVal src) BINARY(NOT64)

    static void shl8(Assembly& as, MVal dst, MVal a, MVal b) TERNARY(SHL8)
    static void shl16(Assembly& as, MVal dst, MVal a, MVal b) TERNARY(SHL16)
    static void shl32(Assembly& as, MVal dst, MVal a, MVal b) TERNARY(SHL32)
    static void shl64(Assembly& as, MVal dst, MVal a, MVal b) TERNARY(SHL64)

    static void shr8(Assembly& as, MVal dst, MVal a, MVal b) TERNARY(SHR8)
    static void shr16(Assembly& as, MVal dst, MVal a, MVal b) TERNARY(SHR16)
    static void shr32(Assembly& as, MVal dst, MVal a, MVal b) TERNARY(SHR32)
    static void shr64(Assembly& as, MVal dst, MVal a, MVal b) TERNARY(SHR64)

    static void sar8(Assembly& as, MVal dst, MVal a, MVal b) TERNARY(SAR8)
    static void sar16(Assembly& as, MVal dst, MVal a, MVal b) TERNARY(SAR16)
    static void sar32(Assembly& as, MVal dst, MVal a, MVal b) TERNARY(SAR32)
    static void sar64(Assembly& as, MVal dst, MVal a, MVal b) TERNARY(SAR64)

    static void rol8(Assembly& as, MVal dst, MVal a, MVal b) TERNARY(ROL8)
    static void rol16(Assembly& as, MVal dst, MVal a, MVal b) TERNARY(ROL16)
    static void rol32(Assembly& as, MVal dst, MVal a, MVal b) TERNARY(ROL32)
    static void rol64(Assembly& as, MVal dst, MVal a, MVal b) TERNARY(ROL64)

    static void ror8(Assembly& as, MVal dst, MVal a, MVal b) TERNARY(ROR8)
    static void ror16(Assembly& as, MVal dst, MVal a, MVal b) TERNARY(ROR16)
    static void ror32(Assembly& as, MVal dst, MVal a, MVal b) TERNARY(ROR32)
    static void ror64(Assembly& as, MVal dst, MVal a, MVal b) TERNARY(ROR64)

    static void bitc8(Assembly& as, MVal dst, MVal src) BINARY(BITC8)
    static void bitc16(Assembly& as, MVal dst, MVal src) BINARY(BITC16)
    static void bitc32(Assembly& as, MVal dst, MVal src) BINARY(BITC32)
    static void bitc64(Assembly& as, MVal dst, MVal src) BINARY(BITC64)

    static void lzc8(Assembly& as, MVal dst, MVal src) BINARY(LZC8)
    static void lzc16(Assembly& as, MVal dst, MVal src) BINARY(LZC16)
    static void lzc32(Assembly& as, MVal dst, MVal src) BINARY(LZC32)
    static void lzc64(Assembly& as, MVal dst, MVal src) BINARY(LZC64)

    static void tzc8(Assembly& as, MVal dst, MVal src) BINARY(TZC8)
    static void tzc16(Assembly& as, MVal dst, MVal src) BINARY(TZC16)
    static void tzc32(Assembly& as, MVal dst, MVal src) BINARY(TZC32)
    static void tzc64(Assembly& as, MVal dst, MVal src) BINARY(TZC64)

    // Comparisons

    static void isz(Assembly& as, MVal dst, MVal src) BINARY(ISZ)
    static void isnz(Assembly& as, MVal dst, MVal src) BINARY(ISNZ)

    static void ccc8(Assembly& as, Condition cc, MVal dst, MVal a, MVal b) CONDITIONAL(c, 8)
    static void ccc16(Assembly& as, Condition cc, MVal dst, MVal a, MVal b) CONDITIONAL(c, 16)
    static void ccc32(Assembly& as, Condition cc, MVal dst, MVal a, MVal b) CONDITIONAL(c, 32)
    static void ccc64(Assembly& as, Condition cc, MVal dst, MVal a, MVal b) CONDITIONAL(c, 64)

    static void cfcc32(Assembly& as, FloatCondition cc, MVal dst, MVal a, MVal b) FCONDITIONAL(cf, 32)
    static void cfcc64(Assembly& as, FloatCondition cc, MVal dst, MVal a, MVal b) FCONDITIONAL(cf, 64)

    // Memory

    static void push8(Assembly& as, MVal dst) UNARY(PUSH8)
    static void push16(Assembly& as, MVal dst) UNARY(PUSH16)
    static void push32(Assembly& as, MVal dst) UNARY(PUSH32)
    static void push64(Assembly& as, MVal dst) UNARY(PUSH64)

    static void pop8(Assembly& as, MVal dst) UNARY(POP8)
    static void pop16(Assembly& as, MVal dst) UNARY(POP16)
    static void pop32(Assembly& as, MVal dst) UNARY(POP32)
    static void pop64(Assembly& as, MVal dst) UNARY(POP64)

    static void fpush32(Assembly& as, MVal dst) UNARY(FPUSH32)
    static void fpush64(Assembly& as, MVal dst) UNARY(FPUSH64)

    static void fpop32(Assembly& as, MVal dst) UNARY(FPOP32)
    static void fpop64(Assembly& as, MVal dst) UNARY(FPOP64)

    static void mov8(Assembly& as, MVal dst, MVal src) BINARY(MOV8)
    static void mov16(Assembly& as, MVal dst, MVal src) BINARY(MOV16)
    static void mov32(Assembly& as, MVal dst, MVal src) BINARY(MOV32)
    static void mov64(Assembly& as, MVal dst, MVal src) BINARY(MOV64)

    static void fmov32(Assembly& as, MVal dst, MVal src) BINARY(FMOV32)
    static void fmov64(Assembly& as, MVal dst, MVal src) BINARY(FMOV64)

    static void ld8(Assembly& as, MVal dst, MVal src) BINARY(LD8)
    static void ld16(Assembly& as, MVal dst, MVal src) BINARY(LD16)
    static void ld32(Assembly& as, MVal dst, MVal src) BINARY(LD32)
    static void ld64(Assembly& as, MVal dst, MVal src) BINARY(LD64)

    static void st8(Assembly& as, MVal dst, MVal src) BINARY(ST8)
    static void st16(Assembly& as, MVal dst, MVal src) BINARY(ST16)
    static void st32(Assembly& as, MVal dst, MVal src) BINARY(ST32)
    static void st64(Assembly& as, MVal dst, MVal src) BINARY(ST64)
    
    static void ldi8(Assembly& as, MVal dst, MVal a, MVal b) TERNARY(LDI8)
    static void ldi16(Assembly& as, MVal dst, MVal a, MVal b) TERNARY(LDI16)
    static void ldi32(Assembly& as, MVal dst, MVal a, MVal b) TERNARY(LDI32)
    static void ldi64(Assembly& as, MVal dst, MVal a, MVal b) TERNARY(LDI64)
    
    static void ldai8(Assembly& as, MVal dst, MVal a, MVal b) TERNARY(LDAI8)
    static void ldai16(Assembly& as, MVal dst, MVal a, MVal b) TERNARY(LDAI16)
    static void ldai32(Assembly& as, MVal dst, MVal a, MVal b) TERNARY(LDAI32)
    static void ldai64(Assembly& as, MVal dst, MVal a, MVal b) TERNARY(LDAI64)
    
    static void sti8(Assembly& as, MVal dst, MVal a, MVal b) TERNARY(STI8)
    static void sti16(Assembly& as, MVal dst, MVal a, MVal b) TERNARY(STI16)
    static void sti32(Assembly& as, MVal dst, MVal a, MVal b) TERNARY(STI32)
    static void sti64(Assembly& as, MVal dst, MVal a, MVal b) TERNARY(STI64)

    static void fld32(Assembly& as, MVal dst, MVal src) BINARY(FLD32)
    static void fld64(Assembly& as, MVal dst, MVal src) BINARY(FLD64)

    static void fst32(Assembly& as, MVal dst, MVal src) BINARY(FST32)
    static void fst64(Assembly& as, MVal dst, MVal src) BINARY(FST64)

    static void fldi32(Assembly& as, MVal dst, MVal a, MVal b) TERNARY(FLDI32)
    static void fldi64(Assembly& as, MVal dst, MVal a, MVal b) TERNARY(FLDI64)

    static void fsti32(Assembly& as, MVal dst, MVal a, MVal b) TERNARY(FSTI32)
    static void fsti64(Assembly& as, MVal dst, MVal a, MVal b) TERNARY(FSTI64)
    
    static void lda(Assembly& as, MVal dst, MVal src) BINARY(LDA)

    static void ldc(Assembly& as, MVal dst, i64 imm) {
        ::write(*output, "  ", ASM_OPCODE_NAMES[(unsigned)ASMOpcode::LDC], ' ', dst, ", ", imm, '\n');
    }

    // Labels

    static void global(Assembly& as, stridx sym) LABEL("", sym)
    static void local(Assembly& as, stridx sym) LABEL(".", sym)

    // Jumps

    static void j(Assembly& as, MVal dst) UNARY(J)

    static void jz(Assembly& as, MVal dst, MVal src) BINARY(JZ)
    static void jnz(Assembly& as, MVal dst, MVal src) BINARY(JNZ)

    static void jcc8(Assembly& as, Condition cc, MVal dst, MVal a, MVal b) CONDITIONAL(j, 8)
    static void jcc16(Assembly& as, Condition cc, MVal dst, MVal a, MVal b) CONDITIONAL(j, 16)
    static void jcc32(Assembly& as, Condition cc, MVal dst, MVal a, MVal b) CONDITIONAL(j, 32)
    static void jcc64(Assembly& as, Condition cc, MVal dst, MVal a, MVal b) CONDITIONAL(j, 64)
    
    static void jfcc32(Assembly& as, FloatCondition cc, MVal dst, MVal a, MVal b) FCONDITIONAL(jf, 32)
    static void jfcc64(Assembly& as, FloatCondition cc, MVal dst, MVal a, MVal b) FCONDITIONAL(jf, 64)

    // Functions

    static void enter(Assembly& as) NULLARY(ENTER)
    static void stack(Assembly& as, MVal dst) UNARY(STACK)
    static void alloca(Assembly& as, MVal dst, MVal src) BINARY(ALLOCA)
    static void unstack(Assembly& as, MVal dst) UNARY(UNSTACK)
    static void leave(Assembly& as) NULLARY(LEAVE)

    static void call(Assembly& as, MVal dst) UNARY(CALL)
    static void ret(Assembly& as) NULLARY(RET)

    // Conversions

    static void sxt8(Assembly& as, MVal dst, MVal src) BINARY(SXT8)
    static void sxt16(Assembly& as, MVal dst, MVal src) BINARY(SXT16)
    static void sxt32(Assembly& as, MVal dst, MVal src) BINARY(SXT32)

    static void zxt8(Assembly& as, MVal dst, MVal src) BINARY(ZXT8)
    static void zxt16(Assembly& as, MVal dst, MVal src) BINARY(ZXT16)
    static void zxt32(Assembly& as, MVal dst, MVal src) BINARY(ZXT32)

    static void i8tof32(Assembly& as, MVal dst, MVal src) BINARY(I8TOF32)
    static void i16tof32(Assembly& as, MVal dst, MVal src) BINARY(I16TOF32)
    static void i32tof32(Assembly& as, MVal dst, MVal src) BINARY(I32TOF32)
    static void i64tof32(Assembly& as, MVal dst, MVal src) BINARY(I64TOF32)

    static void i8tof64(Assembly& as, MVal dst, MVal src) BINARY(I8TOF64)
    static void i16tof64(Assembly& as, MVal dst, MVal src) BINARY(I16TOF64)
    static void i32tof64(Assembly& as, MVal dst, MVal src) BINARY(I32TOF64)
    static void i64tof64(Assembly& as, MVal dst, MVal src) BINARY(I64TOF64)

    static void f32toi8(Assembly& as, MVal dst, MVal src) BINARY(F32TOI8)
    static void f32toi16(Assembly& as, MVal dst, MVal src) BINARY(F32TOI16)
    static void f32toi32(Assembly& as, MVal dst, MVal src) BINARY(F32TOI32)
    static void f32toi64(Assembly& as, MVal dst, MVal src) BINARY(F32TOI64)

    static void f64toi8(Assembly& as, MVal dst, MVal src) BINARY(F64TOI8)
    static void f64toi16(Assembly& as, MVal dst, MVal src) BINARY(F64TOI16)
    static void f64toi32(Assembly& as, MVal dst, MVal src) BINARY(F64TOI32)
    static void f64toi64(Assembly& as, MVal dst, MVal src) BINARY(F64TOI64)

    static void f32tof64(Assembly& as, MVal dst, MVal src) BINARY(F32TOF64)
    static void f64tof32(Assembly& as, MVal dst, MVal src) BINARY(F64TOF32)

    static void f32frombits(Assembly& as, MVal dst, MVal src) BINARY(F32FROMBITS)
    static void f64frombits(Assembly& as, MVal dst, MVal src) BINARY(F64FROMBITS)
    static void f32tobits(Assembly& as, MVal dst, MVal src) BINARY(F32TOBITS)
    static void f64tobits(Assembly& as, MVal dst, MVal src) BINARY(F64TOBITS)

    // Memory
    
    static void mcpy(Assembly& as, MVal dst, MVal a, MVal b) TERNARY(MCPY)
    static void mmov(Assembly& as, MVal dst, MVal a, MVal b) TERNARY(MMOV)
    static void mset(Assembly& as, MVal dst, MVal a, MVal b) TERNARY(MSET)
    static void mcmp(Assembly& as, MVal dst, MVal a, MVal b, MVal c) QUATERNARY(MCMP)

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
stream* ASMPrinter<Target>::output = nullptr;

// Verifies instructions both independent of target and using target information if available.
template<typename Target = FakeTarget>
struct ASMVerifier : public Target {
    #define GP(x) assert(x.kind() == MVal::GPREG); assert(Target::is_gpreg(x.payload.gpreg))
    #define FP(x) assert(x.kind() == MVal::FPREG); assert(Target::is_fpreg(x.payload.fpreg))
    #define IMM(x) assert(x.kind() == MVal::IMM)
    #define GP_OR_IMM(x) assert(x.kind() == MVal::IMM || x.kind() == MVal::GPREG); if (x.kind() == MVal::GPREG) assert(Target::is_gpreg(x.payload.gpreg));
    #define FP_OR_F32(x) assert(x.kind() == MVal::F32 || x.kind() == MVal::FPREG); if (x.kind() == MVal::FPREG) assert(Target::is_fpreg(x.payload.fpreg));
    #define FP_OR_F64(x) assert(x.kind() == MVal::F64 || x.kind() == MVal::FPREG); if (x.kind() == MVal::FPREG) assert(Target::is_fpreg(x.payload.fpreg));
    #define ONLY_ONE_IMM(x, y) assert(x.kind() != MVal::IMM || y.kind() != MVal::IMM);
    #define ONLY_ONE_F32(x, y) assert(x.kind() != MVal::F32 || y.kind() != MVal::F32);
    #define ONLY_ONE_F64(x, y) assert(x.kind() != MVal::F64 || y.kind() != MVal::F64);
    #define MEM(x) assert(x.kind() == MVal::MEM); if (x.payload.memkind == MVal::REG_OFFSET) assert(Target::is_gpreg(x.payload.base));
    #define LABEL(x) assert(x.kind() == MVal::MEM && x.payload.memkind != MVal::REG_OFFSET)
    #define GP_OR_LABEL(x) assert(x.kind() == MVal::GPREG || (x.kind() == MVal::MEM && x.payload.memkind != MVal::REG_OFFSET))
    #define TRUE(a, b) (void)true

    #define NULLARY {}
    #define UNARY(DSTPRED) { DSTPRED(dst); }
    #define BINARY(DSTPRED, SRCPRED) { DSTPRED(dst); SRCPRED(src); }
    #define TERNARY(DSTPRED, APRED, BPRED, ABPRED) { DSTPRED(dst); APRED(a); BPRED(b); ABPRED(a, b); }
    #define QUATERNARY(DSTPRED, APRED, BPRED, CPRED) { DSTPRED(dst); APRED(a); BPRED(b); CPRED(c); }

    static void add8(Assembly& as, MVal dst, MVal a, MVal b) TERNARY(GP, GP_OR_IMM, GP_OR_IMM, ONLY_ONE_IMM)
    static void add16(Assembly& as, MVal dst, MVal a, MVal b) TERNARY(GP, GP_OR_IMM, GP_OR_IMM, ONLY_ONE_IMM)
    static void add32(Assembly& as, MVal dst, MVal a, MVal b) TERNARY(GP, GP_OR_IMM, GP_OR_IMM, ONLY_ONE_IMM)
    static void add64(Assembly& as, MVal dst, MVal a, MVal b) TERNARY(GP, GP_OR_IMM, GP_OR_IMM, ONLY_ONE_IMM)

    static void sub8(Assembly& as, MVal dst, MVal a, MVal b) TERNARY(GP, GP_OR_IMM, GP_OR_IMM, ONLY_ONE_IMM)
    static void sub16(Assembly& as, MVal dst, MVal a, MVal b) TERNARY(GP, GP_OR_IMM, GP_OR_IMM, ONLY_ONE_IMM)
    static void sub32(Assembly& as, MVal dst, MVal a, MVal b) TERNARY(GP, GP_OR_IMM, GP_OR_IMM, ONLY_ONE_IMM)
    static void sub64(Assembly& as, MVal dst, MVal a, MVal b) TERNARY(GP, GP_OR_IMM, GP_OR_IMM, ONLY_ONE_IMM)

    static void mul8(Assembly& as, MVal dst, MVal a, MVal b) TERNARY(GP, GP_OR_IMM, GP_OR_IMM, ONLY_ONE_IMM)
    static void mul16(Assembly& as, MVal dst, MVal a, MVal b) TERNARY(GP, GP_OR_IMM, GP_OR_IMM, ONLY_ONE_IMM)
    static void mul32(Assembly& as, MVal dst, MVal a, MVal b) TERNARY(GP, GP_OR_IMM, GP_OR_IMM, ONLY_ONE_IMM)
    static void mul64(Assembly& as, MVal dst, MVal a, MVal b) TERNARY(GP, GP_OR_IMM, GP_OR_IMM, ONLY_ONE_IMM)

    static void div8s(Assembly& as, MVal dst, MVal a, MVal b) TERNARY(GP, GP_OR_IMM, GP_OR_IMM, ONLY_ONE_IMM)
    static void div16s(Assembly& as, MVal dst, MVal a, MVal b) TERNARY(GP, GP_OR_IMM, GP_OR_IMM, ONLY_ONE_IMM)
    static void div32s(Assembly& as, MVal dst, MVal a, MVal b) TERNARY(GP, GP_OR_IMM, GP_OR_IMM, ONLY_ONE_IMM)
    static void div64s(Assembly& as, MVal dst, MVal a, MVal b) TERNARY(GP, GP_OR_IMM, GP_OR_IMM, ONLY_ONE_IMM)

    static void div8u(Assembly& as, MVal dst, MVal a, MVal b) TERNARY(GP, GP_OR_IMM, GP_OR_IMM, ONLY_ONE_IMM)
    static void div16u(Assembly& as, MVal dst, MVal a, MVal b) TERNARY(GP, GP_OR_IMM, GP_OR_IMM, ONLY_ONE_IMM)
    static void div32u(Assembly& as, MVal dst, MVal a, MVal b) TERNARY(GP, GP_OR_IMM, GP_OR_IMM, ONLY_ONE_IMM)
    static void div64u(Assembly& as, MVal dst, MVal a, MVal b) TERNARY(GP, GP_OR_IMM, GP_OR_IMM, ONLY_ONE_IMM)

    static void rem8s(Assembly& as, MVal dst, MVal a, MVal b) TERNARY(GP, GP_OR_IMM, GP_OR_IMM, ONLY_ONE_IMM)
    static void rem16s(Assembly& as, MVal dst, MVal a, MVal b) TERNARY(GP, GP_OR_IMM, GP_OR_IMM, ONLY_ONE_IMM)
    static void rem32s(Assembly& as, MVal dst, MVal a, MVal b) TERNARY(GP, GP_OR_IMM, GP_OR_IMM, ONLY_ONE_IMM)
    static void rem64s(Assembly& as, MVal dst, MVal a, MVal b) TERNARY(GP, GP_OR_IMM, GP_OR_IMM, ONLY_ONE_IMM)

    static void rem8u(Assembly& as, MVal dst, MVal a, MVal b) TERNARY(GP, GP_OR_IMM, GP_OR_IMM, ONLY_ONE_IMM)
    static void rem16u(Assembly& as, MVal dst, MVal a, MVal b) TERNARY(GP, GP_OR_IMM, GP_OR_IMM, ONLY_ONE_IMM)
    static void rem32u(Assembly& as, MVal dst, MVal a, MVal b) TERNARY(GP, GP_OR_IMM, GP_OR_IMM, ONLY_ONE_IMM)
    static void rem64u(Assembly& as, MVal dst, MVal a, MVal b) TERNARY(GP, GP_OR_IMM, GP_OR_IMM, ONLY_ONE_IMM)

    static void neg8(Assembly& as, MVal dst, MVal src) BINARY(GP, GP)
    static void neg16(Assembly& as, MVal dst, MVal src) BINARY(GP, GP)
    static void neg32(Assembly& as, MVal dst, MVal src) BINARY(GP, GP)
    static void neg64(Assembly& as, MVal dst, MVal src) BINARY(GP, GP)

    // Floating-Point Arithmetic

    static void fadd32(Assembly& as, MVal dst, MVal a, MVal b) TERNARY(FP, FP_OR_F32, FP_OR_F32, ONLY_ONE_F32)
    static void fadd64(Assembly& as, MVal dst, MVal a, MVal b) TERNARY(FP, FP_OR_F64, FP_OR_F64, ONLY_ONE_F64)

    static void fsub32(Assembly& as, MVal dst, MVal a, MVal b) TERNARY(FP, FP_OR_F32, FP_OR_F32, ONLY_ONE_F32)
    static void fsub64(Assembly& as, MVal dst, MVal a, MVal b) TERNARY(FP, FP_OR_F64, FP_OR_F64, ONLY_ONE_F64)

    static void fmul32(Assembly& as, MVal dst, MVal a, MVal b) TERNARY(FP, FP_OR_F32, FP_OR_F32, ONLY_ONE_F32)
    static void fmul64(Assembly& as, MVal dst, MVal a, MVal b) TERNARY(FP, FP_OR_F64, FP_OR_F64, ONLY_ONE_F64)

    static void fdiv32(Assembly& as, MVal dst, MVal a, MVal b) TERNARY(FP, FP_OR_F32, FP_OR_F32, ONLY_ONE_F32)
    static void fdiv64(Assembly& as, MVal dst, MVal a, MVal b) TERNARY(FP, FP_OR_F64, FP_OR_F64, ONLY_ONE_F64)

    static void frem32(Assembly& as, MVal dst, MVal a, MVal b) TERNARY(FP, FP_OR_F32, FP_OR_F32, ONLY_ONE_F32)
    static void frem64(Assembly& as, MVal dst, MVal a, MVal b) TERNARY(FP, FP_OR_F64, FP_OR_F64, ONLY_ONE_F64)

    static void fneg32(Assembly& as, MVal dst, MVal src) BINARY(FP, FP)
    static void fneg64(Assembly& as, MVal dst, MVal src) BINARY(FP, FP)

    static void fsqrt32(Assembly& as, MVal dst, MVal src) BINARY(FP, FP)
    static void fsqrt64(Assembly& as, MVal dst, MVal src) BINARY(FP, FP)

    static void fsin32(Assembly& as, MVal dst, MVal src) BINARY(FP, FP)
    static void fsin64(Assembly& as, MVal dst, MVal src) BINARY(FP, FP)

    static void fcos32(Assembly& as, MVal dst, MVal src) BINARY(FP, FP)
    static void fcos64(Assembly& as, MVal dst, MVal src) BINARY(FP, FP)

    static void ftan32(Assembly& as, MVal dst, MVal src) BINARY(FP, FP)
    static void ftan64(Assembly& as, MVal dst, MVal src) BINARY(FP, FP)

    static void fround32(Assembly& as, MVal dst, MVal src) BINARY(FP, FP)
    static void fround64(Assembly& as, MVal dst, MVal src) BINARY(FP, FP)

    static void ffloor32(Assembly& as, MVal dst, MVal src) BINARY(FP, FP)
    static void ffloor64(Assembly& as, MVal dst, MVal src) BINARY(FP, FP)

    static void fceil32(Assembly& as, MVal dst, MVal src) BINARY(FP, FP)
    static void fceil64(Assembly& as, MVal dst, MVal src) BINARY(FP, FP)

    static void fmin32(Assembly& as, MVal dst, MVal a, MVal b) TERNARY(FP, FP_OR_F32, FP_OR_F32, ONLY_ONE_F32)
    static void fmin64(Assembly& as, MVal dst, MVal a, MVal b) TERNARY(FP, FP_OR_F64, FP_OR_F64, ONLY_ONE_F64)

    static void fmax32(Assembly& as, MVal dst, MVal a, MVal b) TERNARY(FP, FP_OR_F32, FP_OR_F32, ONLY_ONE_F32)
    static void fmax64(Assembly& as, MVal dst, MVal a, MVal b) TERNARY(FP, FP_OR_F64, FP_OR_F64, ONLY_ONE_F64)

    static void fabs32(Assembly& as, MVal dst, MVal src) BINARY(FP, FP)
    static void fabs64(Assembly& as, MVal dst, MVal src) BINARY(FP, FP)

    // Bitwise Operations

    static void and8(Assembly& as, MVal dst, MVal a, MVal b) TERNARY(GP, GP_OR_IMM, GP_OR_IMM, ONLY_ONE_IMM)
    static void and16(Assembly& as, MVal dst, MVal a, MVal b) TERNARY(GP, GP_OR_IMM, GP_OR_IMM, ONLY_ONE_IMM)
    static void and32(Assembly& as, MVal dst, MVal a, MVal b) TERNARY(GP, GP_OR_IMM, GP_OR_IMM, ONLY_ONE_IMM)
    static void and64(Assembly& as, MVal dst, MVal a, MVal b) TERNARY(GP, GP_OR_IMM, GP_OR_IMM, ONLY_ONE_IMM)

    static void xor8(Assembly& as, MVal dst, MVal a, MVal b) TERNARY(GP, GP_OR_IMM, GP_OR_IMM, ONLY_ONE_IMM)
    static void xor16(Assembly& as, MVal dst, MVal a, MVal b) TERNARY(GP, GP_OR_IMM, GP_OR_IMM, ONLY_ONE_IMM)
    static void xor32(Assembly& as, MVal dst, MVal a, MVal b) TERNARY(GP, GP_OR_IMM, GP_OR_IMM, ONLY_ONE_IMM)
    static void xor64(Assembly& as, MVal dst, MVal a, MVal b) TERNARY(GP, GP_OR_IMM, GP_OR_IMM, ONLY_ONE_IMM)

    static void or8(Assembly& as, MVal dst, MVal a, MVal b) TERNARY(GP, GP_OR_IMM, GP_OR_IMM, ONLY_ONE_IMM)
    static void or16(Assembly& as, MVal dst, MVal a, MVal b) TERNARY(GP, GP_OR_IMM, GP_OR_IMM, ONLY_ONE_IMM)
    static void or32(Assembly& as, MVal dst, MVal a, MVal b) TERNARY(GP, GP_OR_IMM, GP_OR_IMM, ONLY_ONE_IMM)
    static void or64(Assembly& as, MVal dst, MVal a, MVal b) TERNARY(GP, GP_OR_IMM, GP_OR_IMM, ONLY_ONE_IMM)

    static void not8(Assembly& as, MVal dst, MVal src) BINARY(GP, GP)
    static void not16(Assembly& as, MVal dst, MVal src) BINARY(GP, GP)
    static void not32(Assembly& as, MVal dst, MVal src) BINARY(GP, GP)
    static void not64(Assembly& as, MVal dst, MVal src) BINARY(GP, GP)

    static void shl8(Assembly& as, MVal dst, MVal a, MVal b) TERNARY(GP, GP_OR_IMM, GP_OR_IMM, ONLY_ONE_IMM)
    static void shl16(Assembly& as, MVal dst, MVal a, MVal b) TERNARY(GP, GP_OR_IMM, GP_OR_IMM, ONLY_ONE_IMM)
    static void shl32(Assembly& as, MVal dst, MVal a, MVal b) TERNARY(GP, GP_OR_IMM, GP_OR_IMM, ONLY_ONE_IMM)
    static void shl64(Assembly& as, MVal dst, MVal a, MVal b) TERNARY(GP, GP_OR_IMM, GP_OR_IMM, ONLY_ONE_IMM)

    static void shr8(Assembly& as, MVal dst, MVal a, MVal b) TERNARY(GP, GP_OR_IMM, GP_OR_IMM, ONLY_ONE_IMM)
    static void shr16(Assembly& as, MVal dst, MVal a, MVal b) TERNARY(GP, GP_OR_IMM, GP_OR_IMM, ONLY_ONE_IMM)
    static void shr32(Assembly& as, MVal dst, MVal a, MVal b) TERNARY(GP, GP_OR_IMM, GP_OR_IMM, ONLY_ONE_IMM)
    static void shr64(Assembly& as, MVal dst, MVal a, MVal b) TERNARY(GP, GP_OR_IMM, GP_OR_IMM, ONLY_ONE_IMM)

    static void sar8(Assembly& as, MVal dst, MVal a, MVal b) TERNARY(GP, GP_OR_IMM, GP_OR_IMM, ONLY_ONE_IMM)
    static void sar16(Assembly& as, MVal dst, MVal a, MVal b) TERNARY(GP, GP_OR_IMM, GP_OR_IMM, ONLY_ONE_IMM)
    static void sar32(Assembly& as, MVal dst, MVal a, MVal b) TERNARY(GP, GP_OR_IMM, GP_OR_IMM, ONLY_ONE_IMM)
    static void sar64(Assembly& as, MVal dst, MVal a, MVal b) TERNARY(GP, GP_OR_IMM, GP_OR_IMM, ONLY_ONE_IMM)

    static void rol8(Assembly& as, MVal dst, MVal a, MVal b) TERNARY(GP, GP_OR_IMM, GP_OR_IMM, ONLY_ONE_IMM)
    static void rol16(Assembly& as, MVal dst, MVal a, MVal b) TERNARY(GP, GP_OR_IMM, GP_OR_IMM, ONLY_ONE_IMM)
    static void rol32(Assembly& as, MVal dst, MVal a, MVal b) TERNARY(GP, GP_OR_IMM, GP_OR_IMM, ONLY_ONE_IMM)
    static void rol64(Assembly& as, MVal dst, MVal a, MVal b) TERNARY(GP, GP_OR_IMM, GP_OR_IMM, ONLY_ONE_IMM)

    static void ror8(Assembly& as, MVal dst, MVal a, MVal b) TERNARY(GP, GP_OR_IMM, GP_OR_IMM, ONLY_ONE_IMM)
    static void ror16(Assembly& as, MVal dst, MVal a, MVal b) TERNARY(GP, GP_OR_IMM, GP_OR_IMM, ONLY_ONE_IMM)
    static void ror32(Assembly& as, MVal dst, MVal a, MVal b) TERNARY(GP, GP_OR_IMM, GP_OR_IMM, ONLY_ONE_IMM)
    static void ror64(Assembly& as, MVal dst, MVal a, MVal b) TERNARY(GP, GP_OR_IMM, GP_OR_IMM, ONLY_ONE_IMM)

    static void bitc8(Assembly& as, MVal dst, MVal src) BINARY(GP, GP)
    static void bitc16(Assembly& as, MVal dst, MVal src) BINARY(GP, GP)
    static void bitc32(Assembly& as, MVal dst, MVal src) BINARY(GP, GP)
    static void bitc64(Assembly& as, MVal dst, MVal src) BINARY(GP, GP)

    static void lzc8(Assembly& as, MVal dst, MVal src) BINARY(GP, GP)
    static void lzc16(Assembly& as, MVal dst, MVal src) BINARY(GP, GP)
    static void lzc32(Assembly& as, MVal dst, MVal src) BINARY(GP, GP)
    static void lzc64(Assembly& as, MVal dst, MVal src) BINARY(GP, GP)

    static void tzc8(Assembly& as, MVal dst, MVal src) BINARY(GP, GP)
    static void tzc16(Assembly& as, MVal dst, MVal src) BINARY(GP, GP)
    static void tzc32(Assembly& as, MVal dst, MVal src) BINARY(GP, GP)
    static void tzc64(Assembly& as, MVal dst, MVal src) BINARY(GP, GP)

    // Comparisons

    static void isz(Assembly& as, MVal dst, MVal src) BINARY(GP, GP)
    static void isnz(Assembly& as, MVal dst, MVal src) BINARY(GP, GP)

    static void ccc8(Assembly& as, Condition cc, MVal dst, MVal a, MVal b) TERNARY(GP, GP_OR_IMM, GP_OR_IMM, ONLY_ONE_IMM)
    static void ccc16(Assembly& as, Condition cc, MVal dst, MVal a, MVal b) TERNARY(GP, GP_OR_IMM, GP_OR_IMM, ONLY_ONE_IMM)
    static void ccc32(Assembly& as, Condition cc, MVal dst, MVal a, MVal b) TERNARY(GP, GP_OR_IMM, GP_OR_IMM, ONLY_ONE_IMM)
    static void ccc64(Assembly& as, Condition cc, MVal dst, MVal a, MVal b) TERNARY(GP, GP_OR_IMM, GP_OR_IMM, ONLY_ONE_IMM)

    static void cfcc32(Assembly& as, FloatCondition cc, MVal dst, MVal a, MVal b) TERNARY(FP, FP_OR_F32, FP_OR_F32, ONLY_ONE_F32)
    static void cfcc64(Assembly& as, FloatCondition cc, MVal dst, MVal a, MVal b) TERNARY(FP, FP_OR_F64, FP_OR_F64, ONLY_ONE_F64)

    // Memory

    static void push8(Assembly& as, MVal dst) UNARY(GP)
    static void push16(Assembly& as, MVal dst) UNARY(GP)
    static void push32(Assembly& as, MVal dst) UNARY(GP)
    static void push64(Assembly& as, MVal dst) UNARY(GP)

    static void pop8(Assembly& as, MVal dst) UNARY(GP)
    static void pop16(Assembly& as, MVal dst) UNARY(GP)
    static void pop32(Assembly& as, MVal dst) UNARY(GP)
    static void pop64(Assembly& as, MVal dst) UNARY(GP)

    static void fpush32(Assembly& as, MVal dst) UNARY(FP)
    static void fpush64(Assembly& as, MVal dst) UNARY(FP)

    static void fpop32(Assembly& as, MVal dst) UNARY(FP)
    static void fpop64(Assembly& as, MVal dst) UNARY(FP)

    static void mov8(Assembly& as, MVal dst, MVal src) BINARY(GP, GP_OR_IMM)
    static void mov16(Assembly& as, MVal dst, MVal src) BINARY(GP, GP_OR_IMM)
    static void mov32(Assembly& as, MVal dst, MVal src) BINARY(GP, GP_OR_IMM)
    static void mov64(Assembly& as, MVal dst, MVal src) BINARY(GP, GP_OR_IMM)

    static void fmov32(Assembly& as, MVal dst, MVal src) BINARY(FP, FP_OR_F32)
    static void fmov64(Assembly& as, MVal dst, MVal src) BINARY(FP, FP_OR_F64)

    static void ld8(Assembly& as, MVal dst, MVal src) BINARY(GP, MEM)
    static void ld16(Assembly& as, MVal dst, MVal src) BINARY(GP, MEM)
    static void ld32(Assembly& as, MVal dst, MVal src) BINARY(GP, MEM)
    static void ld64(Assembly& as, MVal dst, MVal src) BINARY(GP, MEM)

    static void st8(Assembly& as, MVal dst, MVal src) BINARY(MEM, GP_OR_IMM)
    static void st16(Assembly& as, MVal dst, MVal src) BINARY(MEM, GP_OR_IMM)
    static void st32(Assembly& as, MVal dst, MVal src) BINARY(MEM, GP_OR_IMM)
    static void st64(Assembly& as, MVal dst, MVal src) BINARY(MEM, GP_OR_IMM)
    
    static void ldi8(Assembly& as, MVal dst, MVal a, MVal b) TERNARY(GP, MEM, GP, TRUE)
    static void ldi16(Assembly& as, MVal dst, MVal a, MVal b) TERNARY(GP, MEM, GP, TRUE)
    static void ldi32(Assembly& as, MVal dst, MVal a, MVal b) TERNARY(GP, MEM, GP, TRUE)
    static void ldi64(Assembly& as, MVal dst, MVal a, MVal b) TERNARY(GP, MEM, GP, TRUE)
    
    static void ldai8(Assembly& as, MVal dst, MVal a, MVal b) TERNARY(GP, MEM, GP, TRUE)
    static void ldai16(Assembly& as, MVal dst, MVal a, MVal b) TERNARY(GP, MEM, GP, TRUE)
    static void ldai32(Assembly& as, MVal dst, MVal a, MVal b) TERNARY(GP, MEM, GP, TRUE)
    static void ldai64(Assembly& as, MVal dst, MVal a, MVal b) TERNARY(GP, MEM, GP, TRUE)
    
    static void sti8(Assembly& as, MVal dst, MVal a, MVal b) TERNARY(MEM, GP, GP, TRUE)
    static void sti16(Assembly& as, MVal dst, MVal a, MVal b) TERNARY(MEM, GP, GP, TRUE)
    static void sti32(Assembly& as, MVal dst, MVal a, MVal b) TERNARY(MEM, GP, GP, TRUE)
    static void sti64(Assembly& as, MVal dst, MVal a, MVal b) TERNARY(MEM, GP, GP, TRUE)

    static void fld32(Assembly& as, MVal dst, MVal src) BINARY(FP, MEM)
    static void fld64(Assembly& as, MVal dst, MVal src) BINARY(FP, MEM)

    static void fst32(Assembly& as, MVal dst, MVal src) BINARY(MEM, FP_OR_F32)
    static void fst64(Assembly& as, MVal dst, MVal src) BINARY(MEM, FP_OR_F64)

    static void fldi32(Assembly& as, MVal dst, MVal a, MVal b) TERNARY(FP, MEM, GP, TRUE)
    static void fldi64(Assembly& as, MVal dst, MVal a, MVal b) TERNARY(FP, MEM, GP, TRUE)

    static void fsti32(Assembly& as, MVal dst, MVal a, MVal b) TERNARY(MEM, GP, FP, TRUE)
    static void fsti64(Assembly& as, MVal dst, MVal a, MVal b) TERNARY(MEM, GP, FP, TRUE)
    
    static void lda(Assembly& as, MVal dst, MVal src) BINARY(GP, MEM)

    static void ldc(Assembly& as, MVal dst, i64 imm) UNARY(GP)

    // Labels

    static void global(Assembly& as, stridx sym) NULLARY
    static void local(Assembly& as, stridx sym) NULLARY

    // Jumps

    static void j(Assembly& as, MVal dst) UNARY(GP_OR_LABEL)

    static void jz(Assembly& as, MVal dst, MVal src) BINARY(GP_OR_LABEL, GP)
    static void jnz(Assembly& as, MVal dst, MVal src) BINARY(GP_OR_LABEL, GP)

    static void jcc8(Assembly& as, Condition cc, MVal dst, MVal a, MVal b) TERNARY(GP_OR_LABEL, GP_OR_IMM, GP_OR_IMM, ONLY_ONE_IMM)
    static void jcc16(Assembly& as, Condition cc, MVal dst, MVal a, MVal b) TERNARY(GP_OR_LABEL, GP_OR_IMM, GP_OR_IMM, ONLY_ONE_IMM)
    static void jcc32(Assembly& as, Condition cc, MVal dst, MVal a, MVal b) TERNARY(GP_OR_LABEL, GP_OR_IMM, GP_OR_IMM, ONLY_ONE_IMM)
    static void jcc64(Assembly& as, Condition cc, MVal dst, MVal a, MVal b) TERNARY(GP_OR_LABEL, GP_OR_IMM, GP_OR_IMM, ONLY_ONE_IMM)
    
    static void jfcc32(Assembly& as, FloatCondition cc, MVal dst, MVal a, MVal b) TERNARY(GP_OR_LABEL, FP_OR_F32, FP_OR_F32, ONLY_ONE_F32)
    static void jfcc64(Assembly& as, FloatCondition cc, MVal dst, MVal a, MVal b) TERNARY(GP_OR_LABEL, FP_OR_F64, FP_OR_F64, ONLY_ONE_F64)

    // Functions

    static void enter(Assembly& as) NULLARY
    static void stack(Assembly& as, MVal dst) UNARY(GP_OR_IMM)
    static void alloca(Assembly& as, MVal dst, MVal src) BINARY(GP, GP_OR_IMM)
    static void unstack(Assembly& as, MVal dst) UNARY(GP_OR_IMM)
    static void leave(Assembly& as) NULLARY

    static void call(Assembly& as, MVal dst) UNARY(GP_OR_LABEL)
    static void ret(Assembly& as) NULLARY

    // Conversions

    static void sxt8(Assembly& as, MVal dst, MVal src) BINARY(GP, GP)
    static void sxt16(Assembly& as, MVal dst, MVal src) BINARY(GP, GP)
    static void sxt32(Assembly& as, MVal dst, MVal src) BINARY(GP, GP)

    static void zxt8(Assembly& as, MVal dst, MVal src) BINARY(GP, GP)
    static void zxt16(Assembly& as, MVal dst, MVal src) BINARY(GP, GP)
    static void zxt32(Assembly& as, MVal dst, MVal src) BINARY(GP, GP)

    static void i8tof32(Assembly& as, MVal dst, MVal src) BINARY(FP, GP)
    static void i16tof32(Assembly& as, MVal dst, MVal src) BINARY(FP, GP)
    static void i32tof32(Assembly& as, MVal dst, MVal src) BINARY(FP, GP)
    static void i64tof32(Assembly& as, MVal dst, MVal src) BINARY(FP, GP)

    static void i8tof64(Assembly& as, MVal dst, MVal src) BINARY(FP, GP)
    static void i16tof64(Assembly& as, MVal dst, MVal src) BINARY(FP, GP)
    static void i32tof64(Assembly& as, MVal dst, MVal src) BINARY(FP, GP)
    static void i64tof64(Assembly& as, MVal dst, MVal src) BINARY(FP, GP)

    static void f32toi8(Assembly& as, MVal dst, MVal src) BINARY(GP, FP)
    static void f32toi16(Assembly& as, MVal dst, MVal src) BINARY(GP, FP)
    static void f32toi32(Assembly& as, MVal dst, MVal src) BINARY(GP, FP)
    static void f32toi64(Assembly& as, MVal dst, MVal src) BINARY(GP, FP)

    static void f64toi8(Assembly& as, MVal dst, MVal src) BINARY(GP, FP)
    static void f64toi16(Assembly& as, MVal dst, MVal src) BINARY(GP, FP)
    static void f64toi32(Assembly& as, MVal dst, MVal src) BINARY(GP, FP)
    static void f64toi64(Assembly& as, MVal dst, MVal src) BINARY(GP, FP)

    static void f32tof64(Assembly& as, MVal dst, MVal src) BINARY(FP, FP)
    static void f64tof32(Assembly& as, MVal dst, MVal src) BINARY(FP, FP)

    static void f32frombits(Assembly& as, MVal dst, MVal src) BINARY(FP, GP)
    static void f64frombits(Assembly& as, MVal dst, MVal src) BINARY(FP, GP)
    static void f32tobits(Assembly& as, MVal dst, MVal src) BINARY(GP, FP)
    static void f64tobits(Assembly& as, MVal dst, MVal src) BINARY(GP, FP)

    // Memory
    
    static void mcpy(Assembly& as, MVal dst, MVal a, MVal b) TERNARY(MEM, MEM, GP_OR_IMM, TRUE)
    static void mmov(Assembly& as, MVal dst, MVal a, MVal b) TERNARY(MEM, MEM, GP_OR_IMM, TRUE)
    static void mset(Assembly& as, MVal dst, MVal a, MVal b) TERNARY(MEM, MEM, GP_OR_IMM, TRUE)
    static void mcmp(Assembly& as, MVal dst, MVal a, MVal b, MVal c) QUATERNARY(GP, MEM, MEM, GP_OR_IMM)
    
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


    static void add8(Assembly& as, MVal dst, MVal a, MVal b) TERNARY(add8)
    static void add16(Assembly& as, MVal dst, MVal a, MVal b) TERNARY(add16)
    static void add32(Assembly& as, MVal dst, MVal a, MVal b) TERNARY(add32)
    static void add64(Assembly& as, MVal dst, MVal a, MVal b) TERNARY(add64)

    static void sub8(Assembly& as, MVal dst, MVal a, MVal b) TERNARY(sub8)
    static void sub16(Assembly& as, MVal dst, MVal a, MVal b) TERNARY(sub16)
    static void sub32(Assembly& as, MVal dst, MVal a, MVal b) TERNARY(sub32)
    static void sub64(Assembly& as, MVal dst, MVal a, MVal b) TERNARY(sub64)

    static void mul8(Assembly& as, MVal dst, MVal a, MVal b) TERNARY(mul8)
    static void mul16(Assembly& as, MVal dst, MVal a, MVal b) TERNARY(mul16)
    static void mul32(Assembly& as, MVal dst, MVal a, MVal b) TERNARY(mul32)
    static void mul64(Assembly& as, MVal dst, MVal a, MVal b) TERNARY(mul64)

    static void div8s(Assembly& as, MVal dst, MVal a, MVal b) TERNARY(div8s)
    static void div16s(Assembly& as, MVal dst, MVal a, MVal b) TERNARY(div16s)
    static void div32s(Assembly& as, MVal dst, MVal a, MVal b) TERNARY(div32s)
    static void div64s(Assembly& as, MVal dst, MVal a, MVal b) TERNARY(div64s)

    static void div8u(Assembly& as, MVal dst, MVal a, MVal b) TERNARY(div8u)
    static void div16u(Assembly& as, MVal dst, MVal a, MVal b) TERNARY(div16u)
    static void div32u(Assembly& as, MVal dst, MVal a, MVal b) TERNARY(div32u)
    static void div64u(Assembly& as, MVal dst, MVal a, MVal b) TERNARY(div64u)

    static void rem8s(Assembly& as, MVal dst, MVal a, MVal b) TERNARY(rem8s)
    static void rem16s(Assembly& as, MVal dst, MVal a, MVal b) TERNARY(rem16s)
    static void rem32s(Assembly& as, MVal dst, MVal a, MVal b) TERNARY(rem32s)
    static void rem64s(Assembly& as, MVal dst, MVal a, MVal b) TERNARY(rem64s)

    static void rem8u(Assembly& as, MVal dst, MVal a, MVal b) TERNARY(rem8u)
    static void rem16u(Assembly& as, MVal dst, MVal a, MVal b) TERNARY(rem16u)
    static void rem32u(Assembly& as, MVal dst, MVal a, MVal b) TERNARY(rem32u)
    static void rem64u(Assembly& as, MVal dst, MVal a, MVal b) TERNARY(rem64u)

    static void neg8(Assembly& as, MVal dst, MVal src) BINARY(neg8)
    static void neg16(Assembly& as, MVal dst, MVal src) BINARY(neg16)
    static void neg32(Assembly& as, MVal dst, MVal src) BINARY(neg32)
    static void neg64(Assembly& as, MVal dst, MVal src) BINARY(neg64)

    // Floating-Point Arithmetic

    static void fadd32(Assembly& as, MVal dst, MVal a, MVal b) TERNARY(fadd32)
    static void fadd64(Assembly& as, MVal dst, MVal a, MVal b) TERNARY(fadd64)

    static void fsub32(Assembly& as, MVal dst, MVal a, MVal b) TERNARY(fsub32)
    static void fsub64(Assembly& as, MVal dst, MVal a, MVal b) TERNARY(fsub64)

    static void fmul32(Assembly& as, MVal dst, MVal a, MVal b) TERNARY(fmul32)
    static void fmul64(Assembly& as, MVal dst, MVal a, MVal b) TERNARY(fmul64)

    static void fdiv32(Assembly& as, MVal dst, MVal a, MVal b) TERNARY(fdiv32)
    static void fdiv64(Assembly& as, MVal dst, MVal a, MVal b) TERNARY(fdiv64)

    static void frem32(Assembly& as, MVal dst, MVal a, MVal b) TERNARY(frem32)
    static void frem64(Assembly& as, MVal dst, MVal a, MVal b) TERNARY(frem64)

    static void fneg32(Assembly& as, MVal dst, MVal src) BINARY(fneg32)
    static void fneg64(Assembly& as, MVal dst, MVal src) BINARY(fneg64)

    static void fsqrt32(Assembly& as, MVal dst, MVal src) BINARY(fsqrt32)
    static void fsqrt64(Assembly& as, MVal dst, MVal src) BINARY(fsqrt64)

    static void fsin32(Assembly& as, MVal dst, MVal src) BINARY(fsin32)
    static void fsin64(Assembly& as, MVal dst, MVal src) BINARY(fsin64)

    static void fcos32(Assembly& as, MVal dst, MVal src) BINARY(fcos32)
    static void fcos64(Assembly& as, MVal dst, MVal src) BINARY(fcos64)

    static void ftan32(Assembly& as, MVal dst, MVal src) BINARY(ftan32)
    static void ftan64(Assembly& as, MVal dst, MVal src) BINARY(ftan64)

    static void fround32(Assembly& as, MVal dst, MVal src) BINARY(fround32)
    static void fround64(Assembly& as, MVal dst, MVal src) BINARY(fround64)

    static void ffloor32(Assembly& as, MVal dst, MVal src) BINARY(ffloor32)
    static void ffloor64(Assembly& as, MVal dst, MVal src) BINARY(ffloor64)

    static void fceil32(Assembly& as, MVal dst, MVal src) BINARY(fceil32)
    static void fceil64(Assembly& as, MVal dst, MVal src) BINARY(fceil64)

    static void fmin32(Assembly& as, MVal dst, MVal a, MVal b) TERNARY(fmin32)
    static void fmin64(Assembly& as, MVal dst, MVal a, MVal b) TERNARY(fmin64)

    static void fmax32(Assembly& as, MVal dst, MVal a, MVal b) TERNARY(fmax32)
    static void fmax64(Assembly& as, MVal dst, MVal a, MVal b) TERNARY(fmax64)

    static void fabs32(Assembly& as, MVal dst, MVal src) BINARY(fabs32)
    static void fabs64(Assembly& as, MVal dst, MVal src) BINARY(fabs64)

    // Bitwise Operations

    static void and8(Assembly& as, MVal dst, MVal a, MVal b) TERNARY(and8)
    static void and16(Assembly& as, MVal dst, MVal a, MVal b) TERNARY(and16)
    static void and32(Assembly& as, MVal dst, MVal a, MVal b) TERNARY(and32)
    static void and64(Assembly& as, MVal dst, MVal a, MVal b) TERNARY(and64)

    static void xor8(Assembly& as, MVal dst, MVal a, MVal b) TERNARY(xor8)
    static void xor16(Assembly& as, MVal dst, MVal a, MVal b) TERNARY(xor16)
    static void xor32(Assembly& as, MVal dst, MVal a, MVal b) TERNARY(xor32)
    static void xor64(Assembly& as, MVal dst, MVal a, MVal b) TERNARY(xor64)

    static void or8(Assembly& as, MVal dst, MVal a, MVal b) TERNARY(or8)
    static void or16(Assembly& as, MVal dst, MVal a, MVal b) TERNARY(or16)
    static void or32(Assembly& as, MVal dst, MVal a, MVal b) TERNARY(or32)
    static void or64(Assembly& as, MVal dst, MVal a, MVal b) TERNARY(or64)

    static void not8(Assembly& as, MVal dst, MVal src) BINARY(not8)
    static void not16(Assembly& as, MVal dst, MVal src) BINARY(not16)
    static void not32(Assembly& as, MVal dst, MVal src) BINARY(not32)
    static void not64(Assembly& as, MVal dst, MVal src) BINARY(not64)

    static void shl8(Assembly& as, MVal dst, MVal a, MVal b) TERNARY(shl8)
    static void shl16(Assembly& as, MVal dst, MVal a, MVal b) TERNARY(shl16)
    static void shl32(Assembly& as, MVal dst, MVal a, MVal b) TERNARY(shl32)
    static void shl64(Assembly& as, MVal dst, MVal a, MVal b) TERNARY(shl64)

    static void shr8(Assembly& as, MVal dst, MVal a, MVal b) TERNARY(shr8)
    static void shr16(Assembly& as, MVal dst, MVal a, MVal b) TERNARY(shr16)
    static void shr32(Assembly& as, MVal dst, MVal a, MVal b) TERNARY(shr32)
    static void shr64(Assembly& as, MVal dst, MVal a, MVal b) TERNARY(shr64)

    static void sar8(Assembly& as, MVal dst, MVal a, MVal b) TERNARY(sar8)
    static void sar16(Assembly& as, MVal dst, MVal a, MVal b) TERNARY(sar16)
    static void sar32(Assembly& as, MVal dst, MVal a, MVal b) TERNARY(sar32)
    static void sar64(Assembly& as, MVal dst, MVal a, MVal b) TERNARY(sar64)

    static void rol8(Assembly& as, MVal dst, MVal a, MVal b) TERNARY(rol8)
    static void rol16(Assembly& as, MVal dst, MVal a, MVal b) TERNARY(rol16)
    static void rol32(Assembly& as, MVal dst, MVal a, MVal b) TERNARY(rol32)
    static void rol64(Assembly& as, MVal dst, MVal a, MVal b) TERNARY(rol64)

    static void ror8(Assembly& as, MVal dst, MVal a, MVal b) TERNARY(ror8)
    static void ror16(Assembly& as, MVal dst, MVal a, MVal b) TERNARY(ror16)
    static void ror32(Assembly& as, MVal dst, MVal a, MVal b) TERNARY(ror32)
    static void ror64(Assembly& as, MVal dst, MVal a, MVal b) TERNARY(ror64)

    static void bitc8(Assembly& as, MVal dst, MVal src) BINARY(bitc8)
    static void bitc16(Assembly& as, MVal dst, MVal src) BINARY(bitc16)
    static void bitc32(Assembly& as, MVal dst, MVal src) BINARY(bitc32)
    static void bitc64(Assembly& as, MVal dst, MVal src) BINARY(bitc64)

    static void lzc8(Assembly& as, MVal dst, MVal src) BINARY(lzc8)
    static void lzc16(Assembly& as, MVal dst, MVal src) BINARY(lzc16)
    static void lzc32(Assembly& as, MVal dst, MVal src) BINARY(lzc32)
    static void lzc64(Assembly& as, MVal dst, MVal src) BINARY(lzc64)

    static void tzc8(Assembly& as, MVal dst, MVal src) BINARY(tzc8)
    static void tzc16(Assembly& as, MVal dst, MVal src) BINARY(tzc16)
    static void tzc32(Assembly& as, MVal dst, MVal src) BINARY(tzc32)
    static void tzc64(Assembly& as, MVal dst, MVal src) BINARY(tzc64)

    // Comparisons

    static void isz(Assembly& as, MVal dst, MVal src) BINARY(isz)
    static void isnz(Assembly& as, MVal dst, MVal src) BINARY(isnz)

    static void ccc8(Assembly& as, Condition cc, MVal dst, MVal a, MVal b) CONDITIONAL(ccc8)
    static void ccc16(Assembly& as, Condition cc, MVal dst, MVal a, MVal b) CONDITIONAL(ccc16)
    static void ccc32(Assembly& as, Condition cc, MVal dst, MVal a, MVal b) CONDITIONAL(ccc32)
    static void ccc64(Assembly& as, Condition cc, MVal dst, MVal a, MVal b) CONDITIONAL(ccc64)

    static void cfcc32(Assembly& as, FloatCondition cc, MVal dst, MVal a, MVal b) CONDITIONAL(cfcc32)
    static void cfcc64(Assembly& as, FloatCondition cc, MVal dst, MVal a, MVal b) CONDITIONAL(cfcc64)

    // Memory

    static void push8(Assembly& as, MVal dst) UNARY(push8)
    static void push16(Assembly& as, MVal dst) UNARY(push16)
    static void push32(Assembly& as, MVal dst) UNARY(push32)
    static void push64(Assembly& as, MVal dst) UNARY(push64)

    static void pop8(Assembly& as, MVal dst) UNARY(pop8)
    static void pop16(Assembly& as, MVal dst) UNARY(pop16)
    static void pop32(Assembly& as, MVal dst) UNARY(pop32)
    static void pop64(Assembly& as, MVal dst) UNARY(pop64)

    static void fpush32(Assembly& as, MVal dst) UNARY(fpush32)
    static void fpush64(Assembly& as, MVal dst) UNARY(fpush64)

    static void fpop32(Assembly& as, MVal dst) UNARY(fpop32)
    static void fpop64(Assembly& as, MVal dst) UNARY(fpop64)

    static void mov8(Assembly& as, MVal dst, MVal src) BINARY(mov8)
    static void mov16(Assembly& as, MVal dst, MVal src) BINARY(mov16)
    static void mov32(Assembly& as, MVal dst, MVal src) BINARY(mov32)
    static void mov64(Assembly& as, MVal dst, MVal src) BINARY(mov64)

    static void fmov32(Assembly& as, MVal dst, MVal src) BINARY(fmov32)
    static void fmov64(Assembly& as, MVal dst, MVal src) BINARY(fmov64)

    static void ld8(Assembly& as, MVal dst, MVal src) BINARY(ld8)
    static void ld16(Assembly& as, MVal dst, MVal src) BINARY(ld16)
    static void ld32(Assembly& as, MVal dst, MVal src) BINARY(ld32)
    static void ld64(Assembly& as, MVal dst, MVal src) BINARY(ld64)

    static void st8(Assembly& as, MVal dst, MVal src) BINARY(st8)
    static void st16(Assembly& as, MVal dst, MVal src) BINARY(st16)
    static void st32(Assembly& as, MVal dst, MVal src) BINARY(st32)
    static void st64(Assembly& as, MVal dst, MVal src) BINARY(st64)
    
    static void ldi8(Assembly& as, MVal dst, MVal a, MVal b) TERNARY(ldi8)
    static void ldi16(Assembly& as, MVal dst, MVal a, MVal b) TERNARY(ldi16)
    static void ldi32(Assembly& as, MVal dst, MVal a, MVal b) TERNARY(ldi32)
    static void ldi64(Assembly& as, MVal dst, MVal a, MVal b) TERNARY(ldi64)
    
    static void ldai8(Assembly& as, MVal dst, MVal a, MVal b) TERNARY(ldai8)
    static void ldai16(Assembly& as, MVal dst, MVal a, MVal b) TERNARY(ldai16)
    static void ldai32(Assembly& as, MVal dst, MVal a, MVal b) TERNARY(ldai32)
    static void ldai64(Assembly& as, MVal dst, MVal a, MVal b) TERNARY(ldai64)
    
    static void sti8(Assembly& as, MVal dst, MVal a, MVal b) TERNARY(sti8)
    static void sti16(Assembly& as, MVal dst, MVal a, MVal b) TERNARY(sti16)
    static void sti32(Assembly& as, MVal dst, MVal a, MVal b) TERNARY(sti32)
    static void sti64(Assembly& as, MVal dst, MVal a, MVal b) TERNARY(sti64)

    static void fld32(Assembly& as, MVal dst, MVal src) BINARY(fld32)
    static void fld64(Assembly& as, MVal dst, MVal src) BINARY(fld64)

    static void fst32(Assembly& as, MVal dst, MVal src) BINARY(fst32)
    static void fst64(Assembly& as, MVal dst, MVal src) BINARY(fst64)

    static void fldi32(Assembly& as, MVal dst, MVal a, MVal b) TERNARY(fldi32)
    static void fldi64(Assembly& as, MVal dst, MVal a, MVal b) TERNARY(fldi64)

    static void fsti32(Assembly& as, MVal dst, MVal a, MVal b) TERNARY(fsti32)
    static void fsti64(Assembly& as, MVal dst, MVal a, MVal b) TERNARY(fsti64)
    
    static void lda(Assembly& as, MVal dst, MVal src) BINARY(lda)

    static void ldc(Assembly& as, MVal dst, i64 imm) {
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

    static void j(Assembly& as, MVal dst) UNARY(j)

    static void jz(Assembly& as, MVal dst, MVal src) BINARY(jz)
    static void jnz(Assembly& as, MVal dst, MVal src) BINARY(jnz)

    static void jcc8(Assembly& as, Condition cc, MVal dst, MVal a, MVal b) CONDITIONAL(jcc8)
    static void jcc16(Assembly& as, Condition cc, MVal dst, MVal a, MVal b) CONDITIONAL(jcc16)
    static void jcc32(Assembly& as, Condition cc, MVal dst, MVal a, MVal b) CONDITIONAL(jcc32)
    static void jcc64(Assembly& as, Condition cc, MVal dst, MVal a, MVal b) CONDITIONAL(jcc64)
    
    static void jfcc32(Assembly& as, FloatCondition cc, MVal dst, MVal a, MVal b) CONDITIONAL(jfcc32)
    static void jfcc64(Assembly& as, FloatCondition cc, MVal dst, MVal a, MVal b) CONDITIONAL(jfcc64)

    // Functions

    static void enter(Assembly& as) NULLARY(enter)
    static void stack(Assembly& as, MVal dst) UNARY(stack)
    static void alloca(Assembly& as, MVal dst, MVal src) BINARY(alloca)
    static void unstack(Assembly& as, MVal dst) UNARY(unstack)
    static void leave(Assembly& as) NULLARY(leave)

    static void call(Assembly& as, MVal dst) UNARY(call)
    static void ret(Assembly& as) NULLARY(ret)

    // Conversions

    static void sxt8(Assembly& as, MVal dst, MVal src) BINARY(sxt8)
    static void sxt16(Assembly& as, MVal dst, MVal src) BINARY(sxt16)
    static void sxt32(Assembly& as, MVal dst, MVal src) BINARY(sxt32)

    static void zxt8(Assembly& as, MVal dst, MVal src) BINARY(zxt8)
    static void zxt16(Assembly& as, MVal dst, MVal src) BINARY(zxt16)
    static void zxt32(Assembly& as, MVal dst, MVal src) BINARY(zxt32)

    static void i8tof32(Assembly& as, MVal dst, MVal src) BINARY(i8tof32)
    static void i16tof32(Assembly& as, MVal dst, MVal src) BINARY(i16tof32)
    static void i32tof32(Assembly& as, MVal dst, MVal src) BINARY(i32tof32)
    static void i64tof32(Assembly& as, MVal dst, MVal src) BINARY(i64tof32)

    static void i8tof64(Assembly& as, MVal dst, MVal src) BINARY(i8tof64)
    static void i16tof64(Assembly& as, MVal dst, MVal src) BINARY(i16tof64)
    static void i32tof64(Assembly& as, MVal dst, MVal src) BINARY(i32tof64)
    static void i64tof64(Assembly& as, MVal dst, MVal src) BINARY(i64tof64)

    static void f32toi8(Assembly& as, MVal dst, MVal src) BINARY(f32toi8)
    static void f32toi16(Assembly& as, MVal dst, MVal src) BINARY(f32toi16)
    static void f32toi32(Assembly& as, MVal dst, MVal src) BINARY(f32toi32)
    static void f32toi64(Assembly& as, MVal dst, MVal src) BINARY(f32toi64)

    static void f64toi8(Assembly& as, MVal dst, MVal src) BINARY(f64toi8)
    static void f64toi16(Assembly& as, MVal dst, MVal src) BINARY(f64toi16)
    static void f64toi32(Assembly& as, MVal dst, MVal src) BINARY(f64toi32)
    static void f64toi64(Assembly& as, MVal dst, MVal src) BINARY(f64toi64)

    static void f32tof64(Assembly& as, MVal dst, MVal src) BINARY(f32tof64)
    static void f64tof32(Assembly& as, MVal dst, MVal src) BINARY(f64tof32)

    static void f32frombits(Assembly& as, MVal dst, MVal src) BINARY(f32frombits)
    static void f64frombits(Assembly& as, MVal dst, MVal src) BINARY(f64frombits)
    static void f32tobits(Assembly& as, MVal dst, MVal src) BINARY(f32tobits)
    static void f64tobits(Assembly& as, MVal dst, MVal src) BINARY(f64tobits)

    // Memory
    
    static void mcpy(Assembly& as, MVal dst, MVal a, MVal b) TERNARY(mcpy)
    static void mmov(Assembly& as, MVal dst, MVal a, MVal b) TERNARY(mmov)
    static void mset(Assembly& as, MVal dst, MVal a, MVal b) TERNARY(mset)
    static void mcmp(Assembly& as, MVal dst, MVal a, MVal b, MVal c) QUATERNARY(mcmp)
};

#endif