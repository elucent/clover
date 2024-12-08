#ifndef ASM_TARGET_H
#define ASM_TARGET_H

#include "util/buffer.h"
#include "util/io.h"
#include "util/sym.h"
#include "util/vec.h"
#include "util/hash.h"
#include "util/utf.h"

enum Arch : u8 {
    ARCH_X86,
    ARCH_AMD64,
    ARCH_ARM64
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

template<typename IO>
inline IO format_impl(IO io, const TargetDesc& target) {
    return format(io, OS_NAMES[target.os], '-', ARCH_NAMES[target.arch]);
}

struct RegSet {
    u64 regs;

    inline constexpr void add(mreg r) {
        regs |= u64(1) << r;
    }

    inline constexpr void add(const mreg* arr, i32 n) {
        for (i32 i = 0; i < n; i ++)
            regs |= u64(1) << arr[i];
    }

    template<typename... Args>
    inline constexpr void add(mreg r, Args... args) {
        add(r);
        add(args...);
    }

    template<typename... Args>
    inline constexpr void add(const mreg* r, i32 n, Args... args) {
        add(r, n);
        add(args...);
    }

    template<typename... Args>
    inline constexpr RegSet(Args... args): RegSet() {
        add(args...);
    }

    inline constexpr RegSet():
        regs(0) {}

    inline constexpr void remove(mreg r) {
        regs &= ~(u64(1) << r);
    }

    inline constexpr RegSet without(mreg r) {
        RegSet result = *this;
        result.remove(r);
        return result;
    }

    inline constexpr bool operator[](mreg r) const {
        return regs & u64(1) << r;
    }

    inline constexpr operator bool() const {
        return regs;
    }

    inline mreg next() const {
        if (!regs)
            return -1;
        return ctz64(regs);
    }

    inline mreg operator*() const {
        return next();
    }

    inline constexpr bool operator==(const RegSet& other) const {
        return regs == other.regs;
    }

    inline constexpr bool operator!=(const RegSet& other) const {
        return regs != other.regs;
    }

    inline constexpr RegSet begin() const {
        return *this;
    }

    inline constexpr RegSet end() const {
        return RegSet();
    }

    inline constexpr RegSet& operator++() {
        if (regs) remove(next());
        return *this;
    }

    inline constexpr bool empty() const {
        return !regs;
    }

    inline u64 size() const {
        return popcount64(regs);
    }

    inline constexpr RegSet& operator|=(const RegSet& other) {
        regs |= other.regs;
        return *this;
    }

    inline constexpr RegSet& operator&=(const RegSet& other) {
        regs &= other.regs;
        return *this;
    }

    inline constexpr RegSet& operator^=(const RegSet& other) {
        regs ^= other.regs;
        return *this;
    }

    inline constexpr RegSet& operator-=(const RegSet& other) {
        regs &= ~other.regs;
        return *this;
    }

    inline constexpr RegSet operator|(const RegSet& other) const {
        RegSet set = *this;
        set |= other;
        return set;
    }

    inline constexpr RegSet operator&(const RegSet& other) const {
        RegSet set = *this;
        set &= other;
        return set;
    }

    inline constexpr RegSet operator^(const RegSet& other) const {
        RegSet set = *this;
        set ^= other;
        return set;
    }

    inline constexpr RegSet operator-(const RegSet& other) const {
        RegSet set = *this;
        set -= other;
        return set;
    }

    inline constexpr RegSet operator~() const {
        RegSet set = *this;
        set.regs = ~set.regs;
        return set;
    }
};

struct Binding {
    enum Kind : u8 {
        NONE, GP, FP, PAIR, STACK
    };

    union {
        struct { Kind kind; mreg regs[2]; };
        struct { Kind pad1; mreg reg; };
        struct { Kind pad2; i32 offset : 24; };
    };

    inline static Binding none() {
        Binding b;
        b.kind = NONE;
        return b;
    }

    inline static Binding pair(mreg first, mreg second) {
        Binding b;
        b.kind = PAIR;
        b.regs[0] = first;
        b.regs[1] = second;
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
    i32 stack_usage;

    inline Binding param(i32 i) const {
        return bindings[i + 1];
    }

    inline Binding ret() const {
        return bindings[0];
    }

    inline void place_ret(Binding binding) {
        assert(bindings.size() == 0);
        bindings.push(binding);
    }

    inline void add_param(Binding binding) {
        bindings.push(binding);
    }
};

enum Section : u8 {
    CODE_SECTION, DATA_SECTION, STATIC_SECTION
};

enum DefType : u8 {
    DEF_GLOBAL, DEF_LOCAL
};

// Machine-level value.
union ASMVal {
    enum Kind : u8 {
        NONE,
        GP, FP,
        IMM, IMM64,
        F32, F64,
        MEM
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
            i64 imm64;
            float f32;
            double f64;
            mreg gp;
            mreg fp;
            struct { MemKind memkind; mreg base; i32 offset; };
            struct { MemKind : 8;     mreg : 8;  i32 sym; };
            u64 uval;
        };
        Kind kind;
    };

    inline bool operator==(ASMVal other) const {
        return kind == other.kind && uval == other.uval;
    }

    inline bool operator!=(ASMVal other) const {
        return kind != other.kind || uval != other.uval;
    }

    inline bool is_reg() const {
        return kind == GP || kind == FP;
    }

    inline bool is_const() const {
        return kind == IMM || kind == IMM64 || kind == F32 || kind == F64;
    }
};

inline static ASMVal None() {
    ASMVal m;
    m.uval = 0;
    m.kind = ASMVal::NONE;
    return m;
}

inline static ASMVal FP(mreg r) {
    ASMVal m;
    m.uval = 0;
    m.fp = r;
    m.kind = ASMVal::FP;
    return m;
}

inline static ASMVal GP(mreg r) {
    ASMVal m;
    m.uval = 0;
    m.gp = r;
    m.kind = ASMVal::GP;
    return m;
}

inline static ASMVal Imm(i32 imm) {
    ASMVal m;
    m.uval = 0;
    m.kind = ASMVal::IMM;
    m.imm = imm;
    return m;
}

inline static ASMVal Imm64(i64 imm) {
    ASMVal m;
    m.kind = ASMVal::IMM64;
    m.imm64 = imm;
    return m;
}

inline static ASMVal F32(float imm) {
    ASMVal m;
    m.uval = 0;
    m.kind = ASMVal::F32;
    m.f32 = imm;
    return m;
}

inline static ASMVal F64(double imm) {
    ASMVal m;
    m.kind = ASMVal::F64;
    m.f64 = imm;
    return m;
}

inline static ASMVal Mem(mreg base, i32 offset) {
    ASMVal m;
    m.uval = 0;
    m.kind = ASMVal::MEM;
    m.memkind = ASMVal::REG_OFFSET;
    m.base = base;
    m.offset = offset;
    return m;
}

inline static ASMVal Func(Symbol sym) {
    ASMVal m;
    m.uval = 0;
    m.kind = ASMVal::MEM;
    m.memkind = ASMVal::FUNC_LABEL;
    m.sym = sym;
    return m;
}

inline static ASMVal Label(Symbol sym) {
    ASMVal m;
    m.uval = 0;
    m.kind = ASMVal::MEM;
    m.memkind = ASMVal::LOCAL_LABEL;
    m.sym = sym;
    return m;
}

inline static ASMVal Data(Symbol sym) {
    ASMVal m;
    m.uval = 0;
    m.kind = ASMVal::MEM;
    m.memkind = ASMVal::DATA_LABEL;
    m.sym = sym;
    return m;
}

inline static ASMVal Static(Symbol sym) {
    ASMVal m;
    m.uval = 0;
    m.kind = ASMVal::MEM;
    m.memkind = ASMVal::STATIC_LABEL;
    m.sym = sym;
    return m;
}

// Either a simple ASMVal or a pair of ASMVals.
template<typename T>
struct MaybePair {
    T first, second;

    inline MaybePair():
        first(None()), second(None()) {}

    inline MaybePair(T first_in):
        first(first_in), second(None()) {}

    inline MaybePair(T first_in, T second_in):
        first(first_in), second(second_in) {}

    inline bool isPair() const {
        return second.kind != ASMVal::NONE;
    }

    inline bool operator==(const MaybePair& other) const {
        return first == other.first && second == other.second;
    }
};

// Abstract representation of a symbol location.
struct Def {
    i32 offset;
    Symbol sym;
    Section section;
    DefType type;

    inline Def() {}

    inline Def(Section section_in, DefType type_in, i32 offset_in, Symbol sym_in):
        offset(offset_in), sym(sym_in), section(section_in), type(type_in) {}
};

// Abstract representation of a relocation.
struct Reloc : public Def {
    enum Kind : u8 {
        REL8,
        REL16_LE, REL32_LE, REL64_LE,
        REL16_BE, REL32_BE, REL64_BE,
        LAST_RELATIVE_KIND = REL64_BE,
        ABS8,
        ABS16_LE, ABS32_LE, ABS64_LE,
        ABS16_BE, ABS32_BE, ABS64_BE
    };

    Kind kind;

    inline Reloc() {}

    inline Reloc(Section section_in, DefType type_in, Kind kind_in, i32 offset_in, Symbol sym_in):
        Def(section_in, type_in, offset_in, sym_in), kind(kind_in) {}

    inline bool isRelative() const {
        return kind <= LAST_RELATIVE_KIND;
    }

    inline bool isAbsolute() const {
        return kind > LAST_RELATIVE_KIND;
    }
};

// Unified buffer representing fully-linked code.
struct LinkedAssembly {
    slice<memory::page> pages;
    i8 *code, *data, *stat;
    i32 codesize, datasize, statsize;
    ::map<Symbol, iptr> defs;
    SymbolTable* symtab;

    inline LinkedAssembly() {}

    inline LinkedAssembly(LinkedAssembly&& other):
        pages(other.pages), code(other.code), data(other.data), stat(other.stat),
        codesize(other.codesize), datasize(other.datasize), statsize(other.statsize),
        defs(move(other.defs)), symtab(other.symtab) {
        other.pages = { nullptr, iptr(0) };
        other.code = other.data = other.stat = nullptr;
    }

    inline LinkedAssembly& operator=(LinkedAssembly&& other) {
        if (&other != this) {
            if (code || data || stat)
                unload();
            pages = other.pages;
            code = other.code;
            data = other.data;
            stat = other.stat;
            codesize = other.codesize;
            datasize = other.datasize;
            statsize = other.statsize;
            defs = move(other.defs);
            symtab = other.symtab;
            other.pages = { nullptr, iptr(0) };
            other.code = other.data = other.stat = nullptr;
        }
        return *this;
    }

    inline ~LinkedAssembly() {
        if (code || data || stat)
            unload();
    }

    void load();

    inline void unload() {
        memory::unmap(pages);
    }

    template<typename T>
    T* lookup(Symbol sym) const {
        auto it = defs.find(sym);
        if (it == defs.end())
            return nullptr;
        return (T*)it->value;
    }

    template<typename T>
    T* lookup(const_slice<i8> name) const {
        return lookup<T>((*symtab)[name]);
    }

    template<typename T>
    T* lookup(const i8* name) const {
        return lookup<T>((*symtab)[name]);
    }

    inline void printCode() const {
        for (i8 i : const_slice<i8>{ code, codesize })
            print(hex((u8)i, 2), ' ');
        println();
    }

    void writeELFExecutable(fd file);
};

// Collection of buffers for target-specific code.
struct Assembly {
    bytebuf code, data, stat;
    vec<Def, 16> defs;
    vec<Reloc, 16> relocs;
    SymbolTable& symtab;

    inline Assembly(SymbolTable& symtab_in):
        symtab(symtab_in) {}

    inline void clear() {
        code.clear();
        data.clear();
        stat.clear();
        defs.clear();
        relocs.clear();
    }

    inline void def(Section section, DefType type, Symbol sym) {
        bytebuf* ptr;
        switch (section) {
            case CODE_SECTION: ptr = &code; break;
            case DATA_SECTION: ptr = &data; break;
            case STATIC_SECTION: ptr = &stat; break;
        }
        defs.push(Def(section, type, ptr->size(), sym));
    }

    inline void ref(Section section, DefType type, Reloc::Kind kind, Symbol sym) {
        bytebuf* ptr;
        switch (section) {
            case CODE_SECTION: ptr = &code; break;
            case DATA_SECTION: ptr = &data; break;
            case STATIC_SECTION: ptr = &stat; break;
        }
        relocs.push(Reloc(section, type, kind, ptr->size(), sym));
    }

    void linkInto(LinkedAssembly& linked);

    inline LinkedAssembly link() {
        LinkedAssembly linked;
        linkInto(linked);
        return move(linked);
    }

    inline Symbol anon() {
        return symtab.anon();
    }

    template<typename IO, typename Format = Formatter<IO>>
    inline IO serialize(IO io) {
        u32 codeSize = code.size(), dataSize = data.size(), staticSize = stat.size();
        io = format(io, const_slice<i8>{ "\0aob", 4 });
        io = format(io, uleb(codeSize), code);
        io = format(io, uleb(dataSize), data);
        io = format(io, uleb(staticSize), stat);
        io = format(io, uleb(symtab.strings.size()));
        for (const auto& [i, s] : enumerate(symtab.strings))
            io = format(io, uleb(s.size()), s);
        io = format(io, uleb(defs.size()));
        for (Def def : defs)
            io = format(io, (u8)def.section, leb(def.offset), uleb(def.sym));
        io = format(io, uleb(relocs.size()));
        for (Reloc reloc : relocs)
            io = format(io, (u8)reloc.section, (u8)reloc.kind, leb(reloc.offset), uleb(reloc.sym));
        return io;
    }

    template<typename IO, typename Format = Formatter<IO>>
    inline IO deserialize(IO io) {
        array<i8, 4> magic;
        for (u32 i = 0; i < 4; i ++) magic[i] = get<i8>(io);
        if (memory::compare(&magic[0], "\0aob", 4))
            panic("Failed to deserialize assembly!");

        u32 codeSize = get<uleb>(io).value;
        for (u32 i = 0; i < codeSize; i ++)
            code.write<i8>(get<i8>(io));
        u32 dataSize = get<uleb>(io).value;
        for (u32 i = 0; i < dataSize; i ++)
            data.write<i8>(get<i8>(io));
        u32 staticSize = get<uleb>(io).value;
        for (u32 i = 0; i < staticSize; i ++)
            stat.write<i8>(get<i8>(io));

        symtab.strings.clear();
        symtab.strtab.clear();
        u32 syms = get<uleb>(io).value;
        for (u32 i = 0; i < syms; i ++) {
            u32 size = get<uleb>(io).value;
            slice<i8> newString = { new i8[size], size };
            for (u32 j = 0; j < size; j ++) newString[j] = get<i8>(io);
            symtab.strings.push(newString);
            symtab.strtab.put(newString, i);
        }

        u32 ndefs = get<uleb>(io).value;
        for (u32 i = 0; i < ndefs; i ++) {
            Def def;
            def.section = (Section)get<u8>(io);
            def.offset = get<leb>(io).value;
            def.sym = get<uleb>(io).value;
            defs.push(def);
        }

        u32 nrelocs = get<uleb>(io).value;
        for (u32 i = 0; i < nrelocs; i ++) {
            Reloc reloc;
            reloc.section = (Section)get<u8>(io);
            reloc.kind = (Reloc::Kind)get<u8>(io);
            reloc.offset = get<leb>(io).value;
            reloc.sym = get<uleb>(io).value;
            relocs.push(reloc);
        }

        return io;
    }

    void writeELFObject(fd file);
};

struct Offsets {
    u32 code, data, stat;

    inline u32& offset(Section s) {
        switch (s) {
            case CODE_SECTION: return code;
            case DATA_SECTION: return data;
            case STATIC_SECTION: return stat;
        }
    }
};

inline Offsets joinAssembly(Assembly& dest, Offsets offsets, const Assembly& src) {
    dest.code.write(src.code);
    dest.data.write(src.data);
    dest.stat.write(src.stat);
    for (Def def : src.defs) {
        def.offset += offsets.offset(def.section);
        def.sym = &dest.symtab == &src.symtab ? def.sym : dest.symtab[src.symtab[def.sym]];
        dest.defs.push(def);
    }
    for (Reloc ref : src.relocs) {
        ref.offset += offsets.offset(ref.section);
        ref.sym = &dest.symtab == &src.symtab ? ref.sym : dest.symtab[src.symtab[ref.sym]];
        dest.relocs.push(ref);
    }
    offsets.code += src.code.size();
    offsets.data += src.data.size();
    offsets.stat += src.stat.size();
    return offsets;
}

inline void joinWithOffsets(Assembly&, Offsets) {}

template<typename... Args>
inline void joinWithOffsets(Assembly& dest, Offsets offsets, const const_slice<const Assembly*>& src, const Args&... args) {
    for (const Assembly* a : src)
        offsets = joinAssembly(dest, offsets, *a);
    joinWithOffsets(dest, offsets, args...);
}

template<typename... Args>
inline void joinWithOffsets(Assembly& dest, Offsets offsets, const Assembly* src, const Args&... args) {
    joinWithOffsets(dest, joinAssembly(dest, offsets, *src), args...);
}

template<typename... Args>
inline void joinWithOffsets(Assembly& dest, Offsets offsets, const Assembly& src, const Args&... args) {
    joinWithOffsets(dest, joinAssembly(dest, offsets, src), args...);
}

template<typename... Args>
inline void join(Assembly& dest, const Args&... args) {
    joinWithOffsets(dest, { (u32)dest.code.size(), (u32)dest.data.size(), (u32)dest.stat.size() }, args...);
}

enum Condition {
    COND_EQ, COND_NE, COND_LT, COND_LE, COND_GT, COND_GE, COND_ABOVE, COND_AE, COND_BELOW, COND_BE,
    COND_TEST_ZERO, COND_TEST_NONZERO // Test if zero/nonzero with mask.
};

inline Condition commute(Condition cond) {
    switch (cond) {
        case COND_EQ:
        case COND_NE:
        case COND_TEST_ZERO:
        case COND_TEST_NONZERO:
            return cond;
        case COND_LT:
            return COND_GT;
        case COND_LE:
            return COND_GE;
        case COND_GT:
            return COND_LT;
        case COND_GE:
            return COND_LE;
        case COND_ABOVE:
            return COND_BELOW;
        case COND_AE:
            return COND_BE;
        case COND_BELOW:
            return COND_ABOVE;
        case COND_BE:
            return COND_AE;
    }
}

enum FloatCondition {
    FCOND_EQ, FCOND_NE, FCOND_LT, FCOND_LE, FCOND_GT, FCOND_GE
};

inline constexpr Condition invert(Condition condition) {
    switch (condition) {
        case COND_LT: return COND_GE;
        case COND_LE: return COND_GT;
        case COND_GT: return COND_LE;
        case COND_GE: return COND_LT;
        case COND_BELOW: return COND_AE;
        case COND_BE: return COND_ABOVE;
        case COND_ABOVE: return COND_BE;
        case COND_AE: return COND_BELOW;
        case COND_EQ: return COND_NE;
        case COND_NE: return COND_EQ;
        case COND_TEST_ZERO: return COND_TEST_NONZERO;
        case COND_TEST_NONZERO: return COND_TEST_ZERO;
    }
}

inline constexpr FloatCondition invert(FloatCondition condition) {
    switch (condition) {
        case FCOND_LT: return FCOND_GE;
        case FCOND_LE: return FCOND_GT;
        case FCOND_GT: return FCOND_LE;
        case FCOND_GE: return FCOND_LT;
        case FCOND_EQ: return FCOND_NE;
        case FCOND_NE: return FCOND_EQ;
    }
}

constexpr static const i8* ASM_CONDITION_NAMES[] = {
    "eq", "ne", "lt", "le", "gt", "ge", "a", "ae", "b", "be", "tz", "tnz"
};

constexpr static const i8* ASM_FLOAT_CONDITION_NAMES[] = {
    "eq", "ne", "lt", "le", "gt", "ge"
};

enum class Size : u64 {
    BITS8 = 0, BITS16 = 1, BITS32 = 2, BITS64 = 3,
    BYTE = BITS8,
    WORD = BITS16, DWORD = BITS32, QWORD = BITS64,
    HALF = BITS16, INT = BITS32, LONG = BITS64,
    FLOAT32 = 4, FLOAT64 = 5, FLOAT = FLOAT32, DOUBLE = FLOAT64,
    MEMORY = 6, VECTOR = 7
};

inline u32 bytesForSize(Size size) {
    switch (size) {
        case Size::BITS8:
            return 1;
        case Size::BITS16:
            return 2;
        case Size::BITS32:
        case Size::FLOAT32:
            return 4;
        case Size::BITS64:
        case Size::FLOAT64:
            return 8;
        default:
            unreachable("Not a scalar type.");
    }
}

inline Size sizeForBytes(u32 bytes) {
    switch (bytes) {
        case 1:
            return Size::BITS8;
        case 2:
            return Size::BITS16;
        case 3 ... 4:
            return Size::BITS32;
        case 5 ... 8:
            return Size::BITS64;
        default:
            unreachable("Not a scalar size.");
    }
}

struct Repr {
    union {
        struct { Size size : 8; u64 : 56; } scalar;
        struct { Size size : 8; Size laneSize : 8; u64 : 16; u64 length : 32; } vector;
        struct { Size size : 8; u64 align : 8; u64 : 16; u64 length : 32; } memory;
    };

    inline Size kind() const {
        return scalar.size;
    }

    inline u32 size() const {
        switch (scalar.size) {
            case Size::BITS8:
                return 1;
            case Size::BITS16:
                return 2;
            case Size::BITS32:
            case Size::FLOAT32:
                return 4;
            case Size::BITS64:
            case Size::FLOAT64:
                return 8;
            case Size::VECTOR:
                return Scalar(vector.laneSize).size() * vector.length;
            case Size::MEMORY:
                return memory.length;
        }
    }

    inline u32 alignment() const {
        switch (scalar.size) {
            case Size::BITS8:
                return 1;
            case Size::BITS16:
                return 2;
            case Size::BITS32:
            case Size::FLOAT32:
                return 4;
            case Size::BITS64:
            case Size::FLOAT64:
                return 8;
            case Size::VECTOR:
                return Scalar(vector.laneSize).size() * vector.length;
            case Size::MEMORY:
                return memory.align;
        }
    }

    inline static Repr Scalar(Size size) {
        assert(size != Size::MEMORY && size != Size::VECTOR);
        return Repr { .scalar = { .size = size } };
    }

    inline static Repr Memory(u8 alignment, u32 length) {
        return Repr { .memory = { .size = Size::MEMORY, .align = alignment, .length = length } };
    }

    inline static Repr Vector(Size laneSize, u32 length) {
        assert(length);
        if (length == 1)
            return Scalar(laneSize);
        return Repr { .vector = { .size = Size::VECTOR, .laneSize = laneSize, .length = length } };
    }
};

static_assert(sizeof(Repr) == 8);

constexpr const i8* ASM_SIZE_NAMES[] = {
    "8", "16", "32", "64", "f32", "f64", "", "", ""
};

template<typename IO>
inline IO format_impl(IO io, Size size) {
    return format_impl<IO>(io, ASM_SIZE_NAMES[(unsigned)size]);
}

template<typename IO>
inline IO format_impl(IO io, Repr repr) {
    switch (repr.scalar.size) {
        case Size::BITS8:
            return format(io, "bits8");
        case Size::BITS16:
            return format(io, "bits16");
        case Size::BITS32:
            return format(io, "bits32");
        case Size::BITS64:
            return format(io, "bits64");
        case Size::FLOAT32:
            return format(io, "float32");
        case Size::FLOAT64:
            return format(io, "float64");
        case Size::VECTOR:
            return format(io, Repr::Scalar(repr.vector.laneSize), 'x', repr.vector.length);
        case Size::MEMORY:
            io = format(io, "memory[", repr.memory.length);
            if (repr.memory.align > 1) io = format(io, " (align ", repr.memory.align, ")");
            return format(io, "]");
    }
}

/*
 * Opcodes come in the following classes:
 *
 *  - NULLARY: The instruction has no operands.
 *
 *  - UNARY_GP: The instruction takes one GP register operand.
 *  - UNARY_FP: The instruction takes one FP register operand.
 *  - UNARY_JUMP: The instruction takes one operand, which can be a GP register or label.
 *  - UNARY_GP_IMM = The instruction takes one operand, which can be a GP register or immediate.
 *
 *  - BINARY_GP: The instruction takes two GP register operands.
 *  - BINARY_FP: The instruction takes two FP register operands.
 *  - BINARY_FP_GP: The instruction takes two register operands, the first being FP, the second being GP.
 *  - BINARY_GP_FP: The instruction takes two register operands, the first being GP, the second being FP.
 *  - BINARY_GP_IMM: The instruction takes a GP register operand, then either a GP register or immediate operand..
 *  - BINARY_FP_F32: The instruction takes an FP register operand, then either an FP register or F32 constant operand.
 *  - BINARY_FP_F64: The instruction takes an FP register operand, then either an FP register or F64 constant operand.
 *  - BINARY_GP_MEM: The instruction takes a GP register operand, then a memory location.
 *  - BINARY_FP_MEM: The instruction takes an FP register operand, then a memory location.
 *  - BINARY_MEM_GP_IMM: The instruction takes a memory location, then a GP register or immediate.
 *  - BINARY_MEM_FP_F32: The instruction takes a memory location, then an FP register or F32 constant.
 *  - BINARY_MEM_FP_F64: The instruction takes a memory location, then an FP register or F64 constant.
 *  - BINARY_BRANCH_GP: The instruction takes a GP register or label, then a GP register.
 *  - BINARY_GP_IMM64: The instruction takes a GP register, followed by a 64-bit immediate.
 *
 *  - TERNARY_GP_IMM: The instruction takes a GP register, then two operands that can be either a GP register or immediate.
 *  - TERNARY_FP_F32: The instruction takes an FP register, then two operands that can be either an FP register or F32 constant.
 *  - TERNARY_FP_F64: The instruction takes an FP register, then two operands that can be either an FP register or F64 constant.
 *  - TERNARY_LOAD_INDEX_GP: The instruction takes a GP register, then a memory location, then a GP register.
 *  - TERNARY_LOAD_INDEX_FP: The instruction takes an FP register, then a memory location, then a GP register.
 *  - TERNARY_STORE_INDEX_GP_IMM: The instruction takes a memory location, then a GP register or immediate, then a GP register.
 *  - TERNARY_STORE_INDEX_FP_F32: The instruction takes a memory location, then an FP register or F32 constant, then a GP register.
 *  - TERNARY_STORE_INDEX_FP_F64: The instruction takes a memory location, then an FP register or F64 constant, then a GP register.
 *  - TERNARY_MEMORY_OP: The instruction takes two memory locations, then a GP register or immediate.
 *
 *  - COMPARE_GP_IMM: The instruction takes an integer condition, then a GP register, then two operands which can be either a GP register or immediate.
 *  - COMPARE_FP_F32: The instruction takes a float condition, then an FP register, then two operands which can be either an FP register or F32 constant.
 *  - COMPARE_FP_F64: The instruction takes a float condition, then an FP register, then two operands which can be either an FP register or F64 constant.
 *  - COMPARE_MEMORY: The instruction takes a signed integer condition, then a GP register, then two memory locations, then either a GP register or immediate.

 *  - BRANCH_COMPARE_GP_IMM: The instruction takes a GP register or label, then two operands that can be either a GP register or immediate.
 *  - BRANCH_COMPARE_FP_F32: The instruction takes a GP register or label, then two operands that can be either a FP register or F32 constant.
 *  - BRANCH_COMPARE_FP_F64: The instruction takes a GP register or label, then two operands that can be either a FP register or F64 constant.
 *  - BRANCH_COMPARE_MEMORY: The instruction takes a signed integer condition, then a GP register or label, then two memory locations, then either a GP register or immediate.
 *
 *  - SELECT_GP_IMM: The instruction takes an integer condition, then two GP registers or immediates, then another two GP registers or immediates.
 */

#define FOR_EACH_ASM_OPCODE(macro)                                                          \
    /* Block 1: Integer arithmetic*/                                                        \
    macro(ADD8,         add8,           0x00,   Size::BITS8,    TERNARY_GP_IMM)             \
    macro(ADD16,        add16,          0x01,   Size::BITS16,   TERNARY_GP_IMM)             \
    macro(ADD32,        add32,          0x02,   Size::BITS32,   TERNARY_GP_IMM)             \
    macro(ADD64,        add64,          0x03,   Size::BITS64,   TERNARY_GP_IMM)             \
    macro(SUB8,         sub8,           0x04,   Size::BITS8,    TERNARY_GP_IMM)             \
    macro(SUB16,        sub16,          0x05,   Size::BITS16,   TERNARY_GP_IMM)             \
    macro(SUB32,        sub32,          0x06,   Size::BITS32,   TERNARY_GP_IMM)             \
    macro(SUB64,        sub64,          0x07,   Size::BITS64,   TERNARY_GP_IMM)             \
    macro(MUL8,         mul8,           0x08,   Size::BITS8,    TERNARY_GP_IMM)             \
    macro(MUL16,        mul16,          0x09,   Size::BITS16,   TERNARY_GP_IMM)             \
    macro(MUL32,        mul32,          0x0a,   Size::BITS32,   TERNARY_GP_IMM)             \
    macro(MUL64,        mul64,          0x0b,   Size::BITS64,   TERNARY_GP_IMM)             \
    macro(SDIV8,        sdiv8,          0x0c,   Size::BITS8,    TERNARY_GP_IMM)             \
    macro(SDIV16,       sdiv16,         0x0d,   Size::BITS16,   TERNARY_GP_IMM)             \
    macro(SDIV32,       sdiv32,         0x0e,   Size::BITS32,   TERNARY_GP_IMM)             \
    macro(SDIV64,       sdiv64,         0x0f,   Size::BITS64,   TERNARY_GP_IMM)             \
    macro(UDIV8,        udiv8,          0x10,   Size::BITS8,    TERNARY_GP_IMM)             \
    macro(UDIV16,       udiv16,         0x11,   Size::BITS16,   TERNARY_GP_IMM)             \
    macro(UDIV32,       udiv32,         0x12,   Size::BITS32,   TERNARY_GP_IMM)             \
    macro(UDIV64,       udiv64,         0x13,   Size::BITS64,   TERNARY_GP_IMM)             \
    macro(SREM8,        srem8,          0x14,   Size::BITS8,    TERNARY_GP_IMM)             \
    macro(SREM16,       srem16,         0x15,   Size::BITS16,   TERNARY_GP_IMM)             \
    macro(SREM32,       srem32,         0x16,   Size::BITS32,   TERNARY_GP_IMM)             \
    macro(SREM64,       srem64,         0x17,   Size::BITS64,   TERNARY_GP_IMM)             \
    macro(UREM8,        urem8,          0x18,   Size::BITS8,    TERNARY_GP_IMM)             \
    macro(UREM16,       urem16,         0x19,   Size::BITS16,   TERNARY_GP_IMM)             \
    macro(UREM32,       urem32,         0x1a,   Size::BITS32,   TERNARY_GP_IMM)             \
    macro(UREM64,       urem64,         0x1b,   Size::BITS64,   TERNARY_GP_IMM)             \
    macro(NEG8,         neg8,           0x1c,   Size::BITS8,    BINARY_GP)                  \
    macro(NEG16,        neg16,          0x1d,   Size::BITS16,   BINARY_GP)                  \
    macro(NEG32,        neg32,          0x1e,   Size::BITS32,   BINARY_GP)                  \
    macro(NEG64,        neg64,          0x1f,   Size::BITS64,   BINARY_GP)                  \
    \
    /* Block 2: Floating-point arithmetic */                                                \
    macro(FADD32,       fadd32,         0x20,   Size::FLOAT32,  TERNARY_FP_F32)             \
    macro(FADD64,       fadd64,         0x21,   Size::FLOAT64,  TERNARY_FP_F64)             \
    macro(FSUB32,       fsub32,         0x22,   Size::FLOAT32,  TERNARY_FP_F32)             \
    macro(FSUB64,       fsub64,         0x23,   Size::FLOAT64,  TERNARY_FP_F64)             \
    macro(FMUL32,       fmul32,         0x24,   Size::FLOAT32,  TERNARY_FP_F32)             \
    macro(FMUL64,       fmul64,         0x25,   Size::FLOAT64,  TERNARY_FP_F64)             \
    macro(FDIV32,       fdiv32,         0x26,   Size::FLOAT32,  TERNARY_FP_F32)             \
    macro(FDIV64,       fdiv64,         0x27,   Size::FLOAT64,  TERNARY_FP_F64)             \
    macro(FREM32,       frem32,         0x28,   Size::FLOAT32,  TERNARY_FP_F32)             \
    macro(FREM64,       frem64,         0x29,   Size::FLOAT64,  TERNARY_FP_F64)             \
    macro(FNEG32,       fneg32,         0x2a,   Size::FLOAT32,  BINARY_FP)                  \
    macro(FNEG64,       fneg64,         0x2b,   Size::FLOAT64,  BINARY_FP)                  \
    macro(FSQRT32,      fsqrt32,        0x2c,   Size::FLOAT32,  BINARY_FP)                  \
    macro(FSQRT64,      fsqrt64,        0x2d,   Size::FLOAT64,  BINARY_FP)                  \
    macro(FMIN32,       fmin32,         0x2e,   Size::FLOAT32,  TERNARY_FP_F32)             \
    macro(FMIN64,       fmin64,         0x2f,   Size::FLOAT64,  TERNARY_FP_F64)             \
    macro(FMAX32,       fmax32,         0x30,   Size::FLOAT32,  TERNARY_FP_F32)             \
    macro(FMAX64,       fmax64,         0x31,   Size::FLOAT64,  TERNARY_FP_F64)             \
    macro(FROUND32,     fround32,       0x32,   Size::FLOAT32,  BINARY_FP)                  \
    macro(FROUND64,     fround64,       0x33,   Size::FLOAT64,  BINARY_FP)                  \
    macro(FFLOOR32,     ffloor32,       0x34,   Size::FLOAT32,  BINARY_FP)                  \
    macro(FFLOOR64,     ffloor64,       0x35,   Size::FLOAT64,  BINARY_FP)                  \
    macro(FCEIL32,      fceil32,        0x36,   Size::FLOAT32,  BINARY_FP)                  \
    macro(FCEIL64,      fceil64,        0x37,   Size::FLOAT64,  BINARY_FP)                  \
    macro(FTRUNC32,     ftrunc32,       0x38,   Size::FLOAT32,  BINARY_FP)                  \
    macro(FTRUNC64,     ftrunc64,       0x39,   Size::FLOAT64,  BINARY_FP)                  \
    macro(FABS32,       fabs32,         0x3a,   Size::FLOAT32,  BINARY_FP)                  \
    macro(FABS64,       fabs64,         0x3b,   Size::FLOAT64,  BINARY_FP)                  \
    \
    /* Block 3: Integer bitwise operations */                                               \
    macro(AND8,         and8,           0x3c,   Size::BITS8,    TERNARY_GP_IMM)             \
    macro(AND16,        and16,          0x3d,   Size::BITS16,   TERNARY_GP_IMM)             \
    macro(AND32,        and32,          0x3e,   Size::BITS32,   TERNARY_GP_IMM)             \
    macro(AND64,        and64,          0x3f,   Size::BITS64,   TERNARY_GP_IMM)             \
    macro(XOR8,         xor8,           0x40,   Size::BITS8,    TERNARY_GP_IMM)             \
    macro(XOR16,        xor16,          0x41,   Size::BITS16,   TERNARY_GP_IMM)             \
    macro(XOR32,        xor32,          0x42,   Size::BITS32,   TERNARY_GP_IMM)             \
    macro(XOR64,        xor64,          0x43,   Size::BITS64,   TERNARY_GP_IMM)             \
    macro(OR8,          or8,            0x44,   Size::BITS8,    TERNARY_GP_IMM)             \
    macro(OR16,         or16,           0x45,   Size::BITS16,   TERNARY_GP_IMM)             \
    macro(OR32,         or32,           0x46,   Size::BITS32,   TERNARY_GP_IMM)             \
    macro(OR64,         or64,           0x47,   Size::BITS64,   TERNARY_GP_IMM)             \
    macro(NOT8,         not8,           0x48,   Size::BITS8,    BINARY_GP)                  \
    macro(NOT16,        not16,          0x49,   Size::BITS16,   BINARY_GP)                  \
    macro(NOT32,        not32,          0x40,   Size::BITS32,   BINARY_GP)                  \
    macro(NOT64,        not64,          0x49,   Size::BITS64,   BINARY_GP)                  \
    macro(SHL8,         shl8,           0x4a,   Size::BITS8,    TERNARY_GP_IMM)             \
    macro(SHL16,        shl16,          0x4b,   Size::BITS16,   TERNARY_GP_IMM)             \
    macro(SHL32,        shl32,          0x4c,   Size::BITS32,   TERNARY_GP_IMM)             \
    macro(SHL64,        shl64,          0x4d,   Size::BITS64,   TERNARY_GP_IMM)             \
    macro(SHR8,         shr8,           0x4e,   Size::BITS8,    TERNARY_GP_IMM)             \
    macro(SHR16,        shr16,          0x4f,   Size::BITS16,   TERNARY_GP_IMM)             \
    macro(SHR32,        shr32,          0x50,   Size::BITS32,   TERNARY_GP_IMM)             \
    macro(SHR64,        shr64,          0x51,   Size::BITS64,   TERNARY_GP_IMM)             \
    macro(SAR8,         sar8,           0x52,   Size::BITS8,    TERNARY_GP_IMM)             \
    macro(SAR16,        sar16,          0x53,   Size::BITS16,   TERNARY_GP_IMM)             \
    macro(SAR32,        sar32,          0x54,   Size::BITS32,   TERNARY_GP_IMM)             \
    macro(SAR64,        sar64,          0x55,   Size::BITS64,   TERNARY_GP_IMM)             \
    macro(ROL8,         rol8,           0x56,   Size::BITS8,    TERNARY_GP_IMM)             \
    macro(ROL16,        rol16,          0x57,   Size::BITS16,   TERNARY_GP_IMM)             \
    macro(ROL32,        rol32,          0x58,   Size::BITS32,   TERNARY_GP_IMM)             \
    macro(ROL64,        rol64,          0x59,   Size::BITS64,   TERNARY_GP_IMM)             \
    macro(ROR8,         ror8,           0x5a,   Size::BITS8,    TERNARY_GP_IMM)             \
    macro(ROR16,        ror16,          0x5b,   Size::BITS16,   TERNARY_GP_IMM)             \
    macro(ROR32,        ror32,          0x5c,   Size::BITS32,   TERNARY_GP_IMM)             \
    macro(ROR64,        ror64,          0x5d,   Size::BITS64,   TERNARY_GP_IMM)             \
    macro(POPC8,        popc8,          0x5e,   Size::BITS8,    BINARY_GP)                  \
    macro(POPC16,       popc16,         0x5f,   Size::BITS16,   BINARY_GP)                  \
    macro(POPC32,       popc32,         0x60,   Size::BITS32,   BINARY_GP)                  \
    macro(POPC64,       popc64,         0x61,   Size::BITS64,   BINARY_GP)                  \
    macro(LZC8,         lzc8,           0x62,   Size::BITS8,    BINARY_GP)                  \
    macro(LZC16,        lzc16,          0x63,   Size::BITS16,   BINARY_GP)                  \
    macro(LZC32,        lzc32,          0x64,   Size::BITS32,   BINARY_GP)                  \
    macro(LZC64,        lzc64,          0x65,   Size::BITS64,   BINARY_GP)                  \
    macro(TZC8,         tzc8,           0x66,   Size::BITS8,    BINARY_GP)                  \
    macro(TZC16,        tzc16,          0x67,   Size::BITS16,   BINARY_GP)                  \
    macro(TZC32,        tzc32,          0x68,   Size::BITS32,   BINARY_GP)                  \
    macro(TZC64,        tzc64,          0x69,   Size::BITS64,   BINARY_GP)                  \
    \
    /* Block 4: Conditional operations on values. */                                        \
    macro(ISZ,          isz,            0x6a,   Size::OTHER,    BINARY_GP)                  \
    macro(ISNZ,         isnz,           0x6b,   Size::OTHER,    BINARY_GP)                  \
    macro(CMPCC8,       cmpcc8,         0x6c,   Size::BITS8,    COMPARE_GP_IMM)             \
    macro(CMPCC16,      cmpcc16,        0x6d,   Size::BITS16,   COMPARE_GP_IMM)             \
    macro(CMPCC32,      cmpcc32,        0x6e,   Size::BITS32,   COMPARE_GP_IMM)             \
    macro(CMPCC64,      cmpcc64,        0x6f,   Size::BITS64,   COMPARE_GP_IMM)             \
    macro(FCMPCC32,     fcmpcc32,       0x72,   Size::FLOAT32,  COMPARE_FP_F32)             \
    macro(FCMPCC64,     fcmpcc64,       0x73,   Size::FLOAT64,  COMPARE_FP_F64)             \
    macro(SELCC8,       selcc8,         0x74,   Size::BITS8,    SELECT_GP_IMM)              \
    macro(SELCC16,      selcc16,        0x75,   Size::BITS16,   SELECT_GP_IMM)              \
    macro(SELCC32,      selcc32,        0x76,   Size::BITS32,   SELECT_GP_IMM)              \
    macro(SELCC64,      selcc64,        0x77,   Size::BITS64,   SELECT_GP_IMM)              \
    macro(FSELCC32,     fselcc32,       0x78,   Size::FLOAT32,  SELECT_FP_F32)              \
    macro(FSELCC64,     fselcc64,       0x79,   Size::FLOAT64,  SELECT_FP_F64)              \
    \
    /* Block 5: Data operations. */                                                         \
    macro(MOV8,         mov8,           0x7a,   Size::BITS8,    BINARY_GP_IMM)              \
    macro(MOV16,        mov16,          0x7b,   Size::BITS16,   BINARY_GP_IMM)              \
    macro(MOV32,        mov32,          0x7c,   Size::BITS32,   BINARY_GP_IMM)              \
    macro(MOV64,        mov64,          0x7d,   Size::BITS64,   BINARY_GP_IMM)              \
    macro(FMOV32,       fmov32,         0x7e,   Size::FLOAT32,  BINARY_FP_F32)              \
    macro(FMOV64,       fmov64,         0x7f,   Size::FLOAT64,  BINARY_FP_F64)              \
    macro(PUSH8,        push8,          0x80,   Size::BITS8,    UNARY_GP_IMM)               \
    macro(PUSH16,       push16,         0x81,   Size::BITS16,   UNARY_GP_IMM)               \
    macro(PUSH32,       push32,         0x82,   Size::BITS32,   UNARY_GP_IMM)               \
    macro(PUSH64,       push64,         0x83,   Size::BITS64,   UNARY_GP_IMM)               \
    macro(POP8,         pop8,           0x84,   Size::BITS8,    UNARY_GP)                   \
    macro(POP16,        pop16,          0x85,   Size::BITS16,   UNARY_GP)                   \
    macro(POP32,        pop32,          0x86,   Size::BITS32,   UNARY_GP)                   \
    macro(POP64,        pop64,          0x87,   Size::BITS64,   UNARY_GP)                   \
    macro(FPUSH32,      fpush32,        0x88,   Size::FLOAT32,  UNARY_FP_F32)               \
    macro(FPUSH64,      fpush64,        0x89,   Size::FLOAT64,  UNARY_FP_F64)               \
    macro(FPOP32,       fpop32,         0x8a,   Size::FLOAT32,  UNARY_FP)                   \
    macro(FPOP64,       fpop64,         0x8b,   Size::FLOAT64,  UNARY_FP)                   \
    macro(LDS8,         lds8,           0x8c,   Size::BITS8,    BINARY_GP_MEM)              \
    macro(LDS16,        lds16,          0x8d,   Size::BITS16,   BINARY_GP_MEM)              \
    macro(LDS32,        lds32,          0x8e,   Size::BITS32,   BINARY_GP_MEM)              \
    macro(LDZ8,         ldz8,           0x8f,   Size::BITS8,    BINARY_GP_MEM)              \
    macro(LDZ16,        ldz16,          0x90,   Size::BITS16,   BINARY_GP_MEM)              \
    macro(LDZ32,        ldz32,          0x91,   Size::BITS32,   BINARY_GP_MEM)              \
    macro(LD64,         ld64,           0x92,   Size::BITS64,   BINARY_GP_MEM)              \
    macro(ST8,          st8,            0x93,   Size::BITS8,    BINARY_MEM_GP_IMM)          \
    macro(ST16,         st16,           0x94,   Size::BITS16,   BINARY_MEM_GP_IMM)          \
    macro(ST32,         st32,           0x95,   Size::BITS32,   BINARY_MEM_GP_IMM)          \
    macro(ST64,         st64,           0x96,   Size::BITS64,   BINARY_MEM_GP_IMM)          \
    macro(LDIS8,        ldis8,          0x97,   Size::BITS8,    TERNARY_LOAD_INDEX_GP)      \
    macro(LDIS16,       ldis16,         0x98,   Size::BITS16,   TERNARY_LOAD_INDEX_GP)      \
    macro(LDIS32,       ldis32,         0x99,   Size::BITS32,   TERNARY_LOAD_INDEX_GP)      \
    macro(LDIZ8,        ldiz8,          0x9a,   Size::BITS8,    TERNARY_LOAD_INDEX_GP)      \
    macro(LDIZ16,       ldiz16,         0x9b,   Size::BITS16,   TERNARY_LOAD_INDEX_GP)      \
    macro(LDIZ32,       ldiz32,         0x9c,   Size::BITS32,   TERNARY_LOAD_INDEX_GP)      \
    macro(LDI64,        ldi64,          0x9d,   Size::BITS64,   TERNARY_LOAD_INDEX_GP)      \
    macro(LAI8,         lai8,           0x9e,   Size::BITS8,    TERNARY_LOAD_INDEX_GP)      \
    macro(LAI16,        lai16,          0x9f,   Size::BITS16,   TERNARY_LOAD_INDEX_GP)      \
    macro(LAI32,        lai32,          0xa0,   Size::BITS32,   TERNARY_LOAD_INDEX_GP)      \
    macro(LAI64,        lai64,          0xa1,   Size::BITS64,   TERNARY_LOAD_INDEX_GP)      \
    macro(STI8,         sti8,           0xa2,   Size::BITS8,    TERNARY_STORE_INDEX_GP_IMM) \
    macro(STI16,        sti16,          0xa3,   Size::BITS16,   TERNARY_STORE_INDEX_GP_IMM) \
    macro(STI32,        sti32,          0xa4,   Size::BITS32,   TERNARY_STORE_INDEX_GP_IMM) \
    macro(STI64,        sti64,          0xa5,   Size::BITS64,   TERNARY_STORE_INDEX_GP_IMM) \
    macro(FLD32,        fld32,          0xa6,   Size::FLOAT32,  BINARY_FP_MEM)              \
    macro(FLD64,        fld64,          0xa7,   Size::FLOAT64,  BINARY_FP_MEM)              \
    macro(FST32,        fst32,          0xa8,   Size::FLOAT32,  BINARY_MEM_FP_F32)          \
    macro(FST64,        fst64,          0xa9,   Size::FLOAT64,  BINARY_MEM_FP_F64)          \
    macro(FLDI32,       fldi32,         0xaa,   Size::FLOAT32,  TERNARY_LOAD_INDEX_FP)      \
    macro(FLDI64,       fldi64,         0xab,   Size::FLOAT64,  TERNARY_LOAD_INDEX_FP)      \
    macro(FSTI32,       fsti32,         0xac,   Size::FLOAT32,  TERNARY_STORE_INDEX_FP_F32) \
    macro(FSTI64,       fsti64,         0xad,   Size::FLOAT64,  TERNARY_STORE_INDEX_FP_F64) \
    macro(LA,           la,             0xae,   Size::BITS64,   BINARY_GP_MEM)              \
    macro(LC,           lc,             0xaf,   Size::BITS64,   BINARY_GP_IMM64)            \
    \
    /* Block 6: Jumps and branches. */                                                      \
    macro(BR,           br,             0xb0,   Size::OTHER,    UNARY_JUMP)                 \
    macro(BRZ,          brz,            0xb1,   Size::OTHER,    BINARY_BRANCH_GP)           \
    macro(BRNZ,         brnz,           0xb2,   Size::OTHER,    BINARY_BRANCH_GP)           \
    macro(BRCC8,        brcc8,          0xb3,   Size::BITS8,    BRANCH_COMPARE_GP_IMM)      \
    macro(BRCC16,       brcc16,         0xb4,   Size::BITS16,   BRANCH_COMPARE_GP_IMM)      \
    macro(BRCC32,       brcc32,         0xb5,   Size::BITS32,   BRANCH_COMPARE_GP_IMM)      \
    macro(BRCC64,       brcc64,         0xb6,   Size::BITS64,   BRANCH_COMPARE_GP_IMM)      \
    macro(FBRCC32,      fbrcc32,        0xb9,   Size::FLOAT32,  BRANCH_COMPARE_FP_F32)      \
    macro(FBRCC64,      fbrcc64,        0xba,   Size::FLOAT64,  BRANCH_COMPARE_FP_F64)      \
    \
    /* Block 7: Call frame and stack instructions. */                                       \
    macro(ENTER,        enter,          0xbb,   Size::OTHER,    NULLARY)                    \
    macro(LEAVE,        leave,          0xbc,   Size::OTHER,    NULLARY)                    \
    macro(STACK,        stack,          0xbd,   Size::OTHER,    UNARY_GP_IMM)               \
    macro(ALLOCA,       alloca,         0xbe,   Size::OTHER,    BINARY_GP_IMM)              \
    macro(UNSTACK,      unstack,        0xbf,   Size::OTHER,    UNARY_GP_IMM)               \
    macro(CALL,         call,           0xc0,   Size::OTHER,    UNARY_JUMP)                 \
    macro(RET,          ret,            0xc1,   Size::OTHER,    NULLARY)                    \
    \
    /* Block 8: Conversion instructions. */                                                 \
    macro(SXT8,         sxt8,           0xc2,   Size::BITS8,    BINARY_GP)                  \
    macro(SXT16,        sxt16,          0xc3,   Size::BITS16,   BINARY_GP)                  \
    macro(SXT32,        sxt32,          0xc4,   Size::BITS32,   BINARY_GP)                  \
    macro(ZXT8,         zxt8,           0xc5,   Size::BITS8,    BINARY_GP)                  \
    macro(ZXT16,        zxt16,          0xc6,   Size::BITS16,   BINARY_GP)                  \
    macro(ZXT32,        zxt32,          0xc7,   Size::BITS32,   BINARY_GP)                  \
    macro(I8TOF32,      i8tof32,        0xc8,   Size::BITS8,    BINARY_FP_GP)               \
    macro(I16TOF32,     i16tof32,       0xc9,   Size::BITS16,   BINARY_FP_GP)               \
    macro(I32TOF32,     i32tof32,       0xca,   Size::BITS32,   BINARY_FP_GP)               \
    macro(I64TOF32,     i64tof32,       0xcb,   Size::BITS64,   BINARY_FP_GP)               \
    macro(I8TOF64,      i8tof64,        0xcc,   Size::BITS8,    BINARY_FP_GP)               \
    macro(I16TOF64,     i16tof64,       0xcd,   Size::BITS16,   BINARY_FP_GP)               \
    macro(I32TOF64,     i32tof64,       0xce,   Size::BITS32,   BINARY_FP_GP)               \
    macro(I64TOF64,     i64tof64,       0xcf,   Size::BITS64,   BINARY_FP_GP)               \
    macro(F32TOI8,      f32toi8,        0xd0,   Size::BITS8,    BINARY_GP_FP)               \
    macro(F32TOI16,     f32toi16,       0xd1,   Size::BITS16,   BINARY_GP_FP)               \
    macro(F32TOI32,     f32toi32,       0xd2,   Size::BITS32,   BINARY_GP_FP)               \
    macro(F32TOI64,     f32toi64,       0xd3,   Size::BITS64,   BINARY_GP_FP)               \
    macro(F64TOI8,      f64toi8,        0xd4,   Size::BITS8,    BINARY_GP_FP)               \
    macro(F64TOI16,     f64toi16,       0xd5,   Size::BITS16,   BINARY_GP_FP)               \
    macro(F64TOI32,     f64toi32,       0xd6,   Size::BITS32,   BINARY_GP_FP)               \
    macro(F64TOI64,     f64toi64,       0xd7,   Size::BITS64,   BINARY_GP_FP)               \
    macro(F32TOF64,     f32tof64,       0xd8,   Size::FLOAT32,  BINARY_FP)                  \
    macro(F64TOF32,     f64tof32,       0xd9,   Size::FLOAT64,  BINARY_FP)                  \
    macro(F32FROMBITS,  f32frombits,    0xda,   Size::FLOAT32,  BINARY_FP_GP)               \
    macro(F64FROMBITS,  f64frombits,    0xdb,   Size::FLOAT64,  BINARY_FP_GP)               \
    macro(F32TOBITS,    f32tobits,      0xdc,   Size::FLOAT32,  BINARY_GP_FP)               \
    macro(F64TOBITS,    f64tobits,      0xdd,   Size::FLOAT64,  BINARY_GP_FP)               \
    \
    /* Block 9: Bulk memory operations. */                                                  \
    macro(MCPY,         mcpy,           0xde,   Size::OTHER,    TERNARY_MEMORY_OP)          \
    macro(MMOV,         mmov,           0xdf,   Size::OTHER,    TERNARY_MEMORY_OP)          \
    macro(MSET,         mset,           0xe0,   Size::OTHER,    TERNARY_MEMORY_OP)          \
    macro(MCMPCC,       mcmpcc,         0xe1,   Size::OTHER,    COMPARE_MEMORY)             \
    macro(MBRCC,        mbrcc,          0xe2,   Size::OTHER,    BRANCH_COMPARE_MEMORY)      \
    \
    /* Block 10: Misc. */                                                                   \
    macro(TRAP,         trap,           0xe3,   Size::OTHER,    NULLARY)                    \

constexpr static u32 NUM_ASM_OPCODES = 0xe4;

#define DEFINE_OPCODE_ENUM_CXX(upper, ...) upper,
enum class ASMOpcode {
    FOR_EACH_ASM_OPCODE(DEFINE_OPCODE_ENUM_CXX)
};
#undef DEFINE_OPCODE_ENUM_CXX

#define DEFINE_OPCODE_NAME(_, lower, ...) #lower,
constexpr static const i8* ASM_OPCODE_NAMES[] = {
    FOR_EACH_ASM_OPCODE(DEFINE_OPCODE_NAME)
};
#undef DEFINE_OPCODE_NAME

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
struct FakeAssembler {
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
    constexpr static EndianOrder endianness = EndianOrder::LITTLE;

    static inline RegSet caller_saved_gps() {
        return RegSet(GPREGS, 16);
    }

    static inline RegSet caller_saved_fps() {
        return RegSet(FPREGS, 16);
    }

    static inline RegSet caller_saves() {
        return caller_saved_gps() | caller_saved_fps();
    }

    static inline RegSet callee_saved_gps() {
        return RegSet(GPREGS + 16, 14);
    }

    static inline RegSet callee_saved_fps() {
        return RegSet(FPREGS + 16, 16);
    }

    static inline RegSet callee_saves() {
        return callee_saved_gps() | callee_saved_fps();
    }

    static inline RegSet gps() {
        return RegSet(GPREGS, 30);
    }

    static inline RegSet fps() {
        return RegSet(FPREGS, 32);
    }

    static inline bool is_gp(mreg r) {
        return r < 32;
    }

    static inline bool is_fp(mreg r) {
        return r >= 32 && r < 64;
    }

    static inline const_slice<i8> reg_name(mreg r) {
        return const_slice<i8>{ ASM_REGISTER_NAMES[i32(r)], findc(ASM_REGISTER_NAMES[i32(r)], '\0') };
    }

    static inline Size word_size() {
        return Size::BITS64;
    }

    static inline Size ptr_size() {
        return Size::BITS64;
    }

    static inline RegSet clobbers(ASMOpcode opcode) {
        return RegSet();
    }

    static inline void* start_placing_parameters() {
        return nullptr;
    }

    static inline MaybePair<ASMVal> place_scalar_parameter(void* state, Repr repr) {
        return None();
    }

    static inline MaybePair<ASMVal> place_aggregate_parameter(void* state, const_slice<Repr> members) {
        return None();
    }

    static inline void finish_placing_parameters(void* state) {}

    static inline MaybePair<ASMVal> place_scalar_return_value(void* state, Repr repr) {
        return None();
    }

    static inline MaybePair<ASMVal> place_aggregate_return_value(void* state, Repr repr) {
        return None();
    }
};

// Used to mock up an existing backend, but instead write the instructions as textual representation to an output stream.
// You can use this as an example of the static interface of backend target types.
template<typename Assembler = FakeAssembler>
struct Printer : public Assembler {
    using Underlying = Assembler;

    static void write_mval(fd io, Assembly& as, const ASMVal& val) {
        switch (val.kind) {
            case ASMVal::NONE:
                ::write(io, "!invalid");
                break;
            case ASMVal::FP:
                ::write(io, Assembler::reg_name(val.fp));
                break;
            case ASMVal::GP:
                ::write(io, Assembler::reg_name(val.gp));
                break;
            case ASMVal::IMM:
                ::write(io, val.imm);
                break;
            case ASMVal::IMM64:
                ::write(io, val.imm64);
                break;
            case ASMVal::F32:
                ::write(io, val.f32);
                break;
            case ASMVal::F64:
                ::write(io, val.f64);
                break;
            case ASMVal::MEM: switch (val.memkind) {
                case ASMVal::REG_OFFSET:
                    if (val.offset == 0)
                        ::write(io, '[', Assembler::reg_name(val.base), ']');
                    else
                        ::write(io, '[', Assembler::reg_name(val.base), val.offset < 0 ? " - " : " + ", val.offset < 0 ? -val.offset : val.offset, ']');
                    break;
                case ASMVal::FUNC_LABEL:
                    ::write(io, "func:", as.symtab[val.sym]);
                    break;
                case ASMVal::LOCAL_LABEL:
                    ::write(io, as.symtab[val.sym]);
                    break;
                case ASMVal::DATA_LABEL:
                    ::write(io, "data:", as.symtab[val.sym]);
                    break;
                case ASMVal::STATIC_LABEL:
                    ::write(io, "static:", as.symtab[val.sym]);
                    break;
            }
            break;
        }
    }

    static void write_label(fd io, Assembly& as, Symbol label) {
        ::write(io, as.symtab[label], ":\n");
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
        if (opcode == ASMOpcode::BRZ)
            ::write(io, "  br.z ");
        else if (opcode == ASMOpcode::BRNZ)
            ::write(io, "  br.nz ");
        else
            ::write(io, "  ", ASM_OPCODE_NAMES[(unsigned)opcode], ' ');
        write_mval(io, as, dst);
        ::write(io, ", ");
        write_mval(io, as, src);
        ::write(io, '\n');
    }

    static void write_big_constant(fd io, Assembly& as, ASMOpcode opcode, const ASMVal& dst, const ASMVal& src) {
        if (opcode == ASMOpcode::BRZ)
            ::write(io, "  br.z ");
        else if (opcode == ASMOpcode::BRNZ)
            ::write(io, "  br.nz ");
        else
            ::write(io, "  ", ASM_OPCODE_NAMES[(unsigned)opcode], ' ');
        write_mval(io, as, dst);
        ::write(io, ", 0x");
        ::write(io, hex((u64)src.imm64));
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

    static const_slice<i8> trim_conditional_opcode(ASMOpcode opcode) {
        const i8* name = ASM_OPCODE_NAMES[(i32)opcode];
        const i8* end = name + findc(name, 0);
        -- end;
        while (utf8_is_digit(*end))
            -- end;
        while (*end == 'c')
            -- end;
        return { name, (end - name) + 1 };
    }

    static const_slice<i8> trim_conditional_opcode_ext(ASMOpcode opcode) {
        const i8* name = ASM_OPCODE_NAMES[(i32)opcode];
        const i8* end = name + findc(name, 0);
        -- end;
        while (utf8_is_digit(*end))
            -- end;
        return { end + 1, findc(name, 0) - (end - name + 1) };
    }

    static void write_compare(fd io, Assembly& as, ASMOpcode opcode, Condition condition, const ASMVal& dst, const ASMVal& a, const ASMVal& b) {
        ::write(io, "  ");
        ::write(io, trim_conditional_opcode(opcode), trim_conditional_opcode_ext(opcode), '.', ASM_CONDITION_NAMES[condition], ' ');
        write_mval(io, as, dst);
        ::write(io, ", ");
        write_mval(io, as, a);
        ::write(io, ", ");
        write_mval(io, as, b);
        ::write(io, '\n');
    }

    static void write_float_compare(fd io, Assembly& as, ASMOpcode opcode, FloatCondition condition, const ASMVal& dst, const ASMVal& a, const ASMVal& b) {
        ::write(io, "  ");
        ::write(io, trim_conditional_opcode(opcode), trim_conditional_opcode_ext(opcode), '.', ASM_CONDITION_NAMES[condition], ' ');
        write_mval(io, as, dst);
        ::write(io, ", ");
        write_mval(io, as, a);
        ::write(io, ", ");
        write_mval(io, as, b);
        ::write(io, '\n');
    }

    static void write_compare_memory(fd io, Assembly& as, ASMOpcode opcode, Condition condition, const ASMVal& dst, const ASMVal& a, const ASMVal& b, const ASMVal& c) {
        ::write(io, "  ");
        ::write(io, trim_conditional_opcode(opcode), trim_conditional_opcode_ext(opcode), '.', ASM_CONDITION_NAMES[condition], ' ');
        write_mval(io, as, dst);
        ::write(io, ", ");
        write_mval(io, as, a);
        ::write(io, ", ");
        write_mval(io, as, b);
        ::write(io, ", ");
        write_mval(io, as, c);
        ::write(io, '\n');
    }

    static void write_select(fd io, Assembly& as, ASMOpcode opcode, Condition condition, const ASMVal& dst, const ASMVal& a, const ASMVal& b, const ASMVal& c, const ASMVal& d) {
        ::write(io, "  ");
        ::write(io, trim_conditional_opcode(opcode), trim_conditional_opcode_ext(opcode), '.', ASM_CONDITION_NAMES[condition], ' ');
        write_mval(io, as, dst);
        ::write(io, ", ");
        write_mval(io, as, a);
        ::write(io, ", ");
        write_mval(io, as, b);
        ::write(io, ", ");
        write_mval(io, as, c);
        ::write(io, ", ");
        write_mval(io, as, d);
        ::write(io, '\n');
    }

    static void write_float_select(fd io, Assembly& as, ASMOpcode opcode, FloatCondition condition, const ASMVal& dst, const ASMVal& a, const ASMVal& b, const ASMVal& c, const ASMVal& d) {
        ::write(io, "  ");
        ::write(io, trim_conditional_opcode(opcode), trim_conditional_opcode_ext(opcode), '.', ASM_CONDITION_NAMES[condition], ' ');
        write_mval(io, as, dst);
        ::write(io, ", ");
        write_mval(io, as, a);
        ::write(io, ", ");
        write_mval(io, as, b);
        ::write(io, ", ");
        write_mval(io, as, c);
        ::write(io, ", ");
        write_mval(io, as, d);
        ::write(io, '\n');
    }

    static fd output;

    static void write_to(fd output_in) {
        output = output_in;
    }

    #define NULLARY(upper, lower) static void lower(Assembly& as) { write_nullary(output, as, ASMOpcode::upper); }

    #define UNARY(upper, lower) static void lower(Assembly& as, ASMVal dst) { write_unary(output, as, ASMOpcode::upper, dst); }
    #define UNARY_GP(upper, lower) UNARY(upper, lower)
    #define UNARY_FP(upper, lower) UNARY(upper, lower)
    #define UNARY_JUMP(upper, lower) UNARY(upper, lower)
    #define UNARY_GP_IMM(upper, lower) UNARY(upper, lower)
    #define UNARY_FP_F32(upper, lower) UNARY(upper, lower)
    #define UNARY_FP_F64(upper, lower) UNARY(upper, lower)

    #define BINARY(upper, lower) static void lower(Assembly& as, ASMVal dst, ASMVal src) { write_binary(output, as, ASMOpcode::upper, dst, src); }
    #define BINARY_GP(upper, lower) BINARY(upper, lower)
    #define BINARY_FP(upper, lower) BINARY(upper, lower)
    #define BINARY_FP_GP(upper, lower) BINARY(upper, lower)
    #define BINARY_GP_FP(upper, lower) BINARY(upper, lower)
    #define BINARY_GP_IMM(upper, lower) BINARY(upper, lower)
    #define BINARY_FP_F32(upper, lower) BINARY(upper, lower)
    #define BINARY_FP_F64(upper, lower) BINARY(upper, lower)
    #define BINARY_GP_MEM(upper, lower) BINARY(upper, lower)
    #define BINARY_FP_MEM(upper, lower) BINARY(upper, lower)
    #define BINARY_MEM_GP_IMM(upper, lower) BINARY(upper, lower)
    #define BINARY_MEM_FP_F32(upper, lower) BINARY(upper, lower)
    #define BINARY_MEM_FP_F64(upper, lower) BINARY(upper, lower)
    #define BINARY_BRANCH_GP(upper, lower) static void lower(Assembly& as, ASMVal dst, ASMVal src) { write_binary(output, as, ASMOpcode::upper, dst, src); }
    #define BINARY_GP_IMM64(upper, lower) static void lower(Assembly& as, ASMVal dst, ASMVal src) { write_big_constant(output, as, ASMOpcode::upper, dst, src); }

    #define TERNARY(upper, lower) static void lower(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) { write_ternary(output, as, ASMOpcode::upper, dst, a, b); }
    #define TERNARY_GP_IMM(upper, lower) TERNARY(upper, lower)
    #define TERNARY_FP_F32(upper, lower) TERNARY(upper, lower)
    #define TERNARY_FP_F64(upper, lower) TERNARY(upper, lower)
    #define TERNARY_LOAD_INDEX_GP(upper, lower) TERNARY(upper, lower)
    #define TERNARY_LOAD_INDEX_FP(upper, lower) TERNARY(upper, lower)
    #define TERNARY_STORE_INDEX_GP_IMM(upper, lower) TERNARY(upper, lower)
    #define TERNARY_STORE_INDEX_FP_F32(upper, lower) TERNARY(upper, lower)
    #define TERNARY_STORE_INDEX_FP_F64(upper, lower) TERNARY(upper, lower)
    #define TERNARY_MEMORY_OP(upper, lower) TERNARY(upper, lower)

    #define SELECT(upper, lower) static void lower(Assembly& as, Condition cond, ASMVal dst, ASMVal a, ASMVal b, ASMVal c, ASMVal d) { write_select(output, as, ASMOpcode::upper, cond, dst, a, b, c, d); }
    #define SELECT_FLOAT(upper, lower) static void lower(Assembly& as, FloatCondition cond, ASMVal dst, ASMVal a, ASMVal b, ASMVal c, ASMVal d) { write_float_select(output, as, ASMOpcode::upper, cond, dst, a, b, c, d); }
    #define SELECT_GP_IMM(upper, lower) SELECT(upper, lower)
    #define SELECT_FP_F32(upper, lower) SELECT_FLOAT(upper, lower)
    #define SELECT_FP_F64(upper, lower) SELECT_FLOAT(upper, lower)

    #define COMPARE(upper, lower) static void lower(Assembly& as, Condition cond, ASMVal dst, ASMVal a, ASMVal b) { write_compare(output, as, ASMOpcode::upper, cond, dst, a, b); }
    #define COMPARE_FLOAT(upper, lower) static void lower(Assembly& as, FloatCondition cond, ASMVal dst, ASMVal a, ASMVal b) { write_float_compare(output, as, ASMOpcode::upper, cond, dst, a, b); }
    #define COMPARE_MEMORY(upper, lower) static void lower(Assembly& as, Condition cond, ASMVal dst, ASMVal a, ASMVal b, ASMVal c) { write_compare_memory(output, as, ASMOpcode::upper, cond, dst, a, b, c); }
    #define COMPARE_GP_IMM(upper, lower) COMPARE(upper, lower)
    #define COMPARE_FP_F32(upper, lower) COMPARE_FLOAT(upper, lower)
    #define COMPARE_FP_F64(upper, lower) COMPARE_FLOAT(upper, lower)
    #define BRANCH_COMPARE_GP_IMM(upper, lower) COMPARE(upper, lower)
    #define BRANCH_COMPARE_FP_F32(upper, lower) COMPARE_FLOAT(upper, lower)
    #define BRANCH_COMPARE_FP_F64(upper, lower) COMPARE_FLOAT(upper, lower)
    #define BRANCH_COMPARE_MEMORY(upper, lower) COMPARE_MEMORY(upper, lower)

    #define DEFINE_PRINT_IMPL(upper, lower, id, size, kind) kind(upper, lower)
    FOR_EACH_ASM_OPCODE(DEFINE_PRINT_IMPL)
    #undef DEFINE_PRINT_IMPL

    #undef NULLARY
    #undef UNARY
    #undef BINARY
    #undef TERNARY
    #undef COMPARE
    #undef COMPARE_FLOAT
    #undef SELECT
    #undef SELECT_FLOAT

    #undef UNARY_GP
    #undef UNARY_FP
    #undef UNARY_JUMP
    #undef UNARY_GP_IMM
    #undef BINARY_GP
    #undef BINARY_FP
    #undef BINARY_FP_GP
    #undef BINARY_GP_FP
    #undef BINARY_GP_IMM
    #undef BINARY_FP_F32
    #undef BINARY_FP_F64
    #undef BINARY_GP_MEM
    #undef BINARY_FP_MEM
    #undef BINARY_MEM_GP_IMM
    #undef BINARY_MEM_FP_F32
    #undef BINARY_MEM_FP_F64
    #undef BINARY_BRANCH_GP
    #undef BINARY_GP_IMM64
    #undef TERNARY_GP_IMM
    #undef TERNARY_FP_F32
    #undef TERNARY_FP_F64
    #undef TERNARY_LOAD_INDEX_GP
    #undef TERNARY_LOAD_INDEX_FP
    #undef TERNARY_STORE_INDEX_GP_IMM
    #undef TERNARY_STORE_INDEX_FP_F32
    #undef TERNARY_STORE_INDEX_FP_F64
    #undef TERNARY_MEMORY_OP
    #undef SELECT_GP_IMM
    #undef COMPARE_GP_IMM
    #undef COMPARE_FP_F32
    #undef COMPARE_FP_F64
    #undef COMPARE_MEMORY
    #undef BRANCH_COMPARE_GP_IMM
    #undef BRANCH_COMPARE_FP_F32
    #undef BRANCH_COMPARE_FP_F64
    #undef BRANCH_COMPARE_MEMORY

    // Labels

    static void global(Assembly& as, Symbol sym) { write_label(output, as, sym); }
    static void local(Assembly& as, Symbol sym) { write_label(output, as, sym); }
};

template<typename Assembler>
fd Printer<Assembler>::output = 0;

// Combine two targets together.
template<typename A, typename B>
struct Compose {
    using Underlying = B;

    constexpr static mreg fp = B::fp;
    constexpr static mreg sp = B::sp;
    constexpr static EndianOrder endianness = B::endianness;

    #define NULLARY(upper, lower) static void lower(Assembly& as) { A::lower(as); B::lower(as); }

    #define UNARY(upper, lower) static void lower(Assembly& as, ASMVal dst) { A::lower(as, dst); B::lower(as, dst); }
    #define UNARY_GP(upper, lower) UNARY(upper, lower)
    #define UNARY_FP(upper, lower) UNARY(upper, lower)
    #define UNARY_JUMP(upper, lower) UNARY(upper, lower)
    #define UNARY_GP_IMM(upper, lower) UNARY(upper, lower)
    #define UNARY_FP_F32(upper, lower) UNARY(upper, lower)
    #define UNARY_FP_F64(upper, lower) UNARY(upper, lower)

    #define BINARY(upper, lower) static void lower(Assembly& as, ASMVal dst, ASMVal src) { A::lower(as, dst, src); B::lower(as, dst, src); }
    #define BINARY_GP(upper, lower) BINARY(upper, lower)
    #define BINARY_FP(upper, lower) BINARY(upper, lower)
    #define BINARY_FP_GP(upper, lower) BINARY(upper, lower)
    #define BINARY_GP_FP(upper, lower) BINARY(upper, lower)
    #define BINARY_GP_IMM(upper, lower) BINARY(upper, lower)
    #define BINARY_FP_F32(upper, lower) BINARY(upper, lower)
    #define BINARY_FP_F64(upper, lower) BINARY(upper, lower)
    #define BINARY_GP_MEM(upper, lower) BINARY(upper, lower)
    #define BINARY_FP_MEM(upper, lower) BINARY(upper, lower)
    #define BINARY_MEM_GP_IMM(upper, lower) BINARY(upper, lower)
    #define BINARY_MEM_FP_F32(upper, lower) BINARY(upper, lower)
    #define BINARY_MEM_FP_F64(upper, lower) BINARY(upper, lower)
    #define BINARY_BRANCH_GP(upper, lower) static void lower(Assembly& as, ASMVal dst, ASMVal src) { A::lower(as, dst, src); B::lower(as, dst, src); }
    #define BINARY_GP_IMM64(upper, lower) BINARY(upper, lower)

    #define TERNARY(upper, lower) static void lower(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) { A::lower(as, dst, a, b); B::lower(as, dst, a, b); }
    #define TERNARY_GP_IMM(upper, lower) TERNARY(upper, lower)
    #define TERNARY_FP_F32(upper, lower) TERNARY(upper, lower)
    #define TERNARY_FP_F64(upper, lower) TERNARY(upper, lower)
    #define TERNARY_LOAD_INDEX_GP(upper, lower) TERNARY(upper, lower)
    #define TERNARY_LOAD_INDEX_FP(upper, lower) TERNARY(upper, lower)
    #define TERNARY_STORE_INDEX_GP_IMM(upper, lower) TERNARY(upper, lower)
    #define TERNARY_STORE_INDEX_FP_F32(upper, lower) TERNARY(upper, lower)
    #define TERNARY_STORE_INDEX_FP_F64(upper, lower) TERNARY(upper, lower)
    #define TERNARY_MEMORY_OP(upper, lower) TERNARY(upper, lower)

    #define SELECT(upper, lower) static void lower(Assembly& as, Condition cond, ASMVal dst, ASMVal a, ASMVal b, ASMVal c, ASMVal d) { A::lower(as, cond, dst, a, b, c, d); B::lower(as, cond, dst, a, b, c, d); }
    #define SELECT_FLOAT(upper, lower) static void lower(Assembly& as, FloatCondition cond, ASMVal dst, ASMVal a, ASMVal b, ASMVal c, ASMVal d) { A::lower(as, cond, dst, a, b, c, d); B::lower(as, cond, dst, a, b, c, d); }
    #define SELECT_GP_IMM(upper, lower) SELECT(upper, lower)
    #define SELECT_FP_F32(upper, lower) SELECT_FLOAT(upper, lower)
    #define SELECT_FP_F64(upper, lower) SELECT_FLOAT(upper, lower)

    #define COMPARE(upper, lower) static void lower(Assembly& as, Condition cond, ASMVal dst, ASMVal a, ASMVal b) { A::lower(as, cond, dst, a, b); B::lower(as, cond, dst, a, b); }
    #define COMPARE_FLOAT(upper, lower) static void lower(Assembly& as, FloatCondition cond, ASMVal dst, ASMVal a, ASMVal b) { A::lower(as, cond, dst, a, b); B::lower(as, cond, dst, a, b); }
    #define COMPARE_MEMORY(upper, lower) static void lower(Assembly& as, Condition cond, ASMVal dst, ASMVal a, ASMVal b, ASMVal c) { A::lower(as, cond, dst, a, b, c); B::lower(as, cond, dst, a, b, c); }
    #define COMPARE_GP_IMM(upper, lower) COMPARE(upper, lower)
    #define COMPARE_FP_F32(upper, lower) COMPARE_FLOAT(upper, lower)
    #define COMPARE_FP_F64(upper, lower) COMPARE_FLOAT(upper, lower)
    #define BRANCH_COMPARE_GP_IMM(upper, lower) COMPARE(upper, lower)
    #define BRANCH_COMPARE_FP_F32(upper, lower) COMPARE_FLOAT(upper, lower)
    #define BRANCH_COMPARE_FP_F64(upper, lower) COMPARE_FLOAT(upper, lower)
    #define BRANCH_COMPARE_MEMORY(upper, lower) COMPARE_MEMORY(upper, lower)

    #define DEFINE_PROTOTYPE(upper, lower, id, size, kind) kind(upper, lower)
    FOR_EACH_ASM_OPCODE(DEFINE_PROTOTYPE)
    #undef DEFINE_PROTOTYPE

    #undef NULLARY
    #undef UNARY
    #undef BINARY
    #undef TERNARY
    #undef COMPARE
    #undef COMPARE_FLOAT
    #undef SELECT
    #undef SELECT_FLOAT

    #undef UNARY_GP
    #undef UNARY_FP
    #undef UNARY_JUMP
    #undef UNARY_GP_IMM
    #undef BINARY_GP
    #undef BINARY_FP
    #undef BINARY_FP_GP
    #undef BINARY_GP_FP
    #undef BINARY_GP_IMM
    #undef BINARY_FP_F32
    #undef BINARY_FP_F64
    #undef BINARY_GP_MEM
    #undef BINARY_FP_MEM
    #undef BINARY_MEM_GP_IMM
    #undef BINARY_MEM_FP_F32
    #undef BINARY_MEM_FP_F64
    #undef BINARY_BRANCH_GP
    #undef BINARY_GP_IMM64
    #undef TERNARY_GP_IMM
    #undef TERNARY_FP_F32
    #undef TERNARY_FP_F64
    #undef TERNARY_LOAD_INDEX_GP
    #undef TERNARY_LOAD_INDEX_FP
    #undef TERNARY_STORE_INDEX_GP_IMM
    #undef TERNARY_STORE_INDEX_FP_F32
    #undef TERNARY_STORE_INDEX_FP_F64
    #undef TERNARY_MEMORY_OP
    #undef SELECT_GP_IMM
    #undef COMPARE_GP_IMM
    #undef COMPARE_FP_F32
    #undef COMPARE_FP_F64
    #undef COMPARE_MEMORY
    #undef BRANCH_COMPARE_GP_IMM
    #undef BRANCH_COMPARE_FP_F32
    #undef BRANCH_COMPARE_FP_F64
    #undef BRANCH_COMPARE_MEMORY

    // Labels

    static void global(Assembly& as, Symbol sym) { A::global(as, sym); B::global(as, sym); }
    static void local(Assembly& as, Symbol sym) { A::local(as, sym); B::local(as, sym); }

    // Other target methods

    static RegSet caller_saved_gps() { return B::caller_saved_gps(); }
    static RegSet caller_saved_fps() { return B::caller_saved_fps(); }
    static RegSet caller_saves() { return B::caller_saves(); }
    static RegSet callee_saved_gps() { return B::callee_saved_gps(); }
    static RegSet callee_saved_fps() { return B::callee_saved_fps(); }
    static RegSet callee_saves() { return B::callee_saves(); }
    static RegSet gps() { return B::gps(); }
    static RegSet fps() { return B::fps(); }
    static bool is_gp(mreg r) { return B::is_gp(r); }
    static bool is_fp(mreg r) { return B::is_fp(r); }
    static const_slice<i8> reg_name(mreg r) { return B::reg_name(r); }
    static Size word_size() { return B::word_size(); }
    static Size ptr_size() { return B::ptr_size(); }
    static RegSet clobbers(ASMOpcode opcode) { return B::clobbers(opcode); }
    static void* start_placing_parameters() { return B::start_placing_parameters(); }
    static MaybePair<ASMVal> place_scalar_parameter(void* state, Repr repr) { return B::place_scalar_parameter(state, repr); }
    static MaybePair<ASMVal> place_aggregate_parameter(void* state, const_slice<Repr> members) { return B::place_aggregate_parameter(state, members); }
    static void finish_placing_parameters(void* state) { B::finish_placing_parameters(state); }
    static MaybePair<ASMVal> place_scalar_return_value(void* state, Repr repr) { return B::place_scalar_return_value(state, repr); }
    static MaybePair<ASMVal> place_aggregate_return_value(void* state, const_slice<Repr> members) { return B::place_aggregate_return_value(state, members); }
};

// Overridable interface for a Target.
struct TargetInterface {
    #define NULLARY(upper, lower) virtual void lower(Assembly& as) const = 0;

    #define UNARY(upper, lower) virtual void lower(Assembly& as, ASMVal dst) const = 0;
    #define UNARY_GP(upper, lower) UNARY(upper, lower)
    #define UNARY_FP(upper, lower) UNARY(upper, lower)
    #define UNARY_JUMP(upper, lower) UNARY(upper, lower)
    #define UNARY_GP_IMM(upper, lower) UNARY(upper, lower)
    #define UNARY_FP_F32(upper, lower) UNARY(upper, lower)
    #define UNARY_FP_F64(upper, lower) UNARY(upper, lower)

    #define BINARY(upper, lower) virtual void lower(Assembly& as, ASMVal dst, ASMVal src) const = 0;
    #define BINARY_GP(upper, lower) BINARY(upper, lower)
    #define BINARY_FP(upper, lower) BINARY(upper, lower)
    #define BINARY_FP_GP(upper, lower) BINARY(upper, lower)
    #define BINARY_GP_FP(upper, lower) BINARY(upper, lower)
    #define BINARY_GP_IMM(upper, lower) BINARY(upper, lower)
    #define BINARY_FP_F32(upper, lower) BINARY(upper, lower)
    #define BINARY_FP_F64(upper, lower) BINARY(upper, lower)
    #define BINARY_GP_MEM(upper, lower) BINARY(upper, lower)
    #define BINARY_FP_MEM(upper, lower) BINARY(upper, lower)
    #define BINARY_MEM_GP_IMM(upper, lower) BINARY(upper, lower)
    #define BINARY_MEM_FP_F32(upper, lower) BINARY(upper, lower)
    #define BINARY_MEM_FP_F64(upper, lower) BINARY(upper, lower)
    #define BINARY_BRANCH_GP(upper, lower) virtual void lower(Assembly& as, ASMVal dst, ASMVal src) const = 0;
    #define BINARY_GP_IMM64(upper, lower) BINARY(upper, lower)

    #define TERNARY(upper, lower) virtual void lower(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) const = 0;
    #define TERNARY_GP_IMM(upper, lower) TERNARY(upper, lower)
    #define TERNARY_FP_F32(upper, lower) TERNARY(upper, lower)
    #define TERNARY_FP_F64(upper, lower) TERNARY(upper, lower)
    #define TERNARY_LOAD_INDEX_GP(upper, lower) TERNARY(upper, lower)
    #define TERNARY_LOAD_INDEX_FP(upper, lower) TERNARY(upper, lower)
    #define TERNARY_STORE_INDEX_GP_IMM(upper, lower) TERNARY(upper, lower)
    #define TERNARY_STORE_INDEX_FP_F32(upper, lower) TERNARY(upper, lower)
    #define TERNARY_STORE_INDEX_FP_F64(upper, lower) TERNARY(upper, lower)
    #define TERNARY_MEMORY_OP(upper, lower) TERNARY(upper, lower)

    #define SELECT(upper, lower) virtual void lower(Assembly& as, Condition cond, ASMVal dst, ASMVal a, ASMVal b, ASMVal c, ASMVal d) const = 0;
    #define SELECT_FLOAT(upper, lower) virtual void lower(Assembly& as, FloatCondition cond, ASMVal dst, ASMVal a, ASMVal b, ASMVal c, ASMVal d) const = 0;
    #define SELECT_GP_IMM(upper, lower) SELECT(upper, lower)
    #define SELECT_FP_F32(upper, lower) SELECT_FLOAT(upper, lower)
    #define SELECT_FP_F64(upper, lower) SELECT_FLOAT(upper, lower)

    #define COMPARE(upper, lower) virtual void lower(Assembly& as, Condition cond, ASMVal dst, ASMVal a, ASMVal b) const = 0;
    #define COMPARE_FLOAT(upper, lower) virtual void lower(Assembly& as, FloatCondition cond, ASMVal dst, ASMVal a, ASMVal b) const = 0;
    #define COMPARE_MEMORY(upper, lower) virtual void lower(Assembly& as, Condition cond, ASMVal dst, ASMVal a, ASMVal b, ASMVal c) const = 0;
    #define COMPARE_GP_IMM(upper, lower) COMPARE(upper, lower)
    #define COMPARE_FP_F32(upper, lower) COMPARE_FLOAT(upper, lower)
    #define COMPARE_FP_F64(upper, lower) COMPARE_FLOAT(upper, lower)
    #define BRANCH_COMPARE_GP_IMM(upper, lower) COMPARE(upper, lower)
    #define BRANCH_COMPARE_FP_F32(upper, lower) COMPARE_FLOAT(upper, lower)
    #define BRANCH_COMPARE_FP_F64(upper, lower) COMPARE_FLOAT(upper, lower)
    #define BRANCH_COMPARE_MEMORY(upper, lower) COMPARE_MEMORY(upper, lower)

    #define DEFINE_PROTOTYPE(upper, lower, id, size, kind) kind(upper, lower)
    FOR_EACH_ASM_OPCODE(DEFINE_PROTOTYPE)
    #undef DEFINE_PROTOTYPE

    #undef NULLARY
    #undef UNARY
    #undef BINARY
    #undef TERNARY
    #undef COMPARE
    #undef COMPARE_FLOAT
    #undef SELECT
    #undef SELECT_FLOAT

    #undef UNARY_GP
    #undef UNARY_FP
    #undef UNARY_JUMP
    #undef UNARY_GP_IMM
    #undef BINARY_GP
    #undef BINARY_FP
    #undef BINARY_FP_GP
    #undef BINARY_GP_FP
    #undef BINARY_GP_IMM
    #undef BINARY_FP_F32
    #undef BINARY_FP_F64
    #undef BINARY_GP_MEM
    #undef BINARY_FP_MEM
    #undef BINARY_MEM_GP_IMM
    #undef BINARY_MEM_FP_F32
    #undef BINARY_MEM_FP_F64
    #undef BINARY_BRANCH_GP
    #undef BINARY_GP_IMM64
    #undef TERNARY_GP_IMM
    #undef TERNARY_FP_F32
    #undef TERNARY_FP_F64
    #undef TERNARY_LOAD_INDEX_GP
    #undef TERNARY_LOAD_INDEX_FP
    #undef TERNARY_STORE_INDEX_GP_IMM
    #undef TERNARY_STORE_INDEX_FP_F32
    #undef TERNARY_STORE_INDEX_FP_F64
    #undef TERNARY_MEMORY_OP
    #undef SELECT_GP_IMM
    #undef COMPARE_GP_IMM
    #undef COMPARE_FP_F32
    #undef COMPARE_FP_F64
    #undef COMPARE_MEMORY
    #undef BRANCH_COMPARE_GP_IMM
    #undef BRANCH_COMPARE_FP_F32
    #undef BRANCH_COMPARE_FP_F64
    #undef BRANCH_COMPARE_MEMORY

    // Labels

    virtual void global(Assembly& as, Symbol sym) const = 0;
    virtual void local(Assembly& as, Symbol sym) const = 0;

    // Other target methods

    virtual RegSet caller_saved_gps() const = 0;
    virtual RegSet caller_saved_fps() const = 0;
    virtual RegSet caller_saves() const = 0;
    virtual RegSet callee_saved_gps() const = 0;
    virtual RegSet callee_saved_fps() const = 0;
    virtual RegSet callee_saves() const = 0;
    virtual RegSet gps() const = 0;
    virtual RegSet fps() const = 0;
    virtual bool is_gp(mreg r) const = 0;
    virtual bool is_fp(mreg r) const = 0;
    virtual const_slice<i8> reg_name(mreg r) const = 0;
    virtual Size word_size() const = 0;
    virtual Size ptr_size() const = 0;
    virtual RegSet clobbers(ASMOpcode opcode) const = 0;
    virtual void* start_placing_parameters() const = 0;
    virtual MaybePair<ASMVal> place_scalar_parameter(void* state, Repr repr) const = 0;
    virtual MaybePair<ASMVal> place_aggregate_parameter(void* state, const_slice<Repr> members) const = 0;
    virtual void finish_placing_parameters(void* state) const = 0;
    virtual MaybePair<ASMVal> place_scalar_return_value(void* state, Repr repr) const = 0;
    virtual MaybePair<ASMVal> place_aggregate_return_value(void* state, const_slice<Repr> members) const = 0;

    virtual mreg framePtr() const = 0;
    virtual mreg stackPtr() const = 0;
};

template<typename Target>
struct TargetImplementation : public TargetInterface {
    #define NULLARY(upper, lower) virtual void lower(Assembly& as) const override { Target:: lower(as); }

    #define UNARY(upper, lower) virtual void lower(Assembly& as, ASMVal dst) const override { Target:: lower(as, dst); }
    #define UNARY_GP(upper, lower) UNARY(upper, lower)
    #define UNARY_FP(upper, lower) UNARY(upper, lower)
    #define UNARY_JUMP(upper, lower) UNARY(upper, lower)
    #define UNARY_GP_IMM(upper, lower) UNARY(upper, lower)
    #define UNARY_FP_F32(upper, lower) UNARY(upper, lower)
    #define UNARY_FP_F64(upper, lower) UNARY(upper, lower)

    #define BINARY(upper, lower) virtual void lower(Assembly& as, ASMVal dst, ASMVal src) const override { Target:: lower(as, dst, src); }
    #define BINARY_GP(upper, lower) BINARY(upper, lower)
    #define BINARY_FP(upper, lower) BINARY(upper, lower)
    #define BINARY_FP_GP(upper, lower) BINARY(upper, lower)
    #define BINARY_GP_FP(upper, lower) BINARY(upper, lower)
    #define BINARY_GP_IMM(upper, lower) BINARY(upper, lower)
    #define BINARY_FP_F32(upper, lower) BINARY(upper, lower)
    #define BINARY_FP_F64(upper, lower) BINARY(upper, lower)
    #define BINARY_GP_MEM(upper, lower) BINARY(upper, lower)
    #define BINARY_FP_MEM(upper, lower) BINARY(upper, lower)
    #define BINARY_MEM_GP_IMM(upper, lower) BINARY(upper, lower)
    #define BINARY_MEM_FP_F32(upper, lower) BINARY(upper, lower)
    #define BINARY_MEM_FP_F64(upper, lower) BINARY(upper, lower)
    #define BINARY_BRANCH_GP(upper, lower) virtual void lower(Assembly& as, ASMVal dst, ASMVal src) const override { Target:: lower(as, dst, src); }
    #define BINARY_GP_IMM64(upper, lower) BINARY(upper, lower)

    #define TERNARY(upper, lower) virtual void lower(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) const override { Target:: lower(as, dst, a, b); }
    #define TERNARY_GP_IMM(upper, lower) TERNARY(upper, lower)
    #define TERNARY_FP_F32(upper, lower) TERNARY(upper, lower)
    #define TERNARY_FP_F64(upper, lower) TERNARY(upper, lower)
    #define TERNARY_LOAD_INDEX_GP(upper, lower) TERNARY(upper, lower)
    #define TERNARY_LOAD_INDEX_FP(upper, lower) TERNARY(upper, lower)
    #define TERNARY_STORE_INDEX_GP_IMM(upper, lower) TERNARY(upper, lower)
    #define TERNARY_STORE_INDEX_FP_F32(upper, lower) TERNARY(upper, lower)
    #define TERNARY_STORE_INDEX_FP_F64(upper, lower) TERNARY(upper, lower)
    #define TERNARY_MEMORY_OP(upper, lower) TERNARY(upper, lower)

    #define SELECT(upper, lower) virtual void lower(Assembly& as, Condition cond, ASMVal dst, ASMVal a, ASMVal b, ASMVal c, ASMVal d) const override { Target:: lower(as, cond, dst, a, b, c, d); }
    #define SELECT_FLOAT(upper, lower) virtual void lower(Assembly& as, FloatCondition cond, ASMVal dst, ASMVal a, ASMVal b, ASMVal c, ASMVal d) const override { Target:: lower(as, cond, dst, a, b, c, d); }
    #define SELECT_GP_IMM(upper, lower) SELECT(upper, lower)
    #define SELECT_FP_F32(upper, lower) SELECT_FLOAT(upper, lower)
    #define SELECT_FP_F64(upper, lower) SELECT_FLOAT(upper, lower)

    #define COMPARE(upper, lower) virtual void lower(Assembly& as, Condition cond, ASMVal dst, ASMVal a, ASMVal b) const override { Target:: lower(as, cond, dst, a, b); }
    #define COMPARE_FLOAT(upper, lower) virtual void lower(Assembly& as, FloatCondition cond, ASMVal dst, ASMVal a, ASMVal b) const override { Target:: lower(as, cond, dst, a, b); }
    #define COMPARE_MEMORY(upper, lower) virtual void lower(Assembly& as, Condition cond, ASMVal dst, ASMVal a, ASMVal b, ASMVal c) const override { Target:: lower(as, cond, dst, a, b, c); }
    #define COMPARE_GP_IMM(upper, lower) COMPARE(upper, lower)
    #define COMPARE_FP_F32(upper, lower) COMPARE_FLOAT(upper, lower)
    #define COMPARE_FP_F64(upper, lower) COMPARE_FLOAT(upper, lower)
    #define BRANCH_COMPARE_GP_IMM(upper, lower) COMPARE(upper, lower)
    #define BRANCH_COMPARE_FP_F32(upper, lower) COMPARE_FLOAT(upper, lower)
    #define BRANCH_COMPARE_FP_F64(upper, lower) COMPARE_FLOAT(upper, lower)
    #define BRANCH_COMPARE_MEMORY(upper, lower) COMPARE_MEMORY(upper, lower)

    #define DEFINE_METHOD(upper, lower, id, size, kind) kind(upper, lower)
    FOR_EACH_ASM_OPCODE(DEFINE_METHOD)
    #undef DEFINE_METHOD

    #undef NULLARY
    #undef UNARY
    #undef BINARY
    #undef TERNARY
    #undef COMPARE
    #undef COMPARE_FLOAT
    #undef SELECT
    #undef SELECT_FLOAT

    #undef UNARY_GP
    #undef UNARY_FP
    #undef UNARY_JUMP
    #undef UNARY_GP_IMM
    #undef BINARY_GP
    #undef BINARY_FP
    #undef BINARY_FP_GP
    #undef BINARY_GP_FP
    #undef BINARY_GP_IMM
    #undef BINARY_FP_F32
    #undef BINARY_FP_F64
    #undef BINARY_GP_MEM
    #undef BINARY_FP_MEM
    #undef BINARY_MEM_GP_IMM
    #undef BINARY_MEM_FP_F32
    #undef BINARY_MEM_FP_F64
    #undef BINARY_BRANCH_GP
    #undef BINARY_GP_IMM64
    #undef TERNARY_GP_IMM
    #undef TERNARY_FP_F32
    #undef TERNARY_FP_F64
    #undef TERNARY_LOAD_INDEX_GP
    #undef TERNARY_LOAD_INDEX_FP
    #undef TERNARY_STORE_INDEX_GP_IMM
    #undef TERNARY_STORE_INDEX_FP_F32
    #undef TERNARY_STORE_INDEX_FP_F64
    #undef TERNARY_MEMORY_OP
    #undef SELECT_GP_IMM
    #undef COMPARE_GP_IMM
    #undef COMPARE_FP_F32
    #undef COMPARE_FP_F64
    #undef COMPARE_MEMORY
    #undef BRANCH_COMPARE_GP_IMM
    #undef BRANCH_COMPARE_FP_F32
    #undef BRANCH_COMPARE_FP_F64
    #undef BRANCH_COMPARE_MEMORY

    // Labels

    virtual void global(Assembly& as, Symbol sym) const override { Target::global(as, sym); }
    virtual void local(Assembly& as, Symbol sym) const override { Target::local(as, sym); }

    // Other target methods

    virtual RegSet caller_saved_gps() const override { return Target::caller_saved_gps(); };
    virtual RegSet caller_saved_fps() const override { return Target::caller_saved_fps(); };
    virtual RegSet caller_saves() const override { return Target::caller_saves(); };
    virtual RegSet callee_saved_gps() const override { return Target::callee_saved_gps(); };
    virtual RegSet callee_saved_fps() const override { return Target::callee_saved_fps(); };
    virtual RegSet callee_saves() const override { return Target::callee_saves(); };
    virtual RegSet gps() const override { return Target::gps(); }
    virtual RegSet fps() const override { return Target::fps(); }
    virtual bool is_gp(mreg r) const override { return Target::is_gp(r); }
    virtual bool is_fp(mreg r) const override { return Target::is_fp(r); }
    virtual const_slice<i8> reg_name(mreg r) const override { return Target::reg_name(r); }
    virtual Size word_size() const override { return Target::word_size(); }
    virtual Size ptr_size() const override { return Target::ptr_size(); }
    virtual RegSet clobbers(ASMOpcode opcode) const override { return Target::clobbers(opcode); }
    virtual void* start_placing_parameters() const override { return Target::start_placing_parameters(); };
    virtual MaybePair<ASMVal> place_scalar_parameter(void* state, Repr repr) const override { return Target::place_scalar_parameter(state, repr); };
    virtual MaybePair<ASMVal> place_aggregate_parameter(void* state, const_slice<Repr> members) const override { return Target::place_aggregate_parameter(state, members); };
    virtual void finish_placing_parameters(void* state) const override { Target::finish_placing_parameters(state); };
    virtual MaybePair<ASMVal> place_scalar_return_value(void* state, Repr repr) const override { return Target::place_scalar_return_value(state, repr); }
    virtual MaybePair<ASMVal> place_aggregate_return_value(void* state, const_slice<Repr> members) const override { return Target::place_aggregate_return_value(state, members); }

    virtual mreg framePtr() const override { return Target::fp; }
    virtual mreg stackPtr() const override { return Target::sp; }
};

#endif