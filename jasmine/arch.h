#ifndef BASIL_JASMINE_TARGET_H
#define BASIL_JASMINE_TARGET_H

#include "lib/buffer.h"
#include "lib/io.h"
#include "lib/tuple.h"
#include "jasmine/type.h"

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

using mreg = u32;   // Generic type of machine register.

inline void write(stream& io, const TargetDesc& target) {
    ::write(io, OS_NAMES[target.os], '_', ARCH_NAMES[target.arch]);
}

struct Function;
struct TypeTable;

enum SlotType {
    SLOT_NONE, SLOT_STACK, SLOT_GPREG, SLOT_FPREG, SLOT_GPREG_PAIR, SLOT_FPREG_PAIR, SLOT_GPFPREG_PAIR, SLOT_FPGPREG_PAIR
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

inline Slot gpreg_pair(mreg a, mreg b) {
    Slot s;
    s.type = SLOT_GPREG_PAIR;
    s.regpair[0] = a;
    s.regpair[1] = b;
    return s;
}

inline Slot fpreg_pair(mreg a, mreg b) {
    Slot s;
    s.type = SLOT_FPREG_PAIR;
    s.regpair[0] = a;
    s.regpair[1] = b;
    return s;
}

inline Slot gpfpreg_pair(mreg a, mreg b) {
    Slot s;
    s.type = SLOT_GPFPREG_PAIR;
    s.regpair[0] = a;
    s.regpair[1] = b;
    return s;
}

inline Slot fpgpreg_pair(mreg a, mreg b) {
    Slot s;
    s.type = SLOT_FPGPREG_PAIR;
    s.regpair[0] = a;
    s.regpair[1] = b;
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

// General interface for an OS + arch target.
struct Target {
    virtual const TargetDesc& desc() const = 0;     // Description of target.
    virtual const_slice<u32> gpregs() const = 0;    // List of general-purpose registers, in desired allocation order.
    virtual const_slice<u32> fpregs() const = 0;    // List of floating-point registers, in desired allocation order.
    virtual const_slice<i8> gpreg_name(mreg r) const = 0; // Register name getters.
    virtual const_slice<i8> fpreg_name(mreg r) const = 0;

    // Placement functions for allocating parameters and the return value within the target calling convention.
    virtual Placement place_ret(const TypeTable& tab, typeidx return_type) const = 0;
    virtual Placement place_arg(const TypeTable& tab, typeidx arg_type, UsageState state) const = 0;

    // Assemble a function to machine code.
    virtual void lower(const Function& fn, bytebuf<arena>& buf) = 0;

    // Only defined for primitive types.
    virtual u32 primsize(typeidx t) const = 0;
    virtual u32 primalign(typeidx t) const = 0;
};

template<typename T>
struct TargetProps : public Target {
    static const TargetDesc DESC;

    inline const TargetDesc& desc() const override { return DESC; } 

    inline const_slice<u32> gpregs() const override { return T::gpregs(); }
    inline const_slice<u32> fpregs() const override { return T::fpregs(); }
    inline const_slice<i8> gpreg_name(mreg r) const override { return T::gpreg_name(r); };
    inline const_slice<i8> fpreg_name(mreg r) const override { return T::fpreg_name(r); };
    
    inline Placement place_ret(const TypeTable& tab, typeidx return_type) const override { return T::place_ret(tab, return_type); }
    inline Placement place_arg(const TypeTable& tab, typeidx arg_type, UsageState state) const override { return T::place_arg(tab, arg_type, state); }

    inline u32 primsize(typeidx t) const override { return T::primsize(t); }
    inline u32 primalign(typeidx t) const override { return T::primalign(t); }

    inline void lower(const Function& fn, bytebuf<arena>& buf) override {
        return T::lower(fn, buf);
    }

    static const TargetProps INSTANCE_VALUE;
    static const Target& INSTANCE;
};

template<typename T>
const TargetProps<T> TargetProps<T>::INSTANCE_VALUE;

template<typename T>
const Target& TargetProps<T>::INSTANCE = TargetProps<T>::INSTANCE_VALUE;

template<typename T>
const TargetDesc TargetProps<T>::DESC = T::DESC;

#if defined(LIBCORE_LINUX) && defined(LIBCORE_AMD64)
#include "jasmine/arch/amd64.h"
extern const Target& DEFAULT_TARGET;
#endif

#endif