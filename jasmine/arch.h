#ifndef BASIL_JASMINE_TARGET_H
#define BASIL_JASMINE_TARGET_H

#include "lib/buffer.h"
#include "lib/io.h"
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
    Arch arch;
    OS os;
    inline TargetDesc(Arch arch_in, OS os_in): arch(arch_in), os(os_in) {}

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

struct Target {
    virtual const_slice<u32> gpregs() const = 0;
    virtual const_slice<u32> fpregs() const = 0;
    virtual const_slice<u32> gpargs() const = 0;
    virtual const_slice<u32> fpargs() const = 0;
    virtual const_slice<i8> gpreg_name(mreg r) const = 0;
    virtual const_slice<i8> fpreg_name(mreg r) const = 0;
    virtual void lower(const Function& fn, bytebuf<arena>& buf) = 0;

    // Only defined for primitive types.
    virtual u32 primsize(typeidx t) const = 0;
    virtual u32 primalign(typeidx t) const = 0;
};

template<typename T>
struct TargetProps : public Target {
    inline virtual const_slice<u32> gpregs() const { return T::gpregs(); }
    inline virtual const_slice<u32> fpregs() const { return T::fpregs(); }
    inline virtual const_slice<u32> gpargs() const { return T::gpargs(); }
    inline virtual const_slice<u32> fpargs() const { return T::fpargs(); }
    inline virtual const_slice<i8> gpreg_name(mreg r) const { return T::gpreg_name(r); };
    inline virtual const_slice<i8> fpreg_name(mreg r) const { return T::fpreg_name(r); };
    inline virtual u32 primsize(typeidx t) const { return T::primsize(t); }
    inline virtual u32 primalign(typeidx t) const { return T::primalign(t); }

    inline virtual void lower(const Function& fn, bytebuf<arena>& buf) {
        return T::lower(fn, buf);
    }

    static const TargetProps INSTANCE_VALUE;
    static const Target& INSTANCE;
};

template<typename T>
const TargetProps<T> TargetProps<T>::INSTANCE_VALUE;

template<typename T>
const Target& TargetProps<T>::INSTANCE = TargetProps<T>::INSTANCE_VALUE;

#if defined(LIBCORE_LINUX) && defined(LIBCORE_AMD64)
#include "jasmine/arch/amd64.h"
extern const Target& DEFAULT_TARGET;
#endif

#endif