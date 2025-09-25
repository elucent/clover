#ifndef ASM_ARCH_AMD64_H
#define ASM_ARCH_AMD64_H

#include "asm/arch.h"

#if INCLUDE_ARCH_AMD64

struct AMD64Assembler {
    static constexpr mreg
        RAX = 0, RCX = 1, RDX = 2, RBX = 3, RSP = 4, RBP = 5, RSI = 6, RDI = 7,
        R8 = 8, R9 = 9, R10 = 10, R11 = 11, R12 = 12, R13 = 13, R14 = 14, R15 = 15,
        XMM0 = 32, XMM1 = 33, XMM2 = 34, XMM3 = 35, XMM4 = 36, XMM5 = 37, XMM6 = 38, XMM7 = 39,
        XMM8 = 40, XMM9 = 41, XMM10 = 42, XMM11 = 43, XMM12 = 44, XMM13 = 45, XMM14 = 46, XMM15 = 47;
    static constexpr mreg GP_REGS[14] = { RAX, RCX, RDX, RSI, RDI, RBX, R8, R9, R10, R11, R12, R13, R14, R15 };
    static constexpr mreg FP_REGS[16] = { XMM0, XMM1, XMM2, XMM3, XMM4, XMM5, XMM6, XMM7, XMM8, XMM9, XMM10, XMM11, XMM12, XMM13, XMM14, XMM15 };
    static constexpr const i8* GP_REG_NAMES[16] = {
        "rax", "rcx", "rdx", "rbx", "rsp", "rbp", "rsi", "rdi",
        "r8", "r9", "r10", "r11", "r12", "r13", "r14", "r15"
    };
    static constexpr const i8* FP_REG_NAMES[16] = {
        "xmm0", "xmm1", "xmm2", "xmm3", "xmm4", "xmm5", "xmm6", "xmm7",
        "xmm8", "xmm9", "xmm10", "xmm11", "xmm12", "xmm13", "xmm14", "xmm15"
    };

    constexpr static mreg fp = RBP, sp = RSP;
    constexpr static EndianOrder endianness = EndianOrder::LITTLE;

    static constexpr inline RegSet gps() {
        return RegSet(GP_REGS, 14);
    }

    static constexpr inline RegSet fps() {
        return RegSet(FP_REGS, 16);
    }

    static constexpr inline bool is_gp(mreg r) {
        return r < 16;
    }

    static constexpr inline bool is_fp(mreg r) {
        return r >= 32 && r < 48;
    }

    static constexpr inline const_slice<i8> reg_name(mreg r) {
        if (r < XMM0) return { GP_REG_NAMES[i32(r)], findc(GP_REG_NAMES[i32(r)], 0) };
        else return { FP_REG_NAMES[i32(r - XMM0)], findc(FP_REG_NAMES[i32(r - XMM0)], 0) };
    }

    static constexpr inline Size word_size() {
        return Size::BITS64;
    }

    static constexpr inline Size ptr_size() {
        return Size::BITS64;
    }

    static inline constexpr RegSet clobbers(ASMOpcode opcode) {
        switch (opcode) {
            case ASMOpcode::SDIV8:
            case ASMOpcode::UDIV8:
            case ASMOpcode::SREM8:
            case ASMOpcode::UREM8:
                return RegSet(RAX, RDX);
            case ASMOpcode::SDIV16:
            case ASMOpcode::SDIV32:
            case ASMOpcode::SDIV64:
            case ASMOpcode::UDIV16:
            case ASMOpcode::UDIV32:
            case ASMOpcode::UDIV64:
            case ASMOpcode::SREM16:
            case ASMOpcode::SREM32:
            case ASMOpcode::SREM64:
            case ASMOpcode::UREM16:
            case ASMOpcode::UREM32:
            case ASMOpcode::UREM64:
                return RegSet(RAX, RDX, RCX);
            case ASMOpcode::FPUSH32:
            case ASMOpcode::FPUSH64:
            case ASMOpcode::FPOP32:
            case ASMOpcode::FPOP64:
            case ASMOpcode::STI8:
            case ASMOpcode::STI16:
            case ASMOpcode::STI32:
            case ASMOpcode::STI64:
            case ASMOpcode::I8TOF32:
            case ASMOpcode::I16TOF32:
            case ASMOpcode::I8TOF64:
            case ASMOpcode::I16TOF64:
                return RegSet(RAX);
            case ASMOpcode::SHL8:
            case ASMOpcode::SHL16:
            case ASMOpcode::SHL32:
            case ASMOpcode::SHL64:
            case ASMOpcode::SHR8:
            case ASMOpcode::SHR16:
            case ASMOpcode::SHR32:
            case ASMOpcode::SHR64:
            case ASMOpcode::SAR8:
            case ASMOpcode::SAR16:
            case ASMOpcode::SAR32:
            case ASMOpcode::SAR64:
            case ASMOpcode::ROL8:
            case ASMOpcode::ROL16:
            case ASMOpcode::ROL32:
            case ASMOpcode::ROL64:
            case ASMOpcode::ROR8:
            case ASMOpcode::ROR16:
            case ASMOpcode::ROR32:
            case ASMOpcode::ROR64:
                return RegSet(RCX);
            case ASMOpcode::FDIV32:
            case ASMOpcode::FDIV64:
            case ASMOpcode::FREM32:
            case ASMOpcode::FREM64:
            case ASMOpcode::FABS32:
            case ASMOpcode::FABS64:
            case ASMOpcode::FST32:
            case ASMOpcode::FST64:
            case ASMOpcode::FSTI32:
            case ASMOpcode::FSTI64:
                return RegSet(XMM0);
            default:
                return RegSet();
        }
    }

    enum AMD64Size {
        BYTE, WORD, DWORD, QWORD, SINGLE, DOUBLE
    };

    static inline bool needsREX(AMD64Size size, ASMVal v) {
        if (v.kind == ASMVal::MEM) return v.base >= R8;
        else if (v.kind == ASMVal::FP) return v.fp >= XMM8;
        else if (v.kind == ASMVal::GP) return v.gp >= R8;
        else return false;
    }

    static inline bool needs8BitREX(AMD64Size size, ASMVal v) {
        return size == BYTE && v.gp >= RSP;
    }

    static inline void nullary_prefix(Assembly& as, AMD64Size size, ASMVal reg, ASMVal rm) {
        if (size == WORD) as.code.write<u8>(0x66);
        else if (size == QWORD) as.code.write<u8>(0x48);
    }

    static inline void unary_prefix(Assembly& as, AMD64Size size, ASMVal rm) {
        if (size == WORD) as.code.write<u8>(0x66);
        u8 b = size == QWORD ? 0x48 : 0x40;
        if (needsREX(size, rm)) b |= 0x01;
        if (b != 0x40 || needs8BitREX(size, rm)) as.code.write<u8>(b);
    }

    static inline void binary_prefix(Assembly& as, AMD64Size size, ASMVal reg, ASMVal rm) {
        if (size == WORD) as.code.write<u8>(0x66);
        u8 b = size == QWORD ? 0x48 : 0x40;
        if (rm.kind == ASMVal::IMM && needsREX(size, reg))
            b |= 0x01;
        else {
            if (needsREX(size, reg)) b |= 0x04;
            if (needsREX(size, rm)) b |= 0x01;
        }
        if (b != 0x40 || needs8BitREX(size, reg) || needs8BitREX(size, rm)) as.code.write<u8>(b);
    }

    static inline void index_prefix(Assembly& as, AMD64Size size, mreg reg, mreg base, mreg index, AMD64Size scale, i64 offset) {
        if (size == WORD) as.code.write<u8>(0x66);
        u8 b = size == QWORD ? 0x48 : 0x40;
        if (reg >= R8) b |= 0x04;
        if (index >= R8) b |= 0x02;
        if (base >= R8) b |= 0x01;
        if (b != 0x40 || (size == BYTE && reg >= RSP)) as.code.write<u8>(b);
    }

    static inline void index_args(Assembly& as, mreg reg, mreg base, mreg index, AMD64Size scale, i64 offset) {
        u8 modb = 0b00, rmb = RSP & 0b111, regb = reg & 0b111;
        u8 scaleb = (u8)scale, baseb = base & 0b111, indexb = index & 0b111;
        if (offset == 0 && (base & 0b111) != RBP) // Zero displacement.
            modb = 0b00;
        else if (offset < 128 && offset > -129) // Only need 8-bit displacement.
            modb = 0b01;
        else modb = 0b10; // 32-bit displacement.
        as.code.write<u8>(modb << 6 | regb << 3 | rmb);
        as.code.write<u8>(scaleb << 6 | indexb << 3 | baseb);

        if (modb == 0b01) as.code.write<i8>(offset);
        else if (modb) as.code.writeLE<i32>(offset);
    }

    static inline void modrm(Assembly& as, ASMVal reg, ASMVal rm) {
        u8 modb = 0b00, rmb = rm.gp & 0b111, regb = 0b000;
        u8 sib = 0;
        if (reg.kind == ASMVal::GP)
            regb = reg.gp & 0b111;
        if (reg.kind == ASMVal::GP && rm.kind == ASMVal::GP)
            modb = 0b11;
        else if (rm.kind == ASMVal::MEM) {
            auto mkind = rm.memkind;
            if (mkind == ASMVal::REG_OFFSET) {
                auto offset = rm.offset;
                rmb = rm.base & 0b111;
                if (rmb == RSP) sib = RSP << 3 | RSP; // Need SIB for all RSP-relative addressing.
                if (offset == 0 && (rm.base & 0b111) != RBP) // Zero displacement.
                    modb = 0b00;
                else if (offset < 128 && offset > -129) // Only need 8-bit displacement.
                    modb = 0b01;
                else modb = 0b10; // 32-bit displacement.
            }
            else { // Some kind of label.
                modb = 0b00;
                rmb = RBP & 0b111; // RIP-relative displacement encoded using RBP as base.
            }
        }
        else rmb = 0b000;
        as.code.write<u8>(modb << 6 | regb << 3 | rmb);
        if (sib) as.code.write<u8>(sib);

        if (rm.kind == ASMVal::MEM) {
            auto mkind = rm.memkind;
            switch (mkind) {
                case ASMVal::REG_OFFSET:
                    if (modb == 0b00) {}
                    else if (modb == 0b01) as.code.write<i8>(rm.offset);
                    else if (modb == 0b10) as.code.writeLE<i32>(rm.offset);
                    break;
                case ASMVal::LOCAL_LABEL:
                    as.code.writeLE<i32>(0);
                    as.ref(CODE_SECTION, DEF_LOCAL, Reloc::REL32_LE, rm.sym);
                    break;
                case ASMVal::FUNC_LABEL:
                case ASMVal::DATA_LABEL:
                case ASMVal::STATIC_LABEL:
                    as.code.writeLE<i32>(0);
                    as.ref(CODE_SECTION, DEF_GLOBAL, Reloc::REL32_LE, rm.sym);
                    break;
            }
        }
    }

    struct Opcode {
        u8 base, prefix, prefix2;
        i8 ext;
        bool lit;

        static inline Opcode from(u8 prefix, u8 prefix2, u8 base) {
            return { base, prefix, prefix2, i8(-1), false };
        }

        static inline Opcode from(u8 prefix, u8 base) {
            return { base, prefix, 0, i8(-1), false };
        }

        static inline Opcode from(u8 base) {
            return { base, 0, 0, i8(-1), false };
        }

        static inline Opcode literal(u8 prefix, u8 prefix2, u8 base) {
            return { base, prefix, prefix2, i8(-1), true };
        }

        static inline Opcode literal(u8 opcode) {
            return { opcode, 0, 0, i8(-1), true };
        }

        static inline Opcode literal(u8 prefix, u8 opcode) {
            return { opcode, prefix, 0, i8(-1), true };
        }

        static inline Opcode withExt(u8 base, i8 ext) {
            assert(ext >= 0);
            return { base, 0, 0, ext, false };
        }

        static inline Opcode withExt(u8 prefix, u8 base, i8 ext) {
            assert(ext >= 0);
            return { base, prefix, 0, ext, false };
        }

        static inline Opcode litExt(u8 opcode, i8 ext) {
            return { opcode, 0, 0, ext, true };
        }

        static inline Opcode litExt(u8 prefix, u8 opcode, i8 ext) {
            return { opcode, prefix, 0, ext, true };
        }
    };

    static inline void unary_args(Assembly& as, AMD64Size size, Opcode opcode, ASMVal rm) {
        if (opcode.ext < 0) modrm(as, GP(0), rm);
        else modrm(as, GP(opcode.ext), rm);
        if (rm.kind == ASMVal::IMM) {
            if (rm.imm < 128 && rm.imm > -129) as.code.write<i8>(rm.imm);
            else as.code.writeLE<i32>(rm.imm);
        }
    }

    static inline void unaryop(Assembly& as, AMD64Size size, Opcode opcode, ASMVal dst) {
        if (size != BYTE && !opcode.lit)
            opcode.base ++;

        // At this point, dst is our reg field, and src is our rm.
        unary_prefix(as, size, dst);
        if (opcode.prefix) as.code.write<u8>(opcode.prefix);
        if (opcode.prefix2) as.code.write<u8>(opcode.prefix2);
        as.code.write<u8>(opcode.base);
        unary_args(as, size, opcode, dst);
    }

    static inline void binary_args(Assembly& as, AMD64Size size, Opcode opcode, ASMVal reg, ASMVal rm) {
        if (opcode.ext < 0) modrm(as, reg, rm);
        else modrm(as, GP(opcode.ext), reg);
        if (rm.kind == ASMVal::IMM) {
            if ((rm.imm < 128 && rm.imm > -129) || size == BYTE) as.code.write<i8>(rm.imm);
            else if (size == AMD64Size::WORD) as.code.writeLE<i16>(rm.imm);
            else as.code.writeLE<i32>(rm.imm);
        }
    }

    static inline bool is_immediate8(i64 imm) {
        return imm < 128 && imm >= -129;
    }

    static inline void binaryop(Assembly& as, AMD64Size size, Opcode opcode, ASMVal dst, ASMVal src) {
        if (size != BYTE && !opcode.lit)
            opcode.base ++;
        if (((src.kind == ASMVal::MEM
            || (src.kind == ASMVal::IMM && is_immediate8(src.imm) && size != BYTE)) && !opcode.lit))
            opcode.base += 2;

        ASMVal reg = src, rm = dst;
        if (src.kind == ASMVal::MEM || src.kind == ASMVal::IMM)
            swap(reg, rm);
        assert(dst.kind != ASMVal::IMM);

        // At this point, dst is our reg field, and src is our rm.
        binary_prefix(as, size, reg, rm);
        if (opcode.prefix) as.code.write<u8>(opcode.prefix);
        if (opcode.prefix2) as.code.write<u8>(opcode.prefix2);
        as.code.write<u8>(opcode.base);
        binary_args(as, size, opcode, reg, rm);
    }

    static inline bool is_ext(ASMVal val) {
        return (val.kind == ASMVal::FP && val.fp >= XMM8)
            || (val.kind == ASMVal::GP && val.gp >= R8);
    }

    enum VexOpcode {
        TwoByteOpcode = 1,
        ThreeByteOpcode38 = 2,
        ThreeByteOpcode3A = 3
    };

    enum VexPrefix {
        NoPrefix = 0,
        VexPrefix66 = 1,
        VexPrefixF3 = 2,
        VexPrefixF2 = 3
    };

    static inline void vexbinaryprefix(Assembly& as, VexPrefix prefix, bool rexX, bool wide, ASMVal dst, ASMVal& lhs, ASMVal& rhs, bool commutative, VexOpcode opclass) {
        bool needs_three_byte = is_ext(rhs) || opclass != TwoByteOpcode || wide || rexX;
        if (needs_three_byte && !is_ext(lhs) && commutative) {
            swap(lhs, rhs);
            needs_three_byte = opclass != TwoByteOpcode || wide || rexX;
        }

        if (needs_three_byte) { // Needs three-byte VEX prefix
            as.code.write<u8>(0xc4);
            u8 vex1 = 0, vex2 = 0;
            vex1 |= opclass;
            if (!is_ext(dst)) vex1 |= 0b10000000; // ~R bit
            if (!rexX) vex1 |= 0b01000000; // ~X bit
            if (!is_ext(rhs)) vex1 |= 0b00100000; // ~B bit
            vex2 |= prefix;
            vex2 |= (~(lhs.fp - XMM0) & 0b1111) << 3; // Extra operand.
            if (wide) vex2 |= 0b10000000; // Wide
            as.code.write<u8>(vex1);
            as.code.write<u8>(vex2);
        }
        else { // Two-byte VEX prefix
            as.code.write<u8>(0xc5);
            u8 vex = 0;
            if (!is_ext(dst)) vex |= 0b10000000; // ~R bit
            vex |= prefix;
            vex |= (~(lhs.fp - XMM0) & 0b1111) << 3; // Extra operand.
            as.code.write<u8>(vex);
        }
    }

    static inline void vexunaryprefix(Assembly& as, VexPrefix prefix, bool rexX, bool wide, ASMVal dst, ASMVal src, VexOpcode opclass) {
        bool needs_three_byte = is_ext(src) || opclass != TwoByteOpcode || wide || rexX;

        if (needs_three_byte) { // Needs three-byte VEX prefix
            as.code.write<u8>(0xc4);
            u8 vex1 = 0, vex2 = 0;
            vex1 |= opclass;
            if (!is_ext(dst)) vex1 |= 0b10000000; // ~R bit
            if (!rexX) vex1 |= 0b01000000; // ~X bit
            if (!is_ext(src)) vex1 |= 0b00100000; // ~B bit
            vex2 |= prefix;
            vex2 |= 0b1111 << 3; // Encode no prefix operand.
            if (wide) vex2 |= 0b10000000; // Wide
            as.code.write<u8>(vex1);
            as.code.write<u8>(vex2);
        }
        else { // Two-byte VEX prefix
            as.code.write<u8>(0xc5);
            u8 vex = 0;
            if (!is_ext(dst)) vex |= 0b10000000; // ~R bit
            vex |= prefix;
            vex |= 0b1111 << 3; // Encode no prefix operand.
            as.code.write<u8>(vex);
        }
    }

    static inline void vexbinaryop(Assembly& as, VexPrefix prefix, bool wide, Opcode opcode, ASMVal dst, ASMVal lhs, ASMVal rhs, bool commutative, VexOpcode opclass) {
        if (lhs.kind == ASMVal::F32)
            lhs = emitF32Constant(as, lhs);
        if (lhs.kind == ASMVal::F64)
            lhs = emitF64Constant(as, lhs);
        if (rhs.kind == ASMVal::F32)
            rhs = emitF32Constant(as, rhs);
        if (rhs.kind == ASMVal::F64)
            rhs = emitF64Constant(as, rhs);
        if (lhs.kind == ASMVal::MEM && commutative)
            swap(lhs, rhs);
        assert(lhs.kind != ASMVal::MEM);

        vexbinaryprefix(as, prefix, false, wide, dst, lhs, rhs, commutative, opclass);
        as.code.write<u8>(opcode.base);
        ASMVal reg = dst, rm = rhs;
        assert(reg.kind != ASMVal::MEM);
        if (reg.kind == ASMVal::FP)
            reg = GP(reg.fp - XMM0);
        if (rm.kind == ASMVal::FP)
            rm = GP(rm.gp - XMM0);
        modrm(as, reg, rm);
    }

    static inline void vexunaryop(Assembly& as, VexPrefix prefix, bool wide, Opcode opcode, ASMVal dst, ASMVal src, VexOpcode opclass) {
        if (src.kind == ASMVal::F32) {
            fmov32(as, dst, src);
            src = dst;
        }
        if (src.kind == ASMVal::F64) {
            fmov64(as, dst, src);
            src = dst;
        }

        vexunaryprefix(as, prefix, false, wide, dst, src, opclass);
        as.code.write<u8>(opcode.base);
        ASMVal reg = dst, rm = src;
        if (reg.kind == ASMVal::MEM) swap(reg, rm);
        if (reg.kind == ASMVal::FP)
            reg = GP(reg.fp - XMM0);
        if (rm.kind == ASMVal::FP)
            rm = GP(rm.gp - XMM0);
        modrm(as, reg, rm);
    }

    // TODO: Make adds and subtracts into leas

    static inline void add8(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) {
        if (dst == b) swap(a, b);
        if (a.kind == ASMVal::IMM) swap(a, b);
        Opcode op = b.kind == ASMVal::IMM ? Opcode::withExt(0x80, 0x00) : Opcode::from(0x00);
        mov8(as, dst, a);
        if (b.kind == ASMVal::IMM) {
            if (b.imm == 1)
                return unaryop(as, BYTE, Opcode::litExt(0xfe, 0x00), dst); // inc
            if (b.imm == -1)
                return unaryop(as, BYTE, Opcode::litExt(0xfe, 0x01), dst); // dec
        }
        binaryop(as, BYTE, op, dst, b);
    }

    static inline void add16(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) {
        if (dst == b) swap(a, b);
        if (a.kind == ASMVal::IMM) swap(a, b);
        Opcode op = b.kind == ASMVal::IMM ? Opcode::withExt(0x80, 0x00) : Opcode::from(0x00);
        mov16(as, dst, a);
        if (b.kind == ASMVal::IMM) {
            if (b.imm == 1)
                return unaryop(as, WORD, Opcode::litExt(0xff, 0x00), dst); // inc
            if (b.imm == -1)
                return unaryop(as, WORD, Opcode::litExt(0xff, 0x01), dst); // dec
        }
        binaryop(as, WORD, op, dst, b);
    }

    static inline void add32(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) {
        if (dst == b) swap(a, b);
        if (a.kind == ASMVal::IMM) swap(a, b);
        Opcode op = b.kind == ASMVal::IMM ? Opcode::withExt(0x80, 0x00) : Opcode::from(0x00);
        mov32(as, dst, a);
        if (b.kind == ASMVal::IMM) {
            if (b.imm == 1)
                return unaryop(as, DWORD, Opcode::litExt(0xff, 0x00), dst); // inc
            if (b.imm == -1)
                return unaryop(as, DWORD, Opcode::litExt(0xff, 0x01), dst); // dec
        }
        binaryop(as, DWORD, op, dst, b);
    }

    static inline void add64(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) {
        if (dst == b) swap(a, b);

        if (a != dst) {
            if (a.kind == ASMVal::IMM && b.kind == ASMVal::GP)
                return la(as, dst, Mem(b.gp, a.imm));
            if (b.kind == ASMVal::IMM && a.kind == ASMVal::GP)
                return la(as, dst, Mem(a.gp, b.imm));
            if (a.kind == ASMVal::GP && b.kind == ASMVal::GP)
                return lai8(as, dst, Mem(a.gp, 0), b);
        }

        if (a.kind == ASMVal::IMM) swap(a, b);
        Opcode op = b.kind == ASMVal::IMM ? Opcode::withExt(0x80, 0x00) : Opcode::from(0x00);
        mov64(as, dst, a);
        if (b.kind == ASMVal::IMM) {
            if (b.imm == 1)
                return unaryop(as, QWORD, Opcode::litExt(0xff, 0x00), dst); // inc
            if (b.imm == -1)
                return unaryop(as, QWORD, Opcode::litExt(0xff, 0x01), dst); // dec
        }
        binaryop(as, QWORD, op, dst, b);
    }

    static inline void sub8(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) {
        if (a == b)
            return xor8(as, dst, dst, dst);
        if (a.kind == ASMVal::IMM || dst == b)
            return neg8(as, b, b), add8(as, dst, a, b);

        Opcode op = b.kind == ASMVal::IMM ? Opcode::withExt(0x80, 0x05) : Opcode::from(0x28);
        mov8(as, dst, a);
        if (b.kind == ASMVal::IMM) {
            if (b.imm == 1)
                return unaryop(as, BYTE, Opcode::litExt(0xfe, 0x01), dst); // dec
            if (b.imm == -1)
                return unaryop(as, BYTE, Opcode::litExt(0xfe, 0x00), dst); // inc
        }
        binaryop(as, BYTE, op, dst, b);
    }

    static inline void sub16(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) {
        if (a == b)
            return xor16(as, dst, dst, dst);
        if (a.kind == ASMVal::IMM || dst == b)
            return neg16(as, b, b), add16(as, dst, a, b);

        Opcode op = b.kind == ASMVal::IMM ? Opcode::withExt(0x80, 0x05) : Opcode::from(0x28);
        mov16(as, dst, a);
        if (b.kind == ASMVal::IMM) {
            if (b.imm == 1)
                return unaryop(as, WORD, Opcode::litExt(0xff, 0x01), dst); // dec
            if (b.imm == -1)
                return unaryop(as, WORD, Opcode::litExt(0xff, 0x00), dst); // inc
        }
        binaryop(as, WORD, op, dst, b);
    }

    static inline void sub32(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) {
        if (a == b)
            return xor32(as, dst, dst, dst);
        if (a.kind == ASMVal::IMM || dst == b)
            return neg32(as, b, b), add32(as, dst, a, b);

        Opcode op = b.kind == ASMVal::IMM ? Opcode::withExt(0x80, 0x05) : Opcode::from(0x28);
        mov32(as, dst, a);
        if (b.kind == ASMVal::IMM) {
            if (b.imm == 1)
                return unaryop(as, DWORD, Opcode::litExt(0xff, 0x01), dst); // dec
            if (b.imm == -1)
                return unaryop(as, DWORD, Opcode::litExt(0xff, 0x00), dst); // inc
        }
        binaryop(as, DWORD, op, dst, b);
    }

    static inline void sub64(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) {
        if (a == b)
            return xor64(as, dst, dst, dst);
        if (a.kind == ASMVal::IMM || dst == b)
            return neg64(as, b, b), add64(as, dst, a, b);

        Opcode op = b.kind == ASMVal::IMM ? Opcode::withExt(0x80, 0x05) : Opcode::from(0x28);
        mov64(as, dst, a);
        if (b.kind == ASMVal::IMM) {
            if (b.imm == 1)
                return unaryop(as, QWORD, Opcode::litExt(0xff, 0x01), dst); // dec
            if (b.imm == -1)
                return unaryop(as, QWORD, Opcode::litExt(0xff, 0x00), dst); // inc
        }
        binaryop(as, QWORD, op, dst, b);
    }

    static inline void mul8(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) {
        if (a.kind == ASMVal::IMM) swap(a, b);
        if (b.kind == ASMVal::IMM) {
            binaryop(as, WORD, Opcode::literal(0x6b), a, dst); // WORD because no byte multiply exists.
            as.code.write<i8>(b.imm);
            return;
        }
        if (dst == b) swap(a, b);
        mov8(as, dst, a), binaryop(as, WORD, Opcode::literal(0x0f, 0xaf), b, dst);
    }

    static inline void mul16(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) {
        if (a.kind == ASMVal::IMM) swap(a, b);
        if (b.kind == ASMVal::IMM) {
            binaryop(as, WORD, Opcode::literal(0x69), a, dst);
            as.code.writeLE<i16>(b.imm);
            return;
        }
        if (dst == b) swap(a, b);
        mov16(as, dst, a), binaryop(as, WORD, Opcode::literal(0x0f, 0xaf), b, dst);
    }

    static inline void mul32(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) {
        if (a.kind == ASMVal::IMM) swap(a, b);
        if (b.kind == ASMVal::IMM) {
            binaryop(as, DWORD, Opcode::literal(0x69), a, dst);
            as.code.writeLE<i32>(b.imm);
            return;
        }
        if (dst == b) swap(a, b);
        mov32(as, dst, a), binaryop(as, DWORD, Opcode::literal(0x0f, 0xaf), b, dst);
    }

    static inline void mul64(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) {
        if (a.kind == ASMVal::IMM) swap(a, b);
        if (b.kind == ASMVal::IMM) {
            binaryop(as, QWORD, Opcode::literal(0x69), a, dst);
            as.code.writeLE<i32>(b.imm);
            return;
        }
        if (dst == b) swap(a, b);
        mov64(as, dst, a), binaryop(as, QWORD, Opcode::literal(0x0f, 0xaf), b, dst);
    }

    static inline void sdiv8(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) {
        mov8(as, GP(RAX), a);
        as.code.write<u8>(0x66); // cbw
        as.code.write<u8>(0x98);
        if (b.kind == ASMVal::IMM)
            mov8(as, GP(RDX), b), b = GP(RDX);
        unaryop(as, BYTE, Opcode::withExt(0xf6, 0x07), b);
        mov8(as, dst, GP(RAX));
    }

    static inline void sdiv16(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) {
        if (b.gp == RAX || b.gp == RDX)
            mov16(as, GP(RCX), b), b = GP(RCX);
        mov16(as, GP(RAX), a);
        if (b.kind == ASMVal::IMM)
            mov16(as, GP(RCX), b), b = GP(RCX);
        as.code.write<u8>(0x66); // cwd
        as.code.write<u8>(0x99);
        unaryop(as, WORD, Opcode::withExt(0xf6, 0x07), b);
        mov16(as, dst, GP(RAX));
    }

    static inline void sdiv32(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) {
        if (b.gp == RAX || b.gp == RDX)
            mov32(as, GP(RCX), b), b = GP(RCX);
        mov32(as, GP(RAX), a);
        if (b.kind == ASMVal::IMM)
            mov32(as, GP(RCX), b), b = GP(RCX);
        as.code.write<u8>(0x99); // cdq
        unaryop(as, DWORD, Opcode::withExt(0xf6, 0x07), b);
        mov32(as, dst, GP(RAX));
    }

    static inline void sdiv64(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) {
        if (b.gp == RAX || b.gp == RDX)
            mov64(as, GP(RCX), b), b = GP(RCX);
        mov64(as, GP(RAX), a);
        if (b.kind == ASMVal::IMM)
            mov64(as, GP(RCX), b), b = GP(RCX);
        as.code.write<u8>(0x48); // cqo
        as.code.write<u8>(0x99);
        unaryop(as, QWORD, Opcode::withExt(0xf6, 0x07), b);
        mov64(as, dst, GP(RAX));
    }

    static inline void udiv8(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) {
        if (a.kind == ASMVal::IMM)
            mov16(as, GP(RAX), a);
        else
            zxt8(as, GP(RAX), a);
        if (b.kind == ASMVal::IMM)
            mov8(as, GP(RDX), b), b = GP(RDX);
        unaryop(as, BYTE, Opcode::withExt(0xf6, 0x06), b);
        mov8(as, dst, GP(RAX));
    }

    static inline void udiv16(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) {
        if (b.gp == RAX || b.gp == RDX)
            mov16(as, GP(RCX), b), b = GP(RCX);
        mov16(as, GP(RAX), a), a = GP(RAX);
        if (b.kind == ASMVal::IMM)
            mov16(as, GP(RCX), b), b = GP(RCX);
        xor16(as, GP(RDX), GP(RDX), GP(RDX));
        unaryop(as, WORD, Opcode::withExt(0xf6, 0x06), b);
        mov16(as, dst, GP(RAX));
    }

    static inline void udiv32(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) {
        if (b.gp == RAX || b.gp == RDX)
            mov32(as, GP(RCX), b), b = GP(RCX);
        mov32(as, GP(RAX), a);
        if (b.kind == ASMVal::IMM)
            mov32(as, GP(RCX), b), b = GP(RCX);
        xor32(as, GP(RDX), GP(RDX), GP(RDX));
        unaryop(as, DWORD, Opcode::withExt(0xf6, 0x06), b);
        mov32(as, dst, GP(RAX));
    }

    static inline void udiv64(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) {
        if (b.gp == RAX || b.gp == RDX)
            mov64(as, GP(RCX), b), b = GP(RCX);
        mov64(as, GP(RAX), a);
        if (b.kind == ASMVal::IMM)
            mov64(as, GP(RCX), b), b = GP(RCX);
        xor64(as, GP(RDX), GP(RDX), GP(RDX));
        unaryop(as, QWORD, Opcode::withExt(0xf6, 0x06), b);
        mov64(as, dst, GP(RAX));
    }

    static inline void srem8(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) {
        mov8(as, GP(RAX), a);
        as.code.write<u8>(0x66); // cbw
        as.code.write<u8>(0x98);
        if (b.kind == ASMVal::IMM)
            mov8(as, GP(RDX), b), b = GP(RDX);
        unaryop(as, BYTE, Opcode::withExt(0xf6, 0x07), b);
        shr16(as, GP(RAX), GP(RAX), Imm(8));
        mov8(as, dst, GP(RAX));
    }

    static inline void srem16(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) {
        if (b.gp == RAX || b.gp == RDX)
            mov16(as, GP(RCX), b), b = GP(RCX);
        mov16(as, GP(RAX), a);
        if (b.kind == ASMVal::IMM)
            mov16(as, GP(RCX), b), b = GP(RCX);
        as.code.write<u8>(0x66); // cwd
        as.code.write<u8>(0x99);
        unaryop(as, WORD, Opcode::withExt(0xf6, 0x07), b);
        mov16(as, dst, GP(RDX));
    }

    static inline void srem32(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) {
        if (b.gp == RAX || b.gp == RDX)
            mov32(as, GP(RCX), b), b = GP(RCX);
        mov32(as, GP(RAX), a);
        if (b.kind == ASMVal::IMM)
            mov32(as, GP(RCX), b), b = GP(RCX);
        as.code.write<u8>(0x99); // cdq
        unaryop(as, DWORD, Opcode::withExt(0xf6, 0x07), b);
        mov32(as, dst, GP(RDX));
    }

    static inline void srem64(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) {
        if (b.gp == RAX || b.gp == RDX)
            mov64(as, GP(RCX), b), b = GP(RCX);
        mov64(as, GP(RAX), a);
        if (b.kind == ASMVal::IMM)
            mov64(as, GP(RCX), b), b = GP(RCX);
        as.code.write<u8>(0x48); // cqo
        as.code.write<u8>(0x99);
        unaryop(as, QWORD, Opcode::withExt(0xf6, 0x07), b);
        mov64(as, dst, GP(RDX));
    }

    static inline void urem8(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) {
        zxt8(as, GP(RAX), a);
        if (b.kind == ASMVal::IMM)
            mov8(as, GP(RDX), b), b = GP(RDX);
        unaryop(as, BYTE, Opcode::withExt(0xf6, 0x06), b);
        shr16(as, GP(RAX), GP(RAX), Imm(8));
        mov8(as, dst, GP(RAX));
    }

    static inline void urem16(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) {
        if (b.gp == RAX || b.gp == RDX)
            mov16(as, GP(RCX), b), b = GP(RCX);
        zxt16(as, GP(RAX), a);
        if (b.kind == ASMVal::IMM)
            mov16(as, GP(RCX), b), b = GP(RCX);
        xor64(as, GP(RDX), GP(RDX), GP(RDX));
        unaryop(as, WORD, Opcode::withExt(0xf6, 0x06), b);
        mov16(as, dst, GP(RDX));
    }

    static inline void urem32(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) {
        if (b.gp == RAX || b.gp == RDX)
            mov32(as, GP(RCX), b), b = GP(RCX);
        zxt32(as, GP(RAX), a);
        if (b.kind == ASMVal::IMM)
            mov32(as, GP(RCX), b), b = GP(RCX);
        xor64(as, GP(RDX), GP(RDX), GP(RDX));
        unaryop(as, DWORD, Opcode::withExt(0xf6, 0x06), b);
        mov32(as, dst, GP(RDX));
    }

    static inline void urem64(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) {
        if (b.gp == RAX || b.gp == RDX)
            mov64(as, GP(RCX), b), b = GP(RCX);
        mov64(as, GP(RAX), a);
        if (b.kind == ASMVal::IMM)
            mov64(as, GP(RCX), b), b = GP(RCX);
        xor64(as, GP(RDX), GP(RDX), GP(RDX));
        unaryop(as, QWORD, Opcode::withExt(0xf6, 0x06), b);
        mov64(as, dst, GP(RDX));
    }

    static inline void neg8(Assembly& as, ASMVal dst, ASMVal src) {
        mov8(as, dst, src);
        unaryop(as, BYTE, Opcode::withExt(0xf6, 0x03), dst);
    }

    static inline void neg16(Assembly& as, ASMVal dst, ASMVal src) {
        mov16(as, dst, src);
        unaryop(as, WORD, Opcode::withExt(0xf6, 0x03), dst);
    }

    static inline void neg32(Assembly& as, ASMVal dst, ASMVal src) {
        mov32(as, dst, src);
        unaryop(as, DWORD, Opcode::withExt(0xf6, 0x03), dst);
    }

    static inline void neg64(Assembly& as, ASMVal dst, ASMVal src) {
        mov64(as, dst, src);
        unaryop(as, QWORD, Opcode::withExt(0xf6, 0x03), dst);
    }

    // Floating-Point Arithmetic

    static inline void fadd32(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) {
        vexbinaryop(as, VexPrefixF3, false, Opcode::from(0x58), dst, a, b, true, TwoByteOpcode);
    }

    static inline void fadd64(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) {
        vexbinaryop(as, VexPrefixF2, false, Opcode::from(0x58), dst, a, b, true, TwoByteOpcode);
    }

    static inline void fsub32(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) {
        if (a.kind == ASMVal::F32) {
            fneg32(as, dst, b);
            fadd32(as, dst, dst, a);
            return;
        }
        vexbinaryop(as, VexPrefixF3, false, Opcode::from(0x5c), dst, a, b, false, TwoByteOpcode);
    }

    static inline void fsub64(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) {
        if (a.kind == ASMVal::F64) {
            fneg64(as, dst, b);
            fadd64(as, dst, dst, a);
            return;
        }
        vexbinaryop(as, VexPrefixF2, false, Opcode::from(0x5c), dst, a, b, false, TwoByteOpcode);
    }

    static inline void fmul32(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) {
        vexbinaryop(as, VexPrefixF3, false, Opcode::from(0x59), dst, a, b, true, TwoByteOpcode);
    }

    static inline void fmul64(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) {
        vexbinaryop(as, VexPrefixF2, false, Opcode::from(0x59), dst, a, b, true, TwoByteOpcode);
    }

    static inline void fdiv32(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) {
        if (a.kind == ASMVal::F32) {
            if (dst != b) {
                fmov32(as, dst, a);
                a = dst;
            } else {
                fmov32(as, FP(XMM0), a);
                a = FP(XMM0);
            }
        }
        vexbinaryop(as, VexPrefixF3, false, Opcode::from(0x5e), dst, a, b, false, TwoByteOpcode);
    }

    static inline void fdiv64(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) {
        if (a.kind == ASMVal::F64) {
            if (dst != b) {
                fmov64(as, dst, a);
                a = dst;
            } else {
                fmov64(as, FP(XMM0), a);
                a = FP(XMM0);
            }
        }
        vexbinaryop(as, VexPrefixF2, false, Opcode::from(0x5e), dst, a, b, false, TwoByteOpcode);
    }

    static inline void frem32(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) {
        fdiv32(as, FP(XMM0), a, b);
        ftrunc32(as, FP(XMM0), FP(XMM0));
        fmul32(as, FP(XMM0), FP(XMM0), b);
        fsub32(as, dst, a, FP(XMM0));
    }

    static inline void frem64(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) {
        fdiv64(as, FP(XMM0), a, b);
        ftrunc64(as, FP(XMM0), FP(XMM0));
        fmul64(as, FP(XMM0), FP(XMM0), b);
        fsub64(as, dst, a, FP(XMM0));
    }

    static void fsqrt32(Assembly& as, ASMVal dst, ASMVal src) {
        vexunaryop(as, VexPrefixF3, false, Opcode::from(0x51), dst, src, TwoByteOpcode);
    }

    static void fsqrt64(Assembly& as, ASMVal dst, ASMVal src) {
        vexunaryop(as, VexPrefixF2, false, Opcode::from(0x51), dst, src, TwoByteOpcode);
    }

    static void fround32(Assembly& as, ASMVal dst, ASMVal src) {
        vexunaryop(as, VexPrefix66, false, Opcode::from(0x0a), dst, src, ThreeByteOpcode3A);
        as.code.write<u8>(0x00); // Round up or down, breaking ties using banker's rounding.
    }

    static void fround64(Assembly& as, ASMVal dst, ASMVal src) {
        vexunaryop(as, VexPrefix66, false, Opcode::from(0x0b), dst, src, ThreeByteOpcode3A);
        as.code.write<u8>(0x00); // Round up or down, breaking ties using banker's rounding.
    }

    static void ffloor32(Assembly& as, ASMVal dst, ASMVal src) {
        vexunaryop(as, VexPrefix66, false, Opcode::from(0x0a), dst, src, ThreeByteOpcode3A);
        as.code.write<u8>(0x01); // Round towards negative infinity.
    }

    static void ffloor64(Assembly& as, ASMVal dst, ASMVal src) {
        vexunaryop(as, VexPrefix66, false, Opcode::from(0x0b), dst, src, ThreeByteOpcode3A);
        as.code.write<u8>(0x01); // Round towards negative infinity.
    }

    static void fceil32(Assembly& as, ASMVal dst, ASMVal src) {
        vexunaryop(as, VexPrefix66, false, Opcode::from(0x0a), dst, src, ThreeByteOpcode3A);
        as.code.write<u8>(0x02); // Round towards positive infinity.
    }

    static void fceil64(Assembly& as, ASMVal dst, ASMVal src) {
        vexunaryop(as, VexPrefix66, false, Opcode::from(0x0b), dst, src, ThreeByteOpcode3A);
        as.code.write<u8>(0x02); // Round towards positive infinity.
    }

    static void ftrunc32(Assembly& as, ASMVal dst, ASMVal src) {
        vexunaryop(as, VexPrefix66, false, Opcode::from(0x0a), dst, src, ThreeByteOpcode3A);
        as.code.write<u8>(0x03); // Round towards zero.
    }

    static void ftrunc64(Assembly& as, ASMVal dst, ASMVal src) {
        vexunaryop(as, VexPrefix66, false, Opcode::from(0x0b), dst, src, ThreeByteOpcode3A);
        as.code.write<u8>(0x03); // Round towards zero.
    }

    static inline void fneg32(Assembly& as, ASMVal dst, ASMVal src) {
        vexbinaryop(as, NoPrefix, false, Opcode::from(0x57), dst, src, F32(-0.0f), true, TwoByteOpcode);
    }

    static inline void fneg64(Assembly& as, ASMVal dst, ASMVal src) {
        vexbinaryop(as, VexPrefix66, false, Opcode::from(0x57), dst, src, F64(-0.0), true, TwoByteOpcode);
    }

    static void fmin32(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) {
        vexbinaryop(as, VexPrefixF3, false, Opcode::from(0x5d), dst, a, b, true, TwoByteOpcode);
    }

    static void fmin64(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) {
        vexbinaryop(as, VexPrefixF2, false, Opcode::from(0x5d), dst, a, b, true, TwoByteOpcode);
    }

    static void fmax32(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) {
        vexbinaryop(as, VexPrefixF3, false, Opcode::from(0x5f), dst, a, b, true, TwoByteOpcode);
    }

    static void fmax64(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) {
        vexbinaryop(as, VexPrefixF2, false, Opcode::from(0x5f), dst, a, b, true, TwoByteOpcode);
    }

    static void fabs32(Assembly& as, ASMVal dst, ASMVal src) {
        vexbinaryop(as, VexPrefix66, false, Opcode::from(0x76), FP(XMM0), FP(XMM0), FP(XMM0), true, TwoByteOpcode); // pcmpeqd => all 1s
        vexbinaryop(as, VexPrefix66, false, Opcode::from(0x72), FP(XMM0), FP(XMM0), FP(XMM0), true, TwoByteOpcode); // psrld => mask
        as.code.write<u8>(1); // imm8 for shift
        vexbinaryop(as, NoPrefix, false, Opcode::from(0x54), dst, FP(XMM0), src, true, TwoByteOpcode); // andps
    }

    static void fabs64(Assembly& as, ASMVal dst, ASMVal src) {
        vexbinaryop(as, VexPrefix66, false, Opcode::from(0x29), FP(XMM0), FP(XMM0), FP(XMM0), true, ThreeByteOpcode38); // pcmpeqq => all 1s
        vexbinaryop(as, VexPrefix66, false, Opcode::from(0x73), FP(XMM0), FP(XMM0), FP(XMM0), true, TwoByteOpcode); // psrld => mask
        as.code.write<u8>(1); // imm8 for shift
        vexbinaryop(as, VexPrefix66, false, Opcode::from(0x54), dst, FP(XMM0), src, true, TwoByteOpcode); // andpd
    }

    // Bitwise Operations

    static inline void and8(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) {
        if (dst == b) swap(a, b);
        Opcode op = b.kind == ASMVal::IMM ? Opcode::withExt(0x80, 0x04) : Opcode::from(0x20);
        mov8(as, dst, a);
        binaryop(as, BYTE, op, dst, b);
    }

    static inline void and16(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) {
        if (dst == b) swap(a, b);
        Opcode op = b.kind == ASMVal::IMM ? Opcode::withExt(0x80, 0x04) : Opcode::from(0x20);
        mov16(as, dst, a);
        binaryop(as, WORD, op, dst, b);
    }

    static inline void and32(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) {
        if (dst == b) swap(a, b);
        Opcode op = b.kind == ASMVal::IMM ? Opcode::withExt(0x80, 0x04) : Opcode::from(0x20);
        mov32(as, dst, a);
        binaryop(as, DWORD, op, dst, b);
    }

    static inline void and64(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) {
        if (dst == b) swap(a, b);
        Opcode op = b.kind == ASMVal::IMM ? Opcode::withExt(0x80, 0x04) : Opcode::from(0x20);
        mov64(as, dst, a);
        binaryop(as, QWORD, op, dst, b);
    }

    static inline void xor8(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) {
        if (dst == b) swap(a, b);
        Opcode op = b.kind == ASMVal::IMM ? Opcode::withExt(0x80, 0x06) : Opcode::from(0x30);
        mov8(as, dst, a);
        binaryop(as, BYTE, op, dst, b);
    }

    static inline void xor16(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) {
        if (dst == b) swap(a, b);
        Opcode op = b.kind == ASMVal::IMM ? Opcode::withExt(0x80, 0x06) : Opcode::from(0x30);
        mov16(as, dst, a);
        binaryop(as, WORD, op, dst, b);
    }

    static inline void xor32(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) {
        if (dst == b) swap(a, b);
        Opcode op = b.kind == ASMVal::IMM ? Opcode::withExt(0x80, 0x06) : Opcode::from(0x30);
        mov32(as, dst, a);
        binaryop(as, DWORD, op, dst, b);
    }

    static inline void xor64(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) {
        if (dst == b) swap(a, b);
        Opcode op = b.kind == ASMVal::IMM ? Opcode::withExt(0x80, 0x06) : Opcode::from(0x30);
        mov64(as, dst, a);
        binaryop(as, QWORD, op, dst, b);
    }

    static inline void or8(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) {
        if (dst == b) swap(a, b);
        Opcode op = b.kind == ASMVal::IMM ? Opcode::withExt(0x80, 0x01) : Opcode::from(0x08);
        mov8(as, dst, a);
        binaryop(as, BYTE, op, dst, b);
    }

    static inline void or16(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) {
        if (dst == b) swap(a, b);
        Opcode op = b.kind == ASMVal::IMM ? Opcode::withExt(0x80, 0x01) : Opcode::from(0x08);
        mov16(as, dst, a);
        binaryop(as, WORD, op, dst, b);
    }

    static inline void or32(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) {
        if (dst == b) swap(a, b);
        Opcode op = b.kind == ASMVal::IMM ? Opcode::withExt(0x80, 0x01) : Opcode::from(0x08);
        mov32(as, dst, a);
        binaryop(as, DWORD, op, dst, b);
    }

    static inline void or64(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) {
        if (dst == b) swap(a, b);
        Opcode op = b.kind == ASMVal::IMM ? Opcode::withExt(0x80, 0x01) : Opcode::from(0x08);
        mov64(as, dst, a);
        binaryop(as, QWORD, op, dst, b);
    }

    static inline void not8(Assembly& as, ASMVal dst, ASMVal src) {
        mov8(as, dst, src);
        unaryop(as, BYTE, Opcode::withExt(0xf6, 0x02), dst);
    }

    static inline void not16(Assembly& as, ASMVal dst, ASMVal src) {
        mov16(as, dst, src);
        unaryop(as, WORD, Opcode::withExt(0xf6, 0x02), dst);
    }

    static inline void not32(Assembly& as, ASMVal dst, ASMVal src) {
        mov32(as, dst, src);
        unaryop(as, DWORD, Opcode::withExt(0xf6, 0x02), dst);
    }

    static inline void not64(Assembly& as, ASMVal dst, ASMVal src) {
        mov64(as, dst, src);
        unaryop(as, QWORD, Opcode::withExt(0xf6, 0x02), dst);
    }

    static void shl8(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) {
        Opcode op = b.kind == ASMVal::IMM ? Opcode::litExt(0xC0, 0x04) : Opcode::litExt(0xD2, 0x04);
        if (b.kind != ASMVal::IMM)
            mov8(as, GP(RCX), b);
        mov8(as, dst, a);
        if (b.kind == ASMVal::IMM && b.imm % 8 == 0)
            return;
        unaryop(as, BYTE, op, dst);
        if (b.kind == ASMVal::IMM)
            as.code.write<i8>(b.imm);
    }

    static void shl16(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) {
        Opcode op = b.kind == ASMVal::IMM ? Opcode::litExt(0xC1, 0x04) : Opcode::litExt(0xD3, 0x04);
        if (b.kind != ASMVal::IMM)
            mov8(as, GP(RCX), b);
        mov16(as, dst, a);
        if (b.kind == ASMVal::IMM && b.imm % 16 == 0)
            return;
        unaryop(as, WORD, op, dst);
        if (b.kind == ASMVal::IMM)
            as.code.write<i8>(b.imm);
    }

    static void shl32(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) {
        Opcode op = b.kind == ASMVal::IMM ? Opcode::litExt(0xC1, 0x04) : Opcode::litExt(0xD3, 0x04);
        mov8(as, GP(RCX), b);
        mov32(as, dst, a);
        if (b.kind == ASMVal::IMM && b.imm % 32 == 0)
            return;
        unaryop(as, DWORD, op, dst);
        if (b.kind == ASMVal::IMM)
            as.code.write<i8>(b.imm);
    }

    static void shl64(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) {
        Opcode op = b.kind == ASMVal::IMM ? Opcode::litExt(0xC1, 0x04) : Opcode::litExt(0xD3, 0x04);
        if (b.kind != ASMVal::IMM)
            mov8(as, GP(RCX), b);
        mov64(as, dst, a);
        if (b.kind == ASMVal::IMM && b.imm % 64 == 0)
            return;
        unaryop(as, QWORD, op, dst);
        if (b.kind == ASMVal::IMM)
            as.code.write<i8>(b.imm);
    }

    static void shr8(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) {
        Opcode op = b.kind == ASMVal::IMM ? Opcode::litExt(0xC0, 0x05) : Opcode::litExt(0xD2, 0x05);
        if (b.kind != ASMVal::IMM)
            mov8(as, GP(RCX), b);
        mov8(as, dst, a);
        if (b.kind == ASMVal::IMM && b.imm % 8 == 0)
            return;
        unaryop(as, BYTE, op, dst);
        if (b.kind == ASMVal::IMM)
            as.code.write<i8>(b.imm);
    }

    static void shr16(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) {
        Opcode op = b.kind == ASMVal::IMM ? Opcode::litExt(0xC1, 0x05) : Opcode::litExt(0xD3, 0x05);
        if (b.kind != ASMVal::IMM)
            mov8(as, GP(RCX), b);
        mov16(as, dst, a);
        if (b.kind == ASMVal::IMM && b.imm % 16 == 0)
            return;
        unaryop(as, WORD, op, dst);
        if (b.kind == ASMVal::IMM)
            as.code.write<i8>(b.imm);
    }

    static void shr32(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) {
        Opcode op = b.kind == ASMVal::IMM ? Opcode::litExt(0xC1, 0x05) : Opcode::litExt(0xD3, 0x05);
        if (b.kind != ASMVal::IMM)
            mov8(as, GP(RCX), b);
        mov32(as, dst, a);
        if (b.kind == ASMVal::IMM && b.imm % 32 == 0)
            return;
        unaryop(as, DWORD, op, dst);
        if (b.kind == ASMVal::IMM)
            as.code.write<i8>(b.imm);
    }

    static void shr64(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) {
        Opcode op = b.kind == ASMVal::IMM ? Opcode::litExt(0xC1, 0x05) : Opcode::litExt(0xD3, 0x05);
        if (b.kind != ASMVal::IMM)
            mov8(as, GP(RCX), b);
        mov64(as, dst, a);
        if (b.kind == ASMVal::IMM && b.imm % 64 == 0)
            return;
        unaryop(as, QWORD, op, dst);
        if (b.kind == ASMVal::IMM)
            as.code.write<i8>(b.imm);
    }

    static void sar8(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) {
        Opcode op = b.kind == ASMVal::IMM ? Opcode::litExt(0xC0, 0x07) : Opcode::litExt(0xD2, 0x07);
        if (b.kind != ASMVal::IMM)
            mov8(as, GP(RCX), b);
        mov8(as, dst, a);
        if (b.kind == ASMVal::IMM && b.imm % 8 == 0)
            return;
        unaryop(as, BYTE, op, dst);
        if (b.kind == ASMVal::IMM)
            as.code.write<i8>(b.imm);
    }

    static void sar16(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) {
        Opcode op = b.kind == ASMVal::IMM ? Opcode::litExt(0xC1, 0x07) : Opcode::litExt(0xD3, 0x07);
        if (b.kind != ASMVal::IMM)
            mov8(as, GP(RCX), b);
        mov16(as, dst, a);
        if (b.kind == ASMVal::IMM && b.imm % 16 == 0)
            return;
        unaryop(as, WORD, op, dst);
        if (b.kind == ASMVal::IMM)
            as.code.write<i8>(b.imm);
    }

    static void sar32(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) {
        Opcode op = b.kind == ASMVal::IMM ? Opcode::litExt(0xC1, 0x07) : Opcode::litExt(0xD3, 0x07);
        if (b.kind != ASMVal::IMM)
            mov8(as, GP(RCX), b);
        mov32(as, dst, a);
        if (b.kind == ASMVal::IMM && b.imm % 32 == 0)
            return;
        unaryop(as, DWORD, op, dst);
        if (b.kind == ASMVal::IMM)
            as.code.write<i8>(b.imm);
    }

    static void sar64(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) {
        Opcode op = b.kind == ASMVal::IMM ? Opcode::litExt(0xC1, 0x07) : Opcode::litExt(0xD3, 0x07);
        if (b.kind != ASMVal::IMM)
            mov8(as, GP(RCX), b);
        mov64(as, dst, a);
        if (b.kind == ASMVal::IMM && b.imm % 64 == 0)
            return;
        unaryop(as, QWORD, op, dst);
        if (b.kind == ASMVal::IMM)
            as.code.write<i8>(b.imm);
    }

    static void rol8(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) {
        Opcode op = b.kind == ASMVal::IMM ? Opcode::litExt(0xC0, 0x00) : Opcode::litExt(0xD2, 0x00);
        if (b.kind != ASMVal::IMM)
            mov8(as, GP(RCX), b);
        mov8(as, dst, a);
        if (b.kind == ASMVal::IMM && b.imm % 8 == 0)
            return;
        unaryop(as, BYTE, op, dst);
        if (b.kind == ASMVal::IMM)
            as.code.write<i8>(b.imm);
    }

    static void rol16(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) {
        Opcode op = b.kind == ASMVal::IMM ? Opcode::litExt(0xC1, 0x00) : Opcode::litExt(0xD3, 0x00);
        if (b.kind != ASMVal::IMM)
            mov8(as, GP(RCX), b);
        mov16(as, dst, a);
        if (b.kind == ASMVal::IMM && b.imm % 16 == 0)
            return;
        unaryop(as, WORD, op, dst);
        if (b.kind == ASMVal::IMM)
            as.code.write<i8>(b.imm);
    }

    static void rol32(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) {
        Opcode op = b.kind == ASMVal::IMM ? Opcode::litExt(0xC1, 0x00) : Opcode::litExt(0xD3, 0x00);
        if (b.kind != ASMVal::IMM)
            mov8(as, GP(RCX), b);
        mov32(as, dst, a);
        if (b.kind == ASMVal::IMM && b.imm % 32 == 0)
            return;
        unaryop(as, DWORD, op, dst);
        if (b.kind == ASMVal::IMM)
            as.code.write<i8>(b.imm);
    }

    static void rol64(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) {
        Opcode op = b.kind == ASMVal::IMM ? Opcode::litExt(0xC1, 0x00) : Opcode::litExt(0xD3, 0x00);
        if (b.kind != ASMVal::IMM)
            mov8(as, GP(RCX), b);
        mov64(as, dst, a);
        if (b.kind == ASMVal::IMM && b.imm % 64 == 0)
            return;
        unaryop(as, QWORD, op, dst);
        if (b.kind == ASMVal::IMM)
            as.code.write<i8>(b.imm);
    }

    static void ror8(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) {
        Opcode op = b.kind == ASMVal::IMM ? Opcode::litExt(0xC0, 0x01) : Opcode::litExt(0xD2, 0x01);
        if (b.kind != ASMVal::IMM)
            mov8(as, GP(RCX), b);
        mov8(as, dst, a);
        if (b.kind == ASMVal::IMM && b.imm % 8 == 0)
            return;
        unaryop(as, BYTE, op, dst);
        if (b.kind == ASMVal::IMM)
            as.code.write<i8>(b.imm);
    }

    static void ror16(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) {
        Opcode op = b.kind == ASMVal::IMM ? Opcode::litExt(0xC1, 0x01) : Opcode::litExt(0xD3, 0x01);
        if (b.kind != ASMVal::IMM)
            mov8(as, GP(RCX), b);
        mov16(as, dst, a);
        if (b.kind == ASMVal::IMM && b.imm % 16 == 0)
            return;
        unaryop(as, WORD, op, dst);
        if (b.kind == ASMVal::IMM)
            as.code.write<i8>(b.imm);
    }

    static void ror32(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) {
        Opcode op = b.kind == ASMVal::IMM ? Opcode::litExt(0xC1, 0x01) : Opcode::litExt(0xD3, 0x01);
        if (b.kind != ASMVal::IMM)
            mov8(as, GP(RCX), b);
        mov32(as, dst, a);
        if (b.kind == ASMVal::IMM && b.imm % 32 == 0)
            return;
        unaryop(as, DWORD, op, dst);
        if (b.kind == ASMVal::IMM)
            as.code.write<i8>(b.imm);
    }

    static void ror64(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) {
        Opcode op = b.kind == ASMVal::IMM ? Opcode::litExt(0xC1, 0x01) : Opcode::litExt(0xD3, 0x01);
        if (b.kind != ASMVal::IMM)
            mov8(as, GP(RCX), b);
        mov64(as, dst, a);
        if (b.kind == ASMVal::IMM && b.imm % 64 == 0)
            return;
        unaryop(as, QWORD, op, dst);
        if (b.kind == ASMVal::IMM)
            as.code.write<i8>(b.imm);
    }

    static void popcnt8(Assembly& as, ASMVal dst, ASMVal src) {
        and16(as, dst, src, Imm(255));
        as.code.write<u8>(0xf3); // Extra prefix byte before operand size override.
        binaryop(as, WORD, Opcode::literal(0x0f, 0xb8), dst, dst);
    }

    static void popcnt16(Assembly& as, ASMVal dst, ASMVal src) {
        as.code.write<u8>(0xf3); // Extra prefix byte before operand size override.
        binaryop(as, WORD, Opcode::literal(0x0f, 0xb8), src, dst);
    }

    static void popcnt32(Assembly& as, ASMVal dst, ASMVal src) {
        as.code.write<u8>(0xf3); // Extra prefix byte before operand size override.
        binaryop(as, DWORD, Opcode::literal(0x0f, 0xb8), src, dst);
    }

    static void popcnt64(Assembly& as, ASMVal dst, ASMVal src) {
        as.code.write<u8>(0xf3); // Extra prefix byte before operand size override.
        binaryop(as, QWORD, Opcode::literal(0x0f, 0xb8), src, dst);
    }

    static void lzcnt8(Assembly& as, ASMVal dst, ASMVal src) {
        shl16(as, dst, src, Imm(8));
        or16(as, dst, dst, Imm(255));
        as.code.write<u8>(0xf3); // Extra prefix byte before operand size override.
        binaryop(as, WORD, Opcode::literal(0x0f, 0xbd), dst, dst);
    }

    static void lzcnt16(Assembly& as, ASMVal dst, ASMVal src) {
        as.code.write<u8>(0xf3); // Extra prefix byte before operand size override.
        binaryop(as, WORD, Opcode::literal(0x0f, 0xbd), src, dst);
    }

    static void lzcnt32(Assembly& as, ASMVal dst, ASMVal src) {
        as.code.write<u8>(0xf3); // Extra prefix byte before operand size override.
        binaryop(as, DWORD, Opcode::literal(0x0f, 0xbd), src, dst);
    }

    static void lzcnt64(Assembly& as, ASMVal dst, ASMVal src) {
        as.code.write<u8>(0xf3); // Extra prefix byte before operand size override.
        binaryop(as, QWORD, Opcode::literal(0x0f, 0xbd), src, dst);
    }

    static void tzcnt8(Assembly& as, ASMVal dst, ASMVal src) {
        or16(as, dst, src, Imm(0xff00));
        as.code.write<u8>(0xf3); // Extra prefix byte before operand size override.
        binaryop(as, WORD, Opcode::literal(0x0f, 0xbc), dst, dst);
    }

    static void tzcnt16(Assembly& as, ASMVal dst, ASMVal src) {
        as.code.write<u8>(0xf3); // Extra prefix byte before operand size override.
        binaryop(as, WORD, Opcode::literal(0x0f, 0xbc), src, dst);
    }

    static void tzcnt32(Assembly& as, ASMVal dst, ASMVal src) {
        as.code.write<u8>(0xf3); // Extra prefix byte before operand size override.
        binaryop(as, DWORD, Opcode::literal(0x0f, 0xbc), src, dst);
    }

    static void tzcnt64(Assembly& as, ASMVal dst, ASMVal src) {
        as.code.write<u8>(0xf3); // Extra prefix byte before operand size override.
        binaryop(as, QWORD, Opcode::literal(0x0f, 0xbc), src, dst);
    }

    // Comparisons

    static constexpr u8 CCodes[12] = {
        0x4, // Equal
        0x5, // Not Equal
        0xc, // Less
        0xe, // Less Equal
        0xf, // Greater
        0xd, // Greater Equal
        0x7, // Above
        0x3, // Above Equal
        0x2, // Below
        0x6, // Below Equal
        0x4, // Test if zero
        0x5, // Test if nonzero
    };

    static inline void test8(Assembly& as, ASMVal a, ASMVal b) {
        Opcode op = b.kind == ASMVal::IMM ? Opcode::withExt(0xf6, 0x00) : Opcode::from(0x84);
        binaryop(as, BYTE, op, a, b);
    }

    static inline void test16(Assembly& as, ASMVal a, ASMVal b) {
        Opcode op = b.kind == ASMVal::IMM ? Opcode::withExt(0xf6, 0x00) : Opcode::from(0x84);
        binaryop(as, WORD, op, a, b);
    }

    static inline void test32(Assembly& as, ASMVal a, ASMVal b) {
        Opcode op = b.kind == ASMVal::IMM ? Opcode::withExt(0xf6, 0x00) : Opcode::from(0x84);
        binaryop(as, DWORD, op, a, b);
    }

    static inline void test64(Assembly& as, ASMVal a, ASMVal b) {
        Opcode op = b.kind == ASMVal::IMM ? Opcode::withExt(0xf6, 0x00) : Opcode::from(0x84);
        binaryop(as, QWORD, op, a, b);
    }

    static inline void cmp8(Assembly& as, ASMVal a, ASMVal b) {
        Opcode op = b.kind == ASMVal::IMM ? Opcode::withExt(0x80, 0x07) : Opcode::from(0x38);
        binaryop(as, BYTE, op, a, b);
    }

    static inline void cmp16(Assembly& as, ASMVal a, ASMVal b) {
        Opcode op = b.kind == ASMVal::IMM ? Opcode::withExt(0x80, 0x07) : Opcode::from(0x38);
        binaryop(as, WORD, op, a, b);
    }

    static inline void cmp32(Assembly& as, ASMVal a, ASMVal b) {
        Opcode op = b.kind == ASMVal::IMM ? Opcode::withExt(0x80, 0x07) : Opcode::from(0x38);
        binaryop(as, DWORD, op, a, b);
    }

    static inline void cmp64(Assembly& as, ASMVal a, ASMVal b) {
        Opcode op = b.kind == ASMVal::IMM ? Opcode::withExt(0x80, 0x07) : Opcode::from(0x38);
        binaryop(as, QWORD, op, a, b);
    }

    static inline void fcmp32(Assembly& as, ASMVal a, ASMVal b) {
        vexbinaryop(as, NoPrefix, false, Opcode::from(0x2f), a, FP(XMM0), b, false, TwoByteOpcode);
    }

    static inline void fcmp64(Assembly& as, ASMVal a, ASMVal b) {
        vexbinaryop(as, VexPrefix66, false, Opcode::from(0x2f), a, FP(XMM0), b, false, TwoByteOpcode);
    }

    static inline void isz(Assembly& as, ASMVal dst, ASMVal src) {
        test64(as, src, src);
        unaryop(as, BYTE, Opcode::withExt(0x0f, 0x94, 0x00), dst);
    }

    static inline void isnz(Assembly& as, ASMVal dst, ASMVal src) {
        test64(as, src, src);
        unaryop(as, BYTE, Opcode::withExt(0x0f, 0x95, 0x00), dst);
    }

    static inline void cmpcc8(Assembly& as, Condition cc, ASMVal dst, ASMVal a, ASMVal b) {
        if (a.kind == ASMVal::IMM) {
            swap(a, b);
            cc = commute(cc);
        }
        cmp8(as, a, b);
        unaryop(as, BYTE, Opcode::withExt(0x0f, 0x90 + CCodes[cc], 0x00), dst);
        zxt8(as, dst, dst);
    }

    static inline void cmpcc16(Assembly& as, Condition cc, ASMVal dst, ASMVal a, ASMVal b) {
        if (a.kind == ASMVal::IMM) {
            swap(a, b);
            cc = commute(cc);
        }
        cmp16(as, a, b);
        unaryop(as, BYTE, Opcode::withExt(0x0f, 0x90 + CCodes[cc], 0x00), dst);
        zxt8(as, dst, dst);
    }

    static inline void cmpcc32(Assembly& as, Condition cc, ASMVal dst, ASMVal a, ASMVal b) {
        if (a.kind == ASMVal::IMM) {
            swap(a, b);
            cc = commute(cc);
        }
        cmp32(as, a, b);
        unaryop(as, BYTE, Opcode::withExt(0x0f, 0x90 + CCodes[cc], 0x00), dst);
        zxt8(as, dst, dst);
    }

    static inline void cmpcc64(Assembly& as, Condition cc, ASMVal dst, ASMVal a, ASMVal b) {
        if (a.kind == ASMVal::IMM) {
            swap(a, b);
            cc = commute(cc);
        }
        cmp64(as, a, b);
        unaryop(as, BYTE, Opcode::withExt(0x0f, 0x90 + CCodes[cc], 0x00), dst);
        zxt8(as, dst, dst);
    }

    static inline void fcmpcc32(Assembly& as, FloatCondition cc, ASMVal dst, ASMVal a, ASMVal b) {
        fcmp32(as, a, b);
        unaryop(as, BYTE, Opcode::withExt(0x0f, 0x90 + CCodes[cc], 0x00), dst);
        zxt8(as, dst, dst);
    }

    static inline void fcmpcc64(Assembly& as, FloatCondition cc, ASMVal dst, ASMVal a, ASMVal b) {
        fcmp32(as, a, b);
        unaryop(as, BYTE, Opcode::withExt(0x0f, 0x90 + CCodes[cc], 0x00), dst);
        zxt8(as, dst, dst);
    }

    static void selcc8(Assembly& as, Condition cc, ASMVal dst, ASMVal a, ASMVal b, ASMVal c, ASMVal d) {
        cmp8(as, a, b);
        mov8(as, dst, d);
        binaryop(as, BYTE, Opcode::literal(0x0f, 0x40 + CCodes[cc]), c, dst);
    }

    static void selcc16(Assembly& as, Condition cc, ASMVal dst, ASMVal a, ASMVal b, ASMVal c, ASMVal d) {
        cmp16(as, a, b);
        mov16(as, dst, d);
        binaryop(as, WORD, Opcode::literal(0x0f, 0x40 + CCodes[cc]), c, dst);
    }

    static void selcc32(Assembly& as, Condition cc, ASMVal dst, ASMVal a, ASMVal b, ASMVal c, ASMVal d) {
        cmp32(as, a, b);
        mov32(as, dst, d);
        binaryop(as, DWORD, Opcode::literal(0x0f, 0x40 + CCodes[cc]), c, dst);
    }

    static void selcc64(Assembly& as, Condition cc, ASMVal dst, ASMVal a, ASMVal b, ASMVal c, ASMVal d) {
        cmp64(as, a, b);
        mov64(as, dst, d);
        binaryop(as, QWORD, Opcode::literal(0x0f, 0x40 + CCodes[cc]), c, dst);
    }

    static void fselcc32(Assembly& as, FloatCondition cc, ASMVal dst, ASMVal a, ASMVal b, ASMVal c, ASMVal d) {
        unreachable("TODO: Implement floating-point selection on amd64");
    }

    static void fselcc64(Assembly& as, FloatCondition cc, ASMVal dst, ASMVal a, ASMVal b, ASMVal c, ASMVal d) {
        unreachable("TODO: Implement floating-point selection on amd64");
    }

    // Memory

    static inline void push8(Assembly& as, ASMVal src) {
        assert(src.kind == ASMVal::GP);
        as.code.write<u8>(0x66);
        if (needsREX(BYTE, src)) as.code.write<u8>(0x41);
        as.code.write<u8>(0x50 + (src.gp & 0b111));
    }

    static inline void push16(Assembly& as, ASMVal src) {
        assert(src.kind == ASMVal::GP);
        as.code.write<u8>(0x66);
        if (needsREX(BYTE, src)) as.code.write<u8>(0x41);
        as.code.write<u8>(0x50 + (src.gp & 0b111));
    }

    static inline void push32(Assembly& as, ASMVal src) {
        assert(src.kind == ASMVal::GP);
        if (needsREX(BYTE, src)) as.code.write<u8>(0x41);
        as.code.write<u8>(0x50 + (src.gp & 0b111));
    }

    static inline void push64(Assembly& as, ASMVal src) {
        assert(src.kind == ASMVal::GP);
        if (needsREX(BYTE, src)) as.code.write<u8>(0x41);
        as.code.write<u8>(0x50 + (src.gp & 0b111));
    }

    static inline void pop8(Assembly& as, ASMVal dst) {
        assert(dst.kind == ASMVal::GP);
        as.code.write<u8>(0x66);
        if (needsREX(BYTE, dst)) as.code.write<u8>(0x41);
        as.code.write<u8>(0x58 + (dst.gp & 0b111));
    }

    static inline void pop16(Assembly& as, ASMVal dst) {
        assert(dst.kind == ASMVal::GP);
        as.code.write<u8>(0x66);
        if (needsREX(BYTE, dst)) as.code.write<u8>(0x41);
        as.code.write<u8>(0x58 + (dst.gp & 0b111));
    }

    static inline void pop32(Assembly& as, ASMVal dst) {
        assert(dst.kind == ASMVal::GP);
        if (needsREX(BYTE, dst)) as.code.write<u8>(0x41);
        as.code.write<u8>(0x58 + (dst.gp & 0b111));
    }

    static inline void pop64(Assembly& as, ASMVal dst) {
        assert(dst.kind == ASMVal::GP);
        if (needsREX(BYTE, dst)) as.code.write<u8>(0x41);
        as.code.write<u8>(0x58 + (dst.gp & 0b111));
    }

    static inline void fpush32(Assembly& as, ASMVal src) {
        f32tobits(as, GP(RAX), src);
        push32(as, GP(RAX));
    }

    static inline void fpush64(Assembly& as, ASMVal src) {
        f64tobits(as, GP(RAX), src);
        push64(as, GP(RAX));
    }

    static inline void fpop32(Assembly& as, ASMVal dst) {
        pop32(as, GP(RAX));
        f32frombits(as, dst, GP(RAX));
    }

    static inline void fpop64(Assembly& as, ASMVal dst) {
        pop64(as, GP(RAX));
        f64frombits(as, dst, GP(RAX));
    }

    static inline void mov8(Assembly& as, ASMVal dst, ASMVal src) {
        if (dst == src) return;
        if (src.kind == ASMVal::IMM) {
            if (src.imm == 0) return xor8(as, dst, dst, dst);

            unary_prefix(as, BYTE, dst);
            as.code.write<u8>(0xB0 + (dst.gp & 0b111));
            as.code.write<i8>(src.imm);
        }
        else binaryop(as, BYTE, Opcode::from(0x88), dst, src);
    }

    static inline void mov16(Assembly& as, ASMVal dst, ASMVal src) {
        if (dst == src) return;
        if (src.kind == ASMVal::IMM) {
            if (src.imm == 0) return xor16(as, dst, dst, dst);
            unary_prefix(as, WORD, dst);
            as.code.write<u8>(0xB8 + (dst.gp & 0b111));
            as.code.writeLE<i16>(src.imm);
        }
        else binaryop(as, WORD, Opcode::from(0x88), dst, src);
    }

    static inline void mov32(Assembly& as, ASMVal dst, ASMVal src) {
        if (dst == src) return;
        if (src.kind == ASMVal::IMM) {
            if (src.imm == 0) return xor32(as, dst, dst, dst);

            unary_prefix(as, DWORD, dst);
            as.code.write<u8>(0xB8 + (dst.gp & 0b111));
            as.code.writeLE<i32>(src.imm);
        }
        else binaryop(as, DWORD, Opcode::from(0x88), dst, src);
    }

    static inline void mov64(Assembly& as, ASMVal dst, ASMVal src) {
        if (dst == src) return;
        if (src.kind == ASMVal::IMM) {
            if (src.imm == 0) return xor64(as, dst, dst, dst);

            if (src.imm < 0) {
                binary_prefix(as, QWORD, dst, src); // QWORD because we want sign extension.
                as.code.write<u8>(0xc7);
                modrm(as, dst, GP(RAX)); // RAX because the immediate means there's no actual register there.
                as.code.writeLE<i32>(src.imm);
            } else {
                unary_prefix(as, DWORD, dst); // DWORD for shorter encoding size, since zero-extension doesn't affect the value.
                as.code.write<u8>(0xB8 + (dst.gp & 0b111));
                as.code.writeLE<i32>(src.imm);
            }
        }
        else binaryop(as, QWORD, Opcode::from(0x88), dst, src);
    }

    static inline ASMVal emitF32Constant(Assembly& as, ASMVal src) {
        Symbol label = as.symtab.anon();
        as.def(DATA_SECTION, DEF_LOCAL, label);
        as.data.writeLE<i32>(*(i32*)&src.f32);
        return Data(label);
    }

    static inline ASMVal emitF64Constant(Assembly& as, ASMVal src) {
        Symbol label = as.symtab.anon();
        as.def(DATA_SECTION, DEF_LOCAL, label);
        as.data.writeLE<i64>(*(i64*)&src.f64);
        return Data(label);
    }

    static inline void fmov32(Assembly& as, ASMVal dst, ASMVal src) {
        if (src.kind == ASMVal::F32) {
            if (bits_equal(src.f32, 0.0f))
                return vexbinaryop(as, VexPrefix66, false, Opcode::from(0xef), dst, dst, dst, true, TwoByteOpcode);
            fld32(as, dst, emitF32Constant(as, src));
        }
        else vexbinaryop(as, VexPrefixF3, false, Opcode::from(0x10), dst, dst, src, false, TwoByteOpcode);
    }

    static inline void fmov64(Assembly& as, ASMVal dst, ASMVal src) {
        if (src.kind == ASMVal::F64) {
            if (bits_equal(src.f64, 0.0))
                return vexbinaryop(as, VexPrefix66, false, Opcode::from(0xef), dst, dst, dst, true, TwoByteOpcode);
            fld64(as, dst, emitF64Constant(as, src));
        }
        else vexbinaryop(as, VexPrefixF2, false, Opcode::from(0x10), dst, dst, src, false, TwoByteOpcode);
    }

    static inline void lds8(Assembly& as, ASMVal dst, ASMVal src) {
        binaryop(as, QWORD, Opcode::literal(0x0f, 0xbe), dst, src);
    }

    static inline void lds16(Assembly& as, ASMVal dst, ASMVal src) {
        binaryop(as, QWORD,Opcode::literal(0x0f, 0xbf), dst, src);
    }

    static inline void lds32(Assembly& as, ASMVal dst, ASMVal src) {
        binaryop(as, QWORD, Opcode::literal(0x63), dst, src);
    }

    static inline void ldz8(Assembly& as, ASMVal dst, ASMVal src) {
        binaryop(as, QWORD, Opcode::literal(0x0f, 0xb6), dst, src);
    }

    static inline void ldz16(Assembly& as, ASMVal dst, ASMVal src) {
        binaryop(as, QWORD, Opcode::literal(0x0f, 0xb7), dst, src);
    }

    static inline void ldz32(Assembly& as, ASMVal dst, ASMVal src) {
        binaryop(as, DWORD, Opcode::from(0x88), dst, src);
    }

    static inline void ld64(Assembly& as, ASMVal dst, ASMVal src) {
        binaryop(as, QWORD, Opcode::from(0x88), dst, src);
    }

    static inline void st8(Assembly& as, ASMVal dst, ASMVal src) {
        if (src.kind == ASMVal::IMM) {
            unaryop(as, BYTE, Opcode::litExt(0xC6, 0x00), dst);
            as.code.write<i8>(src.imm);
        }
        else binaryop(as, BYTE, Opcode::from(0x88), dst, src);
    }

    static inline void st16(Assembly& as, ASMVal dst, ASMVal src) {
        if (src.kind == ASMVal::IMM) {
            unaryop(as, WORD, Opcode::litExt(0xC7, 0x00), dst);
            as.code.writeLE<i16>(src.imm);
        }
        else binaryop(as, WORD, Opcode::from(0x88), dst, src);
    }

    static inline void st32(Assembly& as, ASMVal dst, ASMVal src) {
        if (src.kind == ASMVal::IMM) {
            unaryop(as, DWORD, Opcode::litExt(0xC7, 0x00), dst);
            as.code.writeLE<i32>(src.imm);
        }
        else binaryop(as, DWORD, Opcode::from(0x88), dst, src);
    }

    static inline void st64(Assembly& as, ASMVal dst, ASMVal src) {
        if (src.kind == ASMVal::IMM) {
            unaryop(as, QWORD, Opcode::litExt(0xC7, 0x00), dst);
            as.code.writeLE<i32>(src.imm);
        }
        else binaryop(as, QWORD, Opcode::from(0x88), dst, src);
    }

    static inline void fld32(Assembly& as, ASMVal dst, ASMVal src) {
        vexunaryop(as, VexPrefixF3, false, Opcode::from(0x10), dst, src, TwoByteOpcode);
    }

    static inline void fld64(Assembly& as, ASMVal dst, ASMVal src) {
        vexunaryop(as, VexPrefixF2, false, Opcode::from(0x10), dst, src, TwoByteOpcode);
    }

    static inline void fst32(Assembly& as, ASMVal dst, ASMVal src) {
        if (src.kind == ASMVal::F64) {
            fmov64(as, FP(XMM0), src);
            src = FP(XMM0);
        }
        vexunaryop(as, VexPrefixF3, false, Opcode::from(0x11), dst, src, TwoByteOpcode);
    }

    static inline void fst64(Assembly& as, ASMVal dst, ASMVal src) {
        if (src.kind == ASMVal::F64) {
            fmov64(as, FP(XMM0), src);
            src = FP(XMM0);
        }
        vexunaryop(as, VexPrefixF2, false, Opcode::from(0x11), dst, src, TwoByteOpcode);
    }

    static inline void ldis8(Assembly& as, ASMVal dst, ASMVal src, ASMVal idx) {
        mreg base = src.base, index = idx.gp;
        i64 offset = src.offset;
        if (src.memkind != ASMVal::REG_OFFSET) {
            la(as, dst, src);
            base = dst.gp;
            offset = 0;
        }
        index_prefix(as, QWORD, dst.gp, base, index, BYTE, offset);
        as.code.write<u8>(0x0f);
        as.code.write<u8>(0xbe);
        index_args(as, dst.gp, base, index, BYTE, offset);
    }

    static inline void ldis16(Assembly& as, ASMVal dst, ASMVal src, ASMVal idx) {
        mreg base = src.base, index = idx.gp;
        i64 offset = src.offset;
        if (src.memkind != ASMVal::REG_OFFSET) {
            la(as, dst, src);
            base = dst.gp;
            offset = 0;
        }
        index_prefix(as, QWORD, dst.gp, base, index, BYTE, offset);
        as.code.write<u8>(0x0f);
        as.code.write<u8>(0xbf);
        index_args(as, dst.gp, base, index, WORD, offset);
    }

    static inline void ldis32(Assembly& as, ASMVal dst, ASMVal src, ASMVal idx) {
        mreg base = src.base, index = idx.gp;
        i64 offset = src.offset;
        if (src.memkind != ASMVal::REG_OFFSET) {
            la(as, dst, src);
            base = dst.gp;
            offset = 0;
        }
        index_prefix(as, QWORD, dst.gp, base, index, BYTE, offset);
        as.code.write<u8>(0x63);
        index_args(as, dst.gp, base, index, DWORD, offset);
    }

    static inline void ldiz8(Assembly& as, ASMVal dst, ASMVal src, ASMVal idx) {
        mreg base = src.base, index = idx.gp;
        i64 offset = src.offset;
        if (src.memkind != ASMVal::REG_OFFSET) {
            la(as, dst, src);
            base = dst.gp;
            offset = 0;
        }
        index_prefix(as, QWORD, dst.gp, base, index, BYTE, offset);
        as.code.write<u8>(0x0f);
        as.code.write<u8>(0xb6);
        index_args(as, dst.gp, base, index, BYTE, offset);
    }

    static inline void ldiz16(Assembly& as, ASMVal dst, ASMVal src, ASMVal idx) {
        mreg base = src.base, index = idx.gp;
        i64 offset = src.offset;
        if (src.memkind != ASMVal::REG_OFFSET) {
            la(as, dst, src);
            base = dst.gp;
            offset = 0;
        }
        index_prefix(as, QWORD, dst.gp, base, index, BYTE, offset);
        as.code.write<u8>(0x0f);
        as.code.write<u8>(0xb7);
        index_args(as, dst.gp, base, index, WORD, offset);
    }

    static inline void ldiz32(Assembly& as, ASMVal dst, ASMVal src, ASMVal idx) {
        mreg base = src.base, index = idx.gp;
        i64 offset = src.offset;
        if (src.memkind != ASMVal::REG_OFFSET) {
            la(as, dst, src);
            base = dst.gp;
            offset = 0;
        }
        index_prefix(as, QWORD, dst.gp, base, index, BYTE, offset);
        as.code.write<u8>(0x8b);
        index_args(as, dst.gp, base, index, DWORD, offset);
    }

    static inline void ldi64(Assembly& as, ASMVal dst, ASMVal src, ASMVal idx) {
        mreg base = src.base, index = idx.gp;
        i64 offset = src.offset;
        if (src.memkind != ASMVal::REG_OFFSET) {
            la(as, dst, src);
            base = dst.gp;
            offset = 0;
        }
        index_prefix(as, QWORD, dst.gp, base, index, BYTE, offset);
        as.code.write<u8>(0x8b);
        index_args(as, dst.gp, base, index, QWORD, offset);
    }

    static inline void lai8(Assembly& as, ASMVal dst, ASMVal src, ASMVal idx) {
        mreg base = src.base, index = idx.gp;
        i64 offset = src.offset;
        if (src.memkind != ASMVal::REG_OFFSET) {
            la(as, dst, src);
            base = dst.gp;
            offset = 0;
        }
        index_prefix(as, QWORD, dst.gp, base, index, BYTE, offset);
        as.code.write<u8>(0x8d);
        index_args(as, dst.gp, base, index, BYTE, offset);
    }

    static inline void lai16(Assembly& as, ASMVal dst, ASMVal src, ASMVal idx) {
        mreg base = src.base, index = idx.gp;
        i64 offset = src.offset;
        if (src.memkind != ASMVal::REG_OFFSET) {
            la(as, dst, src);
            base = dst.gp;
            offset = 0;
        }
        index_prefix(as, QWORD, dst.gp, base, index, WORD, offset);
        as.code.write<u8>(0x8d);
        index_args(as, dst.gp, base, index, WORD, offset);
    }

    static inline void lai32(Assembly& as, ASMVal dst, ASMVal src, ASMVal idx) {
        mreg base = src.base, index = idx.gp;
        i64 offset = src.offset;
        if (src.memkind != ASMVal::REG_OFFSET) {
            la(as, dst, src);
            base = dst.gp;
            offset = 0;
        }
        index_prefix(as, QWORD, dst.gp, base, index, DWORD, offset);
        as.code.write<u8>(0x8d);
        index_args(as, dst.gp, base, index, DWORD, offset);
    }

    static inline void lai64(Assembly& as, ASMVal dst, ASMVal src, ASMVal idx) {
        mreg base = src.base, index = idx.gp;
        i64 offset = src.offset;
        if (src.memkind != ASMVal::REG_OFFSET) {
            la(as, dst, src);
            base = dst.gp;
            offset = 0;
        }
        index_prefix(as, QWORD, dst.gp, base, index, QWORD, offset);
        as.code.write<u8>(0x8d);
        index_args(as, dst.gp, base, index, QWORD, offset);
    }

    static inline void sti8(Assembly& as, ASMVal dst, ASMVal src, ASMVal idx) {
        mreg base = dst.base, index = idx.gp;
        i64 offset = dst.offset;
        if (dst.memkind != ASMVal::REG_OFFSET) {
            la(as, GP(RAX), dst);
            base = RAX;
            offset = 0;
        }
        mreg srcreg = src.kind == ASMVal::IMM ? RAX : src.gp;
        index_prefix(as, BYTE, srcreg, base, index, BYTE, offset);
        as.code.write<u8>(src.kind == ASMVal::IMM ? 0xc6 : 0x88);
        index_args(as, srcreg, base, index, BYTE, offset);
        if (src.kind == ASMVal::IMM)
            as.code.write<i8>(src.imm);
    }

    static inline void sti16(Assembly& as, ASMVal dst, ASMVal src, ASMVal idx) {
        mreg base = dst.base, index = idx.gp;
        i64 offset = dst.offset;
        if (dst.memkind != ASMVal::REG_OFFSET) {
            la(as, GP(RAX), dst);
            base = RAX;
            offset = 0;
        }
        mreg srcreg = src.kind == ASMVal::IMM ? RAX : src.gp;
        index_prefix(as, WORD, srcreg, base, index, WORD, offset);
        as.code.write<u8>(src.kind == ASMVal::IMM ? 0xc7 : 0x89);
        index_args(as, srcreg, base, index, WORD, offset);
        if (src.kind == ASMVal::IMM)
            as.code.write<i16>(src.imm);
    }

    static inline void sti32(Assembly& as, ASMVal dst, ASMVal src, ASMVal idx) {
        mreg base = dst.base, index = idx.gp;
        i64 offset = dst.offset;
        if (dst.memkind != ASMVal::REG_OFFSET) {
            la(as, GP(RAX), dst);
            base = RAX;
            offset = 0;
        }
        mreg srcreg = src.kind == ASMVal::IMM ? RAX : src.gp;
        index_prefix(as, DWORD, srcreg, base, index, DWORD, offset);
        as.code.write<u8>(src.kind == ASMVal::IMM ? 0xc7 : 0x89);
        index_args(as, srcreg, base, index, DWORD, offset);
        if (src.kind == ASMVal::IMM)
            as.code.write<i32>(src.imm);
    }

    static inline void sti64(Assembly& as, ASMVal dst, ASMVal src, ASMVal idx) {
        mreg base = dst.base, index = idx.gp;
        i64 offset = dst.offset;
        if (dst.memkind != ASMVal::REG_OFFSET) {
            la(as, GP(RAX), dst);
            base = RAX;
            offset = 0;
        }
        mreg srcreg = src.kind == ASMVal::IMM ? RAX : src.gp;
        index_prefix(as, QWORD, srcreg, base, index, QWORD, offset);
        as.code.write<u8>(src.kind == ASMVal::IMM ? 0xc7 : 0x89);
        index_args(as, srcreg, base, index, QWORD, offset);
        if (src.kind == ASMVal::IMM)
            as.code.write<i32>(src.imm);
    }

    static inline void fldi32(Assembly& as, ASMVal dst, ASMVal src, ASMVal idx) {
        mreg base = src.base, index = idx.gp;
        i64 offset = src.offset;
        if (src.memkind != ASMVal::REG_OFFSET) {
            la(as, dst, src);
            base = dst.gp;
            offset = 0;
        }
        vexunaryprefix(as, VexPrefixF3, index >= R8, false, dst, src, TwoByteOpcode);
        as.code.write<u8>(0x10);
        index_args(as, dst.fp - XMM0, base, index, DWORD, offset);
    }

    static inline void fldi64(Assembly& as, ASMVal dst, ASMVal src, ASMVal idx) {
        mreg base = src.base, index = idx.gp;
        i64 offset = src.offset;
        if (src.memkind != ASMVal::REG_OFFSET) {
            la(as, dst, src);
            base = dst.gp;
            offset = 0;
        }
        vexunaryprefix(as, VexPrefixF2, index >= R8, false, dst, src, TwoByteOpcode);
        as.code.write<u8>(0x10);
        index_args(as, dst.fp - XMM0, base, index, QWORD, offset);
    }

    static inline void fsti32(Assembly& as, ASMVal dst, ASMVal src, ASMVal idx) {
        mreg base = dst.base, index = idx.gp;
        i64 offset = dst.offset;
        if (dst.memkind != ASMVal::REG_OFFSET) {
            la(as, GP(RAX), dst);
            base = RAX;
            offset = 0;
        }
        if (src.kind == ASMVal::F32) {
            fmov32(as, FP(XMM0), dst);
            src = FP(XMM0);
        }
        vexunaryprefix(as, VexPrefixF3, index >= R8, false, dst, src, TwoByteOpcode);
        as.code.write<u8>(0x11);
        index_args(as, src.fp - XMM0, base, index, DWORD, offset);
    }

    static inline void fsti64(Assembly& as, ASMVal dst, ASMVal src, ASMVal idx) {
        mreg base = dst.base, index = idx.gp;
        i64 offset = dst.offset;
        if (dst.memkind != ASMVal::REG_OFFSET) {
            la(as, GP(RAX), dst);
            base = RAX;
            offset = 0;
        }
        if (src.kind == ASMVal::F64) {
            fmov64(as, FP(XMM0), dst);
            src = FP(XMM0);
        }
        vexunaryprefix(as, VexPrefixF2, index >= R8, false, dst, src, TwoByteOpcode);
        as.code.write<u8>(0x11);
        index_args(as, src.fp - XMM0, base, index, QWORD, offset);
    }

    static inline void la(Assembly& as, ASMVal dst, ASMVal src) {
        binaryop(as, QWORD, Opcode::literal(0x8d), dst, src);
    }

    static inline void lc(Assembly& as, ASMVal dst, ASMVal src) {
        i64 value = src.imm64;
        if (!(value & 0xffffffff00000000ull)) {
            if (value & 0x80000000ull) {
                // Significant unsigned value, so use zero-extending 32-bit mov.
                unary_prefix(as, DWORD, dst);
                as.code.write<u8>(0xB8 + (dst.gp & 0b111));
                as.code.writeLE<u32>(src.imm64);
            } else
                mov32(as, dst, Imm(src.imm64)); // This could have been a 32-bit imm.
            return;
        }

        unary_prefix(as, QWORD, dst);
        as.code.write<u8>(0xB8 + (dst.gp & 0b111));
        as.code.writeLE<i64>(src.imm64);
    }

    // Labels

    static inline void global(Assembly& as, Symbol sym) {
        as.def(CODE_SECTION, DEF_GLOBAL, sym);
    }

    static inline void local(Assembly& as, Symbol sym) {
        as.def(CODE_SECTION, DEF_LOCAL, sym);
    }

    // Jumps

    static inline void br(Assembly& as, ASMVal dst) {
        if (dst.kind == ASMVal::GP) unaryop(as, DWORD, Opcode::litExt(0xff, 0x04), dst);
        else {
            as.code.write<u8>(0xe9);
            as.code.write<i32>(0);
            assert(dst.memkind == ASMVal::LOCAL_LABEL || dst.memkind == ASMVal::FUNC_LABEL);
            as.ref(CODE_SECTION, dst.memkind == ASMVal::LOCAL_LABEL ? DEF_LOCAL : DEF_GLOBAL, Reloc::REL32_LE, dst.sym);
        }
    }

    static inline void jcc(Assembly& as, Condition cc, ASMVal dst) {
        as.code.write<u8>(0x0f);
        as.code.write<u8>(0x80 + CCodes[cc]);
        as.code.writeLE<i32>(0);
        assert(dst.memkind == ASMVal::LOCAL_LABEL || dst.memkind == ASMVal::FUNC_LABEL);
        as.ref(CODE_SECTION, dst.memkind == ASMVal::LOCAL_LABEL ? DEF_LOCAL : DEF_GLOBAL, Reloc::REL32_LE, dst.sym);
    }

    static inline void jcc(Assembly& as, FloatCondition cc, ASMVal dst) {
        as.code.write<u8>(0x0f);
        as.code.write<u8>(0x80 + CCodes[cc]);
        as.code.writeLE<i32>(0);
        assert(dst.memkind == ASMVal::LOCAL_LABEL || dst.memkind == ASMVal::FUNC_LABEL);
        as.ref(CODE_SECTION, dst.memkind == ASMVal::LOCAL_LABEL ? DEF_LOCAL : DEF_GLOBAL, Reloc::REL32_LE, dst.sym);
    }

    static inline void brz(Assembly& as, ASMVal dst, ASMVal cond) {
        test64(as, cond, cond);
        jcc(as, COND_EQ, dst);
    }

    static inline void brnz(Assembly& as, ASMVal dst, ASMVal cond) {
        test64(as, cond, cond);
        jcc(as, COND_NE, dst);
    }

    static inline void brcc8(Assembly& as, Condition cc, ASMVal dst, ASMVal a, ASMVal b) {
        if (a.kind == ASMVal::IMM) {
            swap(a, b);
            cc = commute(cc);
        }
        if (cc == COND_TEST_ZERO || cc == COND_TEST_NONZERO)
            test8(as, a, b);
        else
            cmp8(as, a, b);
        jcc(as, cc, dst);
    }

    static inline void brcc16(Assembly& as, Condition cc, ASMVal dst, ASMVal a, ASMVal b) {
        if (a.kind == ASMVal::IMM) {
            swap(a, b);
            cc = commute(cc);
        }
        if (cc == COND_TEST_ZERO || cc == COND_TEST_NONZERO)
            test16(as, a, b);
        else
            cmp16(as, a, b);
        jcc(as, cc, dst);
    }

    static inline void brcc32(Assembly& as, Condition cc, ASMVal dst, ASMVal a, ASMVal b) {
        if (a.kind == ASMVal::IMM) {
            swap(a, b);
            cc = commute(cc);
        }
        if (cc == COND_TEST_ZERO || cc == COND_TEST_NONZERO)
            test32(as, a, b);
        else
            cmp32(as, a, b);
        jcc(as, cc, dst);
    }

    static inline void brcc64(Assembly& as, Condition cc, ASMVal dst, ASMVal a, ASMVal b) {
        if (a.kind == ASMVal::IMM) {
            swap(a, b);
            cc = commute(cc);
        }
        if (cc == COND_TEST_ZERO || cc == COND_TEST_NONZERO)
            test64(as, a, b);
        else
            cmp64(as, a, b);
        jcc(as, cc, dst);
    }

    static inline void fbrcc32(Assembly& as, FloatCondition cc, ASMVal dst, ASMVal a, ASMVal b) {
        fcmp32(as, a, b);
        jcc(as, cc, dst);
    }

    static inline void fbrcc64(Assembly& as, FloatCondition cc, ASMVal dst, ASMVal a, ASMVal b) {
        fcmp64(as, a, b);
        jcc(as, cc, dst);
    }

    // Functions

    static void enter(Assembly& as) {
        push64(as, GP(RBP));
        mov64(as, GP(RBP), GP(RSP));
    }

    static void stack(Assembly& as, ASMVal dst) {
        sub64(as, GP(RSP), GP(RSP), dst);
    }

    static void alloca(Assembly& as, ASMVal dst, ASMVal src) {
        stack(as, src);
        mov64(as, dst, GP(RSP));
    }

    static void unstack(Assembly& as, ASMVal dst) {
        add64(as, GP(RSP), GP(RSP), dst);
    }

    static void leave(Assembly& as) {
        mov64(as, GP(RSP), GP(RBP));
        pop64(as, GP(RBP));
    }

    static inline void call(Assembly& as, ASMVal dst) {
        if (dst.kind == ASMVal::GP) unaryop(as, DWORD, Opcode::litExt(0xff, 0x02), dst);
        else {
            as.code.write<u8>(0xe8);
            as.code.write<i32>(0);
            assert(dst.memkind == ASMVal::LOCAL_LABEL || dst.memkind == ASMVal::FUNC_LABEL);
            as.ref(CODE_SECTION, dst.memkind == ASMVal::LOCAL_LABEL ? DEF_LOCAL : DEF_GLOBAL, Reloc::REL32_LE, dst.sym);
        }
    }

    static inline void ret(Assembly& as) {
        as.code.write<i8>(0xc3);
    }

    static inline void trap(Assembly& as) {
        as.code.write<i8>(0xcc);
    }

    // Conversions

    static inline void sxt8(Assembly& as, ASMVal dst, ASMVal src) {
        if (src.kind == ASMVal::IMM) {
            mov8(as, dst, src);
            src = dst;
        }
        binary_prefix(as, QWORD, dst, src); // QWORD since that's the size of the destination.
        as.code.write<u8>(0x0f);
        as.code.write<u8>(0xbe);
        binary_args(as, BYTE, Opcode::literal(0x0f, 0xbe), dst, src);
    }

    static inline void sxt16(Assembly& as, ASMVal dst, ASMVal src) {
        if (src.kind == ASMVal::IMM) {
            mov16(as, dst, src);
            src = dst;
        }
        binary_prefix(as, QWORD, dst, src); // QWORD since that's the size of the destination.
        as.code.write<u8>(0x0f);
        as.code.write<u8>(0xbf);
        binary_args(as, WORD, Opcode::literal(0x0f, 0xbf), dst, src);
    }

    static inline void sxt32(Assembly& as, ASMVal dst, ASMVal src) {
        binary_prefix(as, QWORD, dst, src); // QWORD since that's the size of the destination.
        as.code.write<u8>(0x63);
        binary_args(as, DWORD, Opcode::literal(0x63), dst, src);
    }

    static inline void zxt8(Assembly& as, ASMVal dst, ASMVal src) {
        if (src.kind == ASMVal::IMM) {
            mov8(as, dst, src);
            src = dst;
        }
        binary_prefix(as, QWORD, dst, src); // QWORD since that's the size of the destination.
        as.code.write<u8>(0x0f);
        as.code.write<u8>(0xb6);
        binary_args(as, BYTE, Opcode::literal(0x0f, 0xb6), dst, src);
    }

    static inline void zxt16(Assembly& as, ASMVal dst, ASMVal src) {
        if (src.kind == ASMVal::IMM) {
            mov16(as, dst, src);
            src = dst;
        }
        binary_prefix(as, QWORD, dst, src); // QWORD since that's the size of the destination.
        as.code.write<u8>(0x0f);
        as.code.write<u8>(0xb7);
        binary_args(as, WORD, Opcode::literal(0x0f, 0xb7), dst, src);
    }

    static inline void zxt32(Assembly& as, ASMVal dst, ASMVal src) {
        mov32(as, dst, src);
    }

    static inline void i8tof32(Assembly& as, ASMVal dst, ASMVal src) {
        sxt8(as, GP(RAX), src);
        vexbinaryop(as, VexPrefixF3, false, Opcode::from(0x2a), dst, dst, GP(RAX), false, TwoByteOpcode);
    }

    static inline void i16tof32(Assembly& as, ASMVal dst, ASMVal src) {
        sxt16(as, GP(RAX), src);
        vexbinaryop(as, VexPrefixF3, false, Opcode::from(0x2a), dst, dst, GP(RAX), false, TwoByteOpcode);
    }

    static inline void i32tof32(Assembly& as, ASMVal dst, ASMVal src) {
        vexbinaryop(as, VexPrefixF3, false, Opcode::from(0x2a), dst, dst, src, false, TwoByteOpcode);
    }

    static inline void i64tof32(Assembly& as, ASMVal dst, ASMVal src) {
        vexbinaryop(as, VexPrefixF3, true, Opcode::from(0x2a), dst, dst, src, false, TwoByteOpcode);
    }

    static inline void i8tof64(Assembly& as, ASMVal dst, ASMVal src) {
        sxt8(as, GP(RAX), src);
        vexbinaryop(as, VexPrefixF2, false, Opcode::from(0x2a), dst, dst, GP(RAX), false, TwoByteOpcode);
    }

    static inline void i16tof64(Assembly& as, ASMVal dst, ASMVal src) {
        sxt16(as, GP(RAX), src);
        vexbinaryop(as, VexPrefixF2, false, Opcode::from(0x2a), dst, dst, GP(RAX), false, TwoByteOpcode);
    }

    static inline void i32tof64(Assembly& as, ASMVal dst, ASMVal src) {
        vexbinaryop(as, VexPrefixF2, false, Opcode::from(0x2a), dst, dst, src, false, TwoByteOpcode);
    }

    static inline void i64tof64(Assembly& as, ASMVal dst, ASMVal src) {
        vexbinaryop(as, VexPrefixF2, true, Opcode::from(0x2a), dst, dst, src, false, TwoByteOpcode);
    }

    static inline void f32toi8(Assembly& as, ASMVal dst, ASMVal src) {
        vexunaryop(as, VexPrefixF3, false, Opcode::from(0x2c), dst, src, TwoByteOpcode);
    }

    static inline void f32toi16(Assembly& as, ASMVal dst, ASMVal src) {
        vexunaryop(as, VexPrefixF3, false, Opcode::from(0x2c), dst, src, TwoByteOpcode);
    }

    static inline void f32toi32(Assembly& as, ASMVal dst, ASMVal src) {
        vexunaryop(as, VexPrefixF3, false, Opcode::from(0x2c), dst, src, TwoByteOpcode);
    }

    static inline void f32toi64(Assembly& as, ASMVal dst, ASMVal src) {
        vexunaryop(as, VexPrefixF3, true, Opcode::from(0x2c), dst, src, TwoByteOpcode);
    }

    static inline void f64toi8(Assembly& as, ASMVal dst, ASMVal src) {
        vexunaryop(as, VexPrefixF2, false, Opcode::from(0x2c), dst, src, TwoByteOpcode);
    }

    static inline void f64toi16(Assembly& as, ASMVal dst, ASMVal src) {
        vexunaryop(as, VexPrefixF2, false, Opcode::from(0x2c), dst, src, TwoByteOpcode);
    }

    static inline void f64toi32(Assembly& as, ASMVal dst, ASMVal src) {
        vexunaryop(as, VexPrefixF2, false, Opcode::from(0x2c), dst, src, TwoByteOpcode);
    }

    static inline void f64toi64(Assembly& as, ASMVal dst, ASMVal src) {
        vexunaryop(as, VexPrefixF2, true, Opcode::from(0x2c), dst, src, TwoByteOpcode);
    }

    static inline void f32tof64(Assembly& as, ASMVal dst, ASMVal src) {
        vexbinaryop(as, VexPrefixF3, false, Opcode::from(0x5a), dst, dst, src, false, TwoByteOpcode);
    }

    static inline void f64tof32(Assembly& as, ASMVal dst, ASMVal src) {
        vexbinaryop(as, VexPrefixF2, false, Opcode::from(0x5a), dst, dst, src, false, TwoByteOpcode);
    }

    static void f32frombits(Assembly& as, ASMVal dst, ASMVal src) {
        vexunaryop(as, VexPrefix66, false, Opcode::from(0x6e), dst, src, TwoByteOpcode);
    }

    static void f64frombits(Assembly& as, ASMVal dst, ASMVal src) {
        vexunaryop(as, VexPrefix66, true, Opcode::from(0x6e), dst, src, TwoByteOpcode);
    }

    static void f32tobits(Assembly& as, ASMVal dst, ASMVal src) {
        vexunaryop(as, VexPrefix66, false, Opcode::from(0x7e), src, dst, TwoByteOpcode);
    }

    static void f64tobits(Assembly& as, ASMVal dst, ASMVal src) {
        vexunaryop(as, VexPrefix66, true, Opcode::from(0x7e), src, dst, TwoByteOpcode);
    }

    // Memory

    static inline void mpush16(Assembly& as, ASMVal src) {
        unaryop(as, WORD, Opcode::litExt(0xff, 0x06), src);
    }

    static inline void mpush64(Assembly& as, ASMVal src) {
        unaryop(as, DWORD, Opcode::litExt(0xff, 0x06), src);
    }

    static inline void mpop16(Assembly& as, ASMVal dst) {
        unaryop(as, WORD, Opcode::litExt(0x8f, 0x00), dst);
    }

    static inline void mpop64(Assembly& as, ASMVal dst) {
        unaryop(as, DWORD, Opcode::litExt(0x8f, 0x00), dst);
    }

    static void mcpy_fixed(Assembly& as, ASMVal dst, ASMVal src, i32 n) {
        if (dst.memkind != ASMVal::REG_OFFSET)
            la(as, GP(RDI), dst), dst = Mem(RDI, 0);
        if (src.memkind != ASMVal::REG_OFFSET)
            la(as, GP(RSI), src), src = Mem(RSI, 0);
        i32 i = 0;
        while (n >= 8) { // Use push/pop to do 64-bit memory-to-memory moves.
            ASMVal src_offset = src;
            ASMVal dst_offset = dst;
            src_offset.offset += i;
            dst_offset.offset += i;
            mpush64(as, src_offset);
            mpop64(as, dst_offset);
            i += 8;
            n -= 8;
        }
        if (n >= 4) {
            ASMVal src_offset = src;
            ASMVal dst_offset = dst;
            src_offset.offset += i;
            dst_offset.offset += i;
            ldz32(as, GP(RCX), src_offset);
            st32(as, dst_offset, GP(RCX));
            n -= 4;
            i += 4;
        }
        if (n >= 2) {
            ASMVal src_offset = src;
            ASMVal dst_offset = dst;
            src_offset.offset += i;
            dst_offset.offset += i;
            ldz16(as, GP(RCX), src_offset);
            st16(as, dst_offset, GP(RCX));
            n -= 2;
            i += 2;
        }
        if (n) {
            ASMVal src_offset = src;
            ASMVal dst_offset = dst;
            src_offset.offset += i;
            dst_offset.offset += i;
            ldz8(as, GP(RCX), src_offset);
            st8(as, dst_offset, GP(RCX));
        }
    }

    static void mcpy(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) {
        if (b.kind == ASMVal::IMM) {
            mcpy_fixed(as, dst, a, b.imm);
        }
        else {
            la(as, GP(RDI), dst);
            la(as, GP(RSI), a);
            mov64(as, GP(RCX), b);
            as.code.write<u8>(0xf3);    // rep movsb
            as.code.write<u8>(0xa4);
        }
    }

    static void mmov(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) {

    }

    static void mset(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) {

    }

    static void mcmpcc(Assembly& as, Condition cond, ASMVal dst, ASMVal a, ASMVal b, ASMVal c) {

    }

    static void mbrcc(Assembly& as, Condition cond, ASMVal dst, ASMVal a, ASMVal b, ASMVal c) {

    }
};

struct AMD64LinuxAssembler : public AMD64Assembler {
    static constexpr mreg GP_ARGS[6] = { RDI, RSI, RDX, RCX, R8, R9 };
    static constexpr mreg FP_ARGS[8] = { XMM0, XMM1, XMM2, XMM3, XMM4, XMM5, XMM6, XMM7 };
    static const TargetDesc DESC;

    static inline constexpr RegSet caller_saved_gps() {
        return RegSet(RAX, RCX, RDX, RDI, RSI, R8, R9, R10, R11);
    }

    static inline constexpr RegSet caller_saved_fps() {
        return fps();
    }

    static inline constexpr RegSet caller_saves() {
        return caller_saved_gps() | caller_saved_fps();
    }

    static inline constexpr RegSet callee_saved_gps() {
        return RegSet(RBX, RBP, RSP, R12, R13, R14, R15);
    }

    static inline constexpr RegSet callee_saved_fps() {
        return RegSet();
    }

    static inline constexpr RegSet callee_saves() {
        return callee_saved_gps() | callee_saved_fps();
    }

    static void* start_placing_parameters();
    static MaybePair<ASMVal> place_scalar_parameter(void* state, Repr scalar);
    static MaybePair<ASMVal> place_aggregate_parameter(void* state, const_slice<Repr> members);
    static void finish_placing_parameters(void* state);

    static MaybePair<ASMVal> place_scalar_return_value(void* state, Repr scalar);
    static MaybePair<ASMVal> place_aggregate_return_value(void* state, const_slice<Repr> members);
};

struct AMD64DarwinAssembler : public AMD64LinuxAssembler {};

#endif

#endif