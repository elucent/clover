#ifndef BASIL_JASMINE_ARCH_AMD64_H
#define BASIL_JASMINE_ARCH_AMD64_H

#include "jasmine/arch.h"

MODULE(jasmine)

struct AMD64Target {
    static constexpr mreg 
        RAX = 0, RCX = 1, RDX = 2, RBX = 3, RSP = 4, RBP = 5, RSI = 6, RDI = 7,
        R8 = 8, R9 = 9, R10 = 10, R11 = 11, R12 = 12, R13 = 13, R14 = 14, R15 = 15,
        XMM0 = 32, XMM1 = 33, XMM2 = 34, XMM3 = 35, XMM4 = 36, XMM5 = 37, XMM6 = 38, XMM7 = 39,
        XMM8 = 40, XMM9 = 41, XMM10 = 42, XMM11 = 43, XMM12 = 44, XMM13 = 45, XMM14 = 46, XMM15 = 47;
    static const mreg GP_REGS[14], FP_REGS[16];
    static constexpr const i8* GP_REG_NAMES[16] = {
        "rax", "rcx", "rdx", "rbx", "rsp", "rbp", "rsi", "rdi", 
        "r8", "r9", "r10", "r11", "r12", "r13", "r14", "r15"
    };
    static constexpr const i8* FP_REG_NAMES[16] = {
        "xmm0", "xmm1", "xmm2", "xmm3", "xmm4", "xmm5", "xmm6", "xmm7", 
        "xmm8", "xmm9", "xmm10", "xmm11", "xmm12", "xmm13", "xmm14", "xmm15" 
    };
    
    constexpr static mreg fp = RBP, sp = RSP;

    static inline const_slice<mreg> gps() {
        return { GP_REGS, 14 };
    }

    static inline const_slice<mreg> fps() {
        return { FP_REGS, 16 };
    }

    static inline bool is_gp(mreg r) {
        return r < 16;
    }

    static inline bool is_fp(mreg r) {
        return r >= 32 && r < 48;
    }

    static inline const_slice<i8> reg_name(mreg r) {
        if (r < XMM0) return { GP_REG_NAMES[r], cidx(GP_REG_NAMES[r], 0) };
        else return { FP_REG_NAMES[r - XMM0], cidx(FP_REG_NAMES[r - XMM0], 0) };
    }

    static inline u32 primsize(typeidx t) {
        assert(t < 0 && t >= T_REF);
        static constexpr u32 TYPE_SIZES[14] = {
            1, 2, 4, 8, // I8 - I64
            8, 8,       // IWORD, PTR
            4, 8,       // F32, F64
            1, 2, 4, 8, // U8 - U64
            8, 8        // UWORD, REF
        };
        return TYPE_SIZES[-t - 1];
    }

    static inline u32 primalign(typeidx t) {
        return primsize(t);
    }

    static inline mreg hint(const Insn& insn, typeidx t) {
        return -1;
    }

    static inline RegSet clobbers(const Insn& insn) {
        return RegSet();
    }

    static inline Size word_size() {
        return Size::BITS64;
    }

    static inline Size ptr_size() {
        return Size::BITS64;
    }

    enum AMD64Size {
        BYTE, WORD, DWORD, QWORD, SINGLE, DOUBLE
    };

    static inline bool needsREX(AMD64Size size, ASMVal v) {
        if (v.payload.kind == ASMVal::FP) return v.payload.fp >= XMM8;
        else if (v.payload.kind == ASMVal::GP) return v.payload.gp >= R8;
        else return false;
    }

    static inline bool needs8BitREX(AMD64Size size, ASMVal v) {
        return size == BYTE && v.payload.gp >= RSP;
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
        if (needsREX(size, reg)) b |= 0x04;
        if (needsREX(size, rm)) b |= 0x01;
        if (b != 0x40 || needs8BitREX(size, reg) || needs8BitREX(size, rm)) as.code.write<u8>(b);
    }

    static inline void modrm(Assembly& as, ASMVal reg, ASMVal rm) {
        u8 modb = 0b00, rmb = rm.payload.gp & 0b111, regb = 0b000;
        u8 sib = 0;
        if (reg.payload.kind == ASMVal::GP)
            regb = reg.payload.gp & 0b111;
        if (reg.payload.kind == ASMVal::GP && rm.payload.kind == ASMVal::GP)
            modb = 0b11;
        else if (rm.payload.kind == ASMVal::MEM) {
            auto mkind = rm.payload.memkind;
            if (mkind == ASMVal::REG_OFFSET) {
                auto offset = rm.payload.offset;
                rmb = rm.payload.base & 0b111;
                if (rmb == RSP) sib = RSP; // Need SIB for all RSP-relative addressing.
                if (offset == 0) { // Zero displacement.
                    modb = 0b00;
                    if (rm.payload.base == RBP) {
                        rmb = RSP & 0b111;             // RSP indicates we use SIB
                        sib = rm.payload.base & 0b111; // SIB base register holds the register we want
                    }
                }
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
        
        if (rm.payload.kind == ASMVal::MEM) {
            auto mkind = rm.payload.memkind;
            switch (mkind) {
                case ASMVal::REG_OFFSET:
                    if (modb == 0b00) {}
                    else if (modb == 0b01) as.code.write<i8>(rm.payload.offset);
                    else if (modb == 0b10) as.code.writeLE<i32>(rm.payload.offset);
                    break;
                case ASMVal::LOCAL_LABEL:
                case ASMVal::FUNC_LABEL:
                case ASMVal::DATA_LABEL:
                case ASMVal::STATIC_LABEL:
                    as.code.writeLE<i32>(0);
                    as.ref(CODE_SECTION, Reloc::REL32_LE, rm.payload.sym);
                    break;
            }
        }
    }

    struct Opcode {
        u8 base, prefix, prefix2;
        i8 ext;
        bool lit;
    
        static inline Opcode from(u8 prefix, u8 prefix2, u8 base) {
            return { base, prefix, prefix2, -1, false };
        }
    
        static inline Opcode from(u8 prefix, u8 base) {
            return { base, prefix, 0, -1, false };
        }
    
        static inline Opcode from(u8 base) {
            return { base, 0, 0, -1, false };
        }
    
        static inline Opcode literal(u8 prefix, u8 prefix2, u8 base) {
            return { base, prefix, prefix2, -1, true };
        }
    
        static inline Opcode literal(u8 opcode) {
            return { opcode, 0, 0, -1, true };
        }
    
        static inline Opcode literal(u8 prefix, u8 opcode) {
            return { opcode, prefix, 0, -1, true };
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
        if (opcode.ext < 0) modrm(as, ASMVal::gp(0), rm);
        else modrm(as, ASMVal::gp(opcode.ext), rm);
        if (rm.payload.kind == ASMVal::IMM) {
            if (rm.payload.imm < 128 && rm.payload.imm > -129) as.code.write<i8>(rm.payload.imm);
            else as.code.writeLE<i32>(rm.payload.imm);
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
        else modrm(as, ASMVal::gp(opcode.ext), reg);
        if (rm.payload.kind == ASMVal::IMM) {
            if (rm.payload.imm < 128 && rm.payload.imm > -129) as.code.write<i8>(rm.payload.imm);
            else if (size == AMD64Size::WORD) as.code.writeLE<i16>(rm.payload.imm);
            else as.code.writeLE<i32>(rm.payload.imm);
        }
    }

    static inline bool is_immediate8(i64 imm) {
        return imm < 128 && imm >= -129;
    }

    static inline void binaryop(Assembly& as, AMD64Size size, Opcode opcode, ASMVal dst, ASMVal src) {
        if (size != BYTE && !opcode.lit) 
            opcode.base ++;
        if (((src.payload.kind == ASMVal::MEM 
            || (src.payload.kind == ASMVal::IMM && is_immediate8(src.payload.imm) && size != BYTE)) && !opcode.lit))
            opcode.base += 2;

        ASMVal reg = src, rm = dst;
        if (src.payload.kind == ASMVal::MEM || src.payload.kind == ASMVal::IMM)
            swap(reg, rm);
        assert(dst.payload.kind != ASMVal::MEM && dst.payload.kind != ASMVal::IMM);

        // At this point, dst is our reg field, and src is our rm.
        binary_prefix(as, size, reg, rm);
        if (opcode.prefix) as.code.write<u8>(opcode.prefix);
        if (opcode.prefix2) as.code.write<u8>(opcode.prefix2);
        as.code.write<u8>(opcode.base);
        binary_args(as, size, opcode, reg, rm);
    }

    static inline bool is_ext_fp(ASMVal val) {
        return val.kind() == ASMVal::FP && val.payload.fp - XMM0 >= 8;
    }

    static inline void ssebinary_args(Assembly& as, AMD64Size size, Opcode opcode, ASMVal reg, ASMVal rm) {
        if (opcode.ext < 0) 
            modrm(as, ASMVal::gp(reg.payload.fp - XMM0), rm.kind() == ASMVal::FP ? ASMVal::gp(rm.payload.fp - XMM0) : rm);
        else 
            modrm(as, ASMVal::gp(opcode.ext), ASMVal::gp(reg.payload.fp - XMM0));
    }

    static inline void ssebinaryop(Assembly& as, AMD64Size size, Opcode opcode, ASMVal dst, ASMVal src) {
        if (size == SINGLE) as.code.write<u8>(0xf3);
        else if (size == DOUBLE) as.code.write<u8>(0xf2);
        ASMVal reg = dst, rm = src;
        if (dst.payload.kind == ASMVal::MEM)
            swap(reg, rm);
        binary_prefix(as, DWORD, ASMVal::gp(reg.payload.fp - XMM0), rm.kind() == ASMVal::FP ? ASMVal::gp(rm.payload.fp - XMM0) : rm);
        if (opcode.prefix) as.code.write<u8>(opcode.prefix);
        if ((dst.payload.kind == ASMVal::MEM) && !opcode.lit)
            opcode.base += 1;
        as.code.write<u8>(opcode.base);
        ssebinary_args(as, size, opcode, reg, rm);
    }

    enum VexOpcode {
        TwoByteOpcode = 1,
        ThreeByteOpcode38 = 2,
        ThreeByteOpcode3A = 3
    };

    static inline void vexbinaryop(Assembly& as, AMD64Size size, Opcode opcode, ASMVal dst, ASMVal lhs, ASMVal rhs, bool commutative, VexOpcode opclass) {
        if (lhs.kind() == ASMVal::F32) {
            fmov32(as, dst, lhs);
            lhs = dst;
        }
        if (lhs.kind() == ASMVal::F64) {
            fmov64(as, dst, lhs);
            lhs = dst;
        }
        if (rhs.kind() == ASMVal::F32) {
            fmov32(as, dst, rhs);
            rhs = dst;
        }
        if (rhs.kind() == ASMVal::F64) {
            fmov64(as, dst, rhs);
            rhs = dst;
        }

        bool needs_three_byte = is_ext_fp(rhs) || opclass != TwoByteOpcode;
        if (needs_three_byte && !is_ext_fp(lhs) && commutative) {
            swap(lhs, rhs);
            needs_three_byte = false;
        }

        if (needs_three_byte) { // Needs three-byte VEX prefix
            as.code.write<u8>(0xc4);
            u8 vex1 = 0b01100000, vex2 = 0;
            vex1 |= opclass;
            if (!is_ext_fp(dst)) vex1 |= 0b10000000; // ~R bit
            if (size == SINGLE) // Implied prefix bit.
                vex2 |= 0b00000010;
            else if (size == DOUBLE)
                vex2 |= 0b00000011;
            else if (size == WORD)
                vex2 |= 0b00000001;
            vex2 |= (~(lhs.payload.fp - XMM0) & 0b1111) << 3; // Extra operand. 
            as.code.write<u8>(vex1);
            as.code.write<u8>(vex2);
        }
        else { // Two-byte VEX prefix
            as.code.write<u8>(0xc5);
            u8 vex = 0;
            if (!is_ext_fp(dst)) vex |= 0b10000000; // ~R bit
            if (size == SINGLE) // Implied prefix bit.
                vex |= 0b00000010;
            else if (size == DOUBLE)
                vex |= 0b00000011;
            else if (size == WORD)
                vex |= 0b00000001;
            vex |= (~(lhs.payload.fp - XMM0) & 0b1111) << 3; // Extra operand. 
            as.code.write<u8>(vex);
        }
        as.code.write<u8>(opcode.base);
        modrm(as, ASMVal::gp(dst.payload.fp - XMM0 & 0b111), ASMVal::gp(rhs.payload.fp - XMM0 & 0b111));
    }

    static inline void vexunaryop(Assembly& as, AMD64Size size, Opcode opcode, ASMVal dst, ASMVal src, VexOpcode opclass) {
        if (src.kind() == ASMVal::F32) {
            fmov32(as, dst, src);
            src = dst;
        }
        if (src.kind() == ASMVal::F64) {
            fmov64(as, dst, src);
            src = dst;
        }

        bool needs_three_byte = is_ext_fp(src) || opclass != TwoByteOpcode;

        if (needs_three_byte) { // Needs three-byte VEX prefix
            as.code.write<u8>(0xc4);
            u8 vex1 = 0b01100000, vex2 = 0;
            vex1 |= opclass;
            if (!is_ext_fp(dst)) vex1 |= 0b10000000; // ~R bit
            if (size == SINGLE) // Implied prefix bit.
                vex2 |= 0b00000010;
            else if (size == DOUBLE)
                vex2 |= 0b00000011;
            else if (size == WORD)
                vex2 |= 0b00000001;
            vex2 |= 0b1111 << 3; // Encode no prefix operand.
            as.code.write<u8>(vex1);
            as.code.write<u8>(vex2);
        }
        else { // Two-byte VEX prefix
            as.code.write<u8>(0xc5);
            u8 vex = 0;
            if (!is_ext_fp(dst)) vex |= 0b10000000; // ~R bit
            if (size == SINGLE) // Implied prefix bit.
                vex |= 0b00000010;
            else if (size == DOUBLE)
                vex |= 0b00000011;
            else if (size == WORD)
                vex |= 0b00000001;
            vex |= 0b1111 << 3; // Encode no prefix operand.
            as.code.write<u8>(vex);
        }
        as.code.write<u8>(opcode.base);
        modrm(as, ASMVal::gp(dst.payload.fp - XMM0 & 0b111), ASMVal::gp(src.payload.fp - XMM0 & 0b111));
    }

    static inline void add8(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) {
        if (dst == b) swap(a, b);
        if (a.payload.kind == ASMVal::IMM) swap(a, b);
        Opcode op = b.payload.kind == ASMVal::IMM ? Opcode::withExt(0x80, 0x00) : Opcode::from(0x00);
        mov8(as, dst, a);
        binaryop(as, BYTE, op, dst, b);
    }

    static inline void add16(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) {
        if (dst == b) swap(a, b);
        if (a.payload.kind == ASMVal::IMM) swap(a, b);
        Opcode op = b.payload.kind == ASMVal::IMM ? Opcode::withExt(0x80, 0x00) : Opcode::from(0x00);
        mov16(as, dst, a);
        binaryop(as, WORD, op, dst, b);
    }

    static inline void add32(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) {
        if (dst == b) swap(a, b);
        if (a.payload.kind == ASMVal::IMM) swap(a, b);
        Opcode op = b.payload.kind == ASMVal::IMM ? Opcode::withExt(0x80, 0x00) : Opcode::from(0x00);
        mov32(as, dst, a);
        binaryop(as, DWORD, op, dst, b);
    }

    static inline void add64(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) {
        if (dst == b) swap(a, b);
        if (a.payload.kind == ASMVal::IMM) swap(a, b);
        Opcode op = b.payload.kind == ASMVal::IMM ? Opcode::withExt(0x80, 0x00) : Opcode::from(0x00);
        mov64(as, dst, a);
        binaryop(as, QWORD, op, dst, b);
    }

    static inline void sub8(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) {
        if (a == b)
            return xor8(as, dst, dst, dst);
        if (a.payload.kind == ASMVal::IMM || dst == b)
            return neg8(as, b, b), add8(as, dst, a, b);

        Opcode op = b.payload.kind == ASMVal::IMM ? Opcode::withExt(0x80, 0x05) : Opcode::from(0x28);
        mov8(as, dst, a);
        binaryop(as, BYTE, op, dst, b);
    }

    static inline void sub16(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) {
        if (a == b)
            return xor16(as, dst, dst, dst);
        if (a.payload.kind == ASMVal::IMM || dst == b)
            return neg16(as, b, b), add16(as, dst, a, b);

        Opcode op = b.payload.kind == ASMVal::IMM ? Opcode::withExt(0x80, 0x05) : Opcode::from(0x28);
        mov16(as, dst, a);
        binaryop(as, WORD, op, dst, b);
    }

    static inline void sub32(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) {
        if (a == b)
            return xor32(as, dst, dst, dst);
        if (a.payload.kind == ASMVal::IMM || dst == b)
            return neg32(as, b, b), add32(as, dst, a, b);

        Opcode op = b.payload.kind == ASMVal::IMM ? Opcode::withExt(0x80, 0x05) : Opcode::from(0x28);
        mov32(as, dst, a);
        binaryop(as, DWORD, op, dst, b);
    }

    static inline void sub64(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) {
        if (a == b)
            return xor64(as, dst, dst, dst);
        if (a.payload.kind == ASMVal::IMM || dst == b)
            return neg64(as, b, b), add64(as, dst, a, b);

        Opcode op = b.payload.kind == ASMVal::IMM ? Opcode::withExt(0x80, 0x05) : Opcode::from(0x28);
        mov64(as, dst, a);
        binaryop(as, QWORD, op, dst, b);
    }

    static inline void mul8(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) {
        if (a.payload.kind == ASMVal::IMM) swap(a, b);
        if (b.payload.kind == ASMVal::IMM) {
            binaryop(as, WORD, Opcode::literal(0x6b), a, dst); // WORD because no byte multiply exists.
            as.code.write<i8>(b.payload.imm);
            return;
        }
        if (dst == b) swap(a, b);
        mov8(as, dst, a), binaryop(as, WORD, Opcode::literal(0x0f, 0xaf), b, dst);
    }

    static inline void mul16(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) {
        if (a.payload.kind == ASMVal::IMM) swap(a, b);
        if (b.payload.kind == ASMVal::IMM) {
            binaryop(as, WORD, Opcode::literal(0x69), a, dst);
            as.code.writeLE<i16>(b.payload.imm);
            return;
        }
        if (dst == b) swap(a, b);
        mov16(as, dst, a), binaryop(as, WORD, Opcode::literal(0x0f, 0xaf), b, dst);
    }

    static inline void mul32(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) {
        if (a.payload.kind == ASMVal::IMM) swap(a, b);
        if (b.payload.kind == ASMVal::IMM) {
            binaryop(as, DWORD, Opcode::literal(0x69), a, dst);
            as.code.writeLE<i32>(b.payload.imm);
            return;
        }
        if (dst == b) swap(a, b);
        mov32(as, dst, a), binaryop(as, DWORD, Opcode::literal(0x0f, 0xaf), b, dst);
    }

    static inline void mul64(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) {
        if (a.payload.kind == ASMVal::IMM) swap(a, b);
        if (b.payload.kind == ASMVal::IMM) {
            binaryop(as, QWORD, Opcode::literal(0x69), a, dst);
            as.code.writeLE<i32>(b.payload.imm);
            return;
        }
        if (dst == b) swap(a, b);
        mov64(as, dst, a), binaryop(as, QWORD, Opcode::literal(0x0f, 0xaf), b, dst);
    }

    static inline void sdiv8(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) {
        mov8(as, ASMVal::gp(RAX), a);
        as.code.write<u8>(0x66); // cbw
        as.code.write<u8>(0x98);
        if (b.payload.kind == ASMVal::IMM) 
            mov8(as, ASMVal::gp(RDX), b), b = ASMVal::gp(RDX);
        unaryop(as, BYTE, Opcode::withExt(0xf6, 0x07), b);
        mov8(as, dst, ASMVal::gp(RAX));
    }

    static inline void sdiv16(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) {
        mov16(as, ASMVal::gp(RAX), a);
        as.code.write<u8>(0x66); // cwd
        as.code.write<u8>(0x99);
        if (b.payload.kind == ASMVal::IMM) 
            mov16(as, ASMVal::gp(RCX), b), b = ASMVal::gp(RCX);
        unaryop(as, WORD, Opcode::withExt(0xf6, 0x07), b);
        mov16(as, dst, ASMVal::gp(RAX));
    }

    static inline void sdiv32(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) {
        mov32(as, ASMVal::gp(RAX), a);
        as.code.write<u8>(0x99); // cdq
        if (b.payload.kind == ASMVal::IMM) 
            mov32(as, ASMVal::gp(RCX), b), b = ASMVal::gp(RCX);
        unaryop(as, DWORD, Opcode::withExt(0xf6, 0x07), b);
        mov32(as, dst, ASMVal::gp(RAX));
    }

    static inline void sdiv64(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) {
        mov64(as, ASMVal::gp(RAX), a);
        as.code.write<u8>(0x48); // cqo
        as.code.write<u8>(0x99);
        if (b.payload.kind == ASMVal::IMM) 
            mov64(as, ASMVal::gp(RCX), b), b = ASMVal::gp(RCX);
        unaryop(as, QWORD, Opcode::withExt(0xf6, 0x07), b);
        mov64(as, dst, ASMVal::gp(RAX));
    }

    static inline void udiv8(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) {
        if (a.payload.kind == ASMVal::IMM)
            mov16(as, ASMVal::gp(RAX), a);
        else
            zxt8(as, ASMVal::gp(RAX), a);
        if (b.payload.kind == ASMVal::IMM) 
            mov8(as, ASMVal::gp(RDX), b), b = ASMVal::gp(RDX);
        unaryop(as, BYTE, Opcode::withExt(0xf6, 0x06), b);
        mov8(as, dst, ASMVal::gp(RAX));
    }

    static inline void udiv16(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) {
        mov16(as, ASMVal::gp(RAX), a), a = ASMVal::gp(RAX);
        xor16(as, ASMVal::gp(RDX), ASMVal::gp(RDX), ASMVal::gp(RDX));
        if (b.payload.kind == ASMVal::IMM) 
            mov16(as, ASMVal::gp(RCX), b), b = ASMVal::gp(RCX);
        unaryop(as, WORD, Opcode::withExt(0xf6, 0x06), b);
        mov16(as, dst, ASMVal::gp(RAX));
    }

    static inline void udiv32(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) {
        mov32(as, ASMVal::gp(RAX), a);
        xor32(as, ASMVal::gp(RDX), ASMVal::gp(RDX), ASMVal::gp(RDX));
        if (b.payload.kind == ASMVal::IMM) 
            mov32(as, ASMVal::gp(RCX), b), b = ASMVal::gp(RCX);
        unaryop(as, DWORD, Opcode::withExt(0xf6, 0x06), b);
        mov32(as, dst, ASMVal::gp(RAX));
    }

    static inline void udiv64(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) {
        mov64(as, ASMVal::gp(RAX), a);
        xor64(as, ASMVal::gp(RDX), ASMVal::gp(RDX), ASMVal::gp(RDX));
        if (b.payload.kind == ASMVal::IMM) 
            mov64(as, ASMVal::gp(RCX), b), b = ASMVal::gp(RCX);
        unaryop(as, QWORD, Opcode::withExt(0xf6, 0x06), b);
        mov64(as, dst, ASMVal::gp(RAX));
    }

    static inline void srem8(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) {
        unreachable("TODO: implement grabbing AH");
        mov8(as, ASMVal::gp(RAX), a);
        as.code.write<u8>(0x66); // cbw
        as.code.write<u8>(0x98);
        if (b.payload.kind == ASMVal::IMM) 
            mov8(as, ASMVal::gp(RDX), b), b = ASMVal::gp(RDX);
        unaryop(as, BYTE, Opcode::withExt(0xf6, 0x07), b);
        mov8(as, dst, ASMVal::gp(RDX));
    }

    static inline void srem16(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) {
        mov16(as, ASMVal::gp(RAX), a);
        as.code.write<u8>(0x66); // cwd
        as.code.write<u8>(0x99);
        if (b.payload.kind == ASMVal::IMM) 
            mov16(as, ASMVal::gp(RCX), b), b = ASMVal::gp(RCX);
        unaryop(as, WORD, Opcode::withExt(0xf6, 0x07), b);
        mov16(as, dst, ASMVal::gp(RDX));
    }

    static inline void srem32(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) {
        mov32(as, ASMVal::gp(RAX), a);
        as.code.write<u8>(0x99); // cdq
        if (b.payload.kind == ASMVal::IMM) 
            mov32(as, ASMVal::gp(RCX), b), b = ASMVal::gp(RCX);
        unaryop(as, DWORD, Opcode::withExt(0xf6, 0x07), b);
        mov32(as, dst, ASMVal::gp(RDX));
    }

    static inline void srem64(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) {
        mov64(as, ASMVal::gp(RAX), a);
        as.code.write<u8>(0x48); // cqo
        as.code.write<u8>(0x99);
        if (b.payload.kind == ASMVal::IMM) 
            mov64(as, ASMVal::gp(RCX), b), b = ASMVal::gp(RCX);
        unaryop(as, QWORD, Opcode::withExt(0xf6, 0x07), b);
        mov64(as, dst, ASMVal::gp(RDX));
    }

    static inline void urem8(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) {
        unreachable("TODO: implement grabbing AH");
        zxt8(as, ASMVal::gp(RAX), a);
        if (b.payload.kind == ASMVal::IMM) 
            mov8(as, ASMVal::gp(RDX), b), b = ASMVal::gp(RDX);
        unaryop(as, BYTE, Opcode::withExt(0xf6, 0x06), b);
        mov8(as, dst, ASMVal::gp(RAX));
    }

    static inline void urem16(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) {
        zxt16(as, ASMVal::gp(RAX), a);
        xor16(as, ASMVal::gp(RCX), ASMVal::gp(RCX), ASMVal::gp(RCX));
        if (b.payload.kind == ASMVal::IMM) 
            mov16(as, ASMVal::gp(RCX), b), b = ASMVal::gp(RCX);
        unaryop(as, WORD, Opcode::withExt(0xf6, 0x06), b);
        mov16(as, dst, ASMVal::gp(RDX));
    }

    static inline void urem32(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) {
        zxt32(as, ASMVal::gp(RAX), a);
        xor32(as, ASMVal::gp(RCX), ASMVal::gp(RCX), ASMVal::gp(RCX));
        if (b.payload.kind == ASMVal::IMM) 
            mov32(as, ASMVal::gp(RCX), b), b = ASMVal::gp(RCX);
        unaryop(as, DWORD, Opcode::withExt(0xf6, 0x06), b);
        mov32(as, dst, ASMVal::gp(RDX));
    }

    static inline void urem64(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) {
        mov64(as, ASMVal::gp(RAX), a);
        xor64(as, ASMVal::gp(RCX), ASMVal::gp(RCX), ASMVal::gp(RCX));
        if (b.payload.kind == ASMVal::IMM) 
            mov64(as, ASMVal::gp(RCX), b), b = ASMVal::gp(RCX);
        unaryop(as, QWORD, Opcode::withExt(0xf6, 0x06), b);
        mov64(as, dst, ASMVal::gp(RDX));
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
        vexbinaryop(as, SINGLE, Opcode::from(0x58), dst, a, b, true, TwoByteOpcode);
    }

    static inline void fadd64(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) {
        vexbinaryop(as, DOUBLE, Opcode::from(0x58), dst, a, b, true, TwoByteOpcode);
    }

    static inline void fsub32(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) {
        vexbinaryop(as, SINGLE, Opcode::from(0x5c), dst, a, b, true, TwoByteOpcode);
    }

    static inline void fsub64(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) {
        vexbinaryop(as, DOUBLE, Opcode::from(0x5c), dst, a, b, true, TwoByteOpcode);
    }

    static inline void fmul32(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) {
        vexbinaryop(as, SINGLE, Opcode::from(0x59), dst, a, b, true, TwoByteOpcode);
    }

    static inline void fmul64(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) {
        vexbinaryop(as, DOUBLE, Opcode::from(0x59), dst, a, b, true, TwoByteOpcode);
    }

    static inline void fdiv32(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) {
        vexbinaryop(as, SINGLE, Opcode::from(0x5e), dst, a, b, true, TwoByteOpcode);
    }

    static inline void fdiv64(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) {
        vexbinaryop(as, DOUBLE, Opcode::from(0x5e), dst, a, b, true, TwoByteOpcode);
    }

    static inline void frem32(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) {
        fdiv32(as, dst, a, b);
        fmul32(as, dst, dst, b);
        fsub32(as, dst, a, dst);
    }

    static inline void frem64(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) {
        fdiv64(as, dst, a, b);
        fmul64(as, dst, dst, b);
        fsub64(as, dst, a, dst);
    }

    static void fsqrt32(Assembly& as, ASMVal dst, ASMVal src) {
        vexunaryop(as, SINGLE, Opcode::from(0x51), dst, src, TwoByteOpcode);
    }

    static void fsqrt64(Assembly& as, ASMVal dst, ASMVal src) {
        vexunaryop(as, DOUBLE, Opcode::from(0x51), dst, src, TwoByteOpcode);
    }

    static void fround32(Assembly& as, ASMVal dst, ASMVal src) {
        vexunaryop(as, WORD, Opcode::from(0x0a), dst, src, ThreeByteOpcode3A);
        as.code.write<u8>(0x00); // Round up or down, breaking ties using banker's rounding.
    }

    static void fround64(Assembly& as, ASMVal dst, ASMVal src) {
        vexunaryop(as, WORD, Opcode::from(0x0b), dst, src, ThreeByteOpcode3A);
        as.code.write<u8>(0x00); // Round up or down, breaking ties using banker's rounding.
    }

    static void ffloor32(Assembly& as, ASMVal dst, ASMVal src) {
        vexunaryop(as, WORD, Opcode::from(0x0a), dst, src, ThreeByteOpcode3A);
        as.code.write<u8>(0x01); // Round towards negative infinity.
    }

    static void ffloor64(Assembly& as, ASMVal dst, ASMVal src) {
        vexunaryop(as, WORD, Opcode::from(0x0b), dst, src, ThreeByteOpcode3A);
        as.code.write<u8>(0x01); // Round towards negative infinity.
    }

    static void fceil32(Assembly& as, ASMVal dst, ASMVal src) {
        vexunaryop(as, WORD, Opcode::from(0x0a), dst, src, ThreeByteOpcode3A);
        as.code.write<u8>(0x02); // Round towards positive infinity.
    }

    static void fceil64(Assembly& as, ASMVal dst, ASMVal src) {
        vexunaryop(as, WORD, Opcode::from(0x0b), dst, src, ThreeByteOpcode3A);
        as.code.write<u8>(0x02); // Round towards positive infinity.
    }

    static inline void fneg32(Assembly& as, ASMVal dst, ASMVal src) {
        fsub32(as, dst, ASMVal::f32(0), src);
    }

    static inline void fneg64(Assembly& as, ASMVal dst, ASMVal src) {
        fsub64(as, dst, ASMVal::f64(0), src);
    }

    static void fmin32(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) {
        vexbinaryop(as, SINGLE, Opcode::from(0x5d), dst, a, b, true, TwoByteOpcode);
    }

    static void fmin64(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) {
        vexbinaryop(as, DOUBLE, Opcode::from(0x5d), dst, a, b, true, TwoByteOpcode);
    }

    static void fmax32(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) {
        vexbinaryop(as, SINGLE, Opcode::from(0x5f), dst, a, b, true, TwoByteOpcode);
    }

    static void fmax64(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) {
        vexbinaryop(as, DOUBLE, Opcode::from(0x5f), dst, a, b, true, TwoByteOpcode);
    }

    static void fabs32(Assembly& as, ASMVal dst, ASMVal src) {
        vexbinaryop(as, WORD, Opcode::from(0x76), ASMVal::fp(XMM0), ASMVal::fp(XMM0), ASMVal::fp(XMM0), true, TwoByteOpcode); // pcmpeqd => all 1s
        vexbinaryop(as, WORD, Opcode::from(0x72), ASMVal::fp(XMM0), ASMVal::fp(XMM0), ASMVal::fp(XMM0), true, TwoByteOpcode); // psrld => mask
        as.code.write<u8>(1); // imm8 for shift
        vexbinaryop(as, QWORD, Opcode::from(0x54), dst, ASMVal::fp(XMM0), src, true, TwoByteOpcode); // andps
    }

    static void fabs64(Assembly& as, ASMVal dst, ASMVal src) {
        vexbinaryop(as, WORD, Opcode::from(0x29), ASMVal::fp(XMM0), ASMVal::fp(XMM0), ASMVal::fp(XMM0), true, ThreeByteOpcode38); // pcmpeqq => all 1s
        vexbinaryop(as, WORD, Opcode::from(0x73), ASMVal::fp(XMM0), ASMVal::fp(XMM0), ASMVal::fp(XMM0), true, TwoByteOpcode); // psrld => mask
        as.code.write<u8>(1); // imm8 for shift
        vexbinaryop(as, WORD, Opcode::from(0x54), dst, ASMVal::fp(XMM0), src, true, TwoByteOpcode); // andpd
    }

    // Bitwise Operations

    static inline void and8(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) {
        if (dst == b) swap(a, b);
        Opcode op = b.payload.kind == ASMVal::IMM ? Opcode::withExt(0x80, 0x04) : Opcode::from(0x20);
        mov8(as, dst, a);
        binaryop(as, BYTE, op, dst, b);
    }

    static inline void and16(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) {
        if (dst == b) swap(a, b);
        Opcode op = b.payload.kind == ASMVal::IMM ? Opcode::withExt(0x80, 0x04) : Opcode::from(0x20);
        mov16(as, dst, a);
        binaryop(as, WORD, op, dst, b);
    }

    static inline void and32(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) {
        if (dst == b) swap(a, b);
        Opcode op = b.payload.kind == ASMVal::IMM ? Opcode::withExt(0x80, 0x04) : Opcode::from(0x20);
        mov32(as, dst, a);
        binaryop(as, DWORD, op, dst, b);
    }

    static inline void and64(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) {
        if (dst == b) swap(a, b);
        Opcode op = b.payload.kind == ASMVal::IMM ? Opcode::withExt(0x80, 0x04) : Opcode::from(0x20);
        mov64(as, dst, a);
        binaryop(as, QWORD, op, dst, b);
    }

    static inline void xor8(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) {
        if (dst == b) swap(a, b);
        Opcode op = b.payload.kind == ASMVal::IMM ? Opcode::withExt(0x80, 0x06) : Opcode::from(0x30);
        mov8(as, dst, a);
        binaryop(as, BYTE, op, dst, b);
    }

    static inline void xor16(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) {
        if (dst == b) swap(a, b);
        Opcode op = b.payload.kind == ASMVal::IMM ? Opcode::withExt(0x80, 0x06) : Opcode::from(0x30);
        mov16(as, dst, a);
        binaryop(as, WORD, op, dst, b);
    }

    static inline void xor32(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) {
        if (dst == b) swap(a, b);
        Opcode op = b.payload.kind == ASMVal::IMM ? Opcode::withExt(0x80, 0x06) : Opcode::from(0x30);
        mov32(as, dst, a);
        binaryop(as, DWORD, op, dst, b);
    }

    static inline void xor64(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) {
        if (dst == b) swap(a, b);
        Opcode op = b.payload.kind == ASMVal::IMM ? Opcode::withExt(0x80, 0x06) : Opcode::from(0x30);
        mov64(as, dst, a);
        binaryop(as, QWORD, op, dst, b);
    }

    static inline void or8(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) {
        if (dst == b) swap(a, b);
        Opcode op = b.payload.kind == ASMVal::IMM ? Opcode::withExt(0x80, 0x01) : Opcode::from(0x08);
        mov8(as, dst, a);
        binaryop(as, BYTE, op, dst, b);
    }

    static inline void or16(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) {
        if (dst == b) swap(a, b);
        Opcode op = b.payload.kind == ASMVal::IMM ? Opcode::withExt(0x80, 0x01) : Opcode::from(0x08);
        mov16(as, dst, a);
        binaryop(as, WORD, op, dst, b);
    }

    static inline void or32(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) {
        if (dst == b) swap(a, b);
        Opcode op = b.payload.kind == ASMVal::IMM ? Opcode::withExt(0x80, 0x01) : Opcode::from(0x08);
        mov32(as, dst, a);
        binaryop(as, DWORD, op, dst, b);
    }

    static inline void or64(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) {
        if (dst == b) swap(a, b);
        Opcode op = b.payload.kind == ASMVal::IMM ? Opcode::withExt(0x80, 0x01) : Opcode::from(0x08);
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
        Opcode op = b.payload.kind == ASMVal::IMM ? Opcode::litExt(0xC0, 0x04) : Opcode::litExt(0xD2, 0x04);
        if (b.payload.kind != ASMVal::IMM)
            mov8(as, ASMVal::gp(RCX), b);
        mov8(as, dst, a);
        unaryop(as, BYTE, op, dst);
        if (b.payload.kind == ASMVal::IMM) 
            as.code.write<i8>(b.payload.imm);
    }

    static void shl16(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) {
        Opcode op = b.payload.kind == ASMVal::IMM ? Opcode::litExt(0xC1, 0x04) : Opcode::litExt(0xD3, 0x04);
        if (b.payload.kind != ASMVal::IMM)
            mov8(as, ASMVal::gp(RCX), b);
        mov16(as, dst, a);
        unaryop(as, WORD, op, dst);
        if (b.payload.kind == ASMVal::IMM) 
            as.code.write<i8>(b.payload.imm);
    }

    static void shl32(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) {
        Opcode op = b.payload.kind == ASMVal::IMM ? Opcode::litExt(0xC1, 0x04) : Opcode::litExt(0xD3, 0x04);
        mov8(as, ASMVal::gp(RCX), b);
        mov32(as, dst, a);
        unaryop(as, DWORD, op, dst);
        if (b.payload.kind == ASMVal::IMM) 
            as.code.write<i8>(b.payload.imm);
    }

    static void shl64(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) {
        Opcode op = b.payload.kind == ASMVal::IMM ? Opcode::litExt(0xC1, 0x04) : Opcode::litExt(0xD3, 0x04);
        if (b.payload.kind != ASMVal::IMM)
            mov8(as, ASMVal::gp(RCX), b);
        mov64(as, dst, a);
        unaryop(as, QWORD, op, dst);
        if (b.payload.kind == ASMVal::IMM) 
            as.code.write<i8>(b.payload.imm);
    }

    static void shr8(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) {
        Opcode op = b.payload.kind == ASMVal::IMM ? Opcode::litExt(0xC0, 0x05) : Opcode::litExt(0xD2, 0x05);
        if (b.payload.kind != ASMVal::IMM)
            mov8(as, ASMVal::gp(RCX), b);
        mov8(as, dst, a);
        unaryop(as, BYTE, op, dst);
        if (b.payload.kind == ASMVal::IMM) 
            as.code.write<i8>(b.payload.imm);
    }

    static void shr16(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) {
        Opcode op = b.payload.kind == ASMVal::IMM ? Opcode::litExt(0xC1, 0x05) : Opcode::litExt(0xD3, 0x05);
        if (b.payload.kind != ASMVal::IMM)
            mov8(as, ASMVal::gp(RCX), b);
        mov16(as, dst, a);
        unaryop(as, WORD, op, dst);
        if (b.payload.kind == ASMVal::IMM) 
            as.code.write<i8>(b.payload.imm);
    }

    static void shr32(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) {
        Opcode op = b.payload.kind == ASMVal::IMM ? Opcode::litExt(0xC1, 0x05) : Opcode::litExt(0xD3, 0x05);
        if (b.payload.kind != ASMVal::IMM)
            mov8(as, ASMVal::gp(RCX), b);
        mov32(as, dst, a);
        unaryop(as, DWORD, op, dst);
        if (b.payload.kind == ASMVal::IMM) 
            as.code.write<i8>(b.payload.imm);
    }

    static void shr64(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) {
        Opcode op = b.payload.kind == ASMVal::IMM ? Opcode::litExt(0xC1, 0x05) : Opcode::litExt(0xD3, 0x05);
        if (b.payload.kind != ASMVal::IMM)
            mov8(as, ASMVal::gp(RCX), b);
        mov64(as, dst, a);
        unaryop(as, QWORD, op, dst);
        if (b.payload.kind == ASMVal::IMM) 
            as.code.write<i8>(b.payload.imm);
    }

    static void sar8(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) {
        Opcode op = b.payload.kind == ASMVal::IMM ? Opcode::litExt(0xC0, 0x07) : Opcode::litExt(0xD2, 0x07);
        if (b.payload.kind != ASMVal::IMM)
            mov8(as, ASMVal::gp(RCX), b);
        mov8(as, dst, a);
        unaryop(as, BYTE, op, dst);
        if (b.payload.kind == ASMVal::IMM) 
            as.code.write<i8>(b.payload.imm);
    }

    static void sar16(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) {
        Opcode op = b.payload.kind == ASMVal::IMM ? Opcode::litExt(0xC1, 0x07) : Opcode::litExt(0xD3, 0x07);
        if (b.payload.kind != ASMVal::IMM)
            mov8(as, ASMVal::gp(RCX), b);
        mov16(as, dst, a);
        unaryop(as, WORD, op, dst);
        if (b.payload.kind == ASMVal::IMM) 
            as.code.write<i8>(b.payload.imm);
    }

    static void sar32(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) {
        Opcode op = b.payload.kind == ASMVal::IMM ? Opcode::litExt(0xC1, 0x07) : Opcode::litExt(0xD3, 0x07);
        if (b.payload.kind != ASMVal::IMM)
            mov8(as, ASMVal::gp(RCX), b);
        mov32(as, dst, a);
        unaryop(as, DWORD, op, dst);
        if (b.payload.kind == ASMVal::IMM) 
            as.code.write<i8>(b.payload.imm);
    }

    static void sar64(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) {
        Opcode op = b.payload.kind == ASMVal::IMM ? Opcode::litExt(0xC1, 0x07) : Opcode::litExt(0xD3, 0x07);
        if (b.payload.kind != ASMVal::IMM)
            mov8(as, ASMVal::gp(RCX), b);
        mov64(as, dst, a);
        unaryop(as, QWORD, op, dst);
        if (b.payload.kind == ASMVal::IMM) 
            as.code.write<i8>(b.payload.imm);
    }

    static void rol8(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) {
        Opcode op = b.payload.kind == ASMVal::IMM ? Opcode::litExt(0xC0, 0x00) : Opcode::litExt(0xD2, 0x00);
        if (b.payload.kind != ASMVal::IMM)
            mov8(as, ASMVal::gp(RCX), b);
        mov8(as, dst, a);
        unaryop(as, BYTE, op, dst);
        if (b.payload.kind == ASMVal::IMM) 
            as.code.write<i8>(b.payload.imm);
    }

    static void rol16(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) {
        Opcode op = b.payload.kind == ASMVal::IMM ? Opcode::litExt(0xC1, 0x00) : Opcode::litExt(0xD3, 0x00);
        if (b.payload.kind != ASMVal::IMM)
            mov8(as, ASMVal::gp(RCX), b);
        mov16(as, dst, a);
        unaryop(as, WORD, op, dst);
        if (b.payload.kind == ASMVal::IMM) 
            as.code.write<i8>(b.payload.imm);
    }

    static void rol32(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) {
        Opcode op = b.payload.kind == ASMVal::IMM ? Opcode::litExt(0xC1, 0x00) : Opcode::litExt(0xD3, 0x00);
        if (b.payload.kind != ASMVal::IMM)
            mov8(as, ASMVal::gp(RCX), b);
        mov32(as, dst, a);
        unaryop(as, DWORD, op, dst);
        if (b.payload.kind == ASMVal::IMM) 
            as.code.write<i8>(b.payload.imm);
    }

    static void rol64(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) {
        Opcode op = b.payload.kind == ASMVal::IMM ? Opcode::litExt(0xC1, 0x00) : Opcode::litExt(0xD3, 0x00);
        if (b.payload.kind != ASMVal::IMM)
            mov8(as, ASMVal::gp(RCX), b);
        mov64(as, dst, a);
        unaryop(as, QWORD, op, dst);
        if (b.payload.kind == ASMVal::IMM) 
            as.code.write<i8>(b.payload.imm);
    }

    static void ror8(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) {
        Opcode op = b.payload.kind == ASMVal::IMM ? Opcode::litExt(0xC0, 0x01) : Opcode::litExt(0xD2, 0x01);
        if (b.payload.kind != ASMVal::IMM)
            mov8(as, ASMVal::gp(RCX), b);
        mov8(as, dst, a);
        unaryop(as, BYTE, op, dst);
        if (b.payload.kind == ASMVal::IMM) 
            as.code.write<i8>(b.payload.imm);
    }

    static void ror16(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) {
        Opcode op = b.payload.kind == ASMVal::IMM ? Opcode::litExt(0xC1, 0x01) : Opcode::litExt(0xD3, 0x01);
        if (b.payload.kind != ASMVal::IMM)
            mov8(as, ASMVal::gp(RCX), b);
        mov16(as, dst, a);
        unaryop(as, WORD, op, dst);
        if (b.payload.kind == ASMVal::IMM) 
            as.code.write<i8>(b.payload.imm);
    }

    static void ror32(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) {
        Opcode op = b.payload.kind == ASMVal::IMM ? Opcode::litExt(0xC1, 0x01) : Opcode::litExt(0xD3, 0x01);
        if (b.payload.kind != ASMVal::IMM)
            mov8(as, ASMVal::gp(RCX), b);
        mov32(as, dst, a);
        unaryop(as, DWORD, op, dst);
        if (b.payload.kind == ASMVal::IMM) 
            as.code.write<i8>(b.payload.imm);
    }

    static void ror64(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) {
        Opcode op = b.payload.kind == ASMVal::IMM ? Opcode::litExt(0xC1, 0x01) : Opcode::litExt(0xD3, 0x01);
        if (b.payload.kind != ASMVal::IMM)
            mov8(as, ASMVal::gp(RCX), b);
        mov64(as, dst, a);
        unaryop(as, QWORD, op, dst);
        if (b.payload.kind == ASMVal::IMM) 
            as.code.write<i8>(b.payload.imm);
    }

    static void bitc8(Assembly& as, ASMVal dst, ASMVal src) {
        and16(as, dst, src, ASMVal::imm(255));
        as.code.write<u8>(0xf3); // Extra prefix byte before operand size override.
        binaryop(as, WORD, Opcode::literal(0x0f, 0xb8), dst, dst);
    }

    static void bitc16(Assembly& as, ASMVal dst, ASMVal src) {
        as.code.write<u8>(0xf3); // Extra prefix byte before operand size override.
        binaryop(as, WORD, Opcode::literal(0x0f, 0xb8), src, dst);
    }

    static void bitc32(Assembly& as, ASMVal dst, ASMVal src) {
        as.code.write<u8>(0xf3); // Extra prefix byte before operand size override.
        binaryop(as, DWORD, Opcode::literal(0x0f, 0xb8), src, dst);
    }

    static void bitc64(Assembly& as, ASMVal dst, ASMVal src) {
        as.code.write<u8>(0xf3); // Extra prefix byte before operand size override.
        binaryop(as, QWORD, Opcode::literal(0x0f, 0xb8), src, dst);
    }

    static void lzc8(Assembly& as, ASMVal dst, ASMVal src) {
        shl16(as, dst, src, ASMVal::imm(8));
        or16(as, dst, dst, ASMVal::imm(255));
        as.code.write<u8>(0xf3); // Extra prefix byte before operand size override.
        binaryop(as, WORD, Opcode::literal(0x0f, 0xbd), dst, dst);
    }

    static void lzc16(Assembly& as, ASMVal dst, ASMVal src) {
        as.code.write<u8>(0xf3); // Extra prefix byte before operand size override.
        binaryop(as, WORD, Opcode::literal(0x0f, 0xbd), src, dst);
    }

    static void lzc32(Assembly& as, ASMVal dst, ASMVal src) {
        as.code.write<u8>(0xf3); // Extra prefix byte before operand size override.
        binaryop(as, DWORD, Opcode::literal(0x0f, 0xbd), src, dst);
    }

    static void lzc64(Assembly& as, ASMVal dst, ASMVal src) {
        as.code.write<u8>(0xf3); // Extra prefix byte before operand size override.
        binaryop(as, QWORD, Opcode::literal(0x0f, 0xbd), src, dst);
    }

    static void tzc8(Assembly& as, ASMVal dst, ASMVal src) {
        or16(as, dst, src, ASMVal::imm(0xff00));
        as.code.write<u8>(0xf3); // Extra prefix byte before operand size override.
        binaryop(as, WORD, Opcode::literal(0x0f, 0xbc), dst, dst);
    }

    static void tzc16(Assembly& as, ASMVal dst, ASMVal src) {
        as.code.write<u8>(0xf3); // Extra prefix byte before operand size override.
        binaryop(as, WORD, Opcode::literal(0x0f, 0xbc), src, dst);
    }

    static void tzc32(Assembly& as, ASMVal dst, ASMVal src) {
        as.code.write<u8>(0xf3); // Extra prefix byte before operand size override.
        binaryop(as, DWORD, Opcode::literal(0x0f, 0xbc), src, dst);
    }

    static void tzc64(Assembly& as, ASMVal dst, ASMVal src) {
        as.code.write<u8>(0xf3); // Extra prefix byte before operand size override.
        binaryop(as, QWORD, Opcode::literal(0x0f, 0xbc), src, dst);
    }

    // Comparisons

    static constexpr u8 CCodes[6] = {
        0x4, // Equal
        0x5, // Not Equal
        0xc, // Less
        0xe, // Less Equal
        0xd, // Greater
        0xf, // Greater Equal
    };

    static inline void test8(Assembly& as, ASMVal a, ASMVal b) {
        Opcode op = b.payload.kind == ASMVal::IMM ? Opcode::withExt(0xf6, 0x00) : Opcode::from(0x84);
        binaryop(as, BYTE, op, a, b);
    }

    static inline void test16(Assembly& as, ASMVal a, ASMVal b) {
        Opcode op = b.payload.kind == ASMVal::IMM ? Opcode::withExt(0xf6, 0x00) : Opcode::from(0x84);
        binaryop(as, WORD, op, a, b);
    }

    static inline void test32(Assembly& as, ASMVal a, ASMVal b) {
        Opcode op = b.payload.kind == ASMVal::IMM ? Opcode::withExt(0xf6, 0x00) : Opcode::from(0x84);
        binaryop(as, DWORD, op, a, b);
    }

    static inline void test64(Assembly& as, ASMVal a, ASMVal b) {
        Opcode op = b.payload.kind == ASMVal::IMM ? Opcode::withExt(0xf6, 0x00) : Opcode::from(0x84);
        binaryop(as, QWORD, op, a, b);
    }

    static inline void cmp8(Assembly& as, ASMVal a, ASMVal b) {
        Opcode op = b.payload.kind == ASMVal::IMM ? Opcode::withExt(0x80, 0x07) : Opcode::from(0x38);
        binaryop(as, BYTE, op, a, b);
    }

    static inline void cmp16(Assembly& as, ASMVal a, ASMVal b) {
        Opcode op = b.payload.kind == ASMVal::IMM ? Opcode::withExt(0x80, 0x07) : Opcode::from(0x38);
        binaryop(as, WORD, op, a, b);
    }

    static inline void cmp32(Assembly& as, ASMVal a, ASMVal b) {
        Opcode op = b.payload.kind == ASMVal::IMM ? Opcode::withExt(0x80, 0x07) : Opcode::from(0x38);
        binaryop(as, DWORD, op, a, b);
    }

    static inline void cmp64(Assembly& as, ASMVal a, ASMVal b) {
        Opcode op = b.payload.kind == ASMVal::IMM ? Opcode::withExt(0x80, 0x07) : Opcode::from(0x38);
        binaryop(as, QWORD, op, a, b);
    }

    static inline void fcmp32(Assembly& as, ASMVal a, ASMVal b) {
        vexbinaryop(as, SINGLE, Opcode::from(0x2f), a, ASMVal::fp(XMM15), b, false, TwoByteOpcode);
    }

    static inline void fcmp64(Assembly& as, ASMVal a, ASMVal b) {
        vexbinaryop(as, DOUBLE, Opcode::from(0x2f), a, ASMVal::fp(XMM15), b, false, TwoByteOpcode);
    }

    static inline void isz(Assembly& as, ASMVal dst, ASMVal src) {
        test64(as, src, src);
        unaryop(as, BYTE, Opcode::withExt(0x0f, 0x94, 0x00), dst);
    }

    static inline void isnz(Assembly& as, ASMVal dst, ASMVal src) {
        test64(as, src, src);
        unaryop(as, BYTE, Opcode::withExt(0x0f, 0x95, 0x00), dst);
    }

    static inline void ccc8(Assembly& as, Condition cc, ASMVal dst, ASMVal a, ASMVal b) {
        Opcode op = b.payload.kind == ASMVal::IMM ? Opcode::withExt(0x80, 0x07) : Opcode::from(0x38);
        cmp8(as, a, b);
        unaryop(as, BYTE, Opcode::withExt(0x0f, 0x90 + CCodes[cc], 0x00), dst);
    }

    static inline void ccc16(Assembly& as, Condition cc, ASMVal dst, ASMVal a, ASMVal b) {
        Opcode op = b.payload.kind == ASMVal::IMM ? Opcode::withExt(0x80, 0x07) : Opcode::from(0x38);
        cmp16(as, a, b);
        unaryop(as, BYTE, Opcode::withExt(0x0f, 0x90 + CCodes[cc], 0x00), dst);
    }

    static inline void ccc32(Assembly& as, Condition cc, ASMVal dst, ASMVal a, ASMVal b) {
        Opcode op = b.payload.kind == ASMVal::IMM ? Opcode::withExt(0x80, 0x07) : Opcode::from(0x38);
        cmp32(as, a, b);
        unaryop(as, BYTE, Opcode::withExt(0x0f, 0x90 + CCodes[cc], 0x00), dst);
    }

    static inline void ccc64(Assembly& as, Condition cc, ASMVal dst, ASMVal a, ASMVal b) {
        Opcode op = b.payload.kind == ASMVal::IMM ? Opcode::withExt(0x80, 0x07) : Opcode::from(0x38);
        cmp64(as, a, b);
        unaryop(as, BYTE, Opcode::withExt(0x0f, 0x90 + CCodes[cc], 0x00), dst);
    }

    static inline void fccc32(Assembly& as, FloatCondition cc, ASMVal dst, ASMVal a, ASMVal b) {
        fcmp32(as, a, b);
        unaryop(as, BYTE, Opcode::withExt(0x0f, 0x90 + CCodes[cc], 0x00), dst);
    }

    static inline void fccc64(Assembly& as, FloatCondition cc, ASMVal dst, ASMVal a, ASMVal b) {
        fcmp32(as, a, b);
        unaryop(as, BYTE, Opcode::withExt(0x0f, 0x90 + CCodes[cc], 0x00), dst);
    }

    // Memory

    static inline void push8(Assembly& as, ASMVal src) {
        assert(src.payload.kind == ASMVal::GP);
        as.code.write<u8>(0x66);
        if (needsREX(BYTE, src)) as.code.write<u8>(0x41);
        as.code.write<u8>(0x50 + (src.payload.gp & 0b111));
    }

    static inline void push16(Assembly& as, ASMVal src) {
        assert(src.payload.kind == ASMVal::GP);
        as.code.write<u8>(0x66);
        if (needsREX(BYTE, src)) as.code.write<u8>(0x41);
        as.code.write<u8>(0x50 + (src.payload.gp & 0b111));
    }

    static inline void push32(Assembly& as, ASMVal src) {
        assert(src.payload.kind == ASMVal::GP);
        if (needsREX(BYTE, src)) as.code.write<u8>(0x41);
        as.code.write<u8>(0x50 + (src.payload.gp & 0b111));
    }

    static inline void push64(Assembly& as, ASMVal src) {
        assert(src.payload.kind == ASMVal::GP);
        if (needsREX(BYTE, src)) as.code.write<u8>(0x41);
        as.code.write<u8>(0x50 + (src.payload.gp & 0b111));
    }

    static inline void pop8(Assembly& as, ASMVal dst) {
        assert(dst.payload.kind == ASMVal::GP);
        as.code.write<u8>(0x66);
        if (needsREX(BYTE, dst)) as.code.write<u8>(0x41);
        as.code.write<u8>(0x58 + (dst.payload.gp & 0b111));
    }

    static inline void pop16(Assembly& as, ASMVal dst) {
        assert(dst.payload.kind == ASMVal::GP);
        as.code.write<u8>(0x66);
        if (needsREX(BYTE, dst)) as.code.write<u8>(0x41);
        as.code.write<u8>(0x58 + (dst.payload.gp & 0b111));
    }

    static inline void pop32(Assembly& as, ASMVal dst) {
        assert(dst.payload.kind == ASMVal::GP);
        if (needsREX(BYTE, dst)) as.code.write<u8>(0x41);
        as.code.write<u8>(0x58 + (dst.payload.gp & 0b111));
    }

    static inline void pop64(Assembly& as, ASMVal dst) {
        assert(dst.payload.kind == ASMVal::GP);
        if (needsREX(BYTE, dst)) as.code.write<u8>(0x41);
        as.code.write<u8>(0x58 + (dst.payload.gp & 0b111));
    }

    static inline void fpush32(Assembly& as, ASMVal src) {
        f32tobits(as, ASMVal::gp(RAX), src);
        push32(as, ASMVal::gp(RAX));
    }

    static inline void fpush64(Assembly& as, ASMVal src) {
        f64tobits(as, ASMVal::gp(RAX), src);
        push64(as, ASMVal::gp(RAX));
    }

    static inline void fpop32(Assembly& as, ASMVal dst) {
        pop32(as, ASMVal::gp(RAX));
        f32frombits(as, dst, ASMVal::gp(RAX));
    }

    static inline void fpop64(Assembly& as, ASMVal dst) {
        pop64(as, ASMVal::gp(RAX));
        f64frombits(as, dst, ASMVal::gp(RAX));
    }

    static inline void mov8(Assembly& as, ASMVal dst, ASMVal src) {
        if (dst == src) return;
        if (src.payload.kind == ASMVal::IMM) {
            if (src.payload.imm == 0) return xor8(as, dst, dst, dst);

            unary_prefix(as, BYTE, dst);
            as.code.write<u8>(0xB0 + (dst.payload.gp & 0b111));
            as.code.write<i8>(src.payload.imm);
        }
        else binaryop(as, BYTE, Opcode::from(0x88), dst, src);
    }

    static inline void mov16(Assembly& as, ASMVal dst, ASMVal src) {
        if (dst == src) return;
        if (src.payload.kind == ASMVal::IMM) {
            if (src.payload.imm == 0) return xor16(as, dst, dst, dst);
            unary_prefix(as, WORD, dst);
            as.code.write<u8>(0xB8 + (dst.payload.gp & 0b111));
            as.code.writeLE<i16>(src.payload.imm);
        }
        else binaryop(as, WORD, Opcode::from(0x88), dst, src);
    }

    static inline void mov32(Assembly& as, ASMVal dst, ASMVal src) {
        if (dst == src) return;
        if (src.payload.kind == ASMVal::IMM) {
            if (src.payload.imm == 0) return xor32(as, dst, dst, dst);

            unary_prefix(as, DWORD, dst);
            as.code.write<u8>(0xB8 + (dst.payload.gp & 0b111));
            as.code.writeLE<i32>(src.payload.imm);
        }
        else binaryop(as, DWORD, Opcode::from(0x88), dst, src);
    }

    static inline void mov64(Assembly& as, ASMVal dst, ASMVal src) {
        if (dst == src) return;
        if (src.payload.kind == ASMVal::IMM) {
            if (src.payload.imm == 0) return xor64(as, dst, dst, dst);

            unary_prefix(as, DWORD, dst); // DWORD, not QWORD, since imm can only be 32 bits
            as.code.write<u8>(0xB8 + (dst.payload.gp & 0b111));
            as.code.writeLE<i32>(src.payload.imm);

            if (src.payload.imm < 0)
                binaryop(as, QWORD, Opcode::literal(0x63), dst, dst); // movsxd
        }
        else binaryop(as, QWORD, Opcode::from(0x88), dst, src);
    }

    static inline void fmov32(Assembly& as, ASMVal dst, ASMVal src) {
        if (src.kind() == ASMVal::F32) {
            if (src.payload.f32 == 0)
                return ssebinaryop(as, DWORD, Opcode::from(0x0f, 0xef), dst, dst);

            stridx label = as.symtab->anon();
            as.def(DATA_SECTION, label);
            as.data.writeLE<i32>(*(i32*)&src.payload.f32);
            ssebinaryop(as, SINGLE, Opcode::from(0x0f, 0x10), dst, ASMVal::data(label));
        }
        else ssebinaryop(as, SINGLE, Opcode::from(0x0f, 0x10), dst, src);
    }

    static inline void fmov64(Assembly& as, ASMVal dst, ASMVal src) {
        if (src.kind() == ASMVal::F64) {
            if (src.dval == 0)
                return ssebinaryop(as, QWORD, Opcode::from(0x0f, 0xef), dst, dst);

            stridx label = as.symtab->anon();
            as.def(DATA_SECTION, label);
            as.data.writeLE<u64>(src.uval);
            ssebinaryop(as, DOUBLE, Opcode::from(0x0f, 0x10), dst, ASMVal::data(label));
        }
        else ssebinaryop(as, DOUBLE, Opcode::from(0x0f, 0x10), dst, src);
    }

    static inline void ld8(Assembly& as, ASMVal dst, ASMVal src);
    static inline void ld16(Assembly& as, ASMVal dst, ASMVal src);
    static inline void ld32(Assembly& as, ASMVal dst, ASMVal src);
    static inline void ld64(Assembly& as, ASMVal dst, ASMVal src);

    static inline void st8(Assembly& as, ASMVal dst, ASMVal src);
    static inline void st16(Assembly& as, ASMVal dst, ASMVal src);
    static inline void st32(Assembly& as, ASMVal dst, ASMVal src);
    static inline void st64(Assembly& as, ASMVal dst, ASMVal src);

    static inline void fld32(Assembly& as, ASMVal dst, ASMVal src);
    static inline void fld64(Assembly& as, ASMVal dst, ASMVal src);

    static inline void fst32(Assembly& as, ASMVal dst, ASMVal src);
    static inline void fst64(Assembly& as, ASMVal dst, ASMVal src);
    
    static inline void ldi8(Assembly& as, ASMVal dst, ASMVal src, ASMVal idx);
    static inline void ldi16(Assembly& as, ASMVal dst, ASMVal src, ASMVal idx);
    static inline void ldi32(Assembly& as, ASMVal dst, ASMVal src, ASMVal idx);
    static inline void ldi64(Assembly& as, ASMVal dst, ASMVal src, ASMVal idx);
    
    static inline void ldai8(Assembly& as, ASMVal dst, ASMVal src, ASMVal idx);
    static inline void ldai16(Assembly& as, ASMVal dst, ASMVal src, ASMVal idx);
    static inline void ldai32(Assembly& as, ASMVal dst, ASMVal src, ASMVal idx);
    static inline void ldai64(Assembly& as, ASMVal dst, ASMVal src, ASMVal idx);
    
    static inline void sti8(Assembly& as, ASMVal dst, ASMVal src, ASMVal idx);
    static inline void sti16(Assembly& as, ASMVal dst, ASMVal src, ASMVal idx);
    static inline void sti32(Assembly& as, ASMVal dst, ASMVal src, ASMVal idx);
    static inline void sti64(Assembly& as, ASMVal dst, ASMVal src, ASMVal idx);

    static inline void fldi32(Assembly& as, ASMVal dst, ASMVal src, ASMVal idx);
    static inline void fldi64(Assembly& as, ASMVal dst, ASMVal src, ASMVal idx);
    
    static inline void fsti32(Assembly& as, ASMVal dst, ASMVal src, ASMVal idx);
    static inline void fsti64(Assembly& as, ASMVal dst, ASMVal src, ASMVal idx);
    
    static inline void lda(Assembly& as, ASMVal dst, ASMVal src);

    static inline void ldc(Assembly& as, ASMVal dst, i64 imm) {
        unary_prefix(as, QWORD, dst);
        as.code.write<u8>(0xB8 + (dst.payload.gp & 0b111));
        as.code.writeLE<i64>(imm);
    }

    // Labels

    static inline void global(Assembly& as, stridx sym) {
        as.def(CODE_SECTION, sym);
    }

    static inline void local(Assembly& as, stridx sym) {
        as.def(CODE_SECTION, sym);
    }

    // Jumps

    static inline void j(Assembly& as, ASMVal dst) {
        if (dst.payload.kind == ASMVal::GP) unaryop(as, DWORD, Opcode::litExt(0xff, 0x04), dst);
        else {
            as.code.write<u8>(0xe9);
            as.code.write<i32>(0);
            assert(dst.payload.memkind == ASMVal::LOCAL_LABEL || dst.payload.memkind == ASMVal::FUNC_LABEL);
            as.ref(CODE_SECTION, Reloc::REL32_LE, dst.payload.sym);
        }
    }

    static inline void jcc(Assembly& as, Condition cc, ASMVal dst) {
        as.code.write<u8>(0x0f);
        as.code.write<u8>(0x80 + CCodes[cc]);
        as.code.writeLE<i32>(0);
        assert(dst.payload.memkind == ASMVal::LOCAL_LABEL || dst.payload.memkind == ASMVal::FUNC_LABEL);
        as.ref(CODE_SECTION, Reloc::REL32_LE, dst.payload.sym);
    }

    static inline void jcc(Assembly& as, FloatCondition cc, ASMVal dst) {
        as.code.write<u8>(0x0f);
        as.code.write<u8>(0x80 + CCodes[cc]);
        as.code.writeLE<i32>(0);
        assert(dst.payload.memkind == ASMVal::LOCAL_LABEL || dst.payload.memkind == ASMVal::FUNC_LABEL);
        as.ref(CODE_SECTION, Reloc::REL32_LE, dst.payload.sym);
    }

    static inline void jz(Assembly& as, ASMVal dst, ASMVal cond) {
        test64(as, cond, cond);
        jcc(as, COND_EQ, dst);
    }

    static inline void jnz(Assembly& as, ASMVal dst, ASMVal cond) {
        test64(as, cond, cond);
        jcc(as, COND_NEQ, dst);
    }

    static inline void jcc8(Assembly& as, Condition cc, ASMVal dst, ASMVal a, ASMVal b) {
        cmp8(as, a, b);
        jcc(as, cc, dst);
    }

    static inline void jcc16(Assembly& as, Condition cc, ASMVal dst, ASMVal a, ASMVal b) {
        cmp16(as, a, b);
        jcc(as, cc, dst);
    }

    static inline void jcc32(Assembly& as, Condition cc, ASMVal dst, ASMVal a, ASMVal b) {
        cmp32(as, a, b);
        jcc(as, cc, dst);
    }

    static inline void jcc64(Assembly& as, Condition cc, ASMVal dst, ASMVal a, ASMVal b) {
        cmp64(as, a, b);
        jcc(as, cc, dst);
    }
    
    static inline void fjcc32(Assembly& as, FloatCondition cc, ASMVal dst, ASMVal a, ASMVal b) {
        fcmp32(as, a, b);
        jcc(as, cc, dst);
    }

    static inline void fjcc64(Assembly& as, FloatCondition cc, ASMVal dst, ASMVal a, ASMVal b) {
        fcmp64(as, a, b);
        jcc(as, cc, dst);
    }

    // Functions

    static void enter(Assembly& as) {
        push64(as, ASMVal::gp(RBP));
        mov64(as, ASMVal::gp(RBP), ASMVal::gp(RSP));
    }

    static void stack(Assembly& as, ASMVal dst) {
        sub64(as, ASMVal::gp(RSP), ASMVal::gp(RSP), dst);
    }

    static void alloca(Assembly& as, ASMVal dst, ASMVal src) {
        stack(as, src);
        mov64(as, dst, ASMVal::gp(RSP));
    }

    static void unstack(Assembly& as, ASMVal dst) {
        add64(as, ASMVal::gp(RSP), ASMVal::gp(RSP), dst);
    }

    static void leave(Assembly& as) {
        mov64(as, ASMVal::gp(RSP), ASMVal::gp(RBP));
        pop64(as, ASMVal::gp(RBP));
    }

    static inline void call(Assembly& as, ASMVal dst) {
        assert(dst.payload.kind == ASMVal::MEM);

        if (dst.payload.kind == ASMVal::GP) unaryop(as, DWORD, Opcode::litExt(0xff, 0x02), dst);
        else {
            as.code.write<u8>(0xe8);
            as.code.write<i32>(0);
            assert(dst.payload.memkind == ASMVal::LOCAL_LABEL || dst.payload.memkind == ASMVal::FUNC_LABEL);
            if (dst.payload.memkind == ASMVal::LOCAL_LABEL) 
                as.ref(CODE_SECTION, Reloc::REL32_LE, dst.payload.sym);
            else
                as.ref(CODE_SECTION, Reloc::REL32_LE, dst.payload.sym);
        }
    }

    static inline void ret(Assembly& as) {
        as.code.write<i8>(0xc3);
    }

    // Conversions

    static inline void sxt8(Assembly& as, ASMVal dst, ASMVal src) {
        binary_prefix(as, QWORD, dst, src); // QWORD since that's the size of the destination.
        as.code.write<u8>(0x0f); 
        as.code.write<u8>(0xbe);
        binary_args(as, BYTE, Opcode::literal(0x0f, 0xbe), dst, src);
    }

    static inline void sxt16(Assembly& as, ASMVal dst, ASMVal src) {
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
        binary_prefix(as, QWORD, dst, src); // QWORD since that's the size of the destination.
        as.code.write<u8>(0x0f); 
        as.code.write<u8>(0xb6);
        binary_args(as, BYTE, Opcode::literal(0x0f, 0xb6), dst, src);
    }

    static inline void zxt16(Assembly& as, ASMVal dst, ASMVal src) {
        binary_prefix(as, QWORD, dst, src); // QWORD since that's the size of the destination.
        as.code.write<u8>(0x0f); 
        as.code.write<u8>(0xb7);
        binary_args(as, WORD, Opcode::literal(0x0f, 0xb7), dst, src);
    }

    static inline void zxt32(Assembly& as, ASMVal dst, ASMVal src) {
        mov32(as, dst, src);
    }

    static inline void i8tof32(Assembly& as, ASMVal dst, ASMVal src);
    static inline void i16tof32(Assembly& as, ASMVal dst, ASMVal src);
    static inline void i32tof32(Assembly& as, ASMVal dst, ASMVal src);
    static inline void i64tof32(Assembly& as, ASMVal dst, ASMVal src);

    static inline void i8tof64(Assembly& as, ASMVal dst, ASMVal src);
    static inline void i16tof64(Assembly& as, ASMVal dst, ASMVal src);
    static inline void i32tof64(Assembly& as, ASMVal dst, ASMVal src);
    static inline void i64tof64(Assembly& as, ASMVal dst, ASMVal src);

    static inline void f32toi8(Assembly& as, ASMVal dst, ASMVal src);
    static inline void f32toi16(Assembly& as, ASMVal dst, ASMVal src);
    static inline void f32toi32(Assembly& as, ASMVal dst, ASMVal src);
    static inline void f32toi64(Assembly& as, ASMVal dst, ASMVal src);

    static inline void f64toi8(Assembly& as, ASMVal dst, ASMVal src);
    static inline void f64toi16(Assembly& as, ASMVal dst, ASMVal src);
    static inline void f64toi32(Assembly& as, ASMVal dst, ASMVal src);
    static inline void f64toi64(Assembly& as, ASMVal dst, ASMVal src);

    static inline void f32tof64(Assembly& as, ASMVal dst, ASMVal src);
    static inline void f64tof32(Assembly& as, ASMVal dst, ASMVal src);

    static void f32frombits(Assembly& as, ASMVal dst, ASMVal src);
    static void f64frombits(Assembly& as, ASMVal dst, ASMVal src);
    static void f32tobits(Assembly& as, ASMVal dst, ASMVal src);
    static void f64tobits(Assembly& as, ASMVal dst, ASMVal src);

    // Memory
    
    static void mcpy(Assembly& as, ASMVal dst, ASMVal a, ASMVal b);
    static void mmov(Assembly& as, ASMVal dst, ASMVal a, ASMVal b);
    static void mset(Assembly& as, ASMVal dst, ASMVal a, ASMVal b);
    static void mcmp(Assembly& as, ASMVal dst, ASMVal a, ASMVal b, ASMVal c);
};

struct AMD64LinuxTarget : public AMD64Target {
    static const mreg GP_ARGS[6], FP_ARGS[8];
    static const TargetDesc DESC;
    
    static void place_call(CallBindings& call, const TypeTable& tab, typeidx fntype);
};

struct AMD64DarwinTarget : public AMD64LinuxTarget {};

ENDMODULE()

#endif