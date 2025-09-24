#ifndef ASM_ARCH_RISCV64_H
#define ASM_ARCH_RISCV64_H

#include "asm/arch.h"

struct RISCV64Assembler {
static constexpr mreg
        ZERO = 0, RA = 1, SP = 2, GP = 3, TP = 4,
        T0 = 5, T1 = 6, T2 = 7,
        S0 = 8, S1 = 9,
        A0 = 10, A1 = 11, A2 = 12, A3 = 13, A4 = 14, A5 = 15, A6 = 16, A7 = 17,
        S2 = 18, S3 = 19, S4 = 20, S5 = 21, S6 = 22, S7 = 23, S8 = 24, S9 = 25, S10 = 26, S11 = 27,
        T3 = 28, T4 = 29, T5 = 30, T6 = 31,
        FT0 = 32, FT1 = 33, FT2 = 34, FT3 = 35, FT4 = 36, FT5 = 37, FT6 = 38, FT7 = 39,
        FS0 = 40, FS1 = 41,
        FA0 = 42, FA1 = 43, FA2 = 44, FA3 = 45, FA4 = 46, FA5 = 47, FA6 = 48, FA7 = 49,
        FS2 = 50, FS3 = 51, FS4 = 52, FS5 = 53, FS6 = 54, FS7 = 55, FS8 = 56, FS9 = 57, FS10 = 58, FS11 = 59,
        FT8 = 60, FT9 = 61, FT10 = 62, FT11 = 63;
    static constexpr mreg GP_REGS[26] = {
        T0, T1, T2,
        S0, S1,
        A0, A1, A2, A3, A4, A5, A6, A7,
        S2, S3, S4, S5, S6, S7, S8, S9, S10, S11,
        T3, T4, T5
    };
    static constexpr mreg FP_REGS[31] = {
        FT0, FT1, FT2, FT3, FT4, FT5, FT6,
        FS0, FS1,
        FA0, FA1, FA2, FA3, FA4, FA5, FA6, FA7,
        FS2, FS3, FS4, FS5, FS6, FS7, FS8, FS9, FS10, FS11,
        FT8, FT9, FT10, FT11
    };
    static constexpr const i8* GP_REG_NAMES[32] = {
        "zero", "ra", "sp", "gp", "tp",
        "t0", "t1", "t2",
        "s0", "s1",
        "a0", "a1", "a2", "a3", "a4", "a5", "a6", "a7",
        "s2", "s3", "s4", "s5", "s6", "s7", "s8", "s9", "s10", "s11",
        "t3", "t4", "t5", "t6"
    };
    static constexpr const i8* FP_REG_NAMES[32] = {
        "ft0", "ft1", "ft2", "ft3", "ft4", "ft5", "ft6", "ft7",
        "fs0", "fs1",
        "fa0", "fa1", "fa2", "fa3", "fa4", "fa5", "fa6", "fa7",
        "fs2", "fs3", "fs4", "fs5", "fs6", "fs7", "fs8", "fs9", "fs10", "fs11",
        "ft8", "ft9", "ft10", "ft11"
    };

    constexpr static mreg scratch = T6, fscratch = FT7;
    constexpr static mreg fp = S0, sp = SP;
    constexpr static EndianOrder endianness = EndianOrder::LITTLE;

    static constexpr inline RegSet gps() {
        return RegSet(GP_REGS, 26);
    }

    static constexpr inline RegSet fps() {
        return RegSet(FP_REGS, 31);
    }

    static constexpr inline bool is_gp(mreg r) {
        return r < 32;
    }

    static constexpr inline bool is_fp(mreg r) {
        return r >= 32 && r < 64;
    }

    static constexpr inline const_slice<i8> reg_name(mreg r) {
        if (is_gp(r)) return { GP_REG_NAMES[i32(r)], findc(GP_REG_NAMES[i32(r)], 0) };
        else return { FP_REG_NAMES[i32(r - FT0)], findc(FP_REG_NAMES[i32(r - FT0)], 0) };
    }

    static constexpr inline Size word_size() {
        return Size::BITS64;
    }

    static constexpr inline Size ptr_size() {
        return Size::BITS64;
    }

    static inline constexpr RegSet clobbers(ASMOpcode opcode) {
        return RegSet();
    }

    // For now, we include the ABI methods in the base.

    static inline constexpr RegSet caller_saved_gps() {
        return RegSet(T0, T1, T2, T3, T4, T5, T6, A0, A1, A2, A3, A4, A5, A6, A7);
    }

    static inline constexpr RegSet caller_saved_fps() {
        return RegSet(FT0, FT1, FT2, FT3, FT4, FT5, FT6, FT7, FT8, FT9, FT10, FT11, FA0, FA1, FA2, FA3, FA4, FA5, FA6, FA7);
    }

    static inline constexpr RegSet caller_saves() {
        return caller_saved_gps() | caller_saved_fps();
    }

    static inline constexpr RegSet callee_saved_gps() {
        return RegSet(SP, FP, S0, S1, S2, S3, S4, S5, S6, S7, S8, S9, S10, S11);
    }

    static inline constexpr RegSet callee_saved_fps() {
        return RegSet(FS0, FS1, FS2, FS3, FS4, FS5, FS6, FS7, FS8, FS9, FS10, FS11);
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

    // Now let's get to the instructions...

    static inline void ensureAlign(Assembly& as) {
        if (as.code.size() & 2)
            encodeci(as, 0b01, ZERO, 0, 0b000); // addi zero, 0; encodes c.nop
    }

    static inline void encoder(Assembly& as, i8 opcode, mreg rd, mreg rs1, mreg rs2, i8 funct3 = 0, i8 funct7 = 0) {
        // Encodes an R-type instruction.

        ensureAlign(as);

        u32 word = (funct7 & 0b1111111) << 25
            | (rs2 & 0b11111) << 20
            | (rs1 & 0b11111) << 15
            | (funct3 & 0b111) << 12
            | (rd & 0b11111) << 7
            | opcode & 0b1111111;
        as.code.writeLE<u32>(word);
    }

    static inline void encodei(Assembly& as, i8 opcode, mreg rd, mreg rs1, i32 imm, i8 funct3 = 0) {
        // Encodes an I-type instruction.

        ensureAlign(as);

        assert(fitsSigned<12>(imm));
        u32 word = (imm & 0b111111111111) << 20
            | (rs1 & 0b11111) << 15
            | (funct3 & 0b111) << 12
            | (rd & 0b11111) << 7
            | opcode & 0b1111111;
        as.code.writeLE<u32>(word);
    }

    static inline void encodes(Assembly& as, i8 opcode, mreg rs1, mreg rs2, i32 imm, i8 funct3 = 0) {
        // Encodes an S-type instruction.

        ensureAlign(as);

        assert(fitsSigned<12>(imm));
        u32 word = ((imm >> 5) & 0b1111111) << 25
            | (rs2 & 0b11111) << 20
            | (rs1 & 0b11111) << 15
            | (funct3 & 0b111) << 12
            | (imm & 0b11111) << 7
            | opcode & 0b1111111;
        as.code.writeLE<u32>(word);
    }

    static inline void encodeu(Assembly& as, i8 opcode, mreg rd, i32 imm) {
        // Encodes a U-type instruction.

        ensureAlign(as);

        assert(fitsSigned<20>(imm));
        u32 word = (imm & 0xfffff) << 12
            | (rd & 0b11111) << 7
            | opcode & 0b1111111;
        as.code.writeLE<u32>(word);
    }

    static inline bool encodecr(Assembly& as, i8 op, mreg rd, mreg rs2, i8 funct4 = 0) {
        // Tries to encode a CR-type compressed instruction.

        assert(fitsUnsigned<2>(op));
        assert(fitsUnsigned<4>(funct4));
        u16 word = (funct4 & 0b1111) << 12
            | (rd & 0b11111) << 7
            | (rs2 & 0b11111) << 2
            | (op & 0b11);
        as.code.writeLE<u16>(word);
        return true;
    }

    static inline bool encodeci(Assembly& as, i8 op, mreg rd, i32 imm, i8 funct3 = 0, bool isSigned = true) {
        // Tries to encode a CI-type compressed instruction.

        assert(fitsUnsigned<2>(op));
        assert(fitsUnsigned<3>(funct3));
        if ((isSigned && !fitsSigned<6>(imm)) || (!isSigned && !fitsUnsigned<6>(imm)))
            return false;
        u16 word = (funct3 & 0b111) << 13
            | ((imm >> 5) & 1) << 12
            | (rd & 0b11111) << 7
            | (imm & 0b11111) << 2
            | (op & 0b11);
        as.code.writeLE<u16>(word);
        return true;
    }

    static inline bool encodecss(Assembly& as, i8 op, mreg rs2, i32 imm, i8 funct3 = 0) {
        // Tries to encode a CSS-type compressed instruction.

        assert(fitsUnsigned<2>(op));
        assert(fitsUnsigned<3>(funct3));
        if (!fitsUnsigned<6>(imm))
            return false;
        u16 word = (funct3 & 0b111) << 13
            | (imm & 0b111111) << 7
            | (rs2 & 0b11111) << 2
            | (op & 0b11);
        as.code.writeLE<u16>(word);
        return true;
    }

    static inline bool isValidCompressedReg(mreg rd) {
        return rd >= S0 && rd <= A5;
    }

    static inline mreg compressReg(mreg rd) {
        assert(!(rd - 8 & 0b111));
        return rd - 8;
    }

    static inline bool encodeciw(Assembly& as, i8 op, mreg rd, i32 imm, i8 funct3 = 0) {
        // Tries to encode a CIW-type compressed instruction.

        assert(fitsUnsigned<2>(op));
        assert(fitsUnsigned<3>(funct3));
        if (!fitsSigned<8>(imm))
            return false;
        if (!isValidCompressedReg(rd))
            return false;
        u16 word = (funct3 & 0b111) << 13
            | (imm & 0b11111111) << 5
            | compressReg(rd) << 2
            | (op & 0b11);
        as.code.writeLE<u16>(word);
        return true;
    }

    static inline bool encodecl(Assembly& as, i8 op, mreg rd, mreg rs1, i32 imm, i8 funct3 = 0) {
        // Tries to encode a CL-type compressed instruction.

        assert(fitsUnsigned<2>(op));
        assert(fitsUnsigned<3>(funct3));
        if (!fitsUnsigned<5>(imm))
            return false;
        if (!isValidCompressedReg(rd))
            return false;
        if (!isValidCompressedReg(rs1))
            return false;
        u16 word = (funct3 & 0b111) << 13
            | ((imm >> 2) & 0b111) << 10
            | compressReg(rs1) << 7
            | (imm & 0b11) << 5
            | compressReg(rd) << 2
            | (op & 0b11);
        as.code.writeLE<u16>(word);
        return true;
    }

    static inline bool encodecs(Assembly& as, i8 op, mreg rs1, mreg rs2, i32 imm, i8 funct3 = 0) {
        // Tries to encode a CS-type compressed instruction.

        assert(fitsUnsigned<2>(op));
        assert(fitsUnsigned<3>(funct3));
        if (!fitsUnsigned<5>(imm))
            return false;
        if (!isValidCompressedReg(rs2))
            return false;
        if (!isValidCompressedReg(rs1))
            return false;
        u16 word = (funct3 & 0b111) << 13
            | ((imm >> 2) & 0b111) << 10
            | compressReg(rs1) << 7
            | (imm & 0b11) << 5
            | compressReg(rs2) << 2
            | (op & 0b11);
        as.code.writeLE<u16>(word);
        return true;
    }

    static inline bool encodeca(Assembly& as, i8 op, mreg rd, mreg rs2, i8 funct2 = 0, i8 funct6 = 0) {
        // Tries to encode a CA-type compressed instruction.

        assert(fitsUnsigned<2>(op));
        assert(fitsUnsigned<2>(funct2));
        assert(fitsUnsigned<6>(funct6));
        if (!isValidCompressedReg(rs2))
            return false;
        if (!isValidCompressedReg(rd))
            return false;
        u16 word = (funct6 & 0b111111) << 10
            | compressReg(rd) << 7
            | (funct2 & 0b11) << 5
            | compressReg(rs2) << 2
            | (op & 0b11);
        as.code.writeLE<u16>(word);
        return true;
    }

    static inline bool encodecb(Assembly& as, i8 op, mreg rd, i32 offset, i8 funct3 = 0, bool isSigned = true) {
        // Tries to encode a CB-type compressed instruction.

        assert(fitsUnsigned<2>(op));
        assert(fitsUnsigned<3>(funct3));
        if (!isValidCompressedReg(rd))
            return false;
        if ((isSigned && !fitsSigned<8>(offset)) || (!isSigned && !fitsUnsigned<8>(offset)))
            return false;
        u16 word = (funct3 & 0b111) << 13
            | ((offset >> 5) & 0b111) << 10
            | compressReg(rd) << 7
            | (offset & 0b11111) << 2
            | (op & 0b11);
        as.code.writeLE<u16>(word);
        return true;
    }

    static inline bool encodecb2(Assembly& as, i8 op, mreg rd, i32 offset, i8 funct2 = 0, i8 funct3 = 0, bool isSigned = true) {
        // Tries to encode a CB-type compressed instruction.

        assert(fitsUnsigned<2>(op));
        assert(fitsUnsigned<2>(funct2));
        assert(fitsUnsigned<3>(funct3));
        if (!isValidCompressedReg(rd))
            return false;
        if ((isSigned && !fitsSigned<6>(offset)) || (!isSigned && !fitsUnsigned<6>(offset)))
            return false;
        u16 word = (funct3 & 0b111) << 13
            | ((offset >> 5) & 0b1) << 12
            | (funct2 & 0b11) << 10
            | compressReg(rd) << 7
            | (offset & 0b11111) << 2
            | (op & 0b11);
        as.code.writeLE<u16>(word);
        return true;
    }

    static inline bool encodecj(Assembly& as, i8 op, i32 offset, i8 funct3 = 0) {
        // Tries to encode a CJ-type compressed instruction.

        assert(fitsUnsigned<2>(op));
        assert(fitsUnsigned<3>(funct3));
        if (!fitsSigned<11>(offset))
            return false;
        u16 word = (funct3 & 0b111) << 13
            | (offset & 0b11111111111) << 2
            | (op & 0b11);
        as.code.writeLE<u16>(word);
        return true;
    }

    static inline ASMVal materializeImm(Assembly& as, ASMVal dst, i32 src) {
        assert(dst.kind == ASMVal::GP);
        if (fitsSigned<6>(src) && encodeci(as, 0b01, dst.gp, src, 0b010)) // c.li
            return dst;
        i32 upper = src >> 12;
        if (!upper) {
            encodei(as, 0b0010011, dst.gp, ZERO, src & 0xfff); // addi (zero)
        } else {
            if (fitsSigned<6>(upper))
                encodeci(as, 0b01, dst.gp, upper, 0b011); // c.lui
            else
                encodeu(as, 0b0110111, dst.gp, src >> 12); // lui
            if (src & 0xfff)
                encodei(as, 0b0010011, dst.gp, dst.gp, src & 0xfff); // addi
        }
        return dst;
    }

    // For 8-bit arithmetic, since RISC-V has no native instructions for it, we
    // generally just emit the 32-bit form. This is broadly consistent with the
    // semantics of our macro assembly language, in that an N-bit result only
    // guarantees the values of those N bits. We do need to extend for some
    // operations, but these are relatively few and far between and hopefully
    // somewhat rare for small integer widths.

    static inline void add8(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) {
        add32(as, dst, a, b);
    }

    static inline void add16(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) {
        add32(as, dst, a, b);
    }

    static inline void add32(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) {
        if (a.kind == ASMVal::IMM)
            swap(a, b);
        if (b.kind == ASMVal::IMM) {
            if (b.imm == 0)
                return mov32(as, dst, a);
            if (encodeci(as, 0b01, dst.gp, b.imm, 0b001)) // c.addiw
                return;
            if (fitsSigned<12>(b.imm))
                return encodei(as, 0b0011011, dst.gp, a.gp, b.imm, 0b000); // addiw
            b = materializeImm(as, ::GP(scratch), b.imm);
        }
        if (dst == b)
            swap(dst, b);
        if (dst == a && encodeca(as, 0b01, dst.gp, b.gp, 0b01, 0b100111)) // c.addw
            return;
        return encoder(as, 0b0110011, dst.gp, a.gp, b.gp, 0b000, 0b0000000); // addw
    }

    static inline void add64(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) {
        if (a.kind == ASMVal::IMM)
            swap(a, b);
        if (b.kind == ASMVal::IMM) {
            if (b.imm == 0)
                return mov64(as, dst, a);
            if (encodeci(as, 0b01, dst.gp, b.imm, 0b000)) // c.addi
                return;
            if (fitsSigned<12>(b.imm))
                return encodei(as, 0b0010011, dst.gp, a.gp, b.imm, 0b000); // addi
            b = materializeImm(as, ::GP(scratch), b.imm);
        }
        if (dst == b)
            swap(dst, b);
        if (dst == a && encodecr(as, 0b10, dst.gp, b.gp, 0b1001)) // c.add
            return;
        return encoder(as, 0b0110011, dst.gp, a.gp, b.gp, 0b000, 0b0000000); // add
    }

    static inline void sub8(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) {
        sub32(as, dst, a, b);
    }

    static inline void sub16(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) {
        sub32(as, dst, a, b);
    }

    static inline void sub32(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) {
        if (a.kind == ASMVal::IMM)
            swap(a, b);
        if (b.kind == ASMVal::IMM) {
            if (b.imm == 0)
                return mov32(as, dst, a);
            if (encodeci(as, 0b01, dst.gp, -b.imm, 0b001)) // c.addiw
                return;
            if (fitsSigned<12>(b.imm))
                return encodei(as, 0b0011011, dst.gp, a.gp, -b.imm, 0b000); // addiw
            b = materializeImm(as, ::GP(scratch), b.imm);
        }
        if (dst == b)
            swap(dst, b);
        if (dst == a && encodeca(as, 0b01, dst.gp, b.gp, 0b00, 0b100111)) // c.subw
            return;
        return encoder(as, 0b0110011, dst.gp, a.gp, b.gp, 0b000, 0b0100000); // subw
    }

    static inline void sub64(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) {
        if (a.kind == ASMVal::IMM)
            swap(a, b);
        if (b.kind == ASMVal::IMM) {
            if (b.imm == 0)
                return mov64(as, dst, ::GP(ZERO));
            if (encodeci(as, 0b01, dst.gp, -b.imm, 0b000)) // c.addi (negative imm)
                return;
            if (fitsSigned<12>(-b.imm))
                return encodei(as, 0b0010011, dst.gp, a.gp, -b.imm, 0b000); // addi (negative imm)
            b = materializeImm(as, ::GP(scratch), b.imm);
        }
        if (dst == b)
            swap(dst, b);
        if (dst == a && encodeca(as, 0b01, dst.gp, b.gp, 0b00, 0b100011)) // c.sub
            return;
        return encoder(as, 0b0110011, dst.gp, a.gp, b.gp, 0b000, 0b0100000); // sub
    }


    static inline void and8(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) {
        and64(as, dst, a, b);
    }

    static inline void and16(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) {
        and64(as, dst, a, b);
    }

    static inline void and32(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) {
        and64(as, dst, a, b);
    }

    static inline void and64(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) {
        if (a.kind == ASMVal::IMM)
            swap(a, b);
        if (b.kind == ASMVal::IMM) {
            if (b.imm == 0)
                return mov64(as, dst, a);
            if (dst == a && encodecb2(as, 0b01, dst.gp, b.imm, 0b10, 0b100)) // c.andi
                return;
            if (fitsSigned<12>(b.imm))
                return encodei(as, 0b0010011, dst.gp, a.gp, b.imm, 0b111); // andi
            b = materializeImm(as, ::GP(scratch), b.imm);
        }
        if (dst == b)
            swap(dst, b);
        if (dst == a && encodeca(as, 0b01, dst.gp, b.gp, 0b11, 0b100011)) // c.and
            return;
        return encoder(as, 0b0110011, dst.gp, a.gp, b.gp, 0b111, 0b0000000); // and
    }

    static inline void or8(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) {
        or64(as, dst, a, b);
    }

    static inline void or16(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) {
        or64(as, dst, a, b);
    }

    static inline void or32(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) {
        or64(as, dst, a, b);
    }

    static inline void or64(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) {
        if (a.kind == ASMVal::IMM)
            swap(a, b);
        if (b.kind == ASMVal::IMM) {
            if (b.imm == 0)
                return mov64(as, dst, a);
            if (fitsSigned<12>(b.imm))
                return encodei(as, 0b0010011, dst.gp, a.gp, b.imm, 0b110); // ori
            b = materializeImm(as, ::GP(scratch), b.imm);
        }
        if (dst == b)
            swap(dst, b);
        if (dst == a && encodeca(as, 0b01, dst.gp, b.gp, 0b10, 0b100011)) // c.or
            return;
        return encoder(as, 0b0110011, dst.gp, a.gp, b.gp, 0b110, 0b0000000); // or
    }

    static inline void xor8(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) {
        xor64(as, dst, a, b);
    }

    static inline void xor16(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) {
        xor64(as, dst, a, b);
    }

    static inline void xor32(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) {
        xor64(as, dst, a, b);
    }

    static inline void xor64(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) {
        if (a.kind == ASMVal::IMM)
            swap(a, b);
        if (b.kind == ASMVal::IMM) {
            if (b.imm == 0)
                return mov64(as, dst, a);
            if (fitsSigned<12>(b.imm))
                return encodei(as, 0b0010011, dst.gp, a.gp, b.imm, 0b100); // xori
            b = materializeImm(as, ::GP(scratch), b.imm);
        }
        if (dst == b)
            swap(dst, b);
        if (dst == a && encodeca(as, 0b01, dst.gp, b.gp, 0b01, 0b100011)) // c.xor
            return;
        return encoder(as, 0b0110011, dst.gp, a.gp, b.gp, 0b100, 0b0000000); // xor
    }

    static inline void shl8(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) {
        shl64(as, dst, a, b);
    }

    static inline void shl16(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) {
        shl64(as, dst, a, b);
    }

    static inline void shl32(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) {
        if (b.kind == ASMVal::IMM) {
            if (!b.imm)
                return;
            assert(fitsUnsigned<6>(b.imm));
            encodei(as, 0b0011011, dst.gp, a.gp, 0b000000 << 6 | b.imm, 0b001); // slliw
            return;
        }
        encoder(as, 0b0111011, dst.gp, a.gp, b.gp, 0b001, 0b0000000); // sllw
    }

    static inline void shl64(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) {
        if (b.kind == ASMVal::IMM) {
            if (!b.imm)
                return;
            if (dst == a && a.kind == ASMVal::GP && encodeci(as, 0b10, dst.gp, b.imm, 0b000, false)) // c.slli
                return;
            assert(fitsUnsigned<6>(b.imm));
            encodei(as, 0b0010011, dst.gp, a.gp, 0b000000 << 6 | b.imm, 0b001); // slli
            return;
        }
        encoder(as, 0b0110011, dst.gp, a.gp, b.gp, 0b001, 0b0000000); // sll
    }

    static inline void shr8(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) {
        if (b.kind == ASMVal::IMM) {
            shl64(as, dst, a, Imm(56));
            shr64(as, dst, a, Imm(56 + b.imm));
            return;
        }
        zxt8(as, dst, a);
        shr64(as, dst, a, b);
    }

    static inline void shr16(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) {
        if (b.kind == ASMVal::IMM) {
            shl64(as, dst, a, Imm(48));
            shr64(as, dst, a, Imm(48 + b.imm));
            return;
        }
        zxt16(as, dst, a);
        shr64(as, dst, a, b);
    }

    static inline void shr32(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) {
        if (b.kind == ASMVal::IMM) {
            if (!b.imm)
                return;
            assert(fitsUnsigned<6>(b.imm));
            encodei(as, 0b0011011, dst.gp, a.gp, 0b000000 << 6 | b.imm, 0b101); // srliw
            return;
        }
        encoder(as, 0b0111011, dst.gp, a.gp, b.gp, 0b101, 0b0000000); // srlw
    }

    static inline void shr64(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) {
        if (b.kind == ASMVal::IMM) {
            if (!b.imm)
                return;
            assert(fitsUnsigned<6>(b.imm));
            if (dst == a && a.kind == ASMVal::GP && encodecb2(as, 0b10, dst.gp, b.imm, 0b00, 0b100)) // c.srli
                return;
            encodei(as, 0b0010011, dst.gp, a.gp, 0b000000 << 6 | b.imm, 0b101); // srli
            return;
        }
        encoder(as, 0b0110011, dst.gp, a.gp, b.gp, 0b101, 0b0000000); // srl
    }

    static inline void sar8(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) {
        if (b.kind == ASMVal::IMM) {
            shl64(as, dst, a, Imm(56));
            sar64(as, dst, a, Imm(56 + b.imm));
            return;
        }
        sxt8(as, dst, a);
        sar64(as, dst, a, b);
    }

    static inline void sar16(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) {
        if (b.kind == ASMVal::IMM) {
            shl64(as, dst, a, Imm(48));
            sar64(as, dst, a, Imm(48 + b.imm));
            return;
        }
        sxt16(as, dst, a);
        sar64(as, dst, a, b);
    }

    static inline void sar32(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) {
        if (b.kind == ASMVal::IMM) {
            if (!b.imm)
                return;
            assert(fitsUnsigned<6>(b.imm));
            encodei(as, 0b0011011, dst.gp, a.gp, 0b010000 << 6 | b.imm, 0b101); // srliw
            return;
        }
        encoder(as, 0b0111011, dst.gp, a.gp, b.gp, 0b101, 0b0100000); // sraw
    }

    static inline void sar64(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) {
        if (b.kind == ASMVal::IMM) {
            if (!b.imm)
                return;
            assert(fitsUnsigned<6>(b.imm));
            u8 encodedShift = (b.imm & 0b11111) | (b.imm & 0b100000) << 2 | 0b01 << 5;
            if (dst == a && a.kind == ASMVal::GP && encodecb2(as, 0b10, dst.gp, b.imm, 0b01, 0b100)) // c.srai
                return;
            encodei(as, 0b0010011, dst.gp, a.gp, 0b010000 << 6 | b.imm, 0b101); // srli
            return;
        }
        encoder(as, 0b0110011, dst.gp, a.gp, b.gp, 0b101, 0b0100000); // srl
    }

    static inline void rol8(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) {

    }

    static inline void rol16(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) {

    }

    static inline void rol32(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) {

    }

    static inline void rol64(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) {

    }

    static inline void ror8(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) {

    }

    static inline void ror16(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) {

    }

    static inline void ror32(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) {

    }

    static inline void ror64(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) {

    }

    static inline void mov8(Assembly& as, ASMVal dst, ASMVal src) {
        mov64(as, dst, src);
    }

    static inline void mov16(Assembly& as, ASMVal dst, ASMVal src) {
        mov64(as, dst, src);
    }

    static inline void mov32(Assembly& as, ASMVal dst, ASMVal src) {
        mov64(as, dst, src);
    }

    static inline void mov64(Assembly& as, ASMVal dst, ASMVal src) {
        if (dst == src) return;
        if (src.kind == ASMVal::IMM) {
            materializeImm(as, dst, src.imm);
            return;
        }
        encodecr(as, 0b10, dst.gp, src.gp, 0b1000); // c.mv
    }

    static inline void sxt8(Assembly& as, ASMVal dst, ASMVal src) {
        if (src.kind == ASMVal::IMM)
            return mov64(as, dst, src);
        shl64(as, dst, src, Imm(56));
        sar64(as, dst, dst, Imm(56));
    }

    static inline void sxt16(Assembly& as, ASMVal dst, ASMVal src) {
        if (src.kind == ASMVal::IMM)
            return mov64(as, dst, src);
        shl64(as, dst, src, Imm(48));
        sar64(as, dst, dst, Imm(48));
    }

    static inline void sxt32(Assembly& as, ASMVal dst, ASMVal src) {
        if (src.kind == ASMVal::IMM)
            return mov64(as, dst, src);
        encodeci(as, 0b01, dst.gp, 0, 0b001); // c.addiw
    }

    static inline void zxt8(Assembly& as, ASMVal dst, ASMVal src) {
        if (src.kind == ASMVal::IMM)
            return mov64(as, dst, src);
        and64(as, dst, dst, Imm(0xff)); // andi
    }

    static inline void zxt16(Assembly& as, ASMVal dst, ASMVal src) {
        if (src.kind == ASMVal::IMM)
            return mov64(as, dst, src);
        shl64(as, dst, src, Imm(48));
        shr64(as, dst, dst, Imm(48));
    }

    static inline void zxt32(Assembly& as, ASMVal dst, ASMVal src) {
        if (src.kind == ASMVal::IMM)
            return mov64(as, dst, src);
        shl64(as, dst, src, Imm(32));
        shr64(as, dst, dst, Imm(32));
    }

    static inline void ret(Assembly& as) {
        encodecr(as, 0b10, RA, ZERO, 0b1000); // c.jal ra, x0
    }

    static void stack(Assembly& as, ASMVal dst) {
        sub64(as, ::GP(SP), ::GP(SP), dst);
    }

    static void alloca(Assembly& as, ASMVal dst, ASMVal src) {
        stack(as, src);
        mov64(as, dst, ::GP(SP));
    }

    static void unstack(Assembly& as, ASMVal dst) {
        add64(as, ::GP(SP), ::GP(SP), dst);
    }

    static void leave(Assembly& as) {
        mov64(as, GP(RSP), GP(RBP));
        pop64(as, GP(RBP));
    }

    static inline void trap(Assembly& as) {
        as.code.write<u16>(0); // unimp
    }

    // macro(ADD8,         add8,           0x00,   Size::BITS8,    TERNARY_GP_IMM)             \
    // macro(ADD16,        add16,          0x01,   Size::BITS16,   TERNARY_GP_IMM)             \
    // macro(ADD32,        add32,          0x02,   Size::BITS32,   TERNARY_GP_IMM)             \
    // macro(ADD64,        add64,          0x03,   Size::BITS64,   TERNARY_GP_IMM)             \
    // macro(SUB8,         sub8,           0x04,   Size::BITS8,    TERNARY_GP_IMM)             \
    // macro(SUB16,        sub16,          0x05,   Size::BITS16,   TERNARY_GP_IMM)             \
    // macro(SUB32,        sub32,          0x06,   Size::BITS32,   TERNARY_GP_IMM)             \
    // macro(SUB64,        sub64,          0x07,   Size::BITS64,   TERNARY_GP_IMM)             \
    // macro(MUL8,         mul8,           0x08,   Size::BITS8,    TERNARY_GP_IMM)             \
    // macro(MUL16,        mul16,          0x09,   Size::BITS16,   TERNARY_GP_IMM)             \
    // macro(MUL32,        mul32,          0x0a,   Size::BITS32,   TERNARY_GP_IMM)             \
    // macro(MUL64,        mul64,          0x0b,   Size::BITS64,   TERNARY_GP_IMM)             \
    // macro(SDIV8,        sdiv8,          0x0c,   Size::BITS8,    TERNARY_GP_IMM)             \
    // macro(SDIV16,       sdiv16,         0x0d,   Size::BITS16,   TERNARY_GP_IMM)             \
    // macro(SDIV32,       sdiv32,         0x0e,   Size::BITS32,   TERNARY_GP_IMM)             \
    // macro(SDIV64,       sdiv64,         0x0f,   Size::BITS64,   TERNARY_GP_IMM)             \
    // macro(UDIV8,        udiv8,          0x10,   Size::BITS8,    TERNARY_GP_IMM)             \
    // macro(UDIV16,       udiv16,         0x11,   Size::BITS16,   TERNARY_GP_IMM)             \
    // macro(UDIV32,       udiv32,         0x12,   Size::BITS32,   TERNARY_GP_IMM)             \
    // macro(UDIV64,       udiv64,         0x13,   Size::BITS64,   TERNARY_GP_IMM)             \
    // macro(SREM8,        srem8,          0x14,   Size::BITS8,    TERNARY_GP_IMM)             \
    // macro(SREM16,       srem16,         0x15,   Size::BITS16,   TERNARY_GP_IMM)             \
    // macro(SREM32,       srem32,         0x16,   Size::BITS32,   TERNARY_GP_IMM)             \
    // macro(SREM64,       srem64,         0x17,   Size::BITS64,   TERNARY_GP_IMM)             \
    // macro(UREM8,        urem8,          0x18,   Size::BITS8,    TERNARY_GP_IMM)             \
    // macro(UREM16,       urem16,         0x19,   Size::BITS16,   TERNARY_GP_IMM)             \
    // macro(UREM32,       urem32,         0x1a,   Size::BITS32,   TERNARY_GP_IMM)             \
    // macro(UREM64,       urem64,         0x1b,   Size::BITS64,   TERNARY_GP_IMM)             \
    // macro(NEG8,         neg8,           0x1c,   Size::BITS8,    BINARY_GP)                  \
    // macro(NEG16,        neg16,          0x1d,   Size::BITS16,   BINARY_GP)                  \
    // macro(NEG32,        neg32,          0x1e,   Size::BITS32,   BINARY_GP)                  \
    // macro(NEG64,        neg64,          0x1f,   Size::BITS64,   BINARY_GP)                  \

};

#endif