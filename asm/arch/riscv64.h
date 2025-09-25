#ifndef ASM_ARCH_RISCV64_H
#define ASM_ARCH_RISCV64_H

#include "asm/arch.h"

#if INCLUDE_ARCH_RISCV64

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
            | (opcode & 0b1111111);
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
            | (opcode & 0b1111111);
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
            | (opcode & 0b1111111);
        as.code.writeLE<u32>(word);
    }

    static inline void encodeu(Assembly& as, i8 opcode, mreg rd, i32 imm) {
        // Encodes a U-type instruction.

        ensureAlign(as);

        assert(fitsSigned<20>(imm));
        u32 word = (imm & 0xfffff) << 12
            | (rd & 0b11111) << 7
            | (opcode & 0b1111111);
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

    static inline ASMVal materializeImm32(Assembly& as, ASMVal dst, i32 src) {
        assert(dst.kind == ASMVal::GP);
        if (fitsSigned<6>(src) && encodeci(as, 0b01, dst.gp, src, 0b010)) // c.li
            return dst;
        i32 upper = src >> 12, lower = src & 0xfff;
        if (!upper) {
            encodei(as, 0b0010011, dst.gp, ZERO, lower); // addi (zero)
            return;
        }
        if (fitsSigned<6>(upper))
            encodeci(as, 0b01, dst.gp, upper, 0b011); // c.lui
        else
            encodeu(as, 0b0110111, dst.gp, upper); // lui
        if (src & 0xfff) {
            if (fitsSigned<6>(src & 0xfff))
                encodeci(as, 0b01, dst.gp, src & 0xfff, 0b000); // c.addi
            else
                encodei(as, 0b0010011, dst.gp, dst.gp, src & 0xfff); // addi
        }
        return dst;
    }

    template<typename IsSigned>
    static inline ASMVal materializeImm64(Assembly& as, ASMVal dst, i32 src) {
        assert(dst.kind == ASMVal::GP);
        if (fitsSigned<31>(src) || IsSigned)
            return materializeImm32(as, dst, src);

        assert(!IsSigned); // Otherwise we'd already have exited.
        u32 upper = u32(src) >> 12, lower = u32(src) & 0xfff;
        if (auto shift = ctz32(upper)) {
            // If the upper half has trailing zeroes, then we can just lui
            // then shift.
            u32 smallerUpper = upper >> ctz32(upper);
            if (fitsSigned<6>(upper))
                encodeci(as, 0b01, dst.gp, upper, 0b011); // c.lui
            else
                encodeu(as, 0b0110111, dst.gp, src >> 12); // lui
            shl64(as, dst, dst, Imm(shift));

            // If we have a lower component, then addi that in now.
            if (lower)
                encodei(as, 0b0010011, dst.gp, dst.gp, src & 0xfff); // addi
            return;
        }
        if ((upper & 1) == lower >> 11) {
            // There is a bit in common between the upper and lower components,
            // which lets us do lui + slli + addi

        }
    }

    template<bool IsSigned>
    static inline ASMVal materializeImm64(Assembly& as, ASMVal dst, i64 src) {
        if (fitsSigned<32>(src))
            return materializeImm64(as, dst, i32(src));

        // Scratch shouldn't be live across this, so we clobber it in multiple
        // steps.
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
            b = materializeImm64<true>(as, ::GP(scratch), b.imm);
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
            b = materializeImm32(as, ::GP(scratch), b.imm);
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
            b = materializeImm64<true>(as, ::GP(scratch), b.imm);
        }
        if (dst == b)
            swap(dst, b);
        if (dst == a && encodeca(as, 0b01, dst.gp, b.gp, 0b00, 0b100011)) // c.sub
            return;
        return encoder(as, 0b0110011, dst.gp, a.gp, b.gp, 0b000, 0b0100000); // sub
    }

    static inline void mul8(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) {
        mul32(as, dst, a, b);
    }

    static inline void mul16(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) {
        mul32(as, dst, a, b);
    }

    static inline void mul32(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) {
        if (a.kind == ASMVal::IMM)
            swap(a, b); // Ensure immediate if present is in b.
        if (b.kind == ASMVal::IMM) {
            if (b.imm == 0)
                return mov32(as, dst, ::GP(ZERO));
            if (b.imm == 1)
                return mov32(as, dst, a);
            if (b.imm > 0 && isPowerOfTwo(b.imm))
                return shl32(as, dst, a, ::Imm(intLog2(u32(b.imm))));
            b = materializeImm32(as, ::GP(scratch), b.imm);
        }
        encoder(as, 0b0111011, dst.gp, a.gp, b.gp, 0b000, 0b0000001); // mulw
    }

    static inline void mul64(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) {
        if (a.kind == ASMVal::IMM)
            swap(a, b); // Ensure immediate if present is in b.
        if (b.kind == ASMVal::IMM) {
            if (b.imm == 0)
                return mov64(as, dst, ::GP(ZERO));
            if (b.imm == 1)
                return mov64(as, dst, a);
            if (b.imm > 0 && isPowerOfTwo(b.imm))
                return shl64(as, dst, a, ::Imm(intLog2(u32(b.imm))));
            b = materializeImm64<true>(as, ::GP(scratch), b.imm);
        }
        encoder(as, 0b0110011, dst.gp, a.gp, b.gp, 0b000, 0b0000001); // mul
    }

    template<typename Func>
    static inline void extendOperandsForDiv(Assembly& as, Func&& extfunc, ASMVal& dst, ASMVal& a, ASMVal& b) {
        if (a.kind == ASMVal::IMM)
            extfunc(as, dst, b);
        else if (b.kind == ASMVal::IMM)
            extfunc(as, dst, a);
        else {
            // Both operands must be registers.
            if (a == dst) {
                extfunc(as, dst, a);
                extfunc(as, ::GP(scratch), b);
                a = dst, b = ::GP(scratch);
            } else if (b == dst) {
                extfunc(as, dst, b);
                extfunc(as, ::GP(scratch), a);
                a = ::GP(scratch), b = dst;
            }
        }
    }

    template<bool Is32, bool IsRem, bool IsSigned, i8 Opcode, i8 Funct3, i8 Funct7>
    static inline void encodeDivRem(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) {
        if (a.kind == ASMVal::IMM) {
            assert(b.gp != scratch);
            a = Is32 ? materializeImm32(as, ::GP(scratch), a.imm) : materializeImm64<IsSigned>(as, ::GP(scratch), a.imm);
        }
        if (b.kind == ASMVal::IMM) {
            if (b.imm == 1) {
                if (IsRem) {
                    if (Is32)
                        return mov32(as, dst, ::GP(ZERO));
                    return mov64(as, dst, ::GP(ZERO));
                } else {
                    if (Is32)
                        return mov32(as, dst, a);
                    return mov64(as, dst, a);
                }
            }
            b = Is32 ? materializeImm32(as, ::GP(scratch), b.imm) : materializeImm64<IsSigned>(as, ::GP(scratch), b.imm);
        }
        encoder(as, Opcode, dst.gp, a.gp, b.gp, Funct3, Funct7); // divw
    }

    static inline void sdiv8(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) {
        extendOperandsForDiv(as, sxt8, dst, a, b);
        sdiv64(as, dst, a, b);
    }

    static inline void sdiv16(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) {
        extendOperandsForDiv(as, sxt16, dst, a, b);
        sdiv64(as, dst, a, b);
    }

    static inline void sdiv32(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) {
        encodeDivRem<true, false, 0b0111011, 0b100, 0b0000001>(as, dst, a, b); // divw
    }

    static inline void sdiv64(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) {
        encodeDivRem<false, false, 0b0110011, 0b100, 0b0000001>(as, dst, a, b); // div
    }

    static inline void udiv8(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) {
        extendOperandsForDiv(as, zxt8, dst, a, b);
        udiv64(as, dst, a, b);
    }

    static inline void udiv16(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) {
        extendOperandsForDiv(as, zxt16, dst, a, b);
        udiv64(as, dst, a, b);
    }

    static inline void udiv32(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) {
        encodeDivRem<true, false, 0b0111011, 0b101, 0b0000001>(as, dst, a, b); // divuw
    }

    static inline void udiv64(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) {
        encodeDivRem<false, false, 0b0110011, 0b101, 0b0000001>(as, dst, a, b); // divu
    }

    static inline void srem8(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) {
        extendOperandsForDiv(as, sxt8, dst, a, b);
        srem64(as, dst, a, b);
    }

    static inline void srem16(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) {
        extendOperandsForDiv(as, sxt16, dst, a, b);
        srem64(as, dst, a, b);
    }

    static inline void srem32(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) {
        encodeDivRem<true, true, 0b0111011, 0b110, 0b0000001>(as, dst, a, b); // remw
    }

    static inline void srem64(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) {
        encodeDivRem<false, true, 0b0110011, 0b110, 0b0000001>(as, dst, a, b); // rem
    }

    static inline void urem8(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) {
        extendOperandsForDiv(as, zxt8, dst, a, b);
        urem64(as, dst, a, b);
    }

    static inline void urem16(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) {
        extendOperandsForDiv(as, zxt16, dst, a, b);
        urem64(as, dst, a, b);
    }

    static inline void urem32(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) {
        encodeDivRem<true, true, 0b0111011, 0b111, 0b0000001>(as, dst, a, b); // remuw
    }

    static inline void urem64(Assembly& as, ASMVal dst, ASMVal a, ASMVal b) {
        encodeDivRem<false, true, 0b0110011, 0b111, 0b0000001>(as, dst, a, b); // remu
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
        shl64(as, dst, src, Imm(56));
        sar64(as, dst, dst, Imm(56));
    }

    static inline void sxt16(Assembly& as, ASMVal dst, ASMVal src) {
        shl64(as, dst, src, Imm(48));
        sar64(as, dst, dst, Imm(48));
    }

    static inline void sxt32(Assembly& as, ASMVal dst, ASMVal src) {
        encodeci(as, 0b01, dst.gp, 0, 0b001); // c.addiw
    }

    static inline void zxt8(Assembly& as, ASMVal dst, ASMVal src) {
        and64(as, dst, dst, Imm(0xff)); // andi
    }

    static inline void zxt16(Assembly& as, ASMVal dst, ASMVal src) {
        shl64(as, dst, src, Imm(48));
        shr64(as, dst, dst, Imm(48));
    }

    static inline void zxt32(Assembly& as, ASMVal dst, ASMVal src) {
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

    static inline void trap(Assembly& as) {
        as.code.write<u16>(0); // unimp
    }

    static inline void global(Assembly& as, Symbol sym) {
        as.def(CODE_SECTION, DEF_GLOBAL, sym);
    }

    static inline void local(Assembly& as, Symbol sym) {
        as.def(CODE_SECTION, DEF_LOCAL, sym);
    }
};

#endif

#endif