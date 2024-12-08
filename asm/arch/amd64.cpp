#include "asm/arch/amd64.h"
#include "rt/def.h"
#include "util/malloc.h"

const TargetDesc AMD64LinuxAssembler::DESC = TargetDesc(OS_LINUX, ARCH_AMD64);

struct PlacementState {
    vec<mreg, 8> argumentGPs, argumentFPs;
    u32 stackOffset;

    mreg takeGP() {
        assert(argumentGPs.size());
        return argumentGPs.pop();
    }

    mreg takeFP() {
        assert(argumentFPs.size());
        return argumentFPs.pop();
    }

    u32 allocateStack(u32 size, u32 alignment) {
        while (stackOffset % alignment) stackOffset ++;
        u32 before = stackOffset;
        stackOffset += size;
        return before;
    }
};

void* AMD64LinuxAssembler::start_placing_parameters() {
    auto p = new PlacementState { {}, {}, 0 };
    for (i32 i = 5; i >= 0; i --)
        p->argumentGPs.push(GP_ARGS[i]);
    for (i32 i = 7; i >= 0; i --)
        p->argumentFPs.push(FP_ARGS[i]);
    return p;
}

MaybePair<ASMVal> AMD64LinuxAssembler::place_scalar_parameter(void* state, Repr scalar) {
    auto& placement = *(PlacementState*)state;
    switch (scalar.kind()) {
        case Size::BITS8:
        case Size::BITS16:
        case Size::BITS32:
        case Size::BITS64:
            if (placement.argumentGPs.size() == 0)
                return Mem(RSP, placement.allocateStack(scalar.size(), scalar.alignment()));
            else
                return GP(placement.takeGP());
        case Size::FLOAT32:
        case Size::FLOAT64:
        case Size::VECTOR:
            if (placement.argumentFPs.size() == 0)
                return Mem(RSP, placement.allocateStack(scalar.size(), scalar.alignment()));
            else
                return FP(placement.takeFP());
        case Size::MEMORY:
            unreachable("Memory operands should be passed as aggregates.");
    }
}

MaybePair<ASMVal> AMD64LinuxAssembler::place_aggregate_parameter(void* state, const_slice<Repr> members) {
    if (members.size() == 1 && members[0].kind() != Size::MEMORY)
        return place_scalar_parameter(state, members[0]);

    auto& placement = *(PlacementState*)state;
    u32 totalSize = 0, maxAlignment = 1;
    bool sawFloatInFirst = false, sawFloatInSecond = false;
    bool sawIntInFirst = false, sawIntInSecond = false;
    for (Repr r : members) {
        while (totalSize % r.alignment()) ++ totalSize;
        maxAlignment = max<u8>(maxAlignment, r.alignment());
        totalSize += r.size();
        if (r.kind() == Size::FLOAT32 || r.kind() == Size::FLOAT64 || r.kind() == Size::VECTOR) {
            if (totalSize <= 8)
                sawFloatInFirst = true;
            else if (totalSize <= 16)
                sawFloatInSecond = true;
        } else {
            if (totalSize <= 8)
                sawIntInFirst = true;
            else if (totalSize <= 16)
                sawIntInSecond = true;
        }
    }
    if (totalSize <= 8) {
        if (sawFloatInFirst && !sawIntInFirst && placement.argumentFPs.size())
            return FP(placement.takeFP());
        else if (placement.argumentGPs.size())
            return GP(placement.takeGP());
        else
            return Mem(RSP, placement.allocateStack(totalSize, maxAlignment));
    } else if (totalSize <= 16) {
        ASMVal first, second;
        if (sawFloatInFirst && !sawIntInFirst && placement.argumentFPs.size())
            first = FP(placement.takeFP());
        else if (placement.argumentGPs.size())
            first = GP(placement.takeGP());
        else
            return Mem(RSP, placement.allocateStack(totalSize, maxAlignment));

        if (sawFloatInSecond && !sawIntInSecond && placement.argumentFPs.size())
            second = FP(placement.takeFP());
        else if (placement.argumentGPs.size())
            second = GP(placement.takeGP());
        else
            return Mem(RSP, placement.allocateStack(totalSize - 8, maxAlignment));
        return { first, second };
    } else
        return Mem(RSP, placement.allocateStack(totalSize, maxAlignment));
}

void AMD64LinuxAssembler::finish_placing_parameters(void* state) {
    delete (PlacementState*)state;
}

MaybePair<ASMVal> AMD64LinuxAssembler::place_scalar_return_value(void* state, Repr scalar) {
    switch (scalar.kind()) {
        case Size::BITS8:
        case Size::BITS16:
        case Size::BITS32:
        case Size::BITS64:
            return GP(RAX);
        case Size::FLOAT32:
        case Size::FLOAT64:
        case Size::VECTOR:
            return FP(XMM0);
        case Size::MEMORY:
            unreachable("Memory operands should be passed as aggregates.");
    }
}

MaybePair<ASMVal> AMD64LinuxAssembler::place_aggregate_return_value(void* state, const_slice<Repr> members) {
    if (members.size() == 1)
        return place_scalar_return_value(state, members[0]);

    u32 totalSize = 0, maxAlignment = 1;
    bool sawFloatInFirst = false, sawFloatInSecond = false;
    bool sawIntInFirst = false, sawIntInSecond = false;
    for (Repr r : members) {
        while (totalSize % r.alignment()) ++ totalSize;
        maxAlignment = max<u8>(maxAlignment, r.alignment());
        totalSize += r.size();
        if (r.kind() == Size::FLOAT32 || r.kind() == Size::FLOAT64 || r.kind() == Size::VECTOR) {
            if (totalSize <= 8)
                sawFloatInFirst = true;
            else if (totalSize <= 16)
                sawFloatInSecond = true;
        } else {
            if (totalSize <= 8)
                sawIntInFirst = true;
            else if (totalSize <= 16)
                sawIntInSecond = true;
        }
    }
    if (totalSize <= 8) {
        if (sawFloatInFirst && !sawIntInFirst)
            return FP(XMM0);
        else
            return GP(RAX);
    } else if (totalSize <= 16) {
        ASMVal first, second;
        if (sawFloatInFirst && !sawIntInFirst)
            first = FP(XMM0);
        else
            first = GP(RAX);

        if (sawFloatInSecond && !sawIntInSecond)
            second = FP(sawFloatInFirst && !sawIntInFirst ? XMM1 : XMM0);
        else
            second = GP(!sawFloatInFirst || sawIntInFirst ? RDX : RAX);
        return { first, second };
    } else {
        // Big return values mean the caller should allocate a buffer and pass
        // its address as an implied first argument. The callee should then
        // return that same pointer back to the caller after writing the return
        // value into it. We do a bit of a hacky solution here for this case,
        // returning a memory location (to distinguish this location from a
        // single register or register pair), but with the base equal to the
        // register the caller passes the pointer into, and with the offset
        // equal to the register the callee should return it through.
        return { Mem(((PlacementState*)state)->takeGP(), RAX), {} };
    }
}
