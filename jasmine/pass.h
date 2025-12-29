#ifndef JASMINE_PASS_H
#define JASMINE_PASS_H

#include "jasmine/type.h"
#include "jasmine/ir.h"
#include "jasmine/data.h"
#include "util/config.h"
#include "util/maybe.h"

struct Assembly;

namespace jasmine {
    #define FOR_EACH_PASS(macro) /* macro(uppercase, lowercase, is_target_specific) */ \
        macro(TYPECHECKING, typechecking, false) \
        macro(CLEANUP, cleanup, false) \
        macro(DOMINATORS, dominators, false) \
        macro(INLINING, inlining, false) \
        macro(NATURAL_LOOPS, natural_loops, false) \
        macro(HOIST_LOOP_INVARIANTS, hoist_loop_invariants, false) \
        macro(UNROLL_LOOPS, unroll_loops, false) \
        macro(SCALAR_REPLACEMENT, scalar_replacement, false) \
        macro(FREQUENCY, frequency, false) \
        macro(SIMPLIFICATION, simplification, false) \
        macro(SSA_CONVERSION, ssa_conversion, false) \
        macro(COMPUTE_EFFECTS, compute_effects, false) \
        macro(CONSTANT_FOLDING, constant_folding, false) \
        macro(STRENGTH_REDUCTION, strength_reduction, false) \
        macro(CSE, cse, false) \
        macro(LIVENESS, liveness, false) \
        macro(INTERFERENCE, interference, false) \
        macro(VALIDATION, validation, false) \
        macro(REGISTER_ALLOCATION, register_allocation, true) \
        macro(STACK_ALLOCATION, stack_allocation, true) \
        macro(LOWERING, lowering, true) \
        macro(FINALIZE_CFG, finalize_cfg, false) \
        macro(CODE_GENERATION, code_generation, true) \
        macro(NONPASS, nonpass, true) \

    enum class Pass {
        #define DEFINE_ENUM(upper, ...) upper,
        FOR_EACH_PASS(DEFINE_ENUM)
        #undef DEFINE_ENUM

        N_PASSES
    };

    extern const i8* PASS_NAMES[(u32)Pass::N_PASSES];

    struct PassContext;

    struct PassTimer {
        static i64 passSums[(u32)Pass::N_PASSES];

        Pass pass;
        PassTimeUnit timeUnit;
        i64 start, end;

        PassTimer(Pass pass, PassContext&, Function&);
        ~PassTimer();

        i64 current();
        static void printSums();
    };

    struct CompileStats {
        static i64 functionsCompiled;
        static i64 blocksCompiled;
        static i64 nodesCompiled;

        static void printStats();
    };

    #define JASMINE_PASS(passName) PassTimer _timer(Pass:: passName, ctx, fn);

    inline bool shouldPrintFor(Pass pass, Function& fn) {
        auto passName = cstring(PASS_NAMES[(u32)pass]);
        bool passMatches = pass == Pass::NONPASS;
        if (pass != Pass::NONPASS) for (u32 i = 0; i < passName.size(); i ++) {
            bool anyMismatch = false;
            for (u32 j = 0; j < config::jasminePassFilter.size(); j ++) {
                if (passName[i + j] != config::jasminePassFilter[j]) {
                    anyMismatch = true;
                    break;
                }
            }
            if (anyMismatch)
                continue;
            passMatches = true;
            break;
        }

        if (!passMatches)
            return false;

        auto name = fn.name();
        for (u32 i = 0; i < name.size(); i ++) {
            bool anyMismatch = false;
            for (u32 j = 0; j < config::jasmineFunctionFilter.size(); j ++) {
                if (name[i + j] != config::jasmineFunctionFilter[j]) {
                    anyMismatch = true;
                    break;
                }
            }
            if (anyMismatch)
                continue;
            return true;
        }
        return false;
    }

    enum IRTrait : u32 {
        TYPECHECK,      // Types of IR instructions have been validated.
        DOMINATORS,     // Dominator relationships between blocks have been computed.
        NATURAL_LOOPS,  // Natural loops have been detected for the control flow graph.
        PINS,           // We know which variables are and aren't pinned by address-of instructions.
        SSA,            // SSA form enforced for the IR.
        EFFECTS,        // Effect ranges have been computed for all effectful nodes.
        LIVENESS,       // Liveness information has been computed for the function.
        CLOBBERS,       // Clobbers and call information have been computed for the necessary instructions.
        CONTAINMENT,    // The containment graph of the function's live ranges has been computed.
        INTERFERENCE,   // The interference graph of the function's live ranges has been computed.
        ALLOCATE,       // All variables have been allocated to native registers or stack slots.
        LOWER,          // All variables are eliminated and replaced with registers or stack slots.
        FINALIZE,       // Blocks within the function have been scheduled.
        CODEGEN,        // All jasmine instructions have been compiled to machine code.
        FREQUENCY,      // Blocks have had approximate frequency values computed.
    };

    enum class LivenessMode : u8 {
        CONSERVATIVE,
        PRECISE
    };

    enum class RegisterAllocationMode : u8 {
        SINGLE_PASS,
        LINEAR,
        ADVANCED
    };

    struct AllocationResult {
        struct ScratchList {
            u8 count = 0;
            mreg elements[3];

            inline u8 size() const {
                return count;
            }

            inline void push(mreg x) {
                assert(count < 3);
                elements[count ++] = x;
            }

            inline mreg operator[](u32 i) const {
                return elements[i];
            }

            inline mreg& operator[](u32 i) {
                return elements[i];
            }

            inline const mreg* begin() const {
                return elements;
            }

            inline const mreg* end() const {
                return elements + count;
            }
        };

        enum ScratchRegisterMode {
            SharedScratchRegisters,
            PerInstructionScratchRegisters
        };

        enum VariableAllocationMode {
            AllocateVariables,
            AllocateLiveRanges,
            AllocateInPlace
        };

        vec<Operand, 16> allocations;
        vec<Operand, 16> edgeAllocations;
        ScratchList sharedFpScratches, sharedGpScratches;
        vec<ScratchList, 8> scratchesByInstruction;
        VariableAllocationMode allocationMode;
        ScratchRegisterMode scratchMode;
        RegSet calleeSaves;
        i32 stack;
        u32 numVars;

        Operand nthScratch(Node node, u32 n) {
            Function& fn = *node.function;
            if (scratchMode == SharedScratchRegisters) {
                if (isCompare(node.opcode()) && n == 0)
                    return fn.gp(sharedGpScratches[0]); // Override integer result on floating-point compare.
                return isFloat(node.type()) ? fn.fp(sharedFpScratches[n]) : fn.gp(sharedGpScratches[n]);
            } else
                return isFloat(node.type()) ? fn.fp(scratchesByInstruction[node.index()][n]) : fn.gp(scratchesByInstruction[node.index()][n]);
        }

        Operand scratch0(Node node) { return nthScratch(node, 0); }
        Operand scratch1(Node node) { return nthScratch(node, 1); }
        Operand scratch2(Node node) { return nthScratch(node, 2); }

        inline Operand& allocationForPinned(i32 var) {
            if (allocationMode == AllocateVariables)
                return allocations[var];
            else
                return allocations[allocations.size() - numVars + var];
        }

        inline Operand allocationForPinned(i32 var) const {
            if (allocationMode == AllocateVariables)
                return allocations[var];
            else
                return allocations[allocations.size() - numVars + var];
        }

        void initialize(PassContext& ctx, Function& function, VariableAllocationMode allocationMode_in, ScratchRegisterMode scratchMode_in);

        inline void clear() {
            sharedGpScratches.count = 0;
            sharedFpScratches.count = 0;
            allocations.clear();
            edgeAllocations.clear();
            scratchesByInstruction.clear();
            calleeSaves = RegSet();
            stack = 0;
            numVars = 0;
        }
    };

    struct TargetSpecificPassInterface {
        virtual i32 offset(Function& fn, TypeIndex aggregate, i32 field) const = 0;
        virtual MaybePair<ASMVal> placeParameter(Function& fn, TypeIndex type, void* state) const = 0;
        virtual MaybePair<ASMVal> placeReturnValue(Function& fn, TypeIndex type, void* state) const = 0;
        virtual Repr repr(TypeIndex type) const = 0;
        virtual void computeClobbers(PassContext& ctx, Function& fn) = 0;
        virtual void lowerTypes(Module* mod) = 0;
        virtual void lower(PassContext& ctx, Function& fn) = 0;
        virtual void allocateStackOnly(PassContext& ctx, Function& fn) = 0;
        virtual void allocateRegisters(PassContext& ctx, Function& fn, RegisterAllocationMode mode) = 0;
        virtual void generateAssembly(PassContext& ctx, Function& fn, Assembly& as) = 0;
        virtual void generateData(Module* module, Assembly& as) = 0;
        virtual PassContext& getContext() = 0;
    };

    template<typename Target>
    struct TargetSpecificPasses : public TargetSpecificPassInterface {
        vec<mreg, 32> gpScratches, fpScratches;
        vec<Repr, 16> reprs;
        vec<i32, 16> structFieldIndices;
        vec<i32, 48> fieldOffsets;
        PassContext* context;

        TargetSpecificPasses(PassContext& context_in);
        virtual i32 offset(Function& fn, TypeIndex aggregate, i32 field) const override;
        virtual MaybePair<ASMVal> placeParameter(Function& fn, TypeIndex type, void* state) const override;
        virtual MaybePair<ASMVal> placeReturnValue(Function& fn, TypeIndex type, void* state) const override;
        virtual Repr repr(TypeIndex type) const override;
        virtual void computeClobbers(PassContext& ctx, Function& fn) override;
        virtual void lowerTypes(Module* mod) override;
        virtual void lower(PassContext& ctx, Function& fn) override;
        virtual void allocateStackOnly(PassContext& ctx, Function& fn) override;
        virtual void allocateRegisters(PassContext& ctx, Function& fn, RegisterAllocationMode mode) override;
        virtual void generateAssembly(PassContext& ctx, Function& fn, Assembly& as) override;
        virtual void generateData(Module* module, Assembly& as) override;

        struct DefInfo {
            Section section;
            Symbol sym;

            inline DefInfo(Section section_in, Symbol sym_in):
                section(section_in), sym(sym_in) {}

            inline void def(Assembly& as) {
                as.def(section, DEF_GLOBAL, Label::fromSym(sym));
            }
        };

        void generateDataValue(Module* module, Assembly& as, bytebuf& buf, Value value, maybe<DefInfo> def);

        inline virtual PassContext& getContext() override {
            return *context;
        }
    };

    struct Error {
        Function* function;
        BlockIndex blockIndex;
        NodeIndex nodeIndex;
        const_slice<i8> message;
    };

    using LiveRangeIndex = i32;

    struct LiveRange {
        u32 var;
        u16 start, end;
        LiveRangeIndex successor, sibling;

        inline u32 length() const {
            return end - start;
        }
    };

    struct Liveness {
        vec<LiveRange, 128> ranges;
        vec<u32, 32> byBlock;

        inline const_slice<LiveRange> rangesInBlock(BlockIndex block) const {
            if (block >= byBlock.size())
                return { nullptr, 0 };
            u32 index = byBlock[block];
            u32 size = block == byBlock.size() - 1 ? ranges.size() - index : byBlock[block + 1] - index;
            return { ranges.begin() + index, size };
        }

        inline Range<LiveRangeIndex> indicesInBlock(BlockIndex block) const {
            if (block >= byBlock.size())
                return { 0, 0 };
            u32 index = byBlock[block];
            u32 size = block == byBlock.size() - 1 ? ranges.size() - index : byBlock[block + 1] - index;
            return { index, index + size };
        }

        inline const LiveRange& operator[](LiveRangeIndex index) const { return ranges[index]; }
        inline LiveRange& operator[](LiveRangeIndex index) { return ranges[index]; }

        template<u32 N>
        inline void addBlock(BlockIndex block, const vec<LiveRange, N>& ranges_in) {
            assert(block == byBlock.size()); // Maybe we can relax this, but it works for now.
            byBlock.push(ranges.size());
            for (const auto& r : ranges_in)
                ranges.push(r);
        }

        inline void clear() {
            ranges.clear();
            byBlock.clear();
        }
    };

    struct AdjacencySet {
        i32 min, max;
        u32 bits[0];

        inline bool operator[](i32 index) const {
            if (index > max ||index < min)
                return false;
            u32 i = index - min;
            return bits[i / 32u] & 1u << i % 32u;
        }

        inline void set(i32 index) {
            assert(index >= min && index <= max);
            u32 i = index - min;
            bits[i / 32u] |= 1u << i % 32u;
        }

        struct Iterator {
            const u32* baseWord;
            i32 i;
            i32 min, max;

            inline Iterator(const u32* baseWord_in, i32 i_in, i32 min_in, i32 max_in):
                baseWord(baseWord_in), i(i_in), min(min_in), max(max_in) {}

            inline bool operator==(const Iterator& other) const {
                return baseWord == other.baseWord && i == other.i && max == other.max;
            }

            inline bool operator!=(const Iterator& other) const {
                return !(*this == other);
            }

            inline i32 operator*() const {
                return i + min;
            }

            inline Iterator& operator++() {
                if (i + min <= max) {
                    u32 mask = 0xfffffffeu << (i & 31);
                    u32 word = baseWord[i / 32] & mask;

                    if (!word) {
                        i &= ~31;
                        do {
                            i += 32;
                            if (i + min > max) {
                                i = max + 1 - min;
                                return *this;
                            }
                            word = baseWord[i / 32];
                        } while (!word);
                    }
                    i = (i & ~31) + ctz32(word);
                    if UNLIKELY(i + min > max)
                        i = max + 1 - min;
                }
                return *this;
            }
        };

        inline bool empty() const {
            return min == -1;
        }

        Iterator begin() const {
            return { bits, 0, min, max };
        }

        Iterator end() const {
            return { bits, max < 0 ? 0 : max + 1 - min, min, max };
        }
    };

    struct Adjacency {
        vec<u32, 128> data;
        vec<u32, 32> indices;

        inline void add(i32 index, const biasedset<256>& adjacency) {
            // TODO: Do we actually even need to bother encoding this? Maybe
            // it's fine to use the existing biasedsets.
            u32 setIndex = data.size();
            u32 numWords = (adjacency.maxp1 - adjacency.min + 31) / 32;
            data.expandBy(2 + numWords);
            AdjacencySet* set = bitcast<AdjacencySet*>(data.data() + setIndex);
            if (adjacency.empty()) {
                set->min = -1;
                set->max = -2;
            } else {
                set->min = adjacency.min;
                set->max = adjacency.maxp1 - 1;
                u64 word;
                for (u32 i = 0; i < numWords; i += 1) {
                    if (i % 2 == 0) {
                        word = adjacency.bits[i / 2];
                        set->bits[i] = u32(word);
                    } else
                        set->bits[i] = word >> 32;
                }
            }
            indices.expandBy(index - (indices.size() - 1));
            indices[index] = setIndex;
        }

        inline const AdjacencySet& operator[](u32 i) const {
            return *bitcast<AdjacencySet*>(data.begin() + indices[i]);
        }

        inline void clear() {
            data.clear();
            indices.clear();
        }
    };

    struct Dominators {
        Adjacency adjacency;
        vec<i32> idoms;

        struct Handle {
            const Dominators& dominators;
            BlockIndex block;

            inline maybe<BlockIndex> immediateDominator() const {
                return dominators.idoms[block] == -1 ? none<BlockIndex>() : some<BlockIndex>(dominators.idoms[block]);
            }

            inline bool dominates(BlockIndex other) const {
                return dominators.adjacency[other][block];
            }

            inline bool isDominatedBy(BlockIndex other) const {
                return dominators.adjacency[block][other];
            }

            inline bool dominates(Block other) const {
                return dominates(other.index());
            }

            inline bool isDominatedBy(Block other) const {
                return isDominatedBy(other.index());
            }

            using iterator = typename bitset<128>::iterator;
            using Iterable = Span<iterator>;
        };

        inline void clear() {
            adjacency.clear();
            idoms.clear();
        }

        inline Handle operator[](Block block) const {
            return { *this, block.index() };
        }

        inline Handle operator[](BlockIndex block) const {
            return { *this, block };
        }
    };

    using LoopIndex = i32;

    struct Loop {
        Loop* parent = nullptr;
        LoopIndex index;
        BlockIndex headerIndex;
        maybe<BlockIndex> preHeaderIndex = none<BlockIndex>();
        bool isLeaf; // This loop is not the parent of any other loop.
        i16 depth;
        vec<u32, 2> backEdges;
        bitset<128> blocks;

        inline Block header(Function& fn) const {
            return fn.block(headerIndex);
        }
    };

    struct TreeSignature {
        u64 sig;

        inline bool matches(TreeSignature other) const {
            return !(sig & other.sig);
        }
    };

    struct TinyIntSet {
        enum Mode : u32 {
            Bits, Range, All
        };

        u32 mode : 2, bits : 15, lo : 15;

        inline TinyIntSet() {
            mode = Bits;
            bits = 0;
        }

        inline TinyIntSet(u32 val) {
            if (val >= 0x7fff) {
                mode = All;
                bits = 2;
            } else {
                mode = Bits;
                lo = val, bits = 1;
            }
        }

        inline TinyIntSet(u32 lo, u32 hi) {
            if (lo >= 0x7ff0 || hi >= 0x7ff0)
                mode = All;
            else if (hi - lo <= 15) {
                mode = Bits;
                this->lo = lo;
                this->bits = 0xffff >> (16 - (hi - lo));
            } else {
                mode = Range;
                this->lo = lo;
                this->bits = hi;
            }
        }

        inline static u32 maxbit(u32 word) {
            assert(word);
            return 32 - clz32(word);
        }

        inline void add(TinyIntSet other) {
            if (mode == All)
                return;
            if (other.mode == All) {
                mode = All;
                bits = 2; // Nonzero for easy emptiness checking.
                return;
            }
            if (bits == 0) {
                *this = other;
                return;
            }
            if (other.bits == 0)
                return;
            if (mode == Bits && other.mode == Bits) {
                if (abs(lo - other.lo) < 15) {
                    u32 newlo = min(lo, other.lo);
                    u32 newhi = max(lo + maxbit(bits), other.lo + maxbit(other.bits));
                    if (newhi - newlo <= 15) {
                        bits = (bits << (lo - newlo)) | (other.bits << (other.lo - newlo));
                        lo = newlo;
                        return;
                    }
                }
            }
            if (mode == Bits) {
                mode = Range;
                bits = lo + maxbit(bits);
            }
            u32 otherlo = other.lo;
            u32 otherhi = other.bits;
            if (other.mode == Bits)
                otherhi = otherlo + maxbit(otherhi);
            lo = min(lo, other.lo);
            bits = max(bits, otherhi);
        }

        inline bool intersects(TinyIntSet other) const {
            if (!bits || !other.bits)
                return false;
            if (mode == All || other.mode == All)
                return true;
            if (mode == Bits && other.mode == Bits) {
                if (abs(lo - other.lo) < 15) {
                    u32 newlo = min(lo, other.lo);
                    return (bits << (lo - newlo)) & (other.bits << (other.lo - newlo));
                }
            }
            u32 mylo = lo, myhi = bits;
            if (mode == Bits)
                myhi = mylo + maxbit(bits);
            u32 otherlo = other.lo, otherhi = other.bits;
            if (other.mode == Bits)
                otherhi = otherlo + maxbit(otherhi);
            return !(otherhi <= mylo || otherlo >= myhi);
        }

        inline static TinyIntSet all() {
            TinyIntSet s;
            s.mode = All;
            s.bits = 2;
            return s;
        }

        inline bool empty() const {
            return mode == Bits && bits == 0;
        }
    };

    struct Effect {
        TinyIntSet classes, fields;

        inline Effect() {}

        inline Effect(TinyIntSet classes_in, TinyIntSet fields_in):
            classes(classes_in), fields(fields_in) {}

        inline bool intersects(Effect other) const {
            return classes.intersects(other.classes) && fields.intersects(other.fields);
        }

        inline bool empty() const {
            return classes.empty() && fields.empty();
        }

        inline void add(i32 aliasingClass) {
            classes.add(TinyIntSet(aliasingClass));
            fields.add(TinyIntSet::all());
        }

        inline void add(i32 aliasingClass, i32 field) {
            classes.add(TinyIntSet(aliasingClass));
            fields.add(TinyIntSet(field));
        }

        inline void add(Effect other) {
            classes.add(other.classes);
            fields.add(other.fields);
        }
    };

    struct EffectTable {
        // Effect tables allow the mapping of an index type (which can be a
        // NodeIndex, BlockIndex, EdgeIndex, etc - really any integer) to a set
        // of effects corresponding to that index. To save memory, since we
        // don't expect most entries to have nontrivial effects (at least for
        // high-N index systems, like for nodes - blocks and loops are more
        // likely to have effects but there's fewer of them), we initialize
        // most indices as having no effects, and store actual effect scopes
        // for only those indices that have nontrivial effect instances. We
        // also only store one effect scope whenever possible. Most effectful
        // nodes trivially only read or write, not both. But there are a few
        // instructions, namely calls, that can have complex effects reading
        // and writing different parameters. Again to save memory in the common
        // case, we only store a pair of effect scopes in the case we have an
        // index with EntryKind::ReadWrite, in which case the reads are stored
        // at the mapped-to index, and the writes stored immediately after.

        enum EntryKind {
            ReadBit = 1, WriteBit = 2,
            None = 0,
            ReadOnly = ReadBit,
            WriteOnly = WriteBit,
            ReadWrite = ReadBit | WriteBit
        };

        struct Mapping {
            EntryKind kind : 2;
            u32 effectIndex : 30;
        };

        vec<Mapping, 32> indexMapping;
        vec<Effect, 16> effects;

        inline void clear() {
            indexMapping.clear();
            effects.clear();
        }

        inline void init(u32 numIndices) {
            indexMapping.expandTo(numIndices, Mapping { .kind = None, .effectIndex = 0 });
        }

        inline void addReadOnly(u32 index, Effect effect) {
            assert(indexMapping[index].kind == None);
            indexMapping[index].kind = ReadOnly;
            indexMapping[index].effectIndex = effects.size();
            effects.push(effect);
        }

        inline void addWriteOnly(u32 index, Effect effect) {
            assert(indexMapping[index].kind == None);
            indexMapping[index].kind = WriteOnly;
            indexMapping[index].effectIndex = effects.size();
            effects.push(effect);
        }

        inline void addReadWrite(u32 index, Effect reads, Effect writes) {
            assert(indexMapping[index].kind == None);
            indexMapping[index].kind = ReadWrite;
            indexMapping[index].effectIndex = effects.size();
            effects.push(reads);
            effects.push(writes);
        }

        inline bool hasReads(u32 index) const {
            return indexMapping[index].kind & ReadBit;
        }

        inline bool hasWrites(u32 index) const {
            return indexMapping[index].kind & WriteBit;
        }

        inline Effect readsOf(u32 index) const {
            auto mapping = indexMapping[index];
            if (!(mapping.kind & ReadBit))
                return Effect();
            return effects[mapping.effectIndex];
        }

        inline Effect writesOf(u32 index) const {
            auto mapping = indexMapping[index];
            if (!(mapping.kind & WriteBit))
                return Effect();
            return effects[mapping.effectIndex + (mapping.kind & ReadBit)];
        }
    };

    struct Pins {
        bitset<128> pins;
        u32 numPinned;

        inline void pin(i32 variable) {
            numPinned += pins.add(variable) ? 1 : 0;
        }

        inline bool isPinned(i32 variable) const {
            return pins[variable];
        }

        inline u32 count() const {
            return numPinned;
        }

        void clear() {
            numPinned = 0;
            pins.clear();
        }
    };

    template<typename T, IRTrait Trait>
    struct PassBound {
        PassContext* ctx;
        mutable maybe<T> item;

        inline PassBound(PassContext* ctx_in):
            ctx(ctx_in) {}

        inline operator bool() const;
        inline T& get();
        inline const T& get() const;
        inline T& operator*() { return get(); }
        inline const T& operator*() const { return get(); }
        inline T* operator->() { return &get(); }
        inline const T* operator->() const { return &get(); }
        inline void clear() {
            if (item)
                item->clear();
        }
    };

    struct CallSite {
        vec<Operand, 6> parameters;
        Operand returnValue;
        NodeIndex node;
        RegSet liveAcross;
    };

    struct PassContext {
        TargetSpecificPassInterface* targetSpecificPasses;
        u32 traits;
        i32 stack;
        vec<Error> errors;
        using Schedule = vec<BlockIndex>;
        PassBound<Schedule, IRTrait::FINALIZE> schedule;
        PassBound<Dominators, IRTrait::DOMINATORS> dominators;
        using Loops = vec<Loop*, 8>;
        using BlockLoops = vec<LoopIndex, 8>;
        PassBound<Loops, IRTrait::NATURAL_LOOPS> loops;
        PassBound<BlockLoops, IRTrait::NATURAL_LOOPS> blockLoops;
        using Defs = vec<i32, 128>;
        PassBound<Defs, IRTrait::SSA> defs;
        PassBound<Pins, IRTrait::PINS> pins;
        using TreeSignatures = vec<TreeSignature, 64>;
        TreeSignatures treeSignatures;
        PassBound<EffectTable, IRTrait::EFFECTS> effectsByInstruction;
        PassBound<EffectTable, IRTrait::EFFECTS> effectsByBlock;
        PassBound<EffectTable, IRTrait::EFFECTS> effectsByLoop;
        PassBound<Liveness, IRTrait::LIVENESS> liveness;
        PassBound<Adjacency, IRTrait::CONTAINMENT> containment;
        PassBound<Adjacency, IRTrait::INTERFERENCE> interference;
        PassBound<Adjacency, IRTrait::INTERFERENCE> edgeInterference;
        using ClobberList = vec<RegSet>;
        using ScratchCounts = vec<u8>;
        using CallList = vec<i32>;
        using CallSites = vec<CallSite>;
        PassBound<ClobberList, IRTrait::CLOBBERS> clobberList;
        PassBound<ScratchCounts, IRTrait::CLOBBERS> scratchesPerNode;
        PassBound<CallList, IRTrait::CLOBBERS> callInsns;
        PassBound<CallSites, IRTrait::CLOBBERS> callSites;
        PassBound<AllocationResult, IRTrait::ALLOCATE> allocations;
        using BlockFrequency = vec<u8>;
        PassBound<BlockFrequency, IRTrait::FREQUENCY> blockFrequency;
        u32 passCount = 0;
        void* cachedRegalloc = nullptr;
        bool rulesInited = false;

        inline PassContext():
            schedule(this),
            dominators(this),
            loops(this),
            blockLoops(this),
            defs(this),
            pins(this),
            effectsByInstruction(this),
            effectsByBlock(this),
            effectsByLoop(this),
            liveness(this),
            containment(this),
            interference(this),
            edgeInterference(this),
            clobberList(this),
            scratchesPerNode(this),
            callInsns(this),
            callSites(this),
            allocations(this),
            blockFrequency(this) {}

        inline void did(IRTrait trait) {
            traits |= (1u << trait);
        }

        inline bool has(IRTrait trait) {
            return traits & (1u << trait);
        }

        inline void require(IRTrait trait) {
            assert(has(trait));
        }

        template<typename... Args>
        inline void require(IRTrait trait, Args... args) {
            assert(has(trait));
            require(args...);
        }

        inline void invalidate(IRTrait trait) {
            traits &= ~(1u << trait);
        }

        inline void reset() {
            traits = 0;
            errors.clear();
            stack = 0;
            treeSignatures.clear();
            dominators.clear();
            if (loops)
                for (Loop* loop : *loops)
                    delete loop;
            loops.clear();
            blockLoops.clear();
            schedule.clear();
            defs.clear();
            effectsByInstruction.clear();
            effectsByBlock.clear();
            effectsByLoop.clear();
            liveness.clear();
            clobberList.clear();
            scratchesPerNode.clear();
            callInsns.clear();
            callSites.clear();
            interference.clear();
            edgeInterference.clear();
            containment.clear();
            blockFrequency.clear();
            allocations.clear();
            pins.clear();
        }

        inline bool errored() const {
            return errors.size();
        }

        template<typename... Args>
        inline void error(Block block, NodeIndex node, const Args&... args) {
            errors.push({ block.function, block.index(), node, tostring(args...) });
        }

        template<typename... Args>
        inline void error(Block block, Node node, const Args&... args) {
            errors.push({ block.function, block.index(), node.index(), tostring(args...) });
        }

        void printErrors(fd file);
    };

    template<typename T, IRTrait Trait>
    inline PassBound<T, Trait>::operator bool() const {
        return ctx->has(Trait);
    }

    template<typename T, IRTrait Trait>
    inline T& PassBound<T, Trait>::get() {
        assert(ctx->has(Trait));
        if (!item)
            item = some<T>();
        return *item;
    }

    template<typename T, IRTrait Trait>
    inline const T& PassBound<T, Trait>::get() const {
        assert(ctx->has(Trait));
        if (!item)
            item = some<T>();
        return *item;
    }

    struct ScheduleLogger {
        const Function& fn;
        const PassContext::Schedule& schedule;
    };

    /*
     * Infers types statically for all variables used in the function, and
     * verifies that all instructions in the function are well-typed. Reports
     * errors in case of incorrect types, which can be observed by checking
     * ctx.errored() (true if at least one error occurred).
     *
     * Generally, typechecking is the first pass run in the compilation
     * sequence, and all other passes assume both:
     *  - That the function is well-typed.
     *  - That no pass can create incorrectly-typed code.
     * The latter tenet is more loosely held though; passes should be able to
     * break statically correct types if necessary. But they must maintain
     * semantics compatible with the initial type checking.
     */
    void typecheck(PassContext& ctx, Function& fn);

    /*
     * Infers types statically for all variables used in the function, but does
     * no verification. Use this carefully, only if you really really want
     * faster compilation, at the cost of potentially undefined behavior on
     * ill-typed programs.
     */
    void assignTypesUnchecked(PassContext& ctx, Function& fn);

    /*
     * Analyzes the control flow graph and computes dominators and immediate
     * dominators. This is used in some other passes to figure out the optimal
     * places to put certain nodes, or aid in analyzing data flow.
     */
    void findDominators(PassContext& ctx, Function& fn);

    /*
     * Records which instructions define which variables.
     */
    void recordDefs(PassContext& ctx, Function& fn);

    /*
     * Records which variables are pinned by an address-of instruction.
     */
    void computePins(PassContext& ctx, Function& fn);

    /*
     * Puts the function into SSA form. In Jasmine, this takes the form of
     * renaming variables that are assigned multiple times for each assignment,
     * and propagating the new variable to all dominated uses. As part of this,
     * control flow edges between blocks are populated with moves, to handle
     * data flowing between blocks and unification.
     */
    void enforceSSA(PassContext& ctx, Function& fn);

    /*
     * Performs "relatively" cheap clean-up of the IR. The main purpose of this
     * pass is to essentially garbage-collect the IR graph, removing no-ops and
     * never-referenced blocks, and edges and renumbering the remaining nodes
     * appropriately. Does not do any further analysis to detect semantically
     * dead code, or unused variables. See simplify() for this behavior.
     */
    void cleanup(PassContext& ctx, Function& fn);

    /*
     * Attempts to "simplify" the IR, by removing semantically unreachable
     * blocks and unused variables, and combines blocks together when possible.
     * Finally, performs all the same simplifications as the cleanup() pass.
     */
    void simplify(PassContext& ctx, Function& fn);

    /*
     * Folds constant expressions and propagates constant values from their def
     * to all uses.
     */
    void foldConstants(PassContext& ctx, Function& fn);

    /*
     * Discover loop structures within the function and store them in the pass
     * context. We generally use this as a prerequisite for loop analysis
     * passes.
     */
    void findNaturalLoops(PassContext& ctx, Function& fn);

    /*
     * Decompose aggregate variables that are not pinned to memory locations
     * into one or more variables of scalar type.
     */
    void scalarReplacement(PassContext& ctx, Function& fn);

    /*
     * Hoist loop-invariant instructions outside of loops.
     */
    void hoistLoopInvariantCode(PassContext& ctx, Function& fn);

    /*
     * Tries to unroll loops to reduce branching and create more optimizable
     * blocks.
     */
    void unrollLoops(PassContext& ctx, Function& fn);

    /*
     * Instantiates direct calls to known functions into the caller, instead of
     * actually calling out. Heuristically determines whether to inline each
     * function based on function length, complexity, and recursion depth.
     */
    void inlineCalls(PassContext& ctx, Function& fn);

    /*
     * Computes effect regions and effects for all effectful instructions.
     */
    void computeEffects(PassContext& ctx, Function& fn);

    /*
     * Performs tree pattern matching over the instructions in the function and
     * tries to simplify common patterns to a cheaper or more canonical form.
     */
    void reduceStrength(PassContext& ctx, Function& fn);

    /*
     * Performs SSA-based, non-effect-aware common subexpression elimination.
     */
    void eliminateCommonSubexpressions(PassContext& ctx, Function& fn);

    /*
     * Computes liveness information for each variable in the function, which
     * takes the form of a list of live ranges. Each live range maps back to
     * its source variable, which is used to hint register allocation choices.
     */
    void liveness(PassContext& ctx, Function& fn);

    /*
     * Computes the containment graph of previously computed live ranges.
     */
    void containment(PassContext& ctx, Function& fn);

    /*
     * Validates that the IR is sound after it has been lowered by some target-
     * specific lowering code, like stackalloc() or regalloc(). Ensures that
     * operations have a valid form - no more than one memory operand, register
     * operands instead of variables, and other requirements on an opcode-by-
     * opcode basis.
     */
    void validateAfterLowering(PassContext& ctx, Function& fn);

    /*
     * Finalizes the control-flow graph after it has been lowered to a specific
     * target. This implies that SSA has been broken, with all control flow edges
     * rendered out into moves. This pass picks a final block schedule, and does
     * trivial block combining to elide unnecessary branches.
     */
    void finalizeCFG(PassContext& ctx, Function& fn);

    /*
     * Runs a set of default passes based on the provided optimization level.
     * Lower optimization levels run fewer optimizations, with O0 doing almost
     * no optimization. If validate is true, also runs typechecking and lower
     * level instruction validation passes. Writes results to the provided
     * assembly object.
     */
    void compileWithOptimizations(Module* module, Assembly& as, u32 optLevel, bool validate);

    /*
     * Same as compileWithOptimizations but for a single function at a time.
     * Discards the function afterwards, meaning this architecture can't be
     * used for inlining. Used to avoid allocating a whole module at once.
     */
    void compileFunctionOnly(Function* function, Assembly& as, u32 optLevel, bool validate);

    /*
     * Used in conjunction with compileFunctionOnly to generate any module-
     * scoped data into the provided assembly needed to support any functions
     * generated into it.
     */
    void compileModuleOnly(Module* module, Assembly& as, u32 optLevel, bool validate);

    template<typename IO, typename Format = Formatter<IO>>
    inline IO format_impl(IO io, const ScheduleLogger& s) {
        io = format(io, s.fn.name(), '(');
        for (const auto& [i, p] : enumerate(s.fn.parameters)) {
            if (i)
                io = format(io, ", ");
            io = format(io, OperandLogger { s.fn, p.operand }, ": ", TypeLogger(*s.fn.mod, p.type));
        }
        io = format(io, ") {\n");
        for (BlockIndex b : s.schedule)
            io = format(io, s.fn.block(b));
        io = format(io, "}\n");
        return io;
    }

    struct FunctionInPass {
        PassContext& passContext;
        const Function& function;
        Pass pass;
        bool isFirst = false, isLast = false;
    };

    void beginDOT(PassContext& ctx, Function& fn);
    void finishDOT(PassContext& ctx, Function& fn);

    struct EscapedDOT {
        const_slice<i8> str;

        inline EscapedDOT(const_slice<i8> str_in): str(str_in) {}
    };

    template<typename IO, typename Format = Formatter<IO>>
    inline IO format_impl(IO io, const EscapedDOT& escaped) {
        auto str = escaped.str;
        const i8* iter = str.data();
        rune r;
        while (iter != str.end()) {
            iter = utf8_decode_forward(iter, &r);
            if (!utf8_is_letter(r) && !utf8_is_digit(r) && r != '_')
                io = format(io, '_');
            else
                io = format(io, r);
        }
        return io;
    }

    template<typename IO, typename Format = Formatter<IO>>
    inline IO format_impl(IO io, const DOT<FunctionInPass>& dot) {
        const auto& fip = dot.value;
        const auto& fn = fip.function;
        auto& ctx = fip.passContext;
        const auto& pass = fip.pass;

        const_slice<i8> passName = cstring(PASS_NAMES[(u32)pass]);
        if (fip.isFirst) passName = cstring("before_opts");
        if (fip.isLast) passName = cstring("after_opts");
        io = format(io, "    subgraph cluster_margin_", EscapedDOT(fn.name()), "_", ctx.passCount, "_", passName, " {\n");
        io = format(io, "        graph [style=\"invis\", margin=10];\n");
        io = format(io, "        subgraph cluster_", EscapedDOT(fn.name()), "_", ctx.passCount, "_", passName, " {\n");
        io = format(io, "            graph [fontname=\"Courier New\", group=\"", fn.name(), "\", style=\"dashed\", label=\"", fn.name(), "_", ctx.passCount, "_", passName, "\", margin=40];\n");
        io = format(io, "            node [fontname=\"Courier New\", shape=\"rect\"];\n");
        io = format(io, "            edge [fontname=\"Courier New\", overlap=false];\n");
        io = format(io, "            entry_", ctx.passCount, " [label=\"", fn.name(), "(");
        bool first = true;
        for (const auto& p : fn.parameters) {
            if (!first)
                io = format(io, ", ");
            first = false;
            io = format(io, TypeLogger { fn, p.type }, " ", OperandLogger { fn, p.operand });
        }
        io = format(io, ")\"];\n");
        io = format(io, "            entry_", ctx.passCount, " -> bb", fn.entrypoint, "_", ctx.passCount, "\n");
        vec<BlockIndex> order;
        fn.scheduleInReversePostorder(order);
        for (BlockIndex bi : order) {
            Block block = fn.block(bi);
            io = format(io, "            bb", block.index(), "_", ctx.passCount, " [label=\".bb", block.index(), "\\n");
            for (Node node : block.nodes()) {
                io = format(io, "\\n", node);
            }
            io = format(io, "\"];\n");
            for (Edge edge : block.successors()) {
                io = format(io, "            bb", edge.srcIndex(), "_", ctx.passCount, " -> bb", edge.destIndex(), "_", ctx.passCount, " [label=\" ");
                bool first = true;
                for (Move move : edge.moves()) {
                    if (!first)
                        io = format(io, "\\n");
                    first = false;
                    io = format(io, OperandLogger { fn, move.dest }, " <- ", OperandLogger { fn, move.src });
                }
                io = format(io, " \"];\n");
            }
        }
        io = format(io, "        }\n");
        ctx.passCount ++;
        return format(io, "    }\n");
    }
}

#endif