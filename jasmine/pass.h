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
        macro(LIVENESS, liveness, false) \
        macro(CONTAINMENT, containment, false) \
        macro(VALIDATION, validation, false) \
        macro(REGISTER_ALLOCATION, register_allocation, true) \
        macro(STACK_ALLOCATION, stack_allocation, true) \
        macro(LOWERING, lowering, true) \
        macro(FINALIZE_CFG, finalize_cfg, false) \
        macro(CODE_GENERATION, code_generation, true)

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
        static void printPassTime(Pass pass);
        static void printSums();
    };

    struct CompileStats {
        static i64 functionsCompiled;
        static i64 blocksCompiled;
        static i64 nodesCompiled;

        static void printStats();
    };

    #define JASMINE_PASS(passName) PassTimer _timer(Pass:: passName, ctx, fn);

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
            AllocateLiveRanges
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
                as.def(section, DEF_GLOBAL, sym);
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
            return { &ranges[index], size };
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

        template<typename T>
        inline void add(i32 index, const T& adjacency) {
            i32 min = -1, max = -1;
            for (i32 i : adjacency) {
                if (min == -1)
                    min = i;
                max = i;
            }
            u32 bitsNeeded = divideRoundingUp(max - min + 1, 32);
            u32 setIndex = data.size();
            data.expandBy(2 + bitsNeeded);
            AdjacencySet* set = bitcast<AdjacencySet*>(data.data() + setIndex);
            set->min = min;
            set->max = max;
            memory::fill(&set->bits, 0, bitsNeeded * sizeof(u32));
            for (u32 i : adjacency) // Could be sped up, but it's probably unnecessary.
                set->set(i);

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

    using LoopIndex = u32;

    struct Loop {
        Loop* parent = nullptr;
        LoopIndex index;
        BlockIndex headerIndex;
        maybe<BlockIndex> preHeaderIndex = none<BlockIndex>();
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
        bool rulesInited = false;

        inline PassContext():
            schedule(this),
            dominators(this),
            loops(this),
            blockLoops(this),
            defs(this),
            pins(this),
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
     * Deduplicates expressions to reduce redundant computation in the program.
     */
    void eliminateCommonSubexpressions(PassContext& ctx, Function& fn);

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

    template<typename IO, typename Format = Formatter<IO>>
    inline IO format_impl(IO io, const DOT<FunctionInPass>& dot) {
        const auto& fip = dot.value;
        const auto& fn = fip.function;
        auto& ctx = fip.passContext;
        const auto& pass = fip.pass;

        const_slice<i8> passName = cstring(PASS_NAMES[(u32)pass]);
        if (fip.isFirst) passName = cstring("before_opts");
        if (fip.isLast) passName = cstring("after_opts");
        io = format(io, "    subgraph cluster_margin_", fn.name(), "_", ctx.passCount, "_", passName, " {\n");
        io = format(io, "        graph [style=\"invis\", margin=10];\n");
        io = format(io, "        subgraph cluster_", fn.name(), "_", ctx.passCount, "_", passName, " {\n");
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