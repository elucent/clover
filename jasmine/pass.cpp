#include "jasmine/pass.h"
#include "jasmine/mod.h"
#include "util/init.h"

namespace jasmine {
    const i8* PASS_NAMES[(u32)Pass::N_PASSES] = {
        #define DEFINE_STRING(upper, lower, ...) #lower,
        FOR_EACH_PASS(DEFINE_STRING)
        #undef DEFINE_STRING
    };

    static fd DOTFile;

    PassTimer::PassTimer(Pass pass_in, PassContext& ctx, Function& fn):
        pass(pass_in), timeUnit(config::jasminePassTimingUnit) {
        if UNLIKELY(config::printJasmineBeforeEachPass && shouldPrintFor(pass, fn)) {
            println("\n=== IR for function ", fn.name(), " before ", PASS_NAMES[(u32)pass], " ===\n");
            if (ctx.has(FINALIZE))
                println(ScheduledFunction { *ctx.schedule, fn });
            else
                println(fn);
        }
        if UNLIKELY(config::printJasmineDOTBeforeEachPass && shouldPrintFor(pass, fn)) {
            write(DOTFile, FunctionInPass {
                .passContext = ctx,
                .function = fn,
                .pass = pass,
                .isFirst = false,
                .isLast = false
            });
            flush(DOTFile);
        }
        if LIKELY(!config::printJasminePassTimes)
            return;
        start = current();
    }

    PassTimer::~PassTimer() {
        if LIKELY(!config::printJasminePassTimes)
            return;
        i64 intDiff = current() - start;
        passSums[(u32)pass] += intDiff;
    }

    i64 PassTimer::current() {
        switch (timeUnit) {
            case PassTimeUnit::Seconds:
            case PassTimeUnit::Milliseconds:
            case PassTimeUnit::Microseconds:
            case PassTimeUnit::Nanoseconds:
                return time::nanos();
            case PassTimeUnit::Ticks:
                return time::ticks();
        }
    }

    i64 PassTimer::passSums[(u32)Pass::N_PASSES];

    void PassTimer::printSums() {
        vec<entry<const_slice<i8>, i64>> times;

        #define RECORD_PASS_SUM(upper, ...) if (passSums[(u32)Pass:: upper]) times.push({ cstring(PASS_NAMES[(u32)Pass:: upper]), passSums[(u32)Pass:: upper] });
        FOR_EACH_PASS(RECORD_PASS_SUM);
        #undef RECORD_PASS_SUM

        i64 intSum = 0;
        for (u32 i = 0; i < (u32)Pass::N_PASSES; i ++)
            intSum += passSums[i];

        auto formatTime = [&](slice<i8> buf, i64 time) -> slice<i8> {
            double floatSum = time;
            switch (config::jasminePassTimingUnit) {
                case PassTimeUnit::Seconds:
                    return prints(buf, WithPrecision(6, 6, floatSum / 1000000000.0), " s");
                case PassTimeUnit::Milliseconds:
                    return prints(buf, WithPrecision(6, 6, floatSum / 1000000.0), " ms");
                case PassTimeUnit::Microseconds:
                    return prints(buf, WithPrecision(6, 6, floatSum / 1000.0), " us");
                case PassTimeUnit::Nanoseconds:
                    return prints(buf, WithPrecision(6, 6, floatSum), " ns");
                case PassTimeUnit::Ticks:
                    return prints(buf, time, " ticks");
            }
        };

        sort(times, [&](const auto& a, const auto& b) -> bool { return a.value > b.value; });

        array<i8, 128> linePrefix;
        println("Passes in order of time spent: ");
        println("----------------------------------------------------");
        for (const auto& e : times) {
            double percent = (double)e.value / (double)intSum;
            array<i8, 128> percentageBuf;
            auto spaces = cstring("                    ");
            auto percentage = prints(percentageBuf, WithPrecision(2, 2, percent * 100), "%");
            auto name = printsln(linePrefix, '[', spaces.take(7 - percentage.size()), percentage, "] ", e.key);
            for (u32 i = name.size(); i < 36; i ++)
                linePrefix[i] = ' ';
            print((const_slice<i8>)linePrefix);
            array<i8, 128> timebuf;
            auto buf = formatTime(timebuf, e.value);
            println(spaces.take(16 - buf.size()), buf);
        }

        println("----------------------------------------------------");
        print("Total time spent in optimizations was ");
        array<i8, 80> buf;
        println(formatTime(buf, intSum));
    }

    i64 CompileStats::functionsCompiled;
    i64 CompileStats::blocksCompiled;
    i64 CompileStats::nodesCompiled;

    void CompileStats::printStats() {
        println("Compiled:");
        println(" - ", functionsCompiled, " functions");
        println(" - ", blocksCompiled, " blocks");
        println(" - ", nodesCompiled, " instructions");
    }

    void AllocationResult::initialize(PassContext& ctx, Function& function, VariableAllocationMode allocationMode_in, ScratchRegisterMode scratchMode_in) {
        numVars = function.variableList.size();
        allocationMode = allocationMode_in;
        scratchMode = scratchMode_in;
        allocations.clear();
        scratchesByInstruction.clear();
        if (allocationMode == AllocateVariables) {
            for (u32 i = 0; i < function.variableList.size(); i ++)
                allocations.push(function.invalid());
        } else if (allocationMode == AllocateLiveRanges) {
            auto& liveness = *ctx.liveness;
            for (u32 i = 0; i < liveness.ranges.size(); i ++)
                allocations.push(function.variableById(liveness.ranges[i].var));
            if (ctx.has(SSA) && ctx.pins->count()) {
                for (u32 i = 0; i < function.variableList.size(); i ++)
                    allocations.push(function.invalid());
            }
        }
        if (scratchMode == PerInstructionScratchRegisters) {
            for (u32 i = 0; i < function.nodeList.size(); i ++)
                scratchesByInstruction.push({});
        }
        for (u32 i = 0; i < function.edgeList.size(); i ++)
            edgeAllocations.push({});
        stack = 0;
    }

    struct CallGraph {
        vec<bitset<128>> calls;
        bitset<128> makesCalls;
        bitset<128> tempMarks, permMarks;

        inline CallGraph(Module* module) {
            calls.expandTo(module->functions.size());
        }

        inline void addCall(Function* caller, Function* callee) {
            makesCalls.on(caller->indexInModule);
            calls[caller->indexInModule].on(callee->indexInModule);
        }

        inline void sortTopologically(Module* mod, vec<Function*>& order, Function* function) {
            if (permMarks[function->indexInModule])
                return;
            if (tempMarks[function->indexInModule]) // We have a cycle; ignore this for now.
                return;

            tempMarks[function->indexInModule] = true;
            for (u32 callee : calls[function->indexInModule])
                sortTopologically(mod, order, mod->functions[callee]);
            permMarks[function->indexInModule] = true;
            order.push(function);
        }

        inline void bottomUpInliningOrder(Module* mod, vec<Function*>& order) {
            tempMarks.clear();
            permMarks.clear();
            for (Function* function : mod->functions)
                sortTopologically(mod, order, function);
            assert(order.size() == mod->functions.size());
        }

        inline void topDownInliningOrder(Module* mod, vec<Function*>& order) {
            bottomUpInliningOrder(mod, order);
            reverse(order);
        }
    };

    void analyze(PassContext& ctx, Function& fn, bool validate) {
        ctx.reset();
        if (validate) {
            typecheck(ctx, fn);
            if (ctx.errored()) {
                ctx.printErrors(file::stdout);
                return;
            }
        } else
            assignTypesUnchecked(ctx, fn);
    }

    void targetAgnosticOptimizations(PassContext& ctx, Function& fn, u32 optLevel, bool validate) {
        assert(ctx.has(TYPECHECK));
        ctx.reset();
        ctx.did(TYPECHECK);

        if (optLevel > 0) {
            if (optLevel > 1) {
                findDominators(ctx, fn);
                findNaturalLoops(ctx, fn);
                enforceSSA(ctx, fn);
                if (!config::noConstantFolding)
                    foldConstants(ctx, fn);
                if (!config::noStrengthReduction)
                    reduceStrength(ctx, fn);
                if (!config::noCSE)
                    eliminateCommonSubexpressions(ctx, fn);
                cleanup(ctx, fn);
                computeEffects(ctx, fn);
                if (!config::noLICM)
                    hoistLoopInvariantCode(ctx, fn);
                // enforceSSA(ctx, fn);
                // foldConstants(ctx, fn);
                // reduceStrength(ctx, fn);
            }
            simplify(ctx, fn);
            cleanup(ctx, fn);
            if (optLevel > 1)
                liveness(ctx, fn);
        }
    }

    void targetSpecificOptimizations(PassContext& ctx, Function& fn, u32 optLevel, bool validate) {
        ctx.targetSpecificPasses->computeClobbers(ctx, fn);
        if (optLevel > 1) {
            containment(ctx, fn);
            ctx.targetSpecificPasses->allocateRegisters(ctx, fn, RegisterAllocationMode::ADVANCED);
        // else if (optLevel > 0)
        //     ctx.targetSpecificPasses->allocateRegistersQuick(ctx, fn);
        } else
            ctx.targetSpecificPasses->allocateStackOnly(ctx, fn);
        ctx.targetSpecificPasses->lower(ctx, fn);

        finalizeCFG(ctx, fn);

        if (validate)
            validateAfterLowering(ctx, fn);
    }

    void computeCallGraph(Function* function, CallGraph& graph) {
        Module* mod = function->mod;
        for (Node node : function->nodes()) if (isCall(node.opcode()) && node.use(0).kind == Operand::Func) {
            auto it = mod->functionMap.find(node.use(0).sym);
            if (it == mod->functionMap.end())
                continue;
            graph.addCall(function, mod->functions[it->value]);
        }
    }

    void compileWithOptimizations(Module* mod, Assembly& as, u32 optLevel, bool validate) {
        if UNLIKELY(config::forceOptLevel >= 0)
            optLevel = config::forceOptLevel;

        PassContext& ctx = mod->passContext();
        for (Function* function : mod->functions)
            analyze(ctx, *function, validate);

        ctx.targetSpecificPasses->lowerTypes(mod);

        if (optLevel > 1 && !config::noInlining) {
            // Inlining is special, it has to be done before other opts,
            // because jasmine architecturally doesn't share the pass context
            // between function compilations. Normally, this means once we
            // start optimizing a function, we don't stop optimizing that
            // function until it finishes compilation. But inlining needs to
            // be able to merge function contents, and to do that, none of the
            // contents can be transformed too much or depend on the pass
            // context. So, we handle inlining (and any other future global
            // opts) for all functions, and only then proceed to optimize the
            // functions one-by-one.

            CallGraph graph(mod);
            for (Function* function : mod->functions)
                computeCallGraph(function, graph);
            vec<Function*> toInline;
            graph.bottomUpInliningOrder(mod, toInline);

            for (Function* function : toInline) if (graph.makesCalls[function->indexInModule])
                inlineCalls(ctx, *function);
        }

        for (Function*& function : mod->functions) {
            targetAgnosticOptimizations(ctx, *function, optLevel, validate);
            targetSpecificOptimizations(ctx, *function, optLevel, validate);
            ctx.targetSpecificPasses->generateAssembly(ctx, *function, as);
            delete function;
            function = nullptr;
        }

        ctx.targetSpecificPasses->generateData(mod, as);
    }

    void compileFunctionOnly(Function* function, Assembly& as, u32 optLevel, bool validate) {
        if UNLIKELY(config::forceOptLevel >= 0)
            optLevel = config::forceOptLevel;

        PassContext& ctx = function->mod->passContext();
        analyze(ctx, *function, validate);

        ctx.targetSpecificPasses->lowerTypes(function->mod);
        targetAgnosticOptimizations(ctx, *function, optLevel, validate);
        targetSpecificOptimizations(ctx, *function, optLevel, validate);
        ctx.targetSpecificPasses->generateAssembly(ctx, *function, as);
        delete function;
    }

    void compileModuleOnly(Module* mod, Assembly& as, u32 optLevel, bool validate) {
        PassContext& ctx = mod->passContext();
        ctx.targetSpecificPasses->generateData(mod, as);
    }

    void PassContext::printErrors(file::fd file) {
        for (const auto& error : errors) {
            i32 line = 1;
            for (auto [i, b] : enumerate(error.function->blocks())) {
                if (i < error.blockIndex)
                    line += b.length() + 1;
                else if (i == error.blockIndex) {
                    line ++;
                    break;
                }
            }
            for (auto n : error.function->block(error.blockIndex).nodes()) {
                if (n.index() == error.nodeIndex)
                    break;
                else
                    line ++;
            }
            writeln(file, '[', error.function->name(), ":", line, "] Error: ", error.message);
            if (error.nodeIndex != -1) writeln(file, "    ", error.function->node(error.nodeIndex));
        }
    }

    void beginDOT(PassContext& ctx, Function& fn) {
        if (!DOTFile) {
            if (config::jasmineDOTFile.size() > 0)
                DOTFile = file::open(config::jasmineDOTFile, file::WRITE);
            else
                DOTFile = io_stdout;
            write(DOTFile, "digraph {\n");
            write(DOTFile, "    ratio=fill;\n");
            atexit([](){
                write(DOTFile, "}\n");
                flush(DOTFile);
            });
        }

        if (config::printJasmineDOTBeforeOpts && shouldPrintFor(Pass::NONPASS, fn)) {
            write(DOTFile, FunctionInPass {
                .passContext = ctx,
                .function = fn,
                .pass = Pass::CLEANUP,
                .isFirst = true,
                .isLast = false
            });
            flush(DOTFile);
        }
    }

    void finishDOT(PassContext& ctx, Function& fn) {
        if (config::printJasmineDOTAfterOpts && shouldPrintFor(Pass::NONPASS, fn)) {
            write(DOTFile, FunctionInPass {
                .passContext = ctx,
                .function = fn,
                .pass = Pass::CLEANUP,
                .isFirst = false,
                .isLast = true
            });
        }
    }
}