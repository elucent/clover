#include "jasmine/pass.h"
#include "jasmine/mod.h"
#include "util/deque.h"

namespace jasmine {
    using AliasIndex = i32;

    struct AliasClasses {
        struct AliasClass {
            u32 hasLink : 1;
            u32 isPinned : 1;
            u32 isGlobal : 1;
            u32 link : 29;
        };
        static_assert(sizeof(AliasClass) == 4);

        vec<AliasClass> aliasClasses;

        inline AliasClasses() {
            aliasClasses.push({ // Globals
                .hasLink = 0,
                .isPinned = 1,
                .isGlobal = 1,
                .link = 0
            });
        }

        constexpr static AliasIndex Globals = 0;

        inline AliasIndex create() {
            aliasClasses.push({
                .hasLink = 0,
                .isPinned = 0,
                .isGlobal = 0,
                .link = 0
            });
            return aliasClasses.size() - 1;
        }

        inline AliasIndex expand(AliasIndex a) {
            if (aliasClasses[a].hasLink) {
                AliasIndex result = a;
                while (aliasClasses[result].hasLink)
                    result = aliasClasses[result].link;
                aliasClasses[a].link = result;
                return result;
            }
            return a;
        }

        inline AliasIndex link(AliasIndex from, AliasIndex to) {
            aliasClasses[from].hasLink = 1;
            aliasClasses[from].link = expand(to);
            return to;
        }

        inline AliasIndex unify(AliasIndex a, AliasIndex b) {
            a = expand(a);
            b = expand(b);
            if (a == b)
                return a;
            return a == Globals ? link(b, a) : link(a, b);
        }

        inline bool isPinned(AliasIndex a) {
            return aliasClasses[a].isPinned;
        }

        inline void pin(AliasIndex& a) {
            if (a != Globals)
                aliasClasses[a].isPinned = 1;
        }
    };

    bool shouldSROA(Function& fn, TypeIndex type) {
        if (!isCompound(type))
            return true;
        const auto& compoundType = fn.typeContext()[type];
        switch (compoundType.kind()) {
            case CompoundType::ARRAY:
                return isScalar(fn, compoundType.elementType()) && compoundType.length() <= 4;
            case CompoundType::STRUCT:
                if (compoundType.fields().size() > 4)
                    return false;
                for (TypeIndex field : compoundType.fields())
                    if (!isScalar(fn, field))
                        return false;
                return true;
            case CompoundType::FUNCTION:
                return true;
            case CompoundType::VECTOR:
                unreachable("TODO: Implement vectors.");
            default:
                return false;
        }
    }

    void computeEffects(PassContext& ctx, Function& fn) {
        JASMINE_PASS(COMPUTE_EFFECTS);
        ctx.require(SSA, PINS, NATURAL_LOOPS);
        ctx.did(EFFECTS);

        auto& pins = *ctx.pins;

        // This is a beastly function that fuses alias analysis, scalar
        // replacement of aggregates, memory-to-register promotion, and effect
        // analysis. We fuse these processes together to minimize the amount of
        // memory that needs to persist in the pass context between passes, and
        // take advantage of the fact that these passes do similar things and
        // are ostensibly active at the same optimization levels.

        // First, we kick things off with alias analysis. We do this broadly by
        // creating "alias classes", which we unify together via union-find. In
        // addition to alias classes that arise through variable declarations,
        // we have an additional "globals" alias class that represents the
        // possibility that a pointer might alias with global/external memory.
        // Per each variable in the function, we can store up to two classes:
        //
        //  - The "variable" class, in variableAliasing, represents the memory
        //    that variable occupies itself. It is only populated for variables
        //    that before this function were determined to live in memory,
        //    either due to composite type, or because we take their address.
        //
        //  - The "pointer" class, in pointerAliasing, represents the memory
        //    that a variable might point to. This is only populated for
        //    variables with type PTR, or composite types that have pointer
        //    elements/fields.
        //
        // For example, if we have some code like:
        //
        //   addr ptr %p, %x
        //   addr ptr %q, %p
        //
        // Variable %x is referenced by %p, so it gets a variable aliasing
        // class of @0. %p points at %x, so it has @0 as its pointer aliasing
        // class. %p is itself referenced by %q, but %p is itself a distinct
        // memory location from %x. So %p has variable aliasing class @1, which
        // is also %q's pointer aliasing class. %q has no variable aliasing
        // class since its address is unobservable.
        //
        // While we will eventually use alias analysis as the backbone of the
        // effects system, its more immediate role is identifying which
        // variables are really "pinned" - which ones need to be in memory,
        // because their address is observable. We need to identify any places
        // that a pointer might "escape" - getting passed to a function, or
        // written to global memory - and take note of that in the aliasing
        // class. We then propagate the pinnedness of all alias classes down to
        // their union, which tells us which systems of pointers and variables
        // must remain in memory, and which we are allowed to promote.

        AliasClasses aliasClasses;
        vec<AliasIndex, 16> variableAliasing, pointerAliasing;
        variableAliasing.expandTo(fn.variableList.size(), -1);
        pointerAliasing.expandTo(fn.variableList.size(), -1);

        auto unify = [&](AliasIndex& dst, AliasIndex src) {
            if (dst == -1)
                return dst = src;
            return aliasClasses.unify(dst, src);
        };

        auto ensure = [&](AliasIndex& idx) -> AliasIndex& {
            if (idx == -1)
                idx = aliasClasses.create();
            return idx;
        };

        auto containsPointer = [&](TypeIndex type) {
            return type == PTR || (isCompound(type) && fn.typeContext()[type].containsPointers());
        };

        for (Parameter parameter : fn.parameters) if (containsPointer(parameter.type))
            pointerAliasing[parameter.operand.var] = AliasClasses::Globals;

        vec<NodeIndex, 16> storesToRevisit;

        for (Block block : fn.blocks()) {
            for (Node node : block.nodes()) {
                // Any pinned variable gets an aliasing class.
                if (hasDef(node.opcode()) && pins.isPinned(node.operand(0).var))
                    ensure(variableAliasing[node.operand(0).var]);

                switch (node.opcode()) {
                    case Opcode::VAR:
                        // Note this is potential undefined behavior, that we
                        // do capitalize on - we assume any reads to this
                        // variable must happen after it's been properly
                        // initialized, so we don't consider the possibility of
                        // reading the uninitialized address for the purposes
                        // of alias analysis.
                        if (containsPointer(node.type()))
                            ensure(pointerAliasing[node.operand(0).var]);
                        break;

                    case Opcode::ADDR:
                    case Opcode::ADDR_FIELD:
                    case Opcode::ADDR_INDEX:
                        unify(pointerAliasing[node.operand(0).var], ensure(variableAliasing[node.operand(1).var]));
                        break;

                    case Opcode::MOV:
                        if (containsPointer(node.type())) {
                            if (node.operand(1).kind == Operand::Var)
                                unify(pointerAliasing[node.operand(0).var], ensure(pointerAliasing[node.operand(1).var]));
                            else if (node.operand(1).isLabel())
                                unify(pointerAliasing[node.operand(0).var], AliasClasses::Globals);
                        }
                        break;

                    case Opcode::CALL:
                    case Opcode::CALL_VOID: {
                        const auto& funcType = fn.typeContext()[node.type()];
                        for (const auto& [i, use] : enumerate(node.uses().drop(1))) {
                            if (use.kind == Operand::Var && containsPointer(funcType.arguments()[i])) {
                                // This means a pointer, or something that
                                // contains pointers, is about to escape! So
                                // anything they might point to needs to be
                                // pinned.
                                auto& ptrClass = ensure(pointerAliasing[use.var]);
                                aliasClasses.pin(ptrClass);
                            }
                        }
                        if (node.opcode() == Opcode::CALL) {
                            // We don't know where the pointer(s) being returned
                            // came from, so they need to join Globals.
                            if (containsPointer(funcType.returnType()))
                                unify(pointerAliasing[node.operand(0).var], AliasClasses::Globals);
                        }
                        break;
                    }

                    case Opcode::ALLOCA:
                        // Allocas create new aliasing classes without any
                        // variables aliasing them.
                        pointerAliasing[node.operand(0).var] = aliasClasses.create();
                        break;

                    case Opcode::RET:
                        if (containsPointer(node.type()) && node.operand(0).kind == Operand::Var) {
                            // This might indicate undefined behavior, but I'm
                            // afraid of capitalizing on that here. So we treat
                            // returning as escaping.
                            aliasClasses.pin(ensure(pointerAliasing[node.operand(0).var]));
                        }
                        break;

                    case Opcode::GET_FIELD:
                    case Opcode::GET_INDEX: {
                        assert(node.operand(1).kind == Operand::Var);
                        TypeIndex elementType = node.opcode() == Opcode::GET_FIELD
                            ? fn.typeContext()[node.type()].fields()[fn.intValueOf(node.operand(2))]
                            : node.type();
                        ensure(variableAliasing[node.operand(1).var]);
                        if (containsPointer(elementType)) {
                            // This must also mean the composite value contains
                            // pointers, so we unify with its pointer aliasing
                            // class.
                            unify(pointerAliasing[node.operand(0).var], ensure(pointerAliasing[node.operand(1).var]));
                        }
                        if (node.opcode() == Opcode::GET_INDEX && !node.operand(1).isConst()) {
                            // Any alias class we do unpredictable indexed
                            // accesses into must remain in memory.
                            aliasClasses.pin(ensure(variableAliasing[node.operand(1).var]));
                        }
                        break;
                    }

                    case Opcode::SET_FIELD:
                    case Opcode::SET_INDEX: {
                        assert(node.operand(0).kind == Operand::Var);
                        TypeIndex elementType = node.opcode() == Opcode::SET_FIELD
                            ? fn.typeContext()[node.type()].fields()[fn.intValueOf(node.operand(1))]
                            : node.type();
                        ensure(variableAliasing[node.operand(0).var]);
                        if (node.operand(2).kind == Operand::Var && containsPointer(elementType)) {
                            // Unlike storing a pointer, we know the composite
                            // value we're writing into is local to this
                            // function. So there is no possibility of escape,
                            // we just need to consider the possibility of
                            // aliasing.
                            unify(pointerAliasing[node.operand(0).var], ensure(pointerAliasing[node.operand(2).var]));
                        }
                        if (node.opcode() == Opcode::SET_INDEX && !node.operand(1).isConst()) {
                            // Any alias class we do unpredictable indexed
                            // accesses into must remain in memory.
                            aliasClasses.pin(ensure(variableAliasing[node.operand(0).var]));
                        }
                        break;
                    }

                    case Opcode::LOAD:
                    case Opcode::LOAD_FIELD:
                    case Opcode::LOAD_INDEX: {
                        TypeIndex elementType = node.opcode() == Opcode::LOAD_FIELD
                            ? fn.typeContext()[node.type()].fields()[fn.intValueOf(node.operand(2))]
                            : node.type();
                        if (containsPointer(elementType)) {
                            if (node.operand(1).isLabel())
                                unify(pointerAliasing[node.operand(1).var], AliasClasses::Globals);
                            else
                                unify(pointerAliasing[node.operand(0).var], ensure(pointerAliasing[node.operand(1).var]));
                        }
                        if (node.opcode() == Opcode::LOAD_INDEX && !node.operand(2).isConst()) {
                            // Any alias class we do unpredictable indexed
                            // accesses into must remain in memory.
                            aliasClasses.pin(ensure(pointerAliasing[node.operand(1).var]));
                        }
                        break;
                    }

                    case Opcode::STORE:
                    case Opcode::STORE_FIELD:
                    case Opcode::STORE_INDEX: {
                        TypeIndex elementType = node.opcode() == Opcode::STORE_FIELD
                            ? fn.typeContext()[node.type()].fields()[fn.intValueOf(node.operand(1))]
                            : node.type();
                        Operand src = node.opcode() == Opcode::STORE ? node.operand(1) : node.operand(2);
                        if (src.kind == Operand::Var && containsPointer(elementType)) {
                            if (node.operand(0).isLabel()) {
                                // This means we're storing to a global, which
                                // means pointers could escape! We don't need
                                // to alias Globals, but we do need to pin our
                                // class.
                                auto& ptrClass = ensure(pointerAliasing[src.var]);
                                aliasClasses.pin(ptrClass);
                            } else {
                                // We also need to potentially revisit this
                                // store in the future, because uniquely, if we
                                // end up writing to the same alias class as
                                // Globals, we could also have a pointer
                                // escape.
                                storesToRevisit.push(node.index());
                                unify(pointerAliasing[node.operand(0).var], ensure(pointerAliasing[src.var]));
                            }
                        }
                        if (node.opcode() == Opcode::STORE_INDEX && !node.operand(1).isConst()) {
                            // Any alias class we do unpredictable indexed
                            // accesses into must remain in memory.
                            aliasClasses.pin(ensure(pointerAliasing[node.operand(0).var]));
                        }
                        break;
                    }

                    case Opcode::OFFSET_INDEX:
                    case Opcode::OFFSET_FIELD:
                        // We assume offset instructions are in-bounds, so the
                        // aliasing class of the result is simply the same as
                        // the input.
                        if (node.operand(1).isLabel())
                            unify(pointerAliasing[node.operand(0).var], AliasClasses::Globals);
                        else
                            unify(pointerAliasing[node.operand(0).var], ensure(pointerAliasing[node.operand(1).var]));
                        if (node.opcode() == Opcode::OFFSET_INDEX && !node.operand(2).isConst()) {
                            // Any alias class we do unpredictable indexed
                            // accesses into must remain in memory.
                            aliasClasses.pin(pointerAliasing[node.operand(0).var]);
                        }
                        break;

                    default:
                        break;
                }
            }
        }
        for (Edge edge : fn.edges()) for (Move move : edge.moves()) {
            if (move.src.kind == Operand::Var && pointerAliasing[move.src.var] != -1)
                unify(pointerAliasing[move.dest.var], pointerAliasing[move.src.var]);
        }

        for (NodeIndex si : storesToRevisit) {
            Node store = fn.node(si);
            auto dest = aliasClasses.expand(pointerAliasing[store.operand(0).var]);
            if (dest == AliasClasses::Globals)
                aliasClasses.pin(pointerAliasing[store.operand(1).var]);
        }

        // We now have a union-find of all the alias classes in the function.
        // Let's fully expand everything out, since it won't change again.
        // While we do this, we propagate the pinnedness of the aliasing
        // classes down.

        bitset<128> finalClasses;
        vec<pair<AliasIndex, vec<pair<i32, bool>, 16>>, 0> finalClassInfo;

        auto processClass = [&](i32 var, bool isPointer, AliasIndex& i) {
            bool pinned = aliasClasses.isPinned(i);
            AliasIndex old = i;
            i = aliasClasses.expand(i);
            if (pinned)
                aliasClasses.pin(i);
            if (!finalClasses[i]) {
                finalClasses.on(i);
                if UNLIKELY(config::verboseEffects) {
                    aliasClasses.aliasClasses[i].link = finalClassInfo.size();
                    finalClassInfo.push({i, {}});
                    finalClassInfo.back().second.push({ var, isPointer });
                }
            } else if UNLIKELY(config::verboseEffects)
                finalClassInfo[aliasClasses.aliasClasses[i].link].second.push({ var, isPointer });
        };

        for (i32 i = 0; i < variableAliasing.size(); i ++) if (variableAliasing[i] != -1)
            processClass(i, false, variableAliasing[i]);
        for (i32 i = 0; i < pointerAliasing.size(); i ++) if (pointerAliasing[i] != -1)
            processClass(i, true, pointerAliasing[i]);

        if UNLIKELY(config::verboseEffects) {
            println("[EFF]\tDiscovered ", finalClasses.count(), " aliasing classes in function ", fn.name(), ":");
            for (const auto& [i, info] : enumerate(finalClassInfo)) {
                print("[EFF]\t - ");
                if (info.first == AliasClasses::Globals)
                    print("globals");
                else
                    print("@", info.first);
                if (aliasClasses.isPinned(info.first))
                    print(" pinned");
                print(": ");
                bool first = true;
                for (const auto& [v, p] : info.second) {
                    if (!first)
                        print(", ");
                    first = false;
                    if (p) print("*");
                    print(OperandLogger { fn, fn.variableById(v) });
                }
                println();
            }
            println();
            println(fn);
        }

        // With the alias classes computed, we can compute effect scopes for
        // each instruction. These take the form of the Effect class defined
        // in pass.h. One is the set of alias classes the effect applies to,
        // and the other is the set of properties within that alias class the
        // effect applies to, if known. We use this pair to first discriminate
        // by aliasing - two accesses to addresses that don't alias can't
        // interfere. Then within those that might alias, we know that if the
        // accesses are to known disjoint fields, they still can't interfere.
        // We also compute effect scopes for each block, and for each loop,
        // since we have to traverse blocks anyway when walking the instruction
        // tree, and these are useful information for later optimizations.

        auto& effectsByInstruction = *ctx.effectsByInstruction;
        auto& effectsByBlock = *ctx.effectsByBlock;
        auto& effectsByLoop = *ctx.effectsByLoop;
        auto& loops = *ctx.loops;
        auto& blockLoops = *ctx.blockLoops;

        effectsByInstruction.init(fn.nodeList.size());
        effectsByBlock.init(fn.blockList.size());
        effectsByLoop.init(loops.size());

        vec<pair<Effect, Effect>, 16> loopEffects;
        loopEffects.expandTo(loops.size(), Effect(), Effect());

        for (Block block : fn.blocks()) {
            Effect blockReads, blockWrites;

            for (Node node : block.nodes()) {
                Effect nodeReads, nodeWrites;

                auto writeIfPinned = [&](Operand operand) {
                    assert(operand.kind == Operand::Var);
                    if (pins.isPinned(operand.var)) {
                        auto aliasingClass = variableAliasing[operand.var];
                        assert(aliasingClass != -1);
                        nodeWrites.add(aliasingClass);
                    }
                };

                auto readIfPinned = [&](Operand operand) {
                    if (operand.kind == Operand::Var && pins.isPinned(operand.var)) {
                        auto aliasingClass = variableAliasing[operand.var];
                        assert(aliasingClass != -1);
                        nodeReads.add(aliasingClass);
                    }
                };

                switch (node.opcode()) {
                    case Opcode::GET_FIELD:
                        writeIfPinned(node.operand(0));
                        assert(node.operand(1).kind == Operand::Var);
                        assert(variableAliasing[node.operand(1).var] != -1);
                        nodeReads.add(variableAliasing[node.operand(1).var], fn.intValueOf(node.operand(2)));
                        break;

                    case Opcode::GET_INDEX:
                        writeIfPinned(node.operand(0));
                        assert(node.operand(1).kind == Operand::Var);
                        assert(variableAliasing[node.operand(1).var] != -1);
                        if (node.operand(2).isConst())
                            nodeReads.add(variableAliasing[node.operand(1).var], fn.intValueOf(node.operand(2)));
                        else
                            nodeReads.add(variableAliasing[node.operand(1).var]), readIfPinned(node.operand(2));
                        break;

                    case Opcode::SET_FIELD:
                        assert(node.operand(0).kind == Operand::Var);
                        assert(variableAliasing[node.operand(0).var] != -1);
                        nodeWrites.add(variableAliasing[node.operand(0).var], fn.intValueOf(node.operand(1)));
                        readIfPinned(node.operand(2));
                        break;

                    case Opcode::SET_INDEX:
                        assert(node.operand(0).kind == Operand::Var);
                        assert(variableAliasing[node.operand(0).var] != -1);
                        if (node.operand(1).isConst())
                            nodeWrites.add(variableAliasing[node.operand(0).var], fn.intValueOf(node.operand(1)));
                        else
                            nodeReads.add(variableAliasing[node.operand(0).var]), readIfPinned(node.operand(1));
                        readIfPinned(node.operand(2));
                        break;

                    case Opcode::LOAD:
                        writeIfPinned(node.operand(0));
                        if (node.operand(1).isLabel())
                            nodeReads.add(AliasClasses::Globals);
                        else if (node.operand(1).kind == Operand::Var) {
                            auto aliasingClass = pointerAliasing[node.operand(1).var];
                            assert(aliasingClass != -1);
                            readIfPinned(node.operand(1));
                            nodeReads.add(aliasingClass);
                        }
                        break;

                    case Opcode::LOAD_FIELD:
                        writeIfPinned(node.operand(0));
                        readIfPinned(node.operand(1));
                        if (node.operand(1).isLabel())
                            nodeReads.add(AliasClasses::Globals, fn.intValueOf(node.operand(2)));
                        else if (node.operand(1).kind == Operand::Var) {
                            auto aliasingClass = pointerAliasing[node.operand(1).var];
                            assert(aliasingClass != -1);
                            readIfPinned(node.operand(1));
                            nodeReads.add(aliasingClass, fn.intValueOf(node.operand(2)));
                        }
                        break;

                    case Opcode::LOAD_INDEX:
                        writeIfPinned(node.operand(0));
                        if (node.operand(1).isLabel()) {
                            if (node.operand(2).isConst())
                                nodeReads.add(AliasClasses::Globals, fn.intValueOf(node.operand(2)));
                            else
                                nodeReads.add(AliasClasses::Globals), readIfPinned(node.operand(2));
                        } else if (node.operand(1).kind == Operand::Var) {
                            auto aliasingClass = pointerAliasing[node.operand(1).var];
                            assert(aliasingClass != -1);
                            readIfPinned(node.operand(1));
                            if (node.operand(2).isConst())
                                nodeReads.add(aliasingClass, fn.intValueOf(node.operand(2)));
                            else
                                nodeReads.add(aliasingClass), readIfPinned(node.operand(2));
                        }
                        break;

                    case Opcode::STORE:
                        if (node.operand(0).isLabel())
                            nodeWrites.add(AliasClasses::Globals);
                        else if (node.operand(0).kind == Operand::Var) {
                            auto aliasingClass = pointerAliasing[node.operand(0).var];
                            assert(aliasingClass != -1);
                            readIfPinned(node.operand(0));
                            nodeWrites.add(aliasingClass);
                        }
                        readIfPinned(node.operand(1));
                        break;

                    case Opcode::STORE_FIELD:
                        if (node.operand(0).isLabel())
                            nodeWrites.add(AliasClasses::Globals, fn.intValueOf(node.operand(1)));
                        else if (node.operand(0).kind == Operand::Var) {
                            auto aliasingClass = pointerAliasing[node.operand(0).var];
                            assert(aliasingClass != -1);
                            readIfPinned(node.operand(0));
                            nodeWrites.add(aliasingClass, fn.intValueOf(node.operand(1)));
                        }
                        readIfPinned(node.operand(2));
                        break;

                    case Opcode::STORE_INDEX:
                        if (node.operand(0).isLabel()) {
                            if (node.operand(1).isConst())
                                nodeWrites.add(AliasClasses::Globals, fn.intValueOf(node.operand(1)));
                            else
                                nodeWrites.add(AliasClasses::Globals), readIfPinned(node.operand(1));
                        } else if (node.operand(0).kind == Operand::Var) {
                            auto aliasingClass = pointerAliasing[node.operand(0).var];
                            assert(aliasingClass != -1);
                            readIfPinned(node.operand(0));
                            if (node.operand(1).isConst())
                                nodeWrites.add(aliasingClass, fn.intValueOf(node.operand(1)));
                            else
                                nodeWrites.add(aliasingClass), readIfPinned(node.operand(1));
                        }
                        readIfPinned(node.operand(2));
                        break;

                    case Opcode::CALL:
                    case Opcode::CALL_VOID: {
                        if (node.opcode() == Opcode::CALL)
                            writeIfPinned(node.operand(0));
                        readIfPinned(node.use(0)); // In case the called function is a pinned variable.
                        const auto& funcType = fn.typeContext()[node.type()];
                        for (const auto& [i, arg] : enumerate(node.uses().drop(1))) {
                            readIfPinned(arg);
                            if (containsPointer(funcType.arguments()[i])) {
                                if (arg.kind == Operand::Var) {
                                    auto aliasingClass = pointerAliasing[arg.var];
                                    assert(aliasingClass != -1);
                                    nodeReads.add(aliasingClass);
                                    nodeWrites.add(aliasingClass);
                                }

                                // The case where the arg is a label is already
                                // handled - calls always clobber globals.
                            }
                        }
                        nodeReads.add(AliasClasses::Globals);
                        nodeWrites.add(AliasClasses::Globals);
                    }

                    case Opcode::VAR:
                    case Opcode::NOP:
                        break;

                    default:
                        for (auto def : node.defs())
                            writeIfPinned(def);
                        for (auto use : node.uses())
                            readIfPinned(use);
                }

                if (!nodeReads.empty() && !nodeWrites.empty()) {
                    effectsByInstruction.addReadWrite(node.index(), nodeReads, nodeWrites);
                    blockWrites.add(nodeWrites);
                    blockReads.add(nodeReads);
                } else if (!nodeWrites.empty()) {
                    effectsByInstruction.addWriteOnly(node.index(), nodeWrites);
                    blockWrites.add(nodeWrites);
                } else if (!nodeReads.empty()) {
                    effectsByInstruction.addReadOnly(node.index(), nodeReads);
                    blockReads.add(nodeReads);
                }
            }

            if (!blockReads.empty() && !blockWrites.empty()) {
                effectsByBlock.addReadWrite(block.index(), blockReads, blockWrites);
                if (blockLoops[block.index()] != -1) {
                    auto& loop = loopEffects[blockLoops[block.index()]];
                    loop.first.add(blockReads);
                    loop.second.add(blockWrites);
                }
            } else if (!blockWrites.empty()) {
                effectsByBlock.addWriteOnly(block.index(), blockWrites);
                if (blockLoops[block.index()] != -1) {
                    auto& loop = loopEffects[blockLoops[block.index()]];
                    loop.second.add(blockWrites);
                }
            } else if (!blockReads.empty()) {
                effectsByBlock.addReadOnly(block.index(), blockReads);
                if (blockLoops[block.index()] != -1) {
                    auto& loop = loopEffects[blockLoops[block.index()]];
                    loop.first.add(blockReads);
                }
            }
        }

        deque<LoopIndex, 16> frontier;
        bitset<128> inFrontier;
        for (Loop* loop : loops) if (loop->isLeaf)
            frontier.pushr(loop->index), inFrontier.on(loop->index);
        while (frontier.size()) {
            LoopIndex us = frontier.popl();
            Loop* loop = loops[us];
            inFrontier.off(us);
            if (!loop->parent)
                continue;
            LoopIndex parent = loop->parent->index;
            loopEffects[parent].first.add(loopEffects[us].first);
            loopEffects[parent].second.add(loopEffects[us].second);
            if (!inFrontier[parent])
                frontier.pushr(parent), inFrontier.on(parent);
        }

        for (u32 i = 0; i < loopEffects.size(); i ++) {
            auto& effect = loopEffects[i];
            if (!effect.first.empty() && !effect.second.empty())
                effectsByLoop.addReadWrite(i, effect.first, effect.second);
            else if (!effect.second.empty())
                effectsByLoop.addWriteOnly(i, effect.second);
            else if (!effect.first.empty())
                effectsByLoop.addReadOnly(i, effect.first);
        }


        if UNLIKELY(config::verboseEffects) {
            auto printClass = [&](i32 aliasClass) {
                if (aliasClass == AliasClasses::Globals)
                    print("globals");
                else
                    print('@', aliasClass);
            };
            auto printEffect = [&](Effect effect) {
                assert(!effect.classes.empty());
                if (effect.classes.mode == TinyIntSet::Bits) {
                    if (isPowerOfTwo(effect.classes.bits)) // Single entry
                        printClass(effect.classes.lo + ctz32(effect.classes.bits));
                    else {
                        u32 lo = effect.classes.lo, bits = effect.classes.bits;
                        print('{');
                        while (bits) {
                            if (lo != effect.classes.lo)
                                print(", ");
                            lo += ctz32(bits);
                            bits >>= ctz32(bits) + 1;
                            printClass(lo);
                            lo ++;
                        }
                        print('}');
                    }
                } else if (effect.classes.mode == TinyIntSet::Range) {
                    if (effect.classes.lo == effect.classes.bits)
                        printClass(effect.classes.lo);
                    else {
                        print('{');
                        printClass(effect.classes.lo);
                        print("...");
                        printClass(effect.classes.bits);
                        print('}');
                    }
                } else if (effect.classes.mode == TinyIntSet::All)
                    print("{ALL}");
                if (effect.fields.mode == TinyIntSet::Bits) {
                    if (isPowerOfTwo(effect.fields.bits)) // Single entry
                        print('[', effect.fields.lo + ctz32(effect.fields.bits), ']');
                    else {
                        u32 lo = effect.fields.lo, bits = effect.fields.bits;
                        print('[');
                        while (bits) {
                            if (lo != effect.fields.lo)
                                print(", ");
                            lo += ctz32(bits);
                            bits >>= ctz32(bits) + 1;
                            print(lo);
                            lo ++;
                        }
                        print(']');
                    }
                } else if (effect.fields.mode == TinyIntSet::Range) {
                    if (effect.fields.lo == effect.fields.bits)
                        print('[', effect.fields.lo, ']');
                    else
                        print('[', effect.fields.lo, "...", effect.fields.bits, ']');
                } else if (effect.fields.mode == TinyIntSet::All) {
                    // Do nothing; just print the effect class.
                }
            };

            println("[EFF]\tEffects by instruction in function ", fn.name(), ":");
            array<i8, 512> buf;
            auto spaces = cstring("                                        ");
            for (Block block : fn.blocks()) {
                println("[EFF]\t.bb", block.index(), ":");
                for (Node node : block.nodes()) {
                    print("[EFF]\t  ");
                    auto nodestr = prints(buf, node);
                    if (nodestr.size() < 40)
                        print(nodestr, spaces.drop(nodestr.size()));
                    else
                        print(nodestr.take(40), RESET);
                    if (effectsByInstruction.hasReads(node.index()))
                        print("\tReads: "), printEffect(effectsByInstruction.readsOf(node.index()));
                    if (effectsByInstruction.hasWrites(node.index())) {
                        if (effectsByInstruction.hasReads(node.index()))
                            print(", writes: ");
                        else
                            print("\tWrites: ");
                        printEffect(effectsByInstruction.writesOf(node.index()));
                    }
                    println();
                }
            }
            println();
            println("[EFF]\tEffects by block in function ", fn.name(), ":");
            for (Block block : fn.blocks()) {
                print("[EFF]\t  ");
                auto blockstr = prints(buf, ".bb", block.index());
                print(blockstr, spaces.drop(blockstr.size()));
                if (effectsByBlock.hasReads(block.index()))
                    print("\tReads: "), printEffect(effectsByBlock.readsOf(block.index()));
                if (effectsByBlock.hasWrites(block.index())) {
                    if (effectsByBlock.hasReads(block.index()))
                        print(", writes: ");
                    else
                        print("\tWrites: ");
                    printEffect(effectsByBlock.writesOf(block.index()));
                }
                println();
            }
            println();
            println("[EFF]\tEffects by loop in function ", fn.name(), ":");
            for (u32 i = 0; i < loops.size(); i ++) {
                print("[EFF]\t  ");
                auto loopstr = prints(buf, "Loop ", i, ": ", ListFormat(".bb", ", .bb", "", loops[i]->blocks));
                if (loopstr.size() < 40)
                    print(loopstr, spaces.drop(loopstr.size()));
                else
                    print(loopstr.take(40), RESET);

                if (effectsByLoop.hasReads(i))
                    print("\tReads: "), printEffect(effectsByLoop.readsOf(i));
                if (effectsByLoop.hasWrites(i)) {
                    if (effectsByLoop.hasReads(i))
                        print(", writes: ");
                    else
                        print("\tWrites: ");
                    printEffect(effectsByLoop.writesOf(i));
                }
                println();
            }
            println();
        }

        // Now that we have alias class information, we first see if there is
        // anything we can easily promote from memory. We rule things out by
        // the following rules:
        //
        //  1. The class must have exactly one variable aliasing with it, and
        //     cannot alias with any pointer-containing aggregate (i.e., it
        //     must only be pointed to by pointers).
        //
        //  2. We can't ever index into that aliasing class with a non-constant
        //     index, either through a pointer or the variable itself.
        //
        //  3. The class can't be pinned.
        //
        //  4. The class contains at most four members, and none of them may be
        //     composite. (TODO: We should relax this)
        //
        // If these conditions are met, we try to promote the variable out of
        // memory. If the variable is already a scalar, we just unpin it. If
        // it's a composite value, then we create a fresh variable for each
        // scalar field.
        //
        // In both cases, we need to also eliminate any pointers to the value
        // or its members. We do this by hoisting all loads and stores to
        // the promoted aliasing class. If a load reaches an addr, addr_field,
        // or addr_index instruction, we replace it with a corresponding mov,
        // get_field, or get_index. For stores, we do the same, but with mov,
        // set_field, or set_index respectively. We need to maintain relative
        // order between the instructions. If we find that one of the
        // instructions we're hoisting interferes with another instruction
        // along the way, then we see if that instruction has nontrivial
        // effects - if it does, we bail. Otherwise, we add it to our hoisting
        // group.


    }
}