#include "jasmine/pass.h"
#include "jasmine/mod.h"

namespace jasmine {
    using PlaceIndex = u32;

    struct Place {
        constexpr static PlaceIndex Top = 0;
        enum Kind : u32 {
            Scalar,
            Ref,
            Tuple
        };
        union {
            struct { Kind kind : 3; u32 isPinned : 1; i32 var : 20; u32 fieldId : 8; };
            struct { u32 : 4; u32 data : 28; };
            u32 bits;
        };
    };

    static_assert(sizeof(Place) == 4);

    struct Places;

    struct Scope {
        enum Shape : u32 {
            None,
            Set,
            Range
        };

        union {
            struct { Shape shape : 7; u32 isPinned : 1; u32 : 24; u32 set; };
            struct { u32 : 8; PlaceIndex low : 24; PlaceIndex high; };
            u32 bits[2];
        };

        inline static Scope none() {
            Scope scope;
            scope.shape = None;
            return scope;
        }

        inline static Scope of(Places& places, PlaceIndex p);

        inline static Scope top() {
            Scope scope;
            scope.shape = Range;
            scope.low = 0;
            scope.high = Place::Top;
            return scope;
        }

        inline bool tryToFit(PlaceIndex p) {
            i32 diff = i32(p) - i32(low);
            if (diff >= 32)
                return false;
            if (diff >= 0)
                return true;
            u32 newSet = set << -diff;
            if (newSet >> -diff != set)
                return false;
            set = newSet;
            low = p;
            return true;
        }

        inline void add(PlaceIndex p) {
            if (shape == Set) {
                if (tryToFit(p)) {
                    set |= 1ull << (p - low);
                    return;
                }
                shape = Range;
                assert(set);
                high = maxOfSet();
                // Fall through to range case.
            }
            low = min(low, p);
            high = max(high, p);
        }

        inline PlaceIndex maxOfSet() const {
            assert(shape == Set);
            return low + 31 - ctz32(set);
        }

        inline void addAll(Scope other) {
            if (other.shape == None)
                return;
            if (shape == None) {
                *this = other;
                return;
            }
            if (other.shape == Set) {
                if (shape == Set) {
                    assert(set);
                    u32 otherMax = other.maxOfSet();
                    if (tryToFit(otherMax) && tryToFit(other.low)) {
                        set |= other.set << (other.low - low);
                        return;
                    }
                    shape = Range;
                    high = max(maxOfSet(), otherMax);
                    low = min(low, other.low);
                    return;
                }

                assert(shape == Range);
                low = min(low, other.low);
                high = max(high, other.maxOfSet());
                return;
            }

            assert(other.shape == Range);
            if (shape == Set) {
                if (tryToFit(other.low) && tryToFit(other.high)) {
                    u32 otherSet = 0xffffffffu << (other.high - low) >> (other.low - low);
                    set |= otherSet;
                    return;
                }
                shape = Range;
                high = max(maxOfSet(), other.high);
                low = min(low, other.low);
                return;
            }

            high = max(maxOfSet(), other.high);
            low = min(low, other.low);
        }

        inline bool intersects(Scope other) const {
            u32 ourLow = low, ourHigh = high;
            u32 theirLow = other.low, theirHigh = other.high;
            if (shape == None || other.shape == None)
                return false;
            if (shape == Set) {
                if (other.shape == Set) {
                    u32 minSet = set, maxSet = other.set;
                    u32 minLow = low, maxLow = other.low;
                    if (other.low < low)
                        swap(minSet, maxSet), swap(minLow, maxLow);
                    if (maxLow - minLow >= 32)
                        return false;
                    return minSet & (maxSet << (maxLow - minLow));
                }
                ourHigh = maxOfSet();
            } else if (other.shape == Set)
                theirHigh = other.maxOfSet();

            return ourHigh < theirLow || ourLow > theirHigh;
        }

        inline bool contains(PlaceIndex place) const {
            if (shape == None)
                return false;
            if (place < low)
                return false;
            if (shape == Set) {
                if (place - low >= 32)
                    return false;
                return set & (1u << (place - low));
            }
            return place <= high;
        }

        template<typename Func>
        void forEach(Func&& func) {
            if (shape == None)
                return;
            if (shape == Set) {
                u32 t = set, l = low;
                while (t) {
                    u32 tz = ctz32(t);
                    func(l + tz);
                    l += tz + 1;
                    set >>= tz + 1;
                }
            } else if (shape == Range) {
                for (u32 i = low; i <= high; i ++)
                    func(i);
            } else
                unreachable("Unexpected effect scope kind!");
        }

        inline maybe<PlaceIndex> tryGetSinglePointee() {
            if (shape == Set && !(set & (set - 1)))
                return some<PlaceIndex>(low + ctz32(set));
            if (shape == Range && low == high)
                return some<PlaceIndex>((u32)low);
            return ::none<PlaceIndex>();
        }
    };

    static_assert(sizeof(Scope) == 8);

    struct Places {
        vec<u32> placeData;
        vec<Place> places;

        constexpr static PlaceIndex Globals = 0;

        Places() {
            Place globals;
            globals.kind = Place::Scalar;
            globals.isPinned = false;
            globals.var = -1;
            globals.fieldId = 0;
            places.push(globals);
        }

        inline PlaceIndex makeScalar(i32 var = -1) {
            Place place;
            place.kind = Place::Scalar;
            place.isPinned = false;
            place.var = var;
            place.fieldId = 0;
            places.push(place);
            return places.size() - 1;
        }

        inline PlaceIndex makeRef(Scope target) {
            Place place;
            place.kind = Place::Ref;
            place.isPinned = false;
            place.data = placeData.size();
            placeData.push(target.bits[0]);
            placeData.push(target.bits[1]);
            places.push(place);
            return places.size() - 1;
        }

        inline PlaceIndex makeTuple(const_slice<PlaceIndex> fields, i32 var = -1) {
            Place place;
            place.kind = Place::Tuple;
            place.isPinned = false;
            place.data = placeData.size();
            placeData.push(fields.size());

            Scope scope = Scope::none();
            placeData.push(scope.bits[0]);
            placeData.push(scope.bits[1]);

            // Now, we add a reference for each field.
            for (u32 i = 0; i < fields.size(); i ++) {
                PlaceIndex place = fields[i];
                placeData.push(place);
                places[place].var = var;
                places[place].fieldId = i + 1;
            }

            places.push(place);
            return places.size() - 1;
        }

        inline Place operator[](PlaceIndex i) const {
            return places[i];
        }

        inline PlaceIndex fieldOf(PlaceIndex place, u32 i) {
            assert(places[place].kind == Place::Tuple);
            u32 data = places[place].data;
            assert(i < placeData[data]);
            return placeData[data + 3 + i];
        }

        inline const_slice<PlaceIndex> fieldsOf(PlaceIndex place) {
            assert(places[place].kind == Place::Tuple);
            u32 data = places[place].data;
            return { (const PlaceIndex*)&placeData[data + 3], placeData[data] };
        }

        inline Scope tupleScope(PlaceIndex place) {
            assert(places[place].kind == Place::Tuple);

            u32 data = places[place].data;

            Scope scope;
            scope.bits[0] = placeData[data + 1];
            scope.bits[1] = placeData[data + 2];
            if (scope.shape != Scope::None)
                return scope;
            for (PlaceIndex field : fieldsOf(place))
                scope.addAll(Scope::of(*this, field));
            placeData[place + 1] = scope.bits[0];
            placeData[place + 2] = scope.bits[1];
            return scope;
        }

        inline Scope targetOf(PlaceIndex place) {
            assert(places[place].kind == Place::Ref);
            Scope scope;
            u32 data = places[place].data;
            scope.bits[0] = placeData[data];
            scope.bits[1] = placeData[data + 1];
            return scope;
        }

        inline void setTargetOf(PlaceIndex place, Scope scope) {
            assert(places[place].kind == Place::Ref);
            u32 data = places[place].data;
            places[data].bits = scope.bits[0];
            places[data + 1].bits = scope.bits[1];
        }
    };

    inline Scope Scope::of(Places& places, PlaceIndex p) {
        Scope scope;
        scope.shape = Set;
        scope.low = p;
        scope.set = 1;
        scope.isPinned = places.places[p].isPinned;
        return scope;
    }

    struct Effects {
        vec<Scope> readsByInstruction, writesByInstruction;
        vec<Scope> readsByBlock, writesByBlock;
        vec<Scope> readsByLoop, writesByLoop;
        Places places;
        vec<i32> variablePlaces;
    };

    struct PlaceLogger {
        Function& fn;
        Places& places;
        PlaceIndex index;
    };

    struct ScopeLogger {
        Function& fn;
        Places& places;
        Scope scope;
    };

    template<typename IO, typename Format = Formatter<IO>>
    inline IO format_impl(IO io, const PlaceLogger& pl) {
        Place place = pl.places.places[pl.index];
        if (pl.index == Places::Globals)
            return format(io, "<globals>");
        switch (place.kind) {
            case Place::Scalar:
                if (place.var != -1)
                    io = format(io, OperandLogger { pl.fn, pl.fn.variableById(place.var) });
                else
                    io = format(io, '@', pl.index);
                if (place.fieldId > 0)
                    io = format(io, '.', place.fieldId);
                return io;
            case Place::Ref:
                return format(io, '&', ScopeLogger { pl.fn, pl.places, pl.places.targetOf(pl.index )});
            case Place::Tuple: {
                io = format(io, '[');
                bool first = true;
                for (PlaceIndex i : pl.places.fieldsOf(pl.index)) {
                    if (!first)
                        io = format(io, ", ");
                    first = false;
                    io = format(io, PlaceLogger { pl.fn, pl.places, i });
                }
                return format(io, ']');
            }
            default:
                unreachable("Unknown place kind!");
        }
    }

    template<typename IO, typename Format = Formatter<IO>>
    inline IO format_impl(IO io, const ScopeLogger& sl) {
        switch (sl.scope.shape) {
            case Scope::None:
                return format(io, "{}");
            case Scope::Set: {
                u32 set = sl.scope.set;
                u32 i = 0;
                io = format(io, '{');
                while (set) {
                    if (i > 0)
                        io = format(io, ", ");
                    u32 tz = ctz32(set);
                    i += tz;
                    io = format(io, PlaceLogger { sl.fn, sl.places, sl.scope.low + i });
                    i ++;
                    set >>= tz + 1;
                }
                return format(io, '}');
            }
            case Scope::Range: {
                io = format(io, '{', PlaceLogger { sl.fn, sl.places, sl.scope.low }, "...");
                return format(io, PlaceLogger { sl.fn, sl.places, sl.scope.high }, '}');
            }
            default:
                unreachable("Unknown scope kind!");
        }
    }

    void computeEffects(PassContext& ctx, Function& fn) {
        JASMINE_PASS(COMPUTE_EFFECTS);
        ctx.require(SSA, NATURAL_LOOPS);
        ctx.did(EFFECTS);

        Effects effects;
        Places& places = effects.places;
        auto& variablePlaces = effects.variablePlaces;
        variablePlaces.expandTo(fn.variableList.size(), -1);

        auto ensurePlace = [&](TypeIndex type, i32 var) -> PlaceIndex {
            if (variablePlaces[var] == -1) {
                if (isPointer(type))
                    return variablePlaces[var] = places.makeRef(Scope::none());
                else if (isStruct(fn, type)) {
                    const CompoundType& compound = fn.typeContext()[type];
                    vec<PlaceIndex, 16> fields;
                    fields.expandTo(compound.fields().size());
                    for (const auto& [i, t] : enumerate(compound.fields())) {
                        if (isPointer(t))
                            fields[i] = places.makeRef(Scope::none());
                        else
                            fields[i] = places.makeScalar();
                    }
                    variablePlaces[var] = places.makeTuple(fields, var);
                }
                return variablePlaces[var] = places.makeScalar(var);
            }
            return variablePlaces[var];
        };

        for (Parameter parameter : fn.parameters) {
            if (parameter.type == PTR || parameter.type == REF)
                variablePlaces[parameter.operand.var] = places.makeRef(Scope::of(places, Places::Globals));
            else if (isStruct(fn, parameter.type)) {
                const CompoundType& compound = fn.typeContext()[parameter.type];
                vec<PlaceIndex, 16> fields;
                fields.expandTo(compound.fields().size());
                for (const auto& [i, t] : enumerate(compound.fields())) {
                    if (isPointer(t))
                        fields[i] = places.makeRef(Scope::of(places, Places::Globals));
                    else
                        fields[i] = places.makeScalar();
                }
                variablePlaces[parameter.operand.var] = places.makeTuple(fields, parameter.operand.var);
            } else
                variablePlaces[parameter.operand.var] = places.makeScalar(parameter.operand.var);
        }

        // First we traverse the instruction list and synthesize places for each variable.

        vec<pair<NodeIndex, Scope>> callScopes;

        for (Block block : fn.blocks()) for (Node node : block.nodes()) switch (node.opcode()) {
            case Opcode::MOV:
            case Opcode::OFFSET_FIELD:
            case Opcode::OFFSET_INDEX:
                if (node.operand(1).kind == Operand::Static || node.operand(1).kind == Operand::Data)
                    variablePlaces[node.operand(0).var] = places.makeRef(Scope::of(places, Places::Globals));
                else if (node.operand(1).kind == Operand::Var && isPointer(node.type()))
                    variablePlaces[node.operand(0).var] = places.makeRef(places.targetOf(ensurePlace(node.type(), node.operand(1).var)));
                else if (isStruct(fn, node.type()))
                    variablePlaces[node.operand(0).var] = places.makeTuple(places.fieldsOf(ensurePlace(node.type(), node.operand(1).var)), node.operand(0).var);
                break;

            case Opcode::GET_FIELD: {
                assert(isStruct(fn, node.type()));
                assert(node.operand(1).kind == Operand::Var);
                TypeIndex type = fn.typeContext()[node.type()].fields()[fn.intValueOf(node.operand(2))];
                PlaceIndex place = ensurePlace(type, node.operand(1).var);
                if (isPointer(type)) {
                    if (places[place].kind == Place::Tuple)
                        variablePlaces[node.operand(0).var] = places.makeRef(Scope::of(places, places.fieldOf(place, fn.intValueOf(node.operand(2)))));
                    else
                        variablePlaces[node.operand(0).var] = places.makeRef(Scope::of(places, place));
                }
                break;
            }

            case Opcode::SET_FIELD: {
                assert(isStruct(fn, node.type()));
                assert(node.operand(0).kind == Operand::Var);
                ensurePlace(node.type(), node.operand(0).var);
                break;
            }

            case Opcode::ADDR_FIELD: {
                assert(node.operand(1).kind == Operand::Var);
                TypeIndex type = fn.typeContext()[node.type()].fields()[fn.intValueOf(node.operand(2))];
                PlaceIndex place = ensurePlace(type, node.operand(1).var);
                if (places[place].kind == Place::Tuple)
                    variablePlaces[node.operand(0).var] = places.makeRef(Scope::of(places, places.fieldOf(place, fn.intValueOf(node.operand(2)))));
                else
                    variablePlaces[node.operand(0).var] = places.makeRef(Scope::of(places, place));
                break;
            }

            case Opcode::ADDR:
            case Opcode::ADDR_INDEX:
                assert(node.operand(1).kind == Operand::Var);
                variablePlaces[node.operand(0).var] = places.makeRef(Scope::of(places, ensurePlace(I32, node.operand(1).var)));
                break;

            case Opcode::GET_INDEX:
                ensurePlace(I32, node.operand(1).var);
                break;

            case Opcode::SET_INDEX:
                ensurePlace(I32, node.operand(0).var);
                break;

            case Opcode::CALL:
            case Opcode::CALL_VOID: {
                // We update the actual scope later.
                const auto& compound = fn.typeContext()[node.type()];
                assert(compound.kind() == CompoundType::FUNCTION);
                callScopes.push({ node.index(), Scope::of(places, Places::Globals) });
                if (node.opcode() == Opcode::CALL) {
                    if (compound.returnType() == PTR)
                        variablePlaces[node.operand(0).var] = places.makeRef(Scope::of(places, Places::Globals));
                    else
                        ensurePlace(compound.returnType(), node.operand(0).var);
                }
            }
            default:
                break;
        }

        // Next, we need to compute the transitively reachable frontier for
        // each call's arguments. This is because not only do the referenced
        // locations of each pointer escape, but any locations referenced by
        // any of their fields. We also sum up the ranges of each of these
        // pointers and create a unified effect scope for the call.

        auto pinTransitivelyReachable = [&](Scope& root, PlaceIndex base) {
            vec<PlaceIndex, 8> frontier;
            frontier.push(base);
            while (frontier.size()) {
                PlaceIndex place = frontier.pop();
                assert(places[place].kind == Place::Ref);
                Scope target = places.targetOf(place);
                root.addAll(target);
                if (!target.isPinned) {
                    target.isPinned = true;
                    places.setTargetOf(place, target);
                    target.forEach([&](PlaceIndex place) {
                        Place p = places[place];
                        if (p.kind == Place::Ref)
                            frontier.push(place);
                        else if (p.kind == Place::Tuple) {
                            for (PlaceIndex field : places.fieldsOf(place))
                                if (places[field].kind == Place::Ref)
                                    frontier.push(place);
                        }
                    });
                }
            }
        };

        for (auto& [ni, scope] : callScopes) {
            Node node = fn.node(ni);
            const auto& compound = fn.typeContext()[node.type()];
            for (const auto& [i, arg] : enumerate(node.uses().drop(1))) if (arg.kind == Operand::Var) {
                PlaceIndex place = ensurePlace(compound.arguments()[i], arg.var);
                if (places[place].kind == Place::Ref)
                    pinTransitivelyReachable(scope, place);
            }
        }

        if UNLIKELY(config::verboseEffects) {
            println("[EFF]\tComputed places for each variable:");
            for (const auto& [v, i] : enumerate(variablePlaces)) if (i != -1)
                println("[EFF]\t    ", OperandLogger { fn, fn.variableById(v) }, " is at ", PlaceLogger { fn, places, (PlaceIndex)i });
        }

        // Finally, we compute effects based on the scopes involved in each
        // instruction.

        effects.readsByInstruction.expandTo(fn.nodeList.size(), Scope::none());
        effects.writesByInstruction.expandTo(fn.nodeList.size(), Scope::none());
        effects.readsByBlock.expandTo(fn.blockList.size(), Scope::none());
        effects.writesByBlock.expandTo(fn.blockList.size(), Scope::none());
        effects.readsByLoop.expandTo(ctx.loops->size(), Scope::none());
        effects.writesByLoop.expandTo(ctx.loops->size(), Scope::none());

        auto reads = [&](Node node, Scope scope) {
            effects.readsByInstruction[node.index()].addAll(scope);
        };

        auto writes = [&](Node node, Scope scope) {
            effects.writesByInstruction[node.index()].addAll(scope);
        };

        for (Block block : fn.blocks()) for (Node node : block.nodes()) switch (node.opcode()) {
            case Opcode::STORE:
            case Opcode::STORE_INDEX:
                if (node.operand(0).kind == Operand::Var)
                    writes(node, places.targetOf(ensurePlace(PTR, node.operand(0).var)));
                else if (node.operand(0).kind == Operand::Static)
                    writes(node, Scope::of(places, Places::Globals));
                break;
            case Opcode::STORE_FIELD:
                if (node.operand(0).kind == Operand::Var) {
                    auto scope = places.targetOf(ensurePlace(PTR, node.operand(0).var));
                    auto pointee = scope.tryGetSinglePointee();
                    if (pointee && places[*pointee].kind == Place::Tuple)
                        writes(node, Scope::of(places, places.fieldOf(*pointee, fn.intValueOf(node.operand(1)))));
                    else
                        writes(node, places.targetOf(ensurePlace(PTR, node.operand(0).var)));
                } else if (node.operand(0).kind == Operand::Static)
                    writes(node, Scope::of(places, Places::Globals));
                break;
            case Opcode::LOAD:
            case Opcode::LOAD_INDEX:
                if (node.operand(1).kind == Operand::Var)
                    reads(node, places.targetOf(ensurePlace(PTR, node.operand(1).var)));
                else if (node.operand(1).kind == Operand::Static)
                    reads(node, Scope::of(places, Places::Globals));
                break;
            case Opcode::LOAD_FIELD:
                if (node.operand(1).kind == Operand::Var) {
                    auto scope = places.targetOf(ensurePlace(PTR, node.operand(1).var));
                    auto pointee = scope.tryGetSinglePointee();
                    if (pointee && places[*pointee].kind == Place::Tuple)
                        reads(node, Scope::of(places, places.fieldOf(*pointee, fn.intValueOf(node.operand(2)))));
                    else
                        reads(node, places.targetOf(ensurePlace(PTR, node.operand(1).var)));
                } else if (node.operand(1).kind == Operand::Static)
                    reads(node, Scope::of(places, Places::Globals));
                break;
            case Opcode::GET_INDEX:
                assert(node.operand(1).kind == Operand::Var);
                assert(variablePlaces[node.operand(1).var] != -1);
                reads(node, Scope::of(places, variablePlaces[node.operand(1).var]));
                break;
            case Opcode::GET_FIELD: {
                assert(node.operand(1).kind == Operand::Var);
                assert(variablePlaces[node.operand(1).var] != -1);
                auto place = variablePlaces[node.operand(1).var];
                if (places[place].kind == Place::Tuple)
                    reads(node, Scope::of(places, places.fieldOf(place, fn.intValueOf(node.operand(2)))));
                else
                    reads(node, Scope::of(places, place));
                break;
            }
            case Opcode::SET_INDEX:
                assert(node.operand(0).kind == Operand::Var);
                assert(variablePlaces[node.operand(0).var] != -1);
                writes(node, Scope::of(places, variablePlaces[node.operand(0).var]));
                break;
            case Opcode::SET_FIELD: {
                assert(node.operand(0).kind == Operand::Var);
                assert(variablePlaces[node.operand(0).var] != -1);
                auto place = variablePlaces[node.operand(0).var];
                if (places[place].kind == Place::Tuple)
                    writes(node, Scope::of(places, places.fieldOf(place, fn.intValueOf(node.operand(1)))));
                else
                    writes(node, Scope::of(places, place));
                break;
            }
            case Opcode::CALL:
            case Opcode::CALL_VOID:
                reads(node, Scope::top());
                writes(node, Scope::top());
                break;
            default:
                break;
        }

        for (Block block : fn.blocks()) for (Node node : block.nodes()) {
            effects.readsByBlock[block.index()].addAll(effects.readsByInstruction[node.index()]);
            effects.writesByBlock[block.index()].addAll(effects.writesByInstruction[node.index()]);
        }

        for (Loop* loop : *ctx.loops) for (BlockIndex block : loop->blocks) {
            effects.readsByLoop[loop->index].addAll(effects.readsByBlock[block]);
            effects.writesByLoop[loop->index].addAll(effects.writesByBlock[block]);
        }

        if UNLIKELY(config::verboseEffects) {
            println("[EFF]\tEffect scopes for each block, instruction, and loop:");
            for (Block block : fn.blocks()) {
                Scope reads = effects.readsByBlock[block.index()];
                Scope writes = effects.writesByBlock[block.index()];
                println("[EFF]\t    .bb", block.index(), ": [reads: ", ScopeLogger { fn, places, reads }, ", writes: ", ScopeLogger { fn, places, writes }, "]");
                for (Node node : block.nodes()) {
                    Scope reads = effects.readsByInstruction[node.index()];
                    Scope writes = effects.writesByInstruction[node.index()];
                    println("[EFF]\t        ", node, " [reads: ", ScopeLogger { fn, places, reads }, ", writes: ", ScopeLogger { fn, places, writes }, "]");
                }
            }
            println("[EFF]\t");
            for (Loop* loop : *ctx.loops) {
                Scope reads = effects.readsByLoop[loop->index];
                Scope writes = effects.writesByLoop[loop->index];
                println("[EFF]\t    Loop ", loop->index, " of .bb", SeqFormat(", .bb", loop->blocks), " [reads: ", ScopeLogger { fn, places, reads }, ", writes: ", ScopeLogger { fn, places, writes }, "]");
            }
        }
    }
}