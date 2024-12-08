#include "jasmine/mod.h"
#include "util/bits.h"
#include "util/config.h"
#include "util/pool.h"

namespace jasmine {
	struct VariableMapping {
		vec<i32, 256> modifiedVariables;
		vec<i32, 256> originalVariables;
		i32 nModified;

		void init(Function& fn) {
			nModified = 0;
			for (Variable& var : fn.variableList) {
				modifiedVariables.push(-1);
			}
		}

		i32 modifiedCount() {
			return nModified;
		}

		bool isModified(i32 variable) {
			return modifiedVariables[variable] != -1;
		}

		i32 operator[](i32 variable) {
			return modifiedVariables[variable];
		}

		void markModified(i32 variable) {
			if (modifiedVariables[variable] == -1) {
				modifiedVariables[variable] = nModified ++;
				originalVariables.push(variable);
			}
		}
	};

	struct Phi {
		BlockIndex block : 31;
		u32 isNew : 1;
		Operand operand;
	};

	struct Def {
		union {
			struct { i32 inhabited : 1; i32 isPhi : 1; i32 isArg : 1; i32 phi : 29; };
			struct { i32 : 3; i32 node : 29; };
			struct { i32 : 3; i32 arg : 29; };
			u32 bits;
		};
		i32 modifiedVar;
		BlockIndex from;

		inline Def():
			bits(0), modifiedVar(0), from(0) {}

		inline bool operator==(const Def& other) const {
			return inhabited == other.inhabited && isPhi == other.isPhi && isArg == other.isArg && node == other.node;
		}

		inline bool operator!=(const Def& other) const {
			return inhabited != other.inhabited || isPhi != other.isPhi || node != other.node;
		}
	};

	using DefStorage = vec<Def, 256>;

	struct DefList {
		DefStorage* storage;
		i32 start, length;

		inline DefList(DefStorage& storage_in, i32 start_in, i32 length_in):
			storage(&storage_in), start(start_in), length(length_in) {}

		inline const Def* begin() const { return storage->begin() + start; }
		inline const Def* end() const { return begin() + length; }
		inline Def* begin() { return storage->begin() + start; }
		inline Def* end() { return begin() + length; }
	};

	struct UnbakedDefList {
		vec<Def, 16> defs;
		bitset<128> hasDef;

		void ensureInitialized(u32 modifiedVars) {
			while (defs.size() < modifiedVars)
				defs.push({});
			hasDef.clear();
		}

		void reset() {
			for (u32 var : hasDef)
				defs[var] = {};
			hasDef.clear();
		}
	};

	void recordDefs(PassContext& ctx, Function& fn) {
		ctx.require(SSA);

		auto& defs = *ctx.defs;
		if (defs.size())
			return;
		defs.clear();
		while (defs.size() < fn.variableList.size())
			defs.push(-1);
		for (const auto& param : fn.parameters)
			defs[param.operand.var] = -1;
		for (Node n : fn.nodes()) if (hasDef(n.opcode()) && !ctx.pins->isPinned(n.def(0).var))
			defs[n.def(0).var] = n.index();
		for (Edge e : fn.edges()) for (Move move : e.moves())
			defs[move.dest.var] = -(i32)e.index() - 2;
	}

	void computePins(PassContext& ctx, Function& fn) {
		ctx.did(IRTrait::PINS);
		auto& pins = *ctx.pins;
		for (Block block : fn.blocks()) for (Node node : block.nodes()) switch (node.opcode()) {
			case Opcode::ADDR:
			case Opcode::ADDR_FIELD:
			case Opcode::ADDR_INDEX:
				if (node.operand(1).kind == Operand::Var)
					pins.pin(node.operand(1).var);
				break;
			default:
				break;
		}
	}

    void enforceSSA(PassContext& ctx, Function& fn) {
        JASMINE_PASS(SSA_CONVERSION);
		ctx.require(TYPECHECK);
		ctx.did(SSA);

		VariableMapping mapping;
		bool alreadySSA = true;
		bitset<128> defined;
		defined.clear();
		mapping.init(fn);

		// Before anything else, we need to compute the pinned variables of
		// this function. These are any variables we take the address of, and
		// we exclude them from SSA (and SSA-based optimizations) until we can
		// eliminate taking their addresses in some later pass.

		if (!ctx.has(IRTrait::PINS))
			computePins(ctx, fn);
		auto& pins = *ctx.pins;

		// Next, we do an initial pass to detect the variables that are out of
		// SSA. If no variables are out of SSA, then we can skip the rest of
		// this pass. If there are, we create a mapping, assigning indices to
		// each out-of-SSA variable so our analysis can ignore any variables
		// that are already in SSA.

		for (const auto& parameter : fn.parameters)
			defined.on(parameter.operand.var);

        for (Block block : fn.blocks()) {
            for (Edge pred : block.predecessors()) for (Move move : pred.moves()) {
                assert(move.dest.kind == Operand::Var);

                // Defined variables in edges should already essentially be
				// phis. We assume they're well-formed (TODO: validate this
				// during typechecking or something).
                defined.on(move.dest.var);
			}
            for (Node node : block.nodes()) {
                if (hasDef(node.opcode())) {
                    assert(node.operand(0).kind == Operand::Var);

                    // SSA is violated if a variable has multiple defs.
                    if (defined[node.operand(0).var] || config::neverSkipEnforceSSA) {
                        alreadySSA = false;
						if (!pins.isPinned(node.operand(0).var))
							mapping.markModified(node.operand(0).var);
					}
                    defined.on(node.operand(0).var);
                }
            }
        }

        if (alreadySSA) {
            if UNLIKELY(config::verboseSSA)
                println("[SSA]\tSkipping SSA conversion for function ", fn.name(), ": function is already in SSA form.");
            return;
        }

		// We had some variables with multiple defs, so we need to enforce SSA.

		assert(mapping.modifiedCount() > 0);
		ctx.invalidate(SSA);

		if UNLIKELY(config::verboseSSA) {
			print("[SSA]\tPutting function ", fn.name(), " into SSA with regards to variables:");
			for (auto [v, m] : enumerate(mapping.modifiedVariables)) if (m != -1)
				print(' ', OperandLogger { fn, fn.variableById(v) });
			println();
		}

		// We start by creating some data structures. The defs for a block are
		// stored in a contiguous buffer reachingDefs, with each block holding
		// essentially a slice into that vector of definitions for its defs at
		// head. We only store the defs at head because the reaching defs
		// throughout the block can be recovered from that information.

		vec<Def, 256> reachingDefs;
		vec<DefList> defsAtHead;
		for (Block block : fn.blocks())
			defsAtHead.push({reachingDefs, 0, 0});

		// To track the current state as we walk through a block, we create a
		// currentDefs vector, with an entry for each non-SSA variable.

		UnbakedDefList current;
		current.ensureInitialized(mapping.modifiedCount());

		// Since we might start forming the defs-at-head for a block
		// before we reach it, we need some means of tracking the state for
		// each variable. This is also how we know if any two reaching defs
		// are in conflict, and if we need a phi. To handle this without
		// pessimistic memory usage (i.e. a full slab per modified variable
		// per block) we pool the definition vectors. They are acquired lazily,
		// when we first propagate our own tail defs to a successor, and
		// returned to the pool when we reach the head of the block.

		constexpr u32 invalidKey = pool<vec<Def, 128>>::InvalidKey;
		pool<UnbakedDefList> headDefPool;
		vec<u32, 128> wipDefsAtHead;
		for (Block block : fn.blocks())
			wipDefsAtHead.push(invalidKey);

		// We need to record per-block which variables we had to introduce a
		// phi for. In order to do this we use a bit matrix. We also store a
		// list holding information about every individual phi we add, so we
		// can introduce operands for each and use that when transforming the
		// graph later.

		vec<Phi, 32> phis;
		bitset<128> blockPhis;
		u32 numBlocks = fn.blockList.size();

		auto addPhiToBlock = [&](Block block, i32 var) {
			assert(var >= 0);
			assert(var < mapping.modifiedCount());
			blockPhis.on(block.index() * numBlocks + var);
		};

		auto blockHasPhi = [&](Block block, i32 var) -> bool {
			assert(var >= 0);
			assert(var < mapping.modifiedCount());
			return blockPhis[block.index() * numBlocks + var];
		};

		// Finally, we store the lists of phis and newly-introduced variables
		// separately. This is to avoid permuting the graph while iterating the
		// reaching defs until fixpoint, but also not redefining variables
		// unnecessarily.

		vec<Operand, 128> nodeDefs;
		for (Node node : fn.nodes())
			nodeDefs.push({});

		// This function lazily acquires a key to an unbaked def list for a
		// block.

		auto wipDefsForBlock = [&](Block block) -> u32 {
			if (wipDefsAtHead[block.index()] == invalidKey) {
				u32 key = wipDefsAtHead[block.index()] = headDefPool.claim();
				auto& defs = headDefPool[key];
				defs.ensureInitialized(mapping.modifiedCount());
				auto defList = defsAtHead[block.index()];
				for (Def def : defList) {
					defs.hasDef.on(def.modifiedVar);
					defs.defs[def.modifiedVar] = def;
				}
				return key;
			}
			return wipDefsAtHead[block.index()];
		};

		// And this function creates the baked defs-at-head list for a block
		// when we start iterating it.

		auto bakeDefsAtHead = [&](Block block) -> DefList {
			u32 key = wipDefsAtHead[block.index()];
			if (key == invalidKey)
				return defsAtHead[block.index()];

			auto& defs = headDefPool[key];

			// We not only add our defs to the common reachingDefs vector, but
			// reset it for the next user. As a minor space optimization, if we
			// already allocated a def list for this block and it's big enough
			// to contain our current reaching defs, we reuse the space.

			if (defs.hasDef.count() <= defsAtHead[block.index()].length) {
				u32 i = 0;
				u32 start = defsAtHead[block.index()].start;
				for (u32 var : defs.hasDef) {
					reachingDefs[start + i ++] = defs.defs[var];
					defs.defs[var] = {};
				}
				defsAtHead[block.index()].length = i;
			} else {
				defsAtHead[block.index()].start = reachingDefs.size();
				for (u32 var : defs.hasDef) {
					reachingDefs.push(defs.defs[var]);
					defs.defs[var] = {};
				}
				defsAtHead[block.index()].length = reachingDefs.size() - defsAtHead[block.index()].start;
			}
			defs.hasDef.clear();
			headDefPool.release(key);
			wipDefsAtHead[block.index()] = invalidKey;
			return defsAtHead[block.index()];
		};

		// Here's some constructors to create Defs for different situations. A
		// default-constructed Def will represent the absence of a def for that
		// variable in the def list. Beyond that, defs have three flavors:
		//  - A def can correspond to the node which defined that value.
		//  - A def can correspond to a phi variable.
		//  - A def can correspond to a function argument.

		auto defFromNode = [&](Node node, i32 modifiedVar, Block block) -> Def {
			Def def;
			def.inhabited = true;
			def.isPhi = false;
			def.isArg = false;
			def.node = node.index();
			def.modifiedVar = modifiedVar;
			def.from = block.index();
			// def.operand = fn.variableById(mapping.originalVariables[modifiedVar]);
			return def;
		};

		auto defFromPhi = [&](u32 phi, i32 modifiedVar, Block block) -> Def {
			Def def;
			def.inhabited = true;
			def.isPhi = true;
			def.isArg = false;
			def.phi = phi;
			def.modifiedVar = modifiedVar;
			def.from = block.index();
			// def.operand = fn.variableById(mapping.originalVariables[modifiedVar]);
			return def;
		};

		auto defFromArg = [&](i32 parameter, i32 modifiedVar, Block block) -> Def {
			Def def;
			def.inhabited = true;
			def.isPhi = false;
			def.isArg = true;
			def.arg = parameter;
			def.modifiedVar = modifiedVar;
			def.from = block.index();
			// def.operand = fn.variableById(mapping.originalVariables[modifiedVar]);
			return def;
		};

		auto defWithFrom = [&](Def def, Block block) -> Def {
			def.from = block.index();
			return def;
		};

		auto logDef = [&](Def def) {
			auto varLogger = OperandLogger { fn, fn.variableById(mapping.originalVariables[def.modifiedVar]) };
			if (def.isPhi) print("(phi:bb", phis[def.phi].block, ")", varLogger);
			else if (def.isArg) print("(arg)", varLogger);
			else print("(node:", def.node, ")", varLogger);
		};

		// Kind of a regrettable first step, but basically necessary to get
		// halfway decent code quality - we compute conservative liveness of
		// the non-SSA variables for each block, so we can avoid inserting phis
		// in blocks where the variable is dead already. We store liveness as a
		// M x N bit matrix, for M blocks and N non-SSA variables.

		vec<BlockIndex> iterationOrder;
		fn.scheduleInReversePostorder(iterationOrder);

		bitset<128> conservativeLiveness;
		bitset<128> liveVariables;

		auto conservativelyLive = [&](i32 var, Block block) -> bool {
			return conservativeLiveness[block.index() * mapping.modifiedCount() + var];
		};

		bool fixpoint = false;
		u32 livenessPass = 0;
		while (!fixpoint) {
			fixpoint = true;
			for (BlockIndex bi : reversed(iterationOrder)) {
				Block block = fn.block(bi);

				for (i32 i = 0; i < mapping.modifiedCount(); i ++) {
					if (conservativelyLive(i, block))
						liveVariables.on(i);
				}
				u32 blockOffset = block.index() * mapping.modifiedCount();
				for (Edge succ : block.successors()) for (Move move : succ.moves()) {
					if (move.src.kind == Operand::Var && mapping.isModified(move.src.var)) {
						i32 mappedVar = mapping[move.src.var];
						liveVariables.on(mappedVar);
						fixpoint = !conservativeLiveness.add(blockOffset + mappedVar) && fixpoint;
					}
				}
				for (i64 i = block.nodeIndices().size() - 1; i >= 0; i --) {
					Node node = block.node(i);
					for (Operand def : node.defs()) if (mapping.isModified(def.var))
						liveVariables.off(mapping[def.var]);
					for (Operand use : node.uses()) if (use.kind == Operand::Var && mapping.isModified(use.var)) {
						i32 mappedVar = mapping[use.var];
						liveVariables.on(mappedVar);
						fixpoint = !conservativeLiveness.add(blockOffset + mappedVar) && fixpoint;
					}
				}
				for (Edge pred : block.predecessors()) {
					for (Move move : pred.moves()) if (mapping.isModified(move.dest.var))
						liveVariables.off(mapping[move.dest.var]);
					for (i32 var : liveVariables)
					fixpoint = !conservativeLiveness.add(pred.srcIndex() * mapping.modifiedCount() + var) && fixpoint;
				}
			}

			if UNLIKELY(config::verboseSSA) {
				println("[SSA]\tConservative liveness info for function ", fn.name(), " after pass ", livenessPass ++, ":");
				for (Block block : fn.blocks()) {
					print("[SSA]\t - bb", block.index(), ":");
					for (i32 i = 0; i < mapping.modifiedCount(); i ++) if (conservativelyLive(i, block))
						print(' ', OperandLogger { fn, fn.variableById(mapping.originalVariables[i]) });
					println();
				}
			}
		}

		// Here's the logic to fully process a block. It's relatively standard
		// stuff - instructions that define variables create new reaching defs,
		// and we make sure our reaching defs unify onto the defs-at-head of
		// each of our successors, creating phis eagerly (and naively) whenever
		// there is a conflict.
		// We also track whether we are at a fixpoint here - specifically, we
		// assume we are not at a fixpoint if we added a new phi.

		fixpoint = false;
		bool isFirstPass = true;
		defined.clear();

		auto processBlock = [&](Block block) {
			DefList defsAtHead = bakeDefsAtHead(block);
			for (Def def : defsAtHead) {
				current.hasDef.on(def.modifiedVar);
				current.defs[def.modifiedVar] = def;
			}

			// For each node in the block, determine if it contains a def for
			// a non-SSA variable. If it does, update our current defs.

			for (Node node : block.nodes()) if (hasDef(node.opcode())) {
				Operand operand = node.defs()[0];
				assert(operand.kind == Operand::Var);
				if (!mapping.isModified(operand.var))
					continue;
				i32 var = mapping[operand.var];

				current.hasDef.on(var);
				current.defs[var] = defFromNode(node, var, block);
				if (isFirstPass) {
					if UNLIKELY(config::verboseSSA) {
						print("[SSA]\tIntroduced new def ");
						logDef(current.defs[var]);
						println(" at node ", node, " in bb", block.index());
					}
					nodeDefs[node.index()] = defined[var] ? fn.newVersionOf(fn.variableById(mapping.originalVariables[var])) : operand;
					if (!defined[var])
						defined.on(var);
				}
			}

			// For each successor of the block, unify our reaching defs with
			// theirs. If they currently have a reaching def for a variable,
			// and our def doesn't match, we introduce a phi.

			for (Edge edge : block.successors()) {
				Block succ = edge.dest();
				u32 key = wipDefsForBlock(succ);
				auto& succDefs = headDefPool[key];

				for (u32 var : current.hasDef) {
					if (!conservativelyLive(var, succ))
						continue;

					// If there is not already a def of this var in the
					// successor, use the one from this block.
					if (!succDefs.hasDef[var]) {
						succDefs.hasDef.on(var);
						succDefs.defs[var] = defWithFrom(current.defs[var], block); // Update the "from" block of the def to the immediate predecessor.
						continue;
					}

					// If there is a def, and it's the same as ours, continue.
					if (succDefs.defs[var] == current.defs[var])
						continue;

					// If there is a def, and it's already a phi local to that
					// successor, continue.
					if (succDefs.defs[var].isPhi && phis[succDefs.defs[var].phi].block == succ.index())
						continue;

					// If there is a def, and it comes from the current block,
					// replace it with our new def.
					if (succDefs.defs[var].from == block.index()) {
						if UNLIKELY(config::verboseSSA) {
							print("[SSA]\tUpdated def from predecessor bb", block.index(), " from ");
							logDef(succDefs.defs[var]);
							print(" to ");
							logDef(current.defs[var]);
							println(" at head of bb", succ.index());

							print("[SSA]\tDefs at head after this change: ");
							for (u32 var : succDefs.hasDef)
								print(" "), logDef(succDefs.defs[var]);
							println();
						}

						succDefs.defs[var] = defWithFrom(current.defs[var], block);
						continue;
					}

					// Otherwise, we have two conflicting definitions for this
					// variable, and need to introduce a phi.
					if UNLIKELY(config::verboseSSA) {
						print("[SSA]\tIntroduced phi between ");
						logDef(current.defs[var]);
						print(" and ");
						logDef(succDefs.defs[var]);
						println(" in bb", succ.index());
					}

					succDefs.defs[var] = defFromPhi(phis.size(), var, succ);
					phis.push(Phi { .block = succ.index(), .operand = fn.newVersionOf(fn.variableById(mapping.originalVariables[var]))});
					fixpoint = false;
				}
			}
			current.reset();
		};

		// Now we've finished setting things up, and start iterating over the
		// function itself. We initialize the entrypoint with defs for the
		// function's parameters, and then iterate the blocks in reverse
		// postorder until we reach a fixpoint.

		defsAtHead[fn.entrypoint].start = reachingDefs.size();
		for (const auto& [i, parameter] : enumerate(fn.parameters)) {
			if (mapping.isModified(parameter.operand.var)) {
				reachingDefs.push(defFromArg(i, mapping[parameter.operand.var], fn.block(fn.entrypoint)));
				defined.on(mapping[parameter.operand.var]);
			}
		}
		defsAtHead[fn.entrypoint].length = reachingDefs.size() - defsAtHead[fn.entrypoint].start;

		u32 pass = 0;
		while (!fixpoint) {
			fixpoint = true;
			for (BlockIndex index : iterationOrder)
				processBlock(fn.block(index));
			isFirstPass = false;

			if UNLIKELY(config::verboseSSA) {
				println("[SSA]\tDefs at head after pass ", pass, ":");
				for (Block block : fn.blocks()) {
					print("[SSA]\t - bb", block.index(), ":");
					for (Def def : defsAtHead[block.index()])
						print(" "), logDef(def);
					println();
				}
			}

			pass ++;
		}

		// At this point, we expect the defs at the head of each block to have
		// stabilized, and each one should have a unique operand associated
		// with it. We now traverse each block and update its variable uses to
		// use the newly introduced renamed variables, as well as creating
		// moves along outgoing edges into any phis that were created.

		auto operandFromDef = [&](Def def) -> Operand {
			if (def.isPhi)
				return phis[def.phi].operand;
			else if (def.isArg)
				return fn.parameters[def.arg].operand;
			return nodeDefs[def.node];
		};

		auto updateBlock = [&](Block block) {
			DefList head = defsAtHead[block.index()];
			for (Def def : head) {
				current.hasDef.on(def.modifiedVar);
				current.defs[def.modifiedVar] = def;
			}

			if UNLIKELY(config::verboseSSA) {
				print("[SSA]\tUpdating operands in bb", block.index(), " with defs at head");
				for (Def def : defsAtHead[block.index()])
					print(" "), logDef(def), print("=", OperandLogger { fn, operandFromDef(def) });
				println();
			}

			// For each node in the block, determine if it contains a def for
			// a non-SSA variable. If it does, update our current defs.

			for (Node node : block.nodes()) {
				for (Operand& operand : node.uses()) if (operand.kind == Operand::Var && mapping.isModified(operand.var))
					operand = operandFromDef(current.defs[mapping[operand.var]]);

				if (hasDef(node.opcode())) {
					Operand& operand = node.defs()[0];
					assert(operand.kind == Operand::Var);
					if (!mapping.isModified(operand.var))
						continue;
					i32 var = mapping[operand.var];
					current.hasDef.on(var);
					current.defs[var] = defFromNode(node, var, block);
					operand = operandFromDef(current.defs[var]);
				}
			}

			// For each successor of the block, unify our reaching defs with
			// theirs. If they currently have a reaching def for a variable,
			// and our def doesn't match, we introduce a phi.

			for (Edge edge : block.successors()) {
				Block succ = edge.dest();
				auto succDefs = defsAtHead[succ.index()];

				for (Move& move : edge.moves()) {
					if (move.src.kind == Operand::Var && mapping.isModified(move.src.var))
						move.src = operandFromDef(current.defs[mapping[move.src.var]]);
				}

				for (Def succDef : succDefs) if (succDef.isPhi) {
					i32 var = succDef.modifiedVar;
					assert(current.hasDef[var]);
					if (current.defs[var] == succDef)
						continue;
					edge.addMove({ operandFromDef(current.defs[var]), operandFromDef(succDef) });
				}
			}
			current.reset();
		};

		for (BlockIndex block : iterationOrder)
			updateBlock(fn.block(block));

		ctx.did(SSA);
    }
}
