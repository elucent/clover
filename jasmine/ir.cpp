#include "jasmine/ir.h"
#include "util/bits.h"
#include "jasmine/mod.h"
#include "util/init.h"
#include "jasmine/pass.h"

namespace jasmine {
    static bool isInited = false;
    void ensureInited() {
        if (isInited) return;
        isInited = true;

        if UNLIKELY(config::printJasminePassTimes)
            atexit([](){ PassTimer::printSums(); });
        if UNLIKELY(config::printJasmineCompileStats)
            atexit([](){ CompileStats::printStats(); });
    }

    #define DEFINE_NAME(upper, lower, ...) #lower,
    const char* OPCODE_NAMES[NUM_OPCODES] = {
        FOR_EACH_OPCODE(DEFINE_NAME)
    };
    #undef DEFINE_NAME

    #define DEFINE_NAME(upper, lower, ...) #upper,
    const char* OPCODE_NAMES_UPPER[NUM_OPCODES] = {
        FOR_EACH_OPCODE(DEFINE_NAME)
    };
    #undef DEFINE_NAME

    Function::Function(Module& mod_in, Symbol sym_in, TypeIndex returnType_in, FunctionFlags flags_in):
        mod(&mod_in), sym(sym_in), entrypoint(0), returnType(returnType_in), constants(&mod->constants), flags(flags_in) {
        ensureInited();
    }

    const TargetInterface& Function::target() const {
        return *mod->target;
    }

    const TypeContext& Function::typeContext() const {
        return mod->typeContext();
    }

    TypeContext& Function::typeContext() {
        return mod->typeContext();
    }

    SymbolTable& Function::syms() {
        return mod->syms;
    }

    const SymbolTable& Function::syms() const {
        return mod->syms;
    }

    void Function::executeInsertions() {
        for (Block block : blocks()) {
            auto& inserts = blockInsertions[block.index()];
            if (inserts.size() == 0)
                continue;

            u32 oldSize = block.nodeIndices().size();
            u32 oldLength = block.dims().length;
            u32 newSize = oldSize;
            for (u16 i : inserts) newSize += insertions[i].length;
            if (inserts.size() > 1)
                sort(inserts, [&](u16 a, u16 b) -> bool { return insertions[a].indexInBlock < insertions[b].indexInBlock; });

            u32 newLength = newSize + (oldLength - oldSize);
            block.growTo(newLength + 4);

            NodeIndex* writer = &block.nodeIndices()[0] + newSize;
            const NodeIndex* reader = &block.nodeIndices()[0] + oldSize;
            for (i32 i = i32(oldSize) - 1; i >= 0; i --) {
                *--writer = *--reader;
                while (i == insertions[inserts.back()].indexInBlock / 2) {
                    Insertion insertion = insertions[inserts.back()];
                    inserts.pop();
                    for (i32 j = i32(insertion.length) - 1; j >= 0; j --)
                        *--writer = nodesToInsert[insertion.offset + j];
                    if (inserts.size() == 0) {
                        i = -1;
                        break;
                    }
                }
            }
            inserts.clear();
            block.dims().length += newSize - oldSize;
        }
        insertions.clear();
        nodesToInsert.clear();
    }

    void traverseBlocksPreorder(vec<BlockIndex>& indices, const Function& fn, bitset<256>& visited, BlockIndex index) {
        if (visited[index])
            return;
        indices.push(index);
        visited[index] = true;
        Block block = fn.block(index);
        for (Edge successor : block.successors()) {
            if (!visited[successor.destIndex()])
                traverseBlocksPreorder(indices, fn, visited, successor.destIndex());
        }
    }

    void traverseBlocksPostorder(vec<BlockIndex>& indices, const Function& fn, bitset<256>& visited, BlockIndex index) {
        if (visited[index])
            return;
        visited[index] = true;
        Block block = fn.block(index);
        for (Edge successor : block.successors()) {
            if (!visited[successor.destIndex()])
                traverseBlocksPostorder(indices, fn, visited, successor.destIndex());
        }
        indices.push(index);
    }

    void traverseBlocksTopologically(vec<BlockIndex>& indices, const Function& fn, bitset<256>& tempMarks, bitset<256>& permMarks, BlockIndex index) {
        if (permMarks[index])
            return;
        if (tempMarks[index]) // We have a cycle; ignore this for now.
            return;

        tempMarks[index] = true;
        Block block = fn.block(index);
        auto successors = block.successorIndices();
        for (i32 i = successors.size() - 1; i >= 0; -- i)
            traverseBlocksTopologically(indices, fn, tempMarks, permMarks, fn.edge(successors[i]).destIndex());
        permMarks[index] = true;
        indices.push(index);
    }

    void Function::scheduleInPreorder(vec<BlockIndex>& indices) const {
        bitset<256> visited;
        traverseBlocksPreorder(indices, *this, visited, entrypoint);
    }

    void Function::scheduleInPostorder(vec<BlockIndex>& indices) const {
        bitset<256> visited;
        traverseBlocksPostorder(indices, *this, visited, entrypoint);
    }

    void Function::scheduleInReversePostorder(vec<BlockIndex>& indices) const {
        bitset<256> visited;
        traverseBlocksPostorder(indices, *this, visited, entrypoint);
        reverse(indices);
    }

    void Function::scheduleTopologically(vec<BlockIndex>& indices) const {
        bitset<256> tempMarks, permMarks;
        traverseBlocksTopologically(indices, *this, tempMarks, permMarks, entrypoint);
        for (u32 i = 0; i < indices.size() / 2; i ++)
            swap(indices[i], indices[indices.size() - i - 1]);
    }

    void Function::validateAggressively() const {
        if (validatingCount) return;

        validatingCount ++;
        for (Block block : blocks()) {
            for (Node node : block.nodes()) {
                assert((u32)node.opcode() < NUM_OPCODES);
                for (Operand operand : node.operands())
                    assert(operand.kind != Operand::Invalid);
            }
        }
        validatingCount --;
    }
}