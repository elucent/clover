#ifndef JASMINE_REDUCE_H
#define JASMINE_REDUCE_H

#include "jasmine/ir.h"
#include "jasmine/pass.h"

namespace jasmine {
    #define DEFINE_ENUM(upper, ...) upper,
    enum class TreeElement : u8 {
        FOR_EACH_OPCODE(DEFINE_ENUM)
        IntConst, FirstNonOpcode = IntConst, F32Const, F64Const, Var, Unknown
    };
    #undef DEFINE_ENUM

    enum class AbstractType {
        #define DEFINE_ENUM(upper, lower, index) upper = 1 << ~(index),
        FOR_EACH_TYPE_KIND(DEFINE_ENUM)
        #undef DEFINE_ENUM

        STRUCT = 1 << (16 + (i32)CompoundType::STRUCT),
        UNION = 1 << (16 + (i32)CompoundType::UNION),
        ARRAY = 1 << (16 + (i32)CompoundType::ARRAY),
        VECTOR = 1 << (16 + (i32)CompoundType::VECTOR),
        FUNCTION = 1 << (16 + (i32)CompoundType::FUNCTION),

        ANY_SIGNED = I8 | I16 | I32 | I64,
        ANY_UNSIGNED = U8 | U16 | U32 | U64,
        ANY_INT = ANY_SIGNED | ANY_UNSIGNED | PTR | REF,
        ANY_FLOAT = F32 | F64,
        ANY_NUMBER = ANY_INT | ANY_FLOAT,
        ANY = ANY_NUMBER | VOID | UNDEFINED | BOOL | STRUCT | UNION | FUNCTION | VECTOR | ARRAY
    };

    inline AbstractType abstractType(Function& fn, TypeIndex index) {
        if LIKELY(index < 0)
            return AbstractType(1 << ~index);
        return AbstractType(1 << (i32(fn.typeContext()[index].kind()) + 16));
    }

    inline AbstractType operator&(AbstractType a, AbstractType b) {
        return AbstractType(i32(a) & i32(b));
    }

    inline bool operator!(AbstractType a) {
        return !i32(a);
    }

    struct TreeNode {
        u8 length;
        TreeElement children[0];

        inline const_slice<TreeElement> slice() const {
            return const_slice<TreeElement>{ children, length };
        }
    };

    struct MatchResult {
        Operand operand;
        i32 newNode;

        inline static MatchResult fail() {
            return { Operand(), -2 };
        }

        inline static MatchResult success(Node node) {
            return { Operand(), (i32)node.index() };
        }

        inline static MatchResult success(Operand operand) {
            return { operand, -1 };
        }

        inline operator bool() {
            return newNode >= -1;
        }

        inline bool isNode() const {
            return newNode >= 0;
        }

        inline bool isOperand() const {
            return newNode == -1;
        }
    };

    using ReductionRule = MatchResult(*)(PassContext&, Function&, Block, u32, Node);

    constexpr u8 TreeSignatureLength = 8;

    inline constexpr u8 signTreeElement(TreeElement element) {
        u8 base = u8(element);
        return u8((base ^ 149u) * 227u);
    }

    inline constexpr u16 signTreeElement(Opcode element) {
        return signTreeElement((TreeElement)element);
    }

    struct ReductionRules {
        bitset<128> doesOpcodeHaveRules;
        struct Rule {
            TreeSignature signature;
            ReductionRule reduction;
            const i8* name;
        };

        struct RuleSet {
            bitset<256> bloomFilter;
            vec<Rule> rules;

            inline void addToFilter(u64 sig) {
                bloomFilter.on((sig ^ 16726033172258207761ull) * 9697606981381870189ull % 256);
                bloomFilter.on((sig ^ 14564523179542843753ull) * 16045227785858833697ull % 256);
                bloomFilter.on((sig ^ 9956883800679509063ull) * 18403160783636127047ull % 256);
                bloomFilter.on((sig ^ 12989512169740916717ull) * 17774536028827766389ull % 256);
            }

            inline void add(TreeSignature signature, ReductionRule rule, const i8* name) {
                rules.push({ signature, rule, name });
                addToFilter(signature.sig);
            }

            inline bool mayContain(TreeSignature signature) {
                u64 sig = signature.sig;
                return bloomFilter[(sig ^ 16726033172258207761ull) * 9697606981381870189ull % 256]
                    && bloomFilter[(sig ^ 14564523179542843753ull) * 16045227785858833697ull % 256]
                    && bloomFilter[(sig ^ 9956883800679509063ull) * 18403160783636127047ull % 256]
                    && bloomFilter[(sig ^ 12989512169740916717ull) * 17774536028827766389ull % 256];
            }
        };

        vec<u16> data;
        vec<RuleSet*, 128> ruleSets;

        inline ReductionRules() {
            ruleSets.expandTo((u32)NUM_OPCODES, nullptr);
        }

        inline ~ReductionRules() {
            for (auto p : ruleSets) if (p)
                delete p;
        }

        inline RuleSet& ensureRuleSet(Opcode opcode) {
            if (!ruleSets[(i32)opcode])
                ruleSets[(i32)opcode] = new RuleSet();
            return *ruleSets[(i32)opcode];
        }

        inline void add(Opcode opcode, ReductionRule rule, TreeSignature signature, const i8* name) {
            doesOpcodeHaveRules.on((u32)opcode);
            ensureRuleSet(opcode).add(signature, rule, name);
        }
    };
}

#endif