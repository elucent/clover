#include "clover/typecheck.h"
#include "clover/ast.h"
#include "clover/resolve.h"
#include "util/config.h"

namespace clover {

    /*
     * Type checking
     * -------------
     *
     * In Clover, we adopt a two-phase approach to typechecking. It's similar
     * in some ways to a bidirectional checker, but we turn each direction into
     * a pass of its own.
     *
     * First, the inference or evaluation phase. This is a traversal of the AST
     * that assigns a plausible type to every node, with the aim of figuring
     * out what we know about the type of each value. Since we can't rely on
     * types being concrete, we generally leave a lot of placeholders, in the
     * form of type variables, hoping to infer reasonable types for them based
     * on usage. It's also legal to use types that aren't legal at runtime for
     * unification purposes, such as integers with non-native bit widths
     * (i.e. u23 or i5). Despite being generally conservative, it's possible to
     * discover type errors in the inference phase, specifically when two type
     * constraints prove to be incompatible. After inference, we expect every
     * node to have a valid type.
     *
     * Next, the checking phase. In this phase, we take the best-guess type
     * assignments from the inference phase, and make them concrete. For each
     * expression, we pick a type that:
     *  - meets all the constraints inferred in the inference phase,
     *  - is allowed by the expression kind,
     *  - and is legal at runtime, so things like i5 get expanded to i8.
     * We are still inferring constraints in this phase when we can! Picking a
     * type is a fallible operation, and if no valid type is found, that's a
     * type error. But often, if we do pick a valid type, it allows us to
     * tighten the constraints of other types involved in the expression. So we
     * generally try to propagate this information by re-unifying concrete
     * types with remaining variable types.
     */

    struct InferenceContext;

    struct Evaluation {
        TypeIndex t : Limits::TypesPerCompilationBits;
        enum EvaluationKind {
            TypeEvaluation,
            NodeEvaluation,
            UnsignedConst,
            IntConst,
            FloatConst,
            BoolConst,
            CharConst,
            FirstConstant = UnsignedConst
        };

        EvaluationKind kind = TypeEvaluation;
        ASTWord ast;

        inline TypeIndex typeIndex() const {
            return t;
        }

        inline Type type(TypeSystem& ctx) const {
            return ctx.get(typeIndex());
        }

        inline Type type(TypeSystem* ctx) const {
            return type(*ctx);
        }

        inline Type type(Module* ctx) const {
            return type(*ctx->types);
        }

        inline bool isType() const {
            return kind == TypeEvaluation;
        }

        inline bool isNode() const {
            return kind == NodeEvaluation;
        }

        inline bool isKnownValue() const {
            return kind >= FirstConstant;
        }

        inline bool isKnownBool() const {
            return kind == BoolConst;
        }

        inline bool asBool(Module* module) const {
            assert(isKnownBool());
            return AST(module, ast).boolConst();
        }

        inline bool isKnownChar() const {
            return kind == CharConst;
        }

        inline u32 asChar(Module* module) const {
            assert(isKnownChar());
            return AST(module, ast).charConst();
        }

        inline bool isKnownFloat() const {
            return kind == FloatConst;
        }

        inline double asFloat(Module* module) const {
            if LIKELY(isKnownFloat())
                return AST(module, ast).floatConst();
            if (isKnownSigned())
                return double(asSigned(module));
            if (isKnownUnsigned())
                return double(asUnsigned(module));
            unreachable("Failed to convert to float.");
        }

        inline bool isKnownSigned() const {
            return kind == IntConst;
        }

        inline bool isKnownUnsigned() const {
            return kind == UnsignedConst;
        }

        inline bool isKnownInteger() const {
            return isKnownSigned() || isKnownUnsigned();
        }

        inline u64 asUnsigned(Module* module) const {
            if (isKnownUnsigned())
                return AST(module, ast).uintConst();
            if (isKnownSigned()) {
                i64 i = AST(module, ast).intConst();
                assert(i >= 0);
                return i;
            }
            unreachable("Failed to convert to unsigned.");
        }

        inline i64 asSigned(Module* module) const {
            if (isKnownSigned())
                return AST(module, ast).intConst();
            if (isKnownUnsigned()) {
                u64 u = AST(module, ast).uintConst();
                assert(u < 1ull << 63);
                return u;
            }
            unreachable("Failed to convert to signed.");
        }

        inline AST node(Module* module) {
            if (ast.isRef())
                return module->node(ast.child);
            else
                return AST(module, ast);
        }
    };

    inline Evaluation fromType(TypeIndex type) { return { .t = type, .kind = Evaluation::TypeEvaluation }; }
    inline Evaluation fromType(Type type) { return fromType(type.index); }

    inline Evaluation fromNode(TypeIndex type, AST ast) {
        Evaluation eval;
        eval.t = type;
        eval.kind = Evaluation::NodeEvaluation;
        eval.ast.makeRef(ast.node);
        return eval;
    }

    // Bit of a tricky representation here. To save on memory and simplify node
    // replacement, constant values are represented as AST nodes that encode
    // that constant. For primitives, that means inline-encoded AST nodes. If
    // we expand constant prop to compound types in the future, those might be
    // references to compound AST nodes instead.
    //
    // A constant evaluation can never evolve in future iterations to discover
    // new type constraints. So, constants are always considered Boring.

    inline Evaluation makeSigned(Module* module, i64 i) {
        AST ast = module->add(ASTKind::Int, Constant::IntConst(i));
        return Evaluation { .t = I64, .kind = Evaluation::IntConst, .ast = ast.firstWord() };
    }

    inline Evaluation makeUnsigned(Module* module, u64 i) {
        AST ast = module->add(ASTKind::Unsigned, Constant::UnsignedConst(i));
        return Evaluation { .t = U64, .kind = Evaluation::UnsignedConst, .ast = ast.firstWord() };
    }

    inline Evaluation makeFloat(Module* module, f64 f) {
        AST ast = module->add(ASTKind::Float, Constant::FloatConst(f));
        return Evaluation { .t = F64, .kind = Evaluation::FloatConst, .ast = ast.firstWord() };
    }

    inline Evaluation makeBool(Module* module, bool b) {
        AST ast = module->add(ASTKind::Bool, Constant::BoolConst(b));
        return Evaluation { .t = Bool, .kind = Evaluation::BoolConst, .ast = ast.firstWord() };
    }

    inline Evaluation makeChar(Module* module, u32 c) {
        AST ast = module->add(ASTKind::Char, Constant::CharConst(c));
        return Evaluation { .t = Char, .kind = Evaluation::CharConst, .ast = ast.firstWord() };
    }

    inline Type naturalType(Module* module, Evaluation evaluation) {
        if (evaluation.isKnownValue()) {
            if (evaluation.isKnownFloat())
                return module->types->get(ReservedTypes::RangeF32F64); // Optimistically assume f32.
            if (evaluation.isKnownUnsigned() || (evaluation.isKnownSigned() && evaluation.asSigned(module) >= 0)) {
                u64 value = evaluation.isKnownSigned() ? (u64)evaluation.asSigned(module) : evaluation.asUnsigned(module);
                u64 bits = 64 - clz64(value);
                return module->types->unsignedRange(bits);
            }
            if (evaluation.isKnownSigned()) {
                i64 value = evaluation.asSigned(module);
                assert(value < 0); // Otherwise treat it like an unsigned.
                u64 positive = -(value + 1);
                u64 bits = 64 - clz64(positive);
                assert(bits <= 63); // Otherwise, we shouldn't have been a signed in the first place.
                return module->types->signedRange(bits + 1);
            }
            if (evaluation.isKnownBool())
                return module->boolType();
            if (evaluation.isKnownChar())
                return module->charType();
            unreachable("Could not pick natural type for constant value.");
        }
        return evaluation.type(module->types);
    }

    // Here's where we do obligate constant folding, first for unary operators.
    // If an expression is safely foldable and creates no observable effects
    // (including both runtime effects like overflow, and compile-time effects
    // like type errors) we in-place substitute the more complicated expression
    // with a literal constant node.
    //
    // Because we expect all constant folding to be resolved in the initial
    // pass, because it doesn't depend on more derived type information, no new
    // node we create is considered "interesting" yet. Constant evaluations are
    // implicitly Boring, all type evaluations originating in fold() should
    // also be Boring.

    Evaluation fold(Module* module, Function* function, AST ast, Evaluation operand) {
        switch (ast.kind()) {
            case ASTKind::Plus:
            case ASTKind::Paren:
                return operand;
            case ASTKind::Minus:
                if (operand.isKnownFloat())
                    return makeFloat(module, -operand.asFloat(module));
                if (operand.isKnownUnsigned()) {
                    auto u = operand.asUnsigned(module);
                    if (u == 0) // -0 = 0
                        return operand;
                    if (u <= 1ull << 63)
                        return makeSigned(module, i64(~(u - 1ull)));
                    return fromType(U64);
                }
                if (operand.isKnownSigned()) {
                    auto i = operand.asSigned(module);
                    if (i < 0)
                        return makeUnsigned(module, u64(~i) + 1ull);
                    return makeSigned(module, -i);
                }
                unreachable("Unexpected constant kind when folding ", ast);
            case ASTKind::Not:
                assert(operand.isKnownBool());
                return makeBool(module, !operand.asBool(module));
            case ASTKind::BitNot:
                if (operand.isKnownSigned())
                    return makeSigned(module, ~operand.asSigned(module));
                if (operand.isKnownUnsigned())
                    return makeUnsigned(module, ~operand.asUnsigned(module));
                unreachable("Unexpected constant kind when folding ", ast);
            default:
                unreachable("Tried to fold unfoldable node ", ast);
        }
    }

    // Helper to fold binary expressions with all-constant operands.

    Evaluation::EvaluationKind unifyKind(Module* module, const Evaluation& lhs, const Evaluation& rhs) {
        if (lhs.kind == rhs.kind)
            return lhs.kind;
        if (lhs.isKnownFloat() || rhs.isKnownFloat())
            return Evaluation::FloatConst;
        if ((lhs.isKnownSigned() && rhs.isKnownUnsigned()) || (lhs.isKnownUnsigned() && rhs.isKnownSigned())) {
            i64 i = lhs.isKnownSigned() ? lhs.asSigned(module) : rhs.asSigned(module);
            u64 u = lhs.isKnownUnsigned() ? lhs.asUnsigned(module) : rhs.asUnsigned(module);
            if (i >= 0)
                return Evaluation::UnsignedConst;
            if (u >= 1ull << 63ull)
                return Evaluation::TypeEvaluation; // Can't unify.
            return Evaluation::IntConst;
        }
        unreachable("Couldn't unify constant kinds.");
    }

    // ...and here's the folding rules for binary operators.

    Evaluation fold(Module* module, Function* function, AST ast, Evaluation lhs, Evaluation rhs) {
        // We return a type-only evaluation if folding failed.

        auto destKind = unifyKind(module, lhs, rhs);
        switch (ast.kind()) {
            case ASTKind::Add:
                if (destKind == Evaluation::FloatConst)
                    return makeFloat(module, lhs.asFloat(module) + rhs.asFloat(module));
                if (destKind == Evaluation::UnsignedConst) {
                    u64 l = lhs.asUnsigned(module), r = rhs.asUnsigned(module);
                    if (addOverflows<u64>(l, r))
                        return fromType(U64); // Don't fold.
                    return makeUnsigned(module, l + r);
                }
                if (destKind == Evaluation::IntConst) {
                    i64 l = lhs.asSigned(module), r = rhs.asSigned(module);
                    if (addOverflows<i64>(l, r))
                        return fromType(I64); // Don't fold.
                    return makeSigned(module, l + r);
                }
                unreachable("Unexpected constant kind when folding ", ast);
            case ASTKind::Sub:
                if (destKind == Evaluation::FloatConst)
                    return makeFloat(module, lhs.asFloat(module) - rhs.asFloat(module));
                if (destKind == Evaluation::UnsignedConst) {
                    u64 l = lhs.asUnsigned(module), r = rhs.asUnsigned(module);
                    if (subOverflows<u64>(l, r))
                        return fromType(U64); // Don't fold.
                    return makeUnsigned(module, l - r);
                }
                if (destKind == Evaluation::IntConst) {
                    i64 l = lhs.asSigned(module), r = rhs.asSigned(module);
                    if (subOverflows<i64>(l, r))
                        return fromType(I64); // Don't fold.
                    return makeSigned(module, l - r);
                }
                unreachable("Unexpected constant kind when folding ", ast);
            case ASTKind::Mul:
                if (destKind == Evaluation::FloatConst)
                    return makeFloat(module, lhs.asFloat(module) * rhs.asFloat(module));
                if (destKind == Evaluation::UnsignedConst) {
                    u64 l = lhs.asUnsigned(module), r = rhs.asUnsigned(module);
                    if (mulOverflows<u64>(l, r))
                        return fromType(U64); // Don't fold.
                    return makeUnsigned(module, l * r);
                }
                if (destKind == Evaluation::IntConst) {
                    i64 l = lhs.asSigned(module), r = rhs.asSigned(module);
                    if (mulOverflows<i64>(l, r))
                        return fromType(I64); // Don't fold.
                    return makeSigned(module, l * r);
                }
                unreachable("Unexpected constant kind when folding ", ast);
            case ASTKind::Div:
                if (destKind == Evaluation::FloatConst)
                    return makeFloat(module, lhs.asFloat(module) / rhs.asFloat(module));
                if (destKind == Evaluation::UnsignedConst) {
                    u64 l = lhs.asUnsigned(module), r = rhs.asUnsigned(module);
                    if (r == 0)
                        return fromType(U64); // Don't fold if we'd divide by zero.
                    return makeUnsigned(module, l / r);
                }
                if (destKind == Evaluation::IntConst) {
                    i64 l = lhs.asSigned(module), r = rhs.asSigned(module);
                    if (r == 0 || (r == -1 && (l & l + 1) == 0 && clz64(l) % 8 == 0))
                        return fromType(I64); // Don't fold if we'd divide by zero, or underflow.
                    return makeSigned(module, l / r);
                }
                unreachable("Unexpected constant kind when folding ", ast);
            case ASTKind::Rem:
                if (destKind == Evaluation::FloatConst)
                    return makeFloat(module, frem(lhs.asFloat(module), rhs.asFloat(module)));
                if (destKind == Evaluation::UnsignedConst) {
                    u64 l = lhs.asUnsigned(module), r = rhs.asUnsigned(module);
                    if (r == 0)
                        return fromType(U64); // Don't fold if we'd divide by zero.
                    return makeUnsigned(module, l % r);
                }
                if (destKind == Evaluation::IntConst) {
                    i64 l = lhs.asSigned(module), r = rhs.asSigned(module);
                    if (r == 0 || (r == -1 && (l & l + 1) == 0 && clz64(l) % 8 == 0))
                        return fromType(I64); // Don't fold if we'd divide by zero, or underflow.
                    return makeSigned(module, l % r);
                }
                unreachable("Unexpected constant kind when folding ", ast);
            case ASTKind::Exp:
                if (destKind == Evaluation::FloatConst)
                    return makeFloat(module, fpow(lhs.asFloat(module), rhs.asFloat(module)));
                if (destKind == Evaluation::UnsignedConst) {
                    u64 l = lhs.asUnsigned(module), r = rhs.asUnsigned(module);
                    if (auto result = upowOrOverflow(l, r))
                        return makeUnsigned(module, *result);
                    return fromType(U64); // Don't fold if we overflow.
                }
                if (destKind == Evaluation::IntConst) {
                    i64 l = lhs.asSigned(module), r = rhs.asSigned(module);
                    if (r < 0)
                        return makeFloat(module, fpow(l, r));
                    if (auto result = ipowOrOverflow(l, r))
                        return makeSigned(module, *result);
                    return fromType(I64); // Don't fold if we overflow.
                }
                unreachable("Unexpected constant kind when folding ", ast);
            case ASTKind::BitAnd:
                assert(destKind == Evaluation::UnsignedConst || destKind == Evaluation::IntConst);
                if (destKind == Evaluation::UnsignedConst) {
                    u64 l = lhs.asUnsigned(module), r = rhs.asUnsigned(module);
                    return makeUnsigned(module, l & r);
                }
                if (destKind == Evaluation::IntConst) {
                    i64 l = lhs.asSigned(module), r = rhs.asSigned(module);
                    return makeSigned(module, l & r);
                }
                unreachable("Unexpected constant kind when folding ", ast);
            case ASTKind::BitXor:
                assert(destKind == Evaluation::UnsignedConst || destKind == Evaluation::IntConst);
                if (destKind == Evaluation::UnsignedConst) {
                    u64 l = lhs.asUnsigned(module), r = rhs.asUnsigned(module);
                    return makeUnsigned(module, l ^ r);
                }
                if (destKind == Evaluation::IntConst) {
                    i64 l = lhs.asSigned(module), r = rhs.asSigned(module);
                    return makeSigned(module, l ^ r);
                }
                unreachable("Unexpected constant kind when folding ", ast);
            case ASTKind::BitOr:
                assert(destKind == Evaluation::UnsignedConst || destKind == Evaluation::IntConst);
                if (destKind == Evaluation::UnsignedConst) {
                    u64 l = lhs.asUnsigned(module), r = rhs.asUnsigned(module);
                    return makeUnsigned(module, l | r);
                }
                if (destKind == Evaluation::IntConst) {
                    i64 l = lhs.asSigned(module), r = rhs.asSigned(module);
                    return makeSigned(module, l | r);
                }
                unreachable("Unexpected constant kind when folding ", ast);
            case ASTKind::BitShl:
                assert(destKind == Evaluation::UnsignedConst || destKind == Evaluation::IntConst);
                if (destKind == Evaluation::UnsignedConst) {
                    u64 l = lhs.asUnsigned(module), r = rhs.asUnsigned(module);
                    return makeUnsigned(module, l << r);
                }
                if (destKind == Evaluation::IntConst) {
                    i64 l = lhs.asSigned(module), r = rhs.asSigned(module);
                    return makeSigned(module, l << r);
                }
                unreachable("Unexpected constant kind when folding ", ast);
            case ASTKind::BitShr:
                assert(destKind == Evaluation::UnsignedConst || destKind == Evaluation::IntConst);
                if (destKind == Evaluation::UnsignedConst) {
                    u64 l = lhs.asUnsigned(module), r = rhs.asUnsigned(module);
                    return makeUnsigned(module, l >> r);
                }
                if (destKind == Evaluation::IntConst) {
                    i64 l = lhs.asSigned(module), r = rhs.asSigned(module);
                    return makeSigned(module, l >> r);
                }
                unreachable("Unexpected constant kind when folding ", ast);
            case ASTKind::BitRol:
                assert(destKind == Evaluation::UnsignedConst || destKind == Evaluation::IntConst);
                if (destKind == Evaluation::UnsignedConst) {
                    u64 l = lhs.asUnsigned(module), r = rhs.asUnsigned(module);
                    return makeUnsigned(module, rol<u64>(l, r));
                }
                if (destKind == Evaluation::IntConst) {
                    i64 l = lhs.asSigned(module), r = rhs.asSigned(module);
                    return makeSigned(module, rol<i64>(l, r));
                }
                unreachable("Unexpected constant kind when folding ", ast);
            case ASTKind::BitRor:
                assert(destKind == Evaluation::UnsignedConst || destKind == Evaluation::IntConst);
                if (destKind == Evaluation::UnsignedConst) {
                    u64 l = lhs.asUnsigned(module), r = rhs.asUnsigned(module);
                    return makeUnsigned(module, ror<u64>(l, r));
                }
                if (destKind == Evaluation::IntConst) {
                    i64 l = lhs.asSigned(module), r = rhs.asSigned(module);
                    return makeSigned(module, ror<i64>(l, r));
                }
                unreachable("Unexpected constant kind when folding ", ast);
            case ASTKind::Less:
                assert(destKind == Evaluation::UnsignedConst || destKind == Evaluation::IntConst || destKind == Evaluation::FloatConst);
                if (destKind == Evaluation::UnsignedConst) {
                    u64 l = lhs.asUnsigned(module), r = rhs.asUnsigned(module);
                    return makeBool(module, l < r);
                }
                if (destKind == Evaluation::IntConst) {
                    i64 l = lhs.asSigned(module), r = rhs.asSigned(module);
                    return makeBool(module, l < r);
                }
                if (destKind == Evaluation::FloatConst) {
                    f64 l = lhs.asFloat(module), r = rhs.asFloat(module);
                    return makeBool(module, l < r);
                }
                if (destKind == Evaluation::CharConst) {
                    u32 l = lhs.asChar(module), r = rhs.asChar(module);
                    return makeBool(module, l < r);
                }
                unreachable("Unexpected constant kind when folding ", ast);
            case ASTKind::LessEq:
                assert(destKind == Evaluation::UnsignedConst || destKind == Evaluation::IntConst || destKind == Evaluation::FloatConst);
                if (destKind == Evaluation::UnsignedConst) {
                    u64 l = lhs.asUnsigned(module), r = rhs.asUnsigned(module);
                    return makeBool(module, l <= r);
                }
                if (destKind == Evaluation::IntConst) {
                    i64 l = lhs.asSigned(module), r = rhs.asSigned(module);
                    return makeBool(module, l <= r);
                }
                if (destKind == Evaluation::FloatConst) {
                    f64 l = lhs.asFloat(module), r = rhs.asFloat(module);
                    return makeBool(module, l <= r);
                }
                if (destKind == Evaluation::CharConst) {
                    u32 l = lhs.asChar(module), r = rhs.asChar(module);
                    return makeBool(module, l <= r);
                }
                unreachable("Unexpected constant kind when folding ", ast);
            case ASTKind::Greater:
                assert(destKind == Evaluation::UnsignedConst || destKind == Evaluation::IntConst || destKind == Evaluation::FloatConst);
                if (destKind == Evaluation::UnsignedConst) {
                    u64 l = lhs.asUnsigned(module), r = rhs.asUnsigned(module);
                    return makeBool(module, l > r);
                }
                if (destKind == Evaluation::IntConst) {
                    i64 l = lhs.asSigned(module), r = rhs.asSigned(module);
                    return makeBool(module, l > r);
                }
                if (destKind == Evaluation::FloatConst) {
                    f64 l = lhs.asFloat(module), r = rhs.asFloat(module);
                    return makeBool(module, l > r);
                }
                if (destKind == Evaluation::CharConst) {
                    u32 l = lhs.asChar(module), r = rhs.asChar(module);
                    return makeBool(module, l > r);
                }
                unreachable("Unexpected constant kind when folding ", ast);
            case ASTKind::GreaterEq:
                assert(destKind == Evaluation::UnsignedConst || destKind == Evaluation::IntConst || destKind == Evaluation::FloatConst);
                if (destKind == Evaluation::UnsignedConst) {
                    u64 l = lhs.asUnsigned(module), r = rhs.asUnsigned(module);
                    return makeBool(module, l >= r);
                }
                if (destKind == Evaluation::IntConst) {
                    i64 l = lhs.asSigned(module), r = rhs.asSigned(module);
                    return makeBool(module, l >= r);
                }
                if (destKind == Evaluation::FloatConst) {
                    f64 l = lhs.asFloat(module), r = rhs.asFloat(module);
                    return makeBool(module, l >= r);
                }
                if (destKind == Evaluation::CharConst) {
                    u32 l = lhs.asChar(module), r = rhs.asChar(module);
                    return makeBool(module, l >= r);
                }
                unreachable("Unexpected constant kind when folding ", ast);
            case ASTKind::Equal:
                if (destKind == Evaluation::UnsignedConst) {
                    u64 l = lhs.asUnsigned(module), r = rhs.asUnsigned(module);
                    return makeBool(module, l == r);
                }
                if (destKind == Evaluation::IntConst) {
                    i64 l = lhs.asSigned(module), r = rhs.asSigned(module);
                    return makeBool(module, l == r);
                }
                if (destKind == Evaluation::BoolConst) {
                    bool l = lhs.asBool(module), r = rhs.asBool(module);
                    return makeBool(module, l == r);
                }
                if (destKind == Evaluation::FloatConst) {
                    f64 l = lhs.asFloat(module), r = rhs.asFloat(module);
                    return makeBool(module, l == r);
                }
                if (destKind == Evaluation::CharConst) {
                    u32 l = lhs.asChar(module), r = rhs.asChar(module);
                    return makeBool(module, l == r);
                }
                unreachable("Unexpected constant kind when folding ", ast);
            case ASTKind::NotEqual:
                if (destKind == Evaluation::UnsignedConst) {
                    u64 l = lhs.asUnsigned(module), r = rhs.asUnsigned(module);
                    return makeBool(module, l != r);
                }
                if (destKind == Evaluation::IntConst) {
                    i64 l = lhs.asSigned(module), r = rhs.asSigned(module);
                    return makeBool(module, l != r);
                }
                if (destKind == Evaluation::BoolConst) {
                    bool l = lhs.asBool(module), r = rhs.asBool(module);
                    return makeBool(module, l != r);
                }
                if (destKind == Evaluation::FloatConst) {
                    f64 l = lhs.asFloat(module), r = rhs.asFloat(module);
                    return makeBool(module, l != r);
                }
                if (destKind == Evaluation::CharConst) {
                    u32 l = lhs.asChar(module), r = rhs.asChar(module);
                    return makeBool(module, l != r);
                }
                unreachable("Unexpected constant kind when folding ", ast);
            default:
                return {};
        }
    }

    Type pickTypeFromEqualPair(Type lhs, Type rhs) {
        // Kind of a painful special case...because we have an equality
        // relation, not a directional relation like in other binary ops, we
        // have the possibility of a range type in the input (other ops will
        // eliminate the range as bounds of the result var). So we have an
        // additional step of picking a reasonable type from the inputs to
        // verify.
        bool lhsRange = lhs.isRange(), rhsRange = rhs.isRange();
        if (!lhsRange)
            return lhs;
        if (!rhsRange)
            return rhs;
        return lhs.types->encode<TypeKind::Range>(leastCommonSupertype(lhs.asRange().lowerBound(), rhs.asRange().lowerBound(), NoUnifyFlags), greatestCommonSubtype(lhs.asRange().upperBound(), rhs.asRange().upperBound(), NoUnifyFlags));
    }

    Type concreteType(Module* module, Type type) {
        if (type.index < FirstAbstractReservedType) // Non-abstract reserved types are canonical.
            return type;
        if (type.isRange())
            return canonicalTypeInBounds(module->types, type.asRange().lowerBound(), type.asRange().upperBound());
        type.concretify();
        return expand(type);
    }

    // Moving on to actual inference. Type inference in Clover involves three
    // passes:
    //
    // In the first pass, the "discovery" pass, we visit all nodes in the
    // provided AST, and try and discover type constraints. Trivially typed
    // nodes like constants return constant evaluations, and we try to fold
    // constant operations as much as possible to refine our inferred types.
    // In cases where we do not know the type, we create a type variable, and
    // bind it to the node that produces the value of that type. We tentatively
    // rely on the fact that all type variables must be created in this pass,
    // and not later. Once the pass is done, we call into the type constraint
    // solver (see clover::Constraints in type.h) to compute sensible upper and
    // lower bounds for all nodes. At this stage, we also establish an ordering
    // of nodes to revisit in the next pass.
    //
    // In the next pass, we use dependency information learned from the first
    // pass to revisit a subset of nodes in topological order. Nodes with
    // cyclic dependencies on each other, specifically the members of a
    // strongly-connected component, are visited in arbitrary order. We use
    // this pass to make typechecking decisions that rely on previously known
    // information but can't encode their behavior easily into a system of
    // subtyping constraints - consider something like overloading, which is
    // easy to express procedurally as a ranking of different options, but hard
    // to represent structurally (needing intersection types or some relative).
    // Since we can only learn typing information either from the first pass,
    // or from other nodes in this pass, we use this dependency ordering to try
    // and ensure whenever possible that all possible information that may
    // influence the resolution of a node is available by the time we reach it.
    //
    // In the final pass, we do any remaining checks that rely on concrete type
    // information, and are otherwise difficult to integrate into the
    // constraint graph. This includes things like constructors, where the
    // result type is known even before type inference, but checking if the
    // constructor is valid requires concrete knowledge of the inputs. If we
    // handled such cases via ordering constraints, we run the risk of picking
    // too early, when there's really no need to do so since checking the
    // constructor does not influence the values of other type variables.

    struct InferenceContext {
        vec<NodeIndex, 16>* lateChecks;
        vec<TypeIndex, 16>* lateResolves;
        vec<NodeIndex, 16>* discoveredFunctions;
        Constraints* constraints;

        inline void checkLater(AST ast) {
            assert(!ast.isLeaf()); // Otherwise it's hard to recover information about this node later.
            lateChecks->push(ast.node);
        }

        inline void ensureResolved(Type type) {
            if UNLIKELY(config::verboseUnify >= 2)
                println("[TYPE]\tMarking type ", type, " as resolved later.");
            lateResolves->push(type.index);
        }
    };

    Evaluation infer(InferenceContext& ctx, Function* function, AST ast);
    void refineGraph(Module* module, InferenceContext& ctx);

    Evaluation inferChild(InferenceContext& ctx, Function* function, AST ast, u32 i) {
        auto eval = infer(ctx, function, ast.child(i));
        if (eval.isKnownValue() || eval.isNode())
            ast.setChild(i, eval.node(ast.module));
        return eval;
    }

    struct ChangePosition {
        AST ast;
        u32 child;

        inline void replaceWith(AST node) {
            ast.setChild(child, node);
        }
    };

    struct TypeEvaluation : public Evaluation {
        inline TypeEvaluation(const Evaluation& eval):
            Evaluation(eval) {}
    };

    inline TypeEvaluation toType(Module* module, Evaluation evaluation) {
        return evaluation.isKnownValue() ? fromType(naturalType(module, evaluation)) : evaluation;
    }

    void unify(Type srcType, Type destType, AST ast, InferenceContext& ctx) {
        auto result = srcType.unifyOnto(destType, ctx.constraints, Constraining);

        if (!result) {
            printTypeVariableState(srcType.types);
            type_error("Failed to unify ", srcType, " onto ", destType, " in ", ast);
        }
    }

    void unify(Evaluation src, Type destType, AST ast, InferenceContext& ctx) {
        if (src.isKnownValue())
            src = toType(ast.module, src);
        unify(src.type(ast.module), destType, ast, ctx);
    }

    void unify(Type srcType, AST ast, InferenceContext& ctx) {
        unify(srcType, ast.type(), ast, ctx);
    }

    void unify(Evaluation src, AST ast, InferenceContext& ctx) {
        if (src.isKnownValue())
            src = toType(ast.module, src);
        unify(src.type(ast.module), ast, ctx);
    }

    void unifyInPlaceSubstituting(Type srcType, Type destType, AST ast) {
        auto result = srcType.unifyOnto(destType, nullptr, InPlace | MustSubstitute);
        if (!result) {
            printTypeVariableState(srcType.types);
            type_error("Failed to in-place unify ", srcType, " onto ", destType, " in ", ast);
        }
    }

    void unifyInPlace(Type srcType, Type destType, AST ast) {
        auto result = srcType.unifyOnto(destType, nullptr, InPlace);
        if (!result) {
            printTypeVariableState(srcType.types);
            type_error("Failed to in-place unify ", srcType, " onto ", destType, " in ", ast);
        }
    }

    void unifyInPlace(Evaluation src, Type destType, AST ast) {
        if (src.isKnownValue())
            src = toType(ast.module, src);
        return unifyInPlace(src.type(ast.module), destType, ast);
    }

    void unifyInPlace(Type srcType, AST ast) {
        return unifyInPlace(srcType, ast.type(), ast);
    }

    void unifyInPlace(Evaluation src, AST ast) {
        if (src.isKnownValue())
            src = toType(ast.module, src);
        return unifyInPlace(src.type(ast.module), ast);
    }

    bool canUnify(Type srcType, Type destType, AST ast) {
        auto result = srcType.unifyOnto(destType, nullptr, Query);
        return result == UnifySuccess;
    }

    bool canUnify(Evaluation src, Type destType, AST ast) {
        if (src.isKnownValue())
            src = toType(ast.module, src);
        return canUnify(src.type(ast.module), destType, ast);
    }

    bool canUnify(Type srcType, AST ast) {
        return canUnify(srcType, ast.type(), ast);
    }

    bool canUnify(Evaluation src, AST ast) {
        if (src.isKnownValue())
            src = toType(ast.module, src);
        return canUnify(src.type(ast.module), ast);
    }

    Evaluation fromNodeType(AST ast) {
        return fromType(ast.typeIndex());
    }

    void concretifyNode(AST ast) {
        ast.setType(concreteType(ast.module, ast.type()));
    }

    bool isSingleFieldUnresolved(AST ast) {
        assert(ast.kind() == ASTKind::GetField || ast.kind() == ASTKind::SetField || ast.kind() == ASTKind::AddrField);
        return ast.child(1).kind() == ASTKind::Ident;
    }

    bool areMultipleFieldsUnresolved(AST ast) {
        assert(ast.kind() == ASTKind::GetFields || ast.kind() == ASTKind::SetFields || ast.kind() == ASTKind::AddrFields);
        return ast.child(1).kind() == ASTKind::Ident;
    }

    Type eliminateRange(Type type) {
        if UNLIKELY(type.isRange())
            return type.types->var(type.asRange().lowerBound(), type.asRange().upperBound());
        return type;
    }

    Type inferredType(InferenceContext& ctx, Function* function, AST ast) {
        Module* module = function ? function->module : ctx.constraints->module;
        TypeSystem& types = *module->types;

        switch (ast.kind()) {
            case ASTKind::Local: {
                assert(function);
                type_assert(ast.varInfo(function).kind != VariableKind::OverloadedFunction);
                Type type = types.get(ast.varInfo(function).type);
                if (type.isVar() && type.asVar().isEqual()) {
                    type = expand(type);
                    ast.varInfo(function).type = type.index;
                }
                return type;
            }
            case ASTKind::Global: {
                type_assert(ast.varInfo().kind != VariableKind::OverloadedFunction);
                Type type = types.get(ast.varInfo().type);
                if (type.isVar() && type.asVar().isEqual()) {
                    type = expand(type);
                    ast.varInfo().type = type.index;
                }
                return type;
            }
            case ASTKind::Typename:
            case ASTKind::GlobalTypename:
                return evaluateType(module, function, ast);
            case ASTKind::Int:
            case ASTKind::Unsigned:
            case ASTKind::Float:
            case ASTKind::Bool:
            case ASTKind::Char:
                return toType(module, infer(ctx, function, ast)).type(module);
            case ASTKind::String:
                return module->arrayType(module->i8Type(), (u32)module->str(ast.stringConst()).size());

            case ASTKind::ResolvedFunction: {
                return ast.resolvedFunction()->type();
            }

            default:
                assert(!ast.isLeaf());
                return ast.type();
        }
    }

    void addImplicitReturns(InferenceContext& ctx, Type returnType, AST parent, AST node, ChangePosition toChange) {
        Module* module = node.module;
        switch (node.kind()) {
            case ASTKind::IfElse:
                addImplicitReturns(ctx, returnType, node, node.child(1), { node, 1 });
                addImplicitReturns(ctx, returnType, node, node.child(2), { node, 2 });
                return;
            case ASTKind::Match:
                for (AST matchCase : node.children(1))
                    addImplicitReturns(ctx, returnType, matchCase, matchCase.child(1), { matchCase, 1 });
                // TODO: Exhaustivity checking. Probably should be done somewhere other than here?
                return;
            case ASTKind::Do: {
                if (node.arity() == 0)
                    return;
                AST last = node.child(node.arity() - 1);
                addImplicitReturns(ctx, returnType, node, last, { node, node.arity() - 1 });
                return;
            }
            case ASTKind::Return:
                return;
            default: {
                if (returnType == module->voidType()) {
                    // It could be expensive to reallocate a whole block to
                    // append a void return, so we add a `then` expression that
                    // can be easily substituted in place of the previous
                    // expression.
                    AST voidReturn = module->add(ASTKind::Return, node.isLeaf() ? parent.pos() : node.pos(), parent.scope(), module->voidType(), module->add(ASTKind::Missing));
                    AST result = module->add(ASTKind::Then, voidReturn.pos(), parent.scope(), module->invalidType(), node, voidReturn);
                    toChange.replaceWith(result);
                } else {
                    AST newReturn = module->add(ASTKind::Return, node.isLeaf() ? parent.pos() : node.pos(), parent.scope(), module->voidType(), node);
                    unify(inferredType(ctx, parent.function(), node), returnType, newReturn, ctx);
                    toChange.replaceWith(newReturn);
                }
            }
        }
    }

    Type evaluateType(Module* module, Function* function, AST ast) {
        if (ast.kind() == ASTKind::GlobalTypename)
            return module->types->get(ast.varInfo().type);
        if (ast.kind() == ASTKind::Typename)
            return module->types->get(ast.varInfo(function).type);
        return ast.type();
    }

    Type inferPattern(InferenceContext& ctx, Function* function, Type parentType, AST pattern) {
        // At the inference stage, all we really want to do for cases is
        // propagate the type info we know from the pattern (since this should
        // usually be statically known after resolution) into any pattern
        // variables so we can discover constraints on those variables. We do
        // some validation of the patterns too.
        //
        // Note: parentType is allowed to be InvalidType, in case we don't know
        // any target type from the parent pattern.

        Module* module = pattern.module;
        Type type;
        switch (pattern.kind()) {
            case ASTKind::Int:
                type = naturalType(module, makeSigned(module, pattern.intConst()));
                if (parentType)
                    unify(type, parentType, pattern, ctx);
                return type;
            case ASTKind::Unsigned:
                type = naturalType(module, makeUnsigned(module, pattern.uintConst()));
                if (parentType)
                    unify(type, parentType, pattern, ctx);
                return type;
            case ASTKind::Float:
                type = naturalType(module, makeFloat(module, pattern.floatConst()));
                if (parentType)
                    unify(type, parentType, pattern, ctx);
                return type;
            case ASTKind::Bool:
                if (parentType)
                    unify(module->boolType(), parentType, pattern, ctx);
                return module->boolType();
            case ASTKind::Char:
                if (parentType)
                    unify(module->charType(), parentType, pattern, ctx);
                return module->charType();
            case ASTKind::String: {
                Type type = module->arrayType(module->i8Type(), (u32)module->str(pattern.stringConst()).size());
                if (parentType)
                    unify(type, parentType, pattern, ctx);
                return type;
            }

            case ASTKind::Splat:
                // Splats should contain a variable declaration as their
                // only child.
                type_assert(pattern.arity() == 1);
                type_assert(pattern.child(0).kind() == ASTKind::VarDecl);
                inferPattern(ctx, function, parentType, pattern.child(0));
                return module->bottomType(); // Anything is acceptable.

            case ASTKind::VarDecl:
                if (parentType)
                    unify(parentType, pattern.type(), pattern, ctx);
                ctx.ensureResolved(pattern.type());
                return pattern.type();

            case ASTKind::Tuple: {
                if (parentType)
                    type_assert(parentType.is<TypeKind::Tuple>());
                vec<TypeIndex> overallTypes;
                for (u32 i = 0; i < pattern.arity(); i ++) {
                    if (pattern.child(i).kind() == ASTKind::Splat) {
                        type_assert(i == pattern.arity() - 1);
                        Type childType = module->invalidType();
                        if (parentType) {
                            auto tupleType = parentType.as<TypeKind::Tuple>();
                            vec<TypeIndex> remainingTypes;
                            for (u32 j = i; j < tupleType.count(); j ++)
                                remainingTypes.push(tupleType.fieldTypeIndex(j));
                            childType = module->tupleType(remainingTypes);
                            pattern.child(i).setType(childType);
                            overallTypes.push(childType.index);
                        } else
                            overallTypes.push(module->varType().index);
                        inferPattern(ctx, function, childType, pattern.child(i));
                        break;
                    }
                    overallTypes.push(inferPattern(ctx, function, parentType ? parentType.as<TypeKind::Tuple>().fieldType(i) : module->invalidType(), pattern.child(i)).index);
                }
                pattern.setType(module->tupleType(overallTypes));
                return pattern.type();
            }

            case ASTKind::List:
                pattern.setType(module->sliceType(module->varType()));
                ctx.ensureResolved(pattern.type().as<TypeKind::Slice>().elementType());
                for (u32 i = 0; i < pattern.arity(); i ++) {
                    if (pattern.child(i).kind() == ASTKind::Splat) {
                        type_assert(i == pattern.arity() - 1
                            || (i < pattern.arity() - 1 && pattern.child(i + 1).kind() != ASTKind::Splat));
                        inferPattern(ctx, function, pattern.type(), pattern.child(i));
                    } else
                        inferPattern(ctx, function, pattern.type().as<TypeKind::Slice>().elementType(), pattern.child(i));
                }
                return pattern.type();

            case ASTKind::Construct:
                switch (pattern.type().kind()) {
                    case TypeKind::Named:
                        if (pattern.arity() == 0)
                            type_assert(pattern.type().as<TypeKind::Named>().innerType() == Void);
                        else {
                            type_assert(pattern.arity() == 1);
                            inferPattern(ctx, function, pattern.type().as<TypeKind::Named>().innerType(), pattern.child(0));
                        }
                        return pattern.type();
                    case TypeKind::Struct: {
                        auto structType = pattern.type().as<TypeKind::Struct>();
                        for (u32 i = 0; i < pattern.arity(); i ++) {
                            if (pattern.child(i).kind() == ASTKind::Splat) {
                                type_assert(i == pattern.arity() - 1);
                                vec<TypeIndex> remainingTypes;
                                for (u32 j = i; j < structType.count(); j ++)
                                    remainingTypes.push(structType.fieldTypeIndex(j));
                                inferPattern(ctx, function, module->tupleType(remainingTypes), pattern.child(i));
                                break;
                            }

                            inferPattern(ctx, function, structType.fieldType(i), pattern.child(i));
                        }
                        return pattern.type();
                    }
                    default:
                        type_error("Only named and struct types are allowed in constructor patterns.");
                }

            case ASTKind::Typename:
            case ASTKind::GlobalTypename:
            case ASTKind::TypeField:
            case ASTKind::PtrType:
            case ASTKind::SliceType:
            case ASTKind::ArrayType:
            case ASTKind::TupleType:
            case ASTKind::FunType:
                type = evaluateType(module, function, pattern);
                if (parentType)
                    unify(parentType, type, pattern, ctx);
                return type;

            default:
                unreachable("Invalid pattern node ", pattern);
        }
    }

    bool isOverloadedFunction(Function* function, AST ast) {
        if (ast.kind() == ASTKind::Local)
            return function->locals[ast.variable()].kind == VariableKind::OverloadedFunction;
        else if (ast.kind() == ASTKind::Global)
            return ast.module->globals[ast.variable()].kind == VariableKind::OverloadedFunction;
        return false;
    }

    Evaluation infer(InferenceContext& ctx, Function* function, AST ast) {
        Module* module = ast.module;
        auto& types = *module->types;

        // Clobberable variables to use in cases. Don't assume they have any
        // specific value unless you set them in a case.
        Evaluation result, value, lhs, rhs;
        Type resultType, valueType, varType, lhsType, rhsType;

        switch (ast.kind()) {
            // Terminals

            case ASTKind::Local:
                assert(function);
                type_assert(!isOverloadedFunction(function, ast));
                return fromType(types.get(function->locals[ast.variable()].type));
            case ASTKind::Global:
                type_assert(!isOverloadedFunction(function, ast));
                return fromType(types.get(module->globals[ast.variable()].type));
            case ASTKind::Int:
                return makeSigned(module, ast.intConst());
            case ASTKind::Unsigned:
                return makeUnsigned(module, ast.uintConst());
            case ASTKind::Float:
                return makeFloat(module, ast.floatConst());
            case ASTKind::Bool:
                return makeBool(module, ast.boolConst());
            case ASTKind::Char:
                return makeChar(module, ast.charConst());

            case ASTKind::ResolvedFunction:
                return fromType(ast.resolvedFunction()->type());

            case ASTKind::Typename:
            case ASTKind::GlobalTypename:
            case ASTKind::TypeField: {
                Type type = evaluateType(module, function, ast);
                if (type.is<TypeKind::Named>() && type.as<TypeKind::Named>().innerType() == Void) {
                    // It's an atom, so it's allowed to be used in a value position.
                    return fromType(type);
                }
                unreachable("Type expressions are not allowed in value positions unless they are atoms.");
            }

            case ASTKind::Paren: {
                value = inferChild(ctx, function, ast, 0);
                if (value.isKnownValue())
                    return value;

                ast.setType(toType(module, value).type(module));
                return fromNodeType(ast);
            }

            // Arithmetic expressions

            case ASTKind::Plus:
            case ASTKind::Minus: {
                value = inferChild(ctx, function, ast, 0);

                if (value.isKnownValue())
                    result = fold(module, function, ast, value);
                if (result.isKnownValue())
                    return result;

                ast.setType(module->varType(module->bottomNumberType(), module->topNumberType()));
                ctx.ensureResolved(ast.type());
                unify(toType(module, value), ast, ctx);
                return fromNodeType(ast);
            }

            case ASTKind::Add:
            case ASTKind::Sub:
            case ASTKind::Mul:
            case ASTKind::Div:
            case ASTKind::Rem:
            case ASTKind::Exp: {
                lhs = inferChild(ctx, function, ast, 0);
                rhs = inferChild(ctx, function, ast, 1);

                if (lhs.isKnownValue() && rhs.isKnownValue())
                    result = fold(module, function, ast, lhs, rhs);
                if (result.isKnownValue())
                    return result;

                lhs = toType(module, lhs), rhs = toType(module, rhs);
                ast.setType(module->varType(module->bottomNumberType(), module->topNumberType()));
                ctx.ensureResolved(ast.type());
                unify(lhs, ast, ctx);
                unify(rhs, ast, ctx);
                return fromNodeType(ast);
            }

            // Bitwise expressions

            case ASTKind::BitNot: {
                value = inferChild(ctx, function, ast, 0);

                if (value.isKnownValue())
                    result = fold(module, function, ast, value);
                if (result.isKnownValue())
                    return result;

                ast.setType(module->varType(module->bottomNumberType(), module->topIntegerType()));
                ctx.ensureResolved(ast.type());
                unify(toType(module, value), ast, ctx);
                return fromNodeType(ast);
            }

            case ASTKind::BitAnd:
            case ASTKind::BitOr:
            case ASTKind::BitXor:
            case ASTKind::BitShl:
            case ASTKind::BitShr:
            case ASTKind::BitRol:
            case ASTKind::BitRor: {
                lhs = inferChild(ctx, function, ast, 0);
                rhs = inferChild(ctx, function, ast, 1);

                if (lhs.isKnownValue() && rhs.isKnownValue())
                    result = fold(module, function, ast, lhs, rhs);
                if (result.isKnownValue())
                    return result;

                lhs = toType(module, lhs), rhs = toType(module, rhs);
                ast.setType(module->varType(module->bottomNumberType(), module->topIntegerType()));
                ctx.ensureResolved(ast.type());
                unify(lhs, ast, ctx);
                unify(rhs, ast, ctx);
                return fromNodeType(ast);
            }

            // Comparison expressions

            case ASTKind::Less:
            case ASTKind::LessEq:
            case ASTKind::Greater:
            case ASTKind::GreaterEq:
            case ASTKind::Equal:
            case ASTKind::NotEqual: {
                lhs = inferChild(ctx, function, ast, 0);
                rhs = inferChild(ctx, function, ast, 1);
                if (lhs.isKnownValue() && rhs.isKnownValue())
                    result = fold(module, function, ast, lhs, rhs);
                if (result.isKnownValue())
                    return result;

                ast.setType(module->boolType());
                lhs = toType(module, lhs), rhs = toType(module, rhs);
                lhsType = lhs.type(module), rhsType = rhs.type(module);
                unify(lhsType, rhsType, ast, ctx);
                unify(rhsType, lhsType, ast, ctx);

                // Needed to check that the types we are comparing are allowed
                // to be compared. This sort of set-membership check is hard to
                // encode as type constraints and probably isn't worth worrying
                // about since we return a bool no matter what.
                ctx.checkLater(ast);

                return fromNodeType(ast);
            }

            // Logical expressions

            case ASTKind::Not: {
                value = inferChild(ctx, function, ast, 0);
                if (value.isKnownValue())
                    result = fold(module, function, ast, value);
                if (result.isKnownValue())
                    return result;

                ast.setType(module->boolType());
                unify(toType(module, value), ast, ctx);
                return fromNodeType(ast);
            }
            case ASTKind::And:
            case ASTKind::Or: {
                lhs = inferChild(ctx, function, ast, 0);
                rhs = inferChild(ctx, function, ast, 1);
                if (lhs.isKnownValue() && rhs.isKnownValue())
                    result = fold(module, function, ast, lhs, rhs);
                if (result.isKnownValue())
                    return result;

                lhs = toType(module, lhs), rhs = toType(module, rhs);
                ast.setType(module->boolType());
                unify(lhs, ast, ctx);
                unify(rhs, ast, ctx);
                return fromNodeType(ast);
            }

            // Arithmetic assignment expressions

            case ASTKind::AddEq:
            case ASTKind::SubEq:
            case ASTKind::MulEq:
            case ASTKind::DivEq:
            case ASTKind::RemEq:
            case ASTKind::ExpEq: {
                lhs = inferChild(ctx, function, ast, 0);
                rhs = inferChild(ctx, function, ast, 1);

                // We can't fold these operations either, so we skip to types.
                lhs = toType(module, lhs), rhs = toType(module, rhs);
                varType = module->varType(module->bottomNumberType(), module->topNumberType());
                ctx.ensureResolved(varType);
                // At this stage, we assume the lhs is a pointer to the place
                // this node reads and writes. And because lhs is both read and
                // written, we need to unify it onto our node, and vice versa,
                // resulting in an equality bound.
                unify(lhs.type(module), module->ptrType(varType), ast, ctx);
                unify(module->ptrType(varType), lhs.type(module), ast, ctx);
                unify(rhs, varType, ast, ctx);
                ast.setType(module->voidType());
                return fromType(module->voidType()); // These are still assignments, so they don't currently return a value.
            }

            // Bitwise assignment expressions

            case ASTKind::BitAndEq:
            case ASTKind::BitOrEq:
            case ASTKind::BitXorEq:
            case ASTKind::BitShlEq:
            case ASTKind::BitShrEq:
            case ASTKind::BitRolEq:
            case ASTKind::BitRorEq: {
                lhs = inferChild(ctx, function, ast, 0);
                rhs = inferChild(ctx, function, ast, 1);

                // We can't fold these operations either, so we skip to types.
                lhs = toType(module, lhs), rhs = toType(module, rhs);
                varType = module->varType(module->bottomNumberType(), module->topIntegerType());
                ctx.ensureResolved(varType);

                // At this stage, we assume the lhs is a pointer to the place
                // this node reads and writes. And because lhs is both read and
                // written, we need to unify it onto our node, and vice versa,
                // resulting in an equality bound.
                unify(lhs.type(module), module->ptrType(varType), ast, ctx);
                unify(module->ptrType(varType), lhs.type(module), ast, ctx);
                unify(rhs, varType, ast, ctx);
                ast.setType(module->voidType());
                return fromType(module->voidType()); // These are still assignments, so they don't currently return a value.
            }

            // Pre/post increment/decrement

            case ASTKind::PreIncr:
            case ASTKind::PreDecr:
            case ASTKind::PostIncr:
            case ASTKind::PostDecr: {
                value = inferChild(ctx, function, ast, 0);
                value = toType(module, value);

                ast.setType(module->varType(module->bottomNumberType(), module->topNumberType()));
                ctx.ensureResolved(ast.type());

                // At this stage, we assume the value is a pointer to the place
                // this node reads and writes. And because value is both read
                // and written, we need to unify it onto our node, and vice
                // versa, resulting in an equality bound.
                unify(value, module->ptrType(ast.type()), ast, ctx);
                unify(module->ptrType(ast.type()), value.type(module), ast, ctx);
                return fromNodeType(ast); // Unlike compound assignment, increment/decrements return a value.
            }

            // Operations on indexed containers

            case ASTKind::String:
                return fromType(module->arrayType(module->i8Type(), (u32)module->str(ast.stringConst()).size()));

            case ASTKind::List: {
                assert(ast.arity() > 0);
                varType = module->varType();
                ctx.ensureResolved(varType);
                ast.setType(module->arrayType(varType, u32(ast.arity())));
                for (u32 i : indices(ast))
                    unify(inferChild(ctx, function, ast, i), varType, ast, ctx);
                return fromNodeType(ast);
            }

            case ASTKind::GetIndex:
            case ASTKind::AddrIndex:
            case ASTKind::SetIndex: {
                lhs = inferChild(ctx, function, ast, 0);
                lhs = toType(module, lhs);
                lhsType = lhs.type(module);

                rhs = inferChild(ctx, function, ast, 1);
                if (ast.kind() == ASTKind::SetIndex)
                    value = inferChild(ctx, function, ast, 2);

                if (rhs.isKnownValue())
                    assert(rhs.isKnownSigned() || rhs.isKnownUnsigned());
                else
                    unify(rhs, module->topIntegerType(), ast, ctx);

                varType = module->varType(ast.node);
                ctx.ensureResolved(varType);
                switch (ast.kind()) {
                    case ASTKind::GetIndex:
                        ast.setType(varType);
                        break;
                    case ASTKind::SetIndex:
                        ast.setType(module->voidType());
                        break;
                    case ASTKind::AddrIndex:
                        ast.setType(module->ptrType(varType));
                        break;
                    default:
                        unreachable("Expected indexed access.");
                }

                if (ast.kind() == ASTKind::SetIndex)
                    unify(value, varType, ast, ctx);
                ctx.constraints->constrainOrder(lhsType, varType);

                return ast.kind() == ASTKind::SetIndex ? fromType(module->voidType()) : fromNodeType(ast);
            }

            case ASTKind::GetSlice:
            case ASTKind::SetSlice: {
                lhs = inferChild(ctx, function, ast, 0);
                if (!ast.child(1).missing()) {
                    auto low = inferChild(ctx, function, ast, 1);
                    if (low.isKnownValue())
                        assert(low.isKnownInteger());
                    else
                        unify(low, module->topIntegerType(), ast, ctx);
                }
                if (!ast.child(2).missing()) {
                    auto high = inferChild(ctx, function, ast, 2);
                    if (high.isKnownValue())
                        assert(high.isKnownInteger());
                    else
                        unify(high, module->topIntegerType(), ast, ctx);
                }

                if (ast.kind() == ASTKind::SetSlice)
                    rhs = inferChild(ctx, function, ast, 3);

                varType = module->varType();
                ctx.ensureResolved(varType);
                // Because we're potentially writing the result, we want to
                // allow uninit bases for SetSlice but not for GetSlice.
                auto sliceType = module->sliceType(ast.kind() == ASTKind::SetSlice ? Uninit : Unowned, varType);
                unify(toType(module, lhs), sliceType, ast, ctx);

                if (ast.kind() == ASTKind::SetSlice) {
                    // Even though we unified the base onto an uninit slice,
                    // we still want an initialized slice for the source.
                    unify(rhs, module->sliceType(varType), ast, ctx);
                    ast.setType(module->voidType());
                } else
                    ast.setType(sliceType);

                return fromNodeType(ast);
            }

            // Operations on types with fields

            case ASTKind::Tuple: {
                vec<Type> fieldTypes;
                for (u32 i : indices(ast)) {
                    value = inferChild(ctx, function, ast, i);
                    valueType = toType(module, value).type(module);
                    fieldTypes.push(module->varType());
                    ctx.ensureResolved(fieldTypes.back());
                    unify(valueType, fieldTypes.back(), ast, ctx);
                }
                ast.setType(module->tupleType(fieldTypes));
                return fromNodeType(ast);
            }

            case ASTKind::GetField:
            case ASTKind::AddrField:
            case ASTKind::SetField: {
                lhs = inferChild(ctx, function, ast, 0);
                lhs = toType(module, lhs);
                lhsType = lhs.type(module);

                if (ast.kind() == ASTKind::SetField) {
                    rhs = inferChild(ctx, function, ast, 2);
                    rhs = toType(module, rhs);
                    rhsType = rhs.type(module);
                }

                varType = module->varType(ast.node);
                ctx.ensureResolved(varType);
                if (ast.kind() == ASTKind::SetField)
                    ast.setType(module->voidType());
                else if (ast.kind() == ASTKind::AddrField)
                    ast.setType(module->ptrType(varType));
                else if (ast.kind() == ASTKind::GetField)
                    ast.setType(varType);

                ctx.constraints->constrainOrder(lhsType, varType);
                return fromNodeType(ast);
            }

            case ASTKind::GetFields: {
                assert(ast.arity() > 1);
                lhs = inferChild(ctx, function, ast, 0);
                lhs = toType(module, lhs);
                lhsType = lhs.type(module);

                ast.setType(module->varType(ast.node));
                ctx.ensureResolved(ast.type());
                ctx.constraints->constrainOrder(lhsType, ast.type());
                return fromNodeType(ast);
            }

            case ASTKind::Length: {
                value = inferChild(ctx, function, ast, 0);
                ast.setType(module->varType(module->bottomNumberType(), module->topIntegerType()));
                ctx.ensureResolved(ast.type());
                return fromNodeType(ast);
            }

            case ASTKind::SizeOf: {
                ast.setType(module->varType(module->bottomNumberType(), module->topIntegerType()));
                ctx.ensureResolved(ast.type());
                return fromNodeType(ast);
            }

            // Operations involving pointers

            case ASTKind::AddressOf: {
                value = inferChild(ctx, function, ast, 0);
                valueType = toType(module, value).type(module);
                ast.setType(module->ptrType(eliminateRange(valueType)));
                return fromNodeType(ast);
            }

            case ASTKind::Deref: {
                value = inferChild(ctx, function, ast, 0);
                valueType = toType(module, value).type(module);
                ast.setType(module->varType());
                ctx.ensureResolved(ast.type());
                unify(valueType, module->ptrType(ast.type()), ast, ctx);
                return fromNodeType(ast);
            }

            case ASTKind::New: {
                if (ast.child(0).missing()) {
                    // Uninitialized allocation.
                    assert(ast.type().is<TypeKind::Pointer>());
                    assert(ast.type().as<TypeKind::Pointer>().isOwn());
                    assert(ast.type().as<TypeKind::Pointer>().isUninit());
                    return fromNodeType(ast);
                }
                value = inferChild(ctx, function, ast, 0);
                valueType = toType(module, value).type(module);
                if (ast.child(0).kind() != ASTKind::Construct) {
                    // If we were allocating the result of a constructor call,
                    // then we should already be using its type from the
                    // resolution phase.
                    ast.setType(module->ptrType(Own, varType = module->varType()));
                    ctx.ensureResolved(ast.type());
                }
                unify(valueType, ast.type().as<TypeKind::Pointer>().elementType(), ast, ctx);
                return fromNodeType(ast);
            }

            case ASTKind::NewArray: {
                // Should have been set by resolution pass.
                assert(ast.type().is<TypeKind::Slice>());
                assert(ast.type().as<TypeKind::Slice>().isOwn());
                assert(ast.type().as<TypeKind::Slice>().isUninit());
                if (ast.type().as<TypeKind::Slice>().elementType().isVar())
                    ctx.ensureResolved(ast.type());

                value = inferChild(ctx, function, ast, 0);
                valueType = toType(module, value).type(module);
                unify(valueType, module->topIntegerType(), ast, ctx);
                return fromNodeType(ast);
            }

            // Operations involving functions

            case ASTKind::Call:
            case ASTKind::CallMethod: {
                ast.setType(module->varType(ast.node));
                ctx.ensureResolved(ast.type());
                if (!isOverloadedFunction(function, ast.child(0))) {
                    auto func = inferChild(ctx, function, ast, 0);
                    auto funcType = toType(module, func).type(module);
                    ctx.constraints->constrainOrder(funcType, ast.type());
                }
                for (u32 i = 1; i < ast.arity(); i ++) {
                    auto arg = inferChild(ctx, function, ast, i);
                    ctx.constraints->constrainOrder(toType(module, arg).type(module), ast.type());
                }
                return fromNodeType(ast);
            }

            // Casts and constructors

            case ASTKind::Construct: {
                auto type = ast.type();

                // We should already have a concrete type from name resolution.
                // Note: this will break if we ever add some kind of decltype()
                // feature and allow it to return non-concrete types. Probably
                // other stuff will break if this happens too.
                assert(type.isConcrete());

                // Construction nodes are split between two main cases.
                //
                // First, for user-defined struct/named types, values can be
                // constructed based on their internal structure and fields. We
                // typecheck these as if passing parameters to a call.
                //
                // Otherwise, for other primitive types, we have a mess of
                // special cases to check for, for every permitted explicit
                // cast between types in the language.
                //
                // Since we know the type up front, in most cases we can learn
                // nontrivial type constraints by unification during this pass.
                // But we also need to revisit this node later to ensure that
                // we did end up with valid arguments when all's said and done
                // in some particular cases.

                if (type.is<TypeKind::Numeric>() || type.is<TypeKind::Char>()) {
                    // We check later here because we can't unify against
                    // Char | Number.
                    type_assert(ast.arity() == 1);
                    inferChild(ctx, function, ast, 0);
                    ctx.checkLater(ast);
                    return fromNodeType(ast);
                }

                if (type.is<TypeKind::Pointer>() || type.is<TypeKind::Slice>()) {
                    // We check later here because we don't want to force our
                    // input into any one particular kind of pointer or
                    // slice type too early.
                    type_assert(ast.arity() == 1);
                    inferChild(ctx, function, ast, 0);
                    ctx.checkLater(ast);
                    return fromNodeType(ast);
                }

                if (type.is<TypeKind::Named>()) {
                    // Named types can be constructed with a single parameter.

                    if (ast.arity() == 0) {
                        type_assert(type.as<TypeKind::Named>().innerType() == Void);
                        return fromNodeType(ast);
                    }

                    type_assert(ast.arity() == 1);
                    value = inferChild(ctx, function, ast, 0);
                    unify(value, type.as<TypeKind::Named>().innerType(), ast, ctx);
                    return fromNodeType(ast);
                }

                if (type.is<TypeKind::Tuple>()) {
                    // Tuple types can be constructed with exactly as many
                    // parameters as they have members.

                    type_assert(ast.arity() == type.as<TypeKind::Tuple>().count());
                    for (u32 i = 0; i < ast.arity(); i ++) {
                        value = inferChild(ctx, function, ast, i);
                        unify(value, type.as<TypeKind::Tuple>().fieldType(i), ast, ctx);
                    }
                    return fromNodeType(ast);
                }

                if (type.is<TypeKind::Struct>()) {
                    // Struct types can be constructed with exactly as many
                    // parameters as they have members.

                    type_assert(ast.arity() == type.as<TypeKind::Struct>().count());
                    for (u32 i = 0; i < ast.arity(); i ++) {
                        value = inferChild(ctx, function, ast, i);
                        unify(value, type.as<TypeKind::Struct>().fieldType(i), ast, ctx);
                    }
                    return fromNodeType(ast);
                }

                type_error("Invalid constructor call '", ast, "' for type '", type, "'.");
            }

            // Assignment

            case ASTKind::Store: {
                lhs = inferChild(ctx, function, ast, 0);
                assert(!lhs.isKnownValue());
                rhs = inferChild(ctx, function, ast, 1);
                lhsType = eliminateRange(toType(module, lhs).type(module));
                rhsType = eliminateRange(toType(module, rhs).type(module));
                unify(module->ptrType(Own, rhsType), lhsType, ast, ctx);
                ast.setType(module->voidType());
                return fromNodeType(ast);
            }

            case ASTKind::Assign: {
                lhs = inferChild(ctx, function, ast, 0);
                assert(!lhs.isKnownValue());
                rhs = inferChild(ctx, function, ast, 1);
                unify(rhs, toType(module, lhs).type(module), ast, ctx);
                ast.setType(module->voidType());
                return fromType(module->voidType());
            }

            // Declarations

            case ASTKind::VarDecl: {
                ctx.ensureResolved(ast.type()); // Type variable from resolution pass.

                if (ast.child(2).kind() != ASTKind::Missing)
                    unify(value = inferChild(ctx, function, ast, 2), ast, ctx);

                // Resolve the pattern if there is a nontrivial one.
                if (ast.child(1).kind() != ASTKind::Missing
                    && ast.child(1).kind() != ASTKind::Local
                    && ast.child(1).kind() != ASTKind::Global) {
                    assert(!ast.child(2).missing());
                    varType = module->varType(ast.node);
                    unify(value, varType, ast, ctx);
                    inferPattern(ctx, function, module->invalidType(), ast.child(1));
                    ctx.constraints->constrainOrder(ast.type(), varType);
                }
                return fromType(module->voidType());
            }

            case ASTKind::AliasDecl:
            case ASTKind::NamedDecl:
            case ASTKind::NamedCaseDecl:
            case ASTKind::StructDecl:
            case ASTKind::StructCaseDecl:
            case ASTKind::UnionDecl:
            case ASTKind::UnionCaseDecl:
            case ASTKind::GenericFunDecl: {
                // Shouldn't have anything to do.
                return fromType(module->voidType());
            }

            case ASTKind::FunDecl: {
                // Constraints constraints(module, module->types, ctx.constraints->depth + 1);
                // InferenceContext funcCtx;
                // funcCtx.constraints = &constraints;
                // funcCtx.lateChecks = ctx.lateChecks;
                // funcCtx.lateResolves = ctx.lateResolves;
                InferenceContext& funcCtx = ctx;

                auto name = ast.child(1).variable();
                for (AST param : ast.child(2)) {
                    if (param.kind() != ASTKind::VarDecl)
                        continue;
                    if (!param.child(2).missing()) {
                        auto paramValue = inferChild(funcCtx, ast.function(), param, 2);
                        unify(paramValue, param.type(), ast, ctx);
                    }
                }
                Type funType = ast.type();
                if (!funType.isConcrete())
                    funcCtx.ensureResolved(ast.type());
                assert(funType.is<TypeKind::Function>());
                Type returnType = funType.as<TypeKind::Function>().returnType();
                if (!ast.child(4).missing()) { // If we're a stub function, we don't need to do anything.
                    inferChild(funcCtx, ast.function(), ast, 4);
                    addImplicitReturns(funcCtx, returnType, ast, ast.child(4), { ast, 4 });
                }
                // if UNLIKELY(config::printTypeConstraints)
                //     printTypeConstraints(module->types, &constraints);

                // refineGraph(module, funcCtx);
                // for (NodeIndex node : *funcCtx.lateChecks) {
                //     AST ast = module->node(node);
                //     assert(!ast.isLeaf());
                //     check(funcCtx, ast.function(), ast);
                // }
                ctx.checkLater(ast);
                return fromType(module->voidType());
            }

            // Control statements

            case ASTKind::Return: {
                ast.setType(module->bottomType());
                Type funType = function->type();
                assert(funType.is<TypeKind::Function>());
                valueType = ast.child(0).missing() ? module->voidType() : toType(module, inferChild(ctx, function, ast, 0)).type(module);
                unify(valueType, funType.as<TypeKind::Function>().returnType(), ast, ctx);
                return fromType(module->bottomType());
            }

            case ASTKind::While: {
                lhs = inferChild(ctx, function, ast, 0);
                unify(lhs, module->boolType(), ast, ctx);
                inferChild(ctx, function, ast, 1);
                ast.setType(module->voidType());
                return fromNodeType(ast);
            }

            case ASTKind::If: {
                lhs = inferChild(ctx, function, ast, 0);
                unify(lhs, module->boolType(), ast, ctx);
                inferChild(ctx, function, ast, 1);
                ast.setType(module->voidType());
                return fromNodeType(ast);
            }

            case ASTKind::IfElse: {
                auto cond = inferChild(ctx, function, ast, 0);
                unify(cond, module->boolType(), ast, ctx);
                inferChild(ctx, function, ast, 1);
                inferChild(ctx, function, ast, 2);
                ast.setType(module->voidType());
                return fromNodeType(ast);
            }

            case ASTKind::Ternary: {
                auto cond = inferChild(ctx, function, ast, 0);
                unify(cond, module->boolType(), ast, ctx);
                lhs = inferChild(ctx, function, ast, 1);
                rhs = inferChild(ctx, function, ast, 2);

                ast.setType(module->varType());
                ctx.ensureResolved(ast.type());
                unify(lhs, ast, ctx);
                unify(rhs, ast, ctx);
                return fromNodeType(ast);
            }

            case ASTKind::Then: {
                inferChild(ctx, function, ast, 0);
                inferChild(ctx, function, ast, 1);
                ast.setType(module->voidType());
                return fromNodeType(ast);
            }

            case ASTKind::Break:
            case ASTKind::Continue:
                ast.setType(module->voidType());
                return fromNodeType(ast);

            case ASTKind::Match:
                value = inferChild(ctx, function, ast, 0);
                valueType = toType(module, value).type(module);
                varType = module->varType(ast.node);
                unify(value, varType, ast, ctx);
                for (u32 i = 1; i < ast.arity(); i ++) {
                    // We pass invalid type because we don't know, without
                    // inspecting the pattern, what the target type is. We bridge
                    // the gap between the input expression and the pattern during
                    // later refinement.
                    AST matchCase = ast.child(i);
                    if (!matchCase.child(0).missing())
                        inferPattern(ctx, function, module->invalidType(), matchCase.child(0));
                    inferChild(ctx, function, matchCase, 1);
                    matchCase.setType(module->voidType());
                }
                ctx.constraints->constrainOrder(valueType, varType);
                ctx.ensureResolved(varType);
                ast.setType(module->voidType());
                return fromNodeType(ast);

            case ASTKind::Case:
                unreachable("Should have been inferred by match.");

            case ASTKind::Is:
                value = inferChild(ctx, function, ast, 0);
                valueType = toType(module, value).type(module);
                varType = module->varType(ast.node);
                unify(value, varType, ast, ctx);
                inferPattern(ctx, function, module->invalidType(), ast.child(1));
                ctx.constraints->constrainOrder(valueType, varType);
                ctx.ensureResolved(varType);
                ast.setType(module->boolType());
                return fromNodeType(ast);

            case ASTKind::Do:
            case ASTKind::TopLevel: {
                result = fromType(module->voidType());
                for (auto i : indices(ast))
                    result = inferChild(ctx, function, ast, i);
                ast.setType(toType(module, result).type(module));
                if (ast.kind() == ASTKind::TopLevel)
                    ctx.checkLater(ast);
                return fromNodeType(ast);
            }

            default:
                unreachable("Unimplemented type inference for node '", ast, "'.");
        }
    }

    maybe<AST> resolveSingleField(InferenceContext& ctx, Type baseType, AST ast) {
        // Returns none if the fields couldn't yet be resolved, and the
        // resulting AST if the fields could be resolved, including possible
        // swizzling.

        Module* module = ast.module;

        assert(ast.kind() == ASTKind::GetField || ast.kind() == ASTKind::SetField || ast.kind() == ASTKind::AddrField);
        assert(ast.child(1).kind() == ASTKind::Ident);

        Symbol field = ast.child(1).symbol();
        baseType = concreteType(module, baseType);

        if (baseType.is<TypeKind::Struct>()) {
            auto structType = baseType.as<TypeKind::Struct>();
            vec<pair<rune, u32>> swizzleCandidates;
            for (u32 i = 0; i < structType.count(); i ++) {
                if (field == structType.fieldName(i)) {
                    ast.setChild(1, module->add(ASTKind::Field, FieldId(i)));
                    if (ast.kind() != ASTKind::SetField)
                        unify(structType.fieldType(i), ast, ctx);
                    return some<AST>(ast);
                }
                auto name = module->str(structType.fieldName(i));
                if (name.size() == 1 || (name.size() < 4 && utf8_length(name.data(), name.size()) == 1)) {
                    swizzleCandidates.push({ 0, i });
                    if (name.size() == 1)
                        swizzleCandidates.back().first = name[0];
                    else
                        utf8_decode(name.data(), name.size(), &swizzleCandidates.back().first, 1);
                }
            }

            // Try swizzling if there were single-letter fields.
            if (swizzleCandidates.size()) {
                auto name = ast.module->str(field);
                vec<FieldId> indices;
                bool swizzleFailed = false;
                const i8* ptr = name.data();
                while (ptr != name.end()) {
                    rune r;
                    ptr = utf8_decode_forward(ptr, &r);
                    u32 sizeBefore = indices.size();
                    for (auto p : swizzleCandidates) if (p.first == r) {
                        indices.push(FieldId(p.second));
                        break;
                    }
                    if (indices.size() == sizeBefore) {
                        // No match found, swizzle failed.
                        swizzleFailed = true;
                        break;
                    }
                }
                assert(!swizzleFailed); // If swizzling failed, this is just a bad access.

                vec<Type> fieldTypes;
                for (FieldId i : indices)
                    fieldTypes.push(structType.fieldType(i.id));
                Type result = module->tupleType(fieldTypes);
                AST newAST = module->add(ASTKind::GetFields, ast.pos(), ast.scope(), result, ast.child(0), indices);
                return some<AST>(newAST);
            }

            type_error("Undefined field name '", module->str(field), "'");
        }

        return none<AST>();
    }

    bool resolveMultipleFields(InferenceContext& ctx, Type baseType, AST ast) {
        // Returns whether the fields were resolved.

        Module* module = ast.module;

        assert(ast.kind() == ASTKind::GetFields || ast.kind() == ASTKind::SetFields || ast.kind() == ASTKind::AddrFields);
        assert(ast.arity() >= (ast.kind() == ASTKind::SetFields ? 3 : 2));

        baseType = concreteType(module, baseType);

        if (baseType.is<TypeKind::Struct>()) {
            auto structType = baseType.as<TypeKind::Struct>();
            for (u32 i = 1; i < (ast.kind() == ASTKind::SetFields ? ast.arity() - 1 : ast.arity()); i ++) {
                assert(ast.child(i).kind() == ASTKind::Ident);
                Symbol field = ast.child(i).symbol();
                for (u32 j = 0; j < structType.count(); j ++) {
                    if (field == structType.fieldName(j)) {
                        ast.setChild(1, module->add(ASTKind::Field, FieldId(j)));
                        unify(structType.fieldType(j), ast, ctx);
                        continue;
                    }
                }

                type_error("Undefined field name '", module->str(field), "'");
            }
            return true;
        }
        return false;
    }

    bool refinePattern(InferenceContext& ctx, Function* function, Type input, AST pattern) {
        auto unifyOrDereference = [&](Type pattern, Type input, AST ast) {
            // This is basically just unifyInPlace, but it'll try automatically
            // dereferencing the input if it would otherwise fail.
            if (canUnify(pattern, input, ast))
                unifyInPlace(pattern, input, ast);
            else if (expand(input).is<TypeKind::Pointer>())
                unifyInPlace(pattern, expand(input).as<TypeKind::Pointer>().elementType(), ast);
        };
        Module* module = pattern.module;
        switch (pattern.kind()) {
            case ASTKind::Int:
                unifyOrDereference(naturalType(module, makeSigned(module, pattern.intConst())), input, pattern);
                return true;
            case ASTKind::Unsigned:
                unifyOrDereference(naturalType(module, makeUnsigned(module, pattern.uintConst())), input, pattern);
                return true;
            case ASTKind::Float:
                unifyOrDereference(naturalType(module, makeFloat(module, pattern.floatConst())), input, pattern);
                return true;
            case ASTKind::Bool:
                unifyOrDereference(module->boolType(), input, pattern);
                return true;
            case ASTKind::Char:
                unifyOrDereference(module->charType(), input, pattern);
                return true;
            case ASTKind::String:
                unifyOrDereference(module->arrayType(module->i8Type(), (u32)module->str(pattern.stringConst()).size()), input, pattern);
                return true;

            case ASTKind::VarDecl:
                unifyOrDereference(pattern.type(), input, pattern);
                return true;

            case ASTKind::Splat:
                return refinePattern(ctx, function, input, pattern.child(0));

            case ASTKind::Construct:
                // We know the type of the aggregate statically, so we already
                // propagated the field types down to subpatterns during
                // earlier inference. All we need to know at this stage is that
                // our input actually matches the nominal type.
                unifyOrDereference(pattern.type(), input, pattern);
                return true;

            case ASTKind::Tuple: {
                input.concretify();
                input = expand(input);
                type_assert(input.is<TypeKind::Tuple>());
                auto tupleType = input.as<TypeKind::Tuple>();
                for (u32 i = 0; i < pattern.arity(); i ++) {
                    if (pattern.child(i).kind() == ASTKind::Splat) {
                        vec<TypeIndex> subTuple;
                        for (u32 j = i; j < pattern.arity(); j ++)
                            subTuple.push(tupleType.fieldTypeIndex(j));
                        refinePattern(ctx, function, module->tupleType(subTuple), pattern.child(i));
                        return true;
                    }
                    refinePattern(ctx, function, tupleType.fieldType(i), pattern.child(i));
                }
                return true;
            }

            case ASTKind::List: {
                unifyInPlace(input, pattern.type(), pattern);

                // Kind of like for constructor patterns, we already defined a
                // slice type for this pattern and propagated its constraints
                // down through the case body. So as long as we unify onto the
                // overall pattern type, we're good.
                return true;
            }

            case ASTKind::Typename:
            case ASTKind::GlobalTypename:
            case ASTKind::TypeField:
            case ASTKind::PtrType:
            case ASTKind::SliceType:
            case ASTKind::ArrayType:
            case ASTKind::TupleType:
            case ASTKind::FunType: {
                Type type = evaluateType(module, function, pattern);
                if (canUnify(type, input, pattern))
                    return true;
                if (expand(input).is<TypeKind::Pointer>())
                    return canUnify(type, expand(input).as<TypeKind::Pointer>().elementType(), pattern);
                return false;
            }

            default:
                unreachable("Invalid pattern node ", pattern);
        }
    }

    Function* instantiate(InferenceContext& ctx, Function* generic, AST call) {
        Module* module = generic->module;
        auto type = generic->type();
        type_assert(type.is<TypeKind::Function>());
        auto funcType = type.as<TypeKind::Function>();

        for (u32 i = 1; i < call.arity(); i ++) {
            auto arg = inferredType(ctx, generic, call.child(i));
            auto parameterType = funcType.as<TypeKind::Function>().parameterType(i - 1);
            unifyInPlace(arg, parameterType, call);
        }

        AST oldDecl = module->node(generic->decl);
        AST newDecl = module->clone(module->node(generic->decl));
        Function* newFunction = module->addFunction(newDecl, generic->parent);
        Scope* oldScope = oldDecl.scope();
        Scope* newScope = module->addScope(ScopeKind::Function, newDecl.node, oldDecl.scope()->parent, newFunction);
        for (auto e : oldScope->entries) {
            newScope->entries.put(e.key, e.value);
            newScope->inTable.add(e.key.symbol);
            newFunction->locals.push(generic->locals[e.value]);
        }

        newDecl.setScope(newScope);
        newFunction->typeIndex = generic->cloneGenericType().index;
        newDecl.setType(newFunction->type());

        computeScopes(module, newScope, newDecl.child(4));
        newDecl.setChild(4, resolveNode(newDecl.scope(), newDecl, newDecl.child(4)));

        infer(ctx, newFunction, newDecl.child(4));

        if (oldDecl.child(1).missing())
            oldDecl.setChild(1, newDecl);
        else
            oldDecl.setChild(1, module->add(ASTKind::Then, oldDecl.child(1), newDecl));

        return newFunction;
    }

    maybe<TypeIndex> refineOverloadedCall(InferenceContext& ctx, Function* function, AST ast) {
        assert(isOverloadedFunction(function, ast.child(0)));
        Overloads* overloads = ast.module->overloads[ast.child(0).varInfo(function).overloads];

        vec<pair<Function*, TypeIndex>> possibleFunctions;
        Module* module = ast.module;
        overloads->forEachFunction([&](Function* overload) {
            Module* module = overload->module;
            auto type = expand(overload->type());
            type_assert(type.is<TypeKind::Function>());
            auto funcType = type.as<TypeKind::Function>();
            if (ast.arity() - 1 != funcType.as<TypeKind::Function>().parameterCount())
                return;
            for (u32 i = 1; i < ast.arity(); i ++) {
                auto arg = inferredType(ctx, function, ast.child(i));
                auto parameterType = funcType.as<TypeKind::Function>().parameterType(i - 1);
                if (!canUnify(arg, parameterType, ast))
                    return;
            }
            possibleFunctions.push({ overload, type.index });
        });

        type_assert(possibleFunctions.size() <= 1);
        if (possibleFunctions.size() == 1) {
            Function* candidate = possibleFunctions[0].first;
            if (candidate->isGeneric)
                candidate = instantiate(ctx, candidate, ast);
            ast.setChild(0, module->add(ASTKind::ResolvedFunction, candidate));
            return some<TypeIndex>(possibleFunctions[0].second);
        }
        return none<TypeIndex>();
    }

    maybe<TypeIndex> refineCall(InferenceContext& ctx, Function* function, AST ast) {
        auto type = inferredType(ctx, function, ast.child(0));
        type_assert(type.is<TypeKind::Function>());

        auto funcType = type.as<TypeKind::Function>();
        type_assert(ast.arity() - 1 == funcType.as<TypeKind::Function>().parameterCount());
        for (u32 i = 1; i < ast.arity(); i ++) {
            auto arg = inferredType(ctx, function, ast.child(i));
            auto parameterType = funcType.as<TypeKind::Function>().parameterType(i - 1);
            if (!canUnify(arg, parameterType, ast))
                return none<TypeIndex>();
        }

        if (ast.child(0).kind() == ASTKind::ResolvedFunction) {
            Function* callee = ast.child(0).resolvedFunction();
            if (callee->isGeneric) {
                callee = instantiate(ctx, callee, ast);
                ast.setChild(0, ast.module->add(ASTKind::ResolvedFunction, callee));
            }
        }

        return some<TypeIndex>(funcType.index);
    }

    bool refine(InferenceContext& ctx, Function* function, AST ast, Type refinedType) {
        Module* module = ast.module;

        if UNLIKELY(config::verboseUnify >= 3)
            println("[TYPE]\tRefining node ", ast, " for refined type ", refinedType);

        switch (ast.kind()) {
            case ASTKind::GetField:
            case ASTKind::AddrField:
            case ASTKind::SetField: {
                Type baseType = inferredType(ctx, function, ast.child(0));

                if (baseType.is<TypeKind::Pointer>()) {
                    // Implicit dereference is permitted on field access.
                    baseType = expand(baseType.as<TypeKind::Pointer>().elementType());
                }

                if (baseType.is<TypeKind::Struct>()) {
                    assert(ast.child(1).kind() == ASTKind::Ident);
                    Symbol field = ast.child(1).symbol();

                    auto structType = baseType.as<TypeKind::Struct>();
                    vec<pair<rune, u32>> swizzleCandidates;
                    for (u32 i = 0; i < structType.count(); i ++) {
                        if (field == structType.fieldName(i)) {
                            ast.setChild(1, module->add(ASTKind::Field, FieldId(i)));

                            switch (ast.kind()) {
                                case ASTKind::GetField:
                                    unifyInPlace(structType.fieldType(i), refinedType, ast);
                                    break;
                                case ASTKind::AddrField:
                                    unifyInPlaceSubstituting(structType.fieldType(i), refinedType, ast);
                                    break;
                                case ASTKind::SetField:
                                    unifyInPlace(inferredType(ctx, function, ast.child(2)), structType.fieldType(i), ast);
                                    break;
                                default:
                                    unreachable("Expected a field access.");
                            }

                            return true;
                        }
                        auto name = module->str(structType.fieldName(i));
                        if (name.size() == 1 || (name.size() < 4 && utf8_length(name.data(), name.size()) == 1)) {
                            swizzleCandidates.push({ 0, i });
                            if (name.size() == 1)
                                swizzleCandidates.back().first = name[0];
                            else
                                utf8_decode(name.data(), name.size(), &swizzleCandidates.back().first, 1);
                        }
                    }

                    // Try swizzling if there were single-letter fields.
                    if (swizzleCandidates.size()) {
                        auto name = ast.module->str(field);
                        vec<FieldId> indices;
                        bool swizzleFailed = false;
                        const i8* ptr = name.data();
                        while (ptr != name.end()) {
                            rune r;
                            ptr = utf8_decode_forward(ptr, &r);
                            u32 sizeBefore = indices.size();
                            for (auto p : swizzleCandidates) if (p.first == r) {
                                indices.push(FieldId(p.second));
                                break;
                            }
                            if (indices.size() == sizeBefore) {
                                // No match found, swizzle failed.
                                swizzleFailed = true;
                                break;
                            }
                        }
                        assert(!swizzleFailed); // If swizzling failed, this is just a bad access.

                        vec<Type> fieldTypes;
                        for (FieldId i : indices)
                            fieldTypes.push(ast.kind() == ASTKind::AddrField ? module->ptrType(structType.fieldType(i.id)) : structType.fieldType(i.id));
                        Type result = module->tupleType(fieldTypes);
                        unifyInPlace(result, refinedType, ast);
                        AST newAST;
                        switch (ast.kind()) {
                            case ASTKind::GetField:
                                module->replace(ast, module->add(ASTKind::GetFields, ast.pos(), ast.scope(), result, ast.child(0), indices));
                                break;
                            case ASTKind::AddrField:
                                module->replace(ast, module->add(ASTKind::AddrFields, ast.pos(), ast.scope(), result, ast.child(0), indices));
                                break;
                            case ASTKind::SetField:
                                module->replace(ast, module->add(ASTKind::SetFields, ast.pos(), ast.scope(), module->voidType(), ast.child(0), indices, ast.child(2)));
                                unifyInPlace(inferredType(ctx, function, ast.child(2)), result, ast);
                                break;
                            default:
                                unreachable("Expected a field access.");
                        }
                        return true;
                    }

                    type_error("Undefined field name '", module->str(field), "'");
                } else if (baseType.is<TypeKind::Tuple>()) {
                    unreachable("TODO: Implement tuple access.");
                }

                return true;
            }

            case ASTKind::GetFields: {
                Type baseType = inferredType(ctx, function, ast.child(0));

                if (baseType.is<TypeKind::Pointer>()) {
                    // Implicit dereference is permitted on field access.
                    baseType = expand(baseType.as<TypeKind::Pointer>().elementType());
                }

                if (baseType.is<TypeKind::Struct>()) {
                    auto structType = baseType.as<TypeKind::Struct>();

                    if (ast.type().isVar()) {
                        vec<Type> types;
                        for (u32 i = 1; i < ast.arity(); i ++)
                            types.push(module->varType());
                        Type old = ast.type();
                        ast.setType(module->tupleType(types));
                        if (old.isVar())
                            unifyInPlace(old, ast);
                    }
                    type_assert(ast.type().is<TypeKind::Tuple>());
                    auto astTuple = ast.type().as<TypeKind::Tuple>();

                    for (u32 i = 1; i < ast.arity(); i ++) {
                        Symbol field = ast.child(i).symbol();
                        bool foundField = false;
                        for (u32 j = 0; j < structType.count(); j ++) {
                            if (field == structType.fieldName(j)) {
                                ast.setChild(i, module->add(ASTKind::Field, FieldId(j)));
                                unifyInPlace(structType.fieldType(j), astTuple.fieldType(i), ast);
                                foundField = true;
                            }
                        }
                        type_assert(foundField);
                    }
                    return true;
                } else if (baseType.is<TypeKind::Tuple>()) {
                    auto tupleType = baseType.as<TypeKind::Tuple>();
                    unreachable("TODO: Tuple accesses");
                } else
                    unreachable("Unexpected base type '", baseType, "' in multi-field access.");
            }

            case ASTKind::GetIndex:
            case ASTKind::AddrIndex:
            case ASTKind::SetIndex: {
                Type baseType = inferredType(ctx, function, ast.child(0));

                // Indexing supports implicit dereference.
                if (baseType.is<TypeKind::Pointer>())
                    baseType = expand(baseType.as<TypeKind::Pointer>().elementType());
                Type elementType = baseType.is<TypeKind::Slice>() ? expand(baseType.as<TypeKind::Slice>().elementType()) : expand(baseType.as<TypeKind::Array>().elementType());
                if (ast.kind() == ASTKind::SetIndex) {
                    unifyInPlace(refinedType, elementType, ast);
                    unifyInPlace(inferredType(ctx, function, ast.child(2)), refinedType, ast);
                } else if (ast.kind() == ASTKind::GetIndex)
                    unifyInPlace(elementType, refinedType, ast);
                else
                    unifyInPlaceSubstituting(elementType, refinedType, ast);
                return true;
            }

            case ASTKind::Call:
            case ASTKind::CallMethod: {
                maybe<TypeIndex> resolvedType = none<TypeIndex>();
                if (isOverloadedFunction(function, ast.child(0)))
                    resolvedType = refineOverloadedCall(ctx, function, ast);
                else
                    resolvedType = refineCall(ctx, function, ast);
                if (!resolvedType && ast.kind() == ASTKind::CallMethod) {
                    auto arg = inferredType(ctx, function, ast.child(1));
                    switch (ast.child(1).kind()) {
                        case ASTKind::Local:
                        case ASTKind::Global:
                            ast.setChild(1, module->add(ASTKind::AddressOf, ast.pos(), ast.scope(), module->ptrType(eliminateRange(arg)), ast.child(1)));
                            break;
                        case ASTKind::GetIndex:
                            ast.child(1).setKind(ASTKind::AddrIndex);
                            ast.child(1).setType(arg = module->ptrType(eliminateRange(arg)));
                            break;
                        case ASTKind::GetField:
                            ast.child(1).setKind(ASTKind::AddrField);
                            ast.child(1).setType(arg = module->ptrType(eliminateRange(arg)));
                            break;
                        default:
                            type_error("Failed to call method ", ast.child(0), ": argument ", ast.child(1), " could not be implicitly referenced.");
                    }
                    if (isOverloadedFunction(function, ast.child(0)))
                        resolvedType = refineOverloadedCall(ctx, function, ast);
                    else
                        resolvedType = refineCall(ctx, function, ast);
                    type_assert(resolvedType);
                }

                auto funcType = module->types->get(*resolvedType).as<TypeKind::Function>();
                for (u32 i = 1; i < ast.arity(); i ++) {
                    auto arg = inferredType(ctx, function, ast.child(i));
                    auto parameterType = funcType.as<TypeKind::Function>().parameterType(i - 1);
                    unifyInPlace(arg, parameterType, ast);
                }
                unifyInPlace(funcType.as<TypeKind::Function>().returnType(), ast);
                ast.setKind(ASTKind::Call); // These will be truly indistinguishable from this point forward.
                return true;
            }

            case ASTKind::Match: {
                bool result = true;
                Type baseType = expand(refinedType);
                if (baseType.isVar() && baseType.asVar().lowerBound().is<TypeKind::Pointer>()) {
                    baseType.asVar().makeEqual(baseType.asVar().lowerBound());
                    baseType = expand(baseType);
                }
                if (baseType.is<TypeKind::Pointer>())
                    baseType = baseType.as<TypeKind::Pointer>().elementType();
                for (AST matchCase : ast.children(1)) {
                    AST check = matchCase.child(0);
                    AST body = matchCase.child(1);
                    if (!check.missing() && !refinePattern(ctx, function, baseType, check))
                        result = false;
                }
                return result;
            }

            case ASTKind::Is:
            case ASTKind::VarDecl: {
                Type baseType = expand(refinedType);
                if (baseType.isVar() && baseType.asVar().lowerBound().is<TypeKind::Pointer>()) {
                    baseType.asVar().makeEqual(baseType.asVar().lowerBound());
                    baseType = expand(baseType);
                }
                if (baseType.is<TypeKind::Pointer>())
                    baseType = baseType.as<TypeKind::Pointer>().elementType();

                AST pattern = ast.child(1);
                return refinePattern(ctx, function, baseType, pattern);
            }

            default:
                // All other nodes are fine relying on subtype constraints, for
                // now. So we can just assume there's nothing to refine. Really
                // we shouldn't even be here, but it probably doesn't matter.
                return true;
        }
    }

    struct SCCNode {
        ConstraintIndex index;
        ConstraintIndex lowLink : 31;
        u32 onStack : 1;
    };

    using StrongConnectFunc = void(Constraints&, const_slice<ConstraintIndex>);

    void resolveSubtypeCycle(Constraints& constraints, const_slice<ConstraintIndex> cycle) {
        TypeSystem* types = constraints.types;
        bitset<128> group;
        for (ConstraintIndex i : cycle)
            group.on(constraints.expand(i));

        // When we find a subtype cycle, we know that due to the transitivity
        // of subtyping, all types in the cycle must be strictly equal. To
        // apply this result, we use the below algorithm. From the types in the
        // cycle, we pick the first non-variable type to represent the whole
        // cycle, or the first variable type if no non-variable types are in
        // the cycle.
        //
        // For all variables in the cycle, we transfer their constraints to the
        // result type, whether it's concrete or not - we'll shake out whether
        // these constraints are satisfiable later in inference. For all
        // non-variable types, they must be strictly equal to each other, since
        // we aren't allowed to change them. When transferring constraints, we
        // skip any constraints that depend on other elements of the cycle. For
        // subtyping constraints, this is straightforward - we already found
        // the cycle by exploring subtype constraints, and equality implies
        // subtyping in both directions, so as long as we assert the types in
        // the cycle are equal all subtype edges between types are implicitly
        // held. It's a little less obvious why we can do this for ordering
        // edges too, maybe. If a type in the cycle has an ordering constraint
        // on another type in the cycle, then we no longer need to refine its
        // type independently, since we now know at least one other type that
        // it is strictly equal to. So as long as we have a path to figure out
        // at least one type, through one external ordering constraint, we know
        // all the others in the cycle. Thus, all ordering constraints between
        // elements of the cycle are moot.

        Type result = types->invalidType();
        ConstraintIndex resultIndex = InvalidConstraint;
        vec<Constraint> newConstraints;
        bool foundAnyOrderedBesidesResult = false;
        for (ConstraintIndex index : group) {
            Type type = types->get(constraints.constrainedTypes[index]);
            if (!result) {
                result = type;
                resultIndex = index;
                for (Constraint constraint : constraints.constraints[index]) if (!group[constraint.index])
                    newConstraints.push(constraint);
            } else if (!type.isVar()) {
                if (result.isVar()) {
                    ConstraintIndex lower = constraints.index(result.asVar().lowerBound());
                    newConstraints.push({ .index = lower, .kind = Constraint::Subtype });
                    constraints.constrainType(type, result.asVar().upperBound());

                    result.asVar().makeEqual(type);
                    constraints.constraints[resultIndex].forwardTo(index);
                    result = type;
                    resultIndex = index;
                } else if (result != type)
                    type_error("[TYPE]\tFailed to satisfy equality requirement between types ", type, " and ", result, " which share a cycle.");
            } else {
                for (Constraint constraint : constraints.constraints[index]) if (!group[constraint.index])
                    newConstraints.push(constraint);
                foundAnyOrderedBesidesResult = constraints.constraints[index].doesNeedRefinement() || foundAnyOrderedBesidesResult;
                if (result.isVar()) {
                    Type lowerBound = leastCommonSupertype(result.asVar().lowerBound(), type.asVar().lowerBound(), NoUnifyFlags);
                    type_assert(lowerBound);
                    result.asVar().setLowerBound(lowerBound);
                    Type upperBound = greatestCommonSubtype(result.asVar().upperBound(), type.asVar().upperBound(), NoUnifyFlags);
                    type_assert(upperBound);
                    result.asVar().setUpperBound(upperBound);
                } else {
                    ConstraintIndex lower = constraints.index(type.asVar().lowerBound());
                    newConstraints.push({ .index = lower, .kind = Constraint::Subtype });
                    constraints.constrainType(result, type.asVar().upperBound());
                }
                type.asVar().makeEqual(result);
                constraints.constraints[index].forwardTo(resultIndex);
            }
        }

        assert(result);
        assert(resultIndex != InvalidConstraint);
        constraints.constraints[resultIndex].clear();

        if (foundAnyOrderedBesidesResult) {
            // If there were elements of the cycle that need to be refined, but
            // whose ordering edges we're about to strip, then we do something
            // special: record this cycle, and record its index as the first
            // constraint.

            if UNLIKELY(constraints.constraints[resultIndex].hasRefinementList) {
                // In the unlikely case we already have a refinement list, add
                // its elements to the group.
                auto& list = constraints.constraints[resultIndex].refinementList(constraints);
                for (ConstraintIndex i : list)
                    group.on(i);

                if UNLIKELY(config::verboseUnify >= 3) {
                    print("[TYPE]\tMaintaining existing refinement list [");
                    bool first = true;
                    for (ConstraintIndex i : list) {
                        if (!first) print(", ");
                        first = false;
                        print(types->get(constraints.constrainedTypes[i]));
                    }
                    println("] for result variable ", types->get(constraints.constrainedTypes[resultIndex]));
                }
            }

            for (ConstraintIndex i : group) if UNLIKELY(constraints.constraints[i].hasRefinementList) {
                // And, if any other elements of our group have a refinement
                // list, we want to remove it and add it to ours.

                auto& list = constraints.constraints[i].refinementList(constraints);
                for (ConstraintIndex j : list) {
                    assert(!constraints.constraints[j].hasRefinementList);
                    group.on(j);
                }

                if UNLIKELY(config::verboseUnify >= 3) {
                    print("[TYPE]\tStealing refinement list [");
                    bool first = true;
                    for (ConstraintIndex j : list) {
                        if (!first) print(", ");
                        first = false;
                        print(types->get(constraints.constrainedTypes[j]));
                    }
                    println("] from variable ", types->get(constraints.constrainedTypes[i]));
                }

                constraints.constraints[i].dropRefinementList(constraints);
            }

            auto& list = constraints.constraints[resultIndex].ensureRefinementList(constraints);
            list.clear();
            for (ConstraintIndex i : group) if (i != resultIndex && constraints.constraints[i].doesNeedRefinement())
                list.push(i);

            if UNLIKELY(config::verboseUnify >= 3) {
                print("[TYPE]\tFinal refinement list for result type ", result, " is [");
                bool first = true;
                for (ConstraintIndex i : list) {
                    if (!first) print(", ");
                    first = false;
                    print(types->get(constraints.constrainedTypes[i]));
                }
                println("]");
            }
        }

        for (Constraint constraint : newConstraints)
            constraints.constraints[resultIndex].add(constraint);
    }

    void resolveOrderingCycle(Constraints& constraints, const_slice<ConstraintIndex> cycle) {
        TypeSystem* types = constraints.types;
        bitset<128> group;
        for (ConstraintIndex i : cycle)
            group.on(constraints.expand(i));

        // Ordering cycles are kinda weird, because we basically have no good
        // way to handle them. We're going to have to pick some node from the
        // cycle to refine with incomplete information, and just hope that it
        // works out. But this is already handled by the topological sort we do
        // later, so for now, we just remove these edges since they don't do
        // anything other than confuse the inference algorithm.
        //
        // Honestly, are these even possible? It seems like it'd be pretty hard
        // since ordering edges almost always (always?) point from child nodes
        // to parent nodes.

        for (ConstraintIndex node : cycle)
            constraints.constraints[node].removeIf([&](Constraint constraint) -> bool { return constraint.kind == Constraint::Order && group[constraint.index]; });
    }

    void strongconnect(Constraints& constraints, ConstraintIndex node, vec<SCCNode, 31>& nodes, vec<ConstraintIndex>& stack, u32& index, StrongConnectFunc func) {
        TypeSystem* types = constraints.types;
        bool isOrder = func == resolveOrderingCycle; // Kind of icky but we probably won't be adding new functions often.

        nodes[node].index = nodes[node].lowLink = index ++;
        nodes[node].onStack = 1;
        stack.push(node);
        for (Constraint& edge : constraints.constraints[node]) {
            if (isOrder != (edge.kind == Constraint::Order))
                continue;
            edge.index = constraints.expand(edge.index);
            if (nodes[edge.index].index == InvalidConstraint)
                strongconnect(constraints, edge.index, nodes, stack, index, func);
            else if (nodes[edge.index].onStack)
                nodes[node].lowLink = min(nodes[node].lowLink, nodes[edge.index].lowLink);
        }

        if (nodes[node].lowLink == nodes[node].index) {
            if (stack.back() == node) {
                if UNLIKELY(config::verboseUnify >= 2)
                    println("[TYPE]\tFound constrained type ", types->get(constraints.constrainedTypes[node]), " that is not a member of any cycle.");

                // SCC of one, nothing more to be done.
                nodes[stack.back()].onStack = 0;
                stack.pop();
                return;
            }

            ConstraintIndex edge;
            u32 firstInCycle = stack.size() - 1;
            while (stack[firstInCycle] != node) {
                assert(firstInCycle > 0);
                firstInCycle --;
            }
            const_slice<ConstraintIndex> cycle = ((const_slice<ConstraintIndex>)stack).drop(firstInCycle);

            if UNLIKELY(config::verboseUnify >= 1) {
                print("[TYPE]\tFound strongly-connected group of ", cycle.size(), " types: ");
                bool first = true;
                for (ConstraintIndex node : cycle) {
                    if (!first) print(", ");
                    first = false;
                    print(types->get(constraints.constrainedTypes[node]));
                }
                println();
            }

            func(constraints, cycle);
        }
    }

    void scc(Constraints& constraints, StrongConnectFunc func) {
        // Finds all strongly-connected components in the graph, i.e. variables
        // that have cyclic subtyping requirements, and turns them into
        // equality classes. We use Tarjan's algorithm because it only requires
        // us to know about one direction of edge, whereas Kosaraju-Shirar's
        // also requires knowing about incoming ones.

        vec<SCCNode, 31> nodes;
        vec<ConstraintIndex> stack;
        nodes.expandTo(constraints.constraints.size(), SCCNode { InvalidConstraint, 0, 0 });
        u32 index = 0;

        for (ConstraintIndex i = 0; i < nodes.size(); i ++) {
            ConstraintIndex expanded = constraints.expand(i);
            if (nodes[expanded].index == InvalidConstraint)
                strongconnect(constraints, expanded, nodes, stack, index, func);
        }
    }

    void toposort(Constraints& constraints, bitset<128>& tempMarks, bitset<128>& permMarks, vec<ConstraintIndex, 32>& order, ConstraintIndex node) {
        if (permMarks[node])
            return;
        if (tempMarks[node]) {
            // It's some sort of backedge, which means we have a cycle. We know
            // this can't be an ordering cycle, because we eliminated those
            // before reaching this point. But we do record it in the order,
            // since we want each type to be visited as early as possible so
            // that we maximally discover all type information before refining
            // an order-dependent node.
            order.push(node);
            return;
        }

        tempMarks.on(node);
        for (Constraint& other : constraints.constraints[node]) {
            other.index = constraints.expand(other.index);

            if (permMarks[other.index])
                continue;
            if (tempMarks[other.index]) {
                order.push(other.index);
                continue;
            }
            toposort(constraints, tempMarks, permMarks, order, other.index);
        }

        permMarks.on(node);
        order.push(node);
    }

    void cleanUpOrder(vec<ConstraintIndex, 32>& order) {
        // This function removes duplicate nodes from the ordering, since we
        // may have duplicates in the order if there were cycles in the
        // combined ordering/subtyping graph.
        bitset<128> visited;
        ConstraintIndex* writer = order.begin();
        ConstraintIndex* reader = writer;
        while (reader < order.end()) {
            if (!visited[*reader]) {
                *writer ++ = *reader;
                visited.on(*reader);
            }
            reader ++;
        }
        order.trim(reader - writer);
    }

    void refineGraph(Module* module, InferenceContext& ctx) {
        // This is a beastly function, and contains the bulk of the actual
        // inference algorithm. From infer(), we have a constraint graph
        // containing both (1) subtype-supertype edges between types, and
        // (2) intertype dependencies that exist independently of subtyping.
        //
        // The task of this function is, at the top level, to take the inferred
        // types from the previous pass and refine/finalize them for later
        // passes. Broadly, we do this via an iterative deepening search. Based
        // on the available dependency edge information, we create "epochs",
        // where we search upwards from root types (those that have no
        // supertype constraints targeting them) until we find a frontier of
        // nodes that have dependency constraints. Once we have found this
        // frontier, we take all types that didn't have any dependencies and
        // attempt to resolve them. Since we essentially know the final lower
        // bound of each type in this epoch by this point, if we know we'd
        // pick the lower bound of the variable when concretifying the type
        // (i.e. those for which canonicalTypeInBounds(lb, ub) returns lb) we
        // can concretify it to its lower bound. Otherwise, we have to leave
        // the type in the constraint graph.

        u32 epoch = 0;

        Constraints& constraints = *ctx.constraints;
        TypeSystem* types = constraints.types;
        vec<i32> epochs;
        u32 numVisited = 0;
        epochs.expandTo(constraints.constraints.size(), -1);

        // Before anything else, we need to eliminate any initial cycles in
        // the dependency graph. We do two passes, first reducing any cycles
        // in the subtype relation to equivalence classes, then cataloguing any
        // remaining cycles in the ordering graph.

        scc(constraints, resolveSubtypeCycle);
        scc(constraints, resolveOrderingCycle);
        bitset<128> nonForwarded;
        for (ConstraintIndex i : indices(constraints.constraints)) if (!constraints.constraints[i].isForwarded())
            nonForwarded.on(i);

        if UNLIKELY(config::printInferredTreeAfterEachPass) {
            println("| Module after cycle elimination: ");
            println("*---------------------------------");
            module->print(module->compilation), println();
        }
        if (UNLIKELY(config::printTypeConstraints))
            printTypeConstraints(module->types, &constraints);

        // We know the graph no longer holds any ordering or subtype cycles.
        // There still might be some cycles in the combined graph, but these
        // mostly aren't a problem, since they represent cases where we have to
        // make typing decisions based on incomplete information.

        vec<ConstraintIndex, 32> order;
        bitset<128> tempMarks, permMarks;
        for (ConstraintIndex i : nonForwarded) {
            if (!permMarks[i])
                toposort(constraints, tempMarks, permMarks, order, i);
        }

        cleanUpOrder(order);

        if UNLIKELY(config::verboseUnify >= 3) {
            println("[TYPE]\tOrder of types:");
            for (ConstraintIndex i : order) {
                Type t = types->get(constraints.constrainedTypes[i]);
                println("[TYPE]\t - ", t);
            }
        }

        // Now we follow the previously computed order and refine our previous
        // inferences. We do this by two main tactics:
        //
        //  - For each subtype of a node, we refine the lower bound to the
        //    least common supertype of the existing lower bound and the
        //    subtype.
        //
        //  - Then, if we had any ordering constraints, we refine() the node
        //    to try and pick a more specific type based on the information we
        //    now know.
        //
        // Once we have exhausted these for a node, we should know everything
        // there is to know about the lower bound of a type variable. If the
        // currently known lower bound is such that we will never pick the
        // upper bound when concretifying, we can substitute that lower bound
        // for the type variable and simplify the rest of the inference.

        for (ConstraintIndex i : order) {
            // It's important to remember the original type, since once we
            // expand, we will potentially have lost any variable provenance.
            Type origType = constraints.types->get(constraints.constrainedTypes[i]);
            Type type = expand(origType);

            // First we make sure we're greater than all of our subtypes...

            for (Constraint constraint : constraints.constraints[i]) {
                constraint.index = constraints.expand(constraint.index);
                if (constraint.kind == Constraint::Order)
                    continue;

                auto substituteFlag = constraint.kind == Constraint::Substitute ? MustSubstitute : NoUnifyFlags;

                Type other = expand(constraints.types->get(constraints.constrainedTypes[constraint.index]));
                if UNLIKELY(config::verboseUnify >= 3)
                    println("[TYPE]\tTrying to apply subtype constraint against ", other, " to constrained type ", type);

                if (type.index == other.index)
                    continue;

                bool isVar = type.isVar(), otherVar = other.isVar();
                Type bound = expand(type);
                if (type.isVar())
                    bound = type.asVar().lowerBound();
                type_assert(other.unifyOnto(type, nullptr, InPlace));
                if (substituteFlag && !isCaseTypeKind(bound.kind()))
                    type_assert(type.unifyOnto(other, nullptr, InPlace));
            }

            // Next, if we had any ordering constraints, we know we need to be
            // refined. So we refine() the owner of this type.

            if (constraints.constraints[i].doesNeedRefinement()) {
                assert(origType.isVar());
                assert(origType.asVar().hasOwner());
                AST ast = origType.asVar().owner(module);
                assert(!ast.isLeaf());
                type_assert(refine(ctx, ast.function(), ast, type));
            }

            // On a similar note, we need to handle the case where this type
            // has a refinement list. This will happen if our type doesn't need
            // refinement, but another type that does need refinement was
            // forwarded to it.

            if UNLIKELY(constraints.constraints[i].hasRefinementList) {
                auto& list = constraints.constraints[i].refinementList(constraints);

                for (ConstraintIndex i : list) {
                    Type origType = constraints.types->get(constraints.constrainedTypes[i]);
                    assert(origType.isVar());
                    assert(origType.asVar().hasOwner());
                    AST ast = origType.asVar().owner(module);
                    assert(!ast.isLeaf());
                    type_assert(refine(ctx, ast.function(), ast, type));
                }

                constraints.constraints[i].dropRefinementList(constraints);
            }

            // Finally, we try and reduce the type if that's possible based on
            // its current lower bound.

            if (type.isVar() && !type.asVar().isEqual() && canResolveTypeFromLowerBound(constraints.types, type.asVar().lowerBound()))
                type.asVar().concretify();
        }
    }

    void checkConstruct(InferenceContext& ctx, Function* function, AST ast) {
        assert(ast.kind() == ASTKind::Construct);
        Type type = ast.type();
        Module* module = ast.module;
        switch (type.kind()) {
            case TypeKind::Numeric:
            case TypeKind::Char: {
                // Any Char or number can be cast to any other Char or number.
                Type operandType = inferredType(ctx, function, ast.child(0));
                type_assert(operandType.unifyOnto(module->charType(), nullptr, Query) == UnifySuccess
                    || operandType.unifyOnto(module->topNumberType(), nullptr, Query) == UnifySuccess);
                break;
            }
            case TypeKind::Pointer: {
                Type operandType = inferredType(ctx, function, ast.child(0));
                type_assert(operandType.is<TypeKind::Pointer>());
                break;
            }
            case TypeKind::Slice: {
                Type operandType = inferredType(ctx, function, ast.child(0));
                type_assert(operandType.is<TypeKind::Slice>());
                break;
            }
            default:
                break;
        }
    }

    void check(InferenceContext& ctx, Function* function, AST ast) {
        Module* module = ast.module;
        TypeSystem* types = module->types;
        Type lhsType, rhsType, operandType, resultType;

        switch (ast.kind()) {
            case ASTKind::Less:
            case ASTKind::LessEq:
            case ASTKind::Greater:
            case ASTKind::GreaterEq:
            case ASTKind::Equal:
            case ASTKind::NotEqual:
                lhsType = inferredType(ctx, function, ast.child(0));
                rhsType = inferredType(ctx, function, ast.child(1));
                operandType = concreteType(module, pickTypeFromEqualPair(lhsType, rhsType));
                if (ast.kind() == ASTKind::Equal || ast.kind() == ASTKind::NotEqual)
                    type_assert(operandType.is<TypeKind::Numeric>()
                        || operandType.is<TypeKind::Function>()
                        || operandType == Bool
                        || operandType == Char
                        || operandType.is<TypeKind::Pointer>());
                else
                    type_assert(operandType.is<TypeKind::Numeric>() || operandType == Char);
                break;

            case ASTKind::Construct:
                checkConstruct(ctx, function, ast);
                break;

            case ASTKind::TopLevel:
                concretifyNode(ast);
                break;

            case ASTKind::FunDecl:
                ast.function()->typeIndex = concreteType(module, ast.function()->type()).index;
                break;

            default:
                break;
        }
    }

    NOINLINE Artifact* inferAndCheckTypes(Artifact* artifact) {
        assert(artifact->kind == ArtifactKind::ResolvedAST);

        Module* module = artifact->as<Module>();
        module->nodeTypes.expandTo(module->ast.size(), InvalidType);

        // First, discover type and ordering constraints.

        Constraints constraints(module, module->types, 0);
        vec<NodeIndex, 16> lateChecks;
        vec<TypeIndex, 16> lateResolves;

        InferenceContext globalCtx;
        globalCtx.constraints = &constraints;
        globalCtx.lateChecks = &lateChecks;
        globalCtx.lateResolves = &lateResolves;

        Evaluation inference = infer(globalCtx, nullptr, module->getTopLevel());
        if UNLIKELY(config::printTypeConstraintsAsDOT) {
            fd dotfile = file::stdout;
            if (config::cloverDOTFile.size())
                dotfile = file::open(config::cloverDOTFile, file::WRITE | file::APPEND);
            writeln(dotfile, DOT(constraints));
            if (dotfile != file::stdout)
                file::close(dotfile);
        }
        if UNLIKELY(config::printTypeConstraints)
            printTypeConstraints(module->types, &constraints);
        if UNLIKELY(config::printInferredTreeAfterEachPass) {
            println("| Module after type discovery pass: ");
            println("*-----------------------------------");
            module->print(module->compilation), println();
        }

        refineGraph(module, globalCtx);
        if UNLIKELY(config::printInferredTreeAfterEachPass) {
            println("| Module after type refinement pass: ");
            println("*------------------------------------");
            module->print(module->compilation), println();
        }

        for (NodeIndex node : *globalCtx.lateChecks) {
            AST ast = module->node(node);
            assert(!ast.isLeaf());
            check(globalCtx, ast.function(), ast);
        }
        for (TypeIndex v : *globalCtx.lateResolves)
            expand(module->types->get(v)).concretify();

        if UNLIKELY(config::printInferredTreeAfterEachPass) {
            println("| Module after type checking pass: ");
            println("*----------------------------------");
            module->print(module->compilation), println();
        } else if UNLIKELY(config::printInferredTree)
            module->print(module->compilation), println();
        artifact->update(ArtifactKind::CheckedAST, module);
        return artifact;
    }

    template<typename IO, typename Format = Formatter<IO>>
    inline IO format_impl(IO io, const DOT<Constraints>& dot) {
        auto constraints = dot.value;

        io = format(io, "digraph G {\n");
        bitset<128> visited;
        for (u32 i = 0; i < constraints.constraints.size(); i ++) {
            if (constraints.constraints[i].isForwarded())
                i = constraints.constraints[i].expand();
            if (visited[i])
                continue;
            Type type = constraints.types->get(constraints.constrainedTypes[i]);
            const auto& node = constraints.constraints[i];
            io = format(io, "    node_", i, " [label=\"", type);
            if (type.isVar())
                io = format(io, " {", type.asVar().lowerBound(), ":", type.asVar().upperBound(), "}");
            io = format(io, "\"]\n");
            for (u32 j = 0; j < node.size(); j ++) {
                if (node[j].kind == Constraint::Order) {
                    io = format(io, "    node_", node[j].index, " -> node_", i);
                    io = format(io, " [color=red]");
                } else if (node[j].kind == Constraint::Substitute) {
                    io = format(io, "    node_", node[j].index, " -> node_", i);
                    io = format(io, " [color=blue]");
                } else
                    io = format(io, "    node_", i, " -> node_", node[j].index);
                io = format(io, "\n");
            }
        }
        return format(io, "}\n");
    }
}