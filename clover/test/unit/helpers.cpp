#include "clover/test/unit/helpers.h"

bool inExpectErrorsScope = false;

bool sameAST(ArtifactKind kind, Function* function, AST a, AST b, ShouldComparePos posMode) {
    bool result = true;
    if (a.kind() != b.kind())
        result = false;
    else {
        if (a.isLeaf()) {
            switch (a.kind()) {
                case ASTKind::Int:
                    result = a.intConst() == b.intConst() && result;
                    break;
                case ASTKind::Unsigned:
                    result = a.uintConst() == b.uintConst() && result;
                    break;
                case ASTKind::String:
                    result = a.module->str(a.stringConst()) == b.module->str(b.stringConst()) && result;
                    break;
                case ASTKind::Bool:
                    result = a.boolConst() == b.boolConst() && result;
                    break;
                case ASTKind::Float:
                    result = bits_equal(a.floatConst(), b.floatConst()) && result;
                    break;
                case ASTKind::Ident:
                    result = a.module->str(a.symbol()) == b.module->str(b.symbol()) && result;
                    break;
                case ASTKind::Local:
                case ASTKind::Capture:
                case ASTKind::Const:
                case ASTKind::Typename:
                case ASTKind::Global:
                case ASTKind::GlobalTypename:
                    result = a.module->str(a.varInfo(function).name) == b.module->str(b.varInfo(function).name) && result;
                    break;
                case ASTKind::Field:
                    result = a.fieldId() == b.fieldId() && result;
                    break;
                case ASTKind::ResolvedFunction:
                    result = a.resolvedFunction() == b.resolvedFunction() && result;
                    break;
                case ASTKind::ResolvedOverloads:
                    result = a.resolvedOverloads() == b.resolvedOverloads() && result;
                    break;
                case ASTKind::ResolvedNamespace:
                    result = a.resolvedNamespace() == b.resolvedNamespace() && result;
                    break;
                case ASTKind::ResolvedGenericType:
                    result = a.genericType() == b.genericType() && result;
                    break;
                case ASTKind::Wildcard:
                case ASTKind::Missing:
                case ASTKind::Uninit:
                    break;
                default:
                    unreachable("Unexpected leaf kind.");
            }
        } else {
            if (a.arity() != b.arity())
                result = false;
            for (u32 i = 0; i < a.arity(); i ++) {
                if (!sameAST(kind, kind < ArtifactKind::ScopedAST ? nullptr : a.scope()->function, a.child(i), b.child(i), posMode))
                    return false;
            }
        }
    }
    if UNLIKELY(config::printProducts && !result)
        println("Found mismatch between ", a, " and ", b);
    return result;
}