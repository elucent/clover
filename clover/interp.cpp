#include "clover/interp.h"

namespace clover {
    Value eval(Value env, AST ast) {
        switch (ast.kind()) {
            case ASTKind::Const: {
                Value v = get(env, boxUnsigned(ast.variable()));
                type_assert(v.kind() != Value::Undefined);
                return v;
            }
            case ASTKind::Int:
                return boxInt(ast.intConst());
            case ASTKind::Unsigned:
                return boxUnsigned(ast.uintConst());
            case ASTKind::Float:
                return boxFloat(ast.floatConst());
            case ASTKind::Bool:
                return boxBool(ast.boolConst());
            case ASTKind::Char:
                return boxChar(ast.charConst());
            case ASTKind::String:
                return makeString(ast.module->str(ast.stringConst()));

            default:
                unreachable("Couldn't evaluate expression ", ast);
        }
    }

    Value call(AST func, slice<Value> values) {
        unreachable("TODO: Implement interpreter.");
    }
}