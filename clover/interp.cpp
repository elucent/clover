#include "clover/interp.h"

namespace clover {
    f64 toFloat(Value v) {
        auto vk = v.kind();
        if (vk == Value::Float)
            return v.f;
        if (vk == Value::Int)
            return v.i;
        if (vk == Value::Unsigned)
            return v.u;
        unreachable("Tried to materialize float value from non-float value ", v);
    }

    Value eval(Value env, AST ast) {
        switch (ast.kind()) {
            case ASTKind::GlobalConst:
            case ASTKind::Const: {
                Value v = get(env, boxUnsigned(ast.constId()));
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

            case ASTKind::Add: {
                Value lhs = eval(env, ast.child(0));
                Value rhs = eval(env, ast.child(1));
                Value::Kind lk = lhs.kind(), rk = rhs.kind();
                if (lk == Value::Float || rk == Value::Float)
                    return boxFloat(toFloat(lhs) + toFloat(rhs));
                else if (lk == Value::Int && rk == Value::Int) {
                    type_assert(!addOverflows<i64>(lhs.i, rhs.i));
                    return boxInt(lhs.i + rhs.i);
                } else if (lk == Value::Unsigned && rk == Value::Unsigned) {
                    type_assert(!addOverflows<u64>(lhs.u, rhs.u));
                    return boxUnsigned(lhs.u + rhs.u);
                } else {
                    u64 upart = lk == Value::Unsigned ? lhs.u : rhs.u;
                    i64 ipart = lk == Value::Unsigned ? rhs.i : lhs.i;
                    if (upart <= 0x7fffffffffffffffull) {
                        type_assert(!addOverflows<i64>(i64(upart), ipart));
                        i64 result = i64(upart) + ipart;
                        return result >= 0 ? boxUnsigned(result) : boxInt(result);
                    } else if (ipart < 0)
                        return boxUnsigned(upart + ipart);
                    else {
                        type_assert(!addOverflows<u64>(u64(ipart), upart));
                        u64 result = upart + u64(ipart);
                        return boxUnsigned(result);
                    }
                }
            }

            default:
                unreachable("Couldn't evaluate expression ", ast);
        }
    }

    Value call(AST func, slice<Value> values) {
        unreachable("TODO: Implement interpreter.");
    }
}