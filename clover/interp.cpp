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

    bool noPreviousErrors() {
        return true;
    }

    template<typename... Args>
    bool noPreviousErrors(Value value, Args... args) {
        if (value.isError())
            return false;
        return noPreviousErrors(args...);
    }

    Value evalBitwise(Value env, AST l, AST r, i64(*intFunc)(i64, i64), u64(*uintFunc)(u64, u64), const i8* op) {
        Value lhs = eval(env, l);
        Value rhs = eval(env, r);
        Value::Kind lk = lhs.kind(), rk = rhs.kind();
        if (lk == Value::Int || rk == Value::Int)
            return boxInt(intFunc(lhs.i, rhs.i));
        else if (lhs.isAnyInt() && rhs.isAnyInt()) {
            return boxUnsigned(uintFunc(lhs.u, rhs.u));
        } else {
            if (noPreviousErrors(lhs, rhs))
                error(l.module, l, "Couldn't bitwise ", op, " incompatible values '", lhs, "' and '", rhs, "'.");
            return boxError();
        }
    }

    Value eval(Value env, AST ast) {
        switch (ast.kind()) {
            case ASTKind::GlobalConst:
            case ASTKind::Const: {
                Value v = get(env, boxUnsigned(ast.constId()));
                if (v.kind() == Value::Undefined) {
                    error(ast.module, ast, "Tried to access undefined variable const#", ast.constId());
                    return boxError();
                }
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

            case ASTKind::Plus: {
                Value value = eval(env, ast.child(0));
                if (value.isAnyInt() || value.kind() == Value::Float)
                    return value;
                if (noPreviousErrors(value))
                    error(ast.module, ast, "Couldn't apply plus operator to incompatible value '", value, "'.");
                return boxError();
            }
            case ASTKind::Minus: {
                Value value = eval(env, ast.child(0));
                if (value.kind() == Value::Float)
                    return boxFloat(-value.f);
                if (value.isAnyInt())
                    return boxInt(-value.i);
                if (noPreviousErrors(value))
                    error(ast.module, ast, "Couldn't apply minus operator to incompatible value '", value, "'.");
                return boxError();
            }

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
                } else if ((lk == Value::Int || lk == Value::Unsigned)
                           && (rk == Value::Int || rk == Value::Unsigned)) {
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
                } else {
                    if (noPreviousErrors(lhs, rhs))
                        error(ast.module, ast, "Couldn't add incompatible values '", lhs, "' and '", rhs, "'.");
                    return boxError();
                }
            }

            case ASTKind::BitAnd:
                return evalBitwise(env, ast.child(0), ast.child(1), [](i64 a, i64 b) -> i64 { return a & b; }, [](u64 a, u64 b) -> u64 { return a & b; }, "AND");
            case ASTKind::BitOr:
                return evalBitwise(env, ast.child(0), ast.child(1), [](i64 a, i64 b) -> i64 { return a | b; }, [](u64 a, u64 b) -> u64 { return a | b; }, "OR");
            case ASTKind::BitXor:
                return evalBitwise(env, ast.child(0), ast.child(1), [](i64 a, i64 b) -> i64 { return a ^ b; }, [](u64 a, u64 b) -> u64 { return a ^ b; }, "XOR");

            default:
                unreachable("Couldn't evaluate expression ", ast);
        }
    }

    Value call(AST func, slice<Value> values) {
        unreachable("TODO: Implement interpreter.");
    }
}