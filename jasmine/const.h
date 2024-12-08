#ifndef JASMINE_CONST_H
#define JASMINE_CONST_H

#include "rt/def.h"
#include "util/hash.h"
#include "util/vec.h"

namespace jasmine {
    struct Constant {
        enum class Kind {
            INT, F32, F64
        };
        union {
            i64 i;
            f32 f;
            f64 d;
        };
        Kind kind;

        inline static Constant Int(i64 i) {
            Constant c;
            c.kind = Kind::INT;
            c.i = i;
            return c;
        }

        inline static Constant F32(f32 f) {
            Constant c;
            c.kind = Kind::F32;
            c.i = 0;
            c.f = f;
            return c;
        }

        inline static Constant F64(f64 d) {
            Constant c;
            c.kind = Kind::F64;
            c.d = d;
            return c;
        }

        inline bool operator==(const Constant& other) const {
            return kind == other.kind && i == other.i;
        }

        inline bool operator!=(const Constant& other) const {
            return kind != other.kind || i != other.i;
        }
    };

    inline u64 hash(const Constant& constant) {
        return ::hash(i64(constant.kind)) * 4303321134630290291ull + ::hash(constant.i);
    }

    struct ConstantTable {
        inline u32 intern(Constant constant) {
            auto it = constants.find(constant);
            if (it != constants.end())
                return it->value;
            else {
                constants.put(constant, constantList.size());
                constantList.push(constant);
                return constantList.size() - 1;
            }
        }

        inline Constant operator[](u32 i) const {
            return constantList[i];
        }

        map<Constant, i32> constants;
        vec<Constant, 16> constantList;
    };
}

#endif