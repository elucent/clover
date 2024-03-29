#include "jasmine/type.h"
#include "jasmine/obj.h"

MODULE(jasmine)

bool Type::operator==(const Type& other) const {
    if (kind != other.kind) return false;
    switch (kind) {
        case TK_ARR:
            return elt == other.elt && nelts == other.nelts;
        case TK_TUP:
            if (len != other.len) return false;
            for (u64 i = 0; i < len; i ++) if (members[i] != other.members[i]) return false;
            return true;
        case TK_FUN:
            if (ret != other.ret || nargs != other.nargs) return false;
            for (u64 i = 0; i < nargs; i ++) if (members[i] != other.members[i]) return false;
            return true;
        case TK_VEC:
            return velt == other.velt && nvelts == other.nvelts;
    }
}

void format_type(const TypeTable& table, typeidx t, fd io) {
    if (t < 0) {
        switch (i8(t)) {
            case T_I8: return write(io, "i8");
            case T_I16: return write(io, "i16");
            case T_I32: return write(io, "i32");
            case T_I64: return write(io, "i64");
            case T_IWORD: return write(io, "iword");
            case T_PTR: return write(io, "ptr");
            case T_F32: return write(io, "f32");
            case T_F64: return write(io, "f64");
            case T_U8: return write(io, "u8");
            case T_U16: return write(io, "u16");
            case T_U32: return write(io, "u32");
            case T_U64: return write(io, "u64");
            case T_UWORD: return write(io, "uword");
            case T_REF: return write(io, "ref");
            case T_VOID: return write(io, "void");
            default: return;
        }
    }

    const Type& ut = table.types[t];
    bool first = true;
    switch (ut.kind) {
        case TK_ARR:
            format_type(table, ut.elt, io);
            write(io, '[', ut.nelts, ']');
            break;
        case TK_FUN:
            format_type(table, ut.ret, io);
            write(io, '(');
            for (typeidx arg : ut.members) {
                if (!first) write(io, ", ");
                first = false;
                format_type(table, arg, io);
            }
            write(io, ')');
            break;
        case TK_TUP:
            write(io, '{');
            for (typeidx arg : ut.members) {
                if (!first) write(io, ", ");
                first = false;
                format_type(table, arg, io);
            }
            write(io, '}');
            break;
        case TK_VEC:
            write(io, '<');
            format_type(table, ut.velt, io);
            write(io, " x ", ut.nvelts, '>');
            break;
        default:
            break;
    }
}

TypeTable::TypeTable(JasmineModule* obj_in): obj(obj_in) {}

u32 TypeTable::size() const {
    return 8 + 8 * types.size();
}

void TypeTable::write(bytebuf<>& buf) const {
    buf.writeULEB(types.size());

    for (u64 i = 0; i < types.size();) {
        const Type& t = types[i ++];
        u32 n_exts = 0;
        buf.write<u8>(t.kind);
        switch (t.kind) {
            case TK_ARR:
                buf.writeLEB(t.elt);
                buf.writeULEB(t.nelts);
                break;
            case TK_FUN:
                buf.writeLEB(t.ret);
                buf.writeULEB(t.nargs);
                for (const auto& t : t.members) buf.writeLEB(t);
                break;
            case TK_TUP:
                buf.writeULEB(t.len);
                for (const auto& t : t.members) buf.writeLEB(t);
                break;
            case TK_VEC:
                buf.writeLEB(t.velt);
                buf.writeULEB(t.nvelts);
                break;
            default: break;
        }
    }
}

void TypeTable::read(bytebuf<>& buf) {
    u64 n = buf.readULEB(); // Number of types.
    while (types.size() < n) {
        Type t;
        u32 n_exts = 0;
        t.kind = (TypeKind)buf.read<u8>();
        switch (t.kind) {
            case TK_ARR:
                t.elt = buf.readLEB();
                t.nelts = buf.readULEB();
                try_insert(*this, t);
                break;
            case TK_TUP:
                t.len = buf.readULEB();
                for (u32 i = 0; i < t.len; i ++) t.members.push(buf.readLEB());
                try_insert(*this, t);
                break;
            case TK_FUN:
                t.ret = buf.readLEB();
                t.nargs = buf.readULEB();
                for (u32 i = 0; i < t.nargs; i ++) t.members.push(buf.readLEB());
                try_insert(*this, t);
                break;
            case TK_VEC:
                t.velt = buf.readLEB();
                t.nvelts = buf.readULEB();
                try_insert(*this, t);
                break;
            default:
                fatal("Unknown type kind.");
                break;
        }
    }
}

void TypeTable::format(fd io) const {
    ::write(io, " === Type Table === \n");
    for (i32 i = 0; i < types.size(); i ++) {
        ::write(io, "  "), ::write_hex(io, i), ::write(io, ": "), format_type(*this, i, io), ::write(io, '\n');
    }
    ::write(io, '\n');
}

ENDMODULE()
