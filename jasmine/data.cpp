#include "jasmine/data.h"
#include "jasmine/obj.h"

u32 DVal::size(TypeTable* tab, typeidx t) const {
    if (t < 0) switch (i8(t)) {
        case T_I8: 
        case T_U8: 
            return 1;
        case T_I16: 
        case T_U16: 
            return 2;
        case T_I32: 
        case T_U32: 
        case T_F32:
            return 4;
        case T_I64: 
        case T_U64: 
        case T_F64:
            return 8;
        case T_IWORD: 
        case T_UWORD: 
            return sizeof(iword);
        case T_PTR: 
        case T_REF: 
            return sizeof(iptr);
        case T_VOID:
        default:
            unreachable("Invalid data type.");
            return 0;
    }
    const Type& type = tab->types[t];
    if (type.kind == TK_ARR) {
        if (t < 0) switch (i8(type.elt)) {
            case T_I8: 
            case T_U8:
                return type.nelts;
            case T_I16: 
            case T_U16:
                return type.nelts * 2;
            case T_I32: 
            case T_U32:
            case T_F32:
                return type.nelts * 4;
            case T_I64: 
            case T_U64:
            case T_F64:
                return type.nelts * 8;
            case T_IWORD:
            case T_UWORD:
                return type.nelts * sizeof(iword);
            case T_PTR:
            case T_REF:
                return type.nelts * sizeof(iptr);
            case T_VOID:
            default:
                unreachable("Invalid data type.");
                return 0;
        }
        else return fields[0].size(tab, t) * type.nelts;
    }
    else if (type.kind == TK_TUP) {
        u32 sum = 0;
        for (u32 i = 0; i < type.len; i ++) sum += fields[i].size(tab, t);
        return sum;
    }
    else {
        unreachable("Functions are not permitted in the data/static sections.");
        return 0;
    }
}

void DVal::write(TypeTable* tab, typeidx t, bytebuf<arena>& buf) const {
    u32 prev = buf.size();
    if (t < 0) switch (i8(t)) {
        case T_I8: return buf.writeLEB(int8);
        case T_I16: return buf.writeLEB(int16);
        case T_I32: return buf.writeLEB(int32);
        case T_I64: return buf.writeLEB(int64);
        case T_IWORD: return buf.writeLEB(iw);
        case T_PTR: return buf.writeULEB(ptr);
        case T_F32: return buf.writeLE<float>(f32);
        case T_F64: return buf.writeLE<double>(f64);
        case T_U8: return buf.writeULEB(uint8);
        case T_U16: return buf.writeULEB(uint16);
        case T_U32: return buf.writeULEB(uint32);
        case T_U64: return buf.writeULEB(uint64);
        case T_UWORD: return buf.writeULEB(uw);
        case T_REF: return buf.writeULEB(ref);
        case T_VOID:
            unreachable("Invalid data type.");
            return;
    }
    const Type& type = tab->types[t];
    if (type.kind == TK_ARR) {
        if (type.elt < 0) switch (i8(type.elt)) {
            case T_I8: for (auto int8 : arr_i8) buf.writeLEB(int8); return;
            case T_I16: for (auto int16 : arr_i16) buf.writeLEB(int16); return;
            case T_I32: for (auto int32 : arr_i32) buf.writeLEB(int32); return;
            case T_I64: for (auto int64 : arr_i64) buf.writeLEB(int64); return;
            case T_IWORD: for (auto iw : arr_iword) buf.writeLEB(iw); return;
            case T_PTR: for (auto ptr : arr_ptr) buf.writeULEB(ptr); return;
            case T_F32: for (auto f32 : arr_f32) buf.writeLE<float>(f32); return;
            case T_F64: for (auto f64 : arr_f64) buf.writeLE<double>(f64); return;
            case T_U8: for (auto uint8 : arr_u8) buf.writeULEB(uint8); return;
            case T_U16: for (auto uint16 : arr_u16) buf.writeULEB(uint16); return;
            case T_U32: for (auto uint32 : arr_u32) buf.writeULEB(uint32); return;
            case T_U64: for (auto uint64 : arr_u64) buf.writeULEB(uint64); return;
            case T_UWORD: for (auto uw : arr_uword) buf.writeULEB(uw); return;
            case T_REF: for (auto ref : arr_ref) buf.writeULEB(ref); return;
            case T_VOID:
                unreachable("Invalid data type.");
                return;
        }
        else for (const DVal& d : fields) d.write(tab, type.elt, buf);
    }
    else if (type.kind == TK_TUP) for (u32 i = 0; i < type.len; i ++) {
        fields[i].write(tab, type.members[i], buf);
    }
    else unreachable("Functions are not permitted in the data/static sections.");
}

void DVal::read(TypeTable* tab, typeidx t, bytebuf<arena>& buf) {
    u32 prev = buf.size();
    if (t < 0) switch (i8(t)) {
        case T_I8: int8 = buf.readLEB(); return;
        case T_I16: int16 = buf.readLEB(); return;
        case T_I32: int32 = buf.readLEB(); return;
        case T_I64: int64 = buf.readLEB(); return;
        case T_IWORD: iw = buf.readLEB(); return;
        case T_PTR: ptr = buf.readULEB(); return;
        case T_F32: f32 = buf.readLE<float>(); return;
        case T_F64: f64 = buf.readLE<double>(); return;
        case T_U8: uint8 = buf.readULEB(); return;
        case T_U16: uint16 = buf.readULEB(); return;
        case T_U32: uint32 = buf.readULEB(); return;
        case T_U64: uint64 = buf.readULEB(); return;
        case T_UWORD: uw = buf.readULEB(); return;
        case T_REF: ref = buf.readULEB(); return;
        case T_VOID:
            unreachable("Invalid data type.");
            return;
    }
    const Type& type = tab->types[t];
    if (type.kind == TK_ARR) {
        if (type.elt < 0) switch (i8(type.elt)) {
            case T_I8: arr_i8 = {new (tab->obj->modspace) i8[type.nelts], (iptr)type.nelts}; for (auto& int8 : arr_i8) int8 = buf.readLEB(); return;
            case T_I16: arr_i16 = {new (tab->obj->modspace) i16[type.nelts], (iptr)type.nelts}; for (auto& int16 : arr_i16) int16 = buf.readLEB(); return;
            case T_I32: arr_i32 = {new (tab->obj->modspace) i32[type.nelts], (iptr)type.nelts}; for (auto& int32 : arr_i32) int32 = buf.readLEB(); return;
            case T_I64: arr_i64 = {new (tab->obj->modspace) i64[type.nelts], (iptr)type.nelts}; for (auto& int64 : arr_i64) int64 = buf.readLEB(); return;
            case T_IWORD: arr_iword = {new (tab->obj->modspace) iword[type.nelts], (iptr)type.nelts}; for (auto& iw : arr_iword) iw = buf.readLEB(); return;
            case T_PTR: arr_ptr = {new (tab->obj->modspace) iptr[type.nelts], (iptr)type.nelts}; for (auto& ptr : arr_ptr) ptr = buf.readULEB(); return;
            case T_F32: arr_f32 = {new (tab->obj->modspace) float[type.nelts], (iptr)type.nelts}; for (auto& f32 : arr_f32) f32 = buf.readLE<float>(); return;
            case T_F64: arr_f64 = {new (tab->obj->modspace) double[type.nelts], (iptr)type.nelts}; for (auto& f64 : arr_f64) f64 = buf.readLE<double>(); return;
            case T_U8: arr_u8 = {new (tab->obj->modspace) u8[type.nelts], (iptr)type.nelts}; for (auto& uint8 : arr_u8) uint8 = buf.readULEB(); return;
            case T_U16: arr_u16 = {new (tab->obj->modspace) u16[type.nelts], (iptr)type.nelts}; for (auto& uint16 : arr_u16) uint16 = buf.readULEB(); return;
            case T_U32: arr_u32 = {new (tab->obj->modspace) u32[type.nelts], (iptr)type.nelts}; for (auto& uint32 : arr_u32) uint32 = buf.readULEB(); return;
            case T_U64: arr_u64 = {new (tab->obj->modspace) u64[type.nelts], (iptr)type.nelts}; for (auto& uint64 : arr_u64) uint64 = buf.readULEB(); return;
            case T_UWORD: arr_uword = {new (tab->obj->modspace) uword[type.nelts], (iptr)type.nelts}; for (auto& uw : arr_uword) uw = buf.readULEB(); return;
            case T_REF: arr_ref = {new (tab->obj->modspace) iptr[type.nelts], (iptr)type.nelts}; for (auto& ref : arr_ref) ref = buf.readULEB(); return;
            case T_VOID:
                unreachable("Invalid data type.");
                return;
        }
        else {
            fields = {new (tab->obj->modspace) DVal[type.nelts], (iptr)type.nelts};
            for (DVal& d : fields) d.read(tab, type.elt, buf);
        }
    }
    else if (type.kind == TK_TUP) {
        fields = {new (tab->obj->modspace) DVal[type.len], (iptr)type.len};
        for (u32 i = 0; i < type.len; i ++) fields[i].read(tab, type.members[i], buf);
    }
    else unreachable("Functions are not permitted in the data/static sections.");
}

void DVal::format(stream &io, TypeTable* tab, typeidx t) const {
    if (t < 0) switch (i8(t)) {
        case T_I8: return ::write(io, int8);
        case T_I16: return ::write(io, int16);
        case T_I32: return ::write(io, int32);
        case T_I64: return ::write(io, int64);
        case T_IWORD: return ::write(io, iw);
        case T_PTR: return ::write(io, "0x"), ::write_hex(io, ref);
        case T_F32: return ::write(io, f32);
        case T_F64: return ::write(io, f64);
        case T_U8: return ::write(io, uint8);
        case T_U16: return ::write(io, uint16);
        case T_U32: return ::write(io, uint32);
        case T_U64: return ::write(io, uint64);
        case T_UWORD: return ::write(io, uw);
        case T_REF: return ::write(io, "0x"), ::write_hex(io, ref);
        case T_VOID:
            unreachable("Invalid data type.");
            return;
    }
    const Type& type = tab->types[t];
    if (type.kind == TK_ARR) {
        if (type.elt == T_I8) return ::write(io, "\"", (const_slice<i8>)arr_i8, "\"");
        else if (type.elt == T_U8) return ::write(io, "\"", const_slice<i8>{(const i8*)arr_u8.ptr, arr_u8.n}, "\"");
        else {
            ::write(io, '[');
            bool first = true;
            if (type.elt < 0) switch (i8(type.elt)) {
                case T_I16: 
                    for (auto int16 : arr_i16) { if (!first) ::write(io, ", "); first = false; ::write(io, int16); } break;
                case T_I32: 
                    for (auto int32 : arr_i32) { if (!first) ::write(io, ", "); first = false; ::write(io, int32); } break;
                case T_I64: 
                    for (auto int64 : arr_i64) { if (!first) ::write(io, ", "); first = false; ::write(io, int64); } break;
                case T_IWORD: 
                    for (auto iw : arr_iword) { if (!first) ::write(io, ", "); first = false; ::write(io, iw); } break;
                case T_PTR: 
                    for (auto ptr : arr_ptr) { if (!first) ::write(io, ", "); first = false; ::write(io, "0x"), ::write_hex(io, ptr), ::write(io); } break;
                case T_F32: 
                    for (auto f32 : arr_f32) { if (!first) ::write(io, ", "); first = false; ::write(io, f32); } break;
                case T_F64: 
                    for (auto f64 : arr_f64) { if (!first) ::write(io, ", "); first = false; ::write(io, f64); } break;
                case T_U8: 
                    for (auto uint8 : arr_u8) { if (!first) ::write(io, ", "); first = false; ::write(io, uint8); } break;
                case T_U16: 
                    for (auto uint16 : arr_u16) { if (!first) ::write(io, ", "); first = false; ::write(io, uint16); } break;
                case T_U32: 
                    for (auto uint32 : arr_u32) { if (!first) ::write(io, ", "); first = false; ::write(io, uint32); } break;
                case T_U64: 
                    for (auto uint64 : arr_u64) { if (!first) ::write(io, ", "); first = false; ::write(io, uint64); } break;
                case T_UWORD: 
                    for (auto uw : arr_uword) { if (!first) ::write(io, ", "); first = false; ::write(io, uw); } break;
                case T_REF: 
                    for (auto ref : arr_ref) { if (!first) ::write(io, ", "); first = false; ::write(io, "0x"), ::write_hex(io, ref), ::write(io); } break;
                case T_VOID:
                    unreachable("Invalid data type.");
                    return;
            }
            else for (const DVal& d : fields) {
                if (!first) ::write(io, ", ");
                first = false;
                d.format(io, tab, type.elt);
            }
            ::write(io, ']'); 
            return;
        }
    }
    else if (type.kind == TK_TUP) {
        ::write(io, '{');
        bool first = true;
        u32 i = 0;
        for (const DVal& d : fields) {
            if (!first) ::write(io, ", ");
            first = false;
            d.format(io, tab, type.members[i ++]);
        }
        ::write(io, '}');
    }
    else unreachable("Functions are not permitted in the data/static sections.");
}

GlobalTable::GlobalTable(JasmineModule* obj_in): obj(obj_in) {
    entries.alloc = &obj->modspace;
}

void GlobalTable::def(const GlobalEntry& e) {
    entries.push(e);
    entrymap.put(obj->strings.strings[e.name], e);
}

TypeTable* GlobalTable::ttable() {
    return &obj->types;
}

DataTable::DataTable(JasmineModule* obj_in): GlobalTable(obj_in) {}

StaticTable::StaticTable(JasmineModule* obj_in): GlobalTable(obj_in) {}

void GlobalTable::write(bytebuf<arena>& buf) const {
    buf.writeULEB(entries.size());
    for (const auto& e : entries) {
        buf.writeLEB(e.name);
        buf.writeLEB(e.type);
        e.dat.write(&obj->types, e.type, buf);
    }
}

void GlobalTable::read(bytebuf<arena>& buf) {
    u32 prev = buf.size();
    u32 nentries = buf.readULEB();
    for (u32 i = 0; i < nentries; i ++) {
        GlobalEntry e;
        e.name = buf.readLEB();
        e.type = buf.readLEB();
        e.dat.read(&obj->types, e.type, buf);
        entries.push(e);
    }
}

void DataTable::format(stream& io) const {
    ::write(io, " === Data Table ===\n");
    u32 i = 0;
    for (const auto& entry : entries) {
        ::write(io, "  ");
        ::write_hex(io, i ++);
        ::write(io, ": ");
        entry.dat.format(io, &obj->types, entry.type);
        ::write(io, " (");
        format_type(obj->types, entry.type, io);
        ::write(io, ' ', obj->strings.strings[entry.name], ")\n");
    }
    ::write(io, '\n');
}

void StaticTable::format(stream& io) const {
    ::write(io, " === Static Table ===\n");
    u32 i = 0;
    for (const auto& entry : entries) {
        ::write(io, "  ");
        ::write_hex(io, i ++);
        ::write(io, ": ");
        entry.dat.format(io, &obj->types, entry.type);
        ::write(io, " (");
        format_type(obj->types, entry.type, io);
        ::write(io, ' ', obj->strings.strings[entry.name], ")\n");
    }
    ::write(io, '\n');
}