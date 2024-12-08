#ifndef UTILITY_POINTER_HACKS_H
#define UTILITY_POINTER_HACKS_H

template<typename Ptr, typename Extra>
struct ptr_tuple {
#ifdef RT_48BIT_ADDRESS
    struct Padded {
        Extra extra;
        i8 pad[2 - sizeof(Extra)];
    };

    u64 bits;

    inline void setPtr(Ptr ptr) {
        bits &= 0xffff000000000000ull;
        bits |= bitcast<u64>(ptr);
    }

    inline Ptr getPtr() const {
        return bitcast<Ptr>(bits & 0x0000ffffffffffffull);
    }

    inline void setExtra(Extra extra) {
        bits &= 0x0000ffffffffffffull;
        bits |= (u64)bitcast<u16>(Padded { extra, {} }) << 48;
    }
    
    inline Extra& refExtra() {
#ifdef RT_LITTLE_ENDIAN
        return *bitcast<Extra*>(bitcast<i8*>(&bits));
#else
        return *bitcast<Extra*>(bitcast<i8*>(&bits) + 6);
#endif
    }
    
    inline Extra getExtra() const {
        Padded padded = bitcast<Padded>(u16((bits & 0xffff000000000000ull) >> 48));
        return padded.extra;
    }

    inline void setBoth(Ptr ptr, Extra extra) {
        bits = (bitcast<u64>(ptr) & 0x0000ffffffffffffull) | (u64)bitcast<u16>(Padded { extra, {} }) << 48;
    }
#else
    Ptr ptr;
    Extra extra;

    inline void setPtr(Ptr ptr_in) {
        ptr = ptr_in;
    }

    inline Ptr getPtr() const {
        return ptr;
    }

    inline void setExtra(Extra extra_in) {
        extra = extra_in;
    }
    
    inline Extra& refExtra() {
        return extra;
    }
    
    inline Extra getExtra() const {
        return extra;
    }

    inline void setBoth(Ptr ptr, Extra extra) {
        setPtr(ptr);
        setExtra(extra);
    }
#endif

    inline ptr_tuple(Ptr ptr_in, Extra extra_in) {
        setBoth(ptr_in, extra_in);
    }
};

#endif