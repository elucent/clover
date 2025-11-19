type UTF8:
    const Inner: 0b10000000,
        Two: 0b11000000,
        Three: 0b11100000,
        Four: 0b11110000
    const OneMask: 0b01111111,
        InnerMask: 0b00111111,
        TwoMask: 0b00011111,
        ThreeMask: 0b00001111,
        FourMask: 0b00000111
    const OneMax: 0x7f,
        TwoMax: 0x7ff,
        ThreeMax: 0xffff,
        FourMax: 0x1fffff

i64 encode(UTF8, char[] chars, i8[] bytes):
    var dest: 0
    for i < |chars|:
        var codepoint: chars[i] as u32
        if codepoint < UTF8.OneMax:
            bytes[dest ++] = codepoint as i8
        else if codepoint < UTF8.TwoMax:
            bytes[dest] = i8(codepoint >> 6 & UTF8.TwoMask | UTF8.Two)
            bytes[dest + 1] = i8(codepoint & UTF8.InnerMask | UTF8.Inner)
            dest += 2
        else if codepoint < UTF8.ThreeMax:
            bytes[dest] = i8(codepoint >> 12 & UTF8.ThreeMask | UTF8.Three)
            bytes[dest + 1] = i8(codepoint >> 6 & UTF8.InnerMask | UTF8.Inner)
            bytes[dest + 2] = i8(codepoint & UTF8.InnerMask | UTF8.Inner)
            dest += 3
        else:
            bytes[dest] = i8(codepoint >> 18 & UTF8.FourMask | UTF8.Four)
            bytes[dest + 1] = i8(codepoint >> 12 & UTF8.InnerMask | UTF8.Inner)
            bytes[dest + 2] = i8(codepoint >> 6 & UTF8.InnerMask | UTF8.Inner)
            bytes[dest + 3] = i8(codepoint & UTF8.InnerMask | UTF8.Inner)
            dest += 4
    return dest

i64 decode(UTF8, i8[] bytes, char[] chars):
    var dest: 0, i: 0
    while i < |bytes|:
        var byte: bytes[i] as u8
        if byte < UTF8.Inner:
            chars[dest ++] = byte as char
            i ++
        else if byte < UTF8.Two:
            u32 codepoint: (byte & UTF8.TwoMask) << 6
                | bytes[i + 1] & UTF8.InnerMask
            chars[dest ++] = codepoint as char
            i += 2
        else if byte < UTF8.Three:
            u32 codepoint: (byte & UTF8.ThreeMask) << 12
                | (bytes[i + 1] & UTF8.InnerMask) << 6
                | bytes[i + 2] & UTF8.InnerMask
            chars[dest ++] = codepoint as char
            i += 3
        else if byte < UTF8.Four:
            u32 codepoint: (byte & UTF8.FourMask) << 18
                | (bytes[i + 1] & UTF8.InnerMask) << 12
                | (bytes[i + 2] & UTF8.InnerMask) << 6
                | bytes[i + 3] & UTF8.InnerMask
            chars[dest ++] = codepoint as char
            i += 4
    return dest
