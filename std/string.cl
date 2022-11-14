module noalloc:
    module utf8:
        const INNER: 0b10000000
        const TWO: 0b11000000
        const THREE: 0b11100000
        const FOUR: 0b11110000

        const INNER_MASK: 0b00111111
        const TWO_MASK: 0b00011111
        const THREE_MASK: 0b00001111
        const FOUR_MASK: 0b00000111
        
        const ONE_MAX: 0x7f
        const TWO_MAX: 0x7ff
        const THREE_MAX: 0xffff
        const FOUR_MAX: 0x1fffff

        int length(i8[] bytes):
            var idx: 0, numChars: 0
            while idx < |bytes|:
                var front: bytes[idx].u16()
                if front < INNER:
                    idx ++
                    numChars ++
                else if front < THREE:
                    if idx + 1 >= |bytes|:
                        break
                    idx += 2
                    numChars ++
                else if front < FOUR:
                    if idx + 2 >= |bytes|:
                        break
                    idx += 3
                    numChars ++
                else:
                    if idx + 3 >= |bytes|:
                        break
                    idx += 4
                    numChars ++
            return numChars

        type DecodeResult:
            case Chars: 
                char[] chars
                int bytesDecoded
            case OutOfBounds:
                int bytesDecoded
        
        type EncodeResult:
            case Bytes:
                i8[] bytes
                int charsDecoded
            case InvalidEncoding:
                int charsDecoded

        DecodeResult decode(i8[] bytes, char[] out):
            var read: 0, decoded: 0
            while read < |bytes| and decoded < |out|:
                var front: bytes[read].u16()
                if front < INNER:
                    out[decoded ++] = char(front)
                    read ++
                else if front < THREE:
                    if read + 1 >= |bytes|:
                        return DecodeResult.OutOfBounds(read)
                    out[decoded ++] = char((front & TWO_MASK) << 6 
                                           | bytes[read + 1] & INNER_MASK)
                    read += 2
                else if front < FOUR:
                    if read + 2 >= |bytes|:
                        return DecodeResult.OutOfBounds(read)
                    out[decoded ++] = char((front & THREE_MASK) << 12 
                                           | (bytes[read + 1] & INNER_MASK) << 6 
                                           | bytes[read + 2] & INNER_MASK)
                    read += 3
                else:
                    if read + 3 >= |bytes|:
                        return DecodeResult.OutOfBounds(read)
                    out[decoded ++] = char((front & FOUR_MASK) << 18 
                                           | (bytes[read + 1] & INNER_MASK) << 12 
                                           | (bytes[read + 2] & INNER_MASK) << 6 
                                           | bytes[read + 3] & INNER_MASK)
                    read += 4
            return DecodeResult.Chars(out[:decoded], read)

        EncodeResult encode(char[] chars, i8[] out):
            var read: 0, encoded: 0
            while read < |chars| and encoded < |out|:
                var front: chars[read]
                if i32(front) < ONE_MAX:
                    out[encoded ++] = i8(front)
                    read ++
                else if i32(front) < TWO_MAX:
                    if encoded + 1 >= |out|:
                        break
                    out[encoded ++] = i8(i32(front) >> 6 & TWO_MASK | TWO)
                    out[encoded ++] = i8(i32(front) & INNER_MASK | INNER)
                    read ++
                else if i32(front) < THREE_MAX:
                    if encoded + 2 >= |out|:
                        break
                    out[encoded ++] = i8(i32(front) >> 12 & THREE_MASK | THREE)
                    out[encoded ++] = i8(i32(front) >> 6 & INNER_MASK | INNER)
                    out[encoded ++] = i8(i32(front) & INNER_MASK | INNER)
                    read ++
                else if i32(front) < THREE_MAX:
                    if encoded + 3 >= |out|:
                        break
                    out[encoded ++] = i8(i32(front) >> 18 & FOUR_MASK | FOUR)
                    out[encoded ++] = i8(i32(front) >> 12 & INNER_MASK | INNER)
                    out[encoded ++] = i8(i32(front) >> 6 & INNER_MASK | INNER)
                    out[encoded ++] = i8(i32(front) & INNER_MASK | INNER)
                    read ++
                else:
                    return EncodeResult.InvalidEncoding(read)
            return EncodeResult.Bytes(out[:encoded], read)

    type StringIterator:
        string str

        bool empty(): 
            |str| == 0
        
        char read():
            var bytes: i8[](str)
            if bytes[0].u16() < utf8.INNER:
                char(bytes[0])
            else if bytes[0].u16() < utf8.THREE:
                char((bytes[0] & utf8.TWO_MASK) << 6 | bytes[1] & utf8.INNER_MASK)
            else if bytes[0].u16() < utf8.FOUR:
                char((bytes[0] & utf8.THREE_MASK) << 12 | (bytes[1] & utf8.INNER_MASK) << 6 | bytes[2] & utf8.INNER_MASK)
            else:
                char((bytes[0] & utf8.FOUR_MASK) << 18 | (bytes[1] & utf8.INNER_MASK) << 12 | (bytes[2] & utf8.INNER_MASK) << 6 | bytes[3] & utf8.INNER_MASK)

        StringIterator next():
            var bytes: i8[](str)
            if bytes[0].u16() < utf8.INNER:
                StringIterator(string(bytes[1:]))
            else if bytes[0].u16() < utf8.THREE:
                StringIterator(string(bytes[2:]))
            else if bytes[0].u16() < utf8.FOUR:
                StringIterator(string(bytes[3:]))
            else:
                StringIterator(string(bytes[4:]))
        
        StringIterator string.iter():
            StringIterator(this)
    
    char string.first():
        this.iter().read()
    
    char string.last():
        var idx: |this| - 1
        var bytes: i8[](this)
        while bytes[idx].u16() >= utf8.INNER and bytes[idx].u16() < utf8.TWO and idx >= 0:
            idx --
        string(bytes[idx:]).first()

    int string.findc(char ch):
        int i: 0
        for c in this:
            if c == ch:
                return i
            i ++
        return -1

    int string.length(): 
        utf8.length(i8[](this))

    type StringSplitter:
        string src, prev, seq
        
        StringSplitter iter(): this
        bool empty(): |prev| == 0
        string read(): prev
                
        StringSplitter next():
            if |seq| == 0:
                return StringSplitter(src, "", seq)
            var srcIterOriginal: src.iter()
            var srcIter: srcIterOriginal, seqIter: seq.iter()
            until srcIter.empty():
                if srcIter.read() == seqIter.read():
                    seqIter = seqIter.next()
                    if seqIter.empty():
                        var bytes: i8[](src)
                        var length: |src| - |srcIterOriginal.str|
                        var result: bytes[:length]
                        return StringSplitter(srcIter.next().str, string(result), seq)
                    else:
                        srcIter = srcIter.next()
                else:
                    seqIter = seq.iter()
                    srcIter = srcIter.next()
                    srcIterOriginal = srcIter
            StringSplitter("", src, seq)

    StringSplitter string.split(string seq):
        StringSplitter(this, "", seq).next()

    type StringFinder:
        string src, seq
        int prev
        
        StringFinder iter(): this
        bool empty(): |src| == 0
        int read(): prev
                
        StringFinder next():
            if |seq| == 0:
                return StringFinder(src, seq, 0)
            var srcIter: src.iter(), seqIter: seq.iter()
            var idx: 0
            if prev != -1:
                idx = prev + seq.length()
            until srcIter.empty():
                idx ++
                if srcIter.read() == seqIter.read():
                    seqIter = seqIter.next()
                    if seqIter.empty():
                        return StringFinder(srcIter.next().str, seq, idx - seq.length())
                else:
                    seqIter = seq.iter()
                srcIter = srcIter.next()
            StringFinder("", seq, -1)

    StringFinder string.find(string seq):
        StringFinder(this, seq, -1).next()

use noalloc

char[] string.chars():
    var chars: new '\0'[this.length()]
    var i: 0
    for ch in this:
        chars[i ++] = ch
    return chars

string string.append(string s):
    var len: |this| + |s|
    var buf: new i8(0)[len]
    var idx: 0
    for c in i8[](this):
        buf[idx ++] = c
    for c in i8[](s):
        buf[idx ++] = c
    return string(buf)

string string.repeat(int times):
    if times == 1:
        return this
    var len: |this| * times
    if len == 0:
        return ""
    var buf: new i8(0)[len]
    var loop: 0
    var idx: 0
    while loop < times:
        for c in i8[](this):
            buf[idx ++] = c
        loop ++
    return string(buf)

string T?.join(string joiner):
    var len: 0
    var loop: 0
    for s in this:
        if loop > 0:
            len += |joiner|
        len += |s|
        loop ++
    var buf: new i8(0)[len]
    var idx: 0
    loop = 0
    for s in this:
        if loop > 0:
            for c in i8[](joiner):
                buf[idx ++] = c
        for c in i8[](s):
            buf[idx ++] = c
        loop ++
    return string(buf)

string string.replace(string old, string replacement):
    this.split(old).join(replacement)

string string.remove(string toRemove):
    this.replace(toRemove, "")