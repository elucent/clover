#include "clover/lex.h"
#include "util/config.h"

namespace clover {
    Tokens::Tokens(const_slice<i8> source_in):
        source(source_in) {
        lineOffsets.push(0);
    }

    Tokens::~Tokens() {
        if (source.data())
            delete[] source.data();
    }

    maybe<const_slice<i8>> Tokens::getSource() {
        return some<const_slice<i8>>(source);
    }

    maybe<const_slice<u32>> Tokens::getLineOffsets() {
        return some<const_slice<u32>>(lineOffsets);
    }

    const_slice<i8> Tokens::takeSource() {
        auto taken = source;
        source = { nullptr, 0 };
        return taken;
    }

    void Tokens::print(Compilation* compilation) {
        bool firstOnLine = true;
        for (const Token& token : tokens) {
            if (!firstOnLine)
                ::print(", ");
            ::print(compilation->str(token.token), ':', token.pos.line, ':', token.pos.column);
            if (token.token == WhitespaceNewline)
                println(), firstOnLine = true;
            else
                firstOnLine = false;
        }
        if (!firstOnLine)
            println();
    }

    u64 Tokens::reportSize() {
        return source.size() + tokens.size() * sizeof(Token) + lineOffsets.size() * sizeof(u32);
    }

    struct SourceVisitor {
        u32 line, column, offset, size;
        const i8* bytes;
        vec<u32>& lineOffsets;
        inline SourceVisitor(const_slice<i8> source, vec<u32>& lineOffsets_in):
            line(0), column(0), offset(0), size(source.size()), bytes(source.data()), lineOffsets(lineOffsets_in) {}

        inline u8 peek() {
            if UNLIKELY(done())
                return 0;
            return bitcast<u8>(bytes[offset]);
        }

        inline u8 read() {
            if UNLIKELY(done())
                return 0;
            u8 result = bitcast<u8>(bytes[offset ++]);
            if UNLIKELY(result == '\n')
                column = 0, line ++, lineOffsets.push(offset);
            else if UNLIKELY(result == '\t')
                column = roundUpToNearest<u32>(column + 1, 4);
            else
                column ++;
            return result;
        }

        inline rune peekUTF8() {
            u8 head = peek();
            u8 len = 1;
            if (head > UTF8_TWO)
                len = 2;
            if (head > UTF8_THREE)
                len = 3;
            if (head > UTF8_FOUR)
                len = 4;
            if UNLIKELY(offset + len >= size) {
                offset = size;
                return 0;
            }
            rune result;
            utf8_decode_forward(bytes + offset, &result);
            return result;
        }

        inline u32 cursor() {
            return offset;
        }

        inline void advance(rune r) {
            offset += r.bytes();
            column ++;
        }

        inline void readPossiblyUTF8() {
            if UNLIKELY(peek() >= 128)
                advance(peekUTF8());
            else
                read();
        }

        inline const_slice<i8> sliceFrom(u32 start) {
            return { bytes + start, offset - start };
        }

        inline Pos pos() {
            assert(line <= Pos::MaximumLine);
            assert(column <= Pos::MaximumColumn);

            return { line, column };
        }

        inline bool done() const {
            return offset >= size;
        }
    };

    inline bool isDelimiter(u8 ch) {
        constexpr u64 delimiterMask1 = 0
            | 1ull << ' '
            | 1ull << '\t'
            | 1ull << '\n'
            | 1ull << '('
            | 1ull << ')'
            | 1ull << '.'
            | 1ull << ','
            | 1ull << ':'
            | 1ull << ';';
        constexpr u64 delimiterMask2 = 0
            | 1ull << ('[' - 64)
            | 1ull << (']' - 64)
            | 1ull << ('{' - 64)
            | 1ull << ('}' - 64);
        return ch < 64 ? (delimiterMask1 & 1ull << ch) : (delimiterMask2 & 1ull << (ch - 64));
    }

    inline bool isDigit(SourceVisitor& visitor) {
        u8 next = visitor.peek();
        if LIKELY(next < 128) {
            constexpr u64 digitMask = 0
                | 1ull << '0'
                | 1ull << '1'
                | 1ull << '2'
                | 1ull << '3'
                | 1ull << '4'
                | 1ull << '5'
                | 1ull << '6'
                | 1ull << '7'
                | 1ull << '8'
                | 1ull << '9';
            return next < 64 && (digitMask & 1ull << next);
        } else {
            rune r = visitor.peekUTF8();
            return utf8_is_digit(next);
        }
    }

    inline bool isValidInIdentifier(SourceVisitor& visitor) {
        u8 next = visitor.peek();
        if LIKELY(next < 128) {
            constexpr u64 identifierMask1 = 0
                | 1ull << '!'
                | 1ull << '0'
                | 1ull << '1'
                | 1ull << '2'
                | 1ull << '3'
                | 1ull << '4'
                | 1ull << '5'
                | 1ull << '6'
                | 1ull << '7'
                | 1ull << '8'
                | 1ull << '9'
                | 1ull << '?';
            constexpr u64 identifierMask2 = 0
                | 1ull << ('A' - 64) | 1ull << ('B' - 64) | 1ull << ('C' - 64) | 1ull << ('D' - 64) | 1ull << ('E' - 64) | 1ull << ('F' - 64) | 1ull << ('G' - 64)
                | 1ull << ('H' - 64) | 1ull << ('I' - 64) | 1ull << ('J' - 64) | 1ull << ('K' - 64) | 1ull << ('L' - 64) | 1ull << ('M' - 64) | 1ull << ('N' - 64)
                | 1ull << ('O' - 64) | 1ull << ('P' - 64) | 1ull << ('Q' - 64) | 1ull << ('R' - 64) | 1ull << ('S' - 64) | 1ull << ('T' - 64) | 1ull << ('U' - 64)
                | 1ull << ('V' - 64) | 1ull << ('W' - 64) | 1ull << ('X' - 64) | 1ull << ('Y' - 64) | 1ull << ('Z' - 64) | 1ull << ('_' - 64) | 1ull << ('a' - 64)
                | 1ull << ('b' - 64) | 1ull << ('c' - 64) | 1ull << ('d' - 64) | 1ull << ('e' - 64) | 1ull << ('f' - 64) | 1ull << ('g' - 64) | 1ull << ('h' - 64)
                | 1ull << ('i' - 64) | 1ull << ('j' - 64) | 1ull << ('k' - 64) | 1ull << ('l' - 64) | 1ull << ('m' - 64) | 1ull << ('n' - 64) | 1ull << ('o' - 64)
                | 1ull << ('p' - 64) | 1ull << ('q' - 64) | 1ull << ('r' - 64) | 1ull << ('s' - 64) | 1ull << ('t' - 64) | 1ull << ('u' - 64) | 1ull << ('v' - 64)
                | 1ull << ('w' - 64) | 1ull << ('x' - 64) | 1ull << ('y' - 64) | 1ull << ('z' - 64);
            if (next < 64)
                return identifierMask1 & 1ull << next;
            else
                return identifierMask2 & 1ull << (next - 64);
        } else {
            rune r = visitor.peekUTF8();
            return utf8_is_letter(r)
                || utf8_is_letter_number(r)
                || utf8_is_digit(r)
                || utf8_is_nonspacing_mark(r)
                || utf8_is_spacing_combining_mark(r)
                || utf8_is_connector(r);
        }
    }

    inline void lexIdentifier(vec<Token, 32>& tokens, SymbolTable& symbols, SourceVisitor& visitor, u32 start, Pos pos) {
        bool sawTerminalMark = false;
        while (isValidInIdentifier(visitor)) {
            if UNLIKELY(sawTerminalMark && visitor.peek() != '?' && visitor.peek() != '!')
                unreachable("Question or exclamation mark cannot be followed by other characters in the same identifier.");
            if UNLIKELY(visitor.peek() == '?' || visitor.peek() == '!')
                sawTerminalMark = true;
            visitor.readPossiblyUTF8();
        }
        tokens.push({ symbols[visitor.sliceFrom(start)], pos });
    }

    inline void lexNumber(vec<Token, 32>& tokens, SymbolTable& symbols, SourceVisitor& visitor, u32 start, Pos pos) {
        while (isDigit(visitor))
            visitor.readPossiblyUTF8();
        if (visitor.peek() == '.') {
            const_slice<i8> number = visitor.sliceFrom(start);
            Pos preDot = visitor.pos();
            visitor.read();
            bool digitFollowingDot = false;
            while (isDigit(visitor)) {
                digitFollowingDot = true;
                visitor.readPossiblyUTF8();
            }
            if (!digitFollowingDot) {
                tokens.push({ symbols[number], pos });
                tokens.push({ PunctuatorDot, preDot });
                return;
            }
        }
        tokens.push({ symbols[visitor.sliceFrom(start)], pos });
    }

    constexpr u64 spaceMask = 1ull << ' ' | 1ull << '\t' | 1ull << '\r';

    inline void processIndentsOnNewLine(vec<Token, 32>& tokens, vec<u32, 16>& indents, SourceVisitor& visitor) {
        assert(visitor.column == 0);
        u8 ch = visitor.peek();
        while (!visitor.done() && ch < 64 && spaceMask & (1ull << ch))
            visitor.read(), ch = visitor.peek();
        if (visitor.column > indents.back()) {
            tokens.push({ WhitespaceIndent, visitor.pos() });
            indents.push(visitor.column);
        } else if (visitor.column < indents.back()) {
            if (visitor.column <= indents[indents.size() - 2]) {
                tokens.push({ WhitespaceDedent, visitor.pos() });
                indents.pop();
                while (visitor.column < indents.back()) {
                    tokens.push({ WhitespaceDedent, visitor.pos() });
                    indents.pop();
                }
            }
        }
    }

    void lex(vec<Token, 32>& tokens, vec<u32, 16>& indents, SymbolTable& symbols, SourceVisitor& visitor) {
        processIndentsOnNewLine(tokens, indents, visitor);
        while (!visitor.done()) {
            u8 ch = visitor.peek();
            while (ch < 64 && spaceMask & (1ull << ch))
                visitor.read(), ch = visitor.peek();

            if UNLIKELY(visitor.done())
                break;

            u32 start = visitor.cursor();
            Pos pos = visitor.pos();

            switch (ch) {
                case 'a' ... 'z':
                case 'A' ... 'Z':
                case '_':
                    visitor.read();
                    lexIdentifier(tokens, symbols, visitor, start, pos);
                    break;
                case '0' ... '9':
                    visitor.read();
                    lexNumber(tokens, symbols, visitor, start, pos);
                    break;
                case '+':
                    visitor.read();
                    tokens.push({ OperatorAdd, pos });
                    if (visitor.peek() == '=') {
                        visitor.read();
                        tokens.back().token = OperatorAddAssign;
                    } else if (visitor.peek() == '+') {
                        visitor.read();
                        tokens.back().token = OperatorIncr;
                    }
                    break;
                case '-':
                    visitor.read();
                    tokens.push({ OperatorSub, pos });
                    if (visitor.peek() == '=') {
                        visitor.read();
                        tokens.back().token = OperatorSubAssign;
                    } else if (visitor.peek() == '-') {
                        visitor.read();
                        tokens.back().token = OperatorDecr;
                    }
                    break;
                case '*':
                    visitor.read();
                    tokens.push({ OperatorMul, pos });
                    if (visitor.peek() == '=') {
                        visitor.read();
                        tokens.back().token = OperatorMulAssign;
                    } else if (visitor.peek() == '*') {
                        visitor.read();
                        tokens.back().token = OperatorExp;
                        if (visitor.peek() == '=') {
                            visitor.read();
                            tokens.back().token = OperatorExpAssign;
                        }
                    }
                    break;
                case '/':
                    visitor.read();
                    tokens.push({ OperatorDiv, pos });
                    if (visitor.peek() == '<') {
                        visitor.read();
                        if (visitor.peek() == '=') {
                            visitor.read();
                            tokens.back().token = OperatorLeftRotateAssign;
                        } else
                            tokens.back().token = OperatorLeftRotate;
                    } else if (visitor.peek() == '>') {
                        visitor.read();
                        if (visitor.peek() == '=') {
                            visitor.read();
                            tokens.back().token = OperatorRightRotateAssign;
                        } else
                            tokens.back().token = OperatorRightRotate;
                    } else if (visitor.peek() == '=') {
                        visitor.read();
                        tokens.back().token = OperatorDivAssign;
                    }
                    break;
                case '%':
                    visitor.read();
                    tokens.push({ OperatorRem, pos });
                    if (visitor.peek() == '=') {
                        visitor.read();
                        tokens.back().token = OperatorRemAssign;
                    }
                    break;
                case '&':
                    visitor.read();
                    tokens.push({ OperatorBitAnd, pos });
                    if (visitor.peek() == '=') {
                        visitor.read();
                        tokens.back().token = OperatorBitAndAssign;
                    }
                    break;
                case '|':
                    visitor.read();
                    tokens.push({ OperatorBitOr, pos });
                    if (visitor.peek() == '=') {
                        visitor.read();
                        tokens.back().token = OperatorBitOrAssign;
                    }
                    break;
                case '^':
                    visitor.read();
                    tokens.push({ OperatorBitXor, pos });
                    if (visitor.peek() == '=') {
                        visitor.read();
                        tokens.back().token = OperatorBitXorAssign;
                    }
                    break;
                case '<':
                    visitor.read();
                    tokens.push({ OperatorLess, pos });
                    if (visitor.peek() == '=') {
                        visitor.read();
                        tokens.back().token = OperatorLessEqual;
                    } else if (visitor.peek() == '<') {
                        visitor.read();
                        tokens.back().token = OperatorLeftShift;
                        if (visitor.peek() == '=') {
                            visitor.read();
                            tokens.back().token = OperatorLeftShiftAssign;
                        }
                    }
                    break;
                case '>':
                    visitor.read();
                    tokens.push({ OperatorGreater, pos });
                    if (visitor.peek() == '=') {
                        visitor.read();
                        tokens.back().token = OperatorGreaterEqual;
                    } else if (visitor.peek() == '>') {
                        visitor.read();
                        tokens.back().token = OperatorRightShift;
                        if (visitor.peek() == '=') {
                            visitor.read();
                            tokens.back().token = OperatorRightShiftAssign;
                        }
                    }
                    break;
                case '=':
                    visitor.read();
                    tokens.push({ OperatorAssign, pos });
                    if (visitor.peek() == '=') {
                        visitor.read();
                        tokens.back().token = OperatorEqual;
                    }
                    break;
                case '!':
                    visitor.read();
                    tokens.push({ OperatorNotEqual, pos });
                    assert(visitor.peek() == '=');
                    visitor.read();
                    break;
                case '~':
                    visitor.read();
                    tokens.push({ OperatorBitNot, pos });
                    break;
                case '#':
                    while (visitor.peek() != '\n')
                        visitor.read();
                    break;
                case '\'': {
                    visitor.read();
                    u8 next = visitor.peek();
                    if (next == '\\') {
                        visitor.read();
                        visitor.readPossiblyUTF8();
                    } else
                        visitor.readPossiblyUTF8();

                    assert(visitor.peek() == '\'');
                    visitor.read();
                    tokens.push({ symbols[visitor.sliceFrom(start)], pos });
                    break;
                }
                case '"':
                    visitor.read();
                    while (!visitor.done()) {
                        u8 next = visitor.peek();
                        if (next == '\\') {
                            visitor.read();
                            visitor.readPossiblyUTF8();
                        }
                        if (next == '"')
                            break;
                        visitor.read();
                    }
                    visitor.read();
                    tokens.push({ symbols[visitor.sliceFrom(start)], pos });
                    break;
                case '`':
                    visitor.read();
                    while (!visitor.done()) {
                        u8 next = visitor.peek();
                        if (next == '\\') {
                            visitor.read();
                            if (visitor.peek() == '`')
                                visitor.read();
                        }
                        if (next == '`')
                            break;
                        visitor.read();
                    }
                    visitor.read();
                    tokens.push({ symbols[visitor.sliceFrom(start)], pos });
                    break;
                case '\n':
                    visitor.read();
                    tokens.push({ WhitespaceNewline, pos });
                    processIndentsOnNewLine(tokens, indents, visitor);
                    break;
                case '(':
                    tokens.push({ PunctuatorLeftParen, pos });
                    visitor.read();
                    break;
                case ')':
                    tokens.push({ PunctuatorRightParen, pos });
                    visitor.read();
                    break;
                case '[':
                    tokens.push({ PunctuatorLeftBracket, pos });
                    visitor.read();
                    break;
                case ']':
                    tokens.push({ PunctuatorRightBracket, pos });
                    visitor.read();
                    break;
                case '{':
                    tokens.push({ PunctuatorLeftBrace, pos });
                    visitor.read();
                    break;
                case '}':
                    tokens.push({ PunctuatorRightBrace, pos });
                    visitor.read();
                    break;
                case '.':
                    tokens.push({ PunctuatorDot, pos });
                    visitor.read();
                    if UNLIKELY(visitor.peek() == '.') {
                        tokens.back().token = OperatorRange;
                        visitor.read();
                        if UNLIKELY(visitor.peek() == '.'){
                            tokens.back().token = OperatorEllipsis;
                            visitor.read();
                        }
                    }
                    break;
                case ',':
                    tokens.push({ PunctuatorComma, pos });
                    visitor.read();
                    break;
                case ':':
                    tokens.push({ PunctuatorColon, pos });
                    visitor.read();
                    break;
                case ';':
                    tokens.push({ PunctuatorSemicolon, pos });
                    visitor.read();
                    break;
                case 128 ... 255: {
                    rune r = visitor.peekUTF8();

                    if (utf8_is_letter(r)
                        || utf8_is_letter_number(r)) {
                        visitor.advance(r);
                        lexIdentifier(tokens, symbols, visitor, start, pos);
                    } else if (utf8_is_digit(r)) {
                        visitor.advance(r);
                        lexNumber(tokens, symbols, visitor, start, pos);
                    } else {
                        unreachable("Unexpected unicode character");
                    }
                    break;
                }
                default:
                    unreachable("Unexpected character");
            }
        }
    }

    NOINLINE Artifact* lex(Artifact* artifact) {
        assert(artifact->kind == ArtifactKind::Source);
        const_slice<i8> source = artifact->as<Source>()->takeSource();

        Tokens* tokens = new Tokens(source);
        SourceVisitor visitor(source, tokens->lineOffsets);
        vec<u32, 16> indents;
        indents.push(0);
        lex(tokens->tokens, indents, artifact->parent->compilation->symbols, visitor);
        while (indents.size() > 1) {
            tokens->tokens.push({ WhitespaceDedent, visitor.pos() });
            indents.pop();
        }

        artifact->update(tokens);
        if UNLIKELY(config::printTokens)
            tokens->print(artifact->parent->compilation);
        return artifact;
    }
}