#include "clover/lex.h"
#include "core/util.h"
#include "lib/io.h"
#include "lib/utf.h"
#include "clover/err.h"

#define TAB_SPACE 4

inline bool is_letter(rune r) {
    return r >= 'A' && r <= 'Z' || r >= 'a' && r <= 'z' || utf8_is_letter(r);
}

inline bool is_digit(rune r) {
    return r >= '0' && r <= '9' || utf8_is_digit(r);
}

inline bool is_ident(rune r) {
#ifdef INCLUDE_UTF8_LOOKUP_TABLE
    if (r >= 'a' && r <= 'z' || r >= 'A' && r <= 'Z' || r >= '0' && r <= '9' || r == '_') return true;
    if (r == ' ') return false;
    UnicodeCategory cat = utf8_category(r);
    if (cat > UNICODE_PUNCTUATION_CONNECTOR) return false;
    return cat >= UNICODE_CASED_LETTER && cat <= UNICODE_UPPERCASE_LETTER 
        || cat == UNICODE_DECIMAL_NUMBER || cat == UNICODE_NONSPACING_MARK || cat == UNICODE_SPACING_COMBINING_MARK || cat == UNICODE_PUNCTUATION_CONNECTOR;
#else
    return r >= 'A' && r <= 'Z' || r >= 'a' && r <= 'z' || r >= '0' && r <= '9' || r == '_' 
        || utf8_is_letter(r) || utf8_is_digit(r) || utf8_is_nonspacing_mark(r) || utf8_is_spacing_combining_mark(r) || utf8_is_connector(r);
#endif
}

inline bool is_space(rune r) {
    return r == ' ' || r == '\t' || utf8_is_space_separator(r);
}

void advance_lexer(Module* mod, UnicodeBuf& iter) {
    Token& acc = mod->lexer->scratch;
    i32& line = mod->lexer->line;
    i16& col = mod->lexer->column;
    i32& idx = mod->lexer->runeidx;
    auto& buf = mod->lexer->tokens;
    while (iter.idx < iter.length) {
        TokenKind tk = acc.kind;
        rune r = iter.peek();

        i32 rlen = 4;
        if (r <= UTF8_ONE_MAX) rlen = 1;
        else if (r <= UTF8_TWO_MAX) rlen = 2;
        else if (r <= UTF8_THREE_MAX) rlen = 3;

        if (mod->lexer->iscomment) {
            if (r == '\n') mod->lexer->iscomment = false;
            else {
                iter.read(), col ++, idx += rlen;
                continue;
            }
        }
        else if (r == '#' && tk != TK_STRCONST && tk != TK_CHCONST) {
            mod->lexer->iscomment = true;
            iter.read(), col ++, idx += rlen;
            continue;
        }

        switch (tk) {
        case TK_IDENT:
            if (is_ident(r)) acc.end += rlen, iter.read(), col ++, idx += rlen;
            else buf.push(acc), acc.kind = TK_NONE;
            break;
        case TK_ICONST:
            if (is_digit(r)) acc.end += rlen, iter.read(), col ++, idx += rlen;
            else if (r == '.') acc.kind = TK_FCONST, acc.end += rlen, iter.read(), col ++, idx += rlen;
            else if (is_letter(r)) letter_in_number_error(mod, idx, line, 1, col, r), acc.kind = TK_NONE;
            else buf.push(acc), acc.kind = TK_NONE;
            break;
        case TK_FCONST:
            if (is_digit(r)) acc.end += rlen, iter.read(), col ++, idx += rlen;
            else if (is_letter(r)) {
                if (mod->bytes.text[acc.end - 1] == '.') {
                    acc.kind = TK_ICONST;
                    acc.end -= 1;
                    buf.push(acc);
                    buf.push({ TK_DOT, i16(col - 1), line, acc.end, acc.end + 1 });
                    acc.kind = TK_NONE;
                }
                else letter_in_number_error(mod, idx, line, 1, col, r), acc.end += rlen, iter.read(), col ++, idx += rlen;
            }
            else buf.push(acc), acc.kind = TK_NONE;
            break;
        case TK_STRCONST:
            if (r == '\n') no_newline_in_string_error(mod, idx, line, 1, col), acc.end += rlen, iter.read(), col ++, idx += rlen;
            else if (r == '"') iter.read(), col ++, idx += rlen, acc.end += rlen, buf.push(acc), acc.kind = TK_NONE;
            else acc.end += rlen, iter.read(), col ++, idx += rlen;
            break;
        case TK_CHCONST:
            if (r == '\n') no_newline_in_char_error(mod, idx, line, 1, col), acc.end += rlen, iter.read(), col ++, idx += rlen;
            else if (r == '\'') iter.read(), col ++, acc.end += rlen, idx += rlen, buf.push(acc), acc.kind = TK_NONE;
            else acc.end += rlen, iter.read(), col ++, idx += rlen;
            break;
        case TK_PLUS:
            if (r == '+') acc.kind = TK_INCR, buf.push(acc), iter.read(), col ++, idx += rlen, acc.kind = TK_NONE;
            else if (r == '=') acc.kind = TK_PLUSEQ, buf.push(acc), iter.read(), col ++, idx += rlen, acc.kind = TK_NONE;
            else buf.push(acc), acc.kind = TK_NONE;
            break;
        case TK_MINUS:
            if (r == '-') acc.kind = TK_DECR, buf.push(acc), iter.read(), col ++, idx += rlen, acc.kind = TK_NONE;
            else if (r == '=') acc.kind = TK_MINUSEQ, buf.push(acc), iter.read(), col ++, idx += rlen, acc.kind = TK_NONE;
            else buf.push(acc), acc.kind = TK_NONE;
            break;
        case TK_TIMES:
            if (r == '*') acc.kind = TK_EXP, buf.push(acc), iter.read(), col ++, idx += rlen, acc.kind = TK_NONE;
            else if (r == '=') acc.kind = TK_TIMESEQ, buf.push(acc), iter.read(), col ++, idx += rlen, acc.kind = TK_NONE;
            else buf.push(acc), acc.kind = TK_NONE;
            break;
        case TK_DIVIDE:
            if (r == '=') acc.kind = TK_DIVIDEEQ, buf.push(acc), iter.read(), col ++, idx += rlen, acc.kind = TK_NONE;
            else buf.push(acc), acc.kind = TK_NONE;
            break;
        case TK_MODULO:
            if (r == '=') acc.kind = TK_MODULOEQ, buf.push(acc), iter.read(), col ++, idx += rlen, acc.kind = TK_NONE;
            else buf.push(acc), acc.kind = TK_NONE;
            break;
        case TK_BITAND:
            if (r == '=') acc.kind = TK_BITANDEQ, buf.push(acc), iter.read(), col ++, idx += rlen, acc.kind = TK_NONE;
            else buf.push(acc), acc.kind = TK_NONE;
            break;
        case TK_BITOR:
            if (r == '=') acc.kind = TK_BITOREQ, buf.push(acc), iter.read(), col ++, idx += rlen, acc.kind = TK_NONE;
            else buf.push(acc), acc.kind = TK_NONE;
            break;
        case TK_BITXOR:
            if (r == '=') acc.kind = TK_BITXOREQ, buf.push(acc), iter.read(), col ++, idx += rlen, acc.kind = TK_NONE;
            else buf.push(acc), acc.kind = TK_NONE;
            break;
        case TK_NOT:
            if (r == '=') acc.kind = TK_INEQUAL, buf.push(acc), iter.read(), col ++, idx += rlen, acc.kind = TK_NONE;
            else buf.push(acc), acc.kind = TK_NONE;
            break;
        case TK_LESS:
            if (r == '=') acc.kind = TK_LEQUAL, buf.push(acc), iter.read(), col ++, idx += rlen, acc.kind = TK_NONE;
            else if (r == '<') acc.kind = TK_BITLEFT, buf.push(acc), iter.read(), col ++, idx += rlen, acc.kind = TK_NONE;
            else buf.push(acc), acc.kind = TK_NONE;
            break;
        case TK_GREATER:
            if (r == '=') acc.kind = TK_GEQUAL, buf.push(acc), iter.read(), col ++, idx += rlen, acc.kind = TK_NONE;
            else if (r == '>') acc.kind = TK_BITRIGHT, buf.push(acc), iter.read(), col ++, idx += rlen, acc.kind = TK_NONE;
            else buf.push(acc), acc.kind = TK_NONE;
            break;
        case TK_ASSIGN:
            if (r == '=') acc.kind = TK_EQUAL, buf.push(acc), iter.read(), col ++, idx += rlen, acc.kind = TK_NONE;
            else buf.push(acc), acc.kind = TK_NONE;
            break;
        case TK_NONE:
            switch (r) {
            case '\n':
                buf.push({ TK_NEWLINE, col, line, idx, idx + rlen });
                col = -1;
                line ++;
                break;
            case '(': 
                buf.push({ TK_LPAREN, col, line, idx, idx + rlen });
                break;
            case ')': 
                buf.push({ TK_RPAREN, col, line, idx, idx + rlen });
                break;
            case '[': 
                buf.push({ TK_LSQUARE, col, line, idx, idx + rlen });
                break;
            case ']': 
                buf.push({ TK_RSQUARE, col, line, idx, idx + rlen });
                break;
            case '{': 
                buf.push({ TK_LBRACE, col, line, idx, idx + rlen });
                break;
            case '}': 
                buf.push({ TK_RBRACE, col, line, idx, idx + rlen });
                break;
            case '.': 
                buf.push({ TK_DOT, col, line, idx, idx + rlen });
                break;
            case ',': 
                buf.push({ TK_COMMA, col, line, idx, idx + rlen });
                break;
            case ':': 
                buf.push({ TK_COLON, col, line, idx, idx + rlen });
                break;
            case '~':
                buf.push({ TK_BITNOT, col, line, idx, idx + rlen });
                break;
            case '\'':
                acc = { TK_CHCONST, col, line, idx, idx + rlen };
                break;
            case '"':
                acc = { TK_STRCONST, col, line, idx, idx + rlen };
                break;
            case '+':
                acc = { TK_PLUS, col, line, idx, idx + rlen };
                break;
            case '-':
                acc = { TK_MINUS, col, line, idx, idx + rlen };
                break;
            case '*':
                acc = { TK_TIMES, col, line, idx, idx + rlen };
                break;
            case '/':
                acc = { TK_DIVIDE, col, line, idx, idx + rlen };
                break;
            case '%':
                acc = { TK_MODULO, col, line, idx, idx + rlen };
                break;
            case '&':
                acc = { TK_BITAND, col, line, idx, idx + rlen };
                break;
            case '|':
                acc = { TK_BITOR, col, line, idx, idx + rlen };
                break;
            case '^':
                acc = { TK_BITXOR, col, line, idx, idx + rlen };
                break;
            case '!':
                acc = { TK_NOT, col, line, idx, idx + rlen };
                break;
            case '<':
                acc = { TK_LESS, col, line, idx, idx + rlen };
                break;
            case '>':
                acc = { TK_GREATER, col, line, idx, idx + rlen };
                break;
            case '=':
                acc = { TK_ASSIGN, col, line, idx, idx + rlen };
                break;
            case ' ':
                break;
            case '\t':
                col += TAB_SPACE - 1; // we'll add the remaining space after the switch
                break;
            case 'A': case 'B': case 'C': case 'D': case 'E': case 'F': 
            case 'G': case 'H': case 'I': case 'J': case 'K': case 'L': 
            case 'M': case 'N': case 'O': case 'P': case 'Q': case 'R': 
            case 'S': case 'T': case 'U': case 'V': case 'W': case 'X': 
            case 'Y': case 'Z': case 'a': case 'b': case 'c': case 'd': 
            case 'e': case 'f': case 'g': case 'h': case 'i': case 'j': 
            case 'k': case 'l': case 'm': case 'n': case 'o': case 'p': 
            case 'q': case 'r': case 's': case 't': case 'u': case 'v': 
            case 'w': case 'x': case 'y': case 'z': 
                acc = { TK_IDENT, col, line, idx, idx + rlen };
                break;
            case '0': case '1': case '2': case '3': case '4': 
            case '5': case '6': case '7': case '8': case '9':
                acc = { TK_ICONST, col, line, idx, idx + rlen };
                break;
            default:
                if (utf8_is_letter(r)) {
                    acc = { TK_IDENT, col, line, idx, idx + rlen };
                }
                else if (utf8_is_space_separator(r)) {
                    if (r == '\t') col += TAB_SPACE - 1;
                }
                else if (utf8_is_digit(r)) {
                    acc = { TK_ICONST, col, line, idx, idx + rlen };
                }
                else if (utf8_is_letter_number(r)) {
                    acc = { TK_IDENT, col, line, idx, idx + rlen };
                }
                else unexpected_char_error(mod, idx, line, 1, col, r);
            }
            iter.read();
            col ++;
            idx += rlen;
            break;
        default:
            unreachable("Tried to continue unexpected token type.");
            break;
        }
    }
}

Interner::Interner() {
    for (iptr i = TK_FIRST_KEYWORD; i < TK_LAST_KEYWORD; i ++) {
        intern(const_slice<i8>{ KEYWORD_STRINGS[i - TK_FIRST_KEYWORD], cidx(KEYWORD_STRINGS[i - TK_FIRST_KEYWORD], '\0') });
        n_keywords ++;
    }
}

void advance_interner(Module* mod) {
    for (iptr i = 0; i < mod->lexer->tokens.size(); i ++) {
        Token& tok = mod->lexer->tokens[i];
        if (tok.kind == TK_IDENT) {
            prehash<const_slice<i8>> str = const_slice<i8>{ mod->bytes.text + tok.start, tok.end - tok.start };
            Symbol id = mod->interner->intern(str);
            if (mod->interner->is_keyword(id)) tok.kind = TokenKind(TK_FIRST_KEYWORD + id);
            else tok.end = id; // end is unnecessary if we know the length of the interned string
        }
    }
}