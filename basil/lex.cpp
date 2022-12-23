#include "basil/lex.h"
#include "core/def.h"

MODULE(basil)

bool delimiters[128];
OperatorPrecedence operators[128];

void lex(Lexer& lexer, slice<rune> runes, vec<Token>& tokens) {
    mset(delimiters, false, sizeof(delimiters));
    mset(operators, PREC_NONE, sizeof(delimiters));

    delimiters[' '] = delimiters['\n'] = delimiters['\t']
        = delimiters['('] = delimiters[')']
        = delimiters['['] = delimiters[']']
        = delimiters['{'] = delimiters['}']
        = delimiters[','] = delimiters['.'] = true;

    operators[';'] = PREC_SEQ;
    operators['|'] = PREC_OR;
    operators['^'] = PREC_XOR;
    operators['&'] = PREC_AND;
    operators['<'] = operators['>'] = PREC_RELATION;
    operators['='] = PREC_EQUAL;
    operators['@'] = operators['$'] = operators['#'] = operators['~'] = operators['`'] = operators['!'] = PREC_NORMAL;
    operators['+'] = operators['-'] = PREC_PLUS_MINUS;
    operators['*'] = operators['/'] = operators['%'] = PREC_MULTIPLY_DIVIDE;
    operators[':'] = PREC_ANNOTATE;

    i16& col = lexer.col;
    i32& line = lexer.line, &start = lexer.start, &end = lexer.end;
    Token acc = { TK_NONE, col, line, start, end };
    for (rune r : runes) {
        if (acc.kind != TK_NONE) switch (acc.kind) {
            case TK_ICONST:
                acc.end = start;
                if (r.get() < '0' || r.get() > '9') {
                    tokens.push(acc);
                    acc.kind = TK_NONE;
                }
                else {
                    start ++;
                    continue;
                }
                break;
            case TK_OPERATOR:
            case TK_SYMBOL:
                acc.end = start;
                if (r.get() < 128 && delimiters[r.get()]) {
                    tokens.push(acc);
                    acc.kind = TK_NONE;
                }
                else {
                    start ++;
                    continue;
                }
                break;
            default:
                fatal("Unexpected accumulator token kind!");
        }
        switch ((i8)r.get()) {
            case '(':
                tokens.push({TK_LPAREN, col, line, start, start + 1});
                start ++;
                col ++;
                break;
            case ')':
                tokens.push({TK_RPAREN, col, line, start, start + 1});
                start ++;
                col ++;
                break;
            case '[':
                tokens.push({TK_LSQUARE, col, line, start, start + 1});
                start ++;
                col ++;
                break;
            case ']':
                tokens.push({TK_RSQUARE, col, line, start, start + 1});
                start ++;
                col ++;
                break;
            case '{':
                tokens.push({TK_LBRACE, col, line, start, start + 1});
                start ++;
                col ++;
                break;
            case '}':
                tokens.push({TK_RBRACE, col, line, start, start + 1});
                start ++;
                col ++;
                break;
            case ',':
                tokens.push({TK_COMMA, col, line, start, start + 1});
                start ++;
                col ++;
                break;
            case '.':
                tokens.push({TK_DOT, col, line, start, start + 1});
                start ++;
                col ++;
                break;
            case '0' ... '9':
                acc.kind = TK_ICONST;
                acc.col = col;
                acc.line = line;
                acc.start = start;
                start ++;
                col ++;
                break;
            case '\t':
                start ++;
                col += 4;
                break;
            case ' ':
                start ++;
                col ++;
                break;
            case '\n':
                tokens.push({TK_NEWLINE, col, line, start, start + 1});
                start ++;
                line ++;
                col = 0;
                break;
            default:
                acc.kind = r.get() < 128 && operators[r.get()] != PREC_NONE ? TK_OPERATOR : TK_SYMBOL;
                acc.col = col;
                acc.line = line;
                acc.start = start;
                start ++;
                col ++;
                break;
        }
    }
    if (acc.kind != TK_NONE) {
        acc.end = start;
        tokens.push(acc);
    }
}

ENDMODULE()