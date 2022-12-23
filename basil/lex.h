#ifndef BASIL_LEX_H
#define BASIL_LEX_H

#include "core/def.h"
#include "lib/slice.h"
#include "lib/vec.h"
#include "lib/utf.h"

MODULE(basil)

enum TokenKind : i8 {
    TK_NONE, TK_LPAREN, TK_RPAREN, TK_LSQUARE, TK_RSQUARE, TK_LBRACE, TK_RBRACE, TK_DOT, TK_COMMA, TK_OPERATOR, TK_SYMBOL, TK_ICONST, TK_NEWLINE
};

struct Token {
    TokenKind kind;
    i16 col;
    i32 line;
    i32 start, end;
};

enum OperatorPrecedence {
    PREC_NONE = 0,
    PREC_SEQ = 10,
    PREC_OR = 20,
    PREC_XOR = 30,
    PREC_AND = 40,
    PREC_RELATION = 50,
    PREC_EQUAL = 60,
    PREC_NORMAL = 70,
    PREC_PLUS_MINUS = 80,
    PREC_MULTIPLY_DIVIDE = 90,
    PREC_ANNOTATE = 100
};

extern OperatorPrecedence operators[128];

struct Lexer {
    i16 col = 0;
    i32 line = 0, start = 0, end = 0;
};

void lex(Lexer& lexer, slice<rune> runes, vec<Token>& tokens);

ENDMODULE()

#endif