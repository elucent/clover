#ifndef BASIL_CLOVER_LEX_H
#define BASIL_CLOVER_LEX_H

#include "clover/clover.h"
#include "clover/load.h"
#include "lib/buffer.h"
#include "lib/hash.h"
#include "lib/vec.h"

/*
 * TokenKind
 *
 * Each of these values represents a type of Clover token, except for TK_NONE which functions
 * as a sentinel representing the absence of a token.
 */
enum TokenKind : u8 {
    TK_NONE, TK_EOF,
    TK_NEWLINE,
    TK_LPAREN, TK_RPAREN, TK_LSQUARE, TK_RSQUARE, TK_LBRACE, TK_RBRACE,
    TK_COLON, TK_COMMA, TK_DOT, TK_QUESTION,
    TK_LESS, TK_LEQUAL, TK_GREATER, TK_GEQUAL, TK_EQUAL, TK_INEQUAL, TK_INCR, TK_DECR,
    TK_PLUS, TK_MINUS, TK_TIMES, TK_DIVIDE, TK_MODULO, TK_EXP,
    TK_BITAND, TK_BITOR, TK_BITXOR, TK_BITLEFT, TK_BITRIGHT, TK_BITNOT,
    TK_ASSIGN, TK_PLUSEQ, TK_MINUSEQ, TK_TIMESEQ, TK_DIVIDEEQ, TK_MODULOEQ, TK_EXPEQ,
    TK_BITANDEQ, TK_BITOREQ, TK_BITXOREQ, TK_BITLEFTEQ, TK_BITRIGHTEQ,
    TK_FIRST_KEYWORD,
    TK_AND = TK_FIRST_KEYWORD, TK_OR, TK_NOT,
    TK_IF, TK_ELSE, TK_WHILE, TK_UNTIL, TK_WITH, TK_FOR, TK_IN, TK_USE, TK_RETURN, TK_DEFER, TK_AS, TK_IS, 
    TK_FUN, TK_VAR, TK_TYPE, TK_CASE, TK_MATCH, TK_ALIAS, TK_MODULE, TK_NEW, TK_DEL, TK_DO, TK_THEN, TK_BREAK, TK_CONTINUE, TK_PASS,
    TK_TRUE, TK_FALSE, TK_LAST_KEYWORD,
    TK_ICONST = TK_LAST_KEYWORD, TK_FCONST, TK_STRCONST, TK_CHCONST, TK_IDENT
};

static constexpr const i8* KEYWORD_STRINGS[TK_LAST_KEYWORD - TK_FIRST_KEYWORD] = {
    "and", "or", "not", 
    "if", "else", "while", "until", "with", "for", "in", "use", "return", "defer", "as", "is", 
    "fun", "var", "type", "case", "match", "alias", "module", "new", "del", "do", "then", "break", "continue", "pass",
    "true", "false"
};

static constexpr iptr NUM_TOKEN_KINDS = TK_IDENT + 1;

static constexpr const i8 
    PREC_NONE = 0,
    PREC_ASSIGN = 1,
    PREC_OR = 2,
    PREC_AND = 3,
    PREC_EQUALS = 4,
    PREC_COMPARE = 5,
    PREC_BITOR = 6,
    PREC_BITXOR = 7,
    PREC_BITAND = 8,
    PREC_ADD = 9,
    PREC_TIMES = 10,
    PREC_POW = 11,
    PREC_INCR = 12,
    PREC_DOT = 13;         

static constexpr const i8 PRECEDENCE[NUM_TOKEN_KINDS] = {
    PREC_NONE, PREC_NONE,
    PREC_NONE,
    PREC_NONE, PREC_NONE, PREC_NONE, PREC_NONE, PREC_NONE, PREC_NONE,
    PREC_NONE, PREC_NONE, PREC_DOT, PREC_INCR,
    PREC_COMPARE, PREC_COMPARE, PREC_COMPARE, PREC_COMPARE, PREC_EQUALS, PREC_EQUALS, PREC_INCR, PREC_INCR,
    PREC_ADD, PREC_ADD, PREC_TIMES, PREC_TIMES, PREC_TIMES, PREC_POW,
    PREC_BITAND, PREC_BITOR, PREC_BITXOR, PREC_POW, PREC_POW, PREC_INCR,
    PREC_ASSIGN, PREC_ASSIGN, PREC_ASSIGN, PREC_ASSIGN, PREC_ASSIGN, PREC_ASSIGN, PREC_ASSIGN,
    PREC_ASSIGN, PREC_ASSIGN, PREC_ASSIGN, PREC_ASSIGN, PREC_ASSIGN,
    PREC_AND, PREC_OR, PREC_INCR, 
    PREC_NONE, PREC_NONE, PREC_NONE, PREC_NONE, PREC_NONE, PREC_NONE, PREC_NONE, PREC_NONE, PREC_NONE, PREC_NONE, PREC_NONE, PREC_EQUALS,
    PREC_NONE, PREC_NONE, PREC_NONE, PREC_NONE, PREC_NONE, PREC_NONE, PREC_NONE, PREC_NONE, PREC_NONE, PREC_NONE, PREC_NONE, PREC_NONE, PREC_NONE, PREC_NONE,
    PREC_NONE, PREC_NONE,
    PREC_NONE, PREC_NONE, PREC_NONE, PREC_NONE, PREC_NONE
};

/*
 * SourcePos
 *
 * The location of a Clover construct within a source file.
 */
struct SourcePos {
    i16 column;
    i32 line, start, end;

    inline SourcePos(): column(0), line(0), start(0), end(0) {}

    inline SourcePos(i16 column_in, i32 line_in, i32 start_in, i32 end_in):
        column(column_in), line(line_in), start(start_in), end(end_in) {}

    inline SourcePos operator+(SourcePos other) const {
        return {
            column < other.column || line < other.line ? column : other.column,
            line < other.line ? line : other.line,
            start < other.start ? start : other.start,
            end < other.end ? other.end : end
        };
    }
};

/*
 * Token
 *
 * A single Clover token, containing information about where it is positioned in the file.
 */
struct Token {
    TokenKind kind;
    i16 column;
    i32 line;
    i32 start, end;

    inline const_slice<i8> str(Module* mod);
    inline operator SourcePos() const {
        return { column, line, start, end };
    }
};

static_assert(sizeof(Token) <= 16, "Token is larger than 16 bytes.");

/*
 * Lexer
 *
 * Contains a fixed buffer of tokens with at least one space for each element of UnicodeBuf,
 * as well as all necessary context information.
 */
struct Lexer {
    i32 runeidx = 0, line = 0;
    i16 column = 0;
    bool iscomment = false;
    Token scratch = { TK_NONE, 0, 0, 0, 0 };
    buffer<Token, UNICODE_BUFSIZE> tokens;
};

/*
 * Interner
 *
 * Tracks the byte position of tokens, interns the strings backing identifiers, and handles 
 * detection of keywords.
 */
struct Interner {
    i32 byteidx = 0, runeidx = 0, n_keywords = 0;
    map<const_slice<i8>, Symbol> intern_table;
    vec<const_slice<i8>> intern_data;

    // Initializes string table with keywords.
    Interner();

    inline Symbol intern(const const_slice<i8>& str) {
        auto it = intern_table.find(str);
        Symbol id;
        if (it == intern_table.end()) {
            id = intern_data.size(); 
            intern_table.put(str, id);
            intern_data.push(str);
        }
        else id = it->value;
        return id;
    }

    inline Symbol intern(const i8* literal) {
        return intern({literal, cidx(literal, '\0')});
    }

    inline bool is_keyword(Symbol i) const {
        return i < n_keywords;
    }

    inline const const_slice<i8>& str(Symbol sym) const {
        return intern_data[sym];
    }
};

inline const_slice<i8> Token::str(Module* mod) {
    if (kind == TK_IDENT) return mod->interner->intern_data[end];
    else return { mod->bytes.text + start, end - start };
}

void advance_lexer(Module* mod, UnicodeBuf& unicode);
void advance_interner(Module* mod);

#endif