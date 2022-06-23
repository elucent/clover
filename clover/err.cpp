#include "clover/err.h"
#include "clover/lex.h"
#include "clover/ast.h"

ErrorKind err_kind = NO_CLOVER_ERROR;

enum ErrorType : i8 {
    ERR_UTF8_FORMAT,        // Source is not valid UTF-8

    ERR_UNEXPECTED_CHAR,    // Unexpected character in lexer input.
    ERR_LETTER_IN_NUMBER,   // Unexpected letter in numeric constant.
    ERR_STRING_NEWLINE,     // Unexpected newline in string constant.
    ERR_CHAR_NEWLINE,       // Unexpected newline in char constant.
    ERR_UNKNOWN_ESCAPE,     // Unknown escape sequence in string or char constant.
    ERR_UNEXPECTED_EOF,     // Unexpected end of file char.

    ERR_NO_PARAMLIST_SEP,   // No parenthese or comma separator in parameter list.
    ERR_NO_DECL_NL_SEP,     // No newline, colon, or comma in variable declaration.
    ERR_NO_VARDECL_COLON,   // No colon in variable declaration.
    ERR_NO_VARDECL_SEP,     // No newline or comma after variable declaration.
    ERR_EXPECTED_PRIMARY,   // Expected primary expression.
    ERR_NO_FUNDECL_LPAREN,  // No left paren in function declaration.
    ERR_NO_CALL_SEP,        // No right paren or comma in call.
    ERR_NO_SLICE_RBRACKET,  // No right bracket in slice expression.
    ERR_NO_ELSE_IN_IF,      // No 'else' in if statement.
    ERR_NO_SLICE_SEP,       // No right bracket or colon in slice expression.
    ERR_NO_CLOSING_PAREN,   // No right paren in parenthetical expression.
    ERR_NO_BLOCK,           // No colon in statement expecting block.
    ERR_NO_SET_SEP,         // No comma or closing brace in set expression.
    ERR_NO_ARRAY_SEP,       // No comma or closing bracket in array expression.
    ERR_NO_NEWLINE,         // No newline after statement.
    ERR_NO_FORLOOP_IN,      // No 'in' in for loop.
    ERR_NO_NEWLINE_OR_AS,   // No newline or 'as' in use expression.
    ERR_NO_USE_ALIAS,       // No identifier alias in use expression.
    ERR_NO_ALIAS_COLON,     // No colon in alias expression.
    ERR_NO_ALIAS_IDENT,     // No identifier in alias expression.
    ERR_NO_TYPE_COLON_NL,   // No colon or newline in type declaration.
    ERR_NO_TYPEDECL_PARAM,  // Incorrect type declaration parameter.
    ERR_NO_TYPEDECL_NAME,   // Incorrect identifier in type declaration.
    ERR_NO_CASE_COLON_NL,   // No colon or newline in case declaration.
    ERR_NO_MODULE_NAME,     // No identifier in module declaration.
    ERR_NON_IDENT_IN_TVAR,  // No identifier in named type variable.
};

#define CODE static constexpr ErrorType CODE

struct Error {
    ErrorType kind;
    Module* mod;

    union ErrorData {
        ErrorData() {}

        struct UTF8Format       { CODE = ERR_UTF8_FORMAT; UnicodeError uerr; i32 pos; } utf8_format;

        ErrorData(UTF8Format e): utf8_format(e) {}

        struct UnexpectedChar   { CODE = ERR_UNEXPECTED_CHAR; i32 pos, line, len; i16 col; rune ch; } unexpected_char;
        struct LetterInNumber   { CODE = ERR_LETTER_IN_NUMBER; i32 pos, line, len; i16 col; rune ch; } letter_in_number;
        struct StringNewline    { CODE = ERR_STRING_NEWLINE; i32 pos, line, len; i16 col; } string_newline;
        struct CharNewline      { CODE = ERR_CHAR_NEWLINE; i32 pos, line, len; i16 col; } char_newline;
        struct UnknownEscape    { CODE = ERR_UNKNOWN_ESCAPE; i32 pos, line, len; i16 col; rune ch; } unknown_escape;
        struct UnexpectedEOF    { CODE = ERR_UNEXPECTED_EOF; i32 pos, line, len; i16 col; } unexpected_eof;

        ErrorData(UnexpectedChar e): unexpected_char(e) {}
        ErrorData(LetterInNumber e): letter_in_number(e) {}
        ErrorData(StringNewline e): string_newline(e) {}
        ErrorData(CharNewline e): char_newline(e) {}
        ErrorData(UnknownEscape e): unknown_escape(e) {}
        ErrorData(UnexpectedEOF e): unexpected_eof(e) {}

        struct NoParamListSep   { CODE = ERR_NO_PARAMLIST_SEP; Token tk; } no_param_list_sep;
        struct NoDeclNewline    { CODE = ERR_NO_DECL_NL_SEP; Token tk; } no_decl_newline;
        struct NoVarDeclColon   { CODE = ERR_NO_VARDECL_COLON; Token tk; } no_vardecl_colon;
        struct NoVarDeclSep     { CODE = ERR_NO_VARDECL_SEP; Token tk; } no_vardecl_sep;
        struct ExpectedPrimary  { CODE = ERR_EXPECTED_PRIMARY; Token tk; } expected_primary;
        struct NoFunDeclLparen  { CODE = ERR_NO_FUNDECL_LPAREN; Token tk; } no_fundecl_lparen;
        struct NoCallSep        { CODE = ERR_NO_CALL_SEP; Token tk; } no_call_sep;
        struct NoSliceRBracket  { CODE = ERR_NO_SLICE_RBRACKET; Token tk; } no_slice_rbracket;
        struct NoElseInIf       { CODE = ERR_NO_ELSE_IN_IF; Token tk; } no_else_in_if;
        struct NoSliceSep       { CODE = ERR_NO_SLICE_SEP; Token tk; } no_slice_sep;
        struct NoClosingParen   { CODE = ERR_NO_CLOSING_PAREN; Token tk; } no_closing_paren;
        struct NoBlock          { CODE = ERR_NO_BLOCK; Token tk; } no_block;
        struct NoSetSep         { CODE = ERR_NO_SET_SEP; Token tk; } no_set_sep;
        struct NoArraySep       { CODE = ERR_NO_ARRAY_SEP; Token tk; } no_array_sep;
        struct NoNewline        { CODE = ERR_NO_NEWLINE; Token tk; } no_newline;
        struct NoForLoopIn      { CODE = ERR_NO_FORLOOP_IN; Token tk; } no_for_loop_in;
        struct NoNewlineOrAs    { CODE = ERR_NO_NEWLINE_OR_AS; Token tk; } no_newline_or_as;
        struct NoUseAlias       { CODE = ERR_NO_USE_ALIAS; Token tk; } no_use_alias;
        struct NoAliasColon     { CODE = ERR_NO_ALIAS_COLON; Token tk; } no_alias_colon;
        struct NoAliasIdent     { CODE = ERR_NO_ALIAS_IDENT; Token tk; } no_alias_ident;
        struct NoTypeColonNl    { CODE = ERR_NO_TYPE_COLON_NL; Token tk; } no_type_colon_nl;
        struct NoTypeDeclParam  { CODE = ERR_NO_TYPEDECL_PARAM; Token tk; } no_typedecl_param;
        struct NoTypeDeclName   { CODE = ERR_NO_TYPEDECL_NAME; Token tk; } no_typedecl_name;
        struct NoCaseColonNl    { CODE = ERR_NO_CASE_COLON_NL; Token tk; } no_case_colon_nl;
        struct NoModuleName     { CODE = ERR_NO_MODULE_NAME; Token tk; } no_module_name;
        struct NonIdentInTvar   { CODE = ERR_NON_IDENT_IN_TVAR; AST* ast; } non_ident_in_tvar;
        
        ErrorData(NoParamListSep e): no_param_list_sep(e) {}
        ErrorData(NoDeclNewline e): no_decl_newline(e) {}
        ErrorData(NoVarDeclColon e): no_vardecl_colon(e) {}
        ErrorData(NoVarDeclSep e): no_vardecl_sep(e) {}
        ErrorData(ExpectedPrimary e): expected_primary(e) {}
        ErrorData(NoFunDeclLparen e): no_fundecl_lparen(e) {}
        ErrorData(NoCallSep e): no_call_sep(e) {}
        ErrorData(NoSliceRBracket e): no_slice_rbracket(e) {}
        ErrorData(NoElseInIf e): no_else_in_if(e) {}
        ErrorData(NoSliceSep e): no_slice_sep(e) {}
        ErrorData(NoClosingParen e): no_closing_paren(e) {}
        ErrorData(NoBlock e): no_block(e) {}
        ErrorData(NoSetSep e): no_set_sep(e) {}
        ErrorData(NoArraySep e): no_array_sep(e) {}
        ErrorData(NoNewline e): no_newline(e) {}
        ErrorData(NoForLoopIn e): no_for_loop_in(e) {}
        ErrorData(NoNewlineOrAs e): no_newline_or_as(e) {}
        ErrorData(NoUseAlias e): no_use_alias(e) {}
        ErrorData(NoAliasColon e): no_alias_colon(e) {}
        ErrorData(NoAliasIdent e): no_alias_ident(e) {}
        ErrorData(NoTypeColonNl e): no_type_colon_nl(e) {}
        ErrorData(NoTypeDeclParam e): no_typedecl_param(e) {}
        ErrorData(NoTypeDeclName e): no_typedecl_name(e) {}
        ErrorData(NoCaseColonNl e): no_case_colon_nl(e) {}
        ErrorData(NoModuleName e): no_module_name(e) {}
        ErrorData(NonIdentInTvar e): non_ident_in_tvar(e) {}
    } data;
};

#undef CODE

static Error errors[65536];
static i64 n_errors = 0, max_errors = 65536;

template<typename Case, typename ...Args>
void push_error(Module* mod, Args... args) {
    if (n_errors >= max_errors) return;
    Error e;
    e.mod = mod;
    e.kind = Case::CODE;
    e.data = Error::ErrorData(Case{args...});
    errors[n_errors ++] = e;
}

void decode_error() {
    if (err_kind < DECODE_ERROR) err_kind = DECODE_ERROR;
}

void lex_error() {
    if (err_kind < LEX_ERROR) err_kind = LEX_ERROR;
}

void parse_error() {
    if (err_kind < PARSE_ERROR) err_kind = PARSE_ERROR;
}

void utf8_format_error(Module* mod, UnicodeError err, i32 pos) {
    decode_error();
    push_error<Error::ErrorData::UTF8Format>(mod, err, pos);
}

void unexpected_char_error(Module* mod, i32 pos, i32 line, i32 len, i16 col, rune ch) {
    lex_error();
    push_error<Error::ErrorData::UnexpectedChar>(mod, pos, line, len, col, ch);
}

void letter_in_number_error(Module* mod, i32 pos, i32 line, i32 len, i16 col, rune ch) {
    lex_error();
    push_error<Error::ErrorData::LetterInNumber>(mod, pos, line, len, col, ch);
}

void no_newline_in_string_error(Module* mod, i32 pos, i32 line, i32 len, i16 col) {
    lex_error();
    push_error<Error::ErrorData::StringNewline>(mod, pos, line, len, col);
}

void no_newline_in_char_error(Module* mod, i32 pos, i32 line, i32 len, i16 col) {
    lex_error();
    push_error<Error::ErrorData::CharNewline>(mod, pos, line, len, col);
}

void unknown_escape_error(Module* mod, i32 pos, i32 line, i32 len, i16 col, rune ch) {
    lex_error();
    push_error<Error::ErrorData::UnknownEscape>(mod, pos, line, len, col, ch);
}

void unexpected_eof_error(Module* mod, i32 pos, i32 line, i32 len, i16 col) {
    lex_error();
    push_error<Error::ErrorData::UnexpectedEOF>(mod, pos, line, len, col);
}

void no_paramlist_sep_error(Module* mod, const Token& tk) {
    parse_error();
    push_error<Error::ErrorData::NoParamListSep>(mod, tk);
}

void no_decl_newline_error(Module* mod, const Token& tk) {
    parse_error();
    push_error<Error::ErrorData::NoDeclNewline>(mod, tk);
}

void no_vardecl_colon_error(Module* mod, const Token& tk) {
    parse_error();
    push_error<Error::ErrorData::NoVarDeclColon>(mod, tk);
}

void no_vardecl_sep_error(Module* mod, const Token& tk) {
    parse_error();
    push_error<Error::ErrorData::NoVarDeclSep>(mod, tk);
}

void expected_primary_error(Module* mod, const Token& tk) {
    parse_error();
    push_error<Error::ErrorData::ExpectedPrimary>(mod, tk);
}

void no_fundecl_lparen_error(Module* mod, const Token& tk) {
    parse_error();
    push_error<Error::ErrorData::NoFunDeclLparen>(mod, tk);
}

void no_call_sep_error(Module* mod, const Token& tk) {
    parse_error();
    push_error<Error::ErrorData::NoCallSep>(mod, tk);
}

void no_slice_rbracket_error(Module* mod, const Token& tk) {
    parse_error();
    push_error<Error::ErrorData::NoSliceRBracket>(mod, tk);
}

void no_else_in_if_error(Module* mod, const Token& tk) {
    parse_error();
    push_error<Error::ErrorData::NoElseInIf>(mod, tk);
}

void no_slice_sep_error(Module* mod, const Token& tk) {
    parse_error();
    push_error<Error::ErrorData::NoSliceSep>(mod, tk);
}

void no_closing_paren_error(Module* mod, const Token& tk) {
    parse_error();
    push_error<Error::ErrorData::NoClosingParen>(mod, tk);
}

void no_block_error(Module* mod, const Token& tk) {
    parse_error();
    push_error<Error::ErrorData::NoBlock>(mod, tk);
}

void no_set_sep_error(Module* mod, const Token& tk) {
    parse_error();
    push_error<Error::ErrorData::NoSetSep>(mod, tk);
}

void no_array_sep_error(Module* mod, const Token& tk) {
    parse_error();
    push_error<Error::ErrorData::NoArraySep>(mod, tk);
}

void no_newline_error(Module* mod, const Token& tk) {
    parse_error();
    push_error<Error::ErrorData::NoNewline>(mod, tk);
}

void no_for_loop_in_error(Module* mod, const Token& tk) {
    parse_error();
    push_error<Error::ErrorData::NoForLoopIn>(mod, tk);
}

void no_newline_or_as_error(Module* mod, const Token& tk) {
    parse_error();
    push_error<Error::ErrorData::NoNewlineOrAs>(mod, tk);
}

void no_use_alias_error(Module* mod, const Token& tk) {
    parse_error();
    push_error<Error::ErrorData::NoUseAlias>(mod, tk);
}

void no_alias_colon_error(Module* mod, const Token& tk) {
    parse_error();
    push_error<Error::ErrorData::NoAliasColon>(mod, tk);
}

void no_alias_ident_error(Module* mod, const Token& tk) {
    parse_error();
    push_error<Error::ErrorData::NoAliasIdent>(mod, tk);
}

void no_type_colon_nl_error(Module* mod, const Token& tk) {
    parse_error();
    push_error<Error::ErrorData::NoTypeColonNl>(mod, tk);
}

void no_typedecl_param_error(Module* mod, const Token& tk) {
    parse_error();
    push_error<Error::ErrorData::NoTypeDeclParam>(mod, tk);
}

void no_typedecl_name_error(Module* mod, const Token& tk) {
    parse_error();
    push_error<Error::ErrorData::NoTypeDeclName>(mod, tk);
}

void no_case_colon_nl_error(Module* mod, const Token& tk) {
    parse_error();
    push_error<Error::ErrorData::NoCaseColonNl>(mod, tk);
}

void no_module_name_error(Module* mod, const Token& tk) {
    parse_error();
    push_error<Error::ErrorData::NoModuleName>(mod, tk);
}

void non_ident_in_tvar_error(Module* mod, AST* ast) {
    parse_error();
    push_error<Error::ErrorData::NonIdentInTvar>(mod, ast);
}

static const i8* RED = "\e[1;31m";
static const i8* RESET = "\e[0m";

void print_line(Module* mod, stream& io, i32 pos, i32 len, i16 col) {
    i8* p = mod->bytes.text + pos;
    i8* begin = p - col;
    i8* end = begin;
    while (end < mod->bytes.text + mod->bytes.length && *end != '\n' && *end) end ++;
    write(io, "    ");
    for (i64 i = 0; i < end - begin; i ++) {
        if (i == col) write(io, RED);
        if (i == col + len) write(io, RESET);
        write(io, begin[i]);
    }
    write(io, RESET, '\n');
    write(io, "    ", RED);
    for (i64 i = 0; i < end - begin + 1; i ++) {
        if (i >= col && i < col + len) write(io, '^');
        else write(io, ' ');
    }
    write(io, RESET, '\n');
}

void print_line(Module* mod, stream& io, SourcePos pos) {
    print_line(mod, io, pos.start, pos.end - pos.start, pos.column);
}

void print_loc(Module* mod, stream& io, i32 line, i32 col) {
    write(io, mod->path, ':', line + 1, ':', col + 1, ' ');
}

void print_loc(Module* mod, stream& io, SourcePos pos) {
    print_loc(mod, io, pos.line, pos.column);
}

void print_error(stream& io, bool verbose, const Error& e) {
    Module* mod = e.mod;
    switch (e.kind) {
    case ERR_UTF8_FORMAT:
        write(io, RED, "Error", RESET, ": UTF-8 encoding error - ");
        switch (e.data.utf8_format.uerr) {
        case NO_ERROR:
            write(io, "no error.\n");
            break;
        case INCORRECT_FORMAT:
            write(io, "incorrect format.\n");
            break;
        case RAN_OUT_OF_BOUNDS:
            write(io, "ran out of bounds.\n");
            break;
        case BUFFER_TOO_SMALL:
            write(io, "buffer too small.\n");
            break;
        case INVALID_RUNE:
            write(io, "invalid character.\n");
            break;
        }
        break;
    case ERR_LETTER_IN_NUMBER:
        print_loc(mod, io, e.data.letter_in_number.line, e.data.letter_in_number.col);
        write(io, RED, "Error", RESET, ": Found letter character '", e.data.letter_in_number.ch, "' in numeric constant.\n");
        print_line(mod, io, e.data.letter_in_number.pos, e.data.letter_in_number.len, e.data.letter_in_number.col);
        break;
    case ERR_STRING_NEWLINE:
        print_loc(mod, io, e.data.string_newline.line, e.data.string_newline.col);
        write(io, RED, "Error", RESET, ": Line breaks are not permitted in string literals.\n");
        print_line(mod, io, e.data.string_newline.pos, e.data.string_newline.len, e.data.string_newline.col);
        break;
    case ERR_CHAR_NEWLINE:
        print_loc(mod, io, e.data.char_newline.line, e.data.char_newline.col);
        write(io, RED, "Error", RESET, ": Line breaks are not permitted in character literals.\n");
        print_line(mod, io, e.data.char_newline.pos, e.data.char_newline.len, e.data.char_newline.col);
        break;
    case ERR_UNEXPECTED_CHAR:
        print_loc(mod, io, e.data.unexpected_char.line, e.data.unexpected_char.col);
        write(io, RED, "Error", RESET, ": Found unexpected character '", e.data.unexpected_char.ch, "'.\n");
        print_line(mod, io, e.data.unexpected_char.pos, e.data.unexpected_char.len, e.data.unexpected_char.col);
        break;
    case ERR_UNEXPECTED_EOF:
        print_loc(mod, io, e.data.unexpected_eof.line, e.data.unexpected_eof.col);
        write(io, RED, "Error", RESET, ": Found unexpected end of file.\n");
        print_line(mod, io, e.data.unexpected_eof.pos, e.data.unexpected_eof.len, e.data.unexpected_eof.col);
        break;
    case ERR_NO_PARAMLIST_SEP:
        print_loc(mod, io, e.data.no_param_list_sep.tk);
        write(io, RED, "Error", RESET, ": Expected comma (',') or closing parenthesis (')') in parameter list.\n");
        print_line(mod, io, e.data.no_param_list_sep.tk);
        break;
    case ERR_NO_DECL_NL_SEP:
        print_loc(mod, io, e.data.no_decl_newline.tk);
        write(io, RED, "Error", RESET, ": Expected comma (','), colon (':') or line break in declaration.\n");
        print_line(mod, io, e.data.no_decl_newline.tk);
        break;
    case ERR_NO_VARDECL_COLON:
        print_loc(mod, io, e.data.no_vardecl_colon.tk);
        write(io, RED, "Error", RESET, ": Expected colon (':') in variable declaration.\n");
        print_line(mod, io, e.data.no_vardecl_colon.tk);
        break;
    case ERR_NO_VARDECL_SEP:
        print_loc(mod, io, e.data.no_vardecl_sep.tk);
        write(io, RED, "Error", RESET, ": Expected colon (':') or comma (',') in variable declaration.\n");
        print_line(mod, io, e.data.no_vardecl_sep.tk);
        break;
    case ERR_EXPECTED_PRIMARY:
        print_loc(mod, io, e.data.expected_primary.tk);
        write(io, RED, "Error", RESET, ": Expected primary expression.\n");
        print_line(mod, io, e.data.expected_primary.tk);
        break;
    case ERR_NO_FUNDECL_LPAREN:
        print_loc(mod, io, e.data.no_fundecl_lparen.tk);
        write(io, RED, "Error", RESET, ": Expected opening parenthesis ('(') after function name in function declaration.\n");
        print_line(mod, io, e.data.no_fundecl_lparen.tk);
        break;
    case ERR_NO_CALL_SEP:
        print_loc(mod, io, e.data.no_call_sep.tk);
        write(io, RED, "Error", RESET, ": Expected comma (',') or closing parenthesis (')') in call expression arguments.\n");
        print_line(mod, io, e.data.no_call_sep.tk);
        break;
    case ERR_NO_SLICE_RBRACKET:
        print_loc(mod, io, e.data.no_slice_rbracket.tk);
        write(io, RED, "Error", RESET, ": Expected right bracket (']') in slice expression.\n");
        print_line(mod, io, e.data.no_slice_rbracket.tk);
        break;
    case ERR_NO_ELSE_IN_IF:
        print_loc(mod, io, e.data.no_else_in_if.tk);
        write(io, RED, "Error", RESET, ": Expected 'else' keyword in conditional expression.\n");
        print_line(mod, io, e.data.no_else_in_if.tk);
        break;
    case ERR_NO_SLICE_SEP:
        print_loc(mod, io, e.data.no_slice_sep.tk);
        write(io, RED, "Error", RESET, ": Expected right bracket (']') or slice operator (':') in index expression.\n");
        print_line(mod, io, e.data.no_slice_sep.tk);
        break;
    case ERR_NO_CLOSING_PAREN:
        print_loc(mod, io, e.data.no_closing_paren.tk);
        write(io, RED, "Error", RESET, ": Expected closing parenthesis (')').\n");
        print_line(mod, io, e.data.no_closing_paren.tk);
        break;
    case ERR_NO_BLOCK:
        print_loc(mod, io, e.data.no_block.tk);
        write(io, RED, "Error", RESET, ": Expected block.\n");
        print_line(mod, io, e.data.no_block.tk);
        break;
    case ERR_NO_SET_SEP:
        print_loc(mod, io, e.data.no_set_sep.tk);
        write(io, RED, "Error", RESET, ": Expected comma (',') or closing brace ('}') in set expression.\n");
        print_line(mod, io, e.data.no_set_sep.tk);
        break;
    case ERR_NO_ARRAY_SEP:
        print_loc(mod, io, e.data.no_array_sep.tk);
        write(io, RED, "Error", RESET, ": Expected comma (',') or closing bracket (']') in array expression.\n");
        print_line(mod, io, e.data.no_array_sep.tk);
        break;
    case ERR_NO_NEWLINE:
        print_loc(mod, io, e.data.no_newline.tk);
        write(io, RED, "Error", RESET, ": Expected line break after statement.\n");
        print_line(mod, io, e.data.no_newline.tk);
        break;
    case ERR_NO_FORLOOP_IN:
        print_loc(mod, io, e.data.no_for_loop_in.tk);
        write(io, RED, "Error", RESET, ": Expected 'in' keyword after variable bindings in for loop.\n");
        print_line(mod, io, e.data.no_for_loop_in.tk);
        break;
    case ERR_NO_NEWLINE_OR_AS:
        print_loc(mod, io, e.data.no_newline_or_as.tk);
        write(io, RED, "Error", RESET, ": Expected line break or 'as' keyword in use statement.\n");
        print_line(mod, io, e.data.no_newline_or_as.tk);
        break;
    case ERR_NO_USE_ALIAS:
        print_loc(mod, io, e.data.no_use_alias.tk);
        write(io, RED, "Error", RESET, ": Module alias in use statement must be an identifier.\n");
        print_line(mod, io, e.data.no_use_alias.tk);
        break;
    case ERR_NO_ALIAS_COLON:
        print_loc(mod, io, e.data.no_alias_colon.tk);
        write(io, RED, "Error", RESET, ": Expected colon (':') in alias declaration.\n");
        print_line(mod, io, e.data.no_alias_colon.tk);
        break;
    case ERR_NO_ALIAS_IDENT:
        print_loc(mod, io, e.data.no_alias_ident.tk);
        write(io, RED, "Error", RESET, ": Type alias name must be an identifier.\n");
        print_line(mod, io, e.data.no_alias_ident.tk);
        break;
    case ERR_NO_TYPE_COLON_NL:
        print_loc(mod, io, e.data.no_type_colon_nl.tk);
        write(io, RED, "Error", RESET, ": Expected colon (':') or line break in type declaration.\n");
        print_line(mod, io, e.data.no_type_colon_nl.tk);
        break;
    case ERR_NO_TYPEDECL_PARAM:
        print_loc(mod, io, e.data.no_typedecl_param.tk);
        write(io, RED, "Error", RESET, ": Type parameter names must be identifiers.\n");
        print_line(mod, io, e.data.no_typedecl_param.tk);
        break;
    case ERR_NO_TYPEDECL_NAME:
        print_loc(mod, io, e.data.no_typedecl_name.tk);
        write(io, RED, "Error", RESET, ": Expected identifier in type declaration.\n");
        print_line(mod, io, e.data.no_typedecl_name.tk);
        break;
    case ERR_NO_CASE_COLON_NL:
        print_loc(mod, io, e.data.no_case_colon_nl.tk);
        write(io, RED, "Error", RESET, ": Expected colon (':') or line break in case.\n");
        print_line(mod, io, e.data.no_case_colon_nl.tk);
        break;
    case ERR_NO_MODULE_NAME:
        print_loc(mod, io, e.data.no_module_name.tk);
        write(io, RED, "Error", RESET, ": Expected identifier in module declaration.\n");
        print_line(mod, io, e.data.no_module_name.tk);
        break;
    case ERR_NON_IDENT_IN_TVAR:
        print_loc(mod, io, e.data.non_ident_in_tvar.ast->pos);
        write(io, RED, "Error", RESET, ": Expected identifier in named type variable.\n");
        print_line(mod, io, e.data.non_ident_in_tvar.ast->pos);
        break;
    default:
        unreachable("Unknown error kind.");
        break;
    }
}

void print_errors(stream& io, bool verbose) {
    for (i64 i = 0; i < n_errors; i ++) print_error(io, verbose, errors[i]);
}