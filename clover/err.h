#ifndef BASIL_CLOVER_ERR_H
#define BASIL_CLOVER_ERR_H

#include "core/def.h"
#include "lib/utf.h"
#include "clover/clover.h"

struct SourcePos;
struct Token;
struct AST;
struct Type;

enum ErrorKind {
    NO_CLOVER_ERROR, DECODE_ERROR, LEX_ERROR, PARSE_ERROR, TYPE_ERROR, CODEGEN_ERROR
};

// Decoder
void utf8_format_error(Module* mod, UnicodeError err, i32 pos);

// Lexer
void unexpected_char_error(Module* mod, i32 pos, i32 line, i32 len, i16 col, rune ch);
void letter_in_number_error(Module* mod, i32 pos, i32 line, i32 len, i16 col, rune ch);
void no_newline_in_string_error(Module* mod, i32 pos, i32 line, i32 len, i16 col);
void no_newline_in_char_error(Module* mod, i32 pos, i32 line, i32 len, i16 col);
void unknown_escape_error(Module* mod, i32 pos, i32 line, i32 len, i16 col, rune ch);
void unexpected_eof_error(Module* mod, i32 pos, i32 line, i32 len, i16 col);

// Parser
void no_paramlist_sep_error(Module* mod, const Token& tk);
void no_decl_newline_error(Module* mod, const Token& tk);
void no_vardecl_colon_error(Module* mod, const Token& tk);
void no_vardecl_sep_error(Module* mod, const Token& tk);
void expected_primary_error(Module* mod, const Token& tk);
void no_fundecl_lparen_error(Module* mod, const Token& tk);
void no_call_sep_error(Module* mod, const Token& tk);
void no_slice_rbracket_error(Module* mod, const Token& tk);
void no_else_in_if_error(Module* mod, const Token& tk);
void no_slice_sep_error(Module* mod, const Token& tk);
void no_closing_paren_error(Module* mod, const Token& tk);
void no_block_error(Module* mod, const Token& tk);
void no_set_sep_error(Module* mod, const Token& tk);
void no_array_sep_error(Module* mod, const Token& tk);
void no_newline_error(Module* mod, const Token& tk);
void no_for_loop_in_error(Module* mod, const Token& tk);
void no_newline_or_as_error(Module* mod, const Token& tk);
void no_use_alias_error(Module* mod, const Token& tk);
void no_alias_colon_error(Module* mod, const Token& tk);
void no_alias_ident_error(Module* mod, const Token& tk);
void no_type_colon_nl_error(Module* mod, const Token& tk);
void no_typedecl_param_error(Module* mod, const Token& tk);
void no_typedecl_name_error(Module* mod, const Token& tk);
void no_case_colon_nl_error(Module* mod, const Token& tk);
void no_module_name_error(Module* mod, const Token& tk);
void non_ident_in_tvar_error(Module* mod, AST* var);
void empty_sizeof_expr_error(Module* mod, const Token& tk);
void no_closing_sizeof_error(Module* mod, const Token& tk);
void no_newarray_sep_error(Module* mod, const Token& tk);

// Typechecker
void for_loop_var_inited_error(Module* mod, AST* var);
void no_module_found_error(Module* mod, AST* decl);
void case_in_non_union_error(Module* mod, AST* casedecl);
void inferred_field_type_error(Module* mod, AST* vardecl);
void field_in_non_struct_error(Module* mod, AST* vardecl);
void field_or_case_in_named_error(Module* mod, AST* decl);
void empty_case_decl_error(Module* mod, AST* decl);
void empty_type_decl_error(Module* mod, AST* decl);
void non_integer_array_dim_error(Module* mod, AST* dim);
void typename_in_slice_expr_error(Module* mod, AST* name);
void typename_in_ctor_error(Module* mod, AST* name);
void non_type_in_funtype_error(Module* mod, AST* non_type);
void non_type_in_var_decl_error(Module* mod, AST* non_type);

extern ErrorKind err_kind;
inline ErrorKind get_error() {
    return err_kind;
}

void print_errors(stream& io, bool verbose);

#endif