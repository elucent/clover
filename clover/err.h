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
void non_hex_digit_error(Module* mod, i32 pos, i32 line, i32 len, i16 col, rune ch);
void non_binary_digit_error(Module* mod, i32 pos, i32 line, i32 len, i16 col, rune ch);
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
void no_constdecl_colon_error(Module* mod, const Token& tk);
void no_constdecl_sep_error(Module* mod, const Token& tk);

struct Env;
struct Unary;
struct Binary;
struct TypeDecl;
struct CaseDecl;
struct VarDecl;
struct FunDecl;
struct Var;
struct Apply;
struct With;

// Typechecker
void incorrect_defer_env_error(Module* mod, Unary* defer, Env* env);
void no_such_module_error(Module* mod, AST* path);
void non_type_param_in_gentype_error(Module* mod, TypeDecl* decl, AST* param);
void dot_in_typedecl_name_error(Module* mod, TypeDecl* decl);
void no_name_in_typedecl_error(Module* mod, TypeDecl* decl);
void no_name_in_type_case_error(Module* mod, CaseDecl* decl);
void redefined_type_var_error(Module* mod, AST* tvar, AST* prev_decl);

void case_in_non_union_error(Module* mod, CaseDecl* decl);
void inferred_field_error(Module* mod, VarDecl* field);
void field_in_non_struct_error(Module* mod, VarDecl* field);
void unexpected_type_member_error(Module* mod, AST* type);
void redefined_pattern_error(Module* mod, Var* v, VarDecl* prev_decl);
void no_type_in_pattern_error(Module* mod, AST* ast);
void incompatible_pattern_types_error(Module* mod, Type* expected, AST* rhs);
void pattern_params_mismatch_error(Module* mod, AST* pattern, Type* type, i32 n, i32 expected);
void invalid_destructuring_error(Module* mod, AST* pattern, Type* type);
void undefined_var_error(Module* mod, Var* v);
void multiple_array_dims_error(Module* mod, AST* dims);
void unexpected_typename_error(Module* mod, AST* type);
void non_const_array_dim_error(Module* mod, AST* dim);
void expected_array_type_error(Module* mod, AST* type);
void expected_slice_type_error(Module* mod, AST* type);
void expected_ptr_type_error(Module* mod, AST* type);
void expected_fun_type_error(Module* mod, AST* type);
void undefined_typename_error(Module* mod, Var* t);
void expected_typename_error(Module* mod, AST* type);
void typename_in_slice_error(Module* mod, AST* type);
void gentype_params_mismatch_error(Module* mod, Apply* call, TypeDecl* decl, i32 n, i32 expected);
void nontype_gentype_param_error(Module* mod, AST* param, TypeDecl* decl);
void typename_in_ctor_error(Module* mod, Type* ctor, AST* ast);
void val_in_funtype_error(Module* mod, AST* ast);
void nontype_annotation_error(Module* mod, AST* ann);
void nontype_returntype_error(Module* mod, AST* ret);
void no_function_param_list_error(Module* mod, AST* proto);
void unexpected_type_in_fundecl_error(Module* mod, AST* proto);
void val_in_gentype_error(Module* mod, AST* ast);
void typename_in_genctor_error(Module* mod, AST* ctor, AST* ast);
void non_const_in_decl_error(Module* mod, AST* ast);

void return_outside_fun_error(Module* mod, Unary* ret, Env* env);
void invalid_with_env_error(Module* mod, With* with);
void inference_mismatch_error(Module* mod, AST* ast, Type* type, Type* expected);
void incompatible_operand_error(Module* mod, AST* ast, Type* type, Type* expected);
void non_arithmetic_type_error(Module* mod, AST* ast, Type* type);
void non_integral_type_error(Module* mod, AST* ast, Type* type);
void non_comparable_type_error(Module* mod, AST* ast, Type* type);
void non_equality_type_error(Module* mod, AST* ast, Type* type);
void incompatible_assignment_error(Module* mod, AST* ast, Type* type, Type* expected);
void non_pointer_dereference_error(Module* mod, AST* ast, Type* type);
void invalid_delete_error(Module* mod, AST* ast, Type* type);
void non_integral_newarray_dim_error(Module* mod, AST* ast, Type* type);
void invalid_sizeof_operand_error(Module* mod, AST* ast, Type* type);
void invalid_dot_expression_error(Module* mod, AST* ast);
void non_indexable_type_error(Module* mod, AST* ast, Type* type);
void non_sliceable_type_error(Module* mod, AST* ast, Type* type);
void gencall_mismatch_error(Module* mod, AST* call, FunDecl* decl, i32 n, i32 expected);
void call_mismatch_error(Module* mod, AST* call, AST* decl, i32 n, i32 expected);
void non_function_type_error(Module* mod, AST* ast, Type* type);
void incompatible_argument_error(Module* mod, AST* ast, Type* type, Type* expected);
void empty_arguments_error(Module* mod, AST* call, Type* funtype);
void non_array_type_error(Module* mod, AST* array, Type* type);
void incompatible_element_error(Module* mod, AST* element, Type* etype, Type* expected);
void incompatible_initializer_error(Module* mod, AST* init, Type* type, Type* expected);
void ctor_mismatch_error(Module* mod, AST* ctor, Type* ctype, i32 n, i32 expected);
void cast_mismatch_error(Module* mod, AST* ctor, Type* ctype, i32 n, i32 expected);
void invalid_cast_error(Module* mod, AST* ast, Type* type, Type* dest);
void incompatible_ctor_param_error(Module* mod, AST* ast, Type* type, Type* expected);
void union_ctor_error(Module* mod, AST* ctor, Type* type);
void non_boolean_type_error(Module* mod, AST* ast, Type* type);
void incompatible_return_error(Module* mod, AST* ast, Type* type, Type* expected);
void inaccessible_member_error(Module* mod, Var* var, AST* local, AST* def);
void size_of_non_concrete_error(Module* mod, AST* ast, Type* type);
void inference_failure_error(Module* mod, AST* ast);
void no_high_in_ptr_slice_error(Module* mod, AST* ast, Type* type);
void tautological_compare_error(Module* mod, Binary* ast, AST* smaller, Type* larger, bool result);

// Codegen
void cyclic_dependencies_error(Module* mod);

extern ErrorKind err_kind;
inline ErrorKind get_error() {
    return err_kind;
}

void print_errors(stream& io, bool verbose);

#endif