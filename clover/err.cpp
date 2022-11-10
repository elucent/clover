#include "clover/err.h"
#include "clover/lex.h"
#include "clover/ast.h"

ErrorKind err_kind = NO_CLOVER_ERROR;

enum ErrorType : i8 {
    ERR_UTF8_FORMAT,        // Source is not valid UTF-8

    ERR_UNEXPECTED_CHAR,    // Unexpected character in lexer input.
    ERR_LETTER_IN_NUMBER,   // Unexpected letter in numeric constant.
    ERR_NON_HEX_DIGIT,      // Non-hexadecimal digit in binary integer constant.
    ERR_NON_BINARY_DIGIT,   // Non-binary digit in binary integer constant.
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
    ERR_EMPTY_SIZEOF_EXPR,  // Sizeof expr with no expr inside.
    ERR_NO_CLOSING_SIZEOF,  // No closing pipe in sizeof expr.
    ERR_NO_NEWARRAY_SEP,    // No closing bracket in newarray expr.
    ERR_NO_CONSTDECL_COLON, // No colon in constant declaration.
    ERR_NO_CONSTDECL_SEP,   // No newline or comma after constant declaration.

    ERR_INCORRECT_DEFER_ENV,        // Incorrect env for defer statement.
    ERR_NO_SUCH_MODULE,             // Can't find module.
    ERR_NON_TYPE_PARAM_IN_GENTYPE,  // Generic type has invalid type parameter.
    ERR_DOT_IN_TYPEDECL_NAME,       // Dot expression in type name.
    ERR_NO_NAME_IN_TYPEDECL,        // Invalid name in type decl.
    ERR_NO_NAME_IN_TYPE_CASE,       // Invalid name in case type.
    ERR_REDEFINED_TYPE_VAR,         // Type var conflicts with a previous definition.

    ERR_CASE_IN_NON_UNION,          // Casedecl in non-union type.
    ERR_INFERRED_FIELD,             // Field with inferred type.
    ERR_FIELD_IN_NON_STRUCT,        // Field appears in non-struct type.
    ERR_UNEXPECTED_TYPE_MEMBER,     // Typename in members of struct or union type.
    ERR_REDEFINED_PATTERN,          // Redefined pattern variable.
    ERR_NO_TYPE_IN_PATTERN,         // Missing typename in pattern.
    ERR_INCOMPATIBLE_PATTERN_TYPES, // Conflicting types in pattern.
    ERR_PATTERN_PARAMS_MISMATCH,    // Incorrect number of pattern parameters.
    ERR_INVALID_DESTRUCTURING,      // Destructuring type that does not support destructuring.
    ERR_UNDEFINED_VAR,              // Use of undefined variable.
    ERR_MULTIPLE_ARRAY_DIMS,        // Multiple array dimensions in type expr.
    ERR_UNEXPECTED_TYPENAME,        // Unexpected typename in expression.
    ERR_NON_CONST_ARRAY_DIM,        // Array type constructed with non-constant dimension.
    ERR_EXPECTED_ARRAY_TYPE,        // Expected array type, but found something else.
    ERR_EXPECTED_SLICE_TYPE,        // Expected slice type, but found something else.
    ERR_EXPECTED_PTR_TYPE,          // Expected ptr type, but found something else.
    ERR_EXPECTED_FUN_TYPE,          // Expected function type, but found something else.
    ERR_UNDEFINED_TYPENAME,         // Use of undefined type name.
    ERR_EXPECTED_TYPENAME,          // Typename expression does not describe a type.
    ERR_TYPENAME_IN_SLICE,          // Occurrence of type in slice expression.
    ERR_GENTYPE_PARAMS_MISMATCH,    // Incorrect number of parameters for generic type.
    ERR_NONTYPE_GENTYPE_PARAM,      // Non-type parameter in generic type instantiation.
    ERR_TYPENAME_IN_CTOR,           // Occurrence of typename in constructor expression.
    ERR_VAL_IN_FUNTYPE,             // Non-type in function type expression.
    ERR_NONTYPE_ANNOTATION,         // Variable annotation that is not a type.
    ERR_NONTYPE_RETURNTYPE,         // Return type annotation that is not a type.
    ERR_NO_FUNCTION_PARAM_LIST,     // Function was somehow declared without a parameter list.
    ERR_UNEXPECTED_TYPE_IN_FUNDECL, // Function name is a type name.
    ERR_VAL_IN_GENTYPE,             // Value in generic type constructor.
    ERR_TYPENAME_IN_GENCTOR,        // Unexpected typename in generic type constructor.
    ERR_NON_CONST_IN_DECL,          // Non-constant expression in const declaration.

    ERR_RETURN_OUTSIDE_FUN,         // Return statement outside of a function scope.
    ERR_INVALID_WITH_ENV,           // No valid environment found for a with statement.
    ERR_INFERENCE_MISMATCH,         // Inferred type and actual type of expression are in conflict.
    ERR_INCOMPATIBLE_OPERAND,       // Operand to expression is incompatible with expected type.
    ERR_NON_ARITHMETIC_TYPE,        // Expected arithmetic type.
    ERR_NON_INTEGRAL_TYPE,          // Expected integral type.
    ERR_NON_COMPARABLE_TYPE,        // Expected type that supports comparison.
    ERR_NON_EQUALITY_TYPE,          // Expected type that supports equality.
    ERR_INCOMPATIBLE_ASSIGNMENT,    // The rhs of an assignment is incompatible with the lhs.
    ERR_NON_POINTER_DEREFERENCE,    // Attempted dereference of non-pointer value.
    ERR_INVALID_DELETE,             // Attempted delete of non-pointer, non-slice value.
    ERR_NON_INTEGRAL_NEWARRAY_DIM,  // Newarray without an integral size.
    ERR_INVALID_SIZEOF_OPERAND,     // Sizeof on a non-array, non-size value.
    ERR_INVALID_DOT_EXPRESSION,     // Dotted access where the lhs has no environment.
    ERR_NON_INDEXABLE_TYPE,         // Attempted indexing into non-slice, non-array value.
    ERR_NON_SLICEABLE_TYPE,         // Attempted slice of non-sliceable value.
    ERR_GENCALL_MISMATCH,           // Calling a generic function with incorrect number of arguments.
    ERR_CALL_MISMATCH,              // Calling a function with the incorrect number of arguments.
    ERR_NON_FUNCTION_TYPE,          // Expected function type.
    ERR_INCOMPATIBLE_ARGUMENT,      // Calling a function with an incompatible argument value.
    ERR_EMPTY_ARGUMENTS,            // Calling a non-unit function with zero arguments.
    ERR_NON_ARRAY_TYPE,             // Expected an array type.
    ERR_INCOMPATIBLE_ELEMENT,       // Elements of an array constructor are incompatible with each other.
    ERR_INCOMPATIBLE_INITIALIZER,   // Initial value of a variable is incompatible with inferred type.
    ERR_CTOR_MISMATCH,              // Calling constructor with wrong number of arguments.
    ERR_CAST_MISMATCH,              // Casting with wrong number of arguments.
    ERR_INVALID_CAST,               // Casting to a type with invalid arguments.
    ERR_INCOMPATIBLE_CTOR_PARAM,    // Parameter to ctor is incompatible with expected type.
    ERR_UNION_CTOR,                 // Constructing a union on its own is not allowed.
    ERR_NON_BOOLEAN_TYPE,           // Expected a boolean.
    ERR_INCOMPATIBLE_RETURN,        // Return statement returns value incompatible with enclosing function.
    ERR_INACCESSIBLE_MEMBER,        // Member variable is inaccessible from local scope.
    ERR_SIZEOF_NON_CONCRETE,        // Tried to take size of non-concrete type.
    ERR_INFERENCE_FAILURE,          // Couldn't infer valid type.
    ERR_NO_HIGH_IN_PTR_SLICE,       // Missing upper bound in pointer slice.

    ERR_CIRCULAR_DEPENDENCIES,      // A cycle was detected in the program's dependency graph.
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
        struct NonHexDigit   { CODE = ERR_LETTER_IN_NUMBER; i32 pos, line, len; i16 col; rune ch; } non_hex_digit;
        struct NonBinaryDigit   { CODE = ERR_LETTER_IN_NUMBER; i32 pos, line, len; i16 col; rune ch; } non_binary_digit;
        struct StringNewline    { CODE = ERR_STRING_NEWLINE; i32 pos, line, len; i16 col; } string_newline;
        struct CharNewline      { CODE = ERR_CHAR_NEWLINE; i32 pos, line, len; i16 col; } char_newline;
        struct UnknownEscape    { CODE = ERR_UNKNOWN_ESCAPE; i32 pos, line, len; i16 col; rune ch; } unknown_escape;
        struct UnexpectedEOF    { CODE = ERR_UNEXPECTED_EOF; i32 pos, line, len; i16 col; } unexpected_eof;

        ErrorData(UnexpectedChar e): unexpected_char(e) {}
        ErrorData(LetterInNumber e): letter_in_number(e) {}
        ErrorData(NonHexDigit e): non_hex_digit(e) {}
        ErrorData(NonBinaryDigit e): non_binary_digit(e) {}
        ErrorData(StringNewline e): string_newline(e) {}
        ErrorData(CharNewline e): char_newline(e) {}
        ErrorData(UnknownEscape e): unknown_escape(e) {}
        ErrorData(UnexpectedEOF e): unexpected_eof(e) {}

        struct NoParamListSep   { CODE = ERR_NO_PARAMLIST_SEP; Token tk; } no_param_list_sep;
        struct NoDeclNewline    { CODE = ERR_NO_DECL_NL_SEP; Token tk; } no_decl_newline;
        struct NoVarDeclColon   { CODE = ERR_NO_VARDECL_COLON; Token tk; } no_vardecl_colon;
        struct NoVarDeclSep     { CODE = ERR_NO_VARDECL_SEP; Token tk; } no_vardecl_sep;
        struct NoConstDeclColon { CODE = ERR_NO_CONSTDECL_COLON; Token tk; } no_constdecl_colon;
        struct NoConstDeclSep   { CODE = ERR_NO_CONSTDECL_SEP; Token tk; } no_constdecl_sep;
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
        struct EmptySizeofExpr  { CODE = ERR_EMPTY_SIZEOF_EXPR; Token tk; } empty_sizeof_expr;
        struct NoClosingSizeof  { CODE = ERR_NO_CLOSING_SIZEOF; Token tk; } no_closing_sizeof;
        struct NoNewarraySep    { CODE = ERR_NO_NEWARRAY_SEP; Token tk; } no_newarray_sep;
        
        ErrorData(NoParamListSep e): no_param_list_sep(e) {}
        ErrorData(NoDeclNewline e): no_decl_newline(e) {}
        ErrorData(NoVarDeclColon e): no_vardecl_colon(e) {}
        ErrorData(NoVarDeclSep e): no_vardecl_sep(e) {}
        ErrorData(NoConstDeclColon e): no_constdecl_colon(e) {}
        ErrorData(NoConstDeclSep e): no_constdecl_sep(e) {}
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
        ErrorData(EmptySizeofExpr e): empty_sizeof_expr(e) {}
        ErrorData(NoClosingSizeof e): no_closing_sizeof(e) {}
        ErrorData(NoNewarraySep e): no_newarray_sep(e) {}

        struct IncorrectDeferEnv { CODE = ERR_INCORRECT_DEFER_ENV; Unary* defer; Env* env; } incorrect_defer_env;
        struct NoSuchModule { CODE = ERR_NO_SUCH_MODULE; AST* path; } no_such_module;
        struct NonTypeParamInGentype { CODE = ERR_NON_TYPE_PARAM_IN_GENTYPE; TypeDecl* decl; AST* param; } non_type_param_in_gentype;
        struct DotInTypedeclName { CODE = ERR_DOT_IN_TYPEDECL_NAME; TypeDecl* decl; } dot_in_typedecl_name;
        struct NoNameInTypedecl { CODE = ERR_NO_NAME_IN_TYPEDECL; TypeDecl* decl; } no_name_in_typedecl;
        struct NoNameInTypeCase { CODE = ERR_NO_NAME_IN_TYPE_CASE; CaseDecl* decl; } no_name_in_type_case;
        struct RedefinedTypeVar { CODE = ERR_REDEFINED_TYPE_VAR; AST* tvar; AST* prev_decl; } redefined_type_var;
        
        ErrorData(IncorrectDeferEnv e): incorrect_defer_env(e) {}
        ErrorData(NoSuchModule e): no_such_module(e) {}
        ErrorData(NonTypeParamInGentype e): non_type_param_in_gentype(e) {}
        ErrorData(DotInTypedeclName e): dot_in_typedecl_name(e) {}
        ErrorData(NoNameInTypedecl e): no_name_in_typedecl(e) {}
        ErrorData(NoNameInTypeCase e): no_name_in_type_case(e) {}
        ErrorData(RedefinedTypeVar e): redefined_type_var(e) {}

        struct CaseInNonUnion { CODE = ERR_CASE_IN_NON_UNION; CaseDecl* decl; } case_in_non_union;
        struct InferredField { CODE = ERR_INFERRED_FIELD; VarDecl* field; } inferred_field;
        struct FieldInNonStruct { CODE = ERR_FIELD_IN_NON_STRUCT; VarDecl* field; } field_in_non_struct;
        struct UnexpectedTypeMember { CODE = ERR_UNEXPECTED_TYPE_MEMBER; AST* member; } unexpected_type_member;
        struct RedefinedPattern { CODE = ERR_REDEFINED_PATTERN; Var* v; VarDecl* prev_decl; } redefined_pattern;
        struct NoTypeInPattern { CODE = ERR_NO_TYPE_IN_PATTERN; AST* ast; } no_type_in_pattern;
        struct IncompatiblePatternTypes { CODE = ERR_INCOMPATIBLE_PATTERN_TYPES; Type* expected; AST* ast; } incompatible_pattern_types;
        struct PatternParamsMismatch { CODE = ERR_PATTERN_PARAMS_MISMATCH; AST* pattern; Type* type; i32 n, expected; } pattern_params_mismatch;
        struct InvalidDestructuring { CODE = ERR_INVALID_DESTRUCTURING; AST* pattern; Type* type; } invalid_destructuring;
        struct UndefinedVar { CODE = ERR_UNDEFINED_VAR; Var* v; } undefined_var;
        struct MultipleArrayDims { CODE = ERR_MULTIPLE_ARRAY_DIMS; AST* dims; } multiple_array_dims;
        struct UnexpectedTypename { CODE = ERR_UNEXPECTED_TYPENAME; AST* type; } unexpected_typename;
        struct NonConstArrayDim { CODE = ERR_NON_CONST_ARRAY_DIM; AST* dim; } non_const_array_dim;
        struct ExpectedArrayType { CODE = ERR_EXPECTED_ARRAY_TYPE; AST* type; } expected_array_type;
        struct ExpectedSliceType { CODE = ERR_EXPECTED_SLICE_TYPE; AST* type; } expected_slice_type;
        struct ExpectedPtrType { CODE = ERR_EXPECTED_PTR_TYPE; AST* type; } expected_ptr_type;
        struct ExpectedFunType { CODE = ERR_EXPECTED_FUN_TYPE; AST* type; } expected_fun_type;
        struct UndefinedTypename { CODE = ERR_UNDEFINED_TYPENAME; Var* type; } undefined_typename;
        struct ExpectedTypename { CODE = ERR_EXPECTED_TYPENAME; AST* type; } expected_typename;
        struct TypenameInSlice { CODE = ERR_TYPENAME_IN_SLICE; AST* type; } typename_in_slice;
        struct GentypeParamsMismatch { CODE = ERR_GENTYPE_PARAMS_MISMATCH; Apply* call; TypeDecl* decl; i32 n, expected; } gentype_params_mismatch;
        struct NontypeGentypeParam { CODE = ERR_NONTYPE_GENTYPE_PARAM; AST* param; TypeDecl* decl; } nontype_gentype_param;
        struct TypenameInCtor { CODE = ERR_TYPENAME_IN_CTOR; Type* ctor; AST* ast; } typename_in_ctor;
        struct ValInFuntype { CODE = ERR_VAL_IN_FUNTYPE; AST* ast; } val_in_funtype;
        struct NontypeAnnotation { CODE = ERR_NONTYPE_ANNOTATION; AST* ann; } nontype_annotation;
        struct NontypeReturntype { CODE = ERR_NONTYPE_RETURNTYPE; AST* ret; } nontype_returntype;
        struct NoFunctionParamList { CODE = ERR_NO_FUNCTION_PARAM_LIST; AST* proto; } no_function_param_list;
        struct UnexpectedTypeInFundecl { CODE = ERR_UNEXPECTED_TYPE_IN_FUNDECL; AST* proto; } unexpected_type_in_fundecl;
        struct ValInGentype { CODE = ERR_VAL_IN_GENTYPE; AST* ast; } val_in_gentype;
        struct TypenameInGenctor { CODE = ERR_TYPENAME_IN_GENCTOR; AST* ctor; AST* ast; } typename_in_genctor;
        struct NonConstInDecl { CODE = ERR_NON_CONST_IN_DECL; AST* ast; } non_const_in_decl;

        ErrorData(CaseInNonUnion e): case_in_non_union(e) {}
        ErrorData(InferredField e): inferred_field(e) {}
        ErrorData(FieldInNonStruct e): field_in_non_struct(e) {}
        ErrorData(UnexpectedTypeMember e): unexpected_type_member(e) {}
        ErrorData(RedefinedPattern e): redefined_pattern(e) {}
        ErrorData(NoTypeInPattern e): no_type_in_pattern(e) {}
        ErrorData(IncompatiblePatternTypes e): incompatible_pattern_types(e) {}
        ErrorData(PatternParamsMismatch e): pattern_params_mismatch(e) {}
        ErrorData(InvalidDestructuring e): invalid_destructuring(e) {}
        ErrorData(UndefinedVar e): undefined_var(e) {}
        ErrorData(MultipleArrayDims e): multiple_array_dims(e) {}
        ErrorData(UnexpectedTypename e): unexpected_typename(e) {}
        ErrorData(NonConstArrayDim e): non_const_array_dim(e) {}
        ErrorData(ExpectedArrayType e): expected_array_type(e) {}
        ErrorData(ExpectedSliceType e): expected_slice_type(e) {}
        ErrorData(ExpectedPtrType e): expected_ptr_type(e) {}
        ErrorData(ExpectedFunType e): expected_fun_type(e) {}
        ErrorData(UndefinedTypename e): undefined_typename(e) {}
        ErrorData(ExpectedTypename e): expected_typename(e) {}
        ErrorData(TypenameInSlice e): typename_in_slice(e) {}
        ErrorData(GentypeParamsMismatch e): gentype_params_mismatch(e) {}
        ErrorData(NontypeGentypeParam e): nontype_gentype_param(e) {}
        ErrorData(TypenameInCtor e): typename_in_ctor(e) {}
        ErrorData(ValInFuntype e): val_in_funtype(e) {}
        ErrorData(NontypeAnnotation e): nontype_annotation(e) {}
        ErrorData(NontypeReturntype e): nontype_returntype(e) {}
        ErrorData(NoFunctionParamList e): no_function_param_list(e) {}
        ErrorData(UnexpectedTypeInFundecl e): unexpected_type_in_fundecl(e) {}
        ErrorData(ValInGentype e): val_in_gentype(e) {}
        ErrorData(TypenameInGenctor e): typename_in_genctor(e) {}
        ErrorData(NonConstInDecl e): non_const_in_decl(e) {}

        struct ReturnOutsideFun { CODE = ERR_RETURN_OUTSIDE_FUN; Unary* ret; Env* env; } return_outside_fun;
        struct InvalidWithEnv { CODE = ERR_INVALID_WITH_ENV; With* with; } invalid_with_env;
        struct InferenceMismatch { CODE = ERR_INFERENCE_MISMATCH; AST* ast; Type *type, *expected; } inference_mismatch;
        struct IncompatibleOperand { CODE = ERR_INCOMPATIBLE_OPERAND; AST* ast; Type *type, *expected; } incompatible_operand;
        struct NonArithmeticType { CODE = ERR_NON_ARITHMETIC_TYPE; AST* ast; Type* type; } non_arithmetic_type;
        struct NonIntegralType { CODE = ERR_NON_INTEGRAL_TYPE; AST* ast; Type* type; } non_integral_type;
        struct NonComparableType { CODE = ERR_NON_COMPARABLE_TYPE; AST* ast; Type* type; } non_comparable_type;
        struct NonEqualityType { CODE = ERR_NON_EQUALITY_TYPE; AST* ast; Type* type; } non_equality_type;
        struct IncompatibleAssignment { CODE = ERR_INCOMPATIBLE_ASSIGNMENT; AST* ast; Type *type, *expected; } incompatible_assignment;
        struct NonPointerDereference { CODE = ERR_NON_POINTER_DEREFERENCE; AST* ast; Type* type; } non_pointer_dereference;
        struct InvalidDelete { CODE = ERR_INVALID_DELETE; AST* ast; Type* type; } invalid_delete;
        struct NonIntegralNewarrayDim { CODE = ERR_NON_INTEGRAL_NEWARRAY_DIM; AST* ast; Type* type; } non_integral_newarray_dim;
        struct InvalidSizeofOperand { CODE = ERR_INVALID_SIZEOF_OPERAND; AST* ast; Type* type; } invalid_sizeof_operand;
        struct InvalidDotExpression { CODE = ERR_INVALID_DOT_EXPRESSION; AST* ast; } invalid_dot_expression;
        struct NonIndexableType { CODE = ERR_NON_INDEXABLE_TYPE; AST* ast; Type* type; } non_indexable_type;
        struct NonSliceableType { CODE = ERR_NON_SLICEABLE_TYPE; AST* ast; Type* type; } non_sliceable_type;
        struct GencallMismatch { CODE = ERR_GENCALL_MISMATCH; AST* call; FunDecl* decl; i32 n, expected; } gencall_mismatch;
        struct CallMismatch { CODE = ERR_CALL_MISMATCH; AST *call, *decl; i32 n, expected; } call_mismatch;
        struct NonFunctionType { CODE = ERR_NON_FUNCTION_TYPE; AST* ast; Type* type; } non_function_type;
        struct IncompatibleArgument { CODE = ERR_INCOMPATIBLE_ARGUMENT; AST* ast; Type *type, *dest; } incompatible_argument;
        struct EmptyArguments { CODE = ERR_EMPTY_ARGUMENTS; AST* call; Type* funtype; } empty_arguments;
        struct NonArrayType { CODE = ERR_NON_ARRAY_TYPE; AST* array; Type* type; } non_array_type;
        struct IncompatibleElement { CODE = ERR_INCOMPATIBLE_ELEMENT; AST* element; Type *etype, *dest; } incompatible_element;
        struct IncompatibleInitializer { CODE = ERR_INCOMPATIBLE_INITIALIZER; AST* init; Type *type, *dest; } incompatible_initializer;
        struct CtorMismatch { CODE = ERR_CTOR_MISMATCH; AST* ctor; Type* ctype; i32 n, expected; } ctor_mismatch;
        struct CastMismatch { CODE = ERR_CAST_MISMATCH; AST* cast; Type* ctype; i32 n, expected; } cast_mismatch;
        struct InvalidCast { CODE = ERR_INVALID_CAST; AST* ast; Type *type, *dest; } invalid_cast;
        struct IncompatibleCtorParam { CODE = ERR_INCOMPATIBLE_CTOR_PARAM; AST* ast; Type *type, *dest; } incompatible_ctor_param;
        struct UnionCtor { CODE = ERR_UNION_CTOR; AST* ctor; Type* type; } union_ctor;
        struct NonBooleanType { CODE = ERR_NON_BOOLEAN_TYPE; AST* ast; Type* type; } non_boolean_type;
        struct IncompatibleReturn { CODE = ERR_INCOMPATIBLE_RETURN; AST* ast; Type *type, *dest; } incompatible_return;
        struct InaccessibleMember { CODE = ERR_INACCESSIBLE_MEMBER; Var *var; AST *local, *def; } inaccessible_member;
        struct SizeofNonConcrete { CODE = ERR_SIZEOF_NON_CONCRETE; AST* ast; Type* type; } sizeof_non_concrete;
        struct InferenceFailure { CODE = ERR_INFERENCE_FAILURE; AST* ast; } inference_failure;
        struct NoHighInPtrSlice { CODE = ERR_NO_HIGH_IN_PTR_SLICE; AST* ast; Type* type; } no_high_in_ptr_slice;
        
        ErrorData(ReturnOutsideFun e): return_outside_fun(e) {}
        ErrorData(InvalidWithEnv e): invalid_with_env(e) {}
        ErrorData(InferenceMismatch e): inference_mismatch(e) {}
        ErrorData(IncompatibleOperand e): incompatible_operand(e) {}
        ErrorData(NonArithmeticType e): non_arithmetic_type(e) {}
        ErrorData(NonIntegralType e): non_integral_type(e) {}
        ErrorData(NonComparableType e): non_comparable_type(e) {}
        ErrorData(NonEqualityType e): non_equality_type(e) {}
        ErrorData(IncompatibleAssignment e): incompatible_assignment(e) {}
        ErrorData(NonPointerDereference e): non_pointer_dereference(e) {}
        ErrorData(InvalidDelete e): invalid_delete(e) {}
        ErrorData(NonIntegralNewarrayDim e): non_integral_newarray_dim(e) {}
        ErrorData(InvalidSizeofOperand e): invalid_sizeof_operand(e) {}
        ErrorData(InvalidDotExpression e): invalid_dot_expression(e) {}
        ErrorData(NonIndexableType e): non_indexable_type(e) {}
        ErrorData(NonSliceableType e): non_sliceable_type(e) {}
        ErrorData(GencallMismatch e): gencall_mismatch(e) {}
        ErrorData(CallMismatch e): call_mismatch(e) {}
        ErrorData(NonFunctionType e): non_function_type(e) {}
        ErrorData(IncompatibleArgument e): incompatible_argument(e) {}
        ErrorData(EmptyArguments e): empty_arguments(e) {}
        ErrorData(NonArrayType e): non_array_type(e) {}
        ErrorData(IncompatibleElement e): incompatible_element(e) {}
        ErrorData(IncompatibleInitializer e): incompatible_initializer(e) {}
        ErrorData(CtorMismatch e): ctor_mismatch(e) {}
        ErrorData(CastMismatch e): cast_mismatch(e) {}
        ErrorData(InvalidCast e): invalid_cast(e) {}
        ErrorData(IncompatibleCtorParam e): incompatible_ctor_param(e) {}
        ErrorData(UnionCtor e): union_ctor(e) {}
        ErrorData(NonBooleanType e): non_boolean_type(e) {}
        ErrorData(IncompatibleReturn e): incompatible_return(e) {}
        ErrorData(InaccessibleMember e): inaccessible_member(e) {}
        ErrorData(SizeofNonConcrete e): sizeof_non_concrete(e) {}
        ErrorData(InferenceFailure e): inference_failure(e) {}
        ErrorData(NoHighInPtrSlice e): no_high_in_ptr_slice(e) {}
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

void type_error() {
    if (err_kind < TYPE_ERROR) err_kind = TYPE_ERROR;
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

void non_hex_digit_error(Module* mod, i32 pos, i32 line, i32 len, i16 col, rune ch) {
    lex_error();
    push_error<Error::ErrorData::NonHexDigit>(mod, pos, line, len, col, ch);
}

void non_binary_digit_error(Module* mod, i32 pos, i32 line, i32 len, i16 col, rune ch) {
    lex_error();
    push_error<Error::ErrorData::NonBinaryDigit>(mod, pos, line, len, col, ch);
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

void no_constdecl_colon_error(Module* mod, const Token& tk) {
    parse_error();
    push_error<Error::ErrorData::NoConstDeclColon>(mod, tk);
}

void no_constdecl_sep_error(Module* mod, const Token& tk) {
    parse_error();
    push_error<Error::ErrorData::NoConstDeclSep>(mod, tk);
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

void empty_sizeof_expr_error(Module* mod, const Token& tk) {
    parse_error();
    push_error<Error::ErrorData::EmptySizeofExpr>(mod, tk);
}

void no_closing_sizeof_error(Module* mod, const Token& tk) {
    parse_error();
    push_error<Error::ErrorData::NoClosingSizeof>(mod, tk);
}

void no_newarray_sep_error(Module* mod, const Token& tk) {
    parse_error();
    push_error<Error::ErrorData::NoNewarraySep>(mod, tk);
}

// env construction

void incorrect_defer_env_error(Module* mod, Unary* defer, Env* env) {
    type_error();
    push_error<Error::ErrorData::IncorrectDeferEnv>(mod, defer, env);
}

void no_such_module_error(Module* mod, AST* path) {
    type_error();
    push_error<Error::ErrorData::NoSuchModule>(mod, path);
}

void non_type_param_in_gentype_error(Module* mod, TypeDecl* decl, AST* param) {
    type_error();
    push_error<Error::ErrorData::NonTypeParamInGentype>(mod, decl, param);
}

void dot_in_typedecl_name_error(Module* mod, TypeDecl* decl) {
    type_error();
    push_error<Error::ErrorData::DotInTypedeclName>(mod, decl);
}

void no_name_in_typedecl_error(Module* mod, TypeDecl* decl) {
    type_error();
    push_error<Error::ErrorData::NoNameInTypedecl>(mod, decl);
}

void no_name_in_type_case_error(Module* mod, CaseDecl* decl) {
    type_error();
    push_error<Error::ErrorData::NoNameInTypeCase>(mod, decl);
}

void redefined_type_var_error(Module* mod, AST* tvar, AST* prev_decl) {
    type_error();
    push_error<Error::ErrorData::RedefinedTypeVar>(mod, tvar, prev_decl);
}

// type detection

void case_in_non_union_error(Module* mod, CaseDecl* decl) {
    type_error();
    push_error<Error::ErrorData::CaseInNonUnion>(mod, decl);
}

void inferred_field_error(Module* mod, VarDecl* field) {
    type_error();
    push_error<Error::ErrorData::InferredField>(mod, field);
}

void field_in_non_struct_error(Module* mod, VarDecl* field) {
    type_error();
    push_error<Error::ErrorData::FieldInNonStruct>(mod, field);
}

void unexpected_type_member_error(Module* mod, AST* type) {
    type_error();
    push_error<Error::ErrorData::UnexpectedTypeMember>(mod, type);
}

void redefined_pattern_error(Module* mod, Var* v, VarDecl* prev_decl) {
    type_error();
    push_error<Error::ErrorData::RedefinedPattern>(mod, v, prev_decl);
}

void no_type_in_pattern_error(Module* mod, AST* ast) {
    type_error();
    push_error<Error::ErrorData::NoTypeInPattern>(mod, ast);
}

void incompatible_pattern_types_error(Module* mod, Type* expected, AST* rhs) {
    type_error();
    push_error<Error::ErrorData::IncompatiblePatternTypes>(mod, expected, rhs);
}

void pattern_params_mismatch_error(Module* mod, AST* pattern, Type* type, i32 n, i32 expected) {
    type_error();
    push_error<Error::ErrorData::PatternParamsMismatch>(mod, pattern, type, n, expected);
}

void invalid_destructuring_error(Module* mod, AST* pattern, Type* type) {
    type_error();
    push_error<Error::ErrorData::InvalidDestructuring>(mod, pattern, type);
}

void undefined_var_error(Module* mod, Var* v) {
    type_error();
    push_error<Error::ErrorData::UndefinedVar>(mod, v);
}

void multiple_array_dims_error(Module* mod, AST* dims) {
    type_error();
    push_error<Error::ErrorData::MultipleArrayDims>(mod, dims);
}

void unexpected_typename_error(Module* mod, AST* type) {
    type_error();
    push_error<Error::ErrorData::UnexpectedTypename>(mod, type);
}

void non_const_array_dim_error(Module* mod, AST* dim) {
    type_error();
    push_error<Error::ErrorData::NonConstArrayDim>(mod, dim);
}

void expected_array_type_error(Module* mod, AST* type) {
    type_error();
    push_error<Error::ErrorData::ExpectedArrayType>(mod, type);
}

void expected_slice_type_error(Module* mod, AST* type) {
    type_error();
    push_error<Error::ErrorData::ExpectedSliceType>(mod, type);
}

void expected_ptr_type_error(Module* mod, AST* type) {
    type_error();
    push_error<Error::ErrorData::ExpectedPtrType>(mod, type);
}

void expected_fun_type_error(Module* mod, AST* type) {
    type_error();
    push_error<Error::ErrorData::ExpectedFunType>(mod, type);
}

void undefined_typename_error(Module* mod, Var* t) {
    type_error();
    push_error<Error::ErrorData::ExpectedTypename>(mod, t);
}

void expected_typename_error(Module* mod, AST* type) {
    type_error();
    push_error<Error::ErrorData::ExpectedTypename>(mod, type);
}

void typename_in_slice_error(Module* mod, AST* type) {
    type_error();
    push_error<Error::ErrorData::TypenameInSlice>(mod, type);
}

void gentype_params_mismatch_error(Module* mod, Apply* call, TypeDecl* decl, i32 n, i32 expected) {
    type_error();
    push_error<Error::ErrorData::GentypeParamsMismatch>(mod, call, decl, n, expected);
}

void nontype_gentype_param_error(Module* mod, AST* param, TypeDecl* decl) {
    type_error();
    push_error<Error::ErrorData::NontypeGentypeParam>(mod, decl);
}

void typename_in_ctor_error(Module* mod, Type* ctor, AST* ast) {
    type_error();
    push_error<Error::ErrorData::TypenameInCtor>(mod, ctor, ast);
}

void val_in_funtype_error(Module* mod, AST* ast) {
    type_error();
    push_error<Error::ErrorData::ValInFuntype>(mod, ast);
}

void nontype_annotation_error(Module* mod, AST* ann) {
    type_error();
    push_error<Error::ErrorData::NontypeAnnotation>(mod, ann);
}

void nontype_returntype_error(Module* mod, AST* ret) {
    type_error();
    push_error<Error::ErrorData::NontypeReturntype>(mod, ret);
}

void no_function_param_list_error(Module* mod, AST* proto) {
    type_error();
    push_error<Error::ErrorData::NoFunctionParamList>(mod, proto);
}

void unexpected_type_in_fundecl_error(Module* mod, AST* proto) {
    type_error();
    push_error<Error::ErrorData::UnexpectedTypeInFundecl>(mod, proto);
}

void val_in_gentype_error(Module* mod, AST* ast) {
    type_error();
    push_error<Error::ErrorData::ValInGentype>(mod, ast);
}

void typename_in_genctor_error(Module* mod, AST* ctor, AST* ast) {
    type_error();
    push_error<Error::ErrorData::TypenameInGenctor>(mod, ctor, ast);
}

void non_const_in_decl_error(Module* mod, AST* ast) {
    type_error();
    push_error<Error::ErrorData::NonConstInDecl>(mod, ast);
}

// typechecking and inference

void return_outside_fun_error(Module* mod, Unary* ret, Env* env) {
    type_error();
    push_error<Error::ErrorData::ReturnOutsideFun>(mod, ret, env);
}

void invalid_with_env_error(Module* mod, With* with) {
    type_error();
    push_error<Error::ErrorData::InvalidWithEnv>(mod, with);
}

void inference_mismatch_error(Module* mod, AST* ast, Type* type, Type* expected) {
    type_error();
    push_error<Error::ErrorData::InferenceMismatch>(mod, ast, type, expected);
}

void incompatible_operand_error(Module* mod, AST* ast, Type* type, Type* expected) {
    type_error();
    push_error<Error::ErrorData::IncompatibleOperand>(mod, ast, type, expected);
}

void non_arithmetic_type_error(Module* mod, AST* ast, Type* type) {
    type_error();
    push_error<Error::ErrorData::NonArithmeticType>(mod, ast, type);
}

void non_integral_type_error(Module* mod, AST* ast, Type* type) {
    type_error();
    push_error<Error::ErrorData::NonIntegralType>(mod, ast, type);
}

void non_comparable_type_error(Module* mod, AST* ast, Type* type) {
    type_error();
    push_error<Error::ErrorData::NonComparableType>(mod, ast, type);
}

void non_equality_type_error(Module* mod, AST* ast, Type* type) {
    type_error();
    push_error<Error::ErrorData::NonEqualityType>(mod, ast, type);
}

void incompatible_assignment_error(Module* mod, AST* ast, Type* type, Type* expected) {
    type_error();
    push_error<Error::ErrorData::IncompatibleAssignment>(mod, ast, type, expected);
}

void non_pointer_dereference_error(Module* mod, AST* ast, Type* type) {
    type_error();
    push_error<Error::ErrorData::NonPointerDereference>(mod, ast, type);
}

void invalid_delete_error(Module* mod, AST* ast, Type* type) {
    type_error();
    push_error<Error::ErrorData::InvalidDelete>(mod, ast, type);
}

void non_integral_newarray_dim_error(Module* mod, AST* ast, Type* type) {
    type_error();
    push_error<Error::ErrorData::NonIntegralNewarrayDim>(mod, ast, type);
}

void invalid_sizeof_operand_error(Module* mod, AST* ast, Type* type) {
    type_error();
    push_error<Error::ErrorData::InvalidSizeofOperand>(mod, ast, type);
}

void invalid_dot_expression_error(Module* mod, AST* ast) {
    type_error();
    push_error<Error::ErrorData::InvalidDotExpression>(mod, ast);
}

void non_indexable_type_error(Module* mod, AST* ast, Type* type) {
    type_error();
    push_error<Error::ErrorData::NonIndexableType>(mod, ast, type);
}

void non_sliceable_type_error(Module* mod, AST* ast, Type* type) {
    type_error();
    push_error<Error::ErrorData::NonSliceableType>(mod, ast, type);
}

void gencall_mismatch_error(Module* mod, AST* call, FunDecl* decl, i32 n, i32 expected) {
    type_error();
    push_error<Error::ErrorData::GencallMismatch>(mod, call, decl, n, expected);
}

void call_mismatch_error(Module* mod, AST* call, AST* decl, i32 n, i32 expected) {
    type_error();
    push_error<Error::ErrorData::CallMismatch>(mod, call, decl, n, expected);
}

void non_function_type_error(Module* mod, AST* ast, Type* type) {
    type_error();
    push_error<Error::ErrorData::NonFunctionType>(mod, ast, type);
}

void incompatible_argument_error(Module* mod, AST* ast, Type* type, Type* expected) {
    type_error();
    push_error<Error::ErrorData::IncompatibleArgument>(mod, ast, type, expected);
}

void empty_arguments_error(Module* mod, AST* call, Type* funtype) {
    type_error();
    push_error<Error::ErrorData::EmptyArguments>(mod, call, funtype);
}

void non_array_type_error(Module* mod, AST* array, Type* type) {
    type_error();
    push_error<Error::ErrorData::NonArrayType>(mod, array, type);
}

void incompatible_element_error(Module* mod, AST* element, Type* etype, Type* expected) {
    type_error();
    push_error<Error::ErrorData::IncompatibleElement>(mod, element, etype, expected);
}

void incompatible_initializer_error(Module* mod, AST* init, Type* type, Type* expected) {
    type_error();
    push_error<Error::ErrorData::IncompatibleInitializer>(mod, init, type, expected);
}

void ctor_mismatch_error(Module* mod, AST* ctor, Type* ctype, i32 n, i32 expected) {
    type_error();
    push_error<Error::ErrorData::CtorMismatch>(mod, ctor, ctype, n, expected);
}

void cast_mismatch_error(Module* mod, AST* ctor, Type* ctype, i32 n, i32 expected) {
    type_error();
    push_error<Error::ErrorData::CastMismatch>(mod, ctor, ctype, n, expected);
}

void invalid_cast_error(Module* mod, AST* ast, Type* type, Type* dest) {
    type_error();
    push_error<Error::ErrorData::InvalidCast>(mod, ast, type, dest);
}

void incompatible_ctor_param_error(Module* mod, AST* ast, Type* type, Type* expected) {
    type_error();
    push_error<Error::ErrorData::IncompatibleCtorParam>(mod, ast, type, expected);
}

void union_ctor_error(Module* mod, AST* ctor, Type* type) {
    type_error();
    push_error<Error::ErrorData::UnionCtor>(mod, ctor, type);
}

void non_boolean_type_error(Module* mod, AST* ast, Type* type) {
    type_error();
    push_error<Error::ErrorData::NonBooleanType>(mod, ast, type);
}

void incompatible_return_error(Module* mod, AST* ast, Type* type, Type* expected) {
    type_error();
    push_error<Error::ErrorData::IncompatibleReturn>(mod, ast, type, expected);
}

void inaccessible_member_error(Module* mod, Var* var, AST* local, AST* def) {
    type_error();
    push_error<Error::ErrorData::InaccessibleMember>(mod, var, local, def);
}

void size_of_non_concrete_error(Module* mod, AST* ast, Type* type) {
    type_error();
    push_error<Error::ErrorData::SizeofNonConcrete>(mod, ast, type);
}

void inference_failure_error(Module* mod, AST* ast) {
    type_error();
    push_error<Error::ErrorData::InferenceFailure>(mod, ast);
}

void no_high_in_ptr_slice_error(Module* mod, AST* ast, Type* type) {
    type_error();
    push_error<Error::ErrorData::NoHighInPtrSlice>(mod, ast, type);
}

static const i8* RED = "\e[0;91m";
static const i8* PURPLE = "\e[0;35m";
static const i8* RESET = "\e[0m";

void print_line(Module* mod, stream& io, i32 pos, i32 len, i16 col, const i8* color = RED) {
    i8* p = mod->bytes.text + pos;
    i8* begin = p - col;
    i8* end = begin;
    while (end < mod->bytes.text + mod->bytes.length && *end != '\n' && *end) end ++;
    write(io, "    ");
    for (i64 i = 0; i < end - begin; i ++) {
        if (i == col) write(io, color);
        if (i == col + len) write(io, RESET);
        write(io, begin[i]);
    }
    write(io, RESET, '\n');
    write(io, "    ", color);
    for (i64 i = 0; i < end - begin; i ++) {
        if (i >= col && i < col + len) write(io, '^');
        else write(io, ' ');
    }
    write(io, RESET, '\n');
}

void print_line(Module* mod, stream& io, SourcePos pos, const i8* color = RED) {
    print_line(mod, io, pos.start, pos.end - pos.start, pos.column, color);
}

void print_line(Module* mod, stream& io, AST* ast, const i8* color = RED) {
    if (ast->pos.start != 0 || ast->pos.end != 0)
        print_line(mod, io, ast->pos, color);
}

void print_loc(Module* mod, stream& io, i32 line, i32 col) {
    write(io, mod->path, ':', line + 1, ':', col + 1, ' ');
}

void print_loc(Module* mod, stream& io, SourcePos pos) {
    print_loc(mod, io, pos.line, pos.column);
}

void print_loc(Module* mod, stream& io, AST* ast) {
    if (ast->pos.start == 0 && ast->pos.end == 0) write(io, mod->path, ":?:? ");
    else print_loc(mod, io, ast->pos);
}

struct TPrint {
    Module* m;
    Type* t;
};

struct APrint {
    Module* m;
    AST* ast;
};

inline void write_impl(stream& io, const TPrint& p) {
    format(io, p.m, p.t);
}

inline void write_impl(stream& io, const APrint& p) {
    format(io, p.m, p.ast);
}

#define AP(x) APrint{mod, x}
#define TP(x) TPrint{mod, x}

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
    case ERR_NON_HEX_DIGIT:
        print_loc(mod, io, e.data.non_hex_digit.line, e.data.non_hex_digit.col);
        write(io, RED, "Error", RESET, ": Found non-hexadecimal digit '", e.data.non_hex_digit.ch, "' in hexadecimal constant.\n");
        print_line(mod, io, e.data.non_hex_digit.pos, e.data.non_hex_digit.len, e.data.non_hex_digit.col);
        break;
    case ERR_NON_BINARY_DIGIT:
        print_loc(mod, io, e.data.non_binary_digit.line, e.data.non_binary_digit.col);
        write(io, RED, "Error", RESET, ": Found non-binary character '", e.data.non_binary_digit.ch, "' in binary constant.\n");
        print_line(mod, io, e.data.non_binary_digit.pos, e.data.non_binary_digit.len, e.data.non_binary_digit.col);
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
    case ERR_NO_CONSTDECL_COLON:
        print_loc(mod, io, e.data.no_constdecl_colon.tk);
        write(io, RED, "Error", RESET, ": Expected colon (':') in constant declaration.\n");
        print_line(mod, io, e.data.no_constdecl_colon.tk);
        break;
    case ERR_NO_CONSTDECL_SEP:
        print_loc(mod, io, e.data.no_constdecl_sep.tk);
        write(io, RED, "Error", RESET, ": Expected colon (':') or comma (',') in constant declaration.\n");
        print_line(mod, io, e.data.no_constdecl_sep.tk);
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
    case ERR_EMPTY_SIZEOF_EXPR:
        print_loc(mod, io, e.data.empty_sizeof_expr.tk);
        write(io, RED, "Error", RESET, ": Expected expression within size operator.\n");
        print_line(mod, io, e.data.empty_sizeof_expr.tk);
        break;
    case ERR_NO_CLOSING_SIZEOF:
        print_loc(mod, io, e.data.no_closing_sizeof.tk);
        write(io, RED, "Error", RESET, ": Expected closing pipe ('|') after size operator.\n");
        print_line(mod, io, e.data.no_closing_sizeof.tk);
        break;
    case ERR_NO_NEWARRAY_SEP:
        print_loc(mod, io, e.data.no_newarray_sep.tk);
        write(io, RED, "Error", RESET, ": Expected closing bracket (']') after new array dimension.\n");
        print_line(mod, io, e.data.no_newarray_sep.tk);
        break;

    // env construction
    case ERR_INCORRECT_DEFER_ENV:
        print_loc(mod, io, e.data.incorrect_defer_env.defer);
        write(io, RED, "Error", RESET, ": Defer statement only permitted in global or function scope.\n");
        print_line(mod, io, e.data.incorrect_defer_env.defer);
        if (e.data.incorrect_defer_env.env->decl) {
            print_loc(mod, io, e.data.incorrect_defer_env.env->decl);
            write(io, PURPLE, "Note", RESET, ": Enclosing scope starts here.\n");
            print_line(mod, io, e.data.incorrect_defer_env.env->decl, PURPLE);
        }
        break;
    case ERR_NO_SUCH_MODULE:
        print_loc(mod, io, e.data.no_such_module.path);
        write(io, RED, "Error", RESET, ": Couldn't resolve module '");
        format(io, mod, e.data.no_such_module.path);
        write(io, "'.\n");
        print_line(mod, io, e.data.no_such_module.path);
        break;
    case ERR_NON_TYPE_PARAM_IN_GENTYPE:
        print_loc(mod, io, e.data.non_type_param_in_gentype.param);
        write(io, RED, "Error", RESET, ": Expected type parameter name in generic type.\n");
        print_line(mod, io, e.data.non_type_param_in_gentype.param);
        break;
    case ERR_DOT_IN_TYPEDECL_NAME:
        print_loc(mod, io, e.data.dot_in_typedecl_name.decl->name);
        write(io, RED, "Error", RESET, ": Dot expression not permitted in type name.\n");
        print_line(mod, io, e.data.dot_in_typedecl_name.decl->name);
        break;
    case ERR_NO_NAME_IN_TYPEDECL:
        print_loc(mod, io, e.data.no_name_in_typedecl.decl->name);
        write(io, RED, "Error", RESET, ": Expected type name.\n");
        print_line(mod, io, e.data.no_name_in_typedecl.decl->name);
        break;
    case ERR_REDEFINED_TYPE_VAR:
        print_loc(mod, io, e.data.redefined_type_var.tvar);
        write(io, RED, "Error", RESET, ": Type variable conflicts with prior definition.\n");
        print_line(mod, io, e.data.redefined_type_var.tvar);
        if (e.data.redefined_type_var.prev_decl) {
            print_loc(mod, io, e.data.redefined_type_var.prev_decl);
            write(io, PURPLE, "Note", RESET, ": Previously defined here.\n");
            print_line(mod, io, e.data.redefined_type_var.prev_decl, PURPLE);
        }
        break;

    // type detection
    case ERR_CASE_IN_NON_UNION:
        print_loc(mod, io, e.data.case_in_non_union.decl);
        write(io, RED, "Error", RESET, ": Unexpected case type in non-union type definition.\n");
        print_line(mod, io, e.data.case_in_non_union.decl);
        break;
    case ERR_INFERRED_FIELD:
        print_loc(mod, io, e.data.inferred_field.field);
        write(io, RED, "Error", RESET, ": Field types must be explicitly specified.\n");
        print_line(mod, io, e.data.inferred_field.field);
        break;
    case ERR_FIELD_IN_NON_STRUCT:
        print_loc(mod, io, e.data.field_in_non_struct.field);
        write(io, RED, "Error", RESET, ": Unexpected field in non-struct type definition.\n");
        print_line(mod, io, e.data.field_in_non_struct.field);
        break;
    case ERR_UNEXPECTED_TYPE_MEMBER:
        print_loc(mod, io, e.data.unexpected_type_member.member);
        write(io, RED, "Error", RESET, ": Unexpected type name in member definitions.\n");
        print_line(mod, io, e.data.unexpected_type_member.member);
        break;
    case ERR_REDEFINED_PATTERN:
        print_loc(mod, io, e.data.redefined_pattern.v);
        write(io, RED, "Error", RESET, ": Redefinition of pattern variable.\n");
        print_line(mod, io, e.data.redefined_pattern.v);
        if (e.data.redefined_pattern.prev_decl) {
            print_loc(mod, io, e.data.redefined_pattern.prev_decl);
            write(io, PURPLE, "Note", RESET, ": Previously defined here.\n");
            print_line(mod, io, e.data.redefined_pattern.prev_decl, PURPLE);
        }
        break;
    case ERR_NO_TYPE_IN_PATTERN:
        print_loc(mod, io, e.data.no_type_in_pattern.ast);
        write(io, RED, "Error", RESET, ": Expected type name in pattern.\n");
        print_line(mod, io, e.data.no_type_in_pattern.ast);
        break;
    case ERR_INCOMPATIBLE_PATTERN_TYPES:
        print_loc(mod, io, e.data.incompatible_pattern_types.ast);
        write(io, RED, "Error", RESET, ": Incompatible pattern types. Expected '");
        format(io, mod, e.data.incompatible_pattern_types.expected);
        write(io, "', found '");
        format(io, mod, e.data.incompatible_pattern_types.ast->type);
        write(io, "'.\n");
        print_line(mod, io, e.data.incompatible_pattern_types.ast);
        break;
    case ERR_PATTERN_PARAMS_MISMATCH:
        print_loc(mod, io, e.data.pattern_params_mismatch.pattern);
        write(io, RED, "Error", RESET, ": Incorrect number of pattern fields. Expected ", e.data.pattern_params_mismatch.expected);
        write(io, " but found ", e.data.pattern_params_mismatch.n, ".\n");
        print_line(mod, io, e.data.pattern_params_mismatch.pattern);
        if (e.data.pattern_params_mismatch.type->env->decl) {
            AST* d = e.data.pattern_params_mismatch.type->env->decl;
            print_loc(mod, io, d);
            write(io, PURPLE, "Note", RESET, ": Type defined here.\n");
            print_line(mod, io, d, PURPLE);
        }
        break;
    case ERR_INVALID_DESTRUCTURING: {
        print_loc(mod, io, e.data.invalid_destructuring.pattern);
        write(io, RED, "Error", RESET, ": Type '");
        Type* conc = fullsimplify(*mod->typectx, e.data.invalid_destructuring.type);
        format(io, mod, conc);
        write(io, "' cannot be destructured.\n");
        print_line(mod, io, e.data.invalid_destructuring.pattern);
        break;
    }
    case ERR_UNDEFINED_VAR:
        print_loc(mod, io, e.data.undefined_var.v);
        write(io, RED, "Error", RESET, ": Undefined variable '");
        format(io, mod, e.data.undefined_var.v);
        write(io, "'.\n");
        print_line(mod, io, e.data.undefined_var.v);
        break;
    case ERR_MULTIPLE_ARRAY_DIMS:
        print_loc(mod, io, e.data.multiple_array_dims.dims);
        write(io, RED, "Error", RESET, ": Cannot define array type with multiple dimensions.\n");
        print_line(mod, io, e.data.multiple_array_dims.dims);
        break;
    case ERR_UNEXPECTED_TYPENAME:
        print_loc(mod, io, e.data.unexpected_typename.type);
        write(io, RED, "Error", RESET, ": Unexpected type name in expression.\n");
        print_line(mod, io, e.data.unexpected_typename.type);
        break;
    case ERR_NON_CONST_ARRAY_DIM:
        print_loc(mod, io, e.data.non_const_array_dim.dim);
        write(io, RED, "Error", RESET, ": Size of array type must be a constant integer.\n");
        print_line(mod, io, e.data.non_const_array_dim.dim);
        break;
    case ERR_EXPECTED_ARRAY_TYPE:
        print_loc(mod, io, e.data.expected_array_type.type);
        write(io, RED, "Error", RESET, ": Expected array type.\n");
        print_line(mod, io, e.data.expected_array_type.type);
        break;
    case ERR_EXPECTED_SLICE_TYPE:
        print_loc(mod, io, e.data.expected_slice_type.type);
        write(io, RED, "Error", RESET, ": Expected slice type.\n");
        print_line(mod, io, e.data.expected_slice_type.type);
        break;
    case ERR_EXPECTED_PTR_TYPE:
        print_loc(mod, io, e.data.expected_ptr_type.type);
        write(io, RED, "Error", RESET, ": Expected pointer type.\n");
        print_line(mod, io, e.data.expected_ptr_type.type);
        break;
    case ERR_EXPECTED_FUN_TYPE:
        print_loc(mod, io, e.data.expected_fun_type.type);
        write(io, RED, "Error", RESET, ": Expected function type.\n");
        print_line(mod, io, e.data.expected_fun_type.type);
        break;
    case ERR_UNDEFINED_TYPENAME:
        print_loc(mod, io, e.data.undefined_typename.type);
        write(io, RED, "Error", RESET, ": Undefined type name '");
        format(io, mod, e.data.undefined_typename.type);
        write(io, "'.\n");
        print_line(mod, io, e.data.undefined_typename.type);
        break;
    case ERR_EXPECTED_TYPENAME:
        print_loc(mod, io, e.data.expected_typename.type);
        write(io, RED, "Error", RESET, ": Type name does not actually describe a type.\n");
        print_line(mod, io, e.data.expected_typename.type);
        break;
    case ERR_TYPENAME_IN_SLICE:
        print_loc(mod, io, e.data.typename_in_slice.type);
        write(io, RED, "Error", RESET, ": Unexpected type name in slice expression.\n");
        print_line(mod, io, e.data.typename_in_slice.type);
        break;
    case ERR_GENTYPE_PARAMS_MISMATCH:
        print_loc(mod, io, e.data.gentype_params_mismatch.call);
        write(io, RED, "Error", RESET, ": Incorrect number of parameters for generic type. Expected '", e.data.gentype_params_mismatch.expected);
        write(io, "', found '", e.data.gentype_params_mismatch.n, "'.\n");
        print_line(mod, io, e.data.gentype_params_mismatch.call);
        if (e.data.gentype_params_mismatch.decl) {
            AST* d = e.data.gentype_params_mismatch.decl;
            print_loc(mod, io, d);
            write(io, PURPLE, "Note", RESET, ": Type defined here.\n");
            print_line(mod, io, d, PURPLE);
        }
        break;
    case ERR_NONTYPE_GENTYPE_PARAM:
        print_loc(mod, io, e.data.nontype_gentype_param.param);
        write(io, RED, "Error", RESET, ": Expected type name in generic type parameter list.\n");
        print_line(mod, io, e.data.nontype_gentype_param.param);
        if (e.data.nontype_gentype_param.decl) {
            AST* d = e.data.nontype_gentype_param.decl;
            print_loc(mod, io, d);
            write(io, PURPLE, "Note", RESET, ": Type defined here.\n");
            print_line(mod, io, d, PURPLE);
        }
        break;
    case ERR_TYPENAME_IN_CTOR: {
        print_loc(mod, io, e.data.typename_in_ctor.ast);
        write(io, RED, "Error", RESET, ": Unexpected type name in ");
        Type* conc = fullsimplify(*mod->typectx, e.data.typename_in_ctor.ctor);
        format(io, mod, conc);
        write(io, " constructor call.\n");
        print_line(mod, io, e.data.typename_in_ctor.ast);
        break;
    }
    case ERR_VAL_IN_FUNTYPE:
        print_loc(mod, io, e.data.val_in_funtype.ast);
        write(io, RED, "Error", RESET, ": Expected type name in function type.\n");
        print_line(mod, io, e.data.val_in_funtype.ast);
        break;
    case ERR_NONTYPE_ANNOTATION:
        print_loc(mod, io, e.data.nontype_annotation.ann);
        write(io, RED, "Error", RESET, ": Expected type name in variable type annotation.\n");
        print_line(mod, io, e.data.nontype_annotation.ann);
        break;
    case ERR_NONTYPE_RETURNTYPE:
        print_loc(mod, io, e.data.nontype_returntype.ret);
        write(io, RED, "Error", RESET, ": Expected type name in return type annotation.\n");
        print_line(mod, io, e.data.nontype_returntype.ret);
        break;
    case ERR_NO_FUNCTION_PARAM_LIST:
        print_loc(mod, io, e.data.no_function_param_list.proto);
        write(io, RED, "Error", RESET, ": Function defined without a parameter list.\n");
        print_line(mod, io, e.data.no_function_param_list.proto);
        break;
    case ERR_UNEXPECTED_TYPE_IN_FUNDECL:
        print_loc(mod, io, e.data.unexpected_type_in_fundecl.proto);
        write(io, RED, "Error", RESET, ": Unexpected type name in function declaration.\n");
        print_line(mod, io, e.data.unexpected_type_in_fundecl.proto);
        break;
    case ERR_VAL_IN_GENTYPE:
        print_loc(mod, io, e.data.val_in_gentype.ast);
        write(io, RED, "Error", RESET, ": Expected type name in generic type instantiation.\n");
        print_line(mod, io, e.data.val_in_gentype.ast);
        break;
    case ERR_TYPENAME_IN_GENCTOR:
        print_loc(mod, io, e.data.typename_in_genctor.ast);
        write(io, RED, "Error", RESET, ": Unexpected type name in generic type constructor.\n");
        print_line(mod, io, e.data.typename_in_genctor.ast);
        break;
    case ERR_NON_CONST_IN_DECL:
        print_loc(mod, io, e.data.non_const_in_decl.ast);
        write(io, RED, "Error", RESET, ": Found non-constant expression in declaration.\n");
        print_line(mod, io, e.data.non_const_in_decl.ast);
        break;

    // typechecking and inference
    case ERR_RETURN_OUTSIDE_FUN:
        print_loc(mod, io, e.data.return_outside_fun.ret);
        write(io, RED, "Error", RESET, ": Return statements are only valid within function scope.\n");
        print_line(mod, io, e.data.return_outside_fun.ret);
        if (e.data.return_outside_fun.env->decl) {
            AST* d = e.data.return_outside_fun.env->decl;
            print_loc(mod, io, d);
            write(io, PURPLE, "Note", RESET, ": Enclosing scope begins here.\n");
            print_line(mod, io, d, PURPLE);
        }
        break;
    case ERR_INVALID_WITH_ENV:
        print_loc(mod, io, e.data.invalid_with_env.with->bound);
        write(io, RED, "Error", RESET, ": Binding of invalid value in with statement.\n"); // wording on this isn't the clearest...
        print_line(mod, io, e.data.invalid_with_env.with->bound);
        break;
    case ERR_INFERENCE_MISMATCH:
        print_loc(mod, io, e.data.inference_mismatch.ast);
        write(io, RED, "Error", RESET, ": Type mismatch. Expected '", TP(e.data.inference_mismatch.expected), "', found '");
        write(io, TP(e.data.inference_mismatch.type), "'.\n");
        print_line(mod, io, e.data.inference_mismatch.ast);
        break;
    case ERR_INCOMPATIBLE_OPERAND:
        print_loc(mod, io, e.data.incompatible_operand.ast);
        write(io, RED, "Error", RESET, ": Incompatible operand. Expected '", TP(e.data.incompatible_operand.expected), "', found '");
        write(io, TP(e.data.incompatible_operand.type), "'.\n");
        print_line(mod, io, e.data.incompatible_operand.ast);
        break;
    case ERR_NON_ARITHMETIC_TYPE:
        print_loc(mod, io, e.data.non_arithmetic_type.ast);
        write(io, RED, "Error", RESET, ": Expected arithmetic type in expression, found '", TP(e.data.non_arithmetic_type.type), "'.\n");
        print_line(mod, io, e.data.non_arithmetic_type.ast);
        break;
    case ERR_NON_INTEGRAL_TYPE:
        print_loc(mod, io, e.data.non_integral_type.ast);
        write(io, RED, "Error", RESET, ": Expected integral type in expression, found '", TP(e.data.non_arithmetic_type.type), "'.\n");
        print_line(mod, io, e.data.non_integral_type.ast);
        break;
    case ERR_NON_COMPARABLE_TYPE:
        print_loc(mod, io, e.data.non_comparable_type.ast);
        write(io, RED, "Error", RESET, ": Expected comparable type in expression, found '", TP(e.data.non_arithmetic_type.type), "'.\n");
        print_line(mod, io, e.data.non_comparable_type.ast);
        break;
    case ERR_NON_EQUALITY_TYPE:
        print_loc(mod, io, e.data.non_equality_type.ast);
        write(io, RED, "Error", RESET, ": Expected equality-comparable type in expression, found '", TP(e.data.non_arithmetic_type.type), "'.\n");
        print_line(mod, io, e.data.non_equality_type.ast);
        break;
    case ERR_INCOMPATIBLE_ASSIGNMENT:
        print_loc(mod, io, e.data.incompatible_assignment.ast);
        write(io, RED, "Error", RESET, ": Can't assign value of type '", TP(e.data.incompatible_assignment.type));
        write(io, "' to '", TP(e.data.incompatible_assignment.expected), "'.\n");
        print_line(mod, io, e.data.incompatible_assignment.ast);
        break;
    case ERR_NON_POINTER_DEREFERENCE:
        print_loc(mod, io, e.data.non_pointer_dereference.ast);
        write(io, RED, "Error", RESET, ": Tried to dereference non-pointer type '", TP(e.data.non_pointer_dereference.type), "'.\n");
        print_line(mod, io, e.data.non_pointer_dereference.ast);
        break;
    case ERR_INVALID_DELETE:
        print_loc(mod, io, e.data.invalid_delete.ast);
        write(io, RED, "Error", RESET, ": Tried to delete non-deletable type '", TP(e.data.invalid_delete.type), "'.\n");
        print_line(mod, io, e.data.invalid_delete.ast);
        break;
    case ERR_NON_INTEGRAL_NEWARRAY_DIM:
        print_loc(mod, io, e.data.non_integral_newarray_dim.ast);
        write(io, RED, "Error", RESET, ": Size of new array expression must be an integer, but found '", TP(e.data.non_integral_newarray_dim.type), "' instead.\n");
        print_line(mod, io, e.data.non_integral_newarray_dim.ast);
        break;
    case ERR_INVALID_SIZEOF_OPERAND:
        print_loc(mod, io, e.data.invalid_sizeof_operand.ast);
        write(io, RED, "Error", RESET, ": Tried to find size of incompatible type '", TP(e.data.invalid_sizeof_operand.type), "'.\n");
        print_line(mod, io, e.data.invalid_sizeof_operand.ast);
        break;
    case ERR_INVALID_DOT_EXPRESSION:
        print_loc(mod, io, e.data.invalid_dot_expression.ast);
        write(io, RED, "Error", RESET, ": Expression has no accessible fields or scope.\n");
        print_line(mod, io, e.data.invalid_dot_expression.ast);
        break;
    case ERR_NON_INDEXABLE_TYPE:
        print_loc(mod, io, e.data.non_indexable_type.ast);
        write(io, RED, "Error", RESET, ": Tried to index non-indexable type '", TP(e.data.non_indexable_type.type), "'.\n");
        print_line(mod, io, e.data.non_indexable_type.ast);
        break;
    case ERR_NON_SLICEABLE_TYPE:
        print_loc(mod, io, e.data.non_sliceable_type.ast);
        write(io, RED, "Error", RESET, ": Expected type name in function type.\n");
        print_line(mod, io, e.data.non_sliceable_type.ast);
        break;
    case ERR_GENCALL_MISMATCH:
        print_loc(mod, io, e.data.gencall_mismatch.call);
        write(io, RED, "Error", RESET, ": Incorrect number of arguments to call generic function. Expected ");
        write(io, e.data.gencall_mismatch.expected, ", found ", e.data.gencall_mismatch.n, ".\n");
        print_line(mod, io, e.data.gencall_mismatch.call);
        if (e.data.gencall_mismatch.decl) {
            AST* d = e.data.gencall_mismatch.decl;
            print_loc(mod, io, d);
            write(io, PURPLE, "Note", RESET, ": Function defined here.\n");
            print_line(mod, io, d, PURPLE);
        }
        break;
    case ERR_CALL_MISMATCH:
        print_loc(mod, io, e.data.call_mismatch.call);
        write(io, RED, "Error", RESET, ": Incorrect number of arguments to function. Expected ");
        write(io, e.data.call_mismatch.expected, ", found ", e.data.call_mismatch.n, ".\n");
        print_line(mod, io, e.data.call_mismatch.call);
        if (e.data.call_mismatch.decl) {
            AST* d = e.data.call_mismatch.decl;
            print_loc(mod, io, d);
            write(io, PURPLE, "Note", RESET, ": Function defined here.\n");
            print_line(mod, io, d, PURPLE);
        }
        break;
    case ERR_NON_FUNCTION_TYPE:
        print_loc(mod, io, e.data.non_function_type.ast);
        write(io, RED, "Error", RESET, ": Expected function type in call, found '", TP(e.data.non_function_type.type), "'.\n");
        print_line(mod, io, e.data.non_function_type.ast);
        break;
    case ERR_INCOMPATIBLE_ARGUMENT:
        print_loc(mod, io, e.data.incompatible_argument.ast);
        write(io, RED, "Error", RESET, ": Incompatible argument in function call. Expected '", TP(e.data.incompatible_argument.dest));
        write(io, "', but given '", TP(e.data.incompatible_argument.type), "'.\n");
        print_line(mod, io, e.data.incompatible_argument.ast);
        break;
    case ERR_EMPTY_ARGUMENTS:
        print_loc(mod, io, e.data.empty_arguments.call);
        write(io, RED, "Error", RESET, ": No arguments provided in call to non-unit function type '", TP(e.data.empty_arguments.funtype), "'.\n");
        print_line(mod, io, e.data.empty_arguments.call);
        break;
    case ERR_NON_ARRAY_TYPE:
        print_loc(mod, io, e.data.non_array_type.array);
        write(io, RED, "Error", RESET, ": Expected array type in constructor, but found '", TP(e.data.non_array_type.type), "'.\n");
        print_line(mod, io, e.data.non_array_type.array);
        break;
    case ERR_INCOMPATIBLE_ELEMENT:
        print_loc(mod, io, e.data.incompatible_element.element);
        write(io, RED, "Error", RESET, ": Found incompatible element in array constructor. Expected '");
        write(io, TP(e.data.incompatible_element.dest), "', but found '", TP(e.data.incompatible_element.etype), "'.\n");
        print_line(mod, io, e.data.incompatible_element.element);
        break;
    case ERR_INCOMPATIBLE_INITIALIZER:
        print_loc(mod, io, e.data.incompatible_initializer.init);
        write(io, RED, "Error", RESET, ": Incompatible initial value in variable declaration. Expected '");
        write(io, TP(e.data.incompatible_initializer.dest), "', but found '", TP(e.data.incompatible_initializer.type), "'.\n");
        print_line(mod, io, e.data.incompatible_initializer.init);
        break;
    case ERR_CTOR_MISMATCH:
        print_loc(mod, io, e.data.ctor_mismatch.ctor);
        write(io, RED, "Error", RESET, ": Incorrect number of parameters to type constructor. Expected ");
        write(io, e.data.ctor_mismatch.expected, " but found ", e.data.ctor_mismatch.n, ".\n");
        print_line(mod, io, e.data.ctor_mismatch.ctor);
        if (e.data.ctor_mismatch.ctype->env->decl) {
            AST* d = e.data.ctor_mismatch.ctype->env->decl;
            print_loc(mod, io, d);
            write(io, PURPLE, "Note", RESET, ": Type defined here.\n");
            print_line(mod, io, d, PURPLE);
        }
        break;
    case ERR_CAST_MISMATCH:
        print_loc(mod, io, e.data.cast_mismatch.cast);
        write(io, RED, "Error", RESET, ": Incorrect number of parameters to type conversion. Expected ");
        write(io, e.data.cast_mismatch.expected, " but found ", e.data.cast_mismatch.n, ".\n");
        print_line(mod, io, e.data.cast_mismatch.cast);
        break;
    case ERR_INVALID_CAST:
        print_loc(mod, io, e.data.invalid_cast.ast);
        write(io, RED, "Error", RESET, ": Can't convert value of type '", TP(e.data.invalid_cast.type));
        write(io, "' to '", TP(e.data.invalid_cast.dest), "'.\n");
        print_line(mod, io, e.data.invalid_cast.ast);
        break;
    case ERR_INCOMPATIBLE_CTOR_PARAM:
        print_loc(mod, io, e.data.incompatible_ctor_param.ast);
        write(io, RED, "Error", RESET, ": Incompatible argument in constructor call. Expected '", TP(e.data.incompatible_ctor_param.dest));
        write(io, "', but found '", TP(e.data.incompatible_ctor_param.type), "'.\n");
        print_line(mod, io, e.data.incompatible_ctor_param.ast);
        break;
    case ERR_UNION_CTOR:
        print_loc(mod, io, e.data.union_ctor.ctor);
        write(io, RED, "Error", RESET, ": Values of union type can't be constructed directly. Construct one of the cases instead.\n");
        print_line(mod, io, e.data.union_ctor.ctor);
        break;
    case ERR_NON_BOOLEAN_TYPE:
        print_loc(mod, io, e.data.non_boolean_type.ast);
        write(io, RED, "Error", RESET, ": Expected boolean type in condition, but found '", TP(e.data.non_boolean_type.type), "'.\n");
        print_line(mod, io, e.data.non_boolean_type.ast);
        break;
    case ERR_INCOMPATIBLE_RETURN:
        print_loc(mod, io, e.data.incompatible_return.ast);
        write(io, RED, "Error", RESET, ": Returned value of type '", TP(e.data.incompatible_return.type));
        write(io, "' is incompatible with function return type '", TP(e.data.incompatible_return.dest), "'.\n");
        print_line(mod, io, e.data.incompatible_return.ast);
        break;
    case ERR_INACCESSIBLE_MEMBER:
        print_loc(mod, io, e.data.inaccessible_member.var);
        write(io, RED, "Error", RESET, ": Variable '", mod->interner->str(e.data.inaccessible_member.var->name), "' is inaccessible in current scope.\n");
        print_line(mod, io, e.data.inaccessible_member.var);
        if (e.data.inaccessible_member.local) {
            AST* d = e.data.inaccessible_member.local;
            print_loc(mod, io, d);
            write(io, PURPLE, "Note", RESET, ": Local scope begins here.\n");
            print_line(mod, io, d, PURPLE);
        }
        if (e.data.inaccessible_member.def) {
            AST* d = e.data.inaccessible_member.def;
            print_loc(mod, io, d);
            write(io, PURPLE, "Note", RESET, ": Variable defined here.\n");
            print_line(mod, io, d, PURPLE);
        }
        break;
    case ERR_SIZEOF_NON_CONCRETE:
        print_loc(mod, io, e.data.sizeof_non_concrete.ast);
        write(io, RED, "Error", RESET, ": Tried to take size of non-concrete type '", TP(e.data.incompatible_return.type), "'.\n");
        print_line(mod, io, e.data.sizeof_non_concrete.ast);
        break;
    case ERR_INFERENCE_FAILURE:
        print_loc(mod, io, e.data.inference_failure.ast);
        write(io, RED, "Error", RESET, ": Failed to infer valid type for expression.\n");
        print_line(mod, io, e.data.inference_failure.ast);
        break;
    case ERR_NO_HIGH_IN_PTR_SLICE:
        print_loc(mod, io, e.data.no_high_in_ptr_slice.ast);
        write(io, RED, "Error", RESET, ": Missing upper bound taking slice of pointer type '", TP(e.data.no_high_in_ptr_slice.type), "'.\n");
        print_line(mod, io, e.data.no_high_in_ptr_slice.ast);
        break;
    default:
        unreachable("Unknown error kind.");
        break;
    }
}

#undef TP
#undef AP

void print_errors(stream& io, bool verbose) {
    for (i64 i = 0; i < n_errors; i ++) print_error(io, verbose, errors[i]);
    n_errors = 0;
}