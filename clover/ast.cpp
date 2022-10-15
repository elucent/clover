#include "clover/ast.h"
#include "clover/env.h"
#include "clover/type.h"
#include "clover/clover.h"
#include "clover/err.h"
#include "lib/str.h"
#include "lib/utf.h"
#include "lib/math.h"

void parse_program(Lexer& lexer, Parser& parser, Module* mod, Token& next);
void parse_post_primary(Lexer& lexer, Parser& parser, Module* mod, Token& next);
void parse_primary(Lexer& lexer, Parser& parser, Module* mod, Token& next);
void parse_binary(Lexer& lexer, Parser& parser, Module* mod, Token& next);
void parse_statement(Lexer& lexer, Parser& parser, Module* mod, Token& next);
void parse_block(Lexer& lexer, Parser& parser, Module* mod, Token& next);

Parser::Parser(): astspace(1024), program(new(astspace) ASTProgram()) {
    nodes.alloc = &astspace;
    prods.alloc = &astspace;
    counts.alloc = &astspace;
    binops.alloc = &astspace;
    program->toplevel.alloc = &astspace;

    prods.push(parse_program);
}

// Array mapping from token kind to AST kind for unary ops.
static constexpr const ASTKind TOKEN_TO_UNOP[NUM_TOKEN_KINDS] = {
    AST_NONE, AST_NONE,
    AST_NONE,
    AST_NONE, AST_NONE, AST_NONE, AST_NONE, AST_NONE, AST_NONE,
    AST_NONE, AST_NONE, AST_NONE, AST_NONE, 
    AST_NONE, AST_NONE, AST_NONE, AST_NONE, AST_NONE, AST_NONE, AST_INCR, AST_DECR,
    AST_PLUS, AST_NEG, AST_DEREF, AST_NONE, AST_NONE, AST_NONE,
    AST_BITAND, AST_NONE, AST_NONE, AST_NONE, AST_NONE, AST_BITNOT,
    AST_NONE, AST_NONE, AST_NONE, AST_NONE, AST_NONE, AST_NONE, AST_NONE,
    AST_NONE, AST_NONE, AST_NONE, AST_NONE, AST_NONE,
    AST_NONE, AST_NONE, AST_NOT, 
    AST_NONE, AST_NONE, AST_NONE, AST_NONE, AST_NONE, AST_NONE, AST_NONE, AST_NONE, AST_NONE, AST_NONE, AST_NONE, AST_NONE,
    AST_NONE, AST_NONE, AST_NONE, AST_NONE, AST_NONE, AST_NONE, AST_NONE, AST_NONE, AST_NONE,
    AST_NONE, AST_NONE,
    AST_NONE, AST_NONE, AST_NONE, AST_NONE, AST_NONE
};

// Array mapping from token kind to AST kind for binary ops.
static constexpr const ASTKind TOKEN_TO_BINOP[NUM_TOKEN_KINDS] = {
    AST_NONE, AST_NONE,
    AST_NONE,
    AST_NONE, AST_NONE, AST_NONE, AST_NONE, AST_NONE, AST_NONE,
    AST_NONE, AST_NONE, AST_DOT, AST_NONE, 
    AST_LESS, AST_LEQUAL, AST_GREATER, AST_GEQUAL, AST_EQUAL, AST_INEQUAL, AST_NONE, AST_NONE,
    AST_ADD, AST_SUB, AST_STAR, AST_DIV, AST_MOD, AST_EXP,
    AST_BITAND, AST_BITOR, AST_BITXOR, AST_BITLEFT, AST_BITRIGHT, AST_NONE,
    AST_ASSIGN, AST_ADDEQ, AST_SUBEQ, AST_STAREQ, AST_DIVEQ, AST_MODEQ, AST_EXPEQ,
    AST_BITANDEQ, AST_BITOREQ, AST_BITXOREQ, AST_BITLEFTEQ, AST_BITRIGHTEQ,
    AST_AND, AST_OR, AST_NONE, 
    AST_NONE, AST_NONE, AST_NONE, AST_NONE, AST_NONE, AST_NONE, AST_NONE, AST_NONE, AST_NONE, AST_NONE, AST_NONE, AST_IS,
    AST_NONE, AST_NONE, AST_NONE, AST_NONE, AST_NONE, AST_NONE, AST_NONE, AST_NONE, AST_NONE,
    AST_NONE, AST_NONE,
    AST_NONE, AST_NONE, AST_NONE, AST_NONE, AST_NONE
};

inline SourcePos ident_pos(Module* mod, SourcePos pos) {
    auto& str = mod->interner->str(pos.end);
    pos.end = pos.start + str.n;
    return pos;
}

void discard_rest_of_line(Lexer& lexer, Parser& parser, Module* mod, Token& next) {
    parser.consume(lexer);
    if (next.kind == TK_NEWLINE || next.kind == TK_EOF) parser.leave();
}

void parse_trailing_newlines(Lexer& lexer, Parser& parser, Module* mod, Token& next) {
    if (next.kind == TK_NEWLINE) parser.consume(lexer);
    else parser.leave();
}

void parse_fundecl_with_body(Lexer& lexer, Parser& parser, Module* mod, Token& next) {
    AST* body = parser.pop();
    parser.leave(new(parser.astspace) FunDecl(parser.endpos(body->pos), parser.lhs(), parser.rhs(), body))
          .poplr();
}

void parse_fundecl_body(Lexer& lexer, Parser& parser, Module* mod, Token& next) {
    if (next.kind == TK_COLON)
        parser.consume(lexer)
              .tailrec(parse_fundecl_with_body).rec(parse_block);
    else {
        parser.leave(new(parser.astspace) FunDecl(parser.endpos(parser.rhs()->pos), parser.lhs(), parser.rhs(), nullptr))
              .poplr();
    }
}

void parse_indented_block(Lexer& lexer, Parser& parser, Module* mod, Token& next) {
    if (next.kind == TK_NEWLINE) parser.consume(lexer);
    else if (next.column <= parser.indents.back() || next.kind == TK_EOF) { // Dedent found.
        parser.indent = parser.indents.pop();
        parser.leave(new(parser.astspace) List(AST_DO, parser.endpos(parser.back()->pos), parser.take(parser.endcount())));
    }
    else parser.inc().rec(parse_statement);
}

void parse_single_statement_block(Lexer& lexer, Parser& parser, Module* mod, Token& next) {
    parser.leave(new(parser.astspace) List(AST_DO, parser.back()->pos, parser.take(1)));
}

void parse_block(Lexer& lexer, Parser& parser, Module* mod, Token& next) {
    if (next.kind == TK_NEWLINE) { // Indented block.
        parser.indents.push(parser.indent);
        parser.consume_and_indent(lexer)
              .begincount(0)
              .startpos(next)
              .tailrec(parse_indented_block);
    }
    else parser.tailrec(parse_single_statement_block).rec(parse_statement); // Single expression block.
}

void parse_next_vardecl_name(Lexer& lexer, Parser& parser, Module* mod, Token& next);
void parse_vardecl_typed_init(Lexer& lexer, Parser& parser, Module* mod, Token& next);

void parse_next_typed_vardecl(Lexer& lexer, Parser& parser, Module* mod, Token& next) {
    switch (next.kind) {
        case TK_COMMA:
        case TK_NEWLINE: // Uninitialized variable declaration.
            parser.tailrec(parse_vardecl_typed_init, nullptr)
                  .inc();
            return;
        case TK_COLON: // Initialized variable declaration.
            parser.consume(lexer)
                  .tailrec(parse_vardecl_typed_init).rec(parse_binary)
                  .inc();
            return;
        default:
            no_vardecl_sep_error(mod, next);
            parser.leave().endcount();
            return;
    }
}

void parse_vardecl_untyped_init(Lexer& lexer, Parser& parser, Module* mod, Token& next) {
    AST* init = parser.pop();
    if (next.kind == TK_NEWLINE) {
        parser.push(new(parser.astspace) VarDecl(parser.endpos(init->pos), parser.lhs(), parser.rhs(), init))
              .poplr();
        i32 count = parser.endcount();
        if (count == 1) // Single decl.
            parser.consume_and_indent(lexer)
                  .leave();
        else {
            slice<AST*> decls = parser.take(count);
            SourcePos pos;
            if (decls.n > 1) pos = decls[0]->pos + decls[decls.n - 1]->pos;
            parser.consume_and_indent(lexer)
                  .leave(new(parser.astspace) List(AST_DO, pos, decls));
        }
    }
    else if (next.kind == TK_COMMA) {
        AST* type = parser.lhs();
        AST* name = parser.rhs();
        parser.consume(lexer)
              .push(new(parser.astspace) VarDecl(parser.locs.back() + init->pos, type, name, init))
              .poplr()
              .tailrec(parse_next_vardecl_name, type).rec(parse_primary);
    }
    else no_vardecl_sep_error(mod, next), parser.leave().endcount();
}

void parse_vardecl_typed_init(Lexer& lexer, Parser& parser, Module* mod, Token& next) {
    AST* init = parser.pop();
    if (next.kind == TK_NEWLINE) {
        AST* type = parser.lhs();
        AST* name = parser.rhs();
        parser.push(new(parser.astspace) VarDecl(init ? type->pos + init->pos : type->pos + name->pos, type, name, init))
              .poplr();
        i32 count = parser.endcount();
        if (count == 1) // Single decl.
            parser.consume_and_indent(lexer)
                  .leave();
        else {
            slice<AST*> decls = parser.take(count);
            SourcePos pos;
            if (decls.n > 1) pos = decls[0]->pos + decls[decls.n - 1]->pos;
            parser.consume_and_indent(lexer)
                  .leave(new(parser.astspace) List(AST_DO, pos, decls));
        }
    }
    else if (next.kind == TK_COMMA) {
        AST* type = parser.lhs();
        AST* name = parser.rhs();
        parser.consume(lexer)
              .push(new(parser.astspace) VarDecl(init ? type->pos + init->pos : type->pos + name->pos, type, name, init))
              .poplr()
              .tailrec(parse_next_typed_vardecl, type).rec(parse_primary);
    }
    else no_vardecl_sep_error(mod, next), parser.leave().endcount();
}

void parse_next_vardecl_name(Lexer& lexer, Parser& parser, Module* mod, Token& next) {
    if (next.kind == TK_COLON)
        parser.consume(lexer)
              .tailrec(parse_vardecl_untyped_init).rec(parse_binary)
              .inc();
    else no_vardecl_colon_error(mod, next), parser.leave().endcount();
}

void parse_vardecl_name(Lexer& lexer, Parser& parser, Module* mod, Token& next) {
    if (next.kind == TK_COLON)
        parser.consume(lexer)
              .tailrec(parse_vardecl_untyped_init).rec(parse_binary);
    else no_vardecl_colon_error(mod, next), parser.leave().endcount();
}

void parse_var_or_fundecl(Lexer& lexer, Parser& parser, Module* mod, Token& next) {
    if (parser.nodes.back()->kind == AST_APPLY) // Function declaration
        parser.startpos(parser.back()->pos).tailrec(parse_fundecl_body);
    else switch (next.kind) {
        case TK_COMMA:
        case TK_NEWLINE: // Uninitialized variable declaration.
            parser.tailrec(parse_vardecl_typed_init, nullptr)
                  .begincount();
            return;
        case TK_COLON: // Initialized variable declaration.
            parser.consume(lexer)
                  .tailrec(parse_vardecl_typed_init).rec(parse_binary)
                  .begincount();
            return;
        default:
            no_vardecl_sep_error(mod, next), parser.leave();
    }
}

// Called after a binary expression has been fully parsed to maybe declare a var or function.
void parse_maybe_vardecl(Lexer& lexer, Parser& parser, Module* mod, Token& next) {
    if (next.kind == TK_NEWLINE || next.kind == TK_EOF) {
        parser.consume_and_indent(lexer).leave();
        return; 
    }
    else if (next.kind == TK_COLON && parser.nodes.back()->kind == AST_STAR) {
        Binary* b = (Binary*)parser.pop();
        parser.push(new(parser.astspace) Unary(AST_DEREF, b->left->pos, b->left))
              .push(b->right)
              .tailrec(parse_var_or_fundecl);
    }
    else if (next.kind == TK_COMMA && parser.nodes.back()->kind == AST_STAR) {
        Binary* b = (Binary*)parser.pop();
        parser.push(new(parser.astspace) Unary(AST_DEREF, b->left->pos, b->left))
              .push(b->right)
              .tailrec(parse_vardecl_typed_init, nullptr)
              .begincount();
    }
    else parser.tailrec(parse_var_or_fundecl).rec(parse_binary);
}

void parse_constdecl_name(Lexer& lexer, Parser& parser, Module* mod, Token& next);

void parse_constdecl_untyped_init(Lexer& lexer, Parser& parser, Module* mod, Token& next) {
    if (next.kind == TK_NEWLINE) {
        parser.push(new(parser.astspace) ConstDecl(parser.endpos(parser.rhs()->pos), parser.lhs(), parser.rhs()))
              .poplr();
        i32 count = parser.endcount();
        if (count == 1) // Single decl.
            parser.consume_and_indent(lexer)
                  .leave();
        else {
            slice<AST*> decls = parser.take(count);
            SourcePos pos;
            if (decls.n > 1) pos = decls[0]->pos + decls[decls.n - 1]->pos;
            parser.consume_and_indent(lexer)
                  .leave(new(parser.astspace) List(AST_DO, pos, decls));
        }
    }
    else if (next.kind == TK_COMMA) {
        parser.consume(lexer)
              .push(new(parser.astspace) ConstDecl(parser.locs.back() + parser.rhs()->pos, parser.lhs(), parser.rhs()))
              .poplr()
              .tailrec(parse_constdecl_name).rec(parse_primary);
    }
    else no_constdecl_sep_error(mod, next), parser.leave().endcount();
}

void parse_next_constdecl_name(Lexer& lexer, Parser& parser, Module* mod, Token& next) {
    if (next.kind == TK_COLON)
        parser.consume(lexer)
              .tailrec(parse_constdecl_untyped_init).rec(parse_binary)
              .inc();
    else no_constdecl_colon_error(mod, next), parser.leave().endcount();
}

void parse_constdecl_name(Lexer& lexer, Parser& parser, Module* mod, Token& next) {
    if (next.kind == TK_COLON)
        parser.consume(lexer)
              .tailrec(parse_constdecl_untyped_init).rec(parse_binary);
    else no_constdecl_colon_error(mod, next), parser.leave().endcount();
}

void parse_unary_plus(Lexer& lexer, Parser& parser, Module* mod, Token& next) {
    parser.leave(new(parser.astspace) Unary(AST_PLUS, parser.endpos(next), parser.pop()));
}

void parse_unary_minus(Lexer& lexer, Parser& parser, Module* mod, Token& next) {
    parser.leave(new(parser.astspace) Unary(AST_NEG, parser.endpos(next), parser.pop()));
}

void parse_unary_star(Lexer& lexer, Parser& parser, Module* mod, Token& next) {
    parser.leave(new(parser.astspace) Unary(AST_DEREF, parser.endpos(next), parser.pop()));
}

void parse_unary_bitand(Lexer& lexer, Parser& parser, Module* mod, Token& next) {
    parser.leave(new(parser.astspace) Unary(AST_ADDROF, parser.endpos(next), parser.pop()));
}

void parse_unary_not(Lexer& lexer, Parser& parser, Module* mod, Token& next) {
    parser.leave(new(parser.astspace) Unary(AST_NOT, parser.endpos(next), parser.pop()));
}

void parse_unary_bitnot(Lexer& lexer, Parser& parser, Module* mod, Token& next) {
    parser.leave(new(parser.astspace) Unary(AST_BITNOT, parser.endpos(next), parser.pop()));
}

void parse_unary_preincr(Lexer& lexer, Parser& parser, Module* mod, Token& next) {
    parser.leave(new(parser.astspace) Unary(AST_INCR, parser.endpos(next), parser.pop()));
}

void parse_unary_predecr(Lexer& lexer, Parser& parser, Module* mod, Token& next) {
    parser.leave(new(parser.astspace) Unary(AST_DECR, parser.endpos(next), parser.pop()));
}

void parse_unary_new(Lexer& lexer, Parser& parser, Module* mod, Token& next) {
    AST* child = parser.pop();
    if (child->kind == AST_INDEX)
        parser.leave(new(parser.astspace) Binary(AST_NEWARRAY, parser.endpos(next), ((Binary*)child)->left, ((Binary*)child)->right));
    else 
        parser.leave(new(parser.astspace) Unary(AST_NEW, parser.endpos(next), child));
}

void parse_unary_del(Lexer& lexer, Parser& parser, Module* mod, Token& next) {
    parser.leave(new(parser.astspace) Unary(AST_DEL, parser.endpos(next), parser.pop()));
}

void parse_maybe_arg_name(Lexer& lexer, Parser& parser, Module* mod, Token& next) {
    if (next.kind == TK_IDENT) {
        SourcePos ipos = ident_pos(mod, next);
        AST* type = parser.pop();
        if (type) {
            parser.leave(new(parser.astspace) VarDecl(type->pos + ipos, type, new(parser.astspace) Var(ipos, next.end), nullptr))
                  .consume(lexer);
        }
        else parser.leave(new(parser.astspace) VarDecl(parser.endpos(ipos), nullptr, new(parser.astspace) Var(ipos, next.end), nullptr)).consume(lexer);
    }
    else parser.leave();
}

void parse_maybe_untyped_arg(Lexer& lexer, Parser& parser, Module* mod, Token& next) {
    if (next.kind == TK_VAR) {
        parser.consume(lexer).tailrec(parse_maybe_arg_name, nullptr).startpos(next);
    }
    else parser.tailrec(parse_maybe_arg_name).rec(parse_binary);
}

void parse_maybe_untyped_arg_is(Lexer& lexer, Parser& parser, Module* mod, Token& next) {
    if (next.kind == TK_VAR) {
        parser.consume(lexer).tailrec(parse_maybe_arg_name, nullptr).startpos(next);
    }
    else parser.tailrec(parse_maybe_arg_name).rec(parse_primary); // Only primary expr permitted in is expression.
}

void parse_call_separator(Lexer& lexer, Parser& parser, Module* mod, Token& next) {
    if (next.kind == TK_RPAREN) { // End of call, so we create the node and proceed.
        slice<AST*> args = parser.take(parser.endcount());
        parser.consume(lexer)
              .tailrec(parse_post_primary, new(parser.astspace) Apply(parser.back()->pos + next, parser.pop(), args))
              .unnest();
    } 
    else if (next.kind == TK_COMMA) // We expect more arguments, so we count up and parse another expr.
        parser.consume(lexer)
              .inc()
              .rec(parse_maybe_untyped_arg);
    else no_call_sep_error(mod, next), parser.leave().unnest().endcount();
}

void parse_maybe_void_call(Lexer& lexer, Parser& parser, Module* mod, Token& next) {
    // Consider the case where we have a function call with no arguments, i.e. foo()
    AST* fn = parser.pop();
    if (next.kind == TK_RPAREN) {
        parser.consume(lexer)
              .tailrec(parse_post_primary, new(parser.astspace) Apply(fn->pos + next, fn, slice<AST*>{(AST**)nullptr, (iptr)0}))
              .unnest();
    }
    else {
        parser.push(fn)
              .begincount()
              .tailrec(parse_call_separator)
              .rec(parse_maybe_untyped_arg);
    }
}

void parse_slice_right_bracket(Lexer& lexer, Parser& parser, Module* mod, Token& next) {
    if (next.kind == TK_RSQUARE) {
        AST* high = parser.pop();
        parser.consume(lexer)
              .tailrec(parse_post_primary, new(parser.astspace) Slice(parser.lhs()->pos + next, parser.lhs(), parser.rhs(), high)).poplr()
              .unnest();
    }
    else no_slice_rbracket_error(mod, next), parser.unnest().leave();
}

void parse_maybe_slice_right_bracket(Lexer& lexer, Parser& parser, Module* mod, Token& next) {
    if (next.kind == TK_RSQUARE) 
        parser.push(nullptr).tailrec(parse_slice_right_bracket);
    else 
        parser.tailrec(parse_slice_right_bracket).rec(parse_binary);
}

void parse_maybe_slice(Lexer& lexer, Parser& parser, Module* mod, Token& next) {
    if (next.kind == TK_COLON) 
        parser.consume(lexer)
              .tailrec(parse_maybe_slice_right_bracket);
    else if (next.kind == TK_RSQUARE) 
        parser.consume(lexer)
              .tailrec(parse_post_primary, new(parser.astspace) Binary(AST_INDEX, parser.lhs()->pos + next, parser.lhs(), parser.rhs())).poplr()
              .unnest();
    else no_slice_sep_error(mod, next), parser.unnest().leave();
}

void parse_maybe_empty_index(Lexer& lexer, Parser& parser, Module* mod, Token& next) {
    // Consider the case where we have an array dimension with no arguments, i.e. int[]
    if (next.kind == TK_RSQUARE) parser.consume(lexer).tailrec(parse_post_primary, new(parser.astspace) Binary(AST_INDEX, parser.back()->pos + next, parser.pop(), nullptr)).unnest();
    else if (next.kind == TK_COLON) parser.push(nullptr).consume(lexer).tailrec(parse_maybe_slice_right_bracket);
    else parser.tailrec(parse_maybe_slice).rec(parse_binary);
}

void parse_post_primary(Lexer& lexer, Parser& parser, Module* mod, Token& next) {
    if (parser.binops.size() && parser.binops.back() == TK_DOT)                       // Eagerly crunch down dot operators as we see them.
        parser.push(new(parser.astspace) Binary(AST_DOT, parser.lhs()->pos + parser.rhs()->pos, parser.lhs(), parser.rhs())) // This is necessary to ensure left-associativity.
              .poplr()
              .popop();
    AST* back = parser.back();
    switch (next.kind) {
        case TK_LPAREN: // Function call/type application.
            parser.consume(lexer)
                  .tailrec(parse_maybe_void_call)
                  .nest();
            return;
        case TK_LSQUARE: // Index.
            parser.consume(lexer)
                  .tailrec(parse_maybe_empty_index)
                  .nest();
            return;
        case TK_DOT: // Field access.
            parser.consume(lexer)
                  .pushop(TK_DOT)
                  .rec(parse_primary);
            return;
        case TK_INCR:
            parser.consume(lexer)
                  .tailrec(parse_post_primary, new(parser.astspace) Unary(AST_POSTINCR, back->pos + next, parser.pop()));
            return;
        case TK_DECR:
            parser.consume(lexer)
                  .tailrec(parse_post_primary, new(parser.astspace) Unary(AST_POSTDECR, back->pos + next, parser.pop()));
            return;
        case TK_QUESTION: {
            if (parser.back()->kind != AST_VAR) non_ident_in_tvar_error(mod, parser.back());
            AST* var = parser.pop();
            parser.consume(lexer)
                  .tailrec(parse_post_primary, new(parser.astspace) Unary(AST_TYPEVAR, var->pos + next, var));
            return;
        }
        default: parser.leave();
    }
}

void parse_right_paren(Lexer& lexer, Parser& parser, Module* mod, Token& next) {
    if (next.kind == TK_RPAREN) 
        parser.consume(lexer)
              .tailrec(parse_post_primary, new(parser.astspace) Unary(AST_PAREN, parser.endpos(next), parser.pop()))
              .unnest();
    else no_closing_paren_error(mod, next), parser.unnest().leave().endpos({});
}

void parse_if_final(Lexer& lexer, Parser& parser, Module* mod, Token& next) {
    AST* elseBody = parser.pop();
    parser.leave(new(parser.astspace) If(parser.endpos((elseBody ? elseBody : parser.rhs())->pos), parser.lhs(), parser.rhs(), elseBody))
          .poplr();
}

void parse_if_else_body(Lexer& lexer, Parser& parser, Module* mod, Token& next) {
    if (next.kind == TK_COLON) 
        parser.consume(lexer)
              .tailrec(parse_if_final)
              .rec(parse_block);
    else if (next.kind == TK_IF 
        || next.kind == TK_WHILE 
        || next.kind == TK_UNTIL 
        || next.kind == TK_FOR 
        || next.kind == TK_MATCH
        || next.kind == TK_DEFER
        || next.kind == TK_RETURN
        || next.kind == TK_DO
        || next.kind == TK_WITH
        || next.kind == TK_USE
        || next.kind == TK_BREAK
        || next.kind == TK_CONTINUE)
        parser.tailrec(parse_if_final)
              .rec(parse_single_statement_block)
              .rec(parse_statement);
    else no_block_error(mod, next), parser.leave().endpos({});
}

void parse_if_then_body(Lexer& lexer, Parser& parser, Module* mod, Token& next) {
    if (next.kind == TK_ELSE)
        parser.consume(lexer)
              .tailrec(parse_if_final)
              .rec(parse_binary);
    else no_else_in_if_error(mod, next), parser.leave().endpos({});
}

void parse_if_body(Lexer& lexer, Parser& parser, Module* mod, Token& next) {
    if (next.kind == TK_ELSE)
        parser.consume(lexer)
              .tailrec(parse_if_else_body);
    else parser.tailrec(parse_if_final, nullptr);
}

void parse_if_then_condition(Lexer& lexer, Parser& parser, Module* mod, Token& next) {
    if (next.kind == TK_THEN) 
        parser.consume(lexer)
              .tailrec(parse_if_then_body)
              .rec(parse_binary);
    else no_else_in_if_error(mod, next), parser.leave().endpos({});
}

void parse_if_condition(Lexer& lexer, Parser& parser, Module* mod, Token& next) {
    if (next.kind == TK_COLON) 
        parser.consume(lexer)
              .tailrec(parse_if_body)
              .rec(parse_block);
    else no_block_error(mod, next), parser.leave().endpos({});
}

void parse_set_separator(Lexer& lexer, Parser& parser, Module* mod, Token& next) {
    if (next.kind == TK_RBRACE)
        parser.consume(lexer)
              .leave(new(parser.astspace) List(AST_SET, parser.endpos(next), parser.take(parser.endcount())))
              .unnest();
    else if (next.kind == TK_COMMA)
        parser.consume(lexer)
              .inc()
              .rec(parse_binary);
    else no_set_sep_error(mod, next), parser.leave().unnest().endcount(), parser.endpos({});
}

void parse_maybe_empty_set(Lexer& lexer, Parser& parser, Module* mod, Token& next) {
    if (next.kind == TK_RBRACE)
        parser.consume(lexer)
              .leave(new(parser.astspace) List(AST_SET, parser.endpos(next), {(AST**)nullptr, (iptr)0}))
              .unnest();
    else parser.tailrec(parse_set_separator)
               .begincount()
               .rec(parse_binary);
}

void parse_array_separator(Lexer& lexer, Parser& parser, Module* mod, Token& next) {
    if (next.kind == TK_RSQUARE)
        parser.consume(lexer)
              .leave(new(parser.astspace) List(AST_ARRAY, parser.endpos(next), parser.take(parser.endcount())))
              .unnest();
    else if (next.kind == TK_COMMA)
        parser.consume(lexer)
              .inc()
              .rec(parse_binary);
    else no_array_sep_error(mod, next), parser.leave().unnest().endcount(), parser.endpos({});
}

void parse_maybe_empty_array(Lexer& lexer, Parser& parser, Module* mod, Token& next) {
    if (next.kind == TK_RSQUARE)
        parser.consume(lexer)
              .leave(new(parser.astspace) List(AST_ARRAY, parser.endpos(next), {(AST**)nullptr, (iptr)0}))
              .unnest();
    else parser.tailrec(parse_array_separator)
               .begincount()
               .rec(parse_binary);
}

void parse_maybe_unit(Lexer& lexer, Parser& parser, Module* mod, Token& next) {
    if (next.kind == TK_RPAREN)
        parser.consume(lexer)
              .leave(new(parser.astspace) Const(parser.endpos(next)))
              .unnest();
    else parser.tailrec(parse_right_paren)
               .rec(parse_binary);
}

void parse_sizeof_end(Lexer& lexer, Parser& parser, Module* mod, Token& next) {
    if (next.kind == TK_BITOR) {
        parser.consume(lexer)
              .leave(new(parser.astspace) Sizeof(parser.endpos(next), parser.pop()))
              .unnest();
    }
    else no_closing_sizeof_error(mod, next), parser.leave().unnest().endpos({});
}

void parse_sizeof_expr(Lexer& lexer, Parser& parser, Module* mod, Token& next) {
    if (next.kind == TK_BITOR) {
        empty_sizeof_expr_error(mod, next);
        parser.consume(lexer)
              .leave().unnest().endpos({});
    }
    else parser.tailrec(parse_sizeof_end)
               .rec(parse_primary);
}

void parse_primary(Lexer& lexer, Parser& parser, Module* mod, Token& next) {
    switch (next.kind) {
        case TK_ICONST: // Integer constant.
            parser.consume(lexer)
                  .tailrec(parse_post_primary, new(parser.astspace) Const(next, toint(next.str(mod)))); 
            return;
        case TK_FCONST: // Float constant.
            parser.consume(lexer)
                  .tailrec(parse_post_primary, new(parser.astspace) Const(next, tofloat(next.str(mod)))); 
            return;
        case TK_CHCONST: { // Char constant.
            const_slice<i8> str = next.str(mod);
            rune r;
            utf8_decode(str.ptr + 1, str.n - 2, &r, 1); // Strip off the single quotes.
            parser.consume(lexer)
                  .tailrec(parse_post_primary, new(parser.astspace) Const(next, r)); 
            return;
        }
        case TK_STRCONST: // String constant.
            parser.consume(lexer)
                  .tailrec(parse_post_primary, new(parser.astspace) Const(next, next.start + 1, next.end - next.start - 2)); 
            return;
        case TK_TRUE: // Boolean true.
            parser.consume(lexer)
                  .tailrec(parse_post_primary, new(parser.astspace) Const(next, true));
            return;
        case TK_FALSE: // Boolean false.
            parser.consume(lexer)
                  .tailrec(parse_post_primary, new(parser.astspace) Const(next, false));
            return;
        case TK_IDENT: // Variable
            parser.consume(lexer)
                  .tailrec(parse_post_primary, new(parser.astspace) Var(ident_pos(mod, next), next.end));
            return;
        case TK_PLUS: 
            parser.consume(lexer)
                  .startpos(next)
                  .tailrec(parse_unary_plus)
                  .rec(parse_primary); 
            return;
        case TK_MINUS: 
            parser.consume(lexer)
                  .startpos(next)
                  .tailrec(parse_unary_minus)
                  .rec(parse_primary); 
            return;
        case TK_TIMES: 
            parser.consume(lexer)
                  .startpos(next)
                  .tailrec(parse_unary_star)
                  .rec(parse_primary); 
            return;
        case TK_BITAND: 
            parser.consume(lexer)
                  .startpos(next)
                  .tailrec(parse_unary_bitand)
                  .rec(parse_primary); 
            return;
        case TK_NOT: 
            parser.consume(lexer)
                  .startpos(next)
                  .tailrec(parse_unary_not)
                  .rec(parse_primary); 
            return;
        case TK_BITNOT: 
            parser.consume(lexer)
                  .startpos(next)
                  .tailrec(parse_unary_bitnot)
                  .rec(parse_primary); 
            return;
        case TK_INCR: 
            parser.consume(lexer)
                  .startpos(next)
                  .tailrec(parse_unary_preincr)
                  .rec(parse_primary); 
            return;
        case TK_DECR: 
            parser.consume(lexer)
                  .startpos(next)
                  .tailrec(parse_unary_predecr)
                  .rec(parse_primary); 
            return;
        case TK_NEW: 
            parser.consume(lexer)
                  .startpos(next)
                  .tailrec(parse_unary_new)
                  .rec(parse_binary); 
            return;
        case TK_DEL: 
            parser.consume(lexer)
                  .startpos(next)
                  .tailrec(parse_unary_del)
                  .rec(parse_binary); 
            return;
        case TK_LPAREN:
            parser.consume(lexer)
                  .startpos(next)
                  .tailrec(parse_maybe_unit)
                  .nest();
            return;
        case TK_LSQUARE:
            parser.consume(lexer)
                  .startpos(next)
                  .tailrec(parse_maybe_empty_array)
                  .nest();
            return;
        case TK_LBRACE:
            parser.consume(lexer)
                  .startpos(next)
                  .tailrec(parse_maybe_empty_set)
                  .nest();
            return;
        case TK_BITOR:
            parser.consume(lexer)
                  .startpos(next)
                  .tailrec(parse_sizeof_expr)
                  .nest();
            return;
        default:
            expected_primary_error(mod, next), parser.consume(lexer).leave(new(parser.astspace) IllFormed());
    }
}

AST* construct_binary_node(Parser& parser, ASTKind op, SourcePos pos, AST* left, AST* right) {
    if (op == AST_IS) return new(parser.astspace) Is(pos, left, right);
    else return new(parser.astspace) Binary(op, pos, left, right);
}

void parse_binary_return_to_op(Lexer& lexer, Parser& parser, Module* mod, Token& next) {
    // Pops the last operator off the operator stack and creates an AST node.
    parser.leave(construct_binary_node(parser, TOKEN_TO_BINOP[parser.popop()], parser.lhs()->pos + parser.rhs()->pos, parser.lhs(), parser.rhs())).poplr();
}

void parse_binary_op(Lexer& lexer, Parser& parser, Module* mod, Token& next);

void parse_binary_next_op(Lexer& lexer, Parser& parser, Module* mod, Token& next) {
    if (!parser.binops.size()) { // Sometimes we can reach this method without any binops currently present. In this case, we
                                 // look for the normal next binary op.
        parser.tailrec(parse_binary_op);
    }
    else if (TOKEN_TO_BINOP[next.kind]) { // If our expr is followed by yet another op, we need to compare precedence
        TokenKind last = parser.binops.back();
        if (PRECEDENCE[next.kind] > PRECEDENCE[last])
            parser.consume(lexer).pushop(next.kind)
                  .tailrec(parse_binary_return_to_op) // Continue to the right, eventually returning here once the rhs has been parsed.
                  .rec(parse_binary_next_op)
                  .rec(parse_primary)
                  .rec(parse_trailing_newlines);
        else parser.push(construct_binary_node(parser, TOKEN_TO_BINOP[parser.popop()], parser.lhs()->pos + parser.rhs()->pos, parser.lhs(), parser.rhs()))
                   .poplr()
                   .consume(lexer).pushop(next.kind)
                   .tailrec(parse_binary_next_op) // Continue to the right after parsing lhs.
                   .rec(parse_primary)
                   .rec(parse_trailing_newlines);
    }
    else parser.leave(construct_binary_node(parser, TOKEN_TO_BINOP[parser.popop()], parser.lhs()->pos + parser.rhs()->pos, parser.lhs(), parser.rhs()))
               .poplr(); // Otherwise, this expression is done and we can leave it there.
}

void parse_maybe_pointer(Lexer& lexer, Parser& parser, Module* mod, Token& next) {
    if (TOKEN_TO_BINOP[next.kind] && !TOKEN_TO_UNOP[next.kind]) { // If a star is followed by another binary operator...
        parser.push(new(parser.astspace) Unary(AST_DEREF, parser.back()->pos + next, parser.pop())) // We assume it's a pointer.
              .tailrec(parse_binary_op)
              .popop();
    }
    else {
        parser.tailrec(parse_binary_next_op).rec(parse_primary);
    }
}

void parse_binary_op(Lexer& lexer, Parser& parser, Module* mod, Token& next) {
    if (TOKEN_TO_BINOP[next.kind]) { // If it's a binary op, we need to parse another expression and continue to the next potential op.
        if (next.kind == TK_TIMES) 
            parser.consume(lexer).pushop(next.kind).tailrec(parse_maybe_pointer);
        else if (next.kind == TK_IS)
            parser.consume(lexer).pushop(next.kind).tailrec(parse_binary_next_op).rec(parse_maybe_untyped_arg_is).rec(parse_trailing_newlines);
        else
            parser.consume(lexer).pushop(next.kind).tailrec(parse_binary_next_op).rec(parse_primary).rec(parse_trailing_newlines);
    }
    else parser.leave(); // Otherwise, it's just a primary expression and we can leave it at that.
}

void parse_binary(Lexer& lexer, Parser& parser, Module* mod, Token& next) {
    parser.tailrec(parse_binary_op).rec(parse_primary);
}

void parse_while_end(Lexer& lexer, Parser& parser, Module* mod, Token& next) {
    parser.leave(new(parser.astspace) Loop(AST_WHILE, parser.endpos(parser.rhs()->pos), parser.lhs(), parser.rhs()))
          .poplr();
}

void parse_while_condition(Lexer& lexer, Parser& parser, Module* mod, Token& next) {
    if (next.kind == TK_COLON)
        parser.consume(lexer)
              .tailrec(parse_while_end)
              .rec(parse_block);
    else no_block_error(mod, next), parser.tailrec(discard_rest_of_line).endpos({}), parser.pop();
}

void parse_until_end(Lexer& lexer, Parser& parser, Module* mod, Token& next) {
    parser.leave(new(parser.astspace) Loop(AST_UNTIL, parser.endpos(parser.rhs()->pos), parser.lhs(), parser.rhs()))
          .poplr();
}

void parse_until_condition(Lexer& lexer, Parser& parser, Module* mod, Token& next) {
    if (next.kind == TK_COLON)
        parser.consume(lexer)
              .tailrec(parse_until_end)
              .rec(parse_block);
    else no_block_error(mod, next), parser.tailrec(discard_rest_of_line).endpos({}), parser.pop();
}

void parse_for_end(Lexer& lexer, Parser& parser, Module* mod, Token& next) {
    AST* body = parser.nodes.pop();
    parser.leave(new(parser.astspace) For(parser.endpos(body->pos), parser.lhs(), parser.rhs(), body))
          .poplr();
}

void parse_for_body(Lexer& lexer, Parser& parser, Module* mod, Token& next) {
    if (next.kind == TK_COLON)
        parser.consume(lexer)
              .tailrec(parse_for_end)
              .rec(parse_block);
    else no_block_error(mod, next), parser.tailrec(discard_rest_of_line).endpos({}), parser.poplr();
}

void parse_for_binding(Lexer& lexer, Parser& parser, Module* mod, Token& next) {
    if (next.kind == TK_IN) 
        parser.consume(lexer)
              .tailrec(parse_for_body)
              .rec(parse_binary);
    else no_for_loop_in_error(mod, next), parser.tailrec(discard_rest_of_line).endpos({}), parser.pop();
}

void parse_defer_end(Lexer& lexer, Parser& parser, Module* mod, Token& next) {
    parser.leave(new(parser.astspace) Unary(AST_DEFER, parser.endpos(parser.back()->pos), parser.pop()));
}

void parse_defer(Lexer& lexer, Parser& parser, Module* mod, Token& next) {
    if (next.kind == TK_COLON)
        parser.consume(lexer)
              .tailrec(parse_defer_end)
              .rec(parse_block);
    else parser.tailrec(parse_defer_end)
               .rec(parse_single_statement_block)
               .rec(parse_statement);
}

void parse_return_end(Lexer& lexer, Parser& parser, Module* mod, Token& next) {
    if (next.kind == TK_NEWLINE)
        parser.leave(new(parser.astspace) Unary(AST_RETURN, parser.endpos(parser.back()->pos), parser.pop()))
              .consume_and_indent(lexer);
    else no_newline_error(mod, next), parser.tailrec(discard_rest_of_line).endpos({}), parser.pop();
}

void parse_return(Lexer& lexer, Parser& parser, Module* mod, Token& next) {
    if (next.kind == TK_NEWLINE)
        parser.consume(lexer)
              .leave(new(parser.astspace) Unary(AST_RETURN, parser.locs.pop(), nullptr));
    else parser.tailrec(parse_return_end)
               .rec(parse_binary);
}

void parse_break_newline(Lexer& lexer, Parser& parser, Module* mod, Token& next) {
    if (next.kind == TK_NEWLINE) parser.consume_and_indent(lexer).leave();
    else no_newline_error(mod, next), parser.tailrec(discard_rest_of_line);
}

void parse_continue_newline(Lexer& lexer, Parser& parser, Module* mod, Token& next) {
    if (next.kind == TK_NEWLINE) parser.consume_and_indent(lexer).leave();
    else no_newline_error(mod, next), parser.tailrec(discard_rest_of_line);
}

void parse_with_end(Lexer& lexer, Parser& parser, Module* mod, Token& next) {
    parser.leave(new(parser.astspace) With(parser.endpos(parser.rhs()->pos), parser.lhs(), parser.rhs()))
          .poplr();
}

void parse_with_body(Lexer& lexer, Parser& parser, Module* mod, Token& next) {
    if (next.kind == TK_COLON)
        parser.consume(lexer)
              .tailrec(parse_with_end)
              .rec(parse_block);
    else no_block_error(mod, next), parser.tailrec(discard_rest_of_line).endpos({}), parser.pop();
}

void parse_use_end(Lexer& lexer, Parser& parser, Module* mod, Token& next) {
    if (next.kind == TK_NEWLINE)
        parser.consume_and_indent(lexer)
              .leave(new(parser.astspace) Binary(AST_USE, parser.endpos(parser.rhs()->pos), parser.lhs(), parser.rhs()))
              .poplr();
    else no_newline_error(mod, next), parser.tailrec(discard_rest_of_line).endpos({}), parser.poplr();
}

void parse_use_alias(Lexer& lexer, Parser& parser, Module* mod, Token& next) {
    if (next.kind == TK_IDENT)
        parser.consume(lexer)
              .tailrec(parse_use_end, new(parser.astspace) Var(ident_pos(mod, next), next.end));
    else no_use_alias_error(mod, next), parser.tailrec(discard_rest_of_line).endpos({}), parser.pop();
}

void parse_use_suffix(Lexer& lexer, Parser& parser, Module* mod, Token& next) {
    if (next.kind == TK_AS)
        parser.consume(lexer)
              .tailrec(parse_use_alias);
    else if (next.kind == TK_NEWLINE)
        parser.consume_and_indent(lexer)
              .leave(new(parser.astspace) Binary(AST_USE, parser.endpos(parser.back()->pos), parser.pop(), nullptr)); // Null alias name.
    else no_newline_or_as_error(mod, next), parser.tailrec(discard_rest_of_line).endpos({}), parser.pop();
}

void parse_alias_end(Lexer& lexer, Parser& parser, Module* mod, Token& next) {
    if (next.kind == TK_NEWLINE)
        parser.consume_and_indent(lexer)
              .leave(new(parser.astspace) AliasDecl(parser.endpos(parser.rhs()->pos), parser.lhs(), parser.rhs()))
              .poplr();
    else no_newline_error(mod, next), parser.tailrec(discard_rest_of_line).endpos({}), parser.poplr();
}

void parse_alias_body(Lexer& lexer, Parser& parser, Module* mod, Token& next) {
    if (next.kind == TK_COLON)
        parser.consume(lexer)
              .tailrec(parse_alias_end)
              .rec(parse_binary);
    else no_alias_colon_error(mod, next), parser.tailrec(discard_rest_of_line).endpos({}), parser.pop();
}

void parse_alias_name(Lexer& lexer, Parser& parser, Module* mod, Token& next) {
    if (next.kind == TK_IDENT)
        parser.consume(lexer)
              .tailrec(parse_alias_body, new(parser.astspace) Var(ident_pos(mod, next), next.end));
    else no_alias_ident_error(mod, next), parser.tailrec(discard_rest_of_line).endpos({});
}

void parse_typedecl_end(Lexer& lexer, Parser& parser, Module* mod, Token& next) {
    parser.leave(new(parser.astspace) TypeDecl(parser.endpos(parser.rhs()->pos), parser.lhs(), parser.rhs()))
          .poplr();
}

void parse_typedecl_body(Lexer& lexer, Parser& parser, Module* mod, Token& next) {
    if (next.kind == TK_COLON)
        parser.consume(lexer)
              .tailrec(parse_typedecl_end)
              .rec(parse_block);
    else if (next.kind == TK_NEWLINE)
        parser.consume_and_indent(lexer)
              .tailrec(parse_typedecl_end, nullptr); // Null body for unit type.
    else no_type_colon_nl_error(mod, next), parser.tailrec(discard_rest_of_line).endpos({}), parser.pop();
}

void parse_typedecl_param(Lexer& lexer, Parser& parser, Module* mod, Token& next);

void parse_typedecl_params_separator(Lexer& lexer, Parser& parser, Module* mod, Token& next) {
    if (next.kind == TK_RPAREN) {
        slice<AST*> args = parser.take(parser.endcount());
        parser.consume(lexer)
              .tailrec(parse_typedecl_body, new(parser.astspace) Apply(parser.endpos(next), parser.pop(), args))
              .unnest();
    }
    else if (next.kind == TK_COMMA)
        parser.consume(lexer)
              .inc()
              .tailrec(parse_typedecl_param);
    else no_paramlist_sep_error(mod, next), parser.tailrec(discard_rest_of_line).endpos({}), parser.unnest().take(parser.endcount() + 1);
}

void parse_typedecl_param(Lexer& lexer, Parser& parser, Module* mod, Token& next) {
    if (next.kind == TK_IDENT)
        parser.consume(lexer)
              .tailrec(parse_typedecl_params_separator, new(parser.astspace) Var(ident_pos(mod, next), next.end));
    else no_typedecl_param_error(mod, next), parser.tailrec(discard_rest_of_line).endpos({}), parser.pop();
}

void parse_maybe_generic_typedecl(Lexer& lexer, Parser& parser, Module* mod, Token& next) {
    if (next.kind == TK_LPAREN)
        parser.consume(lexer)
              .begincount()
              .startpos(next)
              .tailrec(parse_typedecl_param)
              .nest();
    else parser.tailrec(parse_typedecl_body);
}

void parse_typedecl_name(Lexer& lexer, Parser& parser, Module* mod, Token& next) {
    if (next.kind == TK_IDENT)
        parser.consume(lexer)
              .tailrec(parse_maybe_generic_typedecl, new(parser.astspace) Var(ident_pos(mod, next), next.end));
    else no_typedecl_name_error(mod, next), parser.tailrec(discard_rest_of_line).endpos({});
}

void parse_case_end(Lexer& lexer, Parser& parser, Module* mod, Token& next) {
    parser.leave(new(parser.astspace) CaseDecl(parser.endpos((parser.rhs() ? parser.rhs() : parser.lhs())->pos), parser.lhs(), parser.rhs()))
          .poplr();
}

void parse_case(Lexer& lexer, Parser& parser, Module* mod, Token& next) {
    if (next.kind == TK_COLON)
        parser.consume(lexer)
              .tailrec(parse_case_end)
              .rec(parse_block);
    else if (next.kind == TK_NEWLINE) 
        parser.consume_and_indent(lexer)
              .tailrec(parse_case_end, nullptr);
    else no_case_colon_nl_error(mod, next), parser.tailrec(discard_rest_of_line).endpos({}), parser.pop();
}

void parse_match_end(Lexer& lexer, Parser& parser, Module* mod, Token& next) {
    parser.leave(new(parser.astspace) Binary(AST_MATCH, parser.endpos(parser.rhs()->pos), parser.lhs(), parser.rhs()))
          .poplr();
}

void parse_match(Lexer& lexer, Parser& parser, Module* mod, Token& next) {
    if (next.kind == TK_COLON)
        parser.consume(lexer)
              .tailrec(parse_match_end)
              .rec(parse_block);
    else no_block_error(mod, next), parser.tailrec(discard_rest_of_line).endpos({}), parser.pop();
}

void parse_module_end(Lexer& lexer, Parser& parser, Module* mod, Token& next) {
    parser.leave(new(parser.astspace) ModuleDecl(parser.endpos(parser.rhs()->pos), parser.lhs(), parser.rhs()))
          .poplr();
}

void parse_module_body(Lexer& lexer, Parser& parser, Module* mod, Token& next) {
    if (next.kind == TK_COLON)
        parser.consume(lexer)
              .tailrec(parse_module_end)
              .rec(parse_block);
    else no_block_error(mod, next), parser.tailrec(discard_rest_of_line).endpos({}), parser.pop();
}

void parse_module_name(Lexer& lexer, Parser& parser, Module* mod, Token& next) {
    if (next.kind == TK_IDENT)
        parser.consume(lexer)
              .tailrec(parse_module_body, new(parser.astspace) Var(ident_pos(mod, next), next.end));
    else no_module_name_error(mod, next), parser.tailrec(discard_rest_of_line).endpos({});
}

void parse_statement(Lexer& lexer, Parser& parser, Module* mod, Token& next) {
    switch (next.kind) {
        case TK_NEWLINE: parser.consume_and_indent(lexer).leave(); return; // Parse next statement.
        case TK_VAR: // Parse var declaration.
            parser.consume(lexer)
                  .startpos(next)
                  .push(nullptr) // Null type (inferred).
                  .begincount()
                  .tailrec(parse_vardecl_name)
                  .rec(parse_primary); 
            return;
        case TK_CONST: // Parse var declaration.
            parser.consume(lexer)
                  .startpos(next)
                  .begincount()
                  .tailrec(parse_constdecl_name)
                  .rec(parse_primary); 
            return;
        case TK_FUN: // Parse function declaration.
            parser.consume(lexer)
                  .startpos(next)
                  .push(nullptr) // Null type (inferred).
                  .tailrec(parse_fundecl_body)
                  .rec(parse_binary); 
            return;
        case TK_IF:
            parser.consume(lexer)
                  .startpos(next)
                  .tailrec(parse_if_condition)
                  .rec(parse_binary);
            return;
        case TK_WHILE:
            parser.consume(lexer)
                  .startpos(next)
                  .tailrec(parse_while_condition)
                  .rec(parse_binary);
            return;
        case TK_UNTIL:
            parser.consume(lexer)
                  .startpos(next)
                  .tailrec(parse_until_condition)
                  .rec(parse_binary);
            return;
        case TK_FOR:
            parser.consume(lexer)
                  .startpos(next)
                  .tailrec(parse_for_binding)
                  .rec(parse_maybe_untyped_arg);
            return;
        case TK_DEFER:
            parser.consume(lexer)
                  .startpos(next)
                  .tailrec(parse_defer);
            return;
        case TK_RETURN:
            parser.consume(lexer)
                  .startpos(next)
                  .tailrec(parse_return);
            return;
        case TK_BREAK:
            parser.consume(lexer)
                  .tailrec(parse_break_newline, new(parser.astspace) Expr(AST_BREAK, next));
            return;
        case TK_CONTINUE:
            parser.consume(lexer)
                  .tailrec(parse_continue_newline, new(parser.astspace) Expr(AST_CONTINUE, next));
            return;
        case TK_WITH:
            parser.consume(lexer)
                  .startpos(next)
                  .tailrec(parse_with_body).rec(parse_primary);
            return;
        case TK_USE:
            parser.consume(lexer)
                  .startpos(next)
                  .tailrec(parse_use_suffix).rec(parse_primary);
            return;
        case TK_ALIAS:
            parser.consume(lexer)
                  .startpos(next)
                  .tailrec(parse_alias_name);
            return;
        case TK_TYPE:
            parser.consume(lexer)
                  .startpos(next)
                  .tailrec(parse_typedecl_name);
            return;
        case TK_CASE:
            parser.consume(lexer)
                  .startpos(next)
                  .tailrec(parse_case)
                  .rec(parse_binary);
            return;
        case TK_MATCH:
            parser.consume(lexer)
                  .startpos(next)
                  .tailrec(parse_match)
                  .rec(parse_binary);
            return;
        case TK_MODULE:
            parser.consume(lexer)
                  .startpos(next)
                  .tailrec(parse_module_name);
            return;
        case TK_EOF:
            parser.consume(lexer)
                  .leave();
            return;
        default: parser.tailrec(parse_maybe_vardecl).rec(parse_binary); return;
    }
}

void parse_program(Lexer& lexer, Parser& parser, Module* mod, Token& next) {
    if (next.kind == TK_EOF) parser.consume(lexer).leave();
    else parser.rec(parse_statement);
}

void advance_parser(Module* mod) {
    Lexer& lexer = *mod->lexer;
    Parser& parser = *mod->parser;
    arena& astspace = parser.astspace;
    bindlocal<arena> space(&astspace);
    while (lexer.tokens && parser.prods.size()) {
        Token next = lexer.tokens.peek();
        if (parser.check_indent && next.kind != TK_NEWLINE)
            parser.check_indent = false, parser.indent = next.column;
        while (parser.nesting && next.kind == TK_NEWLINE) lexer.tokens.read(), next = lexer.tokens.peek();
        Production prod = parser.prods.back();
        prod(lexer, parser, mod, next);
    }
}

#define casematch(kind, type, name) case kind : { type name = (type)ast;
#define endmatch break; }

inline void write_unary(stream& io, Module* mod, Unary* node, const char* op) {
    write(io, '(', op, ' ');
    format(io, mod, node->child);
    write(io, ')');
}

inline void write_binary(stream& io, Module* mod, Binary* node, const char* op) {
    write(io, '(', op, ' ');
    format(io, mod, node->left);
    write(io, ' ');
    format(io, mod, node->right);
    write(io, ')');
}

void format(stream& io, Module* mod, const AST* const& ast) {
    // if (ast->type) write(io, '<'), format(io, mod, ast->type), write(io, '>');
    switch (ast->kind) {
    casematch(AST_PROGRAM, ASTProgram*, p)
        for (Statement* stmt : p->toplevel) format(io, mod, stmt), write(io, '\n');
        endmatch
    casematch(AST_ICONST, Const*, c) write(io, c->iconst); endmatch
    casematch(AST_FCONST, Const*, c) write(io, c->fconst); endmatch
    casematch(AST_CHCONST, Const*, c) write(io, '\'', c->chconst, '\''); endmatch
    casematch(AST_STRCONST, Const*, c) write(io, '"', const_slice<i8>{mod->bytes.text + c->strconst.byteidx, c->strconst.len}, '"'); endmatch
    casematch(AST_BOOL, Const*, c) write(io, c->bconst ? "true" : "false"); endmatch
    casematch(AST_UNIT, Const*, c) write(io, "()"); endmatch
    casematch(AST_VAR, Var*, v) write(io, mod->interner->intern_data[v->name]); endmatch
    
    casematch(AST_PLUS, Unary*, u) write_unary(io, mod, u, "+"); endmatch
    casematch(AST_NEG, Unary*, u) write_unary(io, mod, u, "-"); endmatch
    casematch(AST_DEREF, Unary*, u) write_unary(io, mod, u, "*"); endmatch
    casematch(AST_ADDROF, Unary*, u) write_unary(io, mod, u, "&"); endmatch
    casematch(AST_NOT, Unary*, u) write_unary(io, mod, u, "!"); endmatch
    casematch(AST_BITNOT, Unary*, u) write_unary(io, mod, u, "~"); endmatch
    casematch(AST_INCR, Unary*, u) write_unary(io, mod, u, "++"); endmatch
    casematch(AST_DECR, Unary*, u) write_unary(io, mod, u, "--"); endmatch
    casematch(AST_POSTINCR, Unary*, u) write(io, '('); format(io, mod, u->child); write(io, " ++)"); endmatch
    casematch(AST_POSTDECR, Unary*, u) write(io, '('); format(io, mod, u->child); write(io, " --)"); endmatch
    casematch(AST_NEW, Unary*, u) write_unary(io, mod, u, "new"); endmatch
    casematch(AST_DEL, Unary*, u) write_unary(io, mod, u, "del"); endmatch
    casematch(AST_PAREN, Unary*, u) write(io, '('); format(io, mod, u->child); write(io, ")"); endmatch
    casematch(AST_CONV, Unary*, u) 
        write(io, "cast<"); 
        format(io, mod, u->type); 
        write(io, ">("); 
        format(io, mod, u->child); 
        write(io, ")"); 
        endmatch
    casematch(AST_SIZEOF, Sizeof*, u) write(io, "|"); format(io, mod, u->child); write(io, "|"); endmatch
    casematch(AST_ADD, Binary*, b) write_binary(io, mod, b, "+"); endmatch
    casematch(AST_SUB, Binary*, b) write_binary(io, mod, b, "-"); endmatch
    casematch(AST_STAR, Binary*, b) write_binary(io, mod, b, "*"); endmatch
    casematch(AST_DIV, Binary*, b) write_binary(io, mod, b, "/"); endmatch
    casematch(AST_MOD, Binary*, b) write_binary(io, mod, b, "%"); endmatch
    casematch(AST_EXP, Binary*, b) write_binary(io, mod, b, "**"); endmatch
    casematch(AST_BITAND, Binary*, b) write_binary(io, mod, b, "&"); endmatch
    casematch(AST_BITOR, Binary*, b) write_binary(io, mod, b, "|"); endmatch
    casematch(AST_BITXOR, Binary*, b) write_binary(io, mod, b, "^"); endmatch
    casematch(AST_BITLEFT, Binary*, b) write_binary(io, mod, b, "<<"); endmatch
    casematch(AST_BITRIGHT, Binary*, b) write_binary(io, mod, b, ">>"); endmatch
    casematch(AST_LESS, Binary*, b) write_binary(io, mod, b, "<"); endmatch
    casematch(AST_LEQUAL, Binary*, b) write_binary(io, mod, b, "<="); endmatch
    casematch(AST_GREATER, Binary*, b) write_binary(io, mod, b, ">"); endmatch
    casematch(AST_GEQUAL, Binary*, b) write_binary(io, mod, b, ">="); endmatch
    casematch(AST_EQUAL, Binary*, b) write_binary(io, mod, b, "=="); endmatch
    casematch(AST_INEQUAL, Binary*, b) write_binary(io, mod, b, "!="); endmatch
    casematch(AST_AND, Binary*, b) write_binary(io, mod, b, "and"); endmatch
    casematch(AST_OR, Binary*, b) write_binary(io, mod, b, "or"); endmatch
    casematch(AST_DOT, Binary*, b) write_binary(io, mod, b, "."); endmatch
    casematch(AST_ASSIGN, Binary*, b) write_binary(io, mod, b, "="); endmatch
    casematch(AST_ADDEQ, Binary*, b) write_binary(io, mod, b, "+="); endmatch
    casematch(AST_SUBEQ, Binary*, b) write_binary(io, mod, b, "-="); endmatch
    casematch(AST_STAREQ, Binary*, b) write_binary(io, mod, b, "*="); endmatch
    casematch(AST_DIVEQ, Binary*, b) write_binary(io, mod, b, "/="); endmatch
    casematch(AST_MODEQ, Binary*, b) write_binary(io, mod, b, "%="); endmatch
    casematch(AST_EXPEQ, Binary*, b) write_binary(io, mod, b, "**="); endmatch
    casematch(AST_BITANDEQ, Binary*, b) write_binary(io, mod, b, "&="); endmatch
    casematch(AST_BITOREQ, Binary*, b) write_binary(io, mod, b, "|="); endmatch
    casematch(AST_BITXOREQ, Binary*, b) write_binary(io, mod, b, "^="); endmatch
    casematch(AST_BITLEFTEQ, Binary*, b) write_binary(io, mod, b, "<<="); endmatch
    casematch(AST_BITRIGHTEQ, Binary*, b) write_binary(io, mod, b, ">>="); endmatch
    casematch(AST_NEWARRAY, Binary*, b) write(io, "(new "), format(io, mod, b->left), write(io, "["), format(io, mod, b->right), write(io, "])"); endmatch
    casematch(AST_IS, Is*, b) write_binary(io, mod, b, "is"); endmatch
    
    casematch(AST_TYPENAME, Var*, v) write(io, '<', mod->interner->intern_data[v->name], '>'); endmatch
    casematch(AST_TYPECONST, Expr*, e) write(io, '<'), format(io, mod, e->type), write(io, '>'); endmatch
    casematch(AST_MODULENAME, Var*, v) write(io, mod->interner->intern_data[v->name]); endmatch
    casematch(AST_PTRTYPE, Unary*, u) write_unary(io, mod, u, "*"); endmatch
    casematch(AST_SLICETYPE, Binary*, i) 
        format(io, mod, i->left); 
        write(io, "[]");
        endmatch
    casematch(AST_ARRAYTYPE, Binary*, i) 
        format(io, mod, i->left); 
        write(io, '[');
        format(io, mod, i->right);
        write(io, ']');
        endmatch
    casematch(AST_FUNTYPE, Apply*, a) 
        format(io, mod, a->fn); 
        write(io, '(');
        bool first = true;
        for (iptr i = 0; i < a->args.n; i ++) {
            if (!first) write(io, ", ");
            first = false;
            format(io, mod, a->args[i]);
        }
        write(io, ')');
        endmatch
    casematch(AST_TYPEDOT, Binary*, b) write_binary(io, mod, b, "."); endmatch
    casematch(AST_TYPEVAR, Unary*, u) write(io, '<'), format(io, mod, u->child), write(io, "?>"); endmatch

    casematch(AST_INDEX, Binary*, i) 
        format(io, mod, i->left); 
        write(io, '[');
        if (i->right) format(io, mod, i->right);
        write(io, ']');
        endmatch
    casematch(AST_SLICE, Slice*, s) 
        format(io, mod, s->array); 
        write(io, '[');
        if (s->low) format(io, mod, s->low);
        write(io, ':');
        if (s->high) format(io, mod, s->high);
        write(io, ']');
        endmatch
    casematch(AST_APPLY, Apply*, a) 
        format(io, mod, a->fn); 
        write(io, '(');
        bool first = true;
        for (iptr i = 0; i < a->args.n; i ++) {
            if (!first) write(io, ", ");
            first = false;
            format(io, mod, a->args[i]);
        }
        write(io, ')');
        endmatch
    casematch(AST_TYPEINST, Apply*, a) 
        format(io, mod, a->fn); 
        write(io, '<');
        bool first = true;
        for (iptr i = 0; i < a->args.n; i ++) {
            if (!first) write(io, ", ");
            first = false;
            format(io, mod, a->args[i]);
        }
        write(io, '>');
        endmatch
    casematch(AST_CTOR, Apply*, a) 
        format(io, mod, a->fn); 
        write(io, '(');
        bool first = true;
        for (iptr i = 0; i < a->args.n; i ++) {
            if (!first) write(io, ", ");
            first = false;
            format(io, mod, a->args[i]);
        }
        write(io, ')');
        endmatch
    casematch(AST_GENCTOR, Apply*, a) 
        format(io, mod, a->fn); 
        write(io, '(');
        bool first = true;
        for (iptr i = 0; i < a->args.n; i ++) {
            if (!first) write(io, ", ");
            first = false;
            format(io, mod, a->args[i]);
        }
        write(io, ')');
        endmatch
    casematch(AST_ARRAY, List*, l) 
        write(io, '[');
        bool first = true;
        for (iptr i = 0; i < l->items.n; i ++) {
            if (!first) write(io, ", ");
            first = false;
            format(io, mod, l->items[i]);
        }
        write(io, ']');
        endmatch
    casematch(AST_SET, List*, l) 
        write(io, '{');
        bool first = true;
        for (iptr i = 0; i < l->items.n; i ++) {
            if (!first) write(io, ", ");
            first = false;
            format(io, mod, l->items[i]);
        }
        write(io, '}');
        endmatch
    casematch(AST_VARDECL, VarDecl*, d) 
        write(io, '(');  
        if (!d->ann) write(io, "var"); else format(io, mod, d->ann);
        write(io, ' ');
        format(io, mod, d->name);
        if (d->init) write(io, ": "), format(io, mod, d->init);
        write(io, ')');
        endmatch
    casematch(AST_CONSTDECL, ConstDecl*, d) 
        write(io, "(const ");  
        format(io, mod, d->name);
        write(io, ": "), format(io, mod, d->init);
        write(io, ')');
        endmatch
    casematch(AST_PTRDECL, Binary*, d) 
        write(io, '(');  
        format(io, mod, d->left);
        write(io, ' ');
        format(io, mod, d->right);
        write(io, ')');
        endmatch
    casematch(AST_FUNDECL, FunDecl*, d) 
        write(io, "(fun ");
        if (d->returned) format(io, mod, d->returned), write(io, ' ');
        format(io, mod, d->proto);
        write(io, ' ');
        if (d->body) format(io, mod, d->body);
        write(io, ')');
        for (const auto& e : d->insts) {
            write(io, '\n');
            format(stdout, mod, e.value);
        }
        endmatch
    casematch(AST_ARGS, List*, d) 
        write(io, "(args:");  
        for (iptr i = 0; i < d->items.n; i ++) {
            write(io, " ");
            format(io, mod, d->items[i]);
        }
        write(io, ')');
        endmatch
    casematch(AST_DO, List*, d) 
        write(io, "(do:");  
        for (iptr i = 0; i < d->items.n; i ++) {
            write(io, " ");
            format(io, mod, d->items[i]);
        }
        write(io, ')');
        endmatch
    casematch(AST_IF, If*, i) 
        write(io, "(if ");  
        format(io, mod, i->cond);
        write(io, ": ");
        format(io, mod, i->ifTrue);
        if (i->ifFalse) write(io, ' '), format(io, mod, i->ifFalse);
        write(io, ')');
        endmatch
    casematch(AST_WHILE, Binary*, b) 
        write(io, "(while ");  
        format(io, mod, b->left);
        write(io, ": ");
        format(io, mod, b->right);
        write(io, ')');
        endmatch
    casematch(AST_UNTIL, Binary*, b) 
        write(io, "(until ");  
        format(io, mod, b->left);
        write(io, ": ");
        format(io, mod, b->right);
        write(io, ')');
        endmatch
    casematch(AST_FOR, For*, f) 
        write(io, "(for ");  
        format(io, mod, f->binding);
        write(io, " in ");
        format(io, mod, f->items);
        write(io, ": ");
        format(io, mod, f->body);
        write(io, ')');
        endmatch
    casematch(AST_DEFER, Unary*, u) 
        write(io, "(defer ");  
        format(io, mod, u->child);
        write(io, ')');
        endmatch
    casematch(AST_RETURN, Unary*, u) 
        write(io, "(return");  
        if (u->child) write(io, ' '), format(io, mod, u->child);
        write(io, ')');
        endmatch
    casematch(AST_BREAK, Expr*, e) 
        write(io, "break");  
        endmatch
    casematch(AST_CONTINUE, Expr*, e) 
        write(io, "continue");  
        endmatch
    casematch(AST_WITH, With*, w) 
        write(io, "(with ");  
        format(io, mod, w->bound);
        write(io, ": ");
        format(io, mod, w->body);
        write(io, ')');
        endmatch
    casematch(AST_USE, Binary*, b) 
        write(io, "(use ");  
        format(io, mod, b->left);
        if (b->right) {
            write(io, " as ");
            format(io, mod, b->right);
        }
        write(io, ')');
        endmatch
    casematch(AST_ALIASDECL, Binary*, d) 
        write(io, "(alias ");  
        format(io, mod, d->left);
        write(io, ": "), 
        format(io, mod, d->right);
        write(io, ')');
        endmatch
    casematch(AST_TYPEDECL, TypeDecl*, d) 
        write(io, "(type ");  
        format(io, mod, d->name);
        if (d->body) {
            write(io, ": "), 
            format(io, mod, d->body);
        }
        write(io, ')');
        endmatch
    casematch(AST_CASEDECL, CaseDecl*, b) 
        write(io, "(case ");  
        format(io, mod, b->pattern);
        if (b->body) {
            write(io, ": ");
            format(io, mod, b->body);
        }
        write(io, ')');
        endmatch
    casematch(AST_MATCH, Binary*, b) 
        write(io, "(match ");  
        format(io, mod, b->left);
        write(io, ": ");
        format(io, mod, b->right);
        write(io, ')');
        endmatch
    casematch(AST_MODULEDECL, ModuleDecl*, b) 
        write(io, "(module ");  
        format(io, mod, b->name);
        write(io, ": ");
        format(io, mod, b->body);
        write(io, ')');
        endmatch
    default:
        break;
    }
}

Var* anon_var(Module* mod, Env* env, SourcePos pos) {
    i8 buf[16];
    i32 len = 0;

    i32 id = env->anon ++;
    buf[len ++] = '_';
    if (!id) buf[len ++] = '0';
    else {
        u32 olen = len;
        u32 c = 0, d = 0;
        u64 p = 1;
        while (p <= id) p *= 10, ++ c;
        d = c;
        while (c >= 1) {
            buf[-- c + olen] = "0123456789"[id % 10];
            id /= 10;
        }
        len += d;
    }
    slice<i8> sl = slice<i8>{new(mod->envctx->envspace) i8[len], len};
    mcpy(sl.ptr, buf, len);
    i32 name = mod->interner->intern(sl);
    return new(mod->parser->astspace) Var(pos, name);
}

Env* anon_namespace(Module* mod, Env* env) {
    i8 buf[16];
    i32 len = 0;

    i32 id = env->anon ++;
    buf[len ++] = '_';
    if (!id) buf[len ++] = '0';
    else {
        u32 olen = len;
        u32 c = 0, d = 0;
        u64 p = 1;
        while (p <= id) p *= 10, ++ c;
        d = c;
        while (c >= 1) {
            buf[-- c + olen] = "0123456789"[id % 10];
            id /= 10;
        }
        len += d;
    }
    slice<i8> sl = slice<i8>{new(mod->envctx->envspace) i8[len], len};
    mcpy(sl.ptr, buf, len);
    i32 name = mod->interner->intern(sl);
    return mod->envctx->create(ENV_LOCAL, env, name);
}

i32 basename(Module* mod, AST* ast) {
    switch (ast->kind) {
    casematch(AST_VAR, Var*, v) return v->name; endmatch;
    casematch(AST_DOT, Binary*, b) return basename(mod, b->right); endmatch;
    casematch(AST_APPLY, Binary*, b) return basename(mod, b->left); endmatch;
    casematch(AST_INDEX, Binary*, b) return basename(mod, b->left); endmatch;
    default: 
        return -1;
    }
}

bool add_implicit_returns(Module* mod, Env* env, AST*& ast) {
    switch (ast->kind) {
    casematch(AST_DO, List*, l)
        if (l->items.n == 0) return true;
        else return add_implicit_returns(mod, env, l->items[l->items.n - 1]);
        endmatch
    casematch(AST_IF, If*, i)
        if (!i->ifFalse) return true; // if statement must have both branches for implicit return to be valid.
        bool branch = add_implicit_returns(mod, env, i->ifTrue);
        if (i->ifFalse) branch = branch && add_implicit_returns(mod, env, i->ifFalse);
        return branch;
        endmatch
    casematch(AST_RETURN, Unary*, u)
        return true;
        endmatch
    casematch(AST_WITH, With*, w)
        return add_implicit_returns(mod, env, w->body);
        endmatch
    casematch(AST_CASEDECL, CaseDecl*, c)
        return add_implicit_returns(mod, env, c->body);
        endmatch
    casematch(AST_MATCH, Binary*, b)
        bool worked = true;
        for (AST* ast : ((List*)b->right)->items) worked = worked && add_implicit_returns(mod, env, ast);
        return worked;
        endmatch
    case AST_FIRST_EXPR ... AST_LAST_EXPR - 1: {
        ast = new(mod->parser->astspace) Unary(AST_RETURN, ast->pos, ast);
        return true;
    }
    default:
        return true;
    }
}

void add_defers(Module* mod, Env* env, AST*& ast, slice<AST*> defers) {
    switch (ast->kind) {
    casematch(AST_DO, List*, l)
        if (l->items.n == 0) l->items = defers;
        else add_defers(mod, env, l->items[l->items.n - 1], defers);
        endmatch
    casematch(AST_IF, If*, i)
        add_defers(mod, env, i->ifTrue, defers);
        if (i->ifFalse) add_defers(mod, env, i->ifFalse, defers);
        endmatch
    casematch(AST_WITH, With*, w)
        return add_defers(mod, env, w->body, defers);
        endmatch
    casematch(AST_CASEDECL, CaseDecl*, c)
        return add_defers(mod, env, c->body, defers);
        endmatch
    casematch(AST_MATCH, Binary*, b)
        for (AST* ast : ((List*)b->right)->items) add_defers(mod, env, ast, defers);
        endmatch
    casematch(AST_RETURN, Unary*, u)
        i32 size = (u->child ? 2 : 1) + defers.size();
        slice<AST*> list = {new(mod->parser->astspace) AST*[2 + defers.size()], 2 + defers.size()};
        if (u->child) {
            AST* val = u->child;
            Var* v = anon_var(mod, env, val->pos);
            VarDecl* d = new(mod->parser->astspace) VarDecl(u->pos, nullptr, v, val);
            u->child = v;
            list[0] = d;
        }
        for (u32 i = 0; i < defers.n; i ++) list[u->child ? i + 1 : i] = defers[i]->clone(mod->parser->astspace);
        list[list.n - 1] = u;
        ast = new(mod->parser->astspace) List(AST_DO, u->pos, list);
        endmatch
    default:
        slice<AST*> list = {new(mod->parser->astspace) AST*[1 + defers.size()], 1 + defers.size()};
        list[0] = ast;
        for (u32 i = 0; i < defers.n; i ++) list[i + 1] = defers[i]->clone(mod->parser->astspace);
        ast = new(mod->parser->astspace) List(AST_DO, ast->pos, list);
        break;
    }
}

void propagate_env_to_patterns(Module* mod, Env* env, AST* ast) {
    switch (ast->kind) {
    casematch(AST_IS, Is*, i)
        i->env = env;
        endmatch
    casematch(AST_OR, Binary*, b)
        if (b->left->kind == AST_BOOL && !((Const*)b->left)->bconst)
            propagate_env_to_patterns(mod, env, b->right);
        else if (b->right->kind == AST_BOOL && !((Const*)b->right)->bconst)
            propagate_env_to_patterns(mod, env, b->right);
        endmatch
    casematch(AST_AND, Binary*, b)
        propagate_env_to_patterns(mod, env, b->left);
        propagate_env_to_patterns(mod, env, b->right);
        endmatch
    casematch(AST_PAREN, Unary*, u)
        propagate_env_to_patterns(mod, env, u->child);
        endmatch
    default:
        break;
    }
}

TypeDecl* inst_type(Module* mod, Env* env, TypeDecl* d, slice<AST*> params);

void compute_envs(Module* mod, Env* env, AST* ast) {
    switch (ast->kind) {
    casematch(AST_PROGRAM, ASTProgram*, p)
        p->env = mod->envctx->create(ENV_GLOBAL, mod->envctx->root, mod->basename);
        p->env->decl = p;
        mod->envctx->root->def(mod->basename, e_global(p));
        p->deferred = anon_namespace(mod, p->env);
        for (Statement* ast : p->toplevel) if (ast->kind == AST_MODULEDECL) compute_envs(mod, p->env, ast);
        for (Statement* ast : p->toplevel) if (ast->kind != AST_MODULEDECL) compute_envs(mod, p->env, ast);
        for (AST* ast : p->defers) compute_envs(mod, p->deferred, ast);
        endmatch

    casematch(AST_PLUS, Unary*, u) compute_envs(mod, env, u->child); endmatch
    casematch(AST_NEG, Unary*, u) compute_envs(mod, env, u->child); endmatch
    casematch(AST_DEREF, Unary*, u) compute_envs(mod, env, u->child); endmatch
    casematch(AST_ADDROF, Unary*, u) compute_envs(mod, env, u->child); endmatch
    casematch(AST_NOT, Unary*, u) compute_envs(mod, env, u->child); endmatch
    casematch(AST_BITNOT, Unary*, u) compute_envs(mod, env, u->child); endmatch
    casematch(AST_INCR, Unary*, u) compute_envs(mod, env, u->child); endmatch
    casematch(AST_DECR, Unary*, u) compute_envs(mod, env, u->child); endmatch
    casematch(AST_POSTINCR, Unary*, u) compute_envs(mod, env, u->child); endmatch
    casematch(AST_POSTDECR, Unary*, u) compute_envs(mod, env, u->child); endmatch
    casematch(AST_NEW, Unary*, u) compute_envs(mod, env, u->child); endmatch
    casematch(AST_DEL, Unary*, u) 
        compute_envs(mod, env, u->child); 
        endmatch
    casematch(AST_PAREN, Unary*, u) compute_envs(mod, env, u->child); endmatch
    casematch(AST_SIZEOF, Sizeof*, u) compute_envs(mod, env, u->child); endmatch

    casematch(AST_ADD, Binary*, b) compute_envs(mod, env, b->left); compute_envs(mod, env, b->right); endmatch
    casematch(AST_SUB, Binary*, b) compute_envs(mod, env, b->left); compute_envs(mod, env, b->right); endmatch
    casematch(AST_STAR, Binary*, b) compute_envs(mod, env, b->left); compute_envs(mod, env, b->right); endmatch
    casematch(AST_DIV, Binary*, b) compute_envs(mod, env, b->left); compute_envs(mod, env, b->right); endmatch
    casematch(AST_MOD, Binary*, b) compute_envs(mod, env, b->left); compute_envs(mod, env, b->right); endmatch
    casematch(AST_EXP, Binary*, b) compute_envs(mod, env, b->left); compute_envs(mod, env, b->right); endmatch
    casematch(AST_BITAND, Binary*, b) compute_envs(mod, env, b->left); compute_envs(mod, env, b->right); endmatch
    casematch(AST_BITOR, Binary*, b) compute_envs(mod, env, b->left); compute_envs(mod, env, b->right); endmatch
    casematch(AST_BITXOR, Binary*, b) compute_envs(mod, env, b->left); compute_envs(mod, env, b->right); endmatch
    casematch(AST_BITLEFT, Binary*, b) compute_envs(mod, env, b->left); compute_envs(mod, env, b->right); endmatch
    casematch(AST_BITRIGHT, Binary*, b) compute_envs(mod, env, b->left); compute_envs(mod, env, b->right); endmatch
    casematch(AST_LESS, Binary*, b) compute_envs(mod, env, b->left); compute_envs(mod, env, b->right); endmatch
    casematch(AST_LEQUAL, Binary*, b) compute_envs(mod, env, b->left); compute_envs(mod, env, b->right); endmatch
    casematch(AST_GREATER, Binary*, b) compute_envs(mod, env, b->left); compute_envs(mod, env, b->right); endmatch
    casematch(AST_GEQUAL, Binary*, b) compute_envs(mod, env, b->left); compute_envs(mod, env, b->right); endmatch
    casematch(AST_EQUAL, Binary*, b) compute_envs(mod, env, b->left); compute_envs(mod, env, b->right); endmatch
    casematch(AST_INEQUAL, Binary*, b) compute_envs(mod, env, b->left); compute_envs(mod, env, b->right); endmatch
    casematch(AST_AND, Binary*, b) compute_envs(mod, env, b->left); compute_envs(mod, env, b->right); endmatch
    casematch(AST_OR, Binary*, b) compute_envs(mod, env, b->left); compute_envs(mod, env, b->right); endmatch
    casematch(AST_DOT, Binary*, b) 
        compute_envs(mod, env, b->left); 
        compute_envs(mod, env, b->right); 
        endmatch
    casematch(AST_ASSIGN, Binary*, b) compute_envs(mod, env, b->left); compute_envs(mod, env, b->right); endmatch
    casematch(AST_ADDEQ, Binary*, b) compute_envs(mod, env, b->left); compute_envs(mod, env, b->right); endmatch
    casematch(AST_SUBEQ, Binary*, b) compute_envs(mod, env, b->left); compute_envs(mod, env, b->right); endmatch
    casematch(AST_STAREQ, Binary*, b) compute_envs(mod, env, b->left); compute_envs(mod, env, b->right); endmatch
    casematch(AST_DIVEQ, Binary*, b) compute_envs(mod, env, b->left); compute_envs(mod, env, b->right); endmatch
    casematch(AST_MODEQ, Binary*, b) compute_envs(mod, env, b->left); compute_envs(mod, env, b->right); endmatch
    casematch(AST_EXPEQ, Binary*, b) compute_envs(mod, env, b->left); compute_envs(mod, env, b->right); endmatch
    casematch(AST_BITANDEQ, Binary*, b) compute_envs(mod, env, b->left); compute_envs(mod, env, b->right); endmatch
    casematch(AST_BITOREQ, Binary*, b) compute_envs(mod, env, b->left); compute_envs(mod, env, b->right); endmatch
    casematch(AST_BITXOREQ, Binary*, b) compute_envs(mod, env, b->left); compute_envs(mod, env, b->right); endmatch
    casematch(AST_BITLEFTEQ, Binary*, b) compute_envs(mod, env, b->left); compute_envs(mod, env, b->right); endmatch
    casematch(AST_BITRIGHTEQ, Binary*, b) compute_envs(mod, env, b->left); compute_envs(mod, env, b->right); endmatch
    casematch(AST_NEWARRAY, Binary*, b) compute_envs(mod, env, b->left); compute_envs(mod, env, b->right); endmatch
    
    casematch(AST_IS, Is*, b) 
        compute_envs(mod, env, b->left);
        if (!b->env) b->env = anon_namespace(mod, env);
        else b->external_env = true;
        endmatch
    
    casematch(AST_ARRAYTYPE, Binary*, b) compute_envs(mod, env, b->left); endmatch
    casematch(AST_SLICETYPE, Binary*, b) compute_envs(mod, env, b->left); endmatch
    casematch(AST_PTRTYPE, Unary*, u) compute_envs(mod, env, u->child); endmatch
    casematch(AST_FUNTYPE, Apply*, a)
        compute_envs(mod, env, a->fn);
        for (AST* ast : a->args) compute_envs(mod, env, ast);
        endmatch
    casematch(AST_APPLY, Apply*, a)
        compute_envs(mod, env, a->fn);
        for (AST* ast : a->args) compute_envs(mod, env, ast);
        endmatch
    casematch(AST_ARRAY, List*, l)
        endmatch
    casematch(AST_SET, List*, l)
        endmatch
    casematch(AST_VARDECL, VarDecl*, d)
        if (d->ann) compute_envs(mod, env, d->ann);
        env->def(d->basename = basename(mod, d->name), e_var(nullptr, d));
        if (d->init) compute_envs(mod, env, d->init);
        endmatch
    casematch(AST_CONSTDECL, ConstDecl*, d)
        env->def(d->basename = basename(mod, d->name), e_const(nullptr, d));
        compute_envs(mod, env, d->init);
        endmatch
    casematch(AST_INDEX, Binary*, u) 
        compute_envs(mod, env, u->left); 
        if (u->right) compute_envs(mod, env, u->right); 
        endmatch
    casematch(AST_SLICE, Slice*, s) 
        compute_envs(mod, env, s->array); 
        if (s->low) compute_envs(mod, env, s->low); 
        if (s->high) compute_envs(mod, env, s->high); 
        endmatch
    casematch(AST_FUNDECL, FunDecl*, d)
        if (d->proto->kind != AST_APPLY) return no_function_param_list_error(mod, d->proto);

        i32 name = d->basename = basename(mod, d->proto);
        d->env = mod->envctx->create(ENV_FUN, env, name);
        d->env->decl = d;
        if (d->returned) compute_envs(mod, d->env, d->returned);

        // slice<AST*>& args = ((Apply*)d->proto)->args;
        // for (AST* ast : args) compute_envs(mod, d->env, ast);
        // if (args.size() > 0 && args[0]->kind != AST_VARDECL && args[0]->kind != AST_STAR) { // Assume it's a this parameter
        //     args[0] = new(mod->parser->astspace) VarDecl(
        //         args[0]->pos,
        //         args[0], 
        //         new(mod->parser->astspace) Var(args[0]->pos, mod->interner->intern("this")),
        //         nullptr
        //     );
        // }
        // else if (env->kind == ENV_TYPE) {
        //     AST* ast = env->decl;
        //     if (!ast) unreachable("Somehow didn't find typedecl for method.");
        //     AST* v = nullptr;
        //     if (ast->kind == AST_TYPEDECL) v = ((TypeDecl*)ast)->name->clone(mod->parser->astspace);
        //     else if (ast->kind == AST_CASEDECL) v = ((CaseDecl*)ast)->pattern->clone(mod->parser->astspace);
        //     if (!v) unreachable("Typedecl was somehow neither a type nor case.");

        //     slice<AST*> newargs = { new(mod->parser->astspace) AST*[args.n + 1], args.n + 1 };
        //     newargs[0] = new(mod->parser->astspace) VarDecl(
        //         v->pos,
        //         v, 
        //         new(mod->parser->astspace) Var(v->pos, mod->interner->intern("this")),
        //         nullptr
        //     );
        //     for (i32 i = 0; i < args.n; i ++) newargs[i + 1] = args[i];
        //     ((Apply*)d->proto)->args = newargs;
        //     env->def(name, e_fun(nullptr, d)); // We can define a method here since we are already in the type env.
        // }
        // else env->def(name, e_fun(nullptr, d)); // Only define non-methods

        compute_envs(mod, d->env, d->proto); // Declare arguments, if any.
        if (d->body) {
            add_implicit_returns(mod, d->env, d->body);
            compute_envs(mod, d->env, d->body); // Traverse function body, if present.
        }
        endmatch
    casematch(AST_ARGS, List*, l)
        for (AST* ast : l->items) compute_envs(mod, env, ast);
        endmatch
    casematch(AST_DO, List*, l)
        mod->ndefers.push(mod->defers.size());
        for (AST*& ast : l->items) {
            bool last = ast == l->items[l->items.n - 1];
            if (last && mod->defers.size() > mod->ndefers.back()) {
                add_defers(mod, env, ast, slice<AST*>{&mod->defers[mod->ndefers.back()], mod->defers.size() - mod->ndefers.back()});
                while (mod->defers.size() > mod->ndefers.back()) mod->defers.pop();
                compute_envs(mod, env, ast);
                break;
            }
            else compute_envs(mod, env, ast);
        }
        mod->ndefers.pop();
        endmatch
    casematch(AST_IF, If*, i)
        i->env = mod->envctx->create(ENV_LOCAL, env, mod->interner->intern("if"));
        i->env->decl = i;
        propagate_env_to_patterns(mod, i->env, i->cond);
        compute_envs(mod, i->env, i->cond);
        compute_envs(mod, i->env, i->ifTrue);
        if (i->ifFalse) compute_envs(mod, i->env, i->ifFalse);
        endmatch
    casematch(AST_WHILE, Loop*, l)
        l->env = mod->envctx->create(ENV_LOCAL, env, mod->interner->intern("while"));
        l->env->decl = l;
        propagate_env_to_patterns(mod, l->env, l->cond);
        compute_envs(mod, l->env, l->cond);
        compute_envs(mod, l->env, l->body);
        endmatch
    casematch(AST_UNTIL, Loop*, l)
        l->env = mod->envctx->create(ENV_LOCAL, env, mod->interner->intern("until"));
        l->env->decl = l;
        propagate_env_to_patterns(mod, l->env, l->cond);
        compute_envs(mod, l->env, l->cond);
        compute_envs(mod, l->env, l->body);
        endmatch
    casematch(AST_FOR, For*, f)
        f->env = mod->envctx->create(ENV_LOCAL, env, mod->interner->intern("for"));
        f->env->decl = f;
        if (!f->transformed) {
            f->transformed = true;
            AST* binding = f->binding;
            AST* container = new(mod->parser->astspace) VarDecl(f->binding->pos, nullptr, new(mod->parser->astspace) Var(f->binding->pos, mod->interner->intern("_items")), f->items);
            slice<AST*> container_slice = { new(mod->parser->astspace) AST*(new(mod->parser->astspace) Var(f->items->pos, mod->interner->intern("_items"))), 1 };
            AST* iter = new(mod->parser->astspace) VarDecl(f->binding->pos, nullptr, new(mod->parser->astspace) Var(f->pos, mod->interner->intern("_iter")), 
                new(mod->parser->astspace) Apply(f->binding->pos, new(mod->parser->astspace) Var(f->pos, mod->interner->intern("iter")), container_slice)
            );
            slice<AST*> iter_slice = { new(mod->parser->astspace) AST*(new(mod->parser->astspace) Var(f->pos, mod->interner->intern("_iter"))), 1 };
            AST* item = new(mod->parser->astspace) Apply(f->pos, new(mod->parser->astspace) Var(f->pos, mod->interner->intern("read")), iter_slice);
            AST* match = new(mod->parser->astspace) If(f->pos,
                new(mod->parser->astspace) Is(f->binding->pos, item, f->binding),
                f->body, nullptr
            );
            AST* incr = new(mod->parser->astspace) Binary(AST_ASSIGN, f->pos, iter_slice[0], new(mod->parser->astspace) Apply(f->pos, new(mod->parser->astspace) Var(f->pos, mod->interner->intern("next")), iter_slice));
            AST* cond = new(mod->parser->astspace) Apply(f->pos, new(mod->parser->astspace) Var(f->pos, mod->interner->intern("empty")), iter_slice);
            slice<AST*> body_block = { new(mod->parser->astspace) AST*[2], 2 };
            body_block[0] = match;
            body_block[1] = incr;
            f->body = new(mod->parser->astspace) Loop(AST_UNTIL, f->pos, cond, 
                new(mod->parser->astspace) List(AST_DO, f->pos, body_block)
            );
            ((Loop*)f->body)->env = anon_namespace(mod, f->env);
            slice<AST*> outer_block = { new(mod->parser->astspace) AST*[3], 3 };
            outer_block[0] = container;
            outer_block[1] = iter;
            outer_block[2] = f->body;
            f->body = new(mod->parser->astspace) List(AST_DO, f->pos, outer_block);
        }
        compute_envs(mod, f->env, f->body);
        endmatch
    casematch(AST_DEFER, Unary*, u)
        Env* p = env;
        Env* nearest_mod = nullptr;
        while (p->kind != ENV_ROOT && p->kind != ENV_GLOBAL && p->kind != ENV_FUN) {
            if (p->kind == ENV_MOD && !nearest_mod) nearest_mod = p;
            p = p->parent;
        }
        AST* todefer = u->child;
        if (nearest_mod && nearest_mod->decl) {
            ModuleDecl* m = (ModuleDecl*)nearest_mod->decl;
            todefer = new(mod->parser->astspace) With(todefer->pos, m->name, todefer);
        }
        if (p->kind == ENV_FUN) mod->defers.push(todefer);
        else if (p->kind == ENV_GLOBAL) {
            Entry* e = p->parent->lookup(p->name);
            if (e) ((ASTProgram*)e->ast)->defers.push(todefer);
        }
        else return incorrect_defer_env_error(mod, u, p);
        endmatch
    casematch(AST_RETURN, Unary*, u)
        if (u->child) compute_envs(mod, env, u->child);
        endmatch
    casematch(AST_BREAK, Expr*, e)
        endmatch
    casematch(AST_CONTINUE, Expr*, e)
        endmatch
    casematch(AST_WITH, With*, w)
        w->env = anon_namespace(mod, env);
        compute_envs(mod, w->env, w->body);
        endmatch
    casematch(AST_USE, Binary*, b)
        Env* modenv = nullptr;
        i32 modname;
        if (b->left->kind == AST_STRCONST) {
            auto c = ((Const*)b->left)->strconst;
            const_slice<i8> str = { mod->bytes.text + c.byteidx, c.len };
            i32 dotidx = -1;
            for (i32 i = 0; i < c.len; i ++) if (str[i] == '.') dotidx = i;
            if (dotidx < 0) {
                str.n += 4;
                i8* new_ptr = (i8*)mod->parser->astspace.alloc(str.n);
                mcpy(new_ptr, str.ptr, str.n - 4);
                mcpy(new_ptr + str.n - 4, ".cl", 4);
                str.ptr = new_ptr;
            }
            Module* imported = compile_module(mod->cloverinst, mod->interner, mod->typectx, mod->envctx, str);
            if (!imported) return;
            mod->deps.push(imported);
            modenv = imported->parser->program->env;
            modname = imported->basename;
        }
        else if (b->left->kind == AST_VAR) {
            modname = ((Var*)b->left)->name;
            auto* def = env->lookup(modname);
            if (!def || (def->kind != E_MOD && def->kind != E_GLOBAL))
                return no_such_module_error(mod, b->left);
            else if (def->kind == E_MOD) modenv = ((ModuleDecl*)def->ast)->env;
            else if (def->kind == E_GLOBAL) modenv = ((ASTProgram*)def->ast)->env;
        }
        if (!modenv) return no_such_module_error(mod, b->left);
        else if (b->right) {
            ModuleDecl* new_decl = new(mod->parser->astspace) ModuleDecl(b->pos, b->right, nullptr);
            new_decl->env = modenv;
            new_decl->basename = ((Var*)b->right)->name;
            env->def(((Var*)b->right)->name, e_mod(new_decl));
        }
        else env->siblings.push(modenv);
        endmatch
    casematch(AST_ALIASDECL, AliasDecl*, d)
        env->def(d->basename = basename(mod, d->name), e_alias(nullptr, d));
        endmatch
    casematch(AST_TYPEDECL, TypeDecl*, d)
        i32 name = d->basename = basename(mod, d->name);
        d->env = mod->envctx->create(ENV_TYPE, env, name);
        d->env->decl = d;
        if (d->name->kind == AST_APPLY) { // Generic
            for (AST* ast : ((Apply*)d->name)->args) {
                if (ast->kind != AST_VAR) 
                    return non_type_param_in_gentype_error(mod, d, ast);
                else d->env->def(((Var*)ast)->name, e_type(VOID, nullptr)); // Placeholder
            }
            d->generic = true;
            slice<AST*> protoparams = { new(mod->parser->astspace) AST*[((Apply*)d->name)->args.n], ((Apply*)d->name)->args.n };
            for (i32 i = 0; i < ((Apply*)d->name)->args.n; i ++) {
                AST* arg = ((Apply*)d->name)->args[i];
                protoparams[i] = new(mod->parser->astspace) Var(arg->pos, ((Var*)arg)->name);
                protoparams[i]->kind = AST_TYPENAME, protoparams[i]->type = mod->typectx->defvar(mod, d->env);
            }
            d->prototype = inst_type(mod, env, d, protoparams);
            if (!d->prototype) unreachable("Failed to instantiate prototype.");
            else d->prototype->is_prototype = true;
        }
        else if (d->name->kind == AST_DOT || d->kind == AST_TYPEDOT) 
            return dot_in_typedecl_name_error(mod, d);
        else if (d->name->kind != AST_VAR) 
            return no_name_in_typedecl_error(mod, d);
        if (!d->generic && d->body) compute_envs(mod, d->env, d->body);
        if (d->generic) env->def(name, e_gentype(d));
        else env->def(name, e_type(nullptr, d));
        endmatch
    casematch(AST_CASEDECL, CaseDecl*, d)
        i32 name = basename(mod, d->pattern);
        if (env->kind == ENV_TYPE && name != -1) {
            d->env = mod->envctx->create(ENV_TYPE, env, name);
            env->def(name, e_case(nullptr, d));
        }
        else if (env->kind != ENV_TYPE) d->env = anon_namespace(mod, env);
        else return no_name_in_type_case_error(mod, d);
        d->env->decl = d;
        d->basename = name;
        if (d->env->kind == ENV_TYPE) {
            compute_envs(mod, d->env, d->pattern); 
            if (d->body) compute_envs(mod, d->env, d->body);
        }
        endmatch
    casematch(AST_MATCH, Binary*, b)
        compute_envs(mod, env, b->left);
        compute_envs(mod, env, b->right);
        endmatch
    casematch(AST_MODULEDECL, ModuleDecl*, d)
        i32 name = d->basename = basename(mod, d->name);
        d->env = mod->envctx->create(ENV_MOD, env, name);
        d->env->decl = d;
        compute_envs(mod, d->env, d->body);
        env->def(name, e_mod(d));
        endmatch
    casematch(AST_TYPEVAR, Unary*, u)
        i32 name = ((Var*)u->child)->name;
        Entry* existing = env->lookup(name);
        if (!existing || env->find(name) != env) {
            Type* var = mod->typectx->defvar(mod, env);
            env->def(((Var*)u->child)->name, e_alias(var, nullptr));
            u->type = var;
        }
        else if (existing) {
            if (existing->kind == E_TYPE && existing->type->kind == T_VAR)
                u->type = existing->type;
            else redefined_type_var_error(mod, u, existing->ast);
        }
        endmatch
    default:
        break;
    }
}

inline bool is_type(AST* ast) {
    return ast->kind >= AST_FIRST_TYPE && ast->kind < AST_LAST_TYPE; 
}

Type* lookup_type(Module* mod, Env* env, i32 name);
Type* compute_decl_type(Module* mod, Env* env, AST* ast);

enum TypeDeclKind {
    UNKNOWN_KIND,
    HAS_FIELD,
    HAS_CASE,
    NO_FIELDS_OR_CASES
};

inline void handle_decl(Module* mod, Env* env, vec<pair<i32, Type*>, 256, arena>& member_stack, TypeDeclKind& kind, AST* ast) {
    switch (ast->kind) {
    casematch(AST_DO, List*, l)
        for (AST* inner : l->items) handle_decl(mod, env, member_stack, kind, inner);
        endmatch
    casematch(AST_CASEDECL, CaseDecl*, v)
        if (kind != UNKNOWN_KIND && kind != HAS_CASE) return case_in_non_union_error(mod, v);
        member_stack.push({ v->basename, v->type });
        kind = HAS_CASE;
        endmatch
    casematch(AST_VARDECL, VarDecl*, v)
        if (!v->ann) return inferred_field_error(mod, v);
        if (kind != UNKNOWN_KIND && kind != HAS_FIELD) return field_in_non_struct_error(mod, v);
        member_stack.push({ v->basename, v->ann->type });
        kind = HAS_FIELD;
        endmatch
    casematch(AST_PTRDECL, Binary*, b)
        handle_decl(mod, env, member_stack, kind, b->left);
        endmatch
    default:
        if (is_type(ast)) {
            if (kind != UNKNOWN_KIND) return unexpected_type_member_error(mod, ast);
            member_stack.push({ -1, ast->type });
            kind = NO_FIELDS_OR_CASES;
        }
    }
}

// Determines the type declared by a type declaration and returns it.
Type* compute_decl_type(Module* mod, Env* env, AST* ast) {
    vec<pair<i32, Type*>, 256, arena> member_stack;
    member_stack.alloc = &mod->typectx->typespace;

    switch (ast->kind) {
    casematch(AST_CASEDECL, CaseDecl*, d)
        Type* recur = nullptr;
        if (d->body) {
            d->env->def(d->basename, e_type(d->type = recur = mod->typectx->defvar(mod, d->env), d)); // Define type as an empty type variable for recursive types.
            detect_types(mod, d->env, d->body); // Resolve body types.
        }
        i32 init_size = member_stack._size;

        TypeDeclKind kind = UNKNOWN_KIND;
        if (!d->body) return mod->typectx->def<NamedType>(d->basename, d->env, UNIT);
        else if (d->body->kind == AST_DO)
            for (AST* inner : ((List*)d->body)->items) handle_decl(mod, d->env, member_stack, kind, inner);
        else handle_decl(mod, d->env, member_stack, kind, d->body);

        Type* result = UNIT;
        i32 n_members = member_stack._size - init_size;
        pair<i32, Type*>* arr;
        switch (kind) {
        case HAS_FIELD:
            arr = (pair<i32, Type*>*)mod->typectx->typespace.alloc(sizeof(pair<i32, Type*>) * n_members);
            for (i32 i = 0; i < n_members; i ++) arr[i] = member_stack[init_size + i];
            result = mod->typectx->def<StructType>(d->basename, d->env, slice<pair<i32, Type*>>{ arr, n_members });
            break;
        case HAS_CASE:
            arr = (pair<i32, Type*>*)mod->typectx->typespace.alloc(sizeof(pair<i32, Type*>) * n_members);
            for (i32 i = 0; i < n_members; i ++) arr[i] = member_stack[init_size + i], arr[i].second->is_case = true;;
            result = mod->typectx->def<UnionType>(d->basename, d->env, slice<pair<i32, Type*>>{ arr, n_members });
            break;
        case NO_FIELDS_OR_CASES:
            result = mod->typectx->def<NamedType>(d->basename, d->env, member_stack.back().second);
            type_env(result)->def(mod->interner->intern("this"), e_var(member_stack.back().second, nullptr));
            break;
        case UNKNOWN_KIND:
            result = mod->typectx->def<NamedType>(d->basename, d->env, UNIT);
            type_env(result)->def(mod->interner->intern("this"), e_var(UNIT, nullptr));
            break;
        }
        if (recur) { 
            *((VarType*)recur)->binding = result;
            d->env->lookup(d->basename)->type = d->type = result;
        }
        return result;
        endmatch
    casematch(AST_TYPEDECL, TypeDecl*, d)
        Type* recur = nullptr;
        if (d->body) {
            d->env->def(d->basename, e_type(d->type = recur = mod->typectx->defvar(mod, d->env), d)); // Define type as an empty type variable for recursive types.
            detect_types(mod, d->env, d->body); // Resolve body types.
        }
        i32 init_size = member_stack._size;

        TypeDeclKind kind = UNKNOWN_KIND;
        if (!d->body) return mod->typectx->def<NamedType>(d->basename, d->env, UNIT);
        else if (d->body->kind == AST_DO)
            for (AST* inner : ((List*)d->body)->items) handle_decl(mod, d->env, member_stack, kind, inner);
        else handle_decl(mod, d->env, member_stack, kind, d->body);

        Type* result = UNIT;
        i32 n_members = member_stack._size - init_size;
        pair<i32, Type*>* arr;
        switch (kind) {
        case HAS_FIELD:
            arr = (pair<i32, Type*>*)mod->typectx->typespace.alloc(sizeof(pair<i32, Type*>) * n_members);
            for (i32 i = 0; i < n_members; i ++) arr[i] = member_stack[init_size + i];
            result = mod->typectx->def<StructType>(d->basename, d->env, slice<pair<i32, Type*>>{ arr, n_members });
            break;
        case HAS_CASE:
            arr = (pair<i32, Type*>*)mod->typectx->typespace.alloc(sizeof(pair<i32, Type*>) * n_members);
            for (i32 i = 0; i < n_members; i ++) arr[i] = member_stack[init_size + i], arr[i].second->is_case = true;
            result = mod->typectx->def<UnionType>(d->basename, d->env, slice<pair<i32, Type*>>{ arr, n_members });
            break;
        case NO_FIELDS_OR_CASES:
            result = mod->typectx->def<NamedType>(d->basename, d->env, member_stack.back().second);
            type_env(result)->def(mod->interner->intern("this"), e_var(member_stack.back().second, nullptr));
            break;
        case UNKNOWN_KIND:
            result = mod->typectx->def<NamedType>(d->basename, d->env, UNIT);
            type_env(result)->def(mod->interner->intern("this"), e_var(UNIT, nullptr));
            break;
        }
        if (recur) {
            *((VarType*)recur)->binding = result;
            d->env->lookup(d->basename)->type = d->type = result;
        }
        return result;
        endmatch
    casematch(AST_ALIASDECL, AliasDecl*, d)
        detect_types(mod, env, d->body); // Resolve body types.
        i32 name = d->basename; // We can assume it's a var due to the grammar.
        env->lookup(name)->type = d->body->type;
        return d->body->type;
        endmatch
    default:
        return nullptr;
    }
}

Type* compute_env_type(Module* mod, Env* env) {
    if (!env->parent) return nullptr;
    if (env->decl) {
        if (env->decl->type) return env->decl->type;
        else return compute_decl_type(mod, env->parent, env->decl);
    }
    else {
        Entry* e = env->parent->lookup(env->name);
        if (e && (e->kind == E_TYPE || e->kind == E_CASE)) return e->type;
        return nullptr;
    }
}

// Computes the type of a type declaration if it hasn't been computed already.
Type* lookup_type(Module* mod, Env* env, i32 name) {
    Entry* e = env->lookup(name);
    if (!e) return nullptr;
    else if (e->type) return e->type;
    else return e->type = e->ast->type = compute_decl_type(mod, env, e->ast);
}

// Returns the associated environment, if any, of an AST node.
Env* ast_env(Env* env, AST* ast) {
    switch (ast->kind) {
        casematch(AST_TYPENAME, Var*, v)
            return type_env(v->type);
            endmatch
        casematch(AST_PTRTYPE, Unary*, u)
            return type_env(u->child->type);
            endmatch
        casematch(AST_TYPEDOT, Binary*, b)
            return type_env(b->type);
            endmatch
        casematch(AST_TYPEVAR, Unary*, u)
            return type_env(u->type);
            endmatch
        casematch(AST_TYPEINST, Apply*, b)
            return type_env(b->type);
            endmatch
        casematch(AST_MODULENAME, Var*, v)
            Entry* e = env->lookup(v->name);
            if (e && e->kind == E_MOD) return ((ModuleDecl*)e->ast)->env;
            return nullptr;
            endmatch
        casematch(AST_FUNDECL, FunDecl*, d)
            return d->env;
            endmatch
        casematch(AST_TYPEDECL, TypeDecl*, d)
            return d->env;
            endmatch
        casematch(AST_CASEDECL, CaseDecl*, d)
            return d->env;
            endmatch
        casematch(AST_DOT, Binary*, b)
            Env* lenv = ast_env(env, b->left);
            if (lenv) return ast_env(lenv, b->right);
            else return nullptr;
            endmatch
        default:
            return nullptr;
    }
}

Type* method_parent(Module* mod, Apply* a) {
    if (a->args.n && a->args[0]->kind == AST_VARDECL && ((VarDecl*)a->args[0])->basename == mod->interner->intern("this"))
        return ((VarDecl*)a->args[0])->type;
    return nullptr;
}

pair<Type*, Env*>* call_parent(Module* mod, Apply* a) {
    // print("resolving "), format(stdout, mod, a), print('\n');
    if (a->args.n && a->args[0]->type && a->fn->kind == AST_VAR) {
        Type* conc = fullsimplify(*mod->typectx, a->args[0]->type);
        // print("method ", mod->interner->str(((Var*)a->fn)->name), " : "), format(stdout, mod, conc), print('\n');
        // for (const auto& e : mod->envctx->methods) {
        //     print("method ", mod->interner->str(e.key), " is defined for");
        //     for (const auto& p : e.value) print(' '), format(stdout, mod, p.first);
        //     print('\n');
        // }
        pair<Type*, Env*>* ast = mod->envctx->find_method(((Var*)a->fn)->name, conc, mod);
        if (ast) {
            // print("resolved call to "), format(stdout, mod, ast->first), print('\n');
            return ast;
        }
    }
    return nullptr;
}

Symbol inst_symbol(Module* mod, Symbol name, const TypeTuple& params) {
    const_slice<i8> prev = mod->interner->str(name);
    iptr len = prev.n + 2;
    for (Type* t : params.types) len += mod->interner->str(fullconcrete(*mod->typectx, t)->env->name).n + 2;
    i8* name_space = new(mod->parser->astspace) i8[len];
    i8* writer = (i8*)mcpy(name_space, prev.ptr, prev.n);
    *writer ++ = '_';
    *writer ++ = '_';
    for (Type* t : params.types) {
        *writer ++ = '_';
        *writer ++ = '_';
        const_slice<i8> tname = mod->interner->str(fullconcrete(*mod->typectx, t)->env->name);
        writer = (i8*)mcpy(writer, tname.ptr, tname.n);
    }
    return mod->interner->intern({ name_space, len });
}

TypeDecl* inst_type(Module* mod, Env* env, TypeDecl* d, slice<AST*> params) {
    slice<AST*> vars = ((Apply*)d->name)->args;
    slice<Type*> types = { new(mod->typectx->typespace) Type*[params.n], params.n };
    for (int i = 0; i < params.n; i ++) types[i] = params[i]->type;
    TypeTuple tup(types);

    auto it = d->insts.find(tup);
    if (it != d->insts.end()) return it->value;
    else {
        TypeDecl* inst = (TypeDecl*)d->clone(mod->parser->astspace);
        d->insts.put(tup, inst);
        inst->name = new(mod->parser->astspace) Var(d->name->pos, inst_symbol(mod, d->basename, tup));
        compute_envs(mod, d->env->parent, inst);
        for (u32 i = 0; i < types.n; i ++) inst->env->def(((Var*)vars[i])->name, e_alias(types[i], nullptr));
        for (iptr i = 0; i < vars.n; i ++)
            inst->env->def(((Var*)vars[i])->name, e_type(types[i], nullptr));
        detect_types(mod, inst->env->parent, inst);
        mod->envctx->finalize_methods(mod);
        return inst;
    }
}

FunDecl* inst_fun(Module* mod, Env* env, FunDecl* d, slice<AST*> params) {
    slice<AST*> vars = ((Apply*)d->proto)->args;
    i64 n_generic = 0;
    for (i64 i = 0; i < params.n; i ++) if (vars[i]->kind == AST_VAR
        || (vars[i]->kind == AST_VARDECL && !isconcrete(vars[i]->type))) n_generic ++;
    slice<Type*> types = { new(mod->typectx->typespace) Type*[n_generic], n_generic };
    i64 written = 0;
    bool errored = false;
    for (i64 i = 0; i < params.n; i ++) if (vars[i]->kind == AST_VAR
        || (vars[i]->kind == AST_VARDECL && !isconcrete(vars[i]->type))) {
        bool unifiable = unify(vars[i]->type, params[i]->type);
        if (!unifiable) {
            incompatible_argument_error(mod, params[i], fullsimplify(*mod->typectx, params[i]->type), vars[i]->type);
            errored = true;
        }
    }
    if (errored) {
        for (AST* v : vars) unbind(v->type);
        return nullptr;
    }
    map<i32, Type*> aliases;
    for (const auto& e : d->env->entries) {
        if (e.value.kind == E_ALIAS && e.value.type && e.value.type->kind == T_VAR)
            aliases.put(e.key, fullsimplify(*mod->typectx, *((VarType*)e.value.type)->binding));
    }
    for (i64 i = 0; i < params.n; i ++) if (vars[i]->kind == AST_VAR
        || (vars[i]->kind == AST_VARDECL && !isconcrete(vars[i]->type))) {
        types[written ++] = fullsimplify(*mod->typectx, vars[i]->type);
        unbind(vars[i]->type);
    }
    TypeTuple tup(types);
    // bool first = true;
    // for (Type* t : types) {
    //     if (!first) print(", ");
    //     first = false;
    //     format(stdout, mod, t);
    // }
    // print(")\n");

    auto it = d->insts.find(tup);
    if (it != d->insts.end()) return it->value;
    else {
        FunDecl* inst = (FunDecl*)d->clone(mod->parser->astspace);
        d->insts.put(tup, inst);
        inst->isinst = true;
        ((Apply*)inst->proto)->fn = new(mod->parser->astspace) Var(d->proto->pos, inst_symbol(mod, d->basename, tup));
        iptr j = 0;
        for (iptr i = 0; i < vars.n; i ++) {
            if (vars[i]->kind == AST_VAR
                || (vars[i]->kind == AST_VARDECL && !isconcrete(vars[i]->type))) {
                i32 basename = vars[i]->kind == AST_VAR ? ((Var*)vars[i])->name : ((VarDecl*)vars[i])->basename;
                VarDecl* decl = new(mod->parser->astspace) VarDecl(
                    vars[i]->pos,
                    new(mod->parser->astspace) Var(params[i]->pos, types[j]->env->name),
                    new(mod->parser->astspace) Var(vars[i]->pos, basename),
                    nullptr
                );
                decl->ann->kind = AST_TYPENAME;
                decl->ann->type = types[j];
                ((Apply*)inst->proto)->args[i] = decl;
                j ++;
            }
        }
        compute_envs(mod, d->env->parent, inst);
        for (const auto& e : aliases) inst->env->def(e.key, e_alias(e.value, nullptr));
        detect_types(mod, inst->env->parent, inst);
        infer(mod, inst->env->parent, inst);
        typecheck(mod, inst->env->parent, inst);
        d->insts.put(tup, inst);
        return inst;
    }
}

void detect_types_pattern(Module* mod, Env* env, AST* ast) {
    switch (ast->kind) {
    casematch(AST_ICONST, Const*, c) c->type = ICONST; endmatch
    casematch(AST_FCONST, Const*, c) c->type = FCONST; endmatch
    casematch(AST_STRCONST, Const*, c) c->type = STRING; endmatch
    casematch(AST_CHCONST, Const*, c) c->type = CHAR; endmatch
    casematch(AST_BOOL, Const*, c) c->type = BOOL; endmatch
    casematch(AST_UNIT, Const*, c) c->type = UNIT; endmatch
    casematch(AST_VAR, Var*, v)
        if (v->name == mod->interner->intern("_")) v->name = anon_var(mod, env, v->pos)->name;
        v->type = mod->typectx->defvar(mod, env);
        Entry* e = env->lookup(v->name);
        if (e) { // Might be typename.
            // format(stdout, mod, v), print('\n');
            if (e->kind == E_TYPE || e->kind == E_CASE) detect_types(mod, env, v);
            if (!is_type(v) && env->find(v->name) == env && e->ast != v) {
                v->type = ERROR;
                redefined_pattern_error(mod, v, (VarDecl*)e->ast);
            }
        }
        env->def(v->name, e_var(v->type, v));
        endmatch
    casematch(AST_VARDECL, VarDecl*, d)
        compute_envs(mod, env, d->ann);
        detect_types(mod, env, d->ann);
        if (!is_type(d->ann)) 
            no_type_in_pattern_error(mod, d->ann), d->ann->type = ERROR;
        detect_types_pattern(mod, env, d->name);
        d->type = unify(d->ann->type, d->name->type);
        if (!d->type) 
            incompatible_pattern_types_error(mod, d->ann->type, d->name), d->type = ERROR;
        endmatch
    casematch(AST_PTRDECL, Binary*, b)
        detect_types_pattern(mod, env, b->left);
        endmatch
    casematch(AST_STAR, Binary*, b)
        compute_envs(mod, env, b->left);
        detect_types(mod, env, b->left);
        if (is_type(b->left)) {
            if (b->right->kind == AST_UNIT) {
                b->kind = AST_PTRDECL;
                slice<AST*> args = { nullptr, (iptr)0 };
                b->left = new(mod->parser->astspace) Apply(b->pos, new(mod->parser->astspace) Unary(AST_DEREF, b->left->pos, b->left), args);
                detect_types_pattern(mod, env, b->left);
                b->type = b->left->type;
            }
            else if (b->right->kind == AST_PAREN) {
                b->kind = AST_PTRDECL;
                slice<AST*> args = { new(mod->parser->astspace) AST*[1], 1 };
                args[0] = b->right;
                b->left = new(mod->parser->astspace) Apply(b->pos, new(mod->parser->astspace) Unary(AST_DEREF, b->left->pos, b->left), args);
                detect_types_pattern(mod, env, b->left);
                b->type = b->left->type;
            }
            else if (b->right->kind == AST_APPLY) {
                b->kind = AST_PTRDECL;
                b->left = new(mod->parser->astspace) FunDecl(
                    b->pos,
                    new(mod->parser->astspace) Unary(AST_DEREF, b->left->pos, b->left),
                    b->right,
                    nullptr
                );
                detect_types_pattern(mod, env, b->left);
            }
            else if (b->right->kind == AST_ARRAY) {
                b->kind = AST_PTRTYPE;
                if (((List*)b->right)->items.n > 1) {
                    multiple_array_dims_error(mod, b->right);
                    b->type = ERROR;
                }
                else {
                    b->left = new(mod->parser->astspace) Binary(
                        AST_INDEX,
                        b->pos,
                        new(mod->parser->astspace) Unary(AST_DEREF, b->left->pos, b->left),
                        ((List*)b->right)->items.n == 0 ? nullptr : ((List*)b->right)->items[0]
                    );
                    detect_types_pattern(mod, env, b->left);
                    b->type = b->left->type;
                }
            }
            else if (b->right->kind == AST_VAR) {
                b->kind = AST_PTRDECL;
                b->left = new(mod->parser->astspace) VarDecl(
                    b->pos,
                    new(mod->parser->astspace) Unary(AST_DEREF, b->left->pos, b->left),
                    b->right,
                    nullptr
                );
                detect_types_pattern(mod, env, b->left);
                b->type = b->left->type;
            }
            else unexpected_typename_error(mod, b->left), b->type = ERROR;
        }
        else no_type_in_pattern_error(mod, b);
        endmatch
    casematch(AST_APPLY, Apply*, a)
        for (AST* ast : a->args) detect_types_pattern(mod, env, ast);

        // print("fn: "), format(stdout, mod, a->fn), print('\n');
        detect_types(mod, env, a->fn);
        if (!is_type(a->fn) && a->fn->type != ERROR) {
            no_type_in_pattern_error(mod, a->fn), a->type = a->fn->type = ERROR;
            return;
        }
        Type* match_type = a->fn->type;
        switch (match_type->kind) {
            case T_NAMED:
                if (a->args.n != 1) {
                    pattern_params_mismatch_error(mod, a, match_type, a->args.n, 1),
                    a->type = ERROR;
                }
                else if (!a->args[0]->type || !unify(((NamedType*)match_type)->inner, a->args[0]->type)) {
                    incompatible_pattern_types_error(mod, ((NamedType*)match_type)->inner, a->args[0]);
                    a->type = ERROR;
                }
                else a->type = match_type;
                break;
            case T_STRUCT:
                if (a->args.n != ((StructType*)match_type)->fields.n) {
                    pattern_params_mismatch_error(mod, a, match_type, a->args.n, ((StructType*)match_type)->fields.n);
                    a->type = ERROR;
                }
                for (u32 i = 0; i < ((StructType*)match_type)->fields.n; i ++) {
                    Type* ft = ((StructType*)match_type)->fields[i].second;
                    AST* f = a->args[i];
                    Type* unified = unify(ft, f->type);
                    if (!f->type || !unified) {
                        incompatible_pattern_types_error(mod, ft, f);
                        a->type = ERROR;
                    }
                }
                if (a->type != ERROR) a->type = match_type;
                break;
            default:
                invalid_destructuring_error(mod, a->fn, match_type);
                a->type = ERROR;
                break;
        }
        endmatch
    casematch(AST_ARRAY, List*, l)
        Type* elt = nullptr;
        for (AST* n : l->items) {
            Type* orig_elt = elt;
            if (n->kind == AST_BITOR) {
                detect_types_pattern(mod, env, ((Binary*)n)->left);
                detect_types_pattern(mod, env, ((Binary*)n)->right);
                elt = elt ? unify(elt, ((Binary*)n)->left->type) : ((Binary*)n)->left->type;
                if (!elt) {
                    incompatible_pattern_types_error(mod, orig_elt, ((Binary*)n)->left);
                    ast->type = ERROR;
                }
            }
            else {
                detect_types_pattern(mod, env, n);
                elt = elt ? unify(elt, n->type) : n->type;
                if (!elt) {
                    incompatible_pattern_types_error(mod, orig_elt, n);
                    ast->type = ERROR;
                }
            }
        }
        Type* slice = mod->typectx->def<SliceType>(elt);
        if (l->items.n && l->items[l->items.n - 1]->kind == AST_BITOR) {
            AST* tail = ((Binary*)l->items[l->items.n - 1])->right;
            Type* orig_slice = slice;
            slice = unify(slice, tail->type);
            if (!slice) {
                incompatible_pattern_types_error(mod, slice, tail);
                tail->type = ERROR;
            }
            else tail->type = slice;
        }
        if (ast->type != ERROR) ast->type = slice;
        endmatch
    default:
        detect_types(mod, env, ast);
        if (!is_type(ast)) no_type_in_pattern_error(mod, ast);
        break;
    }
}

Type* concretify(Module* mod, Type* type) {
    if (!type) return ERROR;
    else return fullconcrete(*mod->typectx, type);
}

void require_no_types(Module* mod, Env* env, Unary* ast) {
    if (is_type(ast->child)) unexpected_typename_error(mod, ast->child);
}

void require_no_types(Module* mod, Env* env, Binary* ast) {
    if (is_type(ast->left)) unexpected_typename_error(mod, ast->left);
    if (is_type(ast->right)) unexpected_typename_error(mod, ast->right);
}

void detect_types(Module* mod, Env* env, AST* ast) {
    switch (ast->kind) {
    casematch(AST_PROGRAM, ASTProgram*, p)
        for (Statement* stmt : p->toplevel) detect_types(mod, p->env, stmt);
        for (AST* ast : p->defers) detect_types(mod, p->deferred, ast);
        mod->envctx->finalize_methods(mod);
        endmatch
    casematch(AST_ICONST, Const*, c) endmatch
    casematch(AST_FCONST, Const*, c) endmatch
    casematch(AST_CHCONST, Const*, c) endmatch
    casematch(AST_STRCONST, Const*, c) endmatch
    casematch(AST_BOOL, Const*, c) endmatch
    casematch(AST_UNIT, Const*, c) endmatch
    casematch(AST_VAR, Var*, v)         // Handle typenames (T).
        Entry* e = env->lookup(v->name);
        if (!e) return;
        else if (e->kind == E_TYPE || e->kind == E_ALIAS || e->kind == E_CASE) {
            Type* t = lookup_type(mod, env, v->name);
            v->kind = AST_TYPENAME;
            v->type = t;
        }
        else if (e->kind == E_MOD) {
            v->kind = AST_MODULENAME;
            v->type = VOID;
        }
        endmatch
    casematch(AST_PLUS, Unary*, u) detect_types(mod, env, u->child); require_no_types(mod, env, u); endmatch
    casematch(AST_NEG, Unary*, u) detect_types(mod, env, u->child); require_no_types(mod, env, u); endmatch
    casematch(AST_DEREF, Unary*, u)     // Handle pointer types (*T)
        detect_types(mod, env, u->child);
        if (is_type(u->child)) {
            u->kind = AST_PTRTYPE;
            u->type = mod->typectx->def<PtrType>(u->child->type);
        }
        endmatch
    casematch(AST_ADDROF, Unary*, u) detect_types(mod, env, u->child); require_no_types(mod, env, u); endmatch
    casematch(AST_NOT, Unary*, u) detect_types(mod, env, u->child); require_no_types(mod, env, u); endmatch
    casematch(AST_BITNOT, Unary*, u) detect_types(mod, env, u->child); require_no_types(mod, env, u); endmatch
    casematch(AST_INCR, Unary*, u) detect_types(mod, env, u->child); require_no_types(mod, env, u); endmatch
    casematch(AST_DECR, Unary*, u) detect_types(mod, env, u->child); require_no_types(mod, env, u); endmatch
    casematch(AST_POSTINCR, Unary*, u) detect_types(mod, env, u->child); require_no_types(mod, env, u); endmatch
    casematch(AST_POSTDECR, Unary*, u) detect_types(mod, env, u->child); require_no_types(mod, env, u); endmatch
    casematch(AST_NEW, Unary*, u) 
        detect_types(mod, env, u->child); 
        require_no_types(mod, env, u);
        endmatch
    casematch(AST_NEWARRAY, Binary*, b) 
        detect_types(mod, env, b->left);
        detect_types(mod, env, b->right); 
        require_no_types(mod, env, b);
        endmatch
    casematch(AST_DEL, Unary*, u) detect_types(mod, env, u->child); require_no_types(mod, env, u); endmatch
    casematch(AST_PAREN, Unary*, u) detect_types(mod, env, u->child); endmatch
    casematch(AST_SIZEOF, Sizeof*, u) 
        detect_types(mod, env, u->child); 
        if (!u->fold) u->fold = tryfold(mod, env, u);
        endmatch

    casematch(AST_ADD, Binary*, b) detect_types(mod, env, b->left); detect_types(mod, env, b->right); require_no_types(mod, env, b);endmatch
    casematch(AST_SUB, Binary*, b) detect_types(mod, env, b->left); detect_types(mod, env, b->right); require_no_types(mod, env, b);endmatch
    casematch(AST_STAR, Binary*, b) 
        detect_types(mod, env, b->left); 
        if (is_type(b->left)) {
            if (b->right->kind == AST_UNIT) {
                b->kind = AST_PTRDECL;
                slice<AST*> args = { nullptr, (iptr)0 };
                b->left = new(mod->parser->astspace) Apply(b->pos, new(mod->parser->astspace) Unary(AST_DEREF, b->left->pos, b->left), args);
                compute_envs(mod, env, b->left);
                detect_types(mod, env, b->left);
                b->type = b->left->type;
            }
            else if (b->right->kind == AST_PAREN) {
                b->kind = AST_PTRDECL;
                slice<AST*> args = { new(mod->parser->astspace) AST*[1], 1 };
                args[0] = b->right;
                b->left = new(mod->parser->astspace) Apply(b->pos, new(mod->parser->astspace) Unary(AST_DEREF, b->left->pos, b->left), args);
                compute_envs(mod, env, b->left);
                detect_types(mod, env, b->left);
                b->type = b->left->type;
            }
            else if (b->right->kind == AST_APPLY) {
                b->kind = AST_PTRDECL;
                b->left = new(mod->parser->astspace) FunDecl(
                    b->pos,
                    new(mod->parser->astspace) Unary(AST_DEREF, b->left->pos, b->left),
                    b->right,
                    nullptr
                );
                compute_envs(mod, env, b->left);
                detect_types(mod, env, b->left);
            }
            else if (b->right->kind == AST_ARRAY) {
                b->kind = AST_PTRTYPE;
                if (((List*)b->right)->items.n > 1) {
                    multiple_array_dims_error(mod, b->right);
                    b->type = ERROR;
                }
                else {
                    b->left = new(mod->parser->astspace) Binary(
                        AST_INDEX,
                        b->pos,
                        new(mod->parser->astspace) Unary(AST_DEREF, b->left->pos, b->left),
                        ((List*)b->right)->items.n == 0 ? nullptr : ((List*)b->right)->items[0]
                    );
                    compute_envs(mod, env, b->left);
                    detect_types(mod, env, b->left);
                    b->type = b->left->type;
                }
            }
            else if (b->right->kind == AST_VAR) {
                b->kind = AST_PTRDECL;
                b->left = new(mod->parser->astspace) VarDecl(
                    b->pos,
                    new(mod->parser->astspace) Unary(AST_DEREF, b->left->pos, b->left),
                    b->right,
                    nullptr
                );
                compute_envs(mod, env, b->left);
                detect_types(mod, env, b->left);
                b->type = b->left->type;
            }
            else unexpected_typename_error(mod, b->left), b->type = ERROR;
        } 
        else detect_types(mod, env, b->right);
        endmatch
    casematch(AST_DIV, Binary*, b) detect_types(mod, env, b->left); detect_types(mod, env, b->right); require_no_types(mod, env, b);endmatch
    casematch(AST_MOD, Binary*, b) detect_types(mod, env, b->left); detect_types(mod, env, b->right); require_no_types(mod, env, b);endmatch
    casematch(AST_EXP, Binary*, b) detect_types(mod, env, b->left); detect_types(mod, env, b->right); require_no_types(mod, env, b);endmatch
    casematch(AST_BITAND, Binary*, b) detect_types(mod, env, b->left); detect_types(mod, env, b->right); require_no_types(mod, env, b);endmatch
    casematch(AST_BITOR, Binary*, b) detect_types(mod, env, b->left); detect_types(mod, env, b->right); require_no_types(mod, env, b);endmatch
    casematch(AST_BITXOR, Binary*, b) detect_types(mod, env, b->left); detect_types(mod, env, b->right); require_no_types(mod, env, b);endmatch
    casematch(AST_BITLEFT, Binary*, b) detect_types(mod, env, b->left); detect_types(mod, env, b->right); require_no_types(mod, env, b);endmatch
    casematch(AST_BITRIGHT, Binary*, b) detect_types(mod, env, b->left); detect_types(mod, env, b->right); require_no_types(mod, env, b);endmatch
    casematch(AST_LESS, Binary*, b) detect_types(mod, env, b->left); detect_types(mod, env, b->right); require_no_types(mod, env, b);endmatch
    casematch(AST_LEQUAL, Binary*, b) detect_types(mod, env, b->left); detect_types(mod, env, b->right); require_no_types(mod, env, b);endmatch
    casematch(AST_GREATER, Binary*, b) detect_types(mod, env, b->left); detect_types(mod, env, b->right); require_no_types(mod, env, b);endmatch
    casematch(AST_GEQUAL, Binary*, b) detect_types(mod, env, b->left); detect_types(mod, env, b->right); require_no_types(mod, env, b);endmatch
    casematch(AST_EQUAL, Binary*, b) detect_types(mod, env, b->left); detect_types(mod, env, b->right); require_no_types(mod, env, b);endmatch
    casematch(AST_INEQUAL, Binary*, b) detect_types(mod, env, b->left); detect_types(mod, env, b->right); require_no_types(mod, env, b);endmatch
    casematch(AST_AND, Binary*, b) detect_types(mod, env, b->left); detect_types(mod, env, b->right); require_no_types(mod, env, b);endmatch
    casematch(AST_OR, Binary*, b) detect_types(mod, env, b->left); detect_types(mod, env, b->right); require_no_types(mod, env, b);endmatch
    casematch(AST_DOT, Binary*, b) 
        detect_types(mod, env, b->left);
        Env* nenv = ast_env(env, b->left);
        if (nenv) {
            detect_types(mod, nenv, b->right);
            if (is_type(b->right)) b->kind = AST_TYPEDOT, b->type = b->right->type;
        }
        else if (b->left->type == ERROR) b->type = ERROR;
        endmatch
    casematch(AST_ASSIGN, Binary*, b) detect_types(mod, env, b->left); detect_types(mod, env, b->right); require_no_types(mod, env, b);endmatch
    casematch(AST_ADDEQ, Binary*, b) detect_types(mod, env, b->left); detect_types(mod, env, b->right); require_no_types(mod, env, b);endmatch
    casematch(AST_SUBEQ, Binary*, b) detect_types(mod, env, b->left); detect_types(mod, env, b->right); require_no_types(mod, env, b);endmatch
    casematch(AST_STAREQ, Binary*, b) detect_types(mod, env, b->left); detect_types(mod, env, b->right); require_no_types(mod, env, b);endmatch
    casematch(AST_DIVEQ, Binary*, b) detect_types(mod, env, b->left); detect_types(mod, env, b->right); require_no_types(mod, env, b);endmatch
    casematch(AST_MODEQ, Binary*, b) detect_types(mod, env, b->left); detect_types(mod, env, b->right); require_no_types(mod, env, b);endmatch
    casematch(AST_EXPEQ, Binary*, b) detect_types(mod, env, b->left); detect_types(mod, env, b->right); require_no_types(mod, env, b);endmatch
    casematch(AST_BITANDEQ, Binary*, b) detect_types(mod, env, b->left); detect_types(mod, env, b->right); require_no_types(mod, env, b);endmatch
    casematch(AST_BITOREQ, Binary*, b) detect_types(mod, env, b->left); detect_types(mod, env, b->right); require_no_types(mod, env, b);endmatch
    casematch(AST_BITXOREQ, Binary*, b) detect_types(mod, env, b->left); detect_types(mod, env, b->right); require_no_types(mod, env, b);endmatch
    casematch(AST_BITLEFTEQ, Binary*, b) detect_types(mod, env, b->left); detect_types(mod, env, b->right); require_no_types(mod, env, b);endmatch
    casematch(AST_BITRIGHTEQ, Binary*, b) detect_types(mod, env, b->left); detect_types(mod, env, b->right); require_no_types(mod, env, b);endmatch
    casematch(AST_IS, Is*, b) 
        detect_types(mod, env, b->left);
        detect_types_pattern(mod, b->env, b->right); 
        endmatch

    casematch(AST_ARRAYTYPE, Binary*, b)
        if (b->type) return;
        detect_types(mod, env, b->left);
        if (is_type(b->left)) {
            if (!b->right) non_const_array_dim_error(mod, b->right), b->type = ERROR;
            else {
                detect_types(mod, env, b->right);
                Fold f = tryfold(mod, env, b->right);
                if (f.kind != Fold::ICONST) non_const_array_dim_error(mod, b->right), b->type = ERROR;
                else b->type = mod->typectx->def<ArrayType>(b->left->type, f.iconst);
            }
        }
        else expected_array_type_error(mod, b->left), b->type = ERROR;
        endmatch
    casematch(AST_SLICETYPE, Binary*, b)
        if (b->type) return;
        detect_types(mod, env, b->left);
        if (is_type(b->left)) {
            b->type = mod->typectx->def<SliceType>(b->left->type);
        }
        else expected_slice_type_error(mod, b->left), b->type = ERROR;
        endmatch
    casematch(AST_PTRTYPE, Unary*, u)
        if (u->type) return;
        detect_types(mod, env, u->child);
        if (is_type(u->child)) {
            u->type = mod->typectx->def<PtrType>(u->child->type);
        }
        else expected_ptr_type_error(mod, u->child), u->type = ERROR;
        endmatch
    casematch(AST_FUNTYPE, Apply*, a)
        if (a->type) return;
        detect_types(mod, env, a->fn);
        if (is_type(a->fn)) {
            slice<Type*> argts = { new(mod->typectx->typespace) Type*[a->args.n], a->args.n };
            for (u32 i = 0; i < a->args.n; i ++) detect_types(mod, env, a->args[i]), argts[i] = a->args[i]->type;
            a->type = mod->typectx->def<FunType>(argts, a->fn->type);
        }
        else expected_fun_type_error(mod, a->fn), a->type = ERROR;
        endmatch
    casematch(AST_TYPENAME, Var*, v)
        if (v->type) return;
        auto e = env->lookup(v->name);
        if (!e) undefined_typename_error(mod, v), v->type = ERROR;
        else if (e->kind != E_TYPE && e->kind != E_CASE && e->kind != E_ALIAS) 
            expected_typename_error(mod, v), v->type = ERROR;
        else v->type = e->type;
        endmatch

    casematch(AST_INDEX, Binary*, b)    // Handle array types (T[N]) and slice types (T[])
        detect_types(mod, env, b->left);
        if (is_type(b->left)) {
            if (b->right) {
                detect_types(mod, env, b->right);
                b->kind = AST_ARRAYTYPE;
                Fold f = tryfold(mod, env, b->right);
                if (f.kind != Fold::ICONST) {
                    non_const_array_dim_error(mod, b->right);
                    b->type = ERROR;
                }
                else {
                    b->type = mod->typectx->def<ArrayType>(b->left->type, f.iconst);
                }
            }
            else {
                b->kind = AST_SLICETYPE;
                b->type = mod->typectx->def<SliceType>(b->left->type);
            }
        }
        endmatch
    casematch(AST_SLICE, Slice*, s)    // Handle array types (T[N]) and slice types (T[])
        detect_types(mod, env, s->array);
        if (is_type(s->array)) typename_in_slice_error(mod, s->array);
        endmatch
    casematch(AST_APPLY, Apply*, a)
        for (AST* ast : a->args) detect_types(mod, env, ast);
        detect_types(mod, env, a->fn);
        if (is_type(a->fn)) {
            ASTKind newast = AST_APPLY;
            vec<Type*, 256, arena> types;
            types.alloc = &mod->typectx->typespace;
            if (a->args.n == 0) {
                newast = AST_CTOR;
                slice<AST*> newargs = { new(mod->parser->astspace) AST*[1], 1 };
                newargs[0] = new(mod->parser->astspace) Const(a->pos);
                a->args = newargs;
            }
            else for (AST* ast : a->args) {
                if (is_type(ast)) {
                    if (newast == AST_APPLY || newast == AST_FUNTYPE) newast = AST_FUNTYPE, types.push(ast->type);
                    else if (newast == AST_CTOR) {
                        typename_in_ctor_error(mod, a->fn->type, ast);
                        break;
                    }
                }
                else {
                    if (newast == AST_APPLY) newast = AST_CTOR;
                    else if (newast == AST_FUNTYPE) {
                        val_in_funtype_error(mod, ast);
                        a->kind = AST_FUNTYPE;
                        a->type = ERROR;
                        return;
                    }
                }
            }
            a->kind = newast;
            if (a->kind == AST_FUNTYPE) {
                if (types.size() == 0) types.push(UNIT);
                Type** args = (Type**)mod->typectx->typespace.alloc(sizeof(Type*) * types.size());
                mcpy(args, types.data, sizeof(Type*) * types.size());
                a->type = mod->typectx->def<FunType>(slice<Type*>{ args, types.size() }, a->fn->type);
            }
        }
        else {
            Env* tenv = env;
            if (a->fn->kind == AST_DOT) {
                if (is_type(((Binary*)a->fn)->left)) tenv = type_env(((Binary*)a->fn)->left->type);
                else {
                    slice<AST*> newargs = { new(mod->parser->astspace) AST*[a->args.n + 1], a->args.n + 1 };
                    newargs[0] = ((Binary*)a->fn)->left;
                    for (i32 i = 0; i < a->args.n; i ++) newargs[i + 1] = a->args[i];
                    a->fn = ((Binary*)a->fn)->right;
                    a->args = newargs;
                    return detect_types(mod, env, a); // Re-check types now that the function is just a name.
                }
            }
            else if (a->fn->kind == AST_VAR) {
                Entry* e = env->lookup(((Var*)a->fn)->name);
                if (e && e->kind == E_GENTYPE) {
                    ASTKind newast = AST_APPLY;
                    TypeDecl* d = (TypeDecl*)e->ast;
                    if (a->args.n == 0) {
                        newast = AST_GENCTOR;
                        slice<AST*> newargs = { new(mod->parser->astspace) AST*[1], 1 };
                        newargs[0] = new(mod->parser->astspace) Const(a->pos);
                        a->args = newargs;
                    }
                    else for (AST* ast : a->args) {
                        if (is_type(ast)) {
                            if (newast == AST_APPLY || newast == AST_TYPEINST) newast = AST_TYPEINST;
                            else if (newast == AST_GENCTOR) {
                                typename_in_genctor_error(mod, a->fn, ast);
                                break;
                            }
                        }
                        else {
                            if (newast == AST_APPLY) newast = AST_GENCTOR;
                            else if (newast == AST_TYPEINST) {
                                val_in_gentype_error(mod, ast);
                                a->kind = AST_TYPEINST;
                                a->type = ERROR;
                                return;
                            }
                        }
                    }
                    a->kind = newast;
                    if (a->kind == AST_TYPEINST) {
                        if (a->args.n != ((Apply*)d->name)->args.n) {
                            gentype_params_mismatch_error(mod, a, d, a->args.n, ((Apply*)d->name)->args.n);
                            a->type = ERROR;
                            return;
                        }
                        for (iptr i = 0; i < a->args.n; i ++) if (!is_type(a->args[i])) { 
                            nontype_gentype_param_error(mod, a->args[i], d);
                            a->type = ERROR;
                            return;
                        }
                        TypeDecl* inst = inst_type(mod, env, d, a->args);
                        if (inst) a->type = inst->type;
                        else a->type = ERROR;
                    }
                }
            }
        }
        endmatch
    casematch(AST_ARRAY, List*, l) 
        for (AST* ast : l->items) detect_types(mod, env, ast);
        endmatch
    casematch(AST_SET, List*, l) 
        for (AST* ast : l->items) detect_types(mod, env, ast);
        endmatch
    casematch(AST_CONSTDECL, ConstDecl*, d)
        detect_types(mod, env, d->init);
        d->fold = tryfold(mod, env, d->init);
        if (!d->fold) non_const_in_decl_error(mod, d->init), d->type = ERROR;
        else d->type = d->fold.type();
        Entry* e = env->lookup(d->basename);
        if (e) e->type = d->type;
        else unreachable("Somehow didn't find definition.");
        endmatch
    casematch(AST_VARDECL, VarDecl*, d)
        Type* deftype;
        if (d->ann) {
            detect_types(mod, env, d->ann);
            if (!is_type(d->ann)) {
                if (d->ann->type != ERROR) nontype_annotation_error(mod, d->ann);
                deftype = ERROR;
            }
            else deftype = d->ann->type;
        }
        else deftype = mod->typectx->defvar(mod, env);

        if (d->init) detect_types(mod, env, d->init);
        Entry* e = env->lookup(d->basename);
        d->type = deftype;
        if (e) e->type = deftype;
        else unreachable("Somehow didn't find definition.");
        endmatch
    casematch(AST_FUNDECL, FunDecl*, d) 
        Type* rettype;
        if (d->returned) {
            detect_types(mod, env, d->returned);
            if (!is_type(d->returned)) {
                if (d->returned->type != ERROR) nontype_returntype_error(mod, d->returned);
                rettype = ERROR;
            }
            else rettype = d->returned->type;
        }
        else rettype = mod->typectx->defvar(mod, env);

        AST* fn = ((Apply*)d->proto)->fn;
        slice<AST*> args = ((Apply*)d->proto)->args; 
        detect_types(mod, env, fn);

        Env* tenv = env;
        if (is_type(fn)) {
            unexpected_type_in_fundecl_error(mod, d->proto);
            tenv = nullptr;
            return;
        }
        else if ((fn->kind == AST_DOT && is_type(((Binary*)fn)->left)) || env->kind == ENV_TYPE) {
            AST* basename = fn;
            if (fn->kind == AST_DOT && is_type(((Binary*)fn)->left)) {
                d->method_type = ((Binary*)fn)->left->type;
                tenv = type_env(d->method_type);
                basename = ((Binary*)fn)->right;
            }
            else {
                d->method_type = compute_env_type(mod, env);
                if (!d->method_type) tenv = nullptr;
                else tenv = type_env(d->method_type);
            }

            if (tenv && !d->isinst) {
                VarDecl* thisdecl = new(mod->parser->astspace) VarDecl(
                    fn->pos, 
                    new(mod->parser->astspace) Var(fn->pos, tenv->name),
                    new(mod->parser->astspace) Var(fn->pos, mod->interner->intern("this")),
                    nullptr
                );
                thisdecl->ann->kind = AST_TYPENAME, thisdecl->ann->type = d->method_type;
                compute_envs(mod, d->env, thisdecl);
                slice<AST*> newargs = { new(mod->parser->astspace) AST*[args.n + 1], args.n + 1 };
                newargs[0] = thisdecl;
                for (i32 i = 0; i < args.n; i ++) newargs[i + 1] = args[i];
                ((Apply*)d->proto)->args = args = newargs;
                ((Apply*)d->proto)->fn = fn = basename;
            }
        }

        for (i32 i = 0; i < args.n; i ++) {
            AST* ast = args[i];
            if (ast->kind == AST_VAR) {
                if (!d->isinst) d->generic = true;
                args[i] = ast = new(mod->parser->astspace) VarDecl(
                    ast->pos, nullptr, ast, nullptr
                );
                compute_envs(mod, d->env, ast);
            }
            else if (ast->kind == AST_VARDECL) {
                AST* ann = ((VarDecl*)ast)->ann;
                if (!ann) {
                    if (!d->isinst) d->generic = true;
                }
                else {
                    detect_types(mod, d->env, ann);
                    if (!is_type(ann)) nontype_annotation_error(mod, ann);
                    else if (!isconcrete(ann->type) && ann->type != d->method_type && !d->isinst) 
                        d->generic = true;
                }
            }
            detect_types(mod, d->env, ast);
        }

        if (d->method_type && tenv) {
            d->env->siblings.push(d->env->parent), d->env->parent = tenv;
            tenv->def(d->basename, e_fun(nullptr, d));
            mod->envctx->add_method(d->basename, d->method_type, tenv);
        }
        else env->def(d->basename, e_fun(nullptr, d));

        if (d->generic) {
            Entry* e = d->env->parent->lookup(d->basename);
            if (e) e->kind = E_GENFUN;
            else unreachable("Somehow didn't find definition.");
            return;
        }

        vec<Type*, 256, arena> argtypes;
        argtypes.alloc = &mod->parser->astspace;

        for (AST* ast : args) {
            if (ast->kind == AST_VARDECL || ast->kind == AST_PTRDECL) argtypes.push(ast->type);
        }
        if (argtypes.size() == 0) argtypes.push(UNIT);

        Type** argslice = (Type**)mod->parser->astspace.alloc(sizeof(Type*) * argtypes.size());
        for (i32 i = 0; i < argtypes.size(); i ++) argslice[i] = argtypes[i];
        d->type = mod->typectx->def<FunType>(slice<Type*>{ argslice, argtypes.size() }, rettype);

        d->env->parent->set(d->basename, e_fun(d->type, d));
        if (d->body) detect_types(mod, d->env, d->body);
        endmatch
    casematch(AST_ARGS, List*, d) 
        for (AST* ast : d->items) detect_types(mod, env, ast);
        endmatch
    casematch(AST_DO, List*, d) 
        for (AST* ast : d->items) detect_types(mod, env, ast);
        endmatch
    casematch(AST_IF, If*, i) 
        detect_types(mod, i->env, i->cond);
        detect_types(mod, i->env, i->ifTrue);
        if (i->ifFalse) detect_types(mod, i->env, i->ifFalse);
        endmatch
    casematch(AST_WHILE, Loop*, l) 
        detect_types(mod, l->env, l->cond);
        detect_types(mod, l->env, l->body);
        endmatch
    casematch(AST_UNTIL, Loop*, l) 
        detect_types(mod, l->env, l->cond);
        detect_types(mod, l->env, l->body);
        endmatch
    casematch(AST_FOR, For*, f) 
        detect_types(mod, f->env, f->body);
        endmatch
    casematch(AST_DEFER, Unary*, u) 
        endmatch
    casematch(AST_RETURN, Unary*, u) 
        if (u->child) detect_types(mod, env, u->child);
        endmatch
    casematch(AST_BREAK, Expr*, e) 
        endmatch
    casematch(AST_CONTINUE, Expr*, e) 
        endmatch
    casematch(AST_WITH, With*, w) 
        // We skip working with the body of the with until typechecking, since we need to know the 
        // type of the bound value in order to determine the correct sibling namespace.
        endmatch
    casematch(AST_USE, Binary*, b) 
        endmatch
    casematch(AST_ALIASDECL, AliasDecl*, d) 
        lookup_type(mod, env, d->basename);
        endmatch
    casematch(AST_TYPEDECL, TypeDecl*, d) 
        if (d->generic) for (const auto& e : d->insts) {
            if (e.value != d->prototype) detect_types(mod, env, e.value);
        }
        else lookup_type(mod, env, d->basename);
        endmatch
    casematch(AST_CASEDECL, CaseDecl*, d) 
        if (d->env->kind == ENV_TYPE) lookup_type(mod, env, d->basename);
        else {
            detect_types_pattern(mod, d->env, d->pattern);
            if (d->body) compute_envs(mod, d->env, d->body);
            detect_types(mod, d->env, d->body);
        }
        endmatch
    casematch(AST_MATCH, Binary*, b) 
        detect_types(mod, env, b->left);
        endmatch
    casematch(AST_MODULEDECL, ModuleDecl*, b) 
        detect_types(mod, b->env, b->body);
        endmatch
    default:
        break;
    }
}

void unify(Fold& l, Fold& r) {
    if (l.kind == r.kind) return;
    else if (l.kind == Fold::FCONST && r.kind == Fold::ICONST) r = Fold::from_float(r.iconst);
    else if (l.kind == Fold::ICONST && r.kind == Fold::FCONST) l = Fold::from_float(r.fconst);
    else l = r = Fold();
}

Fold tryfold_math(Module* mod, Env* env, Binary* b, i64(*ifunc)(i64, i64), double(*ffunc)(double, double)) {
    Fold l = tryfold(mod, env, b->left), r = tryfold(mod, env, b->right);
    unify(l, r);
    if (l.kind == Fold::ICONST) return Fold::from_int(ifunc(l.iconst, r.iconst));
    else if (l.kind == Fold::FCONST) return Fold::from_float(ffunc(l.fconst, r.fconst));
    else return Fold();
}

Fold tryfold_bitwise(Module* mod, Env* env, Binary* b, i64(*func)(i64, i64)) {
    Fold l = tryfold(mod, env, b->left), r = tryfold(mod, env, b->right);
    unify(l, r);
    if (l.kind == Fold::ICONST) return Fold::from_int(func(l.iconst, r.iconst));
    else return Fold();
}

Fold tryfold_logic(Module* mod, Env* env, Binary* b, bool(*func)(bool, bool)) {
    Fold l = tryfold(mod, env, b->left), r = tryfold(mod, env, b->right);
    unify(l, r);
    if (l.kind == Fold::BOOL) return Fold::from_bool(func(l.bconst, r.bconst));
    else return Fold();
}

Fold tryfold_compare(Module* mod, Env* env, Binary* b, bool(*ifunc)(i64, i64), bool(*ffunc)(double, double)) {
    Fold l = tryfold(mod, env, b->left), r = tryfold(mod, env, b->right);
    unify(l, r);
    if (l.kind == Fold::ICONST) return Fold::from_bool(ifunc(l.iconst, r.iconst));
    else if (l.kind == Fold::FCONST) return Fold::from_bool(ffunc(l.fconst, r.fconst));
    else return Fold();
}

#define operator_func(T, x) [](T a, T b) -> decltype(a x b) { return a x b; }

i64 size_of(Module* mod, AST* ast, Type* t);

i64 size_of_aggregate(Module* mod, AST* ast, i64 size) {
    return size;
}

template<typename... Args>
i64 size_of_aggregate(Module* mod, AST* ast, i64 size_so_far, Type* a, Args... args) {
    i64 asize = size_of(mod, ast, a);
    i64 align = asize > 8 ? 8 : asize;
    while (size_so_far & align - 1) size_so_far ++;
    return size_of_aggregate(mod, ast, size_so_far, args...);
}

i64 size_of(Module* mod, AST* ast, Type* t) {
    // This whole function is a bit flawed, since it can break portability of generated code. What we
    // really want is to propagate sizeof(type) calls through folded expressions and generate a constant
    // that embeds the sizeof() expressions in a way the backend is aware of. For now, we rely on the
    // type sizes in generated code being the same as in this compiler. In the future this should be
    // reworked to support cross compilation better.
    if (t->kind == T_ERROR) return 1;
    Type* orig = t;
    t = concretify(mod, t);
    if (t == ERROR) size_of_non_concrete_error(mod, ast, orig);
    switch (t->kind) {
    case T_VOID: return 0;
    case T_BOOL: return 1;
    case T_UNIT: return 1;
    case T_STRING: return sizeof(slice<i8>);
    case T_CHAR: return sizeof(rune);
    case T_PTR: return sizeof(iptr);
    case T_FUN: return sizeof(iptr);
    case T_SLICE: return sizeof(slice<i8>);
    case T_ARRAY: return ((ArrayType*)t)->size * size_of(mod, ast, ((ArrayType*)t)->element);
    case T_NUMERIC: return ((NumericType*)t)->bytes;
    case T_NAMED: 
        if (t->is_case) return size_of_aggregate(mod, ast, 0, I32, ((NamedType*)t)->inner);
        else return size_of(mod, ast, ((NamedType*)t)->inner);
    case T_UNION: {
        for (const auto& p : ((UnionType*)t)->fields) p.second->is_case = true;
        i64 maxsize = size_of(mod, ast, ((UnionType*)t)->fields[0].second), maxidx = 0;
        for (i64 i = 1; i < ((UnionType*)t)->fields.size(); i ++) {
            i64 isize = size_of(mod, ast, ((UnionType*)t)->fields[i].second);
            if (isize > maxsize) maxsize = isize, maxidx = i;
        }
        Type* maxtype = ((UnionType*)t)->fields[maxidx].second;
        return t->is_case ? size_of_aggregate(mod, ast, 0, I32, maxtype) : maxsize;
    }
    case T_STRUCT: {
        i64 size_so_far = t->is_case ? sizeof(i32) : 0;
        for (const auto& p : ((StructType*)t)->fields) size_so_far = size_of_aggregate(mod, ast, size_so_far, p.second);
        return size_of_aggregate(mod, ast, size_so_far, I64) - sizeof(i64); // Round up to 8-byte alignment.
    }
    default:
        return 0;
    }
}

Fold tryfold(Module* mod, Env* env, AST* ast) {
    switch (ast->kind) {
    casematch(AST_ICONST, Const*, c) return Fold::from_int(c->iconst); endmatch
    casematch(AST_FCONST, Const*, c) return Fold::from_float(c->fconst); endmatch
    casematch(AST_BOOL, Const*, c) return Fold::from_bool(c->bconst); endmatch
    casematch(AST_CHCONST, Const*, c) return Fold::from_char(c->chconst); endmatch
    casematch(AST_UNIT, Const*, c) return Fold(); endmatch
    casematch(AST_VAR, Var*, v)
        Entry* e = env->lookup(v->name);
        if (e && e->kind == E_CONST) return ((ConstDecl*)e->ast)->fold;
        else return Fold();
        endmatch
    casematch(AST_PAREN, Unary*, u) return tryfold(mod, env, u->child); endmatch
    casematch(AST_PLUS, Unary*, u)
        Fold c = tryfold(mod, env, u->child);
        if (c.kind == Fold::ICONST) return Fold::from_int(c.iconst);
        else if (c.kind == Fold::FCONST) return Fold::from_float(c.fconst);
        else return Fold();
        endmatch
    casematch(AST_NEG, Unary*, u)
        Fold c = tryfold(mod, env, u->child);
        if (c.kind == Fold::ICONST) return Fold::from_int(-c.iconst);
        else if (c.kind == Fold::FCONST) return Fold::from_float(-c.fconst);
        else return Fold();
        endmatch
    casematch(AST_BITNOT, Unary*, u)
        Fold c = tryfold(mod, env, u->child);
        if (c.kind == Fold::ICONST) return Fold::from_int(~c.iconst);
        else return Fold();
        endmatch
    casematch(AST_NOT, Unary*, u)
        Fold c = tryfold(mod, env, u->child);
        if (c.kind == Fold::BOOL) return Fold::from_bool(!c.bconst);
        else return Fold();
        endmatch
    casematch(AST_SIZEOF, Sizeof*, u)
        if (is_type(u->child)) return u->fold = Fold::from_int(size_of(mod, u->child, u->child->type));
        else if (u->child->type && u->child->type->kind == T_ARRAY) return u->fold = Fold::from_int(size_of(mod, u->child, ((ArrayType*)u->child->type)->element) * ((ArrayType*)u->child->type)->size);
        else return Fold();
        endmatch
    casematch(AST_BITAND, Binary*, b) return tryfold_bitwise(mod, env, b, operator_func(i64, &)); endmatch
    casematch(AST_BITXOR, Binary*, b) return tryfold_bitwise(mod, env, b, operator_func(i64, ^)); endmatch
    casematch(AST_BITOR, Binary*, b) return tryfold_bitwise(mod, env, b, operator_func(i64, |)); endmatch
    casematch(AST_AND, Binary*, b) return tryfold_logic(mod, env, b, operator_func(bool, &&)); endmatch
    casematch(AST_OR, Binary*, b) return tryfold_logic(mod, env, b, operator_func(bool, ||)); endmatch
    casematch(AST_ADD, Binary*, b)
        return tryfold_math(mod, env, b, operator_func(i64, +), operator_func(double, +));
        endmatch;
    casematch(AST_SUB, Binary*, b)
        return tryfold_math(mod, env, b, operator_func(i64, -), operator_func(double, -));
        endmatch;
    casematch(AST_STAR, Binary*, b)
        return tryfold_math(mod, env, b, operator_func(i64, *), operator_func(double, *));
        endmatch;
    casematch(AST_DIV, Binary*, b)
        return tryfold_math(mod, env, b, operator_func(i64, /), operator_func(double, /));
        endmatch;
    casematch(AST_MOD, Binary*, b)
        return tryfold_math(mod, env, b, operator_func(i64, %), [](double a, double b) { return frem(a, b); });
        endmatch;
    casematch(AST_LESS, Binary*, b)
        return tryfold_compare(mod, env, b, operator_func(i64, <), operator_func(double, <));
        endmatch;
    casematch(AST_LEQUAL, Binary*, b)
        return tryfold_compare(mod, env, b, operator_func(i64, <=), operator_func(double, <=));
        endmatch;
    casematch(AST_GREATER, Binary*, b)
        return tryfold_compare(mod, env, b, operator_func(i64, >), operator_func(double, >));
        endmatch;
    casematch(AST_GEQUAL, Binary*, b)
        return tryfold_compare(mod, env, b, operator_func(i64, >=), operator_func(double, >=));
        endmatch;
    casematch(AST_EQUAL, Binary*, b)
        Fold l = tryfold(mod, env, b->left), r = tryfold(mod, env, b->right);
        unify(l, r);
        if (l.kind == Fold::ICONST) return Fold::from_bool(l.iconst == r.iconst);
        else if (l.kind == Fold::FCONST) return Fold::from_bool(l.fconst == r.fconst);
        else if (l.kind == Fold::BOOL) return Fold::from_bool(l.bconst == r.bconst);
        else if (l.kind == Fold::CHCONST) return Fold::from_bool(l.chconst == r.chconst);
        else return Fold();
        endmatch
    casematch(AST_INEQUAL, Binary*, b)
        Fold l = tryfold(mod, env, b->left), r = tryfold(mod, env, b->right);
        unify(l, r);
        if (l.kind == Fold::ICONST) return Fold::from_bool(l.iconst != r.iconst);
        else if (l.kind == Fold::FCONST) return Fold::from_bool(l.fconst != r.fconst);
        else if (l.kind == Fold::BOOL) return Fold::from_bool(l.bconst != r.bconst);
        else if (l.kind == Fold::CHCONST) return Fold::from_bool(l.chconst != r.chconst);
        else return Fold();
        endmatch
    default:
        return Fold();
    }
}

void infer_arithmetic(Module* mod, Env* env, Unary* ast) {
    infer(mod, env, ast->child);
    unify(ast->child->type, ANY_NUMERIC);
    ast->type = ast->child->type;
}

void infer_arithmetic(Module* mod, Env* env, Binary* ast) {
    infer(mod, env, ast->left);
    infer(mod, env, ast->right);
    Type* result = unify(ast->left->type, ast->right->type);
    unify(result, ANY_NUMERIC);
    ast->type = result ? result : mod->typectx->defvar(mod, env);
}

void infer_bitwise(Module* mod, Env* env, Unary* ast) {
    infer(mod, env, ast->child);
    unify(ast->child->type, ANY_NUMERIC);
    ast->type = ast->child->type;
}

void infer_bitwise(Module* mod, Env* env, Binary* ast) {
    infer(mod, env, ast->left);
    infer(mod, env, ast->right);
    Type* result = unify(ast->left->type, ast->right->type);
    unify(result, ANY_NUMERIC);
    ast->type = result ? result : mod->typectx->defvar(mod, env);
}

void infer_logic(Module* mod, Env* env, Unary* ast) {
    infer(mod, env, ast->child);
    unify(ast->child->type, BOOL);
    ast->type = BOOL;
}

void infer_logic(Module* mod, Env* env, Binary* ast) {
    infer(mod, env, ast->left);
    infer(mod, env, ast->right);
    unify(ast->left->type, ast->right->type);
    ast->type = BOOL;
}

void infer_compare(Module* mod, Env* env, Unary* ast) {
    infer(mod, env, ast->child);
    // Error checking here is hard to do with unification, so we handle it in later typechecking.
    ast->type = BOOL;
}

void infer_compare(Module* mod, Env* env, Binary* ast) {
    infer(mod, env, ast->left);
    infer(mod, env, ast->right);
    unify(ast->left->type, ast->right->type);
    ast->type = BOOL;
}

void infer_equality(Module* mod, Env* env, Unary* ast) {
    infer(mod, env, ast->child);
    // Error checking here is hard to do with unification, so we handle it in later typechecking.
    ast->type = BOOL;
}

void infer_equality(Module* mod, Env* env, Binary* ast) {
    infer(mod, env, ast->left);
    infer(mod, env, ast->right);
    unify(ast->left->type, ast->right->type);
    ast->type = BOOL;
}

void infer_assign(Module* mod, Env* env, Binary* ast) {
    infer(mod, env, ast->left);
    infer(mod, env, ast->right);
    is_subtype(ast->right->type, ast->left->type);
    ast->type = VOID;
}

void infer_arith_assign(Module* mod, Env* env, Binary* ast) {
    infer(mod, env, ast->left);
    infer(mod, env, ast->right);
    unify(ast->left->type, ANY_NUMERIC);
    is_subtype(ast->right->type, ast->left->type);
    ast->type = VOID;
}

void infer_bitwise_assign(Module* mod, Env* env, Binary* ast) {
    infer(mod, env, ast->left);
    infer(mod, env, ast->right);
    unify(ast->left->type, ANY_NUMERIC);
    is_subtype(ast->right->type, ast->left->type);
    ast->type = VOID;
}

void infer(Module* mod, Env* env, AST* ast) {
    switch (ast->kind) {
    casematch(AST_PROGRAM, ASTProgram*, p)
        for (Statement* stmt : p->toplevel) infer(mod, p->env, stmt);
        for (AST* ast : p->defers) infer(mod, p->deferred, ast);
        endmatch
    casematch(AST_ICONST, Const*, c) c->type = ICONST; endmatch
    casematch(AST_FCONST, Const*, c) c->type = FCONST; endmatch
    casematch(AST_CHCONST, Const*, c) c->type = CHAR; endmatch
    casematch(AST_STRCONST, Const*, c) c->type = STRING; endmatch
    casematch(AST_BOOL, Const*, c) c->type = BOOL; endmatch
    casematch(AST_UNIT, Const*, c) c->type = UNIT; endmatch
    casematch(AST_VAR, Var*, v)         // Handle typenames (T).
        Entry* e = env->lookup(v->name);
        if (!e && v->type != ERROR) undefined_var_error(mod, v), v->type = ERROR;
        else v->type = e->type;
        endmatch
    casematch(AST_PLUS, Unary*, u) infer_arithmetic(mod, env, u); endmatch
    casematch(AST_NEG, Unary*, u) infer_arithmetic(mod, env, u); endmatch
    casematch(AST_DEREF, Unary*, u)     // Handle pointer types (*T)
        infer(mod, env, u->child);
        Type* v = mod->typectx->defvar(mod, env);
        unify(mod->typectx->def<PtrType>(v), u->child->type);
        u->type = v;
        endmatch
    casematch(AST_ADDROF, Unary*, u) 
        infer(mod, env, u->child);
        // TODO: check for lvalue
        Type* var = mod->typectx->defvar(mod, env);
        unify(var, u->child->type);
        if (u->child->type) u->type = mod->typectx->def<PtrType>(var);
        else u->type = mod->typectx->def<PtrType>(mod->typectx->defvar(mod, env));
        endmatch
    casematch(AST_NOT, Unary*, u) infer_logic(mod, env, u); endmatch
    casematch(AST_BITNOT, Unary*, u) infer_bitwise(mod, env, u); endmatch
    casematch(AST_INCR, Unary*, u) infer_arithmetic(mod, env, u); endmatch // TODO: check for lvalue
    casematch(AST_DECR, Unary*, u) infer_arithmetic(mod, env, u); endmatch
    casematch(AST_POSTINCR, Unary*, u) infer_arithmetic(mod, env, u); endmatch
    casematch(AST_POSTDECR, Unary*, u) infer_arithmetic(mod, env, u); endmatch
    casematch(AST_NEW, Unary*, u)
        infer(mod, env, u->child);
        Type* var = mod->typectx->defvar(mod, env);
        unify(var, u->child->type);
        if (u->child->type) u->type = mod->typectx->def<PtrType>(var);
        endmatch
    casematch(AST_DEL, Unary*, u)
        infer(mod, env, u->child);
        // Check for pointer or slice in later typechecking.
        u->type = VOID;
        endmatch
    casematch(AST_NEWARRAY, Binary*, b)
        infer(mod, env, b->left);
        infer(mod, env, b->right);
        Type* var = mod->typectx->defvar(mod, env);
        unify(var, b->left->type);
        b->type = mod->typectx->def<SliceType>(var);
        endmatch
    casematch(AST_PAREN, Unary*, u) 
        infer(mod, env, u->child);
        u->type = u->child->type;
        endmatch
    casematch(AST_SIZEOF, Sizeof*, u) 
        infer(mod, env, u->child);
        if (u->fold) u->type = ICONST;
        else u->type = INT;
        // Check for array or slice in later typechecking.
        endmatch

    casematch(AST_ADD, Binary*, b) infer_arithmetic(mod, env, b); endmatch
    casematch(AST_SUB, Binary*, b) infer_arithmetic(mod, env, b); endmatch
    casematch(AST_STAR, Binary*, b) infer_arithmetic(mod, env, b); endmatch
    casematch(AST_DIV, Binary*, b) infer_arithmetic(mod, env, b); endmatch
    casematch(AST_MOD, Binary*, b) infer_arithmetic(mod, env, b); endmatch
    casematch(AST_EXP, Binary*, b) 
        infer_arithmetic(mod, env, b); 

        AST* fn = new(mod->parser->astspace) Var(b->pos, mod->interner->intern("pow"));
        slice<AST*> args = { new(mod->parser->astspace) AST*[2], 2 };
        args[0] = b->left;
        args[1] = b->right;
        b->left->type = b->type;
        b->right->type = b->type;
        b->left = new(mod->parser->astspace) Apply(b->pos, fn, args);
        infer(mod, env, b->left);
        b->type = b->left->type;
        endmatch
    casematch(AST_BITAND, Binary*, b) infer_bitwise(mod, env, b); endmatch
    casematch(AST_BITOR, Binary*, b) infer_bitwise(mod, env, b); endmatch
    casematch(AST_BITXOR, Binary*, b) infer_bitwise(mod, env, b); endmatch
    casematch(AST_BITLEFT, Binary*, b) infer_bitwise(mod, env, b); endmatch
    casematch(AST_BITRIGHT, Binary*, b) infer_bitwise(mod, env, b); endmatch
    casematch(AST_LESS, Binary*, b) infer_compare(mod, env, b); endmatch
    casematch(AST_LEQUAL, Binary*, b) infer_compare(mod, env, b); endmatch
    casematch(AST_GREATER, Binary*, b) infer_compare(mod, env, b); endmatch
    casematch(AST_GEQUAL, Binary*, b) infer_compare(mod, env, b); endmatch
    casematch(AST_EQUAL, Binary*, b) infer_equality(mod, env, b); endmatch
    casematch(AST_INEQUAL, Binary*, b) infer_equality(mod, env, b); endmatch
    casematch(AST_AND, Binary*, b) infer_logic(mod, env, b); endmatch
    casematch(AST_OR, Binary*, b) infer_logic(mod, env, b); endmatch
    casematch(AST_DOT, Binary*, b)
        infer(mod, env, b->left);
        b->type = mod->typectx->defvar(mod, env);
        // Typechecking right expr needs to happen after inference is done.
        endmatch
    casematch(AST_ASSIGN, Binary*, b) infer_assign(mod, env, b); endmatch
    casematch(AST_ADDEQ, Binary*, b) infer_arith_assign(mod, env, b); endmatch
    casematch(AST_SUBEQ, Binary*, b) infer_arith_assign(mod, env, b); endmatch
    casematch(AST_STAREQ, Binary*, b) infer_arith_assign(mod, env, b); endmatch
    casematch(AST_DIVEQ, Binary*, b) infer_arith_assign(mod, env, b); endmatch
    casematch(AST_MODEQ, Binary*, b) infer_arith_assign(mod, env, b); endmatch
    casematch(AST_EXPEQ, Binary*, b) infer_arith_assign(mod, env, b); endmatch
    casematch(AST_BITANDEQ, Binary*, b) infer_bitwise_assign(mod, env, b); endmatch
    casematch(AST_BITOREQ, Binary*, b) infer_bitwise_assign(mod, env, b); endmatch
    casematch(AST_BITXOREQ, Binary*, b) infer_bitwise_assign(mod, env, b); endmatch
    casematch(AST_BITLEFTEQ, Binary*, b) infer_bitwise_assign(mod, env, b); endmatch
    casematch(AST_BITRIGHTEQ, Binary*, b) infer_bitwise_assign(mod, env, b); endmatch
    casematch(AST_IS, Is*, b) 
        infer(mod, env, b->left);
        b->type = BOOL;
        endmatch

    casematch(AST_ARRAYTYPE, Binary*, b) endmatch // This prevents type expressions from validly mixing with normal expressions.
    casematch(AST_SLICETYPE, Binary*, b) endmatch
    casematch(AST_TYPENAME, Var*, b) endmatch
    casematch(AST_FUNTYPE, Apply*, b) endmatch
    casematch(AST_TYPEDOT, Binary*, b) endmatch
    casematch(AST_TYPEINST, Apply*, a) 
        if (a->type->env->decl) infer(mod, a->type->env->parent, a->type->env->decl);
        endmatch
    casematch(AST_PTRTYPE, Unary*, b) endmatch

    casematch(AST_PTRDECL, Binary*, b)
        infer(mod, env, b->left);
        b->type = b->left->type;
        endmatch

    casematch(AST_INDEX, Binary*, b)    // Handle array types (T[N]) and slice types (T[])
        infer(mod, env, b->left);
        infer(mod, env, b->right);
        if (unify(b->left->type, ANY_SLICE)) {
            b->type = mod->typectx->defvar(mod, env);
            b->left->type = unify(b->left->type, mod->typectx->def<SliceType>(b->type));
        }
        else if (unify(b->left->type, ANY_ARRAY)) {
            b->type = mod->typectx->defvar(mod, env);
            b->left->type = unify(b->left->type, mod->typectx->def<ArrayType>(b->type));
        }
        endmatch
    casematch(AST_SLICE, Slice*, s)
        infer(mod, env, s->array);
        if (s->low) {
            infer(mod, env, s->low);
            unify(s->low->type, ANY_NUMERIC);
        }
        if (s->high) {
            infer(mod, env, s->high);
            unify(s->high->type, ANY_NUMERIC);
        }
        s->type = mod->typectx->def<SliceType>(mod->typectx->defvar(mod, env));
        unify(s->type, s->array->type);
        endmatch
    casematch(AST_APPLY, Apply*, a)
        for (AST* ast : a->args) infer(mod, env, ast);
        a->type = mod->typectx->defvar(mod, env);
        
        // Due to method dispatch and generic functions, we can't actually do type inference on anything
        // other than the arguments at this point. Disappointing! Maybe we can look into changing this later...
        endmatch
    casematch(AST_ARRAY, List*, l) 
        if (l->items.n == 0) {
            l->type = mod->typectx->def<SliceType>(mod->typectx->defvar(mod, env));
            return;
        }
        for (AST* ast : l->items) infer(mod, env, ast);
        if (l->items.size() == 0) l->type = mod->typectx->def<ArrayType>(mod->typectx->defvar(mod, env), 0);
        else {
            Type* elt = mod->typectx->defvar(mod, env);
            for (i32 i = 0; i < l->items.n; i ++) {
                Type* new_elt = unify(elt, l->items[i]->type);
                if (!new_elt) {
                    incompatible_element_error(mod, l->items[i], concretify(mod, l->items[i]->type), concretify(mod, elt));
                    elt = ERROR;
                }
                else elt = new_elt;
            }
            l->type = mod->typectx->def<ArrayType>(elt, l->items.n);
        }
        endmatch
    casematch(AST_SET, List*, l) 
        for (AST* ast : l->items) infer(mod, env, ast);
        endmatch
    casematch(AST_VARDECL, VarDecl*, d)
        if (d->init) {
            infer(mod, env, d->init);
            unify(d->init->type, d->type);
        }
        endmatch
    casematch(AST_FUNDECL, FunDecl*, d) 
        if (!d->generic || d->isinst) {
            slice<AST*>& args = ((Apply*)d->proto)->args;
            for (AST* ast : args) infer(mod, d->env, ast);
            if (d->body) infer(mod, d->env, d->body);
        }
        else d->type = TYPE;
        endmatch
    casematch(AST_CTOR, Apply*, a)
        for (AST* ast : a->args) infer(mod, env, ast);
        a->type = a->fn->type; // Always produces a value of the constructed type, if successful.
        endmatch
    casematch(AST_GENCTOR, Apply*, a)
        for (AST* ast : a->args) infer(mod, env, ast);
        a->type = mod->typectx->defvar(mod, env); // We don't know what type will be produced without the types of the arguments.
        endmatch
    casematch(AST_ARGS, List*, d) 
        for (AST* ast : d->items) infer(mod, env, ast);
        d->type = VOID;
        endmatch
    casematch(AST_DO, List*, d) 
        for (AST* ast : d->items) infer(mod, env, ast);
        if (d->items.n) d->type = d->items[d->items.n - 1]->type;
        else d->type = VOID;
        endmatch
    casematch(AST_IF, If*, i) 
        infer(mod, i->env, i->cond);
        unify(i->cond->type, BOOL);
        infer(mod, i->env, i->ifTrue);
        if (i->ifFalse) {
            infer(mod, i->env, i->ifFalse);
            unify(i->ifTrue->type, i->ifFalse->type);
        }
        i->type = VOID;
        endmatch
    casematch(AST_WHILE, Loop*, l) 
        infer(mod, l->env, l->cond);
        unify(l->cond->type, BOOL);
        infer(mod, l->env, l->body);
        l->type = VOID;
        endmatch
    casematch(AST_UNTIL, Loop*, l) 
        infer(mod, l->env, l->cond);
        unify(l->cond->type, BOOL);
        infer(mod, l->env, l->body);
        l->type = VOID;
        endmatch
    casematch(AST_FOR, For*, f) 
        infer(mod, f->env, f->body);
        f->type = VOID;
        endmatch
    casematch(AST_DEFER, Unary*, u)
        u->type = VOID;
        endmatch
    casematch(AST_RETURN, Unary*, u) 
        if (u->child) {
            infer(mod, env, u->child);
            Env* fenv = env;
            while (fenv->kind != ENV_FUN && fenv->kind != ENV_GLOBAL && fenv->kind != ENV_TYPE) 
                fenv = fenv->parent;
            if (fenv->kind != ENV_FUN) {
                return_outside_fun_error(mod, u, fenv);
                u->type = VOID;
                return;
            }
            env = fenv;
            Entry* e = env->parent->lookup(env->name);
            if (!e || e->kind != E_FUN) {
                print(mod->interner->str(env->name), '\n');
                env->parent->format(stdout, mod, 0);
                unreachable("Could not find definition for enclosing function.");
            }
            Type* ret = mod->typectx->defvar(mod, env);
            unify(e->type, mod->typectx->def<FunType>(ret));
            unify(u->child->type, ret);
        }
        u->type = VOID;
        endmatch
    casematch(AST_BREAK, Expr*, e) 
        e->type = VOID;
        endmatch
    casematch(AST_CONTINUE, Expr*, e) 
        e->type = VOID;
        endmatch
    casematch(AST_WITH, With*, w) 
        detect_types(mod, env, w->bound);
        Env* e = nullptr;
        if (is_type(w->bound)) e = type_env(w->bound->type);
        else if (w->bound->kind == AST_MODULENAME) {
            auto en = env->lookup(((Var*)w->bound)->name);
            if (en->kind == E_MOD) e = ((ModuleDecl*)en->ast)->env;
        }
        infer(mod, e, w->bound);
        if (e) w->env->siblings.push(e);
        else {
            invalid_with_env_error(mod, w);
            w->type = VOID;
            return;
        }
        detect_types(mod, w->env, w->body);
        infer(mod, w->env, w->body);
        w->type = VOID;
        endmatch
    casematch(AST_USE, Binary*, b) 
        b->type = VOID;
        endmatch
    casematch(AST_ALIASDECL, AliasDecl*, d) 
        endmatch
    casematch(AST_TYPEDECL, TypeDecl*, d) 
        if (d->generic) {
            d->type = TYPE;
            for (const auto& e : d->insts) if (e.value != d->prototype) infer(mod, env, e.value);
        }
        else if (d->body) infer(mod, d->env, d->body);
        endmatch
    casematch(AST_CASEDECL, CaseDecl*, d)
        if (d->env->kind == ENV_TYPE && d->body) infer(mod, d->env, d->body);
        endmatch
    casematch(AST_MATCH, Binary*, b) 
        infer(mod, env, b->left);
        // Inference for the cases can only be done once we have completed inference.
        b->type = VOID;
        endmatch
    casematch(AST_MODULEDECL, ModuleDecl*, b) 
        infer(mod, b->env, b->body);
        b->type = VOID; 
        endmatch
    default:
        break;
    }
}

AST* coerce(Module* mod, Env* env, AST* ast, Type* dest) {
    // print("coercing "), format(stdout, mod, ast), print(" : "), format(stdout, mod, ast->type), print(" to "), format(stdout, mod, dest), print('\n');
    ast->type = concretify(mod, ast->type);
    if (ast->type == ERROR) return ast;
    if (!dest) return nullptr;
    if (!is_subtype(ast->type, dest)) return nullptr;
    if (is_subtype_generic(ast->type, dest)) return ast;
    
    AST* conv = new(mod->parser->astspace) Unary(AST_CONV, ast->pos, ast);
    conv->type = dest;
    return conv;
}

void check_arithmetic(Module* mod, Env* env, Unary* ast) {
    typecheck(mod, env, ast->child);
    ast->type = unify(ast->type, ast->child->type);
    if (!unify(ast->type, ANY_NUMERIC)) {
        non_arithmetic_type_error(mod, ast, ast->type);
        ast->type = ERROR;
    }
    else {
        AST* child = coerce(mod, env, ast->child, ast->type);
        if (!child) incompatible_operand_error(mod, ast->child, concretify(mod, ast->child->type), concretify(mod, ast->type)), ast->type = ERROR;
        else ast->child = child;
    }
}

void check_arithmetic(Module* mod, Env* env, Binary* ast) {
    typecheck(mod, env, ast->left);
    typecheck(mod, env, ast->right);
    Type* result = unify(ast->left->type, ast->right->type);
    unify(result, ast->type);
    if (!unify(ast->type, ANY_NUMERIC)) {
        non_arithmetic_type_error(mod, ast, ast->type);
        ast->type = ERROR;
    }
    else {
        AST* left = coerce(mod, env, ast->left, ast->type), *right = coerce(mod, env, ast->right, ast->type);
        if (left) ast->left = left;
        else incompatible_operand_error(mod, ast->left, concretify(mod, ast->left->type), concretify(mod, ast->type));
        if (right) ast->right = right;
        else incompatible_operand_error(mod, ast->right, concretify(mod, ast->right->type), concretify(mod, ast->type));
        if (!left || !right) ast->type = ERROR;
    }
}

void check_bitwise(Module* mod, Env* env, Unary* ast) {
    typecheck(mod, env, ast->child);
    ast->type = concretify(mod, unify(ast->type, ast->child->type));
    if (ast->type != ERROR && (ast->type->kind != T_NUMERIC || ((NumericType*)ast->type)->floating)) {
        non_integral_type_error(mod, ast, ast->type);
        ast->type = ERROR;
    }
    else {
        AST* child = coerce(mod, env, ast->child, ast->type);
        if (!child) incompatible_operand_error(mod, ast->child, concretify(mod, ast->child->type), concretify(mod, ast->type)), ast->type = ERROR;
        else ast->child = child;
    }
}

void check_bitwise(Module* mod, Env* env, Binary* ast) {
    typecheck(mod, env, ast->left);
    typecheck(mod, env, ast->right);
    Type* result = unify(ast->left->type, ast->right->type);
    ast->type = concretify(mod, unify(ast->type, result));
    if (ast->type != ERROR && (ast->type->kind != T_NUMERIC || ((NumericType*)ast->type)->floating)) {
        non_integral_type_error(mod, ast, ast->type);
        ast->type = ERROR;
    }
    else {
        AST* left = coerce(mod, env, ast->left, ast->type), *right = coerce(mod, env, ast->right, ast->type);
        if (left) ast->left = left;
        else incompatible_operand_error(mod, ast->left, concretify(mod, ast->left->type), concretify(mod, ast->type));
        if (right) ast->right = right;
        else incompatible_operand_error(mod, ast->right, concretify(mod, ast->right->type), concretify(mod, ast->type));
        if (!left || !right) ast->type = ERROR;
    }
}

void check_logic(Module* mod, Env* env, Unary* ast) {
    typecheck(mod, env, ast->child);
    AST* child = coerce(mod, env, ast->child, BOOL);
    if (!child) incompatible_operand_error(mod, ast->child, concretify(mod, ast->child->type), BOOL);
    else ast->child = child;
}

void check_logic(Module* mod, Env* env, Binary* ast) {
    typecheck(mod, env, ast->left);
    typecheck(mod, env, ast->right);
    AST* left = coerce(mod, env, ast->left, BOOL), *right = coerce(mod, env, ast->right, BOOL);
    if (left) ast->left = left;
    else incompatible_operand_error(mod, ast->left, concretify(mod, ast->left->type), BOOL);
    if (right) ast->right = right;
    else incompatible_operand_error(mod, ast->right, concretify(mod, ast->right->type), BOOL);
}

void check_compare(Module* mod, Env* env, Binary* ast) {
    typecheck(mod, env, ast->left);
    typecheck(mod, env, ast->right);
    Type* result = unify(ast->left->type, ast->right->type);
    if (!unify(result, ANY_NUMERIC) && !unify(result, ANY_PTR) && !unify(result, STRING) && !unify(result, CHAR)) {
        non_comparable_type_error(mod, ast, result);
    }
    else {
        AST* left = coerce(mod, env, ast->left, result), *right = coerce(mod, env, ast->right, concretify(mod, result));
        if (left) ast->left = left;
        else incompatible_operand_error(mod, ast->left, concretify(mod, ast->left->type), concretify(mod, result));
        if (right) ast->right = right;
        else incompatible_operand_error(mod, ast->right, concretify(mod, ast->right->type), concretify(mod, result));
    }
}

void check_equality(Module* mod, Env* env, Binary* ast) {
    typecheck(mod, env, ast->left);
    typecheck(mod, env, ast->right);
    Type* result = unify(ast->left->type, ast->right->type);
    if (!unify(result, ANY_NUMERIC) && !unify(result, ANY_PTR) 
        && !unify(result, STRING) && !unify(result, CHAR) 
        && !unify(result, ANY_FUNCTION) && !unify(result, BOOL)) {
        non_equality_type_error(mod, ast, result);
    }
    else {
        AST* left = coerce(mod, env, ast->left, result), *right = coerce(mod, env, ast->right, concretify(mod, result));
        if (left) ast->left = left;
        else incompatible_operand_error(mod, ast->left, concretify(mod, ast->left->type), concretify(mod, result));
        if (right) ast->right = right;
        else incompatible_operand_error(mod, ast->right, concretify(mod, ast->right->type), concretify(mod, result));
    }
}

void check_assign(Module* mod, Env* env, Binary* ast) {
    typecheck(mod, env, ast->left);
    typecheck(mod, env, ast->right);
    AST* right = coerce(mod, env, ast->right, ast->left->type);
    if (!right) incompatible_assignment_error(mod, ast->right, concretify(mod, ast->right->type), concretify(mod, ast->left->type));
    else ast->right = right;
}

void check_arith_assign(Module* mod, Env* env, Binary* ast) {
    typecheck(mod, env, ast->left);
    typecheck(mod, env, ast->right);
    AST* right = coerce(mod, env, ast->right, ast->left->type);
    if (!right) incompatible_assignment_error(mod, ast->right, concretify(mod, ast->right->type), concretify(mod, ast->left->type));
    else ast->right = right;
    ast->left->type = concretify(mod, ast->left->type);
    if (ast->left->type != ERROR && ast->left->type->kind != T_NUMERIC)
        non_arithmetic_type_error(mod, ast->left, ast->left->type);
}

void check_bitwise_assign(Module* mod, Env* env, Binary* ast) {
    typecheck(mod, env, ast->left);
    typecheck(mod, env, ast->right);
    AST* right = coerce(mod, env, ast->right, ast->left->type);
    if (!right) incompatible_assignment_error(mod, ast->right, concretify(mod, ast->right->type), concretify(mod, ast->left->type));
    else ast->right = right;
    ast->left->type = concretify(mod, ast->left->type);
    if (ast->left->type != ERROR && (ast->left->type->kind != T_NUMERIC || ((NumericType*)ast->left->type)->floating))
        non_integral_type_error(mod, ast->left, ast->left->type);
}

bool verify_accessible_field(Env* env, Env* defenv, i32 name) {
    if (env == defenv) return true;
    else if (defenv == env->parent && defenv->kind == ENV_TYPE && env->kind == ENV_FUN) return true;

    bool result = false;
    if (env->kind == ENV_MOD || env->kind == ENV_LOCAL) result = result || verify_accessible_field(env->parent, defenv, name);
    for (Env* sib : env->siblings) if (!result) result = result || verify_accessible_field(sib, defenv, name);
    return result;
}

void typecheck(Module* mod, Env* env, AST* ast) {
    switch (ast->kind) {
    casematch(AST_PROGRAM, ASTProgram*, p)
        for (Statement* stmt : p->toplevel) typecheck(mod, p->env, stmt);
        for (AST* ast : p->defers) typecheck(mod, p->deferred, ast);
        endmatch

    // Constants don't need typechecking - they are always valid and concrete.

    casematch(AST_VAR, Var*, v)
        Entry* e = env->lookup(v->name);
        if (!e && v->type != ERROR) undefined_var_error(mod, v), v->type = ERROR;
        if (v->type != ERROR) {
            Env* defenv = env->find(v->name);
            if (defenv && defenv != env && defenv->kind != ENV_ROOT && defenv->kind != ENV_GLOBAL) {
                AST* def = defenv->lookup(v->name)->ast;
                Env* ptr = env;
                if (!verify_accessible_field(ptr, defenv, v->name)) {
                    while (ptr->kind == ENV_MOD || ptr->kind == ENV_LOCAL) ptr = ptr->parent;
                    inaccessible_member_error(mod, v, ptr->decl, def), v->type = ERROR; 
                }
            }
        }
        if (v->type != ERROR) v->type->referenced_by_name = true;
        endmatch
    
    casematch(AST_PLUS, Unary*, u) check_arithmetic(mod, env, u); endmatch
    casematch(AST_NEG, Unary*, u) check_arithmetic(mod, env, u); endmatch
    casematch(AST_DEREF, Unary*, u)
        typecheck(mod, env, u->child);
        if (!unify(u->child->type, ANY_PTR)) 
            non_pointer_dereference_error(mod, u->child, concretify(mod, u->child->type)), u->type = ERROR;
        endmatch
    casematch(AST_ADDROF, Unary*, u)
        typecheck(mod, env, u->child);
        // TODO: check for lvalue
        endmatch
    casematch(AST_NOT, Unary*, u) check_logic(mod, env, u); endmatch
    casematch(AST_BITNOT, Unary*, u) check_bitwise(mod, env, u); endmatch
    casematch(AST_INCR, Unary*, u) check_arithmetic(mod, env, u); endmatch // TODO: check for lvalue
    casematch(AST_DECR, Unary*, u) check_arithmetic(mod, env, u); endmatch
    casematch(AST_POSTINCR, Unary*, u) check_arithmetic(mod, env, u); endmatch
    casematch(AST_POSTDECR, Unary*, u) check_arithmetic(mod, env, u); endmatch
    casematch(AST_NEW, Unary*, u)
        typecheck(mod, env, u->child);
        mod->cnew_types.insert(u->child->type);
        u->child->type->gen_placement_new = true;
        endmatch
    casematch(AST_DEL, Unary*, u)
        typecheck(mod, env, u->child);
        u->child->type = concretify(mod, u->child->type);
        if (u->child->type != ERROR && u->child->type->kind != T_PTR && u->child->type->kind != T_SLICE)
            invalid_delete_error(mod, u->child, u->child->type);
        endmatch
    casematch(AST_NEWARRAY, Binary*, b)
        typecheck(mod, env, b->left);
        typecheck(mod, env, b->right);
        b->right->type = concretify(mod, b->right->type);
        if (b->right->type->kind != T_NUMERIC || ((NumericType*)b->right->type)->floating) {
            if (b->right->type != ERROR) non_integral_newarray_dim_error(mod, b->right, b->right->type);
            b->type = ERROR;
        }
        else {
            b->type = concretify(mod, b->type);
            if (b->type->kind != T_SLICE) unreachable("Somehow newarray type was not a slice.");
            AST* r = coerce(mod, env, b->left, ((SliceType*)b->type)->element);
            if (!r) {
                inference_mismatch_error(mod, b->right, b->left->type, ((SliceType*)b->type)->element);
                b->type = ERROR;
            }
            else b->left = r;
            b->left->type = concretify(mod, b->left->type);
            mod->cnew_types.insert(((SliceType*)b->type)->element);
            ((SliceType*)b->type)->element->gen_placement_new = true;
        }
        endmatch
    casematch(AST_PAREN, Unary*, u) 
        typecheck(mod, env, u->child);
        u->type = u->child->type;
        endmatch
    casematch(AST_SIZEOF, Sizeof*, u) 
        typecheck(mod, env, u->child);
        u->child->type = concretify(mod, u->child->type);
        if (!u->fold) u->fold = tryfold(mod, env, u);
        if (is_type(u->child)) u->type = ICONST;
        else if (u->child->type->kind == T_ARRAY) u->type = ICONST;
        else if (u->child->type->kind == T_SLICE) u->type = INT;
        else if (u->child->type != ERROR) invalid_sizeof_operand_error(mod, u->child, u->child->type), u->type = ERROR;
        endmatch

    casematch(AST_ADD, Binary*, b) check_arithmetic(mod, env, b); endmatch
    casematch(AST_SUB, Binary*, b) check_arithmetic(mod, env, b); endmatch
    casematch(AST_STAR, Binary*, b) check_arithmetic(mod, env, b); endmatch
    casematch(AST_DIV, Binary*, b) check_arithmetic(mod, env, b); endmatch
    casematch(AST_MOD, Binary*, b) check_arithmetic(mod, env, b); endmatch
    casematch(AST_EXP, Binary*, b) 
        typecheck(mod, env, ((Apply*)b->left)->args[0]);
        typecheck(mod, env, ((Apply*)b->left)->args[1]);
        Type* result = unify(((Apply*)b->left)->args[0]->type, ((Apply*)b->left)->args[1]->type);
        if (!unify(result, ANY_NUMERIC)) {
            non_arithmetic_type_error(mod, b, result);
            b->type = ERROR;
        }
        else {
            AST* left = coerce(mod, env, ((Apply*)b->left)->args[0], result);
            if (!left) incompatible_operand_error(mod, ((Apply*)b->left)->args[0], concretify(mod, ((Apply*)b->left)->args[0]->type), concretify(mod, result));
            else ((Apply*)b->left)->args[0] = left;
            AST* right = coerce(mod, env, ((Apply*)b->left)->args[1], result);
            if (!right) incompatible_operand_error(mod, ((Apply*)b->left)->args[1], concretify(mod, ((Apply*)b->left)->args[1]->type), concretify(mod, result));
            else ((Apply*)b->left)->args[1] = right;
            if (!left || !right) b->type = ERROR;
            else {
                typecheck(mod, env, b->left);
                b->type = b->left->type;
            }
        }
        endmatch
    casematch(AST_BITAND, Binary*, b) check_bitwise(mod, env, b); endmatch
    casematch(AST_BITOR, Binary*, b) check_bitwise(mod, env, b); endmatch
    casematch(AST_BITXOR, Binary*, b) check_bitwise(mod, env, b); endmatch
    casematch(AST_BITLEFT, Binary*, b) check_bitwise(mod, env, b); endmatch
    casematch(AST_BITRIGHT, Binary*, b) check_bitwise(mod, env, b); endmatch
    casematch(AST_LESS, Binary*, b) check_compare(mod, env, b); endmatch
    casematch(AST_LEQUAL, Binary*, b) check_compare(mod, env, b); endmatch
    casematch(AST_GREATER, Binary*, b) check_compare(mod, env, b); endmatch
    casematch(AST_GEQUAL, Binary*, b) check_compare(mod, env, b); endmatch
    casematch(AST_EQUAL, Binary*, b) check_equality(mod, env, b); endmatch
    casematch(AST_INEQUAL, Binary*, b) check_equality(mod, env, b); endmatch
    casematch(AST_AND, Binary*, b) check_logic(mod, env, b); endmatch
    casematch(AST_OR, Binary*, b) check_logic(mod, env, b); endmatch
    casematch(AST_DOT, Binary*, b)
        // print("typechecking dot "), format(stdout, mod, b), print(" in env "), env->format(stdout, mod, 0);
        typecheck(mod, env, b->left);
        if (b->left->type == ERROR) {
            b->type = ERROR;
            return;
        }
        Env* nenv = ast_env(env, b->left);
        if (nenv) { // Static field or module access.
            infer(mod, nenv, b->right);
            typecheck(mod, nenv, b->right);
            b->type = b->right->type;
        }
        else if ((nenv = type_env(b->left->type = concretify(mod, b->left->type)))) { // Field access instead.
            // print("reading field from env "), nenv->format(stdout, mod, 0);
            if (b->left->type->kind == T_PTR) nenv = type_env(((PtrType*)b->left->type)->target);
            infer(mod, nenv, b->right);
            typecheck(mod, nenv, b->right);
            b->type = b->right->type;
        }
        else invalid_dot_expression_error(mod, b->left), b->type = ERROR;
        endmatch
    casematch(AST_ASSIGN, Binary*, b) check_assign(mod, env, b); endmatch
    casematch(AST_ADDEQ, Binary*, b) check_arith_assign(mod, env, b); endmatch
    casematch(AST_SUBEQ, Binary*, b) check_arith_assign(mod, env, b); endmatch
    casematch(AST_STAREQ, Binary*, b) check_arith_assign(mod, env, b); endmatch
    casematch(AST_DIVEQ, Binary*, b) check_arith_assign(mod, env, b); endmatch
    casematch(AST_MODEQ, Binary*, b) check_arith_assign(mod, env, b); endmatch
    casematch(AST_EXPEQ, Binary*, b) check_arith_assign(mod, env, b); endmatch
    casematch(AST_BITANDEQ, Binary*, b) check_bitwise_assign(mod, env, b); endmatch
    casematch(AST_BITOREQ, Binary*, b) check_bitwise_assign(mod, env, b); endmatch
    casematch(AST_BITXOREQ, Binary*, b) check_bitwise_assign(mod, env, b); endmatch
    casematch(AST_BITLEFTEQ, Binary*, b) check_bitwise_assign(mod, env, b); endmatch
    casematch(AST_BITRIGHTEQ, Binary*, b) check_bitwise_assign(mod, env, b); endmatch
    casematch(AST_IS, Is*, b) 
        typecheck(mod, env, b->left);
        Type* unified = unify(b->left->type, b->right->type);
        if (!unified) b->reachable = false;
        endmatch

    // Type expressions do not need to be revisited since they are always type 'type'.

    casematch(AST_PTRDECL, Binary*, b) 
        typecheck(mod, env, b->left);
        b->type = b->left->type;
        endmatch
    casematch(AST_TYPEINST, Apply*, a) 
        if (a->type->env->decl) typecheck(mod, a->type->env->parent, a->type->env->decl);
        endmatch

    casematch(AST_INDEX, Binary*, b)
        typecheck(mod, env, b->left);
        typecheck(mod, env, b->right);
        Type* lt = concretify(mod, b->left->type);
        Type* rt = concretify(mod, b->right->type);
        if (rt != ERROR && (rt->kind != T_NUMERIC || ((NumericType*)rt)->floating))
            non_integral_type_error(mod, b->right, rt);
        Type* et = nullptr;
        if (lt->kind == T_ARRAY) et = ((ArrayType*)lt)->element;
        else if (lt->kind == T_SLICE) et = ((SliceType*)lt)->element;
        else non_indexable_type_error(mod, b->left, lt);
        if (b->type != ERROR) {
            Type* bt = concretify(mod, unify(et, b->type));
            if (bt == ERROR) inference_mismatch_error(mod, b, concretify(mod, b->type), et);
            else b->type = bt;
        }
        endmatch
    casematch(AST_SLICE, Slice*, s)
        typecheck(mod, env, s->array);
        if (s->low) {
            typecheck(mod, env, s->low);
            s->low->type = concretify(mod, s->low->type);
            if (s->low->type->kind != T_NUMERIC || ((NumericType*)s->low->type)->floating)
                non_integral_type_error(mod, s->low, s->low->type);
        }
        if (s->high) {
            typecheck(mod, env, s->high);
            s->high->type = concretify(mod, s->high->type);
            if (s->high->type->kind != T_NUMERIC || ((NumericType*)s->high->type)->floating)
                non_integral_type_error(mod, s->high, s->high->type);
        }
        s->array->type = concretify(mod, s->array->type);
        Type* dt = nullptr;
        if (s->array->type->kind == T_ARRAY) dt = mod->typectx->def<SliceType>(((ArrayType*)s->array->type)->element);
        else if (s->array->type->kind == T_SLICE) dt = s->array->type;
        else {
            non_sliceable_type_error(mod, s->array, s->array->type);
            s->type = ERROR;
        }
        if (s->type != ERROR) {
            Type* st = concretify(mod, unify(dt, s->type));
            if (st == ERROR) inference_mismatch_error(mod, s, concretify(mod, s->type), dt);
            else s->type = st;
        }
        endmatch
    casematch(AST_APPLY, Apply*, a)
        vec<Type*, 256, arena> argv;
        argv.alloc = &mod->typectx->typespace;
        for (AST* ast : a->args) {
            typecheck(mod, env, ast);
            argv.push(ast->type);
        }
        for (Type* t : argv) if (t == ERROR) {
            a->type = ERROR;
            return;
        }
        
        Env* e = env;
        auto p = call_parent(mod, a);
        if (p) {
            e = p->second;
            Type* dest = p->first;
            a->args[0]->type = concretify(mod, a->args[0]->type);
            if (a->args.n && a->args[0]->type != dest && a->args[0]->type->kind == T_PTR) {
                Type* orig = a->args[0]->type;
                a->args[0] = new(mod->parser->astspace) Unary(AST_DEREF, a->args[0]->pos, a->args[0]);
                a->args[0]->type = ((PtrType*)orig)->target;
                AST* arg = coerce(mod, env, a->args[0], dest);
                if (!arg) incompatible_argument_error(mod, a->args[0], a->args[0]->type, dest);
                else a->args[0] = arg;
            }
        }
        
        if (a->fn->kind == AST_VAR) {
            Entry* en = e->lookup(((Var*)a->fn)->name);
            if (en && en->kind == E_GENFUN) {
                FunDecl* d = (FunDecl*)en->ast;
                if (((Apply*)d->proto)->args.n != a->args.size()) {
                    gencall_mismatch_error(mod, a, d, a->args.size(), ((Apply*)d->proto)->args.n);
                    a->type = ERROR;
                    return;
                }
                FunDecl* inst = inst_fun(mod, env, d, a->args);
                if (!inst) {
                    a->type = ERROR;
                    return;
                }
                else ((Var*)a->fn)->name = inst->basename;
            }
        }

        infer(mod, e, a->fn);
        typecheck(mod, e, a->fn);
        a->fn->type = concretify(mod, a->fn->type);
        if (a->fn->type->kind != T_FUN) {
            if (a->fn->type != ERROR) non_function_type_error(mod, a->fn, a->fn->type);
            a->type = ERROR;
            return;
        }
        if (argv.size() == 0) argv.push(UNIT);
        slice<Type*>& argt = ((FunType*)a->fn->type)->arg;
        if (argt.n != argv.size()) {
            AST* d = nullptr;
            if (a->fn->kind == AST_VAR) {
                Entry* e = env->lookup(((Var*)a->fn)->name);
                if (e && e->ast) d = e->ast;
            }
            call_mismatch_error(mod, a, d, argv.size(), argt.n);
            a->type = ERROR;
            return;
        }
        if (a->args.n == 0) {
            if (argt.n != 1 || argt[0] != UNIT) {
                empty_arguments_error(mod, a, a->fn->type);
                a->type = ERROR;
            }
        }
        else for (i32 i = 0; i < argt.n; i ++) {
            Type* orig = a->args[i]->type;
            AST* arg = coerce(mod, env, a->args[i], argt[i]);
            if (!arg) {
                incompatible_argument_error(mod, a->args[i], concretify(mod, a->args[i]->type), argt[i]);
                a->type = ERROR;
            }
            else a->args[i] = arg;
        }
        if (a->type != ERROR) a->type = ((FunType*)a->fn->type)->ret;
        endmatch
    casematch(AST_GENCTOR, Apply*, a)
        for (AST* ast : a->args) 
            typecheck(mod, env, ast), ast->type = concretify(mod, ast->type);
        if (a->fn->kind != AST_VAR) unreachable("Somehow generic type no longer is a var.");
        Entry* e = env->lookup(((Var*)a->fn)->name);
        if (!e || e->kind != E_GENTYPE) unreachable("Somehow couldn't find generic type definition.");
        map<Symbol, Type*, 8, arena> typebindings;
        typebindings.alloc = &mod->typectx->typespace;
        TypeDecl* gend = (TypeDecl*)e->ast;
        TypeDecl* proto = gend->prototype;
        const_slice<AST*> tparams = ((Apply*)gend->name)->args;

        if (proto->type->kind == T_UNION) {
            union_ctor_error(mod, a, proto->type); 
            a->type = ERROR;
            return;
        }

        slice<AST*> instparams = { new(mod->parser->astspace) AST*[tparams.n], tparams.n };
        if (proto->type->kind == T_STRUCT) {
            const_slice<pair<i32, Type*>> fields = ((StructType*)proto->type)->fields;
            if (fields.n != a->args.n) fatal("Generic ctor mismatch.");
            for (i32 i = 0; i < fields.n; i ++) unify(fields[i].second, a->args[i]->type);
            
            for (i32 i = 0; i < tparams.n; i ++) {
                Type* t = proto->env->lookup(((Var*)tparams[i])->name)->type;
                instparams[i] = new(mod->parser->astspace) Var(tparams[i]->pos, ((Var*)tparams[i])->name);
                instparams[i]->kind = AST_TYPENAME, instparams[i]->type = fullsimplify(*mod->typectx, concretify(mod, t));
            }

            for (i32 i = 0; i < fields.n; i ++) unbind(fields[i].second);
        }
        else if (proto->type->kind == T_NAMED) {
            if (a->args.n != 1) fatal("Named ctor mismatch.");
            unify(((NamedType*)proto->type)->inner, a->args[0]->type);

            Type* t = proto->env->lookup(((Var*)tparams[0])->name)->type;
            instparams[0] = new(mod->parser->astspace) Var(tparams[0]->pos, ((Var*)tparams[0])->name);
            instparams[0]->kind = AST_TYPENAME, instparams[0]->type = fullsimplify(*mod->typectx, concretify(mod, t));

            unbind(((NamedType*)proto->type)->inner);
        }

        // print("instantiating "), format(stdout, mod, gend), print(" on ");
        // bool first = true;
        // for (AST* ast : instparams) {
        //     if (!first) print(", ");
        //     first = false;
        //     format(stdout, mod, ast->type);
        // }
        // print('\n');

        TypeDecl* decl = inst_type(mod, env, gend, instparams);
        if (!decl) a->type = ERROR;
        else {
            a->fn->kind = AST_TYPENAME;
            a->fn->type = decl->type;
            a->kind = AST_CTOR;
            infer(mod, decl->env->parent, decl);
            typecheck(mod, decl->env->parent, decl);
            typecheck(mod, env, a);
        }
        endmatch
    casematch(AST_ARRAY, List*, l) 
        if (l->items.n == 0) return;
        
        for (AST* ast : l->items) typecheck(mod, env, ast);
        l->type = concretify(mod, l->type);
        if (l->type->kind != T_ARRAY) non_array_type_error(mod, l, l->type);
        else for (i32 i = 0; i < l->items.n; i ++) {
            AST* item = coerce(mod, env, l->items[i], ((ArrayType*)l->type)->element);
            if (!item) {
                incompatible_element_error(mod, l->items[i], concretify(mod, l->items[i]->type), ((ArrayType*)l->type)->element);
                l->type = ERROR;
            }
            else l->items[i] = item;
        }
        if(l->type != ERROR) l->type->ctor_called = true;
        endmatch
    casematch(AST_SET, List*, l) 
        for (AST* ast : l->items) typecheck(mod, env, ast);
        endmatch
    casematch(AST_VARDECL, VarDecl*, d)
        if (d->init) {
            typecheck(mod, env, d->init);
            Type* orig = d->init->type;
            if (d->type) {
                // print("coercing "), format(stdout, mod, d->init), print(" : "), format(stdout, mod, d->init->type), print(" to "), format(stdout, mod, d->type), print('\n');
                AST* init = coerce(mod, env, d->init, d->type);
                d->type = concretify(mod, d->type);
                if (!init) {
                    incompatible_initializer_error(mod, d->init, concretify(mod, d->init->type), d->type);
                }
                else d->init = init;
            }
        }
        endmatch
    casematch(AST_FUNDECL, FunDecl*, d) 
        if (!d->generic || d->isinst) {
            slice<AST*>& args = ((Apply*)d->proto)->args;
            for (AST* ast : args) typecheck(mod, d->env, ast);
            if (d->method_type) {
                d->method_type = args[0]->type = concretify(mod, d->method_type);
            }
            if (d->body) typecheck(mod, d->env, d->body); 
        }
        for (const auto& e : d->insts) typecheck(mod, env, e.value);
        endmatch
    casematch(AST_CTOR, Apply*, a)
        infer(mod, env, a->fn);
        typecheck(mod, env, a->fn);
        if (!is_type(a->fn)) unreachable("Expected typename in constructor call.");
        Type* t = concretify(mod, a->fn->type); // We assume it's a typename.
        switch (t->kind) {
            case T_NUMERIC:
                if (a->args.n != 1) {
                    cast_mismatch_error(mod, a, t, a->args.n, 1);
                    break;
                }
                infer(mod, env, a->args[0]);
                typecheck(mod, env, a->args[0]);
                if (a->args[0]->type->kind == T_PTR && t == IPTR) break;
                break;
            case T_PTR:
                if (a->args.n != 1) {
                    cast_mismatch_error(mod, a, t, a->args.n, 1);
                    break;
                }
                infer(mod, env, a->args[0]);
                typecheck(mod, env, a->args[0]);
                a->args[0]->type = concretify(mod, a->args[0]->type); 
                if (a->args[0]->type != IPTR && !unify(a->args[0]->type, ANY_PTR) && !unify(a->args[0]->type, ANY_SLICE)) {
                    if (a->args[0]->type != ERROR) invalid_cast_error(mod, a->args[0], a->args[0]->type, t);
                    break;
                }
                break;
            case T_CHAR:
                if (a->args.n != 1) {
                    cast_mismatch_error(mod, a, t, a->args.n, 1);
                    break;
                }
                infer(mod, env, a->args[0]);
                typecheck(mod, env, a->args[0]);
                a->args[0]->type = concretify(mod, a->args[0]->type);
                if (a->args[0]->type->kind != T_NUMERIC || ((NumericType*)a->args[0]->type)->floating)
                    invalid_cast_error(mod, a->args[0], a->args[0]->type, t);
                break;
            case T_STRING: {
                if (a->args.n != 1) {
                    cast_mismatch_error(mod, a, t, a->args.n, 1);
                    break;
                }
                infer(mod, env, a->args[0]);
                typecheck(mod, env, a->args[0]);
                Type* dst = mod->typectx->def<SliceType>(I8);
                AST* arg = coerce(mod, env, a->args[0], dst);
                if (!arg) invalid_cast_error(mod, a->args[0], concretify(mod, a->args[0]->type), dst);
                else a->args[0] = arg;
                break;
            }
            case T_SLICE: 
                if (a->args.n == 2) {
                    infer(mod, env, a->args[0]);
                    typecheck(mod, env, a->args[0]);
                    infer(mod, env, a->args[1]);
                    typecheck(mod, env, a->args[1]);
                    if (!is_subtype_generic(a->args[0]->type, ANY_PTR) || !is_subtype_generic(ICONST, a->args[1]->type))
                        invalid_cast_error(mod, a->args[0], a->args[0]->type, t);
                    break;
                }
                if (a->args.n != 1) {
                    cast_mismatch_error(mod, a, t, a->args.n, 1);
                    break;
                }
                infer(mod, env, a->args[0]);
                typecheck(mod, env, a->args[0]);
                if (is_subtype_generic(((SliceType*)t)->element, I8) && is_subtype_generic(a->args[0]->type, STRING)) break;
                if (unify(a->args[0]->type, ANY_ARRAY) && is_subtype(a->args[0]->type, t)) break;
                invalid_cast_error(mod, a->args[0], a->args[0]->type, t);
                break;
            case T_NAMED: {
                Type* inner = ((NamedType*)t)->inner;
                if (a->args.n != 1) {
                    ctor_mismatch_error(mod, a, t, a->args.n, 1);
                    break;
                }
                infer(mod, env, a->args[0]);
                typecheck(mod, env, a->args[0]);
                AST* arg = coerce(mod, env, a->args[0], inner);
                if (!arg)
                    incompatible_ctor_param_error(mod, a->args[0], concretify(mod, a->args[0]->type), inner);
                else {
                    a->args[0] = arg;
                    t->ctor_called = true;
                }
                break;
            }
            case T_STRUCT: {
                slice<pair<i32, Type*>>& fields = ((StructType*)t)->fields;
                if (a->args.n != fields.n) {
                    ctor_mismatch_error(mod, a, t, a->args.n, fields.n);
                    break;
                }
                for (AST* arg : a->args) infer(mod, env, arg);
                for (i32 i = 0; i < a->args.n; i ++) is_subtype(a->args[i]->type, fields[i].second);
                for (AST* arg : a->args) typecheck(mod, env, arg);
                for (i32 i = 0; i < a->args.n; i ++) {
                    AST* arg = coerce(mod, env, a->args[i], fields[i].second);
                    if (!arg) incompatible_ctor_param_error(mod, a->args[i], concretify(mod, a->args[i]->type), fields[i].second);
                    else a->args[i] = arg;
                }
                t->ctor_called = true;
                break;
            }
            case T_UNION:
                union_ctor_error(mod, a, t);
                a->type = ERROR;
                return;
            default:
                if (a->args.n != 1) {
                    cast_mismatch_error(mod, a, t, a->args.n, 1);
                    break;
                }
                infer(mod, env, a->args[0]);
                typecheck(mod, env, a->args[0]);
                AST* arg = coerce(mod, env, a->args[0], t);
                if (!arg) invalid_cast_error(mod, a->args[0], concretify(mod, a->args[0]->type), t);
                else a->args[0] = arg;
                break;
        }
        a->type = t; // Always produces a value of the constructed type, if successful.
        endmatch
    casematch(AST_ARGS, List*, d) 
        for (AST* ast : d->items) typecheck(mod, env, ast);
        d->type = VOID;
        endmatch
    casematch(AST_DO, List*, d) 
        for (AST* ast : d->items) typecheck(mod, env, ast);
        if (d->items.n) d->type = d->items[d->items.n - 1]->type;
        else d->type = VOID;
        endmatch
    casematch(AST_IF, If*, i) 
        typecheck(mod, i->env, i->cond);
        AST* cond = coerce(mod, i->env, i->cond, BOOL);
        if (!cond) non_boolean_type_error(mod, i->cond, concretify(mod, i->cond->type));
        else i->cond = cond;
        typecheck(mod, i->env, i->ifTrue);
        if (i->ifFalse) typecheck(mod, i->env, i->ifFalse);
        i->type = VOID;
        endmatch
    casematch(AST_WHILE, Loop*, l) 
        typecheck(mod, l->env, l->cond);
        AST* cond = coerce(mod, l->env, l->cond, BOOL);
        if (!cond) non_boolean_type_error(mod, l->cond, concretify(mod, l->cond->type));
        else l->cond = cond;
        typecheck(mod, l->env, l->body);
        l->type = VOID;
        endmatch
    casematch(AST_UNTIL, Loop*, l) 
        typecheck(mod, l->env, l->cond);
        AST* cond = coerce(mod, l->env, l->cond, BOOL);
        if (!cond) non_boolean_type_error(mod, l->cond, concretify(mod, l->cond->type));
        else l->cond = cond;
        typecheck(mod, l->env, l->body);
        l->type = VOID;
        endmatch
    casematch(AST_FOR, For*, f) 
        typecheck(mod, f->env, f->body);
        f->type = VOID;
        endmatch
    casematch(AST_DEFER, Unary*, u) 
        u->type = VOID;
        endmatch
    casematch(AST_RETURN, Unary*, u) 
        Env* fenv = env;
        while (fenv->kind != ENV_FUN && fenv->kind != ENV_ROOT && fenv->kind != ENV_TYPE) fenv = fenv->parent;
        if (fenv->kind != ENV_FUN) {
            return_outside_fun_error(mod, u, fenv);
            return;
        }
        Entry* e = fenv->parent->lookup(fenv->name);
        if (!e || e->kind != E_FUN) unreachable("Could not find definition for enclosing function.");
        Type* ret = e->type;
        if (ret->kind != T_FUN) unreachable("Function declaration somehow isn't a function type.");
        ret = ((FunType*)ret)->ret;
        if (u->child && ret->kind != T_VOID) {
            typecheck(mod, env, u->child);
            AST* child = coerce(mod, env, u->child, ret);
            if (!child) {
                incompatible_return_error(mod, u->child, concretify(mod, u->child->type), ret);
            }
            else u->child = child;
        }
        else if (ret != VOID) incompatible_return_error(mod, u, VOID, ret);
        u->type = VOID;
        endmatch
    
    // Break and continue are unnecessary to typecheck.

    casematch(AST_WITH, With*, w) 
        typecheck(mod, env, w->bound);
        typecheck(mod, w->env, w->body);
        endmatch
    
    // Use and alias are unnecessary to typecheck.

    casematch(AST_TYPEDECL, TypeDecl*, d) 
        if (d->generic) {
            for (const auto& e : d->insts) if (e.value != d->prototype) typecheck(mod, env, e.value);
        }
        else if (d->body) typecheck(mod, d->env, d->body);
        endmatch
    casematch(AST_CASEDECL, CaseDecl*, d)
        if (d->env->kind == ENV_TYPE && d->body) typecheck(mod, d->env, d->body);
        endmatch
    casematch(AST_MATCH, Binary*, b) 
        typecheck(mod, env, b->left);
        List* body = ((List*)b->right);
        Type* ptype = concretify(mod, b->left->type);
        if (ptype->kind == T_UNION) for (AST* a : body->items) {
            CaseDecl* c = (CaseDecl*)a;
            c->env->siblings.push(type_env(ptype));
        }
        Type* result = nullptr;
        for (AST* a : body->items) {
            CaseDecl* c = (CaseDecl*)a;
            detect_types(mod, env, c);
            // print("b = "), format(stdout, mod, b->left->type), print('\n');
            // print("pat = "), format(stdout, mod, c->pattern->type), print('\n');
            Type* unified = unify(c->pattern->type, ptype);
            if (!unified) {
                c->reachable = false;
                continue;
            }
            // for (const auto& e : c->env->entries) {
            //     print("  ", mod->interner->str(e.key), " : "); format(stdout, mod, e.value.type), print('\n');
            // }
            infer(mod, c->env, c->body);
            typecheck(mod, c->env, c->body);
        }
        endmatch
    casematch(AST_MODULEDECL, ModuleDecl*, b) 
        typecheck(mod, b->env, b->body);
        endmatch
    default:
        break;
    }
}

void emit_c_prelude(Module* mod, Env* env, CContext& ctx, const i8* cpath, const i8* hpath) {
    write(ctx.h, "#pragma once\n");
    write(ctx.h, "#include \"cclover.h\"\n");
    write(ctx.h, '\n');
    
    write(ctx.c, "#include \"cclover.h\"\n");
    write(ctx.c, "#include \"", hpath, "\"\n");
    write(ctx.c, '\n');

    I8->mangled = mod->interner->intern("i8");
    I16->mangled = mod->interner->intern("i16");
    I32->mangled = mod->interner->intern("i32");
    I64->mangled = mod->interner->intern("i64");
    INT->mangled = mod->interner->intern("iword");
    IPTR->mangled = mod->interner->intern("iptr");
    ICONST->mangled = mod->interner->intern("iword");
    F32->mangled = mod->interner->intern("f32");
    F64->mangled = mod->interner->intern("f64");
    FLOAT->mangled = mod->interner->intern("f64");
    FCONST->mangled = mod->interner->intern("f64");
    UNIT->mangled = mod->interner->intern("unit");
    CHAR->mangled = mod->interner->intern("int32_t");
    STRING->mangled = mod->interner->intern("string");
    BOOL->mangled = mod->interner->intern("bool_t");
    VOID->mangled = mod->interner->intern("void");
}

i32 env_fq_name(Module* mod, Env* env, CContext& ctx) {
    if (env->fqname != -1) return env->fqname;
    i32 size = 0;
    const_slice<i8> parent((i8*)nullptr, 1);
    if (env->parent && env->parent->kind != ENV_ROOT && (env->parent->kind != ENV_GLOBAL)) {
        parent = mod->interner->str(env_fq_name(mod, env->parent, ctx));
        size += parent.n;
        size += 2;
    } 
    const_slice<i8> name = mod->interner->str(env->name);
    size += name.n;
    i8* fqname = (i8*)mod->envctx->envspace.alloc(size);
    i8* writer = fqname;
    if (env->parent && env->parent->kind != ENV_ROOT && (env->parent->kind != ENV_GLOBAL)) {
        mcpy(writer, parent.ptr, parent.n);
        writer += parent.n;
        *writer ++ = '_';
        *writer ++ = '_';
    } 
    mcpy(writer, name.ptr, name.n);
    for (u32 i = 0; i < size; i ++) if (fqname[i] == '/' || fqname[i] == '\\') fqname[i] = '_';
    env->fqname = mod->interner->intern(const_slice<i8>{fqname, size});
    return env->fqname;
}

void write_sym(stream& io, Module* mod, i32 sym) {
    const const_slice<i8>& str = mod->interner->str(sym);
    write(io, str);
}

inline void indent(stream& stream, int n) {
    while (n > 0) write(stream, ' '), n --;
}

i32 mangle_tvar(Module* mod, Type* type) {
    Type* inner = simplify(*((VarType*)type)->binding);
    if (inner->kind == T_VAR) return inner->mangled = mangle_tvar(mod, inner);
    else return inner->mangled;
}

void emit_c_typename(stream& io, Module* mod, Type* type, CContext& ctx) {
    type = simplify(type);
    if (type->kind == T_VAR) type->mangled = mangle_tvar(mod, type);
    if (type->mangled != -1) write_sym(io, mod, type->mangled);
    else unreachable("Type should have had mangled name by now.");
}

void emit_c_binary(Module* mod, Env* env, AST* left, AST* right, CContext& ctx, const i8* op) {
    emit_c(mod, env, left, ctx);
    write(ctx.c, op);
    emit_c(mod, env, right, ctx);
}

void emit_c_strcmp(Module* mod, Env* env, AST* left, AST* right, CContext& ctx, const i8* op) {
    write(ctx.c, "(__clover__strcmp(");
    emit_c(mod, env, left, ctx);
    write(ctx.c, ", ");
    emit_c(mod, env, right, ctx);
    write(ctx.c, ')', op, ')');
}

void emit_fqsym(stream& io, Module* mod, Env* env, Symbol sym, CContext& ctx) {
    env = env->find(sym);
    if (env && env->kind != ENV_ROOT && env->kind != ENV_GLOBAL) {
        i32 envname = env_fq_name(mod, env, ctx);
        write_sym(io, mod, envname);
        write(io, "__");
    }
    write_sym(io, mod, sym);
}

void emit_c_member(Module* mod, Env* env, AST* ast, CContext& ctx);

void emit_c_placement_new(Module* mod, Type* t, CContext& ctx) {
    indent(ctx.h, ctx.h_indent);
    write(ctx.h, "inline ");
    emit_c_typename(ctx.h, mod, t, ctx);
    write(ctx.h, "* ");
    emit_c_typename(ctx.h, mod, t, ctx);
    write(ctx.h, "__pnew(");
    emit_c_typename(ctx.h, mod, t, ctx);
    write(ctx.h, "* out, ");
    emit_c_typename(ctx.h, mod, t, ctx);
    write(ctx.h, " in) { *out = in; return out; }\n");
}

void emit_c_array_new(Module* mod, Type* t, Type* e, CContext& ctx) {
    indent(ctx.h, ctx.h_indent);
    write(ctx.h, "inline ");
    emit_c_typename(ctx.h, mod, t, ctx);
    write(ctx.h, " ");
    emit_c_typename(ctx.h, mod, t, ctx);
    write(ctx.h, "__anew(");
    emit_c_typename(ctx.h, mod, e, ctx);
    write(ctx.h, " in, intptr_t size) {\n");
    ctx.h_indent += 4;
    indent(ctx.h, ctx.h_indent), emit_c_typename(ctx.h, mod, e, ctx), write(ctx.h, "* ptr = (");
    emit_c_typename(ctx.h, mod, e, ctx), write(ctx.h, "*)__clover__malloc(sizeof(");
    emit_c_typename(ctx.h, mod, e, ctx), write(ctx.h, ") * size);\n");
    indent(ctx.h, ctx.h_indent), write(ctx.h, "for (intptr_t i = 0; i < size; i ++) { ");
    emit_c_typename(ctx.h, mod, e, ctx);
    write(ctx.h, "__pnew(ptr + i, in); }\n");
    indent(ctx.h, ctx.h_indent), write(ctx.h, "return (");
    emit_c_typename(ctx.h, mod, t, ctx);
    write(ctx.h, ") { ptr, size };\n");
    ctx.h_indent -= 4;
    indent(ctx.h, ctx.h_indent), write(ctx.h, "}\n");
}

void emit_c_typedef(Module* mod, Type* t, CContext& ctx) {
    i32 len = 0;
    t = simplify(t);
    Env* tenv = type_env(t);
    bool is_case = t->is_case;
    if (is_case && tenv->parent->decl && tenv->parent->decl->type->mangled == -1) {
        emit_c_typedef(mod, tenv->parent->decl->type, ctx);
        if (t->mangled != -1) return;
    }
    switch (t->kind) {
        case T_UNIT:    // All primitives can only be defined in root and are given names in the prelude.
        case T_VOID:
        case T_BOOL:
        case T_CHAR:
        case T_STRING:
        case T_NUMERIC:
            break;
        case T_VAR: {
            Type* binding = *((VarType*)t)->binding;
            if (binding->mangled == -1) emit_c_typedef(mod, binding, ctx);
            t->mangled = binding->mangled;
            break;
        }
        case T_PTR: {
            Type* target = ((PtrType*)t)->target;
            if (target->mangled == -1) emit_c_typedef(mod, target, ctx);
            const const_slice<i8>& target_name = mod->interner->str(target->mangled);
            i32 newlen = target_name.size() + 3;
            i8* name = (i8*)mod->typectx->typespace.alloc(newlen);
            mcpy(name, target_name.ptr, target_name.size());
            mcpy(name + target_name.size(), "__P", 3);
            t->mangled = mod->interner->intern(const_slice<i8>{name, newlen});
            indent(ctx.h, ctx.h_indent); write(ctx.h, "typedef ");
            emit_c_typename(ctx.h, mod, target, ctx);
            write(ctx.h, "* ");
            emit_c_typename(ctx.h, mod, t, ctx);
            write(ctx.h, ";\n");
            break;
        }
        case T_SLICE: {
            Type* element = ((SliceType*)t)->element;
            if (element->mangled == -1) emit_c_typedef(mod, element, ctx);
            const const_slice<i8>& element_name = mod->interner->str(element->mangled);
            i32 newlen = element_name.size() + 3; 
            i8* name = (i8*)mod->typectx->typespace.alloc(newlen);
            mcpy(name, element_name.ptr, element_name.size());
            mcpy(name + element_name.size(), "__S", 3);
            t->mangled = mod->interner->intern(const_slice<i8>{name, newlen});

            if (element != I8) { // Byte slices are defined in cclover.h
                write(ctx.h, "__clover__slice_def(");
                emit_fqsym(ctx.h, mod, type_env(element)->parent, type_env(element)->name, ctx);
                write(ctx.h, ", ");
                emit_c_typename(ctx.h, mod, element, ctx);
                write(ctx.h, ");\n");
            }
            break;
        }
        case T_ARRAY: {
            Type* element = ((ArrayType*)t)->element;
            if (element->mangled == -1) emit_c_typedef(mod, element, ctx);
            const const_slice<i8>& element_name = mod->interner->str(element->mangled);
            i32 newlen = element_name.size() + 28;
            i32 array_name_len = element_name.size() + 5;
            i8* name = (i8*)mod->typectx->typespace.alloc(newlen);
            mcpy(name, element_name.ptr, element_name.size());
            mcpy(name + element_name.size(), "__A__", 5);
            if (!((ArrayType*)t)->size) {
                name[array_name_len ++] = '0';
            }
            else {
                u64 u = ((ArrayType*)t)->size;
                i32 c = 0, d = 0;
                u64 p = 1;
                while (p <= u) p *= 10, ++ c;
                d = c;
                while (c) {
                    name[array_name_len + c - 1] = "0123456789"[u % 10];
                    u /= 10;
                    c --;
                }
                array_name_len += d;
            }
            t->mangled = mod->interner->intern(const_slice<i8>{name, array_name_len});

            write(ctx.h, "__clover__array_def(");
            emit_fqsym(ctx.h, mod, type_env(element)->parent, type_env(element)->name, ctx);
            write(ctx.h, ", ");
            emit_c_typename(ctx.h, mod, element, ctx);
            write(ctx.h, ", ", ((ArrayType*)t)->size, ");\n");
            if (t->ctor_called) {
                write(ctx.h, "inline ");
                write_sym(ctx.h, mod, t->mangled);
                write(ctx.h, " ");
                write_sym(ctx.h, mod, t->mangled);
                write(ctx.h, "_new(");
                for (i32 i = 0; i < ((ArrayType*)t)->size; i ++) {
                    if (i > 0) write(ctx.h, ", ");
                    emit_c_typename(ctx.h, mod, element, ctx);
                    write(ctx.h, " _p", i);
                }
                write(ctx.h, ") {\n");
                write(ctx.h, "    "), emit_c_typename(ctx.h, mod, t, ctx), write(ctx.h, " acc;\n");
                for (i32 i = 0; i < ((ArrayType*)t)->size; i ++) {
                    write(ctx.h, "    acc.ptr[", i, "] = _p", i, ";\n");
                }
                write(ctx.h, "    return acc;\n");
                write(ctx.h, "}\n\n");
            }
            break;
        }
        case T_STRUCT: {
            t->mangled = env_fq_name(mod, t->env, ctx);
            indent(ctx.h, ctx.h_indent), write(ctx.h, "typedef struct ");
            write_sym(ctx.h, mod, t->mangled);
            write(ctx.h, ' ');
            write_sym(ctx.h, mod, t->mangled);
            write(ctx.h, ";\n");
            for (const auto& p : ((StructType*)t)->fields) {
                if (p.second->mangled == -1) emit_c_typedef(mod, p.second, ctx);
            }
            indent(ctx.h, ctx.h_indent); write(ctx.h, "typedef struct ");
            write_sym(ctx.h, mod, t->mangled), write(ctx.h, " {\n");
            ctx.h_indent += 4;
            if (is_case) indent(ctx.h, ctx.h_indent), write(ctx.h, "i32 __tag;\n");
            if (AST* ast = ((TypeDecl*)t->env->decl)->body) {
                if (ast->kind == AST_DO) for (AST* a : ((List*)ast)->items) {
                    indent(ctx.h, ctx.h_indent), emit_c_member(mod, t->env, a, ctx), write(ctx.h, ";\n");
                }
                else indent(ctx.h, ctx.h_indent), emit_c_member(mod, t->env, ast, ctx), write(ctx.h, ";\n");
            }
            ctx.h_indent -= 4;
            indent(ctx.h, ctx.h_indent);write(ctx.h, "} ");
            write_sym(ctx.h, mod, t->mangled);
            write(ctx.h, ";\n");
            if (t->ctor_called) {
                write(ctx.h, "inline ");
                emit_c_typename(ctx.h, mod, t, ctx);
                write(ctx.h, ' ');
                write_sym(ctx.h, mod, t->mangled);
                write(ctx.h, "__new(");
                for (const auto& p : ((StructType*)t)->fields) {
                    if (&p != &((StructType*)t)->fields[0]) write(ctx.h, ", ");
                    emit_c_typename(ctx.h, mod, p.second, ctx);
                    write(ctx.h, ' ');
                    write_sym(ctx.h, mod, p.first);
                    write(ctx.h, "_in");
                }
                write(ctx.h, ") {\n");
                ctx.h_indent += 4;
                indent(ctx.h, ctx.h_indent), emit_c_typename(ctx.h, mod, t, ctx), write(ctx.h, " out;\n");
                if (is_case) indent(ctx.h, ctx.h_indent), write(ctx.h, "out.__tag = ", t->caseid, ";\n");
                for (const auto& p : ((StructType*)t)->fields) {
                    indent(ctx.h, ctx.h_indent), write(ctx.h, "out.");
                    emit_fqsym(ctx.h, mod, t->env, p.first, ctx);
                    write(ctx.h, " = ");
                    write_sym(ctx.h, mod, p.first);
                    write(ctx.h, "_in;\n");
                }
                indent(ctx.h, ctx.h_indent), write(ctx.h, "return out;\n");
                ctx.h_indent -= 4;
                write(ctx.h, "}\n\n");
            }
            break;
        }
        case T_UNION: {
            vec<i32, 256, arena> cases;
            cases.alloc = &mod->typectx->typespace;
            i32 i = 0;
            t->mangled = env_fq_name(mod, t->env, ctx);
            indent(ctx.h, ctx.h_indent), write(ctx.h, "typedef struct ");
            write_sym(ctx.h, mod, t->mangled);
            write(ctx.h, ' ');
            write_sym(ctx.h, mod, t->mangled);
            write(ctx.h, ";\n");
            for (auto& e : t->env->entries) {
                if (e.value.kind == E_CASE) {
                    if (e.value.type->mangled == -1) emit_c_typedef(mod, e.value.type, ctx);
                    cases.push(e.value.type->mangled);
                }
            }
            indent(ctx.h, ctx.h_indent); write(ctx.h, "typedef enum {\n");
            ctx.h_indent += 4;
            for (i32 i : cases) 
                indent(ctx.h, ctx.h_indent), emit_fqsym(ctx.h, mod, t->env, i, ctx), write(ctx.h, "__tag,\n");
            indent(ctx.h, ctx.h_indent), write_sym(ctx.h, mod, t->env->name), write(ctx.h, "__MAX_TAG = ", 0x7fffffff, '\n'); // Force size to int.
            ctx.h_indent -= 4;
            indent(ctx.h, ctx.h_indent); write(ctx.h, "} ");
            write_sym(ctx.h, mod, env_fq_name(mod, t->env, ctx));
            write(ctx.h, "__tag;\n");
            indent(ctx.h, ctx.h_indent); write(ctx.h, "typedef struct ");
            write_sym(ctx.h, mod, t->mangled), write(ctx.h, " {\n");
            ctx.h_indent += 4;
            if (is_case) indent(ctx.h, ctx.h_indent), write(ctx.h, "i32 __tag;\n");
            indent(ctx.h, ctx.h_indent); write(ctx.h, "union {\n");
            ctx.h_indent += 4;
            indent(ctx.h, ctx.h_indent);
            write_sym(ctx.h, mod, env_fq_name(mod, t->env, ctx));
            write(ctx.h, "__tag __case;\n");
            for (i32 i : cases) {
                indent(ctx.h, ctx.h_indent), emit_fqsym(ctx.h, mod, t->env, i, ctx), write(ctx.h, " ");
                emit_fqsym(ctx.h, mod, t->env, i, ctx), write(ctx.h, "__var;\n");
            } 
            ctx.h_indent -= 4;
            indent(ctx.h, ctx.h_indent); write(ctx.h, "};\n");
            ctx.h_indent -= 4;
            indent(ctx.h, ctx.h_indent);
            write(ctx.h, "} ");
            write_sym(ctx.h, mod, t->mangled);
            write(ctx.h, ";\n");
            for (auto& e : t->env->entries) {
                if (e.value.kind == E_CASE) {
                    indent(ctx.h, ctx.h_indent), write(ctx.h, "inline ");
                    emit_c_typename(ctx.h, mod, t, ctx);
                    write(ctx.h, ' ');
                    emit_c_typename(ctx.h, mod, t, ctx);
                    write(ctx.h, "__from__");
                    write_sym(ctx.h, mod, e.value.type->mangled);
                    write(ctx.h, '(');
                    emit_c_typename(ctx.h, mod, e.value.type, ctx);
                    write(ctx.h, " in) {\n");
                    ctx.h_indent += 4;
                    indent(ctx.h, ctx.h_indent), emit_c_typename(ctx.h, mod, t, ctx);
                    write(ctx.h, " out;\n");
                    if (is_case) indent(ctx.h, ctx.h_indent), write(ctx.h, "out.__case = ", t->caseid, ";\n");
                    indent(ctx.h, ctx.h_indent), write(ctx.h, "out."), emit_c_typename(ctx.h, mod, e.value.type, ctx), write(ctx.h, "__var = in;\n");
                    indent(ctx.h, ctx.h_indent), write(ctx.h, "return out;\n");
                    ctx.h_indent -= 4;
                    indent(ctx.h, ctx.h_indent), write(ctx.h, "}\n");
                    Type* c = e.value.type;
                }
            }
            write(ctx.h, '\n');
            break;
        }
        case T_NAMED: {
            t->mangled = env_fq_name(mod, t->env, ctx);
            indent(ctx.h, ctx.h_indent), write(ctx.h, "typedef struct ");
            write_sym(ctx.h, mod, t->mangled);
            write(ctx.h, ' ');
            write_sym(ctx.h, mod, t->mangled);
            write(ctx.h, ";\n");
            if (((NamedType*)t)->inner->mangled == -1)
                emit_c_typedef(mod, ((NamedType*)t)->inner, ctx);
            indent(ctx.h, ctx.h_indent); write(ctx.h, "typedef struct ");
            write_sym(ctx.h, mod, t->mangled), write(ctx.h, " {\n");

            ctx.h_indent += 4;
            if (is_case) indent(ctx.h, ctx.h_indent), write(ctx.h, "i32 __tag;\n");
            indent(ctx.h, ctx.h_indent);
            emit_c_typename(ctx.h, mod, ((NamedType*)t)->inner, ctx);
            write(ctx.h, ' ');
            emit_fqsym(ctx.h, mod, t->env, mod->interner->intern("this"), ctx);
            write(ctx.h, ";\n");
            ctx.h_indent -= 4;

            indent(ctx.h, ctx.h_indent), write(ctx.h, "} ");
            write_sym(ctx.h, mod, t->mangled);
            write(ctx.h, ";\n");
            write(ctx.h, "inline ");
            emit_c_typename(ctx.h, mod, t, ctx);
            write(ctx.h, ' ');
            write_sym(ctx.h, mod, t->mangled);
            write(ctx.h, "__new(");
            emit_c_typename(ctx.h, mod, ((NamedType*)t)->inner, ctx);
            write(ctx.h, " in) {\n");
            ctx.h_indent += 4;
            indent(ctx.h, ctx.h_indent), emit_c_typename(ctx.h, mod, t, ctx), write(ctx.h, " out;\n");
            if (is_case) indent(ctx.h, ctx.h_indent), write(ctx.h, "out.__tag = ", t->caseid, ";\n");
            indent(ctx.h, ctx.h_indent), write(ctx.h, "out."), emit_fqsym(ctx.h, mod, t->env, mod->interner->intern("this"), ctx);
            write(ctx.h, " = in;\n");
            indent(ctx.h, ctx.h_indent), write(ctx.h, "return out;\n");
            ctx.h_indent -= 4;
            write(ctx.h, "}\n\n");
            break;
        }
        case T_FUN: {
            if (!t->referenced_by_name) break;
            if (((FunType*)t)->ret->mangled == -1)
                emit_c_typedef(mod, ((FunType*)t)->ret, ctx);
            int arg_lens = 0;
            for (Type* a : ((FunType*)t)->arg) {
                if (a->mangled == -1) emit_c_typedef(mod, a, ctx);
                arg_lens += mod->interner->str(a->mangled).n + 2;
            }

            const const_slice<i8>& return_name = mod->interner->str(((FunType*)t)->ret->mangled);
            i32 newlen = return_name.size() + arg_lens + 28;
            i32 fn_name_len = return_name.size() + 3;
            i8* name = (i8*)mod->typectx->typespace.alloc(newlen);
            mcpy(name, return_name.ptr, return_name.size());
            mcpy(name + return_name.size(), "__F", 3);
            if (!((FunType*)t)->arg.n) {
                name[fn_name_len ++] = '0';
            }
            else {
                u64 u = ((FunType*)t)->arg.n;
                i32 c = 0, d = 0;
                u64 p = 1;
                while (p <= u) p *= 10, ++ c;
                d = c;
                while (c) {
                    name[fn_name_len ++] = "0123456789"[u % 10];
                    u /= 10;
                    c --;
                }
            }
            for (Type* a : ((FunType*)t)->arg) {
                name[fn_name_len ++] = '_';
                name[fn_name_len ++] = '_';
                const const_slice<i8>& arg_name = mod->interner->str(a->mangled);
                mcpy(name + fn_name_len, arg_name.ptr, arg_name.n);
                fn_name_len += arg_name.n;
            }
            t->mangled = mod->interner->intern(const_slice<i8>{name, fn_name_len});

            indent(ctx.h, ctx.h_indent); write(ctx.h, "typedef ");
            emit_c_typename(ctx.h, mod, ((FunType*)t)->ret, ctx);
            write(ctx.h, " (*");
            write_sym(ctx.h, mod, ((FunType*)t)->mangled);
            write(ctx.h, ")(");
            bool first = true;
            for (Type* const& a : ((FunType*)t)->arg) {
                if (!first) write(ctx.h, ", ");
                if (a != UNIT) {
                    emit_c_typename(ctx.h, mod, a, ctx);
                    first = false;
                }
            }
            write(ctx.h, ");\n");

            break;
        }
        default:
            break;
    }
}

bool is_prototype(Type* type) {
    return type->env && type->env->decl && type->env->decl->kind == AST_TYPEDECL && ((TypeDecl*)type->env->decl)->is_prototype;
}

void emit_c_types(Module* mod, Env* env, CContext& ctx) {
    for (Module* dep : mod->deps) {
        write(ctx.h, "#include \"", (const i8*)dep->hpath, "\"\n");
    }
    map<TypeKey, Type*> concrete_types;
    map<TypeKey, Type*> type_manglings;
    for (auto& entry : mod->typectx->typemap) {
        Type* t = fullsimplify(*mod->typectx, entry.value);
        if (!isconcrete(t)) continue;

        t->ctor_called = t->ctor_called || entry.value->ctor_called;
        t->referenced_by_name = t->referenced_by_name || entry.value->referenced_by_name;
        t->gen_placement_new = t->gen_placement_new || entry.value->gen_placement_new;
        auto it = concrete_types.find(t);
        if (concrete_types.find(t) == concrete_types.end()) {
            concrete_types.put(t, t); 
            type_manglings.put(entry.value, t);
        }
        else type_manglings.put(entry.value, it->value);
    }
    for (auto& entry : concrete_types) if (entry.value->mangled == -1 && !entry.value->is_case && !is_prototype(entry.value)) emit_c_typedef(mod, entry.value, ctx);
    for (auto& entry : type_manglings) if (entry.key.type->mangled == -1) entry.key.type->mangled = entry.value->mangled;
    for (Type* t : mod->cnew_types) if (t->gen_placement_new) emit_c_placement_new(mod, t, ctx);
    for (const auto& entry : concrete_types) {
        if (entry.value->kind == T_SLICE 
            && concrete_types.find(((SliceType*)entry.value)->element) != concrete_types.end()
            && concrete_types[((SliceType*)entry.value)->element]->gen_placement_new)
            emit_c_array_new(mod, entry.value, ((SliceType*)entry.value)->element, ctx);
    }
}

void emit_c_member(Module* mod, Env* env, AST* ast, CContext& ctx) {
    switch (ast->kind) {
    casematch(AST_DO, List*, l)
        for (AST* ast : l->items) {
            if (ast != l->items[0]) indent(ctx.h, ctx.h_indent);
            emit_c_member(mod, env, ast, ctx);
            if (ast != l->items[l->items.n - 1]) write(ctx.h, ";\n");
        }
        endmatch
    casematch(AST_VARDECL, VarDecl*, d)
        // Prototype
        if (env->kind == ENV_GLOBAL) write(ctx.h, "extern ");
        emit_c_typename(ctx.h, mod, d->type, ctx);
        write(ctx.h, ' ');
        emit_fqsym(ctx.h, mod, env, d->basename, ctx);
        endmatch
    casematch(AST_PTRDECL, Binary*, b)
        emit_c_member(mod, env, b->left, ctx);
        endmatch
    default:
        break;
    }
}

void emit_c_toplevel(Module* mod, Env* env, AST* ast, CContext& ctx, vec<pair<AST*, Env*>, 64, arena>& main);

void emit_c_static_member(Module* mod, Env* env, AST* ast, CContext& ctx, vec<pair<AST*, Env*>, 64, arena>& main) {
    switch (ast->kind) {
    casematch(AST_DO, List*, l)
        for (AST* ast : l->items) {
            if (ast != l->items[0]) indent(ctx.h, ctx.h_indent);
            emit_c_static_member(mod, env, ast, ctx, main);
            if (ast != l->items[l->items.n - 1]) write(ctx.h, ";\n");
        }
        endmatch
    casematch(AST_CASEDECL, CaseDecl*, d)
        emit_c_toplevel(mod, d->env, d, ctx, main);
        endmatch
    casematch(AST_FUNDECL, FunDecl*, d)
        emit_c_toplevel(mod, d->env, d, ctx, main);
        endmatch
    casematch(AST_TYPEDECL, TypeDecl*, d)
        emit_c_toplevel(mod, d->env, d, ctx, main);
        endmatch
    casematch(AST_MODULEDECL, ModuleDecl*, d)
        emit_c_toplevel(mod, d->env, d, ctx, main);
        endmatch
    default:
        break;
    }
}

void emit_c_toplevel(Module* mod, Env* env, AST* ast, CContext& ctx, vec<pair<AST*, Env*>, 64, arena>& main) {
    if (ast->kind >= AST_FIRST_DECL && ast->kind < AST_LAST_DECL) {    
        if (ast->kind == AST_MODULEDECL) {
            emit_c_toplevel(mod, ((ModuleDecl*)ast)->env, ((ModuleDecl*)ast)->body, ctx, main);
        }
        else if (ast->kind == AST_PTRDECL) {
            emit_c_toplevel(mod, env, ((Binary*)ast)->left, ctx, main);
            return;
        }
        else {
            if (ast->kind == AST_FUNDECL && ((FunDecl*)ast)->generic) {
                for (const auto& e : ((FunDecl*)ast)->insts) emit_c_toplevel(mod, env, e.value, ctx, main);
                return;
            }
            if (ast->kind == AST_TYPEDECL && ((TypeDecl*)ast)->generic) {
                for (const auto& e : ((TypeDecl*)ast)->insts) if (e.value != ((TypeDecl*)ast)->prototype) emit_c_toplevel(mod, env, e.value, ctx, main);
                return;
            }
            if (ast->kind == AST_FUNDECL && ((FunDecl*)ast)->body && !((FunDecl*)ast)->generic)
                emit_c_static_member(mod, ((FunDecl*)ast)->env, ((FunDecl*)ast)->body, ctx, main);
            if (ast->kind == AST_CASEDECL && ((CaseDecl*)ast)->env->kind == ENV_TYPE && ((CaseDecl*)ast)->body) 
                return emit_c_static_member(mod, ((CaseDecl*)ast)->env, ((CaseDecl*)ast)->body, ctx, main);
            if (ast->kind == AST_TYPEDECL && ((TypeDecl*)ast)->body && !((TypeDecl*)ast)->generic) 
                return emit_c_static_member(mod, ((TypeDecl*)ast)->env, ((TypeDecl*)ast)->body, ctx, main);
            emit_c(mod, env, ast, ctx), write(ctx.c, ";\n"), write(ctx.h, ";\n");
            if (ast->kind == AST_VARDECL && ((VarDecl*)ast)->init) {
                AST* assign = new(mod->parser->astspace) Binary(AST_ASSIGN, ast->pos, ((VarDecl*)ast)->name, ((VarDecl*)ast)->init);
                assign->type = VOID;
                main.push({ assign, env });
            }
        }
    }
    else if (ast->kind == AST_DO) {
        for (AST* ast : ((List*)ast)->items) emit_c_toplevel(mod, env, ast, ctx, main);
    }
    else if (mod == ctx.main) main.push({ ast, env });
}

void emit_c_type_pattern(Module* mod, Env* env, Type* match, AST* val, CContext& ctx) {
    Type* vt = fullconcrete(*mod->typectx, val->type);
    if (is_subtype_generic(match, vt)) {
        write(ctx.c, "1");
    }
    else if (vt->kind == T_UNION) {
        i64 i = -1;
        for (const auto& f : ((UnionType*)vt)->fields) {
            // format(stdout, mod, f.second), print(' '), format(stdout, mod, match), print('\n');
            if (is_subtype_generic(f.second, match)) {
                i = &f - ((UnionType*)vt)->fields.ptr;
                break;
            }
        }
        if (i < 0) write(ctx.c, "0");
        else {
            write(ctx.c, '('), emit_c(mod, env, val, ctx), write(ctx.c, '.'), write(ctx.c, "__case == ", i, ')');
        }
    }
    else write(ctx.c, "0");
}

void emit_c_pattern_match(Module* mod, Env* env, AST* ast, AST* val, CContext& ctx) {
    switch (ast->kind) {
    case AST_ICONST:
    case AST_FCONST:
    case AST_CHCONST:
        write(ctx.c, '(');
        emit_c(mod, env, ast, ctx);
        write(ctx.c, " == ");
        emit_c(mod, env, val, ctx);
        write(ctx.c, ')');
        break;
    casematch(AST_STRCONST, Const*, c)
        write(ctx.c, "!strcmp(");
        emit_c(mod, env, ast, ctx);
        write(ctx.c, ", ");
        emit_c(mod, env, val, ctx);
        write(ctx.c, ')');
        endmatch
    casematch(AST_BOOL, Const*, c)
        write(ctx.c, c->bconst ? "" : "!");
        emit_c(mod, env, val, ctx);
        endmatch
    casematch(AST_UNIT, Const*, c)
        write(ctx.c, "1");
        endmatch
    casematch(AST_VAR, Var*, v)
        write(ctx.c, "1");
        endmatch
    casematch(AST_PTRDECL, Binary*, b)
        emit_c_pattern_match(mod, env, b->left, val, ctx);
        endmatch
    casematch(AST_VARDECL, VarDecl*, d)
        emit_c_type_pattern(mod, env, d->type, val, ctx);
        endmatch
    casematch(AST_APPLY, Apply*, a)
        Type* match_type = a->fn->type;
        write(ctx.c, '(');
        emit_c_type_pattern(mod, env, match_type, val, ctx);
        switch (match_type->kind) {
            case T_NAMED:
                write(ctx.c, " && ");
                emit_c_pattern_match(mod, env, a->args[0], val, ctx);
                break;
            case T_STRUCT:
                for (u32 i = 0; i < ((StructType*)match_type)->fields.n; i ++) {
                    write(ctx.c, " && ");
                    emit_c_pattern_match(mod, env, a->args[i], val, ctx);
                }
                break;
            default:
                unreachable("Can only match named or struct type.");
                break;
        }
        write(ctx.c, ')');
        endmatch
    casematch(AST_ARRAY, List*, l)
        Type* match_type = l->type;
        Type* vt = fullconcrete(*mod->typectx, val->type);
        bool endslice = l->items.n && l->items[l->items.n - 1]->kind == AST_BITOR;
        write(ctx.c, '(');
        if (vt->kind == T_ARRAY) {
            bool valid = endslice ? ((ArrayType*)vt)->size >= l->items.n : ((ArrayType*)vt)->size == l->items.n;
            write(ctx.c, valid ? "1" : "0");
        }
        else if (vt->kind == T_SLICE) {
            write(ctx.c, '(');
            emit_c(mod, env, val, ctx);
            write(ctx.c, ".size", endslice ? " == " : " >= ", l->items.n, ')');
        }
        Binary* idx = new(mod->parser->astspace) Binary(AST_INDEX, val->pos, val, new(mod->parser->astspace) Const(val->pos, (i64)0));
        for (AST* n : l->items) {
            if (n->kind == AST_BITOR) {
                write(ctx.c, " && ");
                emit_c_pattern_match(mod, env, ((Binary*)n)->left, idx, ctx);
                ((Const*)idx->right)->iconst ++;
                Slice* slice = new(mod->parser->astspace) Slice(val->pos, val, idx->right, nullptr);
                write(ctx.c, " && ");
                emit_c_pattern_match(mod, env, ((Binary*)n)->right, slice, ctx);
            }
            else {
                write(ctx.c, " && ");
                emit_c_pattern_match(mod, env, n, idx, ctx);
                ((Const*)idx->right)->iconst ++;
            }
        }
        write(ctx.c, ')');
        endmatch
    default:
        if (!is_type(ast)) unreachable("Unexpected non-type match expression.");
        emit_c_type_pattern(mod, env, ast->type, val, ctx);
        break;
    }
}

void emit_c_pattern_defs(Module* mod, Env* env, AST* ast, AST* val, CContext& ctx) {
    switch (ast->kind) {
    case AST_ICONST:
    case AST_FCONST:
    case AST_CHCONST:
    case AST_STRCONST:
    case AST_BOOL:
    case AST_UNIT:
        break;
    casematch(AST_VAR, Var*, v)
        emit_c_typename(ctx.c, mod, ast->type, ctx);
        write(ctx.c, ' ');
        emit_c(mod, env, v, ctx);
        write(ctx.c, " = ");
        emit_c(mod, env, val, ctx);
        endmatch
    casematch(AST_VARDECL, VarDecl*, d)
        emit_c_typename(ctx.c, mod, d->type, ctx);
        write(ctx.c, ' ');
        emit_c(mod, env, d->name, ctx);
        write(ctx.c, " = ");
        AST* conv = new(mod->parser->astspace) Unary(AST_CONV, val->pos, val);
        conv->type = d->type;
        emit_c(mod, env, conv, ctx);
        endmatch
    casematch(AST_PTRDECL, Binary*, b)
        emit_c_pattern_defs(mod, env, b->left, val, ctx);
        endmatch
    casematch(AST_APPLY, Apply*, a)
        Type* match_type = a->fn->type;

        const_slice<i8> casename = mod->interner->str(match_type->mangled);
        slice<i8> newname = { new(mod->envctx->envspace) i8[casename.n + 5], casename.n + 5 };
        mcpy(newname.ptr, casename.ptr, casename.n);
        mcpy(newname.ptr + casename.n, "__var", 5);
        i32 newfield = mod->interner->intern(newname);
        Binary* casefield = new(mod->parser->astspace) Binary(
            AST_DOT, val->pos,
            val->clone(mod->parser->astspace),
            new(mod->parser->astspace) Var(val->pos, newfield)
        );
        casefield->type = casefield->left->type = match_type;
        casefield->right->type = match_type;
        switch (match_type->kind) {
            case T_NAMED: {
                Binary* dot = new(mod->parser->astspace) Binary(
                    AST_DOT, val->pos, 
                    casefield, 
                    new(mod->parser->astspace) Var(val->pos, mod->interner->intern("this"))
                );
                dot->right->type = ((NamedType*)match_type)->inner;
                // format(stdout, mod, dot), print('\n');
                emit_c_pattern_defs(mod, env, a->args[0], dot, ctx);
                break;
            }
            case T_STRUCT:
                for (u32 i = 0; i < ((StructType*)match_type)->fields.n; i ++) {
                    if (i > 0) write(ctx.c, ";\n"), indent(ctx.c, ctx.c_indent);
                    Binary* dot = new(mod->parser->astspace) Binary(
                        AST_DOT, val->pos, 
                        casefield, 
                        new(mod->parser->astspace) Var(val->pos, ((StructType*)match_type)->fields[i].first)
                    );
                    dot->type = dot->right->type = ((StructType*)match_type)->fields[i].second;
                    emit_c_pattern_defs(mod, env, a->args[i], dot, ctx);
                }
                break;
            default:
                unreachable("Can only match named or struct type.");
                break;
        }
        endmatch
    casematch(AST_ARRAY, List*, l)
        Type* match_type = l->type;
        Type* vt = fullconcrete(*mod->typectx, val->type);
        if (vt->kind == T_ARRAY && ((ArrayType*)vt)->size < l->items.n) break;
        bool endslice = l->items.n && l->items[l->items.n - 1]->kind == AST_BITOR;
        Binary* idx = new(mod->parser->astspace) Binary(AST_INDEX, val->pos, val, new(mod->parser->astspace) Const(val->pos, (i64)0));
        for (AST* n : l->items) {
            if (n != l->items[0]) write(ctx.c, ";\n"), indent(ctx.c, ctx.c_indent);
            if (n->kind == AST_BITOR) {
                emit_c_pattern_defs(mod, env, ((Binary*)n)->left, idx, ctx);
                ((Const*)idx->right)->iconst ++;
                Slice* slice = new(mod->parser->astspace) Slice(val->pos, val, idx->right, nullptr);
                write(ctx.c, ";\n"), indent(ctx.c, ctx.c_indent);
                emit_c_pattern_defs(mod, env, ((Binary*)n)->right, slice, ctx);
            }
            else {
                emit_c_pattern_defs(mod, env, n, idx, ctx);
                ((Const*)idx->right)->iconst ++;
            }
        }
        endmatch
    default:
        if (!is_type(ast)) unreachable("Unexpected non-type match expression.");
        break;
    }
}

void toposort_dfs(Module* m, vec<Module*, 64, arena>& toposort) {
    if (m->mark == MARK_PERM) return;
    if (m->mark == MARK_TEMP) fatal("Circular dependencies detected.");

    m->mark = MARK_TEMP;
    for (Module* m : m->deps) toposort_dfs(m, toposort);

    m->mark = MARK_PERM;
    toposort.push(m);
}

void emit_c_inits(Module* mod, CContext& ctx) {
    vec<Module*, 64, arena> toposort;
    toposort.alloc = &ctx.textspace;
    toposort_dfs(mod, toposort);

    for (Module* m : toposort) {
        indent(ctx.c, ctx.c_indent), write(ctx.c, "__init_"); 
        i32 sym = env_fq_name(mod, m->parser->program->env, ctx);
        write_sym(ctx.c, m, sym); 
        write(ctx.c, "();\n");
    }
    for (i64 i = toposort.size() - 1; i >= 0; i --) {
        indent(ctx.c, ctx.c_indent), write(ctx.c, "__deinit_"); 
        i32 sym = env_fq_name(mod, toposort[i]->parser->program->env, ctx);
        write_sym(ctx.c, toposort[i], sym); 
        write(ctx.c, "();\n");
    }
}

void emit_c(Module* mod, Env* env, AST* ast, CContext& ctx) {
    switch (ast->kind) {
    casematch(AST_PROGRAM, ASTProgram*, p)
        vec<pair<AST*, Env*>, 64, arena> main;
        main.alloc = &ctx.textspace;
        for (AST* ast : p->toplevel) emit_c_toplevel(mod, env, ast, ctx, main);
        for (AST* ast : mod->automethods) indent(ctx.c, ctx.c_indent), indent(ctx.h, ctx.h_indent), emit_c(mod, env, ast, ctx), write(ctx.c, ";\n"), write(ctx.h, ";\n");
        write(ctx.h, "extern void __init_"), write_sym(ctx.h, mod, env_fq_name(mod, env, ctx)), write(ctx.h, "();\n");
        write(ctx.h, "extern void __deinit_"), write_sym(ctx.h, mod, env_fq_name(mod, env, ctx)), write(ctx.h, "();\n");
        write(ctx.c, "void __init_"), write_sym(ctx.c, mod, env_fq_name(mod, env, ctx)), write(ctx.c, "() {\n");
        ctx.c_indent += 4;
        for (const auto& p : main) indent(ctx.c, ctx.c_indent), emit_c(mod, p.second, p.first, ctx), write(ctx.c, ";\n");
        ctx.c_indent -= 4;
        write(ctx.c, "}\n");
        write(ctx.c, "void __deinit_"), write_sym(ctx.c, mod, env_fq_name(mod, env, ctx)), write(ctx.c, "() {\n");
        ctx.c_indent += 4;
        for (AST* ast : p->defers) indent(ctx.c, ctx.c_indent), emit_c(mod, p->deferred, ast, ctx), write(ctx.c, ";\n");
        ctx.c_indent -= 4;
        write(ctx.c, "}\n");
        if (mod == mod->cloverinst->main) {
            write(ctx.c, "int main(int argc, char** argv) {\n");
            ctx.c_indent += 4;
            indent(ctx.c, ctx.c_indent), write(ctx.c, "__clover__core_init();\n");
            emit_c_inits(mod, ctx);
            indent(ctx.c, ctx.c_indent), write(ctx.c, "__clover__core_deinit();\n");
            indent(ctx.c, ctx.c_indent), write(ctx.c, "return 0;\n");
            ctx.c_indent -= 4;
            write(ctx.c, "}\n");
        }
        endmatch
    casematch(AST_ICONST, Const*, c) write(ctx.c, c->iconst); endmatch
    casematch(AST_FCONST, Const*, c) write(ctx.c, c->fconst); endmatch
    casematch(AST_BOOL, Const*, c) write(ctx.c, c->bconst ? '1' : '0'); endmatch
    casematch(AST_CHCONST, Const*, c) write(ctx.c, (i32)c->chconst); endmatch
    casematch(AST_STRCONST, Const*, c) write(ctx.c, "(string){\"", const_slice<i8>{ mod->bytes.text + c->strconst.byteidx, c->strconst.len }, "\", ", c->strconst.len, "}"); endmatch
    casematch(AST_UNIT, Const*, c) write(ctx.c, '0'); endmatch
    casematch(AST_MODULENAME, Var*, v)
        emit_fqsym(ctx.c, mod, env, v->name, ctx);
        endmatch
    casematch(AST_VAR, Var*, v)
        // print("looking up var ", mod->interner->str(v->name), " in "), env->format(stdout, mod, 0);
        Entry* e = env->lookup(v->name);
        if (e && e->kind == E_CONST) {
            Fold f = ((ConstDecl*)e->ast)->fold;
            switch (f.kind) {
                case Fold::ICONST: return write(ctx.c, f.iconst);
                case Fold::BOOL: return write(ctx.c, f.bconst ? '1' : '0');
                case Fold::FCONST: return write(ctx.c, f.fconst);
                case Fold::CHCONST: return write(ctx.c, (i32)f.chconst);
                case Fold::NONE: break;
            }
        }
        Env* parent = env->find(v->name);
        if (parent && parent->kind == ENV_TYPE && parent != env) {
            emit_fqsym(ctx.c, mod, env, mod->interner->intern("this"), ctx);
            write(ctx.c, '.');
        }
        emit_fqsym(ctx.c, mod, env, v->name, ctx);
        endmatch
    casematch(AST_PLUS, Unary*, u) write(ctx.c, "(+"); emit_c(mod, env, u->child, ctx); write(ctx.c, ')'); endmatch
    casematch(AST_NEG, Unary*, u) write(ctx.c, "(-"); emit_c(mod, env, u->child, ctx); write(ctx.c, ')'); endmatch
    casematch(AST_DEREF, Unary*, u)
        write(ctx.c, '*'); emit_c(mod, env, u->child, ctx);
        endmatch
    casematch(AST_ADDROF, Unary*, u) 
        write(ctx.c, '&'); emit_c(mod, env, u->child, ctx);
        endmatch
    casematch(AST_NOT, Unary*, u) write(ctx.c, '!'); emit_c(mod, env, u->child, ctx); endmatch
    casematch(AST_BITNOT, Unary*, u) write(ctx.c, '~'); emit_c(mod, env, u->child, ctx); endmatch
    casematch(AST_INCR, Unary*, u) write(ctx.c, "++ "); emit_c(mod, env, u->child, ctx); endmatch
    casematch(AST_DECR, Unary*, u) write(ctx.c, "-- "); emit_c(mod, env, u->child, ctx); endmatch
    casematch(AST_POSTINCR, Unary*, u) emit_c(mod, env, u->child, ctx); write(ctx.c, " ++"); endmatch
    casematch(AST_POSTDECR, Unary*, u) emit_c(mod, env, u->child, ctx); write(ctx.c, " --"); endmatch
    casematch(AST_NEW, Unary*, u)
        write(ctx.c, "((");
        emit_c_typename(ctx.c, mod, u->child->type, ctx);
        write(ctx.c, "*)");
        write(ctx.c, "__clover__new(");
        emit_c_typename(ctx.c, mod, u->child->type, ctx);
        write(ctx.c, ", ");
        emit_c(mod, env, u->child, ctx);
        write(ctx.c, "))");
        endmatch
    casematch(AST_NEWARRAY, Binary*, b)
        write(ctx.c, "__clover__newarray(");
        emit_c_typename(ctx.c, mod, (SliceType*)b->type, ctx);
        write(ctx.c, ", ");
        emit_c(mod, env, b->right, ctx);
        write(ctx.c, ", ");
        emit_c(mod, env, b->left, ctx);
        write(ctx.c, ')');
        endmatch
    casematch(AST_DEL, Unary*, u)
        write(ctx.c, "__clover__del(");
        if (u->child->type->kind == T_SLICE) write(ctx.c, '(');
        emit_c(mod, env, u->child, ctx);
        if (u->child->type->kind == T_SLICE) write(ctx.c, ").ptr");
        write(ctx.c, ')');
        endmatch
    casematch(AST_PAREN, Unary*, u) 
        write(ctx.c, '(');
        emit_c(mod, env, u->child, ctx);
        write(ctx.c, ')');
        endmatch
    casematch(AST_ARRAY, List*, l) 
        if (l->items.n == 0) {
            write(ctx.c, '(');
            emit_c_typename(ctx.c, mod, concretify(mod, l->type), ctx);
            write(ctx.c, "){ 0, 0 }");
            return;
        }
        emit_c_typename(ctx.c, mod, l->type, ctx);
        write(ctx.c, "_new(");
        for (AST* ast : l->items) {
            if (ast != l->items[0]) write(ctx.c, ", ");
            emit_c(mod, env, ast, ctx);
        }
        write(ctx.c, ")");
        endmatch
    casematch(AST_SLICE, Slice*, s)
        Type* arrt = s->array->type;
        if (arrt->kind == T_ARRAY) {
            write(ctx.c, '('); emit_c_typename(ctx.c, mod, s->type, ctx); write(ctx.c, "){(");
            emit_c(mod, env, s->array, ctx), write(ctx.c, ").ptr");
            if (s->low) write(ctx.c, " + "), emit_c(mod, env, s->low, ctx); 
            write(ctx.c, ", ");
            if (s->high && s->low) {
                emit_c(mod, env, s->high, ctx);
                write(ctx.c, " - ");
                emit_c(mod, env, s->low, ctx);
            }
            else if (s->high) emit_c(mod, env, s->high, ctx);
            else if (s->low) write(ctx.c, ((ArrayType*)arrt)->size, " - "), emit_c(mod, env, s->low, ctx);
            else write(ctx.c, ((ArrayType*)arrt)->size);
            write(ctx.c, '}');
        }
        else if (arrt->kind == T_SLICE) {
            write(ctx.c, '('); emit_c_typename(ctx.c, mod, s->type, ctx); write(ctx.c, "){(");
            emit_c(mod, env, s->array, ctx), write(ctx.c, ").ptr");
            if (s->low) write(ctx.c, " + "), emit_c(mod, env, s->low, ctx); 
            write(ctx.c, ", ");
            if (s->high && s->low) {
                emit_c(mod, env, s->high, ctx);
                write(ctx.c, " - ");
                emit_c(mod, env, s->low, ctx);
            }
            else if (s->high) emit_c(mod, env, s->high, ctx);
            else if (s->low) {
                emit_c(mod, env, s->array, ctx);
                write(ctx.c, ".size - ");
                emit_c(mod, env, s->low, ctx);
            }
            else {
                emit_c(mod, env, s->array, ctx);
                write(ctx.c, ".size");
            }
            write(ctx.c, '}');
        }
        endmatch
    casematch(AST_CONV, Unary*, u)
        Type* dest = fullsimplify(*mod->typectx, u->type);
        Type* src = u->child->type;
        // print("casting "), format(stdout, mod, u->child), print(" : "), format(stdout, mod, src), print(" to "), format(stdout, mod, dest), print('\n');
        if (is_subtype_generic(u->child->type, dest)) return emit_c(mod, env, u->child, ctx);
        switch(dest->kind) {
        case T_SLICE:
            if (src->kind == T_ARRAY) {
                write(ctx.c, '('); emit_c_typename(ctx.c, mod, dest, ctx); write(ctx.c, "){(");
                emit_c(mod, env, u->child, ctx);
                write(ctx.c, ").ptr, ", ((ArrayType*)u->child->type)->size, '}');
            }
            break;
        case T_UNION:
            emit_c_typename(ctx.c, mod, dest, ctx);
            write(ctx.c, "__from__");
            emit_c_typename(ctx.c, mod, u->child->type, ctx);
            write(ctx.c, '(');
            emit_c(mod, env, u->child, ctx);
            write(ctx.c, ')');
            break;
        case T_NUMERIC:
        case T_PTR:
        default:
            if (src->kind == T_UNION && dest->is_case && dest->env->parent == src->env) {
                write(ctx.c, "(");
                emit_c(mod, env, u->child, ctx);
                write(ctx.c, ")."), emit_c_typename(ctx.c, mod, dest, ctx), write(ctx.c, "__var");
            }
            else {
                write(ctx.c, "((");
                emit_c_typename(ctx.c, mod, dest, ctx);
                write(ctx.c, ")(");
                emit_c(mod, env, u->child, ctx);
                write(ctx.c, "))");
            }
            break;
        }
        endmatch
    casematch(AST_SIZEOF, Sizeof*, u)
        Type* ct = u->child->type;
        if (is_type(u->child)) {
            Fold f = u->fold;
            if (f.kind != Fold::ICONST) fatal("Somehow folded sizeof to non-integer.");
            write(ctx.c, f.iconst);
        }
        else if (ct->kind == T_ARRAY) write(ctx.c, ((ArrayType*)ct)->size);
        else if (ct->kind == T_SLICE) {
            emit_c(mod, env, u->child, ctx);
            write(ctx.c, ".size");
        }
        endmatch

    casematch(AST_ADD, Binary*, b) emit_c_binary(mod, env, b->left, b->right, ctx, " + "); endmatch
    casematch(AST_SUB, Binary*, b) emit_c_binary(mod, env, b->left, b->right, ctx, " - "); endmatch
    casematch(AST_STAR, Binary*, b) emit_c_binary(mod, env, b->left, b->right, ctx, " * "); endmatch
    casematch(AST_DIV, Binary*, b) emit_c_binary(mod, env, b->left, b->right, ctx, " / "); endmatch
    casematch(AST_MOD, Binary*, b) emit_c_binary(mod, env, b->left, b->right, ctx, " % "); endmatch
    casematch(AST_BITAND, Binary*, b) emit_c_binary(mod, env, b->left, b->right, ctx, " & "); endmatch
    casematch(AST_BITOR, Binary*, b) emit_c_binary(mod, env, b->left, b->right, ctx, " | "); endmatch
    casematch(AST_BITXOR, Binary*, b) emit_c_binary(mod, env, b->left, b->right, ctx, " ^ "); endmatch
    casematch(AST_BITLEFT, Binary*, b) emit_c_binary(mod, env, b->left, b->right, ctx, " << "); endmatch
    casematch(AST_BITRIGHT, Binary*, b) emit_c_binary(mod, env, b->left, b->right, ctx, " >> "); endmatch
    casematch(AST_AND, Binary*, b) emit_c_binary(mod, env, b->left, b->right, ctx, " && "); endmatch
    casematch(AST_OR, Binary*, b) emit_c_binary(mod, env, b->left, b->right, ctx, " || "); endmatch
    casematch(AST_EXP, Binary*, b)
        emit_c(mod, env, b->left, ctx);
        endmatch
    casematch(AST_EQUAL, Binary*, b) 
        if (b->left->type->kind == T_STRING) emit_c_strcmp(mod, env, b->left, b->right, ctx, " == 0");
        else emit_c_binary(mod, env, b->left, b->right, ctx, " == "); 
        endmatch
    casematch(AST_INEQUAL, Binary*, b) 
        if (b->left->type->kind == T_STRING) emit_c_strcmp(mod, env, b->left, b->right, ctx, " != 0");
        else emit_c_binary(mod, env, b->left, b->right, ctx, " != "); 
        endmatch
    casematch(AST_LESS, Binary*, b) 
        if (b->left->type->kind == T_STRING) emit_c_strcmp(mod, env, b->left, b->right, ctx, " < 0");
        else emit_c_binary(mod, env, b->left, b->right, ctx, " < "); 
        endmatch
    casematch(AST_LEQUAL, Binary*, b) 
        if (b->left->type->kind == T_STRING) emit_c_strcmp(mod, env, b->left, b->right, ctx, " <= 0");
        else emit_c_binary(mod, env, b->left, b->right, ctx, " <= "); 
        endmatch
    casematch(AST_GREATER, Binary*, b) 
        if (b->left->type->kind == T_STRING) emit_c_strcmp(mod, env, b->left, b->right, ctx, " > 0");
        else emit_c_binary(mod, env, b->left, b->right, ctx, " > "); 
        endmatch
    casematch(AST_GEQUAL, Binary*, b) 
        if (b->left->type->kind == T_STRING) emit_c_strcmp(mod, env, b->left, b->right, ctx, " >= 0");
        else emit_c_binary(mod, env, b->left, b->right, ctx, " >= "); 
        endmatch
    casematch(AST_ADDEQ, Binary*, b) emit_c_binary(mod, env, b->left, b->right, ctx, " += "); endmatch
    casematch(AST_SUBEQ, Binary*, b) emit_c_binary(mod, env, b->left, b->right, ctx, " -= "); endmatch
    casematch(AST_STAREQ, Binary*, b) emit_c_binary(mod, env, b->left, b->right, ctx, " *= "); endmatch
    casematch(AST_DIVEQ, Binary*, b) emit_c_binary(mod, env, b->left, b->right, ctx, " /= "); endmatch
    casematch(AST_MODEQ, Binary*, b) emit_c_binary(mod, env, b->left, b->right, ctx, " %= "); endmatch
    casematch(AST_BITANDEQ, Binary*, b) emit_c_binary(mod, env, b->left, b->right, ctx, " &= "); endmatch
    casematch(AST_BITOREQ, Binary*, b) emit_c_binary(mod, env, b->left, b->right, ctx, " |= "); endmatch
    casematch(AST_BITXOREQ, Binary*, b) emit_c_binary(mod, env, b->left, b->right, ctx, " ^= "); endmatch
    casematch(AST_BITLEFTEQ, Binary*, b) emit_c_binary(mod, env, b->left, b->right, ctx, " <<= "); endmatch
    casematch(AST_BITRIGHTEQ, Binary*, b) emit_c_binary(mod, env, b->left, b->right, ctx, " >>= "); endmatch
    casematch(AST_ASSIGN, Binary*, b) emit_c_binary(mod, env, b->left, b->right, ctx, " = "); endmatch
    casematch(AST_IS, Is*, i) 
        emit_c_pattern_match(mod, env, i->right, i->left, ctx);
        if (i->external_env) ctx.patn_defs.push({i->right, i->left});
        endmatch

    casematch(AST_DEFER, Unary*, u) 
        endmatch
    
    casematch(AST_DOT, Binary*, b) 
        Env* aenv = ast_env(env, b->left);
        if (aenv && (aenv->kind == ENV_MOD || aenv->kind == ENV_GLOBAL)) {  // Module access
            emit_c(mod, aenv, b->right, ctx);
        }
        else {
            const i8* op = b->left->type->kind == T_PTR ? "->" : ".";
            Env* tenv = b->left->type->kind == T_PTR ? type_env(((PtrType*)b->left->type)->target) : type_env(b->left->type);
            if (!tenv) tenv = env;
            emit_c(mod, env, b->left, ctx);
            write(ctx.c, op);
            emit_c(mod, tenv, b->right, ctx);
        }
        endmatch
    casematch(AST_INDEX, Binary*, b) 
        emit_c(mod, env, b->left, ctx);
        write(ctx.c, ".ptr[");
        emit_c(mod, env, b->right, ctx);
        write(ctx.c, "]");
        endmatch
    casematch(AST_APPLY, Apply*, a)
        if (auto p = call_parent(mod, a)) emit_c(mod, p->second, a->fn, ctx);
        else emit_c(mod, env, a->fn, ctx);
        bool first = true;
        write(ctx.c, '(');
        for (AST* a : a->args) {
            if (!first) write(ctx.c, ", ");

            if (a->type != UNIT) {
                first = false;
                emit_c(mod, env, a, ctx);
            }
        }
        write(ctx.c, ')');
        endmatch
    casematch(AST_CTOR, Apply*, a)
        Type* t = a->fn->type;
        switch (t->kind) {
        case T_SLICE:
            if (a->args.n == 1 && a->args[0]->type->kind == T_STRING) {
                write(ctx.c, "__clover__str_to_bytes(");
                emit_c(mod, env, a->args[0], ctx);
                write(ctx.c, ')');
            }
            else if (a->args.n == 1 && a->args[0]->type->kind == T_SLICE) {
                write(ctx.c, '(');
                emit_c_typename(ctx.c, mod, t, ctx);
                write(ctx.c, "){(");
                emit_c(mod, env, a->args[0], ctx);
                write(ctx.c, ").ptr, ");
                emit_c(mod, env, a->args[1], ctx);
                write(ctx.c, '}');
            }
            else if (a->args.n == 2) {
                write(ctx.c, '(');
                emit_c_typename(ctx.c, mod, t, ctx);
                write(ctx.c, "){");
                emit_c(mod, env, a->args[0], ctx);
                write(ctx.c, ", ");
                emit_c(mod, env, a->args[1], ctx);
                write(ctx.c, '}');
            }
            break;
        case T_PTR:
            if (a->args[0]->type->kind == T_SLICE) {
                write(ctx.c, "((");
                emit_c_typename(ctx.c, mod, t, ctx);
                write(ctx.c, ")(");
                emit_c(mod, env, a->args[0], ctx);
                write(ctx.c, ").ptr)");
            }
            else if (a->args[0]->type == IPTR || a->args[0]->type->kind == T_PTR) {
                write(ctx.c, "((");
                emit_c_typename(ctx.c, mod, t, ctx);
                write(ctx.c, ")(");
                emit_c(mod, env, a->args[0], ctx);
                write(ctx.c, "))");
            }
            break;
        case T_STRING:
            // Must be slice.
            write(ctx.c, "__clover__bytes_to_str(");
            emit_c(mod, env, a->args[0], ctx);
            write(ctx.c, ')');
            break;
        case T_STRUCT:
        case T_NAMED: {
            emit_c_typename(ctx.c, mod, t, ctx);
            write(ctx.c, "__new(");
            bool first = true;
            for (AST* a : a->args) {
                if (!first) write(ctx.c, ", ");
                first = false;
                emit_c(mod, env, a, ctx);
            }
            write(ctx.c, ')');
            break;
        }
        case T_UNION:
            unreachable("Cannot construct union directly.");
            break;
        default:
            write(ctx.c, "((");
            emit_c_typename(ctx.c, mod, t, ctx);
            write(ctx.c, ")(");
            emit_c(mod, env, a->args[0], ctx);
            write(ctx.c, "))");
            break;
        }
        endmatch
    casematch(AST_DO, List*, l)
        for (AST* ast : l->items) {
            if (ast != l->items[0]) indent(ctx.c, ctx.c_indent);
            emit_c(mod, env, ast, ctx);
            if (ast != l->items[l->items.n - 1]) write(ctx.c, ";\n");
        }
        endmatch
    casematch(AST_WITH, With*, w)
        if (is_type(w->bound) || w->bound->kind == AST_MODULENAME) emit_c(mod, w->env, w->body, ctx);
        else unreachable("with expressions are not fully supported yet.");
        endmatch
    casematch(AST_IF, If*, i)
        write(ctx.c, "if (");
        emit_c(mod, env, i->cond, ctx);
        write(ctx.c, ") {\n");
        ctx.c_indent += 4;
        for (auto asts : ctx.patn_defs) {
            indent(ctx.c, ctx.c_indent);
            emit_c_pattern_defs(mod, i->env, asts.first, asts.second, ctx);
            write(ctx.c, ";\n");
        }
        ctx.patn_defs.clear();
        
        indent(ctx.c, ctx.c_indent), emit_c(mod, i->env, i->ifTrue, ctx);
        write(ctx.c, ";\n");
        ctx.c_indent -= 4;
        indent(ctx.c, ctx.c_indent), write(ctx.c, "}");
        if (i->ifFalse) {
            write(ctx.c, '\n');
            indent(ctx.c, ctx.c_indent), write(ctx.c, "else {\n");
            ctx.c_indent += 4;
            indent(ctx.c, ctx.c_indent), emit_c(mod, i->env, i->ifFalse, ctx);
            write(ctx.c, ";\n");
            ctx.c_indent -= 4;
            indent(ctx.c, ctx.c_indent), write(ctx.c, "}");
        }
        endmatch
    casematch(AST_WHILE, Loop*, l)
        write(ctx.c, "while (");
        emit_c(mod, env, l->cond, ctx);
        write(ctx.c, ") {\n");
        ctx.c_indent += 4;
        for (auto asts : ctx.patn_defs) {
            indent(ctx.c, ctx.c_indent);
            emit_c_pattern_defs(mod, l->env, asts.first, asts.second, ctx);
            write(ctx.c, ";\n");
        }
        ctx.patn_defs.clear();
        indent(ctx.c, ctx.c_indent), emit_c(mod, l->env, l->body, ctx);
        write(ctx.c, ";\n");
        ctx.c_indent -= 4;
        indent(ctx.c, ctx.c_indent), write(ctx.c, "}");
        endmatch
    casematch(AST_FOR, For*, f)
        write(ctx.c, "{\n");
        ctx.c_indent += 4;
        indent(ctx.c, ctx.c_indent), emit_c(mod, f->env, f->body, ctx);
        write(ctx.c, ";\n");
        ctx.c_indent -= 4;
        indent(ctx.c, ctx.c_indent), write(ctx.c, "}");
        endmatch
    casematch(AST_UNTIL, Loop*, l)
        write(ctx.c, "while (!(");
        emit_c(mod, env, l->cond, ctx);
        write(ctx.c, ")) {\n");
        ctx.c_indent += 4;
        for (auto asts : ctx.patn_defs) {
            indent(ctx.c, ctx.c_indent);
            emit_c_pattern_defs(mod, l->env, asts.first, asts.second, ctx);
            write(ctx.c, ";\n");
        }
        ctx.patn_defs.clear();
        indent(ctx.c, ctx.c_indent), emit_c(mod, l->env, l->body, ctx);
        write(ctx.c, ";\n");
        ctx.c_indent -= 4;
        indent(ctx.c, ctx.c_indent), write(ctx.c, "}");
        endmatch
    casematch(AST_BREAK, AST*, a)
        write(ctx.c, "break");
        endmatch
    casematch(AST_CONTINUE, AST*, a)
        write(ctx.c, "continue");
        endmatch
    casematch(AST_RETURN, Unary*, u)
        if (u->child) {
            if (concretify(mod, u->child->type) != VOID) write(ctx.c, "return ");
            emit_c(mod, env, u->child, ctx);
        }
        else write(ctx.c, "return");
        endmatch
    casematch(AST_VARDECL, VarDecl*, d)
        // Prototype
        if (env->kind == ENV_GLOBAL || env->kind == ENV_MOD) {
            write(ctx.h, "extern ");
            emit_c_typename(ctx.h, mod, d->type, ctx);
            write(ctx.h, ' ');
            emit_fqsym(ctx.h, mod, env, d->basename, ctx);
        }

        // Implementation
        emit_c_typename(ctx.c, mod, d->type, ctx);
        write(ctx.c, ' ');
        emit_fqsym(ctx.c, mod, env, d->basename, ctx);
        if (d->init && env->kind != ENV_GLOBAL && env->kind != ENV_MOD) {
            write(ctx.c, " = ");
            emit_c(mod, env, d->init, ctx);
        }
        endmatch
    casematch(AST_ALIASDECL, AliasDecl*, d)
        endmatch
    casematch(AST_USE, Binary*, b)
        endmatch
    casematch(AST_PTRDECL, Binary*, b)
        emit_c(mod, env, b->left, ctx);
        endmatch
    casematch(AST_FUNDECL, FunDecl*, d)
        if (d->emitted) return;
        d->emitted = true;
        // Prototype
        if (!d->body) write(ctx.h, "extern ");
        emit_c_typename(ctx.h, mod, ((FunType*)d->type)->ret, ctx);
        slice<AST*>& args = ((Apply*)d->proto)->args;
        write(ctx.h, ' ');
        write_sym(ctx.h, mod, env_fq_name(mod, d->env, ctx));
        write(ctx.h, '(');
        bool first = true;
        for (AST* p : args) {
            if (!first) write(ctx.h, ", ");
            first = false;
            emit_c_typename(ctx.h, mod, p->type, ctx);
            write(ctx.h, " ");
            if (p->kind == AST_PTRDECL) p = ((Binary*)p)->left;
            emit_fqsym(ctx.h, mod, d->env, ((VarDecl*)p)->basename, ctx);
        }
        write(ctx.h, ")");

        if (d->body) {
            // Implementation
            emit_c_typename(ctx.c, mod, ((FunType*)d->type)->ret, ctx);
            write(ctx.c, ' ');
            write_sym(ctx.c, mod, env_fq_name(mod, d->env, ctx));
            write(ctx.c, '(');
            bool first = true;
            for (AST* p : ((Apply*)d->proto)->args) {
                if (!first) write(ctx.c, ", ");
                first = false;
                emit_c_typename(ctx.c, mod, p->type, ctx);
                write(ctx.c, " ");
                if (p->kind == AST_PTRDECL) p = ((Binary*)p)->left;
                emit_fqsym(ctx.c, mod, d->env, ((VarDecl*)p)->basename, ctx);
            }
            write(ctx.c, ") {\n");
            ctx.c_indent += 4;
            indent(ctx.c, ctx.c_indent), emit_c(mod, d->env, d->body, ctx), write(ctx.c, ";\n");
            ctx.c_indent -= 4;
            indent(ctx.c, ctx.c_indent), write(ctx.c, "}");
        }
        endmatch
    casematch(AST_CONSTDECL, ConstDecl*, d)
        endmatch
    casematch(AST_CASEDECL, CaseDecl*, d)
        endmatch
    casematch(AST_TYPEDECL, TypeDecl*, d)
        endmatch
    casematch(AST_MODULEDECL, ModuleDecl*, d)
        emit_c(mod, d->env, d->body, ctx);
        endmatch
    casematch(AST_MATCH, Binary*, b)
        AST* patn = b->left;
        List* l = ((List*)b->right);
        bool first = true;
        write(ctx.c, "{\n");
        ctx.c_indent += 4;
        indent(ctx.c, ctx.c_indent), emit_c_typename(ctx.c, mod, patn->type, ctx); 
        write(ctx.c, " __match = "), emit_c(mod, env, patn, ctx), write(ctx.c, ";\n");
        Var* v = new(mod->parser->astspace) Var(patn->pos, mod->interner->intern("__match"));
        v->type = patn->type;
        for (AST* a : l->items) if (((CaseDecl*)a)->reachable) {
            CaseDecl* d = (CaseDecl*)a;
            indent(ctx.c, ctx.c_indent);
            write(ctx.c, first ? "if (" : "else if (");
            first = false;
            emit_c_pattern_match(mod, d->env, d->pattern, v, ctx);
            write(ctx.c, ") {\n");
            ctx.c_indent += 4;
            indent(ctx.c, ctx.c_indent), emit_c_pattern_defs(mod, d->env, d->pattern, v, ctx);
            write(ctx.c, ";\n");
            indent(ctx.c, ctx.c_indent), emit_c(mod, d->env, d->body, ctx);
            write(ctx.c, ";\n");
            ctx.c_indent -= 4;
            indent(ctx.c, ctx.c_indent), write(ctx.c, "}\n");
        }
        ctx.c_indent -= 4;
        indent(ctx.c, ctx.c_indent), write(ctx.c, '}');
        endmatch
    default:
        unreachable("Unsupported expr!");
    }
}

#undef casematch