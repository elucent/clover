#include "clover/ast.h"
#include "clover/env.h"
#include "clover/type.h"
#include "clover/clover.h"
#include "clover/err.h"
#include "lib/str.h"
#include "lib/utf.h"

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
    AST_NONE, AST_NONE, AST_NONE, AST_NONE, AST_NONE, AST_NONE, AST_NONE, AST_NONE, AST_NONE, AST_NONE,
    AST_NONE, AST_NONE, AST_NONE, AST_NONE, AST_NONE, AST_NONE, AST_NONE, AST_NONE,
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
    AST_NONE, AST_NONE, AST_NONE, AST_NONE, AST_NONE, AST_NONE, AST_NONE, AST_NONE, AST_NONE, AST_NONE,
    AST_NONE, AST_NONE, AST_NONE, AST_NONE, AST_NONE, AST_NONE, AST_NONE, AST_NONE,
    AST_NONE, AST_NONE,
    AST_NONE, AST_NONE, AST_NONE, AST_NONE, AST_NONE
};

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
        parser.indents.pop();
        parser.leave(new(parser.astspace) List(AST_DO, parser.endpos(parser.back()->pos), parser.take(parser.endcount())));
    }
    else parser.inc().rec(parse_statement);
}

void parse_block(Lexer& lexer, Parser& parser, Module* mod, Token& next) {
    if (next.kind == TK_NEWLINE) { // Indented block.
        parser.indents.push(parser.indent);
        parser.consume_and_indent(lexer)
              .begincount()
              .startpos(next)
              .tailrec(parse_indented_block).rec(parse_statement);
    }
    else parser.tailrec(parse_statement); // Single expression block.
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
        parser.push(new(parser.astspace) VarDecl(parser.endpos(parser.rhs()->pos), parser.lhs(), parser.rhs(), init))
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
        parser.push(new(parser.astspace) VarDecl(init ? parser.lhs()->pos + init->pos : parser.lhs()->pos + parser.rhs()->pos, parser.lhs(), parser.rhs(), init))
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
    else parser.tailrec(parse_var_or_fundecl).rec(parse_binary);
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
    parser.leave(new(parser.astspace) Unary(AST_NEW, parser.endpos(next), parser.pop()));
}

void parse_maybe_argdecl(Lexer& lexer, Parser& parser, Module* mod, Token& next) {
    if (next.kind == TK_IDENT) {
        parser.leave(new(parser.astspace) VarDecl(parser.back()->pos + next, parser.pop(), new(parser.astspace) Var(next, next.end), nullptr))
              .consume(lexer);
    }
    else parser.leave();
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
              .rec(parse_maybe_argdecl).rec(parse_binary);
    else no_call_sep_error(mod, next), parser.leave().unnest().endcount();
}

void parse_maybe_void_call(Lexer& lexer, Parser& parser, Module* mod, Token& next) {
    // Consider the case where we have a function call with no arguments, i.e. foo()
    AST* fn = parser.pop();
    if (next.kind == TK_RPAREN) {
        if (fn->kind == AST_DOT) {
            parser.push(((Binary*)fn)->left)
                  .consume(lexer)
                  .tailrec(parse_post_primary, new(parser.astspace) Apply(fn->pos + next, ((Binary*)fn)->right, parser.take(1)))
                  .unnest();
        }
        else {
            parser.consume(lexer)
                  .tailrec(parse_post_primary, new(parser.astspace) Apply(fn->pos + next, fn, slice<AST*>{(AST**)nullptr, (iptr)0}))
                  .unnest();
        }
    }
    else {
        if (fn->kind == AST_DOT) {
            parser.push(((Binary*)fn)->right)
                  .push(((Binary*)fn)->left)
                  .begincount()
                  .inc()
                  .tailrec(parse_call_separator)
                  .rec(parse_maybe_argdecl)
                  .rec(parse_binary);
        }
        else {
            parser.push(fn)
                  .begincount()
                  .tailrec(parse_call_separator)
                  .rec(parse_maybe_argdecl)
                  .rec(parse_binary);
        }
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
                  .tailrec(parse_post_primary, new(parser.astspace) Unary(AST_POSTINCR, parser.back()->pos + next, parser.back()))
                  .pop();
            return;
        case TK_DECR:
            parser.consume(lexer)
                  .tailrec(parse_post_primary, new(parser.astspace) Unary(AST_POSTDECR, parser.back()->pos + next, parser.back()))
                  .pop();
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
                  .tailrec(parse_post_primary, new(parser.astspace) Var(next, next.end));
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
        case TK_LPAREN:
            parser.consume(lexer)
                  .startpos(next)
                  .tailrec(parse_maybe_unit)
                  .nest();
            return;
        case TK_IF:
            parser.consume(lexer)
                  .startpos(next)
                  .tailrec(parse_if_then_condition)
                  .rec(parse_binary);
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
        default:
            expected_primary_error(mod, next), parser.consume(lexer).leave(new(parser.astspace) IllFormed());
    }
}

void parse_binary_return_to_op(Lexer& lexer, Parser& parser, Module* mod, Token& next) {
    // Pops the last operator off the operator stack and creates an AST node.
    parser.leave(new(parser.astspace) Binary(TOKEN_TO_BINOP[parser.popop()], parser.lhs()->pos + parser.rhs()->pos, parser.lhs(), parser.rhs())).poplr();
}

void parse_trailing_newlines(Lexer& lexer, Parser& parser, Module* mod, Token& next) {
    if (next.kind == TK_NEWLINE) parser.consume(lexer);
    else parser.leave();
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
        else parser.push(new(parser.astspace) Binary(TOKEN_TO_BINOP[parser.popop()], parser.lhs()->pos + parser.rhs()->pos, parser.lhs(), parser.rhs()))
                   .poplr()
                   .consume(lexer).pushop(next.kind)
                   .tailrec(parse_binary_next_op) // Continue to the right after parsing lhs.
                   .rec(parse_primary)
                   .rec(parse_trailing_newlines);
    }
    else parser.leave(new(parser.astspace) Binary(TOKEN_TO_BINOP[parser.popop()], parser.lhs()->pos + parser.rhs()->pos, parser.lhs(), parser.rhs()))
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
        else
            parser.consume(lexer).pushop(next.kind).tailrec(parse_binary_next_op).rec(parse_primary).rec(parse_trailing_newlines);
    }
    else parser.leave(); // Otherwise, it's just a primary expression and we can leave it at that.
}

void parse_binary(Lexer& lexer, Parser& parser, Module* mod, Token& next) {
    parser.tailrec(parse_binary_op).rec(parse_primary);
}

void parse_while_end(Lexer& lexer, Parser& parser, Module* mod, Token& next) {
    parser.leave(new(parser.astspace) Binary(AST_WHILE, parser.endpos(parser.rhs()->pos), parser.lhs(), parser.rhs()))
          .poplr();
}

void parse_while_condition(Lexer& lexer, Parser& parser, Module* mod, Token& next) {
    if (next.kind == TK_COLON)
        parser.consume(lexer)
              .tailrec(parse_while_end)
              .rec(parse_block);
    else no_block_error(mod, next), parser.leave().endpos({});
}

void parse_until_end(Lexer& lexer, Parser& parser, Module* mod, Token& next) {
    parser.leave(new(parser.astspace) Binary(AST_UNTIL, parser.endpos(parser.rhs()->pos), parser.lhs(), parser.rhs()))
          .poplr();
}

void parse_until_condition(Lexer& lexer, Parser& parser, Module* mod, Token& next) {
    if (next.kind == TK_COLON)
        parser.consume(lexer)
              .tailrec(parse_until_end)
              .rec(parse_block);
    else no_block_error(mod, next), parser.leave().endpos({});
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
    else no_block_error(mod, next), parser.leave().endpos({});
}

void parse_for_binding(Lexer& lexer, Parser& parser, Module* mod, Token& next) {
    if (next.kind == TK_IN) 
        parser.consume(lexer)
              .tailrec(parse_for_body)
              .rec(parse_binary);
    else no_for_loop_in_error(mod, next), parser.leave().endpos({});
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
               .rec(parse_statement);
}

void parse_return_end(Lexer& lexer, Parser& parser, Module* mod, Token& next) {
    if (next.kind == TK_NEWLINE)
        parser.leave(new(parser.astspace) Unary(AST_RETURN, parser.endpos(parser.back()->pos), parser.pop()))
              .consume_and_indent(lexer);
    else no_newline_error(mod, next), parser.leave().endpos({});
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
    else no_newline_error(mod, next), parser.leave();
}

void parse_continue_newline(Lexer& lexer, Parser& parser, Module* mod, Token& next) {
    if (next.kind == TK_NEWLINE) parser.consume_and_indent(lexer).leave();
    else no_newline_error(mod, next), parser.leave();
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
    else no_block_error(mod, next), parser.leave().endpos({});
}

void parse_use_end(Lexer& lexer, Parser& parser, Module* mod, Token& next) {
    if (next.kind == TK_NEWLINE)
        parser.consume_and_indent(lexer)
              .leave(new(parser.astspace) Binary(AST_USE, parser.endpos(parser.rhs()->pos), parser.lhs(), parser.rhs()))
              .poplr();
    else no_newline_error(mod, next), parser.leave().endpos({});
}

void parse_use_alias(Lexer& lexer, Parser& parser, Module* mod, Token& next) {
    if (next.kind == TK_IDENT)
        parser.consume(lexer)
              .tailrec(parse_use_end, new(parser.astspace) Var(next, next.end));
    else no_use_alias_error(mod, next), parser.leave().endpos({});
}

void parse_use_suffix(Lexer& lexer, Parser& parser, Module* mod, Token& next) {
    if (next.kind == TK_AS)
        parser.consume(lexer)
              .tailrec(parse_use_alias);
    else if (next.kind == TK_NEWLINE)
        parser.consume_and_indent(lexer)
              .leave(new(parser.astspace) Binary(AST_USE, parser.endpos(parser.back()->pos), parser.pop(), nullptr)); // Null alias name.
    else no_newline_or_as_error(mod, next), parser.leave().endpos({});
}

void parse_alias_end(Lexer& lexer, Parser& parser, Module* mod, Token& next) {
    if (next.kind == TK_NEWLINE)
        parser.consume_and_indent(lexer)
              .leave(new(parser.astspace) AliasDecl(parser.endpos(parser.rhs()->pos), parser.lhs(), parser.rhs()))
              .poplr();
    else no_newline_error(mod, next), parser.leave().endpos({});
}

void parse_alias_body(Lexer& lexer, Parser& parser, Module* mod, Token& next) {
    if (next.kind == TK_COLON)
        parser.consume(lexer)
              .tailrec(parse_alias_end)
              .rec(parse_binary);
    else no_alias_colon_error(mod, next), parser.leave().endpos({});
}

void parse_alias_name(Lexer& lexer, Parser& parser, Module* mod, Token& next) {
    if (next.kind == TK_IDENT)
        parser.consume(lexer)
              .tailrec(parse_alias_body, new(parser.astspace) Var(next, next.end));
    else no_alias_ident_error(mod, next), parser.leave().endpos({});
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
    else no_type_colon_nl_error(mod, next), parser.leave().endpos({});
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
    else no_paramlist_sep_error(mod, next), parser.leave().endpos({});
}

void parse_typedecl_param(Lexer& lexer, Parser& parser, Module* mod, Token& next) {
    if (next.kind == TK_IDENT)
        parser.consume(lexer)
              .tailrec(parse_typedecl_params_separator, new(parser.astspace) Var(next, next.end));
    else no_typedecl_param_error(mod, next), parser.leave().endpos({});
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
              .tailrec(parse_maybe_generic_typedecl, new(parser.astspace) Var(next, next.end));
    else no_typedecl_name_error(mod, next), parser.leave().endpos({});
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
    else no_case_colon_nl_error(mod, next), parser.leave().endpos({});
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
    else no_block_error(mod, next), parser.leave().endpos({});
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
    else no_block_error(mod, next), parser.leave().endpos({});
}

void parse_module_name(Lexer& lexer, Parser& parser, Module* mod, Token& next) {
    if (next.kind == TK_IDENT)
        parser.consume(lexer)
              .tailrec(parse_module_body, new(parser.astspace) Var(next, next.end));
    else no_module_name_error(mod, next), parser.leave().endpos({});
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
                  .rec(parse_maybe_argdecl)
                  .rec(parse_primary);
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
    if (parser.check_indent && lexer.tokens && lexer.tokens.peek().kind != TK_NEWLINE)
        parser.check_indent = false, parser.indent = lexer.tokens.peek().column;
    while (lexer.tokens && parser.prods.size()) {
        Token next = lexer.tokens.peek();
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
    casematch(AST_VAR, Var*, v) write(io, mod->interner->intern_data[v->name].t); endmatch
    
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
    casematch(AST_PAREN, Unary*, u) write(io, '('); format(io, mod, u->child); write(io, ")"); endmatch
    casematch(AST_CONV, Unary*, u) 
        write(io, "cast<"); 
        format(io, mod, u->type); 
        write(io, ">("); 
        format(io, mod, u->child); 
        write(io, ")"); 
        endmatch
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
    
    casematch(AST_TYPENAME, Var*, v) write(io, '<', mod->interner->intern_data[v->name].t, '>'); endmatch
    casematch(AST_TYPECONST, Expr*, e) write(io, '<'), format(io, mod, e->type), write(io, '>'); endmatch
    casematch(AST_MODULENAME, Var*, v) write(io, mod->interner->intern_data[v->name].t); endmatch
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
        write(io, "(return ");  
        format(io, mod, u->child);
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

Env* anon_namespace(Module* mod, Env* env) {
    i8 buf[16];
    i32 len = 0;

    i32 id = env->anon ++;
    if (!id) buf[len ++] = '0';
    else {
        i32 c = 0, d = 0;
        u64 p = 1;
        while (p <= id) p *= 10, ++ c;
        d = c;
        while (c >= 1) {
            buf[c - 1] = "0123456789"[id % 10]; 
            id /= 10;
            c -= 1;
        }
        len = d;
    }
    i32 name = mod->interner->intern({{buf, len}});
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

void compute_envs(Module* mod, Env* env, AST* ast) {
    switch (ast->kind) {
    casematch(AST_PROGRAM, ASTProgram*, p)
        p->env = mod->envctx->create(ENV_GLOBAL, mod->envctx->root, mod->basename);
        mod->envctx->root->def(mod->basename, e_global(p));
        for (Statement* ast : p->toplevel) if (ast->kind == AST_MODULEDECL) compute_envs(mod, p->env, ast);
        for (Statement* ast : p->toplevel) if (ast->kind != AST_MODULEDECL) compute_envs(mod, p->env, ast);
        endmatch

    // All constants, unary ops, and binary ops are omitted because they cannot contain decls.

    casematch(AST_APPLY, Apply*, a)
        for (AST* ast : a->args) compute_envs(mod, env, ast);
        endmatch
    casematch(AST_ARRAY, List*, l)
        endmatch
    casematch(AST_SET, List*, l)
        endmatch
    casematch(AST_VARDECL, VarDecl*, d)
        if (d->ann) compute_envs(mod, env, d->ann);
        env->def(d->basename = basename(mod, d->name), e_var(nullptr, d));
        endmatch
    casematch(AST_DEREF, Unary*, u) compute_envs(mod, env, u->child); endmatch
    casematch(AST_INDEX, Binary*, u) 
        compute_envs(mod, env, u->left); 
        if (u->right) compute_envs(mod, env, u->right); 
        endmatch
    casematch(AST_SLICE, Slice*, s) 
        compute_envs(mod, env, s->array); 
        compute_envs(mod, env, s->low); 
        compute_envs(mod, env, s->high); 
        endmatch
    casematch(AST_FUNDECL, FunDecl*, d)
        i32 name = d->basename = basename(mod, d->proto);
        d->env = mod->envctx->create(ENV_FUN, env, name);
        if (d->returned) compute_envs(mod, d->env, d->returned);

        slice<AST*>& args = ((Apply*)d->proto)->args;
        if (args.size() > 0 && args[0]->kind != AST_VARDECL) { // Assume it's a this parameter
            args[0] = new(mod->parser->astspace) VarDecl(
                args[0]->pos,
                args[0], 
                new(mod->parser->astspace) Var(args[0]->pos, mod->interner->intern("this")),
                nullptr
            );
        }
        else env->def(name, e_fun(nullptr, d)); // Only define non-methods

        compute_envs(mod, d->env, d->proto); // Declare arguments, if any.
        if (d->body)
            compute_envs(mod, d->env, d->body); // Traverse function body, if present.
        endmatch
    casematch(AST_ARGS, List*, l)
        for (AST* ast : l->items) compute_envs(mod, env, ast);
        endmatch
    casematch(AST_DO, List*, l)
        for (AST* ast : l->items) compute_envs(mod, env, ast);
        endmatch
    casematch(AST_IF, If*, i)
        compute_envs(mod, env, i->ifTrue);
        if (i->ifFalse) compute_envs(mod, env, i->ifFalse);
        endmatch
    casematch(AST_WHILE, Binary*, b)
        compute_envs(mod, env, b->right);
        endmatch
    casematch(AST_UNTIL, Binary*, b)
        compute_envs(mod, env, b->right);
        endmatch
    casematch(AST_FOR, For*, f)
        f->env = mod->envctx->create(ENV_LOCAL, env, mod->interner->intern("for"));
        AST* binding = f->binding;
        AST* container = new(mod->parser->astspace) VarDecl(f->binding->pos, nullptr, new(mod->parser->astspace) Var(f->binding->pos, mod->interner->intern("_items")), f->items);
        slice<AST*> container_slice = { new(mod->parser->astspace) AST*(new(mod->parser->astspace) Var(f->items->pos, mod->interner->intern("_items"))), 1 };
        AST* iter = new(mod->parser->astspace) VarDecl(f->binding->pos, nullptr, new(mod->parser->astspace) Var(f->pos, mod->interner->intern("_iter")), 
            new(mod->parser->astspace) Apply(f->binding->pos, new(mod->parser->astspace) Var(f->pos, mod->interner->intern("iter")), container_slice)
        );
        slice<AST*> iter_slice = { new(mod->parser->astspace) AST*(new(mod->parser->astspace) Var(f->pos, mod->interner->intern("_iter"))), 1 };
        if (binding->kind == AST_VAR) binding = new(mod->parser->astspace) VarDecl(f->binding->pos, nullptr, binding, 
            new(mod->parser->astspace) Apply(f->pos, new(mod->parser->astspace) Var(f->pos, mod->interner->intern("read")), iter_slice)
        );
        else if (binding->kind == AST_VARDECL) {
            if (((VarDecl*)binding)->init) fatal("Cannot initialize iteration variable in for loop.");
            ((VarDecl*)binding)->init = new(mod->parser->astspace) Apply(f->pos, new(mod->parser->astspace) Var(f->pos, mod->interner->intern("read")), iter_slice);
        }
        AST* incr = new(mod->parser->astspace) Binary(AST_ASSIGN, f->pos, iter_slice[0], new(mod->parser->astspace) Apply(f->pos, new(mod->parser->astspace) Var(f->pos, mod->interner->intern("next")), iter_slice));
        AST* cond = new(mod->parser->astspace) Apply(f->pos, new(mod->parser->astspace) Var(f->pos, mod->interner->intern("empty")), iter_slice);
        slice<AST*> body_block = { new(mod->parser->astspace) AST*[3], 3 };
        body_block[0] = binding;
        body_block[1] = f->body;
        body_block[2] = incr;
        f->body = new(mod->parser->astspace) Binary(AST_UNTIL, f->pos, cond, 
            new(mod->parser->astspace) List(AST_DO, f->body->pos, body_block)
        );
        slice<AST*> outer_block = { new(mod->parser->astspace) AST*[3], 3 };
        outer_block[0] = container;
        outer_block[1] = iter;
        outer_block[2] = f->body;
        f->body = new(mod->parser->astspace) List(AST_DO, f->pos, outer_block);
        compute_envs(mod, f->env, f->body);
        endmatch
    casematch(AST_DEFER, Unary*, u)
        compute_envs(mod, env, u->child);
        endmatch
    casematch(AST_RETURN, Unary*, u)
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
            mod->deps.push(imported);
            modenv = imported->parser->program->env;
            modname = imported->basename;
        }
        else if (b->left->kind == AST_VAR) {
            modname = ((Var*)b->left)->name;
            auto* def = env->lookup(modname);
            if (!def || (def->kind != E_MOD && def->kind != E_GLOBAL))
                fatal("Could not import module.");
            if (def->kind == E_MOD) modenv = ((ModuleDecl*)def->ast)->env;
            else if (def->kind == E_GLOBAL) modenv = ((ASTProgram*)def->ast)->env;
        }
        if (!modenv) fatal("Could not import module.");
        
        if (b->right) {
            ModuleDecl* new_decl = new(mod->parser->astspace) ModuleDecl(b->pos, new(mod->parser->astspace) Var(b->right->pos, modname), nullptr);
            new_decl->env = modenv;
            new_decl->basename = modname;
            env->def(modname, e_mod(new_decl));
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
                    fatal("Expected type parameter name.");
                else d->env->def(((Var*)ast)->name, e_type(VOID, nullptr)); // Placeholder
            }
            d->generic = true;
        }
        else if (d->name->kind == AST_DOT || d->kind == AST_TYPEDOT) 
            fatal("Dots are not permitted in type names.");
        else if (d->name->kind != AST_VAR)
            fatal("Expected type name.");
        if (!d->generic && d->body) compute_envs(mod, d->env, d->body);
        if (d->generic) env->def(name, e_gentype(d));
        else env->def(name, e_type(nullptr, d));
        endmatch
    casematch(AST_CASEDECL, CaseDecl*, d)
        i32 name = basename(mod, d->pattern);
        if (name != -1) {
            d->env = mod->envctx->create(env->kind == ENV_TYPE ? ENV_TYPE : ENV_LOCAL, env, name);
            env->def(name, e_case(nullptr, d));
        }
        else d->env = anon_namespace(mod, env);
        d->env->decl = d;
        d->basename = name;
        compute_envs(mod, d->env, d->pattern);
        if (d->body) compute_envs(mod, d->env, d->body);
        endmatch
    casematch(AST_MATCH, Binary*, b)
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
            env->def(((Var*)u->child)->name, e_type(var, nullptr));
            u->type = var;
        }
        else if (existing) {
            if (existing->kind == E_TYPE && existing->type->kind == T_VAR)
                u->type = existing->type;
            else fatal("Can't redefine name as type var.");
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
        if (kind != UNKNOWN_KIND && kind != HAS_CASE) fatal("Unexpected case in non-union type.");
        member_stack.push({ v->basename, v->type });
        kind = HAS_CASE;
        endmatch
    casematch(AST_VARDECL, VarDecl*, v)
        if (!v->ann) fatal("Fields cannot have inferred types.");
        if (kind != UNKNOWN_KIND && kind != HAS_FIELD) fatal("Unexpected field in non-struct type.");
        member_stack.push({ v->basename, v->ann->type });
        kind = HAS_FIELD;
        endmatch
    default:
        if (is_type(ast)) {
            if (kind != UNKNOWN_KIND) fatal("Named type aliases cannot have fields or cases.");
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
            d->env->def(d->basename, e_type(recur = mod->typectx->defvar(mod, d->env), d)); // Define type as an empty type variable for recursive types.
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
            for (i32 i = 0; i < n_members; i ++) arr[i] = member_stack[init_size + i];
            result = mod->typectx->def<UnionType>(d->basename, d->env, slice<pair<i32, Type*>>{ arr, n_members });
            break;
        case NO_FIELDS_OR_CASES:
            result = mod->typectx->def<NamedType>(d->basename, d->env, member_stack.back().second);
            type_env(result)->def(mod->interner->intern("this"), e_var(member_stack.back().second, nullptr));
            break;
        case UNKNOWN_KIND:
            fatal("Case declaration has empty body.");
        }
        if (recur) *((VarType*)recur)->binding = result;
        return result;
        endmatch
    casematch(AST_TYPEDECL, TypeDecl*, d)
        Type* recur = nullptr;
        if (d->body) {
            d->env->def(d->basename, e_type(recur = mod->typectx->defvar(mod, d->env), d)); // Define type as an empty type variable for recursive types.
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
            for (i32 i = 0; i < n_members; i ++) arr[i] = member_stack[init_size + i];
            result = mod->typectx->def<UnionType>(d->basename, d->env, slice<pair<i32, Type*>>{ arr, n_members });
            break;
        case NO_FIELDS_OR_CASES:
            result = mod->typectx->def<NamedType>(d->basename, d->env, member_stack.back().second);
            type_env(result)->def(mod->interner->intern("this"), e_var(member_stack.back().second, nullptr));
            break;
        case UNKNOWN_KIND:
            fatal("Type declaration has empty body.");
        }
        if (recur) *((VarType*)recur)->binding = result;
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
            return type_env(concrete(u->type));
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

Env* call_parent(Module* mod, Apply* a) {
    if (a->args.n && a->args[0]->type && a->fn->kind == AST_VAR) {
        pair<Type*, Env*>* ast = mod->envctx->find_method(((Var*)a->fn)->name, a->args[0]->type);
        if (ast) return ast->second;
    }
    return nullptr;
}

Symbol inst_symbol(Module* mod, Symbol name, const TypeTuple& params) {
    const_slice<i8> prev = mod->interner->str(name);
    iptr len = prev.n + 1;
    for (Type* t : params.types) len += mod->interner->str(concrete(t)->env->name).n + 1;
    i8* name_space = new(mod->parser->astspace) i8[len];
    i8* writer = (i8*)mcpy(name_space, prev.ptr, prev.n);
    *writer ++ = '$';
    for (Type* t : params.types) {
        *writer ++ = '$';
        const_slice<i8> tname = mod->interner->str(concrete(t)->env->name);
        writer = (i8*)mcpy(writer, tname.ptr, tname.n);
    }
    return mod->interner->intern({{ name_space, len }});
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
        inst->name = new(mod->parser->astspace) Var(d->name->pos, inst_symbol(mod, d->basename, tup));
        compute_envs(mod, d->env->parent, inst);
        for (iptr i = 0; i < vars.n; i ++)
            inst->env->def(((Var*)vars[i])->name, e_type(types[i], nullptr));
        detect_types(mod, inst->env->parent, inst);
        d->insts.put(tup, inst);
        return inst;
    }
}

FunDecl* inst_fun(Module* mod, Env* env, FunDecl* d, slice<AST*> params) {
    slice<AST*> vars = ((Apply*)d->proto)->args;
    int n_generic = 0;
    for (int i = 0; i < params.n; i ++) if (vars[i]->kind == AST_VAR
        || (vars[i]->kind == AST_VARDECL && !isconcrete(vars[i]->type))) n_generic ++;
    slice<Type*> types = { new(mod->typectx->typespace) Type*[n_generic], n_generic };
    int written = 0;
    for (int i = 0; i < params.n; i ++) if (vars[i]->kind == AST_VAR
        || (vars[i]->kind == AST_VARDECL && !isconcrete(vars[i]->type))) types[written ++] = params[i]->type;
    TypeTuple tup(types);

    auto it = d->insts.find(tup);
    if (it != d->insts.end()) return it->value;
    else {
        FunDecl* inst = (FunDecl*)d->clone(mod->parser->astspace);
        inst->isinst = true;
        ((Apply*)inst->proto)->fn = new(mod->parser->astspace) Var(d->proto->pos, inst_symbol(mod, d->basename, tup));
        for (iptr i = 0; i < vars.n; i ++) {
            if (vars[i]->kind == AST_VAR) {
                AST* ann = new(mod->parser->astspace) Expr(AST_TYPECONST, params[i]->pos);
                ann->type = params[i]->type;
                ((Apply*)inst->proto)->args[i] = new(mod->parser->astspace) VarDecl(
                    vars[i]->pos, 
                    ann,
                    ((Apply*)inst->proto)->args[i],
                    nullptr
                );
            }
        }
        compute_envs(mod, d->env->parent, inst);
        detect_types(mod, inst->env->parent, inst);
        for (iptr i = 0; i < vars.n; i ++) {
            slice<AST*>& args = ((Apply*)inst->proto)->args;
            if (args[i]->kind == AST_VARDECL && !isconcrete(args[i]->type))
                unify(args[i]->type, params[i]->type);
        }
        detect_types(mod, inst->env->parent, inst);
        d->insts.put(tup, inst);
        return inst;
    }
}

void detect_types(Module* mod, Env* env, AST* ast) {
    switch (ast->kind) {
    casematch(AST_PROGRAM, ASTProgram*, p)
        for (Statement* stmt : p->toplevel) detect_types(mod, p->env, stmt);
        endmatch
    casematch(AST_ICONST, Const*, c) endmatch
    casematch(AST_FCONST, Const*, c) endmatch
    casematch(AST_CHCONST, Const*, c) endmatch
    casematch(AST_STRCONST, Const*, c) endmatch
    casematch(AST_BOOL, Const*, c) endmatch
    casematch(AST_UNIT, Const*, c) endmatch
    casematch(AST_VAR, Var*, v)         // Handle typenames (T).
        Entry* e = env->lookup(v->name);
        if (!e) fatal("Undefined variable.");
        else if (e->kind == E_TYPE || e->kind == E_ALIAS || (e->kind == E_CASE && env->kind == ENV_TYPE)) {
            Type* t = lookup_type(mod, env, v->name);
            v->kind = AST_TYPENAME;
            v->type = t;
        }
        else if (e->kind == E_MOD) {
            v->kind = AST_MODULENAME;
            v->type = VOID;
        }
        endmatch
    casematch(AST_PLUS, Unary*, u) detect_types(mod, env, u->child); endmatch
    casematch(AST_NEG, Unary*, u) detect_types(mod, env, u->child); endmatch
    casematch(AST_DEREF, Unary*, u)     // Handle pointer types (*T)
        detect_types(mod, env, u->child);
        if (is_type(u->child)) {
            u->kind = AST_PTRTYPE;
            u->type = mod->typectx->def<PtrType>(u->child->type);
        }
        endmatch
    casematch(AST_ADDROF, Unary*, u) detect_types(mod, env, u->child); endmatch
    casematch(AST_NOT, Unary*, u) detect_types(mod, env, u->child); endmatch
    casematch(AST_BITNOT, Unary*, u) detect_types(mod, env, u->child); endmatch
    casematch(AST_INCR, Unary*, u) detect_types(mod, env, u->child); endmatch
    casematch(AST_DECR, Unary*, u) detect_types(mod, env, u->child); endmatch
    casematch(AST_POSTINCR, Unary*, u) detect_types(mod, env, u->child); endmatch
    casematch(AST_POSTDECR, Unary*, u) detect_types(mod, env, u->child); endmatch
    casematch(AST_NEW, Unary*, u) detect_types(mod, env, u->child); endmatch
    casematch(AST_PAREN, Unary*, u) detect_types(mod, env, u->child); endmatch

    casematch(AST_ADD, Binary*, b) detect_types(mod, env, b->left); detect_types(mod, env, b->right); endmatch
    casematch(AST_SUB, Binary*, b) detect_types(mod, env, b->left); detect_types(mod, env, b->right); endmatch
    casematch(AST_STAR, Binary*, b) 
        detect_types(mod, env, b->left); 
        if (is_type(b->left)) {
            b->kind = AST_PTRDECL;
            b->left = new(mod->parser->astspace) Unary(AST_DEREF, b->left->pos, b->left);
            detect_types(mod, env, b->left);
            b->type = b->left->type;
            env->def(basename(mod, b->right), e_var(b->type, b));
        } 
        else detect_types(mod, env, b->right);
        endmatch
    casematch(AST_DIV, Binary*, b) detect_types(mod, env, b->left); detect_types(mod, env, b->right); endmatch
    casematch(AST_MOD, Binary*, b) detect_types(mod, env, b->left); detect_types(mod, env, b->right); endmatch
    casematch(AST_EXP, Binary*, b) detect_types(mod, env, b->left); detect_types(mod, env, b->right); endmatch
    casematch(AST_BITAND, Binary*, b) detect_types(mod, env, b->left); detect_types(mod, env, b->right); endmatch
    casematch(AST_BITOR, Binary*, b) detect_types(mod, env, b->left); detect_types(mod, env, b->right); endmatch
    casematch(AST_BITXOR, Binary*, b) detect_types(mod, env, b->left); detect_types(mod, env, b->right); endmatch
    casematch(AST_BITLEFT, Binary*, b) detect_types(mod, env, b->left); detect_types(mod, env, b->right); endmatch
    casematch(AST_BITRIGHT, Binary*, b) detect_types(mod, env, b->left); detect_types(mod, env, b->right); endmatch
    casematch(AST_LESS, Binary*, b) detect_types(mod, env, b->left); detect_types(mod, env, b->right); endmatch
    casematch(AST_LEQUAL, Binary*, b) detect_types(mod, env, b->left); detect_types(mod, env, b->right); endmatch
    casematch(AST_GREATER, Binary*, b) detect_types(mod, env, b->left); detect_types(mod, env, b->right); endmatch
    casematch(AST_GEQUAL, Binary*, b) detect_types(mod, env, b->left); detect_types(mod, env, b->right); endmatch
    casematch(AST_EQUAL, Binary*, b) detect_types(mod, env, b->left); detect_types(mod, env, b->right); endmatch
    casematch(AST_INEQUAL, Binary*, b) detect_types(mod, env, b->left); detect_types(mod, env, b->right); endmatch
    casematch(AST_AND, Binary*, b) detect_types(mod, env, b->left); detect_types(mod, env, b->right); endmatch
    casematch(AST_OR, Binary*, b) detect_types(mod, env, b->left); detect_types(mod, env, b->right); endmatch
    casematch(AST_DOT, Binary*, b) 
        detect_types(mod, env, b->left); 
        Env* nenv = ast_env(env, b->left);
        if (nenv) {
            detect_types(mod, nenv, b->right);
            if (is_type(b->right)) b->kind = AST_TYPEDOT, b->type = b->right->type;
        }
        endmatch
    casematch(AST_ASSIGN, Binary*, b) detect_types(mod, env, b->left); detect_types(mod, env, b->right); endmatch
    casematch(AST_ADDEQ, Binary*, b) detect_types(mod, env, b->left); detect_types(mod, env, b->right); endmatch
    casematch(AST_SUBEQ, Binary*, b) detect_types(mod, env, b->left); detect_types(mod, env, b->right); endmatch
    casematch(AST_STAREQ, Binary*, b) detect_types(mod, env, b->left); detect_types(mod, env, b->right); endmatch
    casematch(AST_DIVEQ, Binary*, b) detect_types(mod, env, b->left); detect_types(mod, env, b->right); endmatch
    casematch(AST_MODEQ, Binary*, b) detect_types(mod, env, b->left); detect_types(mod, env, b->right); endmatch
    casematch(AST_EXPEQ, Binary*, b) detect_types(mod, env, b->left); detect_types(mod, env, b->right); endmatch
    casematch(AST_BITANDEQ, Binary*, b) detect_types(mod, env, b->left); detect_types(mod, env, b->right); endmatch
    casematch(AST_BITOREQ, Binary*, b) detect_types(mod, env, b->left); detect_types(mod, env, b->right); endmatch
    casematch(AST_BITXOREQ, Binary*, b) detect_types(mod, env, b->left); detect_types(mod, env, b->right); endmatch
    casematch(AST_BITLEFTEQ, Binary*, b) detect_types(mod, env, b->left); detect_types(mod, env, b->right); endmatch
    casematch(AST_BITRIGHTEQ, Binary*, b) detect_types(mod, env, b->left); detect_types(mod, env, b->right); endmatch

    casematch(AST_INDEX, Binary*, b)    // Handle array types (T[N]) and slice types (T[])
        detect_types(mod, env, b->left);
        if (is_type(b->left)) {
            if (b->right && b->right->kind != AST_ICONST)
                fatal("Size of array type must be an integer constant.");
            b->kind = b->right ? AST_ARRAYTYPE : AST_SLICETYPE;
            b->type = b->right 
                ? mod->typectx->def<ArrayType>(b->left->type, ((Const*)b->right)->iconst) 
                : mod->typectx->def<SliceType>(b->left->type);
        }
        endmatch
    casematch(AST_SLICE, Slice*, s)    // Handle array types (T[N]) and slice types (T[])
        detect_types(mod, env, s->array);
        if (is_type(s->array)) fatal("Typename not allowed in slice expression.");
        endmatch
    casematch(AST_APPLY, Apply*, a)
        for (AST* ast : a->args) detect_types(mod, env, ast);
        // if (a->args.n && is_type(a->args[0])) { // We accidentally parsed a constructor call as a method.
        //     a->fn = new(mod->parser->astspace) Binary(AST_DOT, a->args[0], a->fn);
        //     a->args = { a->args.ptr + 1, a->args.n - 1 }; // Skip first argument.
        // }
        
        if (a->fn->kind != AST_VAR || env->lookup(((Var*)a->fn)->name)) // Defer undefined function checks until later,
            detect_types(mod, env, a->fn);                              // we need the type of the first arg to check if it's
                                                                        // a method.
        if (a->fn->kind == AST_VAR) {
            Entry* e = env->lookup(((Var*)a->fn)->name);
            if (e && e->kind == E_GENTYPE) {
                TypeDecl* d = (TypeDecl*)e->ast;
                if (a->args.n != ((Apply*)d->name)->args.n)
                    fatal("Incorrect number of type arguments for generic type.");
                for (iptr i = 0; i < a->args.n; i ++) if (!is_type(a->args[i])) 
                    fatal("Non-type parameter passed to generic type.");
                TypeDecl* inst = inst_type(mod, env, d, a->args);
                if (inst) a->type = inst->type, a->kind = AST_TYPEINST;
                return;
            }
        }
        if (is_type(a->fn)) {
            ASTKind newast = AST_APPLY;
            vec<Type*, 256, arena> types;
            types.alloc = &mod->typectx->typespace;
            for (AST* ast : a->args) {
                detect_types(mod, env, ast);
                if (is_type(ast)) {
                    if (newast == AST_APPLY || newast == AST_FUNTYPE) newast = AST_FUNTYPE, types.push(ast->type);
                    else if (newast == AST_CTOR) fatal("Unexpected type in constructor call.");
                }
                else {
                    if (newast == AST_APPLY) newast = AST_CTOR;
                    else if (newast == AST_FUNTYPE) fatal("Unexpected expression in function type.");
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
        endmatch
    casematch(AST_ARRAY, List*, l) 
        for (AST* ast : l->items) detect_types(mod, env, ast);
        endmatch
    casematch(AST_SET, List*, l) 
        for (AST* ast : l->items) detect_types(mod, env, ast);
        endmatch
    casematch(AST_VARDECL, VarDecl*, d)
        Type* deftype;
        if (d->ann) {
            detect_types(mod, env, d->ann);
            if (!is_type(d->ann)) fatal("Found non-type expression in variable type annotation.");
            deftype = d->ann->type;
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
            if (!is_type(d->returned)) fatal("Found non-type expression in function return type.");
            rettype = d->returned->type;
        }
        else rettype = mod->typectx->defvar(mod, env);

        AST* proto = d->proto;
        if (proto->kind == AST_APPLY) {
            slice<AST*> args = ((Apply*)proto)->args;
            for (iptr i = 0; i < args.n; i ++) {
                AST* ast = args[i];
                if (ast->kind == AST_VAR) {
                    Entry* e = env->lookup(((Var*)ast)->name);
                    if (!e || e->kind != E_TYPE) d->generic = true;
                    else detect_types(mod, d->env, ast);
                }
                else if (ast->kind == AST_VARDECL && i == 0 && ((VarDecl*)ast)->basename == mod->interner->intern("this")
                    && ((VarDecl*)ast)->ann->kind == AST_VAR) {
                    AST* ann = ((VarDecl*)ast)->ann;
                    Entry* e = env->lookup(((Var*)ann)->name);
                    if (!e || e->kind != E_TYPE) {
                        d->generic = true, args[i] = ann;
                        env->def(d->basename, e_fun(nullptr, d)); // Non-method now needs to be properly defined.
                    }
                    else detect_types(mod, d->env, ast);
                }
                else detect_types(mod, d->env, ast);
            }
        }

        vec<Type*, 256, arena> argtypes;
        argtypes.alloc = &mod->parser->astspace;
        if (proto->kind == AST_APPLY) {
            slice<AST*> args = ((Apply*)proto)->args;

            for (AST* ast : args) if (ast->kind == AST_VAR 
                || (ast->kind == AST_VARDECL && !isconcrete(ast->type) && !d->isinst)) { // At least one generic parameter.
                d->generic = true;
                Entry* e = d->env->parent->lookup(d->basename);
                if (e) e->kind = E_GENFUN;
                else unreachable("Somehow didn't find definition.");
                return;
            }

            if (args.n && args[0]->kind == AST_VARDECL && ((VarDecl*)args[0])->basename == mod->interner->intern("this")) {
                Env* tenv = type_env(((VarDecl*)args[0])->type);
                if (tenv) {
                    d->env->siblings.push(d->env->parent), d->env->parent = tenv; // Switch parent to type env, add
                                                                                  // prev. parent as sibling.
                    tenv->def(d->basename, e_fun(nullptr, d));
                    mod->envctx->add_method(d->basename, ((VarDecl*)args[0])->type, tenv);
                }
            }

            for (AST* ast : args) if (ast->kind == AST_VARDECL || ast->kind == AST_PTRDECL) argtypes.push(ast->type);
            if (argtypes.size() == 0) argtypes.push(UNIT);
            Type** argslice = (Type**)mod->parser->astspace.alloc(sizeof(Type*) * argtypes.size());
            for (i32 i = 0; i < argtypes.size(); i ++) argslice[i] = argtypes[i];
            d->type = mod->typectx->def<FunType>(slice<Type*>{ argslice, argtypes.size() }, rettype);
        }
        else fatal("Expected function parameter list in declaration.");

        Entry* e = d->env->parent->lookup(d->basename);
        if (e) e->type = d->type;
        else unreachable("Somehow didn't find definition.");
        if (d->body) detect_types(mod, d->env, d->body);
        endmatch
    casematch(AST_ARGS, List*, d) 
        for (AST* ast : d->items) detect_types(mod, env, ast);
        endmatch
    casematch(AST_DO, List*, d) 
        for (AST* ast : d->items) detect_types(mod, env, ast);
        endmatch
    casematch(AST_IF, If*, i) 
        detect_types(mod, env, i->cond);
        detect_types(mod, env, i->ifTrue);
        if (i->ifFalse) detect_types(mod, env, i->ifFalse);
        endmatch
    casematch(AST_WHILE, Binary*, b) 
        detect_types(mod, env, b->left);
        detect_types(mod, env, b->right);
        endmatch
    casematch(AST_UNTIL, Binary*, b) 
        detect_types(mod, env, b->left);
        detect_types(mod, env, b->right);
        endmatch
    casematch(AST_FOR, For*, f) 
        detect_types(mod, f->env, f->body);
        endmatch
    casematch(AST_DEFER, Unary*, u) 
        detect_types(mod, env, u->child);
        endmatch
    casematch(AST_RETURN, Unary*, u) 
        detect_types(mod, env, u->child);
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
        if (d->generic) for (const auto& e : d->insts) detect_types(mod, env, e.value);
        else lookup_type(mod, env, d->basename);
        endmatch
    casematch(AST_CASEDECL, CaseDecl*, d) 
        lookup_type(mod, env, d->basename);
        endmatch
    casematch(AST_MATCH, Binary*, b) 
        endmatch
    casematch(AST_MODULEDECL, ModuleDecl*, b) 
        detect_types(mod, b->env, b->body);
        endmatch
    default:
        break;
    }
}

AST* coerce(Module* mod, Env* env, AST* ast, Type* dest) {
    if (!is_subtype(ast->type, dest)) return nullptr;
    if (is_subtype_generic(ast->type, dest)) return ast;
    
    AST* conv = new(mod->parser->astspace) Unary(AST_CONV, ast->pos, ast);
    conv->type = dest;
    return conv;
}

void check_arithmetic(Module* mod, Env* env, Unary* ast) {
    typecheck(mod, env, ast->child);
    if (concrete(ast->child->type)->kind != T_NUMERIC) 
        fatal("Expected numeric type in arithmetic expression.");
    ast->type = ast->child->type;
}

void check_arithmetic(Module* mod, Env* env, Binary* ast) {
    typecheck(mod, env, ast->left);
    typecheck(mod, env, ast->right);
    Type* result = concrete(unify(ast->left->type, ast->right->type));
    if (result->kind != T_NUMERIC) 
        format(stdout, mod, result), fatal("Expected numeric type in arithmetic expression.");
    ast->left = coerce(mod, env, ast->left, result), ast->right = coerce(mod, env, ast->right, result);
    ast->type = result;
}

void check_bitwise(Module* mod, Env* env, Unary* ast) {
    typecheck(mod, env, ast->child);
    Type* type = concrete(ast->child->type);
    if (type->kind != T_NUMERIC || ((NumericType*)type)->floating) 
        fatal("Expected integer type in bitwise expression.");
    ast->type = ast->child->type;
}

void check_bitwise(Module* mod, Env* env, Binary* ast) {
    typecheck(mod, env, ast->left);
    typecheck(mod, env, ast->right);
    Type* result = concrete(unify(ast->left->type, ast->right->type));
    if (result->kind != T_NUMERIC || ((NumericType*)result)->floating) 
        fatal("Expected integer type in bitwise expression.");
    ast->left = coerce(mod, env, ast->left, result), ast->right = coerce(mod, env, ast->right, result);
    ast->type = result;
}

void check_logic(Module* mod, Env* env, Unary* ast) {
    typecheck(mod, env, ast->child);
    if (concrete(ast->child->type)->kind != T_BOOL)
        fatal("Expected boolean type in logical expression.");
    ast->type = BOOL;
}

void check_logic(Module* mod, Env* env, Binary* ast) {
    typecheck(mod, env, ast->left);
    typecheck(mod, env, ast->right);
    Type* result = unify(ast->left->type, ast->right->type);
    if (result->kind != T_BOOL)
        fatal("Expected boolean type in logical expression.");
    ast->left = coerce(mod, env, ast->left, result), ast->right = coerce(mod, env, ast->right, result);
    ast->type = BOOL;
}

void check_compare(Module* mod, Env* env, Unary* ast) {
    typecheck(mod, env, ast->child);
    TypeKind tk = concrete(ast->child->type)->kind;
    if (tk != T_NUMERIC && tk != T_PTR && tk != T_STRING && tk != T_CHAR)
        fatal("Unexpected type in comparison expression.");
    ast->type = BOOL;
}

void check_compare(Module* mod, Env* env, Binary* ast) {
    typecheck(mod, env, ast->left);
    typecheck(mod, env, ast->right);
    Type* result = unify(ast->left->type, ast->right->type);
    TypeKind tk = concrete(result)->kind;
    if (tk != T_NUMERIC && tk != T_PTR && tk != T_STRING && tk != T_CHAR)
        fatal("Unexpected type in comparison expression.");
    ast->left = coerce(mod, env, ast->left, result), ast->right = coerce(mod, env, ast->right, result);
    ast->type = BOOL;
}

void check_equality(Module* mod, Env* env, Unary* ast) {
    typecheck(mod, env, ast->child);
    TypeKind tk = concrete(ast->child->type)->kind;
    if (tk != T_NUMERIC && tk != T_PTR && tk != T_STRING && tk != T_CHAR && tk != T_FUN && tk != T_BOOL)
        fatal("Unexpected type in equality expression.");
    ast->type = BOOL;
}

void check_equality(Module* mod, Env* env, Binary* ast) {
    typecheck(mod, env, ast->left);
    typecheck(mod, env, ast->right);
    Type* result = unify(ast->left->type, ast->right->type);
    TypeKind tk = concrete(result)->kind;
    if (tk != T_NUMERIC && tk != T_PTR && tk != T_STRING && tk != T_CHAR && tk != T_FUN && tk != T_BOOL)
        fatal("Unexpected type in equality expression.");
    ast->left = coerce(mod, env, ast->left, result), ast->right = coerce(mod, env, ast->right, result);
    ast->type = BOOL;
}

void check_assign(Module* mod, Env* env, Binary* ast) {
    typecheck(mod, env, ast->left);
    typecheck(mod, env, ast->right);
    ast->right = coerce(mod, env, ast->right, ast->left->type);
    if (!ast->right) fatal("Incompatible types in assignment.");
    ast->type = UNIT;
}

void check_arith_assign(Module* mod, Env* env, Binary* ast) {
    typecheck(mod, env, ast->left);
    typecheck(mod, env, ast->right);
    ast->right = coerce(mod, env, ast->right, ast->left->type);
    if (!ast->right) fatal("Incompatible types in assignment.");
    if (concrete(ast->left->type)->kind != T_NUMERIC)
        fatal("Expected numeric type in arithmetic assignment.");
    ast->type = UNIT;
}

void check_bitwise_assign(Module* mod, Env* env, Binary* ast) {
    typecheck(mod, env, ast->left);
    typecheck(mod, env, ast->right);
    ast->right = coerce(mod, env, ast->right, ast->left->type);
    if (!ast->right) fatal("Incompatible types in assignment.");
    if (concrete(ast->left->type)->kind != T_NUMERIC || ((NumericType*)ast->left->type)->floating)
        fatal("Expected integer type in arithmetic assignment.");
    ast->type = UNIT;
}

bool add_implicit_returns(Module* mod, Env* env, Type* dst, AST*& ast) {
    switch (ast->kind) {
    casematch(AST_DO, List*, l)
        if (l->items.n == 0) return true;
        else return add_implicit_returns(mod, env, dst, l->items[l->items.n - 1]);
        endmatch
    casematch(AST_IF, If*, i)
        if (!i->ifFalse) return true; // if statement must have both branches for implicit return to be valid.
        bool branch = add_implicit_returns(mod, env, dst, i->ifTrue);
        if (i->ifFalse) branch = branch && add_implicit_returns(mod, env, dst, i->ifFalse);
        return branch;
        endmatch
    casematch(AST_RETURN, Unary*, u)
        AST* retexpr = coerce(mod, env, u->child, dst);
        if (!retexpr) return false;
        u->child = retexpr;
        return true;
        endmatch
    case AST_FIRST_EXPR ... AST_LAST_EXPR - 1: {
        AST* retexpr = coerce(mod, env, ast, dst);
        if (!retexpr) return false;
        ast = new(mod->parser->astspace) Unary(AST_RETURN, ast->pos, retexpr);
        return true;
    }
    default:
        return true;
    }
}

void typecheck(Module* mod, Env* env, AST* ast) {
    switch (ast->kind) {
    casematch(AST_PROGRAM, ASTProgram*, p)
        for (Statement* stmt : p->toplevel) typecheck(mod, p->env, stmt);
        endmatch
    casematch(AST_ICONST, Const*, c) c->type = ICONST; endmatch
    casematch(AST_FCONST, Const*, c) c->type = FCONST; endmatch
    casematch(AST_CHCONST, Const*, c) c->type = CHAR; endmatch
    casematch(AST_STRCONST, Const*, c) c->type = STRING; endmatch
    casematch(AST_BOOL, Const*, c) c->type = BOOL; endmatch
    casematch(AST_UNIT, Const*, c) c->type = UNIT; endmatch
    casematch(AST_VAR, Var*, v)         // Handle typenames (T).
        Entry* e = env->lookup(v->name);
        if (!e) fatal("Undefined variable.");
        v->type = e->type;
        v->type->referenced_by_name = true;
        endmatch
    
    casematch(AST_PLUS, Unary*, u) check_arithmetic(mod, env, u); endmatch
    casematch(AST_NEG, Unary*, u) check_arithmetic(mod, env, u); endmatch
    casematch(AST_DEREF, Unary*, u)     // Handle pointer types (*T)
        typecheck(mod, env, u->child);
        if (u->child->type->kind != T_PTR) fatal("Tried to dereference non-pointer type.");
        u->type = ((PtrType*)u->child->type)->target;
        endmatch
    casematch(AST_ADDROF, Unary*, u) 
        typecheck(mod, env, u->child);
        // TODO: check for lvalue
        u->type = mod->typectx->def<PtrType>(u->child->type);
        endmatch
    casematch(AST_NOT, Unary*, u) check_logic(mod, env, u); endmatch
    casematch(AST_BITNOT, Unary*, u) check_bitwise(mod, env, u); endmatch
    casematch(AST_INCR, Unary*, u) check_arithmetic(mod, env, u); endmatch // TODO: check for lvalue
    casematch(AST_DECR, Unary*, u) check_arithmetic(mod, env, u); endmatch
    casematch(AST_POSTINCR, Unary*, u) check_arithmetic(mod, env, u); endmatch
    casematch(AST_POSTDECR, Unary*, u) check_arithmetic(mod, env, u); endmatch
    casematch(AST_NEW, Unary*, u)
        typecheck(mod, env, u->child);
        u->type = mod->typectx->def<PtrType>(u->child->type);
        mod->cnew_types.insert(u->child->type);
        concrete(u->child->type)->gen_placement_new = true;
        endmatch
    casematch(AST_PAREN, Unary*, u) 
        typecheck(mod, env, u->child);
        u->type = u->child->type;
        endmatch

    casematch(AST_ADD, Binary*, b) check_arithmetic(mod, env, b); endmatch
    casematch(AST_SUB, Binary*, b) check_arithmetic(mod, env, b); endmatch
    casematch(AST_STAR, Binary*, b) check_arithmetic(mod, env, b); endmatch
    casematch(AST_DIV, Binary*, b) check_arithmetic(mod, env, b); endmatch
    casematch(AST_MOD, Binary*, b) check_arithmetic(mod, env, b); endmatch
    casematch(AST_EXP, Binary*, b) 
        check_arithmetic(mod, env, b); 

        AST* fn = new(mod->parser->astspace) Var(b->pos, mod->interner->intern("$pow"));
        slice<AST*> args = { new(mod->parser->astspace) AST*[2], 2 };
        args[0] = b->left;
        args[1] = b->right;
        b->left->type = b->type;
        b->right->type = b->type;
        b->left = new(mod->parser->astspace) Apply(b->pos, fn, args);
        typecheck(mod, env, b->left);
        b->type = b->left->type;
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
        typecheck(mod, env, b->left);
        Env* nenv = ast_env(env, b->left);
        if (nenv) { // Static field or module access.
            typecheck(mod, nenv, b->right);
            b->type = b->right->type;
        }
        else if ((nenv = type_env(b->left->type))) { // Field access instead.
            if (b->left->type->kind == T_PTR) nenv = type_env(((PtrType*)b->left->type)->target);
            typecheck(mod, nenv, b->right);
            b->type = b->right->type;
        }
        else unreachable("Left hand side of dot operator should have an environment.");
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

    casematch(AST_ARRAYTYPE, Binary*, b) b->type = TYPE; endmatch // This prevents type expressions from validly mixing with normal expressions.
    casematch(AST_SLICETYPE, Binary*, b) b->type = TYPE; endmatch
    casematch(AST_TYPENAME, Var*, b) b->type = TYPE; endmatch
    casematch(AST_FUNTYPE, Apply*, b) b->type = TYPE; endmatch
    casematch(AST_TYPEDOT, Binary*, b) b->type = TYPE; endmatch
    casematch(AST_TYPEINST, Apply*, a) a->type = TYPE; endmatch
    casematch(AST_PTRTYPE, Unary*, b) b->type = TYPE; endmatch

    casematch(AST_INDEX, Binary*, b)    // Handle array types (T[N]) and slice types (T[])
        typecheck(mod, env, b->left);
        typecheck(mod, env, b->right);
        if (b->right->type->kind != T_NUMERIC || ((NumericType*)b->right->type)->floating)
            fatal("Expected integer type in index expression.");
        if (b->left->type->kind == T_ARRAY) b->type = ((ArrayType*)b->left->type)->element;
        else if (b->left->type->kind == T_SLICE) b->type = ((SliceType*)b->left->type)->element;
        else fatal("Expected array or slice type in index expression.");
        endmatch
    casematch(AST_SLICE, Slice*, s)
        typecheck(mod, env, s->array);
        if (s->low) {
            typecheck(mod, env, s->low);
            if (s->low->type->kind != T_NUMERIC || ((NumericType*)s->low->type)->floating)
                fatal("Expected integer type in index expression.");
        }
        if (s->high) {
            typecheck(mod, env, s->high);
            if (s->high->type->kind != T_NUMERIC || ((NumericType*)s->high->type)->floating)
                fatal("Expected integer type in index expression.");
        }
        Type* arrt = concrete(s->array->type);
        if (arrt->kind == T_ARRAY) s->type = mod->typectx->def<SliceType>(((ArrayType*)arrt)->element);
        else if (arrt->kind == T_SLICE) s->type = s->array->type;
        else fatal("Expected array or slice type in slice expression.");
        endmatch
    casematch(AST_APPLY, Apply*, a)
        vec<Type*, 256, arena> argv;
        argv.alloc = &mod->typectx->typespace;
        for (AST* ast : a->args) {
            if (!ast->type) typecheck(mod, env, ast);
            argv.push(ast->type);
        }
        
        Env* e = call_parent(mod, a);
        if (!e) e = env;
        if (a->fn->kind == AST_VAR) {
            Entry* en = e->lookup(((Var*)a->fn)->name);
            if (en && en->kind == E_GENFUN) {
                FunDecl* d = (FunDecl*)en->ast;
                if (((Apply*)d->proto)->args.n != a->args.size()) 
                    fatal("Incorrect number of arguments for generic function.");
                FunDecl* inst = inst_fun(mod, env, d, a->args);
                typecheck(mod, env, inst);
                ((Var*)a->fn)->name = inst->basename;
            }
        }
        typecheck(mod, e, a->fn);
        if (concrete(a->fn->type)->kind != T_FUN) fatal("Expected function type in call expression.");
        if (argv.size() == 0) argv.push(UNIT);
        slice<Type*>& argt = ((FunType*)concrete(a->fn->type))->arg;
        if (argt.n != argv.size()) {
            fatal("Incorrect number of arguments for function.");
        }
        if (a->args.n == 0) {
            if (argt.n != 1 || concrete(argt[0]) != UNIT) fatal("Incompatible argument for function.");
        }
        else for (i32 i = 0; i < argt.n; i ++) {  
            a->args[i] = coerce(mod, env, a->args[i], argt[i]);
            if (!a->args[i]) 
                fatal("Incompatible argument for function.");
        }
        a->type = ((FunType*)concrete(a->fn->type))->ret;
        endmatch
    casematch(AST_ARRAY, List*, l) 
        for (AST* ast : l->items) typecheck(mod, env, ast);
        if (l->items.size() == 0) l->type = mod->typectx->def<ArrayType>(mod->typectx->defvar(mod, env), 0);
        else {
            Type* elt = l->items[0]->type;
            for (i32 i = 1; i < l->items.n; i ++)
                elt = unify(elt, l->items[i]->type);
            for (i32 i = 0; i < l->items.n; i ++) {
                l->items[i] = coerce(mod, env, l->items[i], elt);
                if (!l->items[i]) fatal("Incompatible element type in array constructor.");
            }
            l->type = mod->typectx->def<ArrayType>(elt, l->items.n);
        }
        concrete(l->type)->ctor_called = true;
        endmatch
    casematch(AST_SET, List*, l) 
        for (AST* ast : l->items) typecheck(mod, env, ast);
        endmatch
    casematch(AST_PTRDECL, Binary*, d)
        endmatch
    casematch(AST_VARDECL, VarDecl*, d)
        if (d->init) {
            typecheck(mod, env, d->init);
            Type* orig = d->init->type;
            d->init = coerce(mod, env, d->init, d->type);
            if (!d->init) fatal("Incompatible initializer in variable declaration.");
            if (d->type->kind == T_VAR && d->init->type != d->type) {
                d->init = new(mod->parser->astspace) Unary(AST_CONV, d->init->pos, d->init);
                d->init->type = d->type;
            }
        }
        endmatch
    casematch(AST_FUNDECL, FunDecl*, d) 
        if (!d->generic) {
            slice<AST*>& args = ((Apply*)d->proto)->args;
            for (AST* ast : args) typecheck(mod, d->env, ast);
            if (d->body) {
                typecheck(mod, d->env, d->body); 
                if (!add_implicit_returns(mod, env, ((FunType*)d->type)->ret, d->body))
                    fatal("Incompatible function body for return type.");
            }
        }
        else d->type = TYPE;
        for (const auto& e : d->insts) typecheck(mod, env, e.value);
        endmatch
    casematch(AST_CTOR, Apply*, a)
        if (!is_type(a->fn)) unreachable("Expected typename in constructor call.");
        Type* t = concrete(a->fn->type); // We assume it's a typename.
        switch (t->kind) {
            case T_CHAR:
                if (a->args.n != 1) fatal("Expected single parameter in casting constructor.");
                typecheck(mod, env, a->args[0]);
                if (concrete(a->args[0]->type)->kind != T_NUMERIC || ((NumericType*)concrete(a->args[0]->type))->floating)
                    fatal("Can only construct char from integers.");
                break;
            case T_STRING: {
                if (a->args.n != 1) fatal("Expected single parameter in casting constructor.");
                typecheck(mod, env, a->args[0]);
                Type* dst = mod->typectx->def<SliceType>(I8);
                a->args[0] = coerce(mod, env, a->args[0], dst);
                if (!a->args[0]) fatal("Can only construct string from byte slice.");
                break;
            }
            case T_SLICE: 
                if (a->args.n != 1) fatal("Expected single parameter in casting constructor.");
                typecheck(mod, env, a->args[0]);
                if (is_subtype_generic(((SliceType*)t)->element, I8) && is_subtype_generic(a->args[0]->type, STRING)) {}
                else if (concrete(a->args[0]->type)->kind == T_ARRAY && is_subtype(a->args[0]->type, t)) {}
                else fatal("Can only construct slice from array or string.");
                break;
            case T_NAMED: {
                Type* inner = ((NamedType*)t)->inner;
                if (a->args.n != 1) fatal("Expected single parameter in casting constructor.");
                typecheck(mod, env, a->args[0]);
                a->args[0] = coerce(mod, env, a->args[0], inner);
                if (!a->args[0]) fatal("Cannot construct named type from argument.");
                t->ctor_called = true;
                break;
            }
            case T_STRUCT: {
                slice<pair<i32, Type*>>& fields = ((StructType*)t)->fields;
                if (a->args.n != fields.n) fatal("Incorrect number of constructor arguments for struct.");
                for (AST* arg : a->args) typecheck(mod, env, arg);
                for (i32 i = 0; i < a->args.n; i ++) {
                    a->args[i] = coerce(mod, env, a->args[i], fields[i].second);
                    if (!a->args[i]) fatal("Incompatible field in struct constructor.");
                }
                t->ctor_called = true;
                break;
            }
            case T_UNION:
                fatal("Cannot construct union directly. Construct a case instead.");
                break;
            default:
                if (a->args.n != 1) fatal("Expected single parameter in casting constructor.");
                typecheck(mod, env, a->args[0]);
                a->args[0] = coerce(mod, env, a->args[0], t);
                if (!a->args[0]) fatal("Cannot cast value to desired type.");
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
        d->type = d->items[d->items.n - 1]->type;
        endmatch
    casematch(AST_IF, If*, i) 
        typecheck(mod, env, i->cond);
        i->cond = coerce(mod, env, i->cond, BOOL);
        if (!i->cond) fatal("Condition in if statement is not convertible to bool.");
        typecheck(mod, env, i->ifTrue);
        if (i->ifFalse) {
            typecheck(mod, env, i->ifFalse);
            Type* unified = unify(i->ifTrue->type, i->ifFalse->type);
            i->ifTrue = coerce(mod, env, i->ifTrue, unified);
            i->ifFalse = coerce(mod, env, i->ifFalse, unified);
            if (!i->ifTrue || !i->ifFalse)
                fatal("Incompatible types in branches of if statement.");
            else i->type = unified;
        }
        else i->type = VOID;
        endmatch
    casematch(AST_WHILE, Binary*, b) 
        typecheck(mod, env, b->left);
        b->left = coerce(mod, env, b->left, BOOL);
        if (!b->left) fatal("Condition in while statement is not convertible to bool.");
        typecheck(mod, env, b->right);
        b->type = VOID;
        endmatch
    casematch(AST_UNTIL, Binary*, b) 
        typecheck(mod, env, b->left);
        b->left = coerce(mod, env, b->left, BOOL);
        if (!b->left) fatal("Condition in until statement is not convertible to bool.");
        typecheck(mod, env, b->right);
        b->type = UNIT;
        endmatch
    casematch(AST_FOR, For*, f) 
        typecheck(mod, f->env, f->body);
        f->type = VOID;
        endmatch
    casematch(AST_DEFER, Unary*, u) 
        typecheck(mod, env, u->child);
        u->type = VOID;
        endmatch
    casematch(AST_RETURN, Unary*, u) 
        typecheck(mod, env, u->child);
        if (env->kind != ENV_FUN) fatal("Return statements are not permitted outside of functions.");
        Entry* e = env->parent->lookup(env->name);
        if (!e || e->kind != E_FUN) unreachable("Could not find definition for enclosing function.");
        Type* ret = e->type;
        if (ret->kind != T_FUN) unreachable("Function declaration somehow isn't a function type.");
        ret = ((FunType*)ret)->ret;
        u->child = coerce(mod, env, u->child, ret);
        if (!u->child) fatal("Returned value is incompatible with function return type.");
        u->type = VOID;
        endmatch
    casematch(AST_BREAK, Expr*, e) 
        e->type = VOID;
        endmatch
    casematch(AST_CONTINUE, Expr*, e) 
        e->type = VOID;
        endmatch
    casematch(AST_WITH, With*, w) 
        typecheck(mod, env, w->bound);
        Env* env = type_env(w->bound->type);
        w->env->siblings.push(env);
        detect_types(mod, w->env, w->body);
        typecheck(mod, w->env, w->body);
        w->type = w->body->type;
        endmatch
    casematch(AST_USE, Binary*, b) 
        b->type = VOID;
        endmatch
    casematch(AST_ALIASDECL, AliasDecl*, d) 
        endmatch
    casematch(AST_TYPEDECL, TypeDecl*, d) 
        if (d->generic) {
            d->type = TYPE;
            for (const auto& e : d->insts) typecheck(mod, env, e.value);
        }
        else typecheck(mod, d->env, d->body);
        endmatch
    casematch(AST_CASEDECL, CaseDecl*, d) 
        typecheck(mod, d->env, d->body);
        endmatch
    casematch(AST_MATCH, Binary*, b) 
        endmatch
    casematch(AST_MODULEDECL, ModuleDecl*, b) 
        typecheck(mod, b->env, b->body);
        b->type = VOID;
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
        size ++; // $
    } 
    const_slice<i8> name = mod->interner->str(env->name);
    size += name.n;
    i8* fqname = (i8*)ctx.textspace.alloc(size);
    i8* writer = fqname;
    if (env->parent && env->parent->kind != ENV_ROOT && (env->parent->kind != ENV_GLOBAL)) {
        mcpy(writer, parent.ptr, parent.n);
        writer += parent.n;
        *writer = '$';
        writer ++;
    } 
    mcpy(writer, name.ptr, name.n);
    return env->fqname = mod->interner->intern(const_slice<i8>{fqname, size});
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
    else fatal("Type should have had mangled name by now.");
}

void emit_c_binary(Module* mod, Env* env, AST* left, AST* right, CContext& ctx, const i8* op) {
    emit_c(mod, env, left, ctx);
    write(ctx.c, op);
    emit_c(mod, env, right, ctx);
}

void emit_c_strcmp(Module* mod, Env* env, AST* left, AST* right, CContext& ctx, const i8* op) {
    write(ctx.c, "($strcmp(");
    emit_c(mod, env, left, ctx);
    write(ctx.c, ", ");
    emit_c(mod, env, right, ctx);
    write(ctx.c, ')', op, ')');
}

void emit_fqsym(stream& io, Module* mod, Env* env, Symbol sym, CContext& ctx) {
    env = env->find(sym);
    if (env && env->kind != ENV_ROOT && (env->kind != ENV_GLOBAL)) {
        i32 envname = env_fq_name(mod, env, ctx);
        write_sym(io, mod, envname);
        write(io, '$');
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
    write(ctx.h, "$_pnew(");
    emit_c_typename(ctx.h, mod, t, ctx);
    write(ctx.h, "* out, ");
    emit_c_typename(ctx.h, mod, t, ctx);
    write(ctx.h, " in) { *out = in; return out; }\n");
}

void emit_c_typedef(Module* mod, Type* t, CContext& ctx) {
    i32 len = 0;
    t = simplify(t);
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
            i32 newlen = target_name.size() + 2;
            i8* name = (i8*)ctx.textspace.alloc(newlen);
            mcpy(name, target_name.ptr, target_name.size());
            mcpy(name + target_name.size(), "$P", 2);
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
            i32 newlen = element_name.size() + 2; // strlen("$slice$")
            i8* name = (i8*)ctx.textspace.alloc(newlen);
            mcpy(name, element_name.ptr, element_name.size());
            mcpy(name + element_name.size(), "$S", 2);
            t->mangled = mod->interner->intern(const_slice<i8>{name, newlen});

            if (concrete(element) != I8) { // Byte slices are defined in cclover.h
                write(ctx.h, "$clover_slice_def(");
                emit_fqsym(ctx.h, mod, type_env(element), type_env(element)->name, ctx);
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
            i32 array_name_len = element_name.size() + 2; // strlen("$array$");
            i8* name = (i8*)ctx.textspace.alloc(newlen);
            mcpy(name, element_name.ptr, element_name.size());
            mcpy(name + element_name.size(), "$A", 2);
            name[array_name_len ++] = '$';
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

            write(ctx.h, "$clover_array_def(");
            emit_fqsym(ctx.h, mod, type_env(element), type_env(element)->name, ctx);
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
            for (const auto& p : ((StructType*)t)->fields) {
                if (p.second->mangled == -1) emit_c_typedef(mod, p.second, ctx);
            }
            t->mangled = env_fq_name(mod, t->env, ctx);
            indent(ctx.h, ctx.h_indent); write(ctx.h, "typedef struct {\n");
            ctx.h_indent += 4;
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
                write(ctx.h, "$_new(");
                for (const auto& p : ((StructType*)t)->fields) {
                    if (&p != &((StructType*)t)->fields[0]) write(ctx.h, ", ");
                    emit_c_typename(ctx.h, mod, p.second, ctx);
                    write(ctx.h, ' ');
                    write_sym(ctx.h, mod, p.first);
                    write(ctx.h, "_in");
                }
                write(ctx.h, ") {\n");
                write(ctx.h, "    "), emit_c_typename(ctx.h, mod, t, ctx), write(ctx.h, " out;\n");
                for (const auto& p : ((StructType*)t)->fields) {
                    write(ctx.h, "    out.");
                    emit_fqsym(ctx.h, mod, t->env, p.first, ctx);
                    write(ctx.h, " = ");
                    write_sym(ctx.h, mod, p.first);
                    write(ctx.h, "_in;\n");
                }
                write(ctx.h, "    return out;\n");
                write(ctx.h, "}\n\n");
            }
            break;
        }
        case T_UNION: {
            vec<i32, 256, arena> cases;
            cases.alloc = &ctx.textspace;
            for (auto& e : t->env->entries) {
                if (e.value.kind == E_CASE) {
                    if (e.value.type->mangled == -1) emit_c_typedef(mod, e.value.type, ctx);
                    cases.push(e.value.type->mangled);
                }
            }
            t->mangled = env_fq_name(mod, t->env, ctx);
            indent(ctx.h, ctx.h_indent); write(ctx.h, "typedef enum {\n");
            ctx.h_indent += 4;
            for (i32 i : cases) 
                indent(ctx.h, ctx.h_indent), emit_fqsym(ctx.h, mod, t->env, i, ctx), write(ctx.h, "$_tag,\n");
            ctx.h_indent -= 4;
            indent(ctx.h, ctx.h_indent); write(ctx.h, "} ");
            write_sym(ctx.h, mod, env_fq_name(mod, t->env, ctx));
            write(ctx.h, "$_tag;\n");
            indent(ctx.h, ctx.h_indent); write(ctx.h, "typedef struct {\n");
            ctx.h_indent += 4;
            indent(ctx.h, ctx.h_indent);
            write_sym(ctx.h, mod, env_fq_name(mod, t->env, ctx));
            write(ctx.h, "$_tag $tag;\n");
            indent(ctx.h, ctx.h_indent); write(ctx.h, "union {\n");
            ctx.h_indent += 4;
            for (i32 i : cases) {
                indent(ctx.h, ctx.h_indent), emit_fqsym(ctx.h, mod, t->env, i, ctx), write(ctx.h, " ");
                emit_fqsym(ctx.h, mod, t->env, i, ctx), write(ctx.h, "$_var;\n");
            }
            ctx.h_indent -= 4;
            indent(ctx.h, ctx.h_indent); write(ctx.h, "};\n");
            ctx.h_indent -= 4;
            indent(ctx.h, ctx.h_indent);write(ctx.h, "} ");
            write_sym(ctx.h, mod, t->mangled);
            write(ctx.h, ";\n");
            for (auto& e : t->env->entries) {
                if (e.value.kind == E_CASE) {
                    indent(ctx.h, ctx.h_indent), write(ctx.h, "inline ");
                    emit_c_typename(ctx.h, mod, t, ctx);
                    write(ctx.h, ' ');
                    emit_c_typename(ctx.h, mod, t, ctx);
                    write(ctx.h, "$_from_$");
                    write_sym(ctx.h, mod, e.value.type->mangled);
                    write(ctx.h, '(');
                    emit_c_typename(ctx.h, mod, e.value.type, ctx);
                    write(ctx.h, " in) {\n");
                    ctx.h_indent += 4;
                    indent(ctx.h, ctx.h_indent), emit_c_typename(ctx.h, mod, t, ctx);
                    write(ctx.h, " out;\n");
                    indent(ctx.h, ctx.h_indent), write(ctx.h, "out.$tag = "), emit_c_typename(ctx.h, mod, e.value.type, ctx), write(ctx.h, "$_tag;\n");
                    indent(ctx.h, ctx.h_indent), write(ctx.h, "out."), emit_c_typename(ctx.h, mod, e.value.type, ctx), write(ctx.h, "$_var = in;\n");
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
            if (((NamedType*)t)->inner->mangled == -1)
                emit_c_typedef(mod, ((NamedType*)t)->inner, ctx);
            t->mangled = env_fq_name(mod, t->env, ctx);
            indent(ctx.h, ctx.h_indent); write(ctx.h, "typedef struct {\n");

            ctx.h_indent += 4;
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
            write(ctx.h, "$_new(");
            emit_c_typename(ctx.h, mod, ((NamedType*)t)->inner, ctx);
            write(ctx.h, " in) {\n");
            write(ctx.h, "    "), emit_c_typename(ctx.h, mod, t, ctx), write(ctx.h, " out;\n");
            write(ctx.h, "    "), write(ctx.h, "out."), emit_fqsym(ctx.h, mod, t->env, mod->interner->intern("this"), ctx);
            write(ctx.h, " = in;\n");
            write(ctx.h, "    return out;\n");
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
                arg_lens += mod->interner->str(a->mangled).n + 1;
            }

            const const_slice<i8>& return_name = mod->interner->str(((FunType*)t)->ret->mangled);
            i32 newlen = return_name.size() + arg_lens + 28;
            i32 fn_name_len = return_name.size() + 2; // strlen("$array$");
            i8* name = (i8*)ctx.textspace.alloc(newlen);
            mcpy(name, return_name.ptr, return_name.size());
            mcpy(name + return_name.size(), "$F", 2);
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
                name[fn_name_len ++] = '$';
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
                if (concrete(a) != UNIT) {
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

void emit_c_types(Module* mod, Env* env, CContext& ctx) {
    for (Module* dep : mod->deps) {
        write(ctx.h, "#include \"", (const i8*)dep->hpath, "\"\n");
    }
    map<TypeKey, Type*> concrete_types;
    map<TypeKey, Type*> type_manglings;
    for (auto& entry : mod->typectx->typemap) {
        Type* t = fullconcrete(*mod->typectx, entry.value);
        t->ctor_called = entry.value->ctor_called;
        t->referenced_by_name = entry.value->referenced_by_name;
        t->gen_placement_new = entry.value->gen_placement_new;
        auto it = concrete_types.find(t);
        if (concrete_types.find(t) == concrete_types.end()) {
            concrete_types.put(t, t); 
            type_manglings.put(entry.value, t);
        }
        else type_manglings.put(entry.value, it->value);
    }
    for (auto& entry : concrete_types) if (entry.value->mangled == -1) emit_c_typedef(mod, entry.value, ctx);
    for (auto& entry : type_manglings) if (entry.key.type->mangled == -1) entry.key.type->mangled = entry.value->mangled;
    for (Type* t : mod->cnew_types) if (t->gen_placement_new) emit_c_placement_new(mod, t, ctx);
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
    casematch(AST_FUNDECL, FunDecl*, d) 
        if (d->generic) {
            for (const auto& e : d->insts) emit_c(mod, env, e.value, ctx);
            return;
        }
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
                emit_fqsym(ctx.c, mod, d->env, ((VarDecl*)p)->basename, ctx);
            }
            write(ctx.c, ") {\n");
            ctx.c_indent += 4;
            indent(ctx.c, ctx.c_indent), emit_c(mod, d->env, d->body, ctx), write(ctx.c, ";\n");
            ctx.c_indent -= 4;
            indent(ctx.c, ctx.c_indent), write(ctx.c, "}");
        }
        endmatch
    default:
        break;
    }
}

void emit_c_toplevel(Module* mod, Env* env, AST* ast, CContext& ctx, vec<AST*, 512, arena>& main) {
    if (ast->kind >= AST_FIRST_DECL && ast->kind < AST_LAST_DECL) {
        if (ast->kind == AST_FUNDECL && ((FunDecl*)ast)->generic) {
            for (const auto& e : ((FunDecl*)ast)->insts) emit_c_toplevel(mod, env, e.value, ctx, main);
            return;
        }
        emit_c(mod, env, ast, ctx), write(ctx.c, ";\n"), write(ctx.h, ";\n");
        if (ast->kind == AST_VARDECL && ((VarDecl*)ast)->init) {
            AST* assign = new(mod->parser->astspace) Binary(AST_ASSIGN, ast->pos, ((VarDecl*)ast)->name, ((VarDecl*)ast)->init);
            assign->type = VOID;
            main.push(assign);
        }
    }
    else if (ast->kind == AST_DO) {
        for (AST* ast : ((List*)ast)->items) emit_c_toplevel(mod, env, ast, ctx, main);
    }
    else if (mod == ctx.main) main.push(ast);
}

void emit_c(Module* mod, Env* env, AST* ast, CContext& ctx) {
    switch (ast->kind) {
    casematch(AST_PROGRAM, ASTProgram*, p)
        vec<AST*, 512, arena> main;
        main.alloc = &ctx.textspace;
        for (AST* ast : p->toplevel) emit_c_toplevel(mod, env, ast, ctx, main);
        if (mod == mod->cloverinst->main) {
            write(ctx.c, "int main(int argc, char** argv) {\n");
            ctx.c_indent += 4;
            indent(ctx.c, ctx.c_indent), write(ctx.c, "$init();\n");
            for (AST* ast : main) indent(ctx.c, ctx.c_indent), emit_c(mod, env, ast, ctx), write(ctx.c, ";\n");
            indent(ctx.c, ctx.c_indent), write(ctx.c, "$deinit();\n");
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
        write(ctx.c, "$new(");
        emit_c_typename(ctx.c, mod, ((PtrType*)u->type)->target, ctx);
        write(ctx.c, ", ");
        emit_c(mod, env, u->child, ctx);
        write(ctx.c, ')');
        endmatch
    casematch(AST_PAREN, Unary*, u) 
        write(ctx.c, '(');
        emit_c(mod, env, u->child, ctx);
        write(ctx.c, ')');
        endmatch
    casematch(AST_ARRAY, List*, l) 
        emit_c_typename(ctx.c, mod, l->type, ctx);
        write(ctx.c, "_new(");
        for (AST* ast : l->items) {
            if (ast != l->items[0]) write(ctx.c, ", ");
            emit_c(mod, env, ast, ctx);
        }
        write(ctx.c, ")");
        endmatch
    casematch(AST_SLICE, Slice*, s)
        Type* arrt = concrete(s->array->type);
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
        Type* dest = concrete(u->type);
        if (dest == concrete(u->child->type)) return emit_c(mod, env, u->child, ctx);
        switch(dest->kind) {
        case T_SLICE:
            if (u->child->type->kind == T_ARRAY) {
                write(ctx.c, '('); emit_c_typename(ctx.c, mod, dest, ctx); write(ctx.c, "){(");
                emit_c(mod, env, u->child, ctx);
                write(ctx.c, ").ptr, ", ((ArrayType*)u->child->type)->size, '}');
            }
            break;
        case T_UNION:
            emit_c_typename(ctx.c, mod, dest, ctx);
            write(ctx.c, "$_from_$");
            emit_c_typename(ctx.c, mod, u->child->type, ctx);
            write(ctx.c, '(');
            emit_c(mod, env, u->child, ctx);
            write(ctx.c, ')');
            break;
        default:
            write(ctx.c, "((");
            emit_c_typename(ctx.c, mod, dest, ctx);
            write(ctx.c, ')');
            emit_c(mod, env, u->child, ctx);
            write(ctx.c, ')');
            break;
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
        if (concrete(b->left->type)->kind == T_STRING) emit_c_strcmp(mod, env, b->left, b->right, ctx, " == 0");
        else emit_c_binary(mod, env, b->left, b->right, ctx, " == "); 
        endmatch
    casematch(AST_INEQUAL, Binary*, b) 
        if (concrete(b->left->type)->kind == T_STRING) emit_c_strcmp(mod, env, b->left, b->right, ctx, " != 0");
        else emit_c_binary(mod, env, b->left, b->right, ctx, " != "); 
        endmatch
    casematch(AST_LESS, Binary*, b) 
        if (concrete(b->left->type)->kind == T_STRING) emit_c_strcmp(mod, env, b->left, b->right, ctx, " < 0");
        else emit_c_binary(mod, env, b->left, b->right, ctx, " < "); 
        endmatch
    casematch(AST_LEQUAL, Binary*, b) 
        if (concrete(b->left->type)->kind == T_STRING) emit_c_strcmp(mod, env, b->left, b->right, ctx, " <= 0");
        else emit_c_binary(mod, env, b->left, b->right, ctx, " <= "); 
        endmatch
    casematch(AST_GREATER, Binary*, b) 
        if (concrete(b->left->type)->kind == T_STRING) emit_c_strcmp(mod, env, b->left, b->right, ctx, " > 0");
        else emit_c_binary(mod, env, b->left, b->right, ctx, " > "); 
        endmatch
    casematch(AST_GEQUAL, Binary*, b) 
        if (concrete(b->left->type)->kind == T_STRING) emit_c_strcmp(mod, env, b->left, b->right, ctx, " >= 0");
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
        if (Env* e = call_parent(mod, a)) emit_c(mod, e, a->fn, ctx);
        else emit_c(mod, env, a->fn, ctx);
        bool first = true;
        write(ctx.c, '(');
        for (AST* a : a->args) {
            if (!first) write(ctx.c, ", ");

            if (concrete(a->type) != UNIT) {
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
            if (concrete(a->args[0]->type)->kind == T_STRING) {
                write(ctx.c, "$str_to_bytes(");
                emit_c(mod, env, a->args[0], ctx);
                write(ctx.c, ')');
            }
            break;
        case T_STRING:
            // Must be slice.
            write(ctx.c, "$bytes_to_str(");
            emit_c(mod, env, a->args[0], ctx);
            write(ctx.c, ')');
            break;
        case T_STRUCT:
        case T_NAMED: {
            emit_c_typename(ctx.c, mod, t, ctx);
            write(ctx.c, "$_new(");
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
            write(ctx.c, ')');
            emit_c(mod, env, a->args[0], ctx);
            write(ctx.c, ')');
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
    casematch(AST_IF, If*, i)
        write(ctx.c, "if (");
        emit_c(mod, env, i->cond, ctx);
        write(ctx.c, ") {\n");
        ctx.c_indent += 4;
        indent(ctx.c, ctx.c_indent), emit_c(mod, env, i->ifTrue, ctx);
        write(ctx.c, ";\n");
        ctx.c_indent -= 4;
        indent(ctx.c, ctx.c_indent), write(ctx.c, "}\n");
        indent(ctx.c, ctx.c_indent), write(ctx.c, "else {\n");
        ctx.c_indent += 4;
        indent(ctx.c, ctx.c_indent), emit_c(mod, env, i->ifFalse, ctx);
        write(ctx.c, ";\n");
        ctx.c_indent -= 4;
        indent(ctx.c, ctx.c_indent), write(ctx.c, "}");
        endmatch
    casematch(AST_WHILE, Binary*, b)
        write(ctx.c, "while (");
        emit_c(mod, env, b->left, ctx);
        write(ctx.c, ") {\n");
        ctx.c_indent += 4;
        indent(ctx.c, ctx.c_indent), emit_c(mod, env, b->right, ctx);
        write(ctx.c, ";\n");
        ctx.c_indent -= 4;
        indent(ctx.c, ctx.c_indent), write(ctx.c, "}\n");
        endmatch
    casematch(AST_FOR, For*, f)
        write(ctx.c, "{\n");
        ctx.c_indent += 4;
        indent(ctx.c, ctx.c_indent), emit_c(mod, f->env, f->body, ctx);
        write(ctx.c, ";\n");
        ctx.c_indent -= 4;
        indent(ctx.c, ctx.c_indent), write(ctx.c, "}\n");
        endmatch
    casematch(AST_UNTIL, Binary*, b)
        write(ctx.c, "while (!(");
        emit_c(mod, env, b->left, ctx);
        write(ctx.c, ")) {\n");
        ctx.c_indent += 4;
        indent(ctx.c, ctx.c_indent), emit_c(mod, env, b->right, ctx);
        write(ctx.c, ";\n");
        ctx.c_indent -= 4;
        indent(ctx.c, ctx.c_indent), write(ctx.c, "}\n");
        endmatch
    casematch(AST_BREAK, AST*, a)
        write(ctx.c, "break");
        endmatch
    casematch(AST_CONTINUE, AST*, a)
        write(ctx.c, "continue");
        endmatch
    casematch(AST_RETURN, Unary*, u)
        write(ctx.c, "return ");
        emit_c(mod, env, u->child, ctx);
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
    casematch(AST_FUNDECL, FunDecl*, d)
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
                emit_fqsym(ctx.c, mod, d->env, ((VarDecl*)p)->basename, ctx);
            }
            write(ctx.c, ") {\n");
            ctx.c_indent += 4;
            indent(ctx.c, ctx.c_indent), emit_c(mod, d->env, d->body, ctx), write(ctx.c, ";\n");
            ctx.c_indent -= 4;
            indent(ctx.c, ctx.c_indent), write(ctx.c, "}");
        }
        endmatch
    casematch(AST_TYPEDECL, TypeDecl*, d) // Handled in emit_typedef
        endmatch
    casematch(AST_MODULEDECL, ModuleDecl*, d)
        emit_c(mod, d->env, d->body, ctx);
        endmatch
    default:
        fatal("Unsupported expr!");
    }
}

#undef casematch