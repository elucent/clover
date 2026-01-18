#include "clover/parse.h"
#include "clover/lex.h"
#include "clover/error.h"
#include "util/config.h"
#include "util/str.h"

namespace clover {
    struct TokenFormat {
        Module* module;
        Token token;

        inline TokenFormat(Module* module_in, Token token_in):
            module(module_in), token(token_in) {}
    };

    template<typename IO, typename Format = Formatter<IO>>
    inline IO format_impl(IO io, const TokenFormat& tok) {
        return format(io, tok.module->str(tok.token.token));
    }

    struct TokenVisitor {
        Module* module;
        const vec<Token, 32>& tokens;
        vec<Pos, 8> whitespaceStack;
        u32 offset;

        // Used to ignore dedents that originated from indents in enclosed
        // expressions. Consider:
        //
        // (1 + 2 +
        //    3 + 4)
        //
        // We want this to act like one expression, but the lexer identifies
        // an indent before the 3. The line after the closing paren should be
        // dedented, but that dedent token will be after both the closing paren
        // and the newline. We want to skip exactly the number of escaping
        // dedents that originated in the enclosed expression - if we don't
        // skip enough, we'll see spurious dedents floating around and get
        // parse errors. If we don't have enough dedents after, that means the
        // line after the parenthetical is spuriously indented.
        u32 dedentsToIgnore;

        // If we finish parsing some enclosed sequence, and have ignorable
        // dedents left over, that implies that we actually have an indent - we
        // didn't return to the previous column. Until we work through these,
        // we return fake new indent tokens.
        u32 impliedIndents;

        bool justParsedLine;

        constexpr static u64 SkipIfEnclosedMask
            = 1ull << WhitespaceIndent
            | 1ull << WhitespaceDedent
            | 1ull << WhitespaceNewline;

        inline TokenVisitor(Module* module_in, const vec<Token, 32>& tokens_in):
            module(module_in), tokens(tokens_in), offset(0), dedentsToIgnore(0), impliedIndents(0), justParsedLine(false) {}

        Token peek() const {
            if UNLIKELY(offset >= tokens.size())
                return Token(MetaNone, Pos(0, 0));
            if UNLIKELY(impliedIndents && justParsedLine)
                return Token(WhitespaceIndent, tokens[offset].pos);
            return tokens[offset];
        }

        Token peek2() const {
            if UNLIKELY(offset + 1 >= tokens.size())
                return Token(MetaNone, Pos(0, 0));
            if UNLIKELY((impliedIndents && tokens[offset].token == WhitespaceNewline)
                || (impliedIndents > 1 && justParsedLine))
                return Token(WhitespaceIndent, tokens[offset + 1].pos);
            return tokens[offset + 1];
        }

        Token peekIgnoringWhitespace() const {
            u32 i = offset;
            while (i < tokens.size() && tokens[i].token.symbol < 64ull && (1ull << tokens[i].token.symbol & SkipIfEnclosedMask))
                i ++;
            if (i >= tokens.size())
                return Token(MetaNone, Pos(0, 0));
            return tokens[i];
        }

        Token last() const {
            return tokens[tokens.size() - 1];
        }

        Token readIgnoringWhitespace() {
            ignoreWhitespace(tokens[offset].pos);
            Token result = read();
            stopIgnoringWhitespace();
            return result;
        }

        Token read() {
            if UNLIKELY(offset >= tokens.size())
                return Token(MetaNone, Pos(0, 0));
            if UNLIKELY(impliedIndents && justParsedLine && !isIgnoringWhitespace()) {
                impliedIndents --;
                return Token { WhitespaceIndent, tokens[offset].pos };
            }
            justParsedLine = false;
            Token next = tokens[offset ++];
            if (isIgnoringWhitespace()) {
                while (offset < tokens.size() && tokens[offset].token.symbol < 64ull && (1ull << tokens[offset].token.symbol & SkipIfEnclosedMask)) {
                    if (tokens[offset].token == WhitespaceIndent)
                        dedentsToIgnore ++;
                    else if (tokens[offset].token == WhitespaceDedent)
                        dedentsToIgnore --;
                    offset ++;
                }
            } else if (dedentsToIgnore && next.token == WhitespaceNewline) {
                while (dedentsToIgnore && offset < tokens.size() && tokens[offset].token == WhitespaceDedent)
                    offset ++, dedentsToIgnore --;
                impliedIndents = dedentsToIgnore;
                justParsedLine = true;
            }
            return next;
        }

        void readUpTo(Symbol tok) {
            readUpToAndIncluding(tok);
            read();
        }

        void readUpToAndIncluding(Symbol tok) {
            while (!done() && peek().token != tok)
                read();
        }

        bool done() const {
            return offset >= tokens.size();
        }

        bool isIgnoringWhitespace() const {
            return whitespaceStack.size();
        }

        void ignoreWhitespace(Pos pos) {
            whitespaceStack.push(pos);
        }

        void stopIgnoringWhitespace() {
            whitespaceStack.pop();
        }
    };

    bool isHexDigit(i8 c) {
        return (c >= '0' && c <= '9')
            || (c >= 'a' && c <= 'f')
            || (c >= 'A' && c <= 'F');
    }

    u32 hexValueOf(i8 c) {
        if (c <= '9')
            return c - '0';
        else if (c <= 'F')
            return 10 + c - 'A';
        assert(c <= 'f');
        return 10 + c - 'a';
    }

    AST parsePrimaryNoSuffix(Module* module, TokenVisitor& visitor);
    AST parsePrimary(Module* module, TokenVisitor& visitor);
    AST parseExpression(Module* module, TokenVisitor& visitor, u32 minPrecedence = 0);
    AST parseExpressionOrJuxtaposition(Module* module, TokenVisitor& visitor, bool allowMultiple, bool allowFunction, bool allowInitializer);
    AST parseStatementInitial(Module* module, TokenVisitor& visitor);
    AST parseInlineStatement(Module* module, TokenVisitor& visitor);
    AST parseStatement(Module* module, TokenVisitor& visitor);

    Symbol escapeAndUnquote(Module* module, Pos pos, const_slice<i8> string) {
        vec<i8, 256> v;
        assert(string[0] == '"' || string[0] == '\'' || string[0] == '`');
        u32 i = 1;
        while (i < string.size() - 1) {
            if UNLIKELY(string[i] == '\\') {
                // Handle escapes...
                i ++;
                switch (string[i]) {
                    case 'a':
                        v.push('\a');
                        break;
                    case 'b':
                        v.push('\b');
                        break;
                    case 'f':
                        v.push('\f');
                        break;
                    case 'n':
                        v.push('\n');
                        break;
                    case 'r':
                        v.push('\r');
                        break;
                    case 't':
                        v.push('\t');
                        break;
                    case 'v':
                        v.push('\v');
                        break;
                    case 'x': {
                        i ++;
                        i8 a = string[i ++];
                        if UNLIKELY(!isHexDigit(a)) {
                            error(module, pos.withOffset(i - 1), "Expected hex digit (0-9, A-F, a-f) in \\x escape sequence, found '", a, "'.");
                            i ++;
                            break;
                        }
                        i8 b = string[i ++];
                        if UNLIKELY(!isHexDigit(b)) {
                            error(module, pos.withOffset(i - 1), "Expected hex digit (0-9, A-F, a-f) in \\x escape sequence, found '", b, "'.");
                            break;
                        }
                        v.push(i8(hexValueOf(a) << 4 | hexValueOf(b)));
                        break;
                    }
                    case 'u': {
                        i ++;
                        u32 acc = 0;
                        if (string[i] == '{') {
                            // Handle arbitrary-length escape sequence.
                            i ++;
                            while (isHexDigit(string[i]))
                                acc <<= 4, acc |= hexValueOf(string[i ++]);
                            if (string[i] != '}') {
                                error(module, pos.withOffset(i), "Expected hex digit (0-9, A-F, a-f) or '}' in \\u escape sequence, found '", string[i], "'.");
                                break;
                            }
                            i ++;
                        } else {
                            u32 acc = 0;
                            for (u32 j = 0; j < 4; j ++) {
                                if UNLIKELY(!isHexDigit(string[i])) {
                                    error(module, pos.withOffset(i), "Expected hex digit (0-9, A-F, a-f) in \\u escape sequence, found '", string[i], "'.");
                                    break;
                                }
                                acc <<= 4;
                                acc |= hexValueOf(string[i ++]);
                            }
                        }
                        rune r = acc;
                        i8 encoded[4];
                        auto length = utf8_encode(&r, 1, encoded, 4);
                        for (u32 i = 0; i < length; i ++)
                            v.push(encoded[i]);
                        break;
                    }
                    case '"':
                        v.push('"');
                        break;
                    case '\'':
                        v.push('\'');
                        break;
                    case '`':
                        v.push('`');
                        break;
                    case '\\':
                        v.push('\\');
                        break;
                    case '0':
                        v.push('\0');
                        break;
                    default:
                        error(module, pos.withOffset(i), "Invalid escape sequence '\\", string[i], "'.");
                        i ++;
                        break;
                }
            } else
                v.push(string[i]);
            i ++;
        }
        return module->sym((const_slice<i8>)v).symbol;
    }

    // Expressions are organized into the following abstract grammar:
    //
    //  - A PrimaryExprNoSuffix is a terminal or a compound expression uniquely
    //    distinguished by a particular token - this includes prefix operators
    //    like ++ or not, and also things like parentheticals.
    //
    //  - A PrimaryExpr is a PrimaryExprNoSuffix with any number of optional
    //    suffixes. These suffixes can be any of the following:
    //     - An indexing expression (i.e. foo[bar])
    //     - A call expression (i.e. foo(bar, baz))
    //     - A field access (i.e. foo.bar)
    //    All suffixes are left-associative - think of them as tacked onto a
    //    primary expression, even something like . that looks infix.
    //
    //  - A PostfixExpr is a PrimaryExpr with any number of postfix operators,
    //    i.e. increment and decrement.
    //
    //  - An Expr is a sequence of one or more PostfixExprs interlaced
    //    with binary operators. The child PostfixExprs are grouped based on
    //    the numerical precedence and associativity of the binary operators
    //    between them.
    //
    //  - A PrimaryStmt is either an Expr, or a special statement uniquely
    //    identified by a prefix operator or keyword, like `break` or
    //    `raise Foo`. They are distinctive in that they cannot contain other
    //    sub-statements, as a somewhat arbitrary restriction to prevent multi-
    //    line indented constructs like `if` or `for` from being used with wild
    //    abandon.
    //
    //  - An InlineStmt is a PrimaryStmt followed by any number of suffix
    //    statements. Suffix statements, similar to expression suffixes, behave
    //    like left-associative same-precedence binary operators.
    //
    //  - An InlineStmtOrThen is an InlineStmt or any number of InlineStmts
    //    joined by the binary `then` operator. This basically just serves to
    //    make `then` lower precedence than other inline statement keywords.
    //
    //  - A Stmt is an InlineStmtOrThen followed by a terminator, or a non-
    //    inline statement like prefix `if` or `while`. This is where most
    //    multi-line constructs are. Statements also include declarations, so
    //    large top-level constructs like function or type definitions, or
    //    sub-file module scopes are all parsed here.
    //
    //  - Finally, the TopLevel node is made up of any number of Stmts. That's
    //    a source file, broken down.

    AST parsePrimaryNoSuffix(Module* module, TokenVisitor& visitor) {
        Token token = visitor.peek();
        if (token.token.symbol < NumReservedSymbols) switch (token.token.symbol) {
            case PunctuatorLeftParen: {
                visitor.ignoreWhitespace(token.pos);
                visitor.read();
                if (visitor.peek().token == PunctuatorRightParen) {
                    visitor.stopIgnoringWhitespace();
                    visitor.read();
                    return module->add(ASTKind::Tuple, token.pos); // Empty tuple expression.
                }
                AST nextExpr = parseExpression(module, visitor);
                if (visitor.peek().token == PunctuatorComma) { // Tuple expression.
                    vec<AST> tup;
                    tup.push(nextExpr);
                    while (!visitor.done() && visitor.peek().token != PunctuatorRightParen) {
                        if UNLIKELY(visitor.peek().token != PunctuatorComma) {
                            error(module, visitor.peek(), "Expected comma in tuple expression, found '", TokenFormat(module, visitor.peek()), "'.");
                            visitor.stopIgnoringWhitespace();
                            visitor.readUpToAndIncluding(PunctuatorRightParen);
                            return module->add(ASTKind::Tuple, token.pos, tup);
                        }
                        visitor.read();
                        Pos beforePos = visitor.peek().pos;
                        tup.push(parseExpression(module, visitor));
                        if (visitor.peek().token == PunctuatorColon) {
                            Pos pos = visitor.read().pos;
                            if UNLIKELY(tup.back().kind() != ASTKind::Ident && tup.back().kind() != ASTKind::Missing)
                                error(module, beforePos, "Expected identifier in named tuple field, found '", tup.back(), "'.");
                            tup.back() = module->add(ASTKind::NamedParameter, token.pos, tup.back(), parseExpression(module, visitor));
                        }
                    }
                    visitor.stopIgnoringWhitespace();
                    if (visitor.done())
                        error(module, visitor.last(), "Unexpected end of file in tuple expression.");
                    else if (visitor.peek().token != PunctuatorRightParen) {
                        error(module, visitor.peek(), "Expected closing parenthese ')', found '", TokenFormat(module, visitor.peek()), "'.");
                        visitor.readUpToAndIncluding(PunctuatorRightParen);
                    } else
                        visitor.read();
                    return module->add(ASTKind::Tuple, token.pos, tup);
                }
                visitor.stopIgnoringWhitespace();
                if (visitor.done())
                    error(module, visitor.last().pos, "Unexpected end of file in parenthetical expression.");
                else if (visitor.peek().token != PunctuatorRightParen) {
                    error(module, visitor.peek().pos, "Expected closing parenthese ')', found '", TokenFormat(module, visitor.peek()), "'.");
                    visitor.readUpToAndIncluding(PunctuatorRightParen);
                } else {
                    visitor.read();
                }
                return module->add(ASTKind::Paren, token.pos, nextExpr);
            }
            case PunctuatorLeftBracket: {
                visitor.ignoreWhitespace(token.pos);
                visitor.read();
                if (visitor.peek().token == PunctuatorRightBracket) { // Empty list.
                    visitor.stopIgnoringWhitespace();
                    visitor.read();
                    return module->add(ASTKind::List, token.pos);
                }
                vec<AST> items;
                items.push(parseExpression(module, visitor));
                if (visitor.peek().token == PunctuatorComma) { // List expression.
                    while (!visitor.done() && visitor.peek().token != PunctuatorRightBracket) {
                        if UNLIKELY(visitor.peek().token != PunctuatorComma) {
                            error(module, visitor.peek(), "Expected comma in list expression, found '", TokenFormat(module, visitor.peek()), "'.");
                            visitor.stopIgnoringWhitespace();
                            visitor.readUpToAndIncluding(PunctuatorRightBracket);
                            return module->add(ASTKind::List, token.pos, items);
                        }
                        visitor.read();
                        items.push(parseExpression(module, visitor));
                    }
                }
                visitor.stopIgnoringWhitespace();
                if (visitor.done())
                    error(module, visitor.last().pos, "Unexpected end of file in list expression.");
                else if (visitor.peek().token != PunctuatorRightBracket) {
                    error(module, visitor.peek().pos, "Expected comma or closing bracket ']', found '", TokenFormat(module, visitor.peek()), "'.");
                    visitor.readUpToAndIncluding(PunctuatorRightBracket);
                } else {
                    visitor.read();
                }
                AST result = module->add(ASTKind::List, token.pos, items);
                return result;
            }
            case OperatorBitOr: {
                visitor.read();
                AST nextExpr = parsePrimary(module, visitor);
                if (visitor.done())
                    error(module, visitor.last().pos, "Unexpected end of file in length expression.");
                else if (visitor.peek().token != OperatorBitOr)
                    error(module, visitor.peek().pos, "Expected closing pipe '|', found '", TokenFormat(module, visitor.peek()), "'.");
                else
                    visitor.read();
                return module->add(ASTKind::Length, token.pos, nextExpr);
            }
            case OperatorIncr: {
                visitor.read();
                return module->add(ASTKind::PreIncr, token.pos, parsePrimary(module, visitor));
            }
            case OperatorDecr: {
                visitor.read();
                return module->add(ASTKind::PreDecr, token.pos, parsePrimary(module, visitor));
            }
            case OperatorAdd: {
                visitor.read();
                return module->add(ASTKind::Plus, token.pos, parsePrimary(module, visitor));
            }
            case OperatorSub: {
                visitor.read();
                return module->add(ASTKind::Minus, token.pos, parsePrimary(module, visitor));
            }
            case OperatorMul: {
                visitor.read();
                return module->add(ASTKind::Deref, token.pos, parsePrimary(module, visitor));
            }
            case OperatorBitNot: {
                visitor.read();
                return module->add(ASTKind::BitNot, token.pos, parsePrimary(module, visitor));
            }
            case KeywordNot: {
                visitor.read();
                return module->add(ASTKind::Not, token.pos, parsePrimary(module, visitor));
            }
            case OperatorBitAnd: {
                visitor.read();
                return module->add(ASTKind::AddressOf, token.pos, parsePrimary(module, visitor));
            }
            case KeywordTrue: {
                visitor.read();
                return module->add(ASTKind::Bool, Constant::BoolConst(true));
            }
            case KeywordFalse: {
                visitor.read();
                return module->add(ASTKind::Bool, Constant::BoolConst(false));
            }
            case KeywordNew: {
                visitor.read();
                return module->add(ASTKind::New, token.pos, parseExpression(module, visitor));
            }
            case KeywordOwn: {
                visitor.read();
                AST child;
                if (visitor.peek().token == KeywordUninit || visitor.peek().token == KeywordOwn)
                    child = parsePrimaryNoSuffix(module, visitor); // Group modifiers together.
                else
                    child = parseExpression(module, visitor);
                return module->add(ASTKind::OwnType, token.pos, child);
            }
            case KeywordUninit: {
                visitor.read();
                AST child;
                if (visitor.peek().token == KeywordUninit || visitor.peek().token == KeywordOwn)
                    child = parsePrimaryNoSuffix(module, visitor); // Group modifiers together.
                else
                    child = parseExpression(module, visitor);
                return module->add(ASTKind::UninitType, token.pos, child);
            }
            case OperatorEllipsis: {
                visitor.read();
                return module->add(ASTKind::Splat, token.pos, module->add(ASTKind::Missing));
            }
            default:
                error(module, token, "Unexpected token '", TokenFormat(module, token), "'.");
                visitor.read();
                return module->add(ASTKind::Missing);
        } else {
            const_slice<i8> text = module->str(token.token);
            assert(text.size() > 0);
            switch (text[0]) {
                case 'a' ... 'z':
                case 'A' ... 'Z':
                case '_':
                    visitor.read();
                    return module->add(ASTKind::Ident, Identifier(token.token));
                case '0' ... '9':
                    visitor.read();
                    for (i8 c : text) if (c == '.')
                        return module->add(ASTKind::Float, Constant::FloatConst(tofloat(text)));
                    return module->add(ASTKind::Unsigned, Constant::UnsignedConst(touint(text)));
                case '.':
                    // We know from above we aren't a reserved symbol like ., .., or ...; so
                    // we must be a decimal number that starts with the point.
                    visitor.read();
                    return module->add(ASTKind::Float, Constant::FloatConst(tofloat(text)));
                case '"': {
                    visitor.read();
                    return module->add(ASTKind::String, Constant::StringConst(escapeAndUnquote(module, token.pos, text)));
                }
                case '\'': {
                    visitor.read();
                    auto str = module->str(escapeAndUnquote(module, token.pos, text));
                    rune r;
                    utf8_decode(str.data(), str.size(), &r, 1);
                    return module->add(ASTKind::Char, Constant::CharConst(r.get()));
                }
                case '`':
                    visitor.read();
                    return module->add(ASTKind::Ident, Identifier(escapeAndUnquote(module, token.pos, text)));
                case 128 ... 255: {
                        visitor.read();
                    rune first;
                    utf8_decode(text.data(), text.size(), &first, 1);
                    if (utf8_is_digit(first)) {
                        for (i8 c : text) if (c == '.')
                            return module->add(ASTKind::Float, Constant::FloatConst(tofloat(text)));
                        return module->add(ASTKind::Unsigned, Constant::UnsignedConst(toint(text)));
                    } else
                        return module->add(ASTKind::Ident, Identifier(token.token));
                    fallthrough;
                }
                default:
                    error(module, token, "Unexpected token '", TokenFormat(module, token), "'.");
                    visitor.read();
                    return module->add(ASTKind::Missing);
            }
        }
    }

    AST parseIdentifier(Module* module, TokenVisitor& visitor) {
        Token token = visitor.read();
        const_slice<i8> text = module->str(token.token);
        assert(text.size() > 0);
        switch (text[0]) {
            case 'a' ... 'z':
            case 'A' ... 'Z':
            case '_':
                return module->add(ASTKind::Ident, Identifier(token.token));
            case '`':
                return module->add(ASTKind::Ident, Identifier(escapeAndUnquote(module, token.pos, text)));
            case 128 ... 255: {
                rune first;
                utf8_decode(text.data(), text.size(), &first, 1);
                if (!utf8_is_digit(first))
                    return module->add(ASTKind::Ident, Identifier(token.token));
                fallthrough;
            }
            default:
                error(module, token, "Unexpected token '", TokenFormat(module, token), "'.");
                return module->add(ASTKind::Missing);
        }
    }

    bool isIdentifier(Module* module, Token token) {
        if (token.token.symbol <= (u32)LastKeyword)
            return false;
        const_slice<i8> text = module->str(token.token);
        assert(text.size() > 0);
        switch (text[0]) {
            case 'a' ... 'z':
            case 'A' ... 'Z':
            case '_':
                return true;
            case '`':
                return true;
            case 128 ... 255: {
                rune first;
                utf8_decode(text.data(), text.size(), &first, 1);
                if (!utf8_is_digit(first))
                    return true;
                fallthrough;
            }
            default:
                return false;
        }
    }

    AST parsePrimarySuffix(Module* module, TokenVisitor& visitor, AST ast) {
        while (!visitor.done()) {
            // Tracks whether we've already seen a non-decimal constant. We
            // detect these here as a special case of integer coefficients,
            // i.e. 0x1234 would otherwise be parsed as 0 * x1234. We don't
            // want to allow this multiple times as in 0x1234x5678 though.

            bool allowNonDecimalConstant = true;
            Token token = visitor.peek();
            switch (token.token.symbol) {
                case PunctuatorLeftParen: { // Call
                    if (ast.kind() == ASTKind::List) {
                        // This is a hack that takes care of a very specific
                        // and obviously invalid situation. Consider the case
                        // we have a type like:
                        //
                        //  i32*[](i32)
                        //
                        // Parsing this normally, we'd expect `*` to bind less
                        // tightly than the call. But this means we need to dig
                        // deeply into the call to drag the [] back up once we
                        // know it's really meant to be a slice type. This is a
                        // big chore, and there is no situation in which you
                        // can apply a list literal anyway, so we just reject
                        // it in advance.
                        return ast;
                    }
                    visitor.ignoreWhitespace(token.pos);
                    visitor.read();
                    ASTKind callKind = ast.kind() == ASTKind::GetField ? ASTKind::CallMethod : ASTKind::Call;
                    vec<AST> arguments;
                    AST function = ast;
                    if (ast.kind() == ASTKind::GetField) {
                        arguments.push(ast.child(0));
                        function = ast.child(1);
                    }
                    if (visitor.peek().token == PunctuatorRightParen) {
                        visitor.stopIgnoringWhitespace();
                        visitor.read();
                        ast = module->add(callKind, token.pos, function, arguments);
                        break;
                    }
                    while (!visitor.done() && visitor.peek().token != PunctuatorRightParen) {
                        // Somewhat unusually, we permit juxtapositions and some
                        // other types of parameter definitions in call
                        // expressions. This covers two cases:
                        //  - Patterns, which support juxtapositions, but
                        //    otherwise look like calls.
                        //  - Function declarations that return pointers.
                        //    Consider i32* foo(type T, i32 x) - we sadly don't
                        //    know that this is a function declaration and not
                        //    a multiplication until name resolution.
                        Token token = visitor.peek();
                        AST argument;
                        if (token.token == KeywordType) {
                            // Type parameter.
                            visitor.read();
                            AST ident = parseIdentifier(module, visitor);

                            if (visitor.peek().token == PunctuatorLeftParen) {
                                error(module, visitor.peek().pos, "Type parameter list not permitted in type parameter declaration.");
                                visitor.readUpToAndIncluding(PunctuatorRightParen);
                            }

                            AST init = module->add(ASTKind::Missing);
                            if (visitor.peek().token == PunctuatorColon) {
                                visitor.read();
                                init = parseExpression(module, visitor);
                            }
                            argument = module->add(ASTKind::AliasDecl, token.pos, ident, module->add(ASTKind::Missing), init);
                        } else if (token.token == KeywordConst) {
                            // Const parameter.
                            visitor.read();
                            Pos namePos = visitor.peek().pos;
                            AST name = parseExpression(module, visitor);
                            AST type = module->add(ASTKind::Missing);
                            if (visitor.peek().token != PunctuatorRightParen && visitor.peek().token != PunctuatorComma)
                                type = name, namePos = visitor.peek().pos, name = parseIdentifier(module, visitor);
                            if (name.kind() != ASTKind::Ident && name.kind() != ASTKind::Missing)
                                error(module, namePos, "Expected identifier in const parameter declaration, found '", name, "'.");
                            AST init = module->add(ASTKind::Missing);
                            if (visitor.peek().token == PunctuatorColon) {
                                visitor.read();
                                init = parseExpression(module, visitor);
                            }
                            argument = module->add(ASTKind::ConstVarDecl, namePos, type, name, init);
                        } else
                            argument = parseExpressionOrJuxtaposition(module, visitor, false, true, true);
                        if (visitor.peek().token == PunctuatorColon) {
                            Pos pos = visitor.read().pos;
                            if (argument.kind() != ASTKind::Ident && argument.kind() != ASTKind::Missing)
                                error(module, pos, "Expected identifier in parameter declaration, found '", argument, "'.");
                            argument = module->add(ASTKind::NamedParameter, pos, argument, parseExpression(module, visitor));
                        }
                        arguments.push(argument);
                        if (visitor.peek().token == PunctuatorComma)
                            visitor.read();
                    }
                    visitor.stopIgnoringWhitespace();
                    if (visitor.done())
                        error(module, visitor.last(), "Unexpected end of file in call expression.");
                    else if (visitor.peek().token != PunctuatorRightParen) {
                        error(module, visitor.peek(), "Expected closing parenthese ')' in call, found '", TokenFormat(module, visitor.peek()), "'.");
                        visitor.readUpToAndIncluding(PunctuatorRightParen);
                    } else
                        visitor.read();
                    ast = module->add(callKind, token.pos, function, arguments);
                    break;
                }
                case PunctuatorLeftBracket: { // Indexing
                    visitor.ignoreWhitespace(token.pos);
                    visitor.read();
                    if (visitor.peek().token == PunctuatorColon) { // Slice access with no start
                        visitor.read();
                        if (visitor.peek().token == PunctuatorRightBracket) {
                            visitor.stopIgnoringWhitespace();
                            visitor.read();
                            ast = module->add(ASTKind::GetSlice, ast, module->add(ASTKind::Missing), module->add(ASTKind::Missing));
                            break;
                        }
                        AST end = parseExpression(module, visitor);
                        visitor.stopIgnoringWhitespace();
                        if (visitor.peek().token != PunctuatorRightBracket) {
                            error(module, visitor.peek(), "Expected closing bracket ']', found '", TokenFormat(module, visitor.peek()), "'.");
                            visitor.readUpToAndIncluding(PunctuatorRightBracket);
                        } else
                            visitor.read();
                        ast = module->add(ASTKind::GetSlice, ast, module->add(ASTKind::Missing), end);
                        break;
                    }
                    if (visitor.peek().token == PunctuatorRightBracket) {
                        visitor.stopIgnoringWhitespace();
                        visitor.read();
                        ast = module->add(ASTKind::SliceType, token.pos, ast);
                        break;
                    }
                    AST index = parseExpression(module, visitor);
                    switch (visitor.peek().token.symbol) {
                        case PunctuatorRightBracket:
                            ast = module->add(ASTKind::GetIndex, token.pos, ast, index);
                            break;
                        case PunctuatorComma: { // Multi-index access
                            vec<AST> indices;
                            indices.push(index);
                            while (!visitor.done() && visitor.peek().token != PunctuatorRightBracket) {
                                if (visitor.peek().token != PunctuatorComma)
                                    error(module, visitor.peek(), "Expected comma or closing bracket ']', found '", TokenFormat(module, visitor.peek()), "'.");
                                else
                                    visitor.read();
                                indices.push(parseExpression(module, visitor));
                            }
                            ast = module->add(ASTKind::GetIndices, token.pos, ast, indices);
                            break;
                        }
                        case PunctuatorColon: { // Slice access
                            visitor.read();
                            if (visitor.peek().token == PunctuatorRightBracket) { // No upper bound
                                ast = module->add(ASTKind::GetSlice, token.pos, ast, index, module->add(ASTKind::Missing));
                                break;
                            }
                            AST end = parseExpression(module, visitor);
                            ast = module->add(ASTKind::GetSlice, token.pos, ast, index, end);
                            break;
                        }
                        default:
                            error(module, visitor.peek(), "Unexpected token '", TokenFormat(module, visitor.peek()), "' in indexing expression.");
                            visitor.read();
                            break;
                    }
                    visitor.stopIgnoringWhitespace();
                    if (visitor.done())
                        error(module, visitor.last(), "Unexpected end of file in indexing expression.");
                    else if (visitor.peek().token != PunctuatorRightBracket) {
                        error(module, visitor.peek(), "Expected closing bracket ']', found '", TokenFormat(module, visitor.peek()), "'.");
                        visitor.readUpToAndIncluding(PunctuatorRightBracket);
                    } else
                        visitor.read();
                    break;
                }
                case PunctuatorDot: { // Field access
                    visitor.read();
                    if UNLIKELY(visitor.peek().token == PunctuatorLeftParen) { // Multi-field access
                        vec<AST> fields;
                        visitor.ignoreWhitespace(visitor.peek().pos);
                        visitor.read();
                        while (!visitor.done() && visitor.peek().token != PunctuatorRightParen) {
                            Pos fieldPos = visitor.peek().pos;
                            AST field = parsePrimaryNoSuffix(module, visitor);
                            if (field.kind() != ASTKind::Ident && field.kind() != ASTKind::Missing)
                                error(module, fieldPos, "Expected identifier in field access, found '", field, "'.");
                            fields.push(field);
                            if (visitor.peek().token == PunctuatorComma)
                                visitor.read();
                        }
                        visitor.stopIgnoringWhitespace();
                        if (visitor.done())
                            error(module, visitor.last(), "Unexpected end of file in multi-field access.");
                        else if (visitor.peek().token != PunctuatorRightParen) {
                            error(module, visitor.peek(), "Expected closing parenthese ')', found '", TokenFormat(module, visitor.peek()), "'.");
                            visitor.readUpToAndIncluding(PunctuatorRightParen);
                        } else
                            visitor.read();
                        ast = module->add(ASTKind::GetFields, token.pos, ast, fields);
                        break;
                    }
                    Pos fieldPos = visitor.peek().pos;
                    AST v = parsePrimaryNoSuffix(module, visitor);
                    if (v.kind() != ASTKind::Ident && v.kind() != ASTKind::Missing
                        && v.kind() != ASTKind::OwnType && v.kind() != ASTKind::UninitType)
                        error(module, fieldPos, "Expected identifier in field access, found '", v, "'.");
                    ast = module->add(ASTKind::GetField, token.pos, ast, v);
                    break;
                }
                default:
                    if ((ast.kind() == ASTKind::Unsigned || ast.kind() == ASTKind::Float) && isIdentifier(module, token)) {
                        if (ast.kind() == ASTKind::Unsigned && ast.uintConst() == 0 && allowNonDecimalConstant) {
                            // Special weird case! A coefficient of integer
                            // zero is forbidden (it would be redundant anyway)
                            // and instead indicates a non-decimal integer
                            // constant like 0xdeadbeef.
                            visitor.read();
                            allowNonDecimalConstant = false;
                            const_slice<i8> multiplicand = module->str(token.token);
                            switch (multiplicand[0]) {
                                case 'x': // Hexadecimal literal.
                                    ast = module->add(ASTKind::Unsigned, Constant::UnsignedConst(hextouint(multiplicand.drop(1))));
                                    break;
                                case 'b': // Binary literal.
                                    ast = module->add(ASTKind::Unsigned, Constant::UnsignedConst(binarytouint(multiplicand.drop(1))));
                                    break;
                                case 'o': // Octal literal.
                                    ast = module->add(ASTKind::Unsigned, Constant::UnsignedConst(octaltouint(multiplicand.drop(1))));
                                    break;
                                default:
                                    error(module, token.pos, "Invalid base for non-decimal integer constant '", multiplicand[0], "'.");
                            }
                        } else
                            ast = module->add(ASTKind::Mul, token.pos, ast, parseIdentifier(module, visitor));
                        break;
                    }
                    // Not a suffix, break the loop and return.
                    return ast;
            }
        }
        return ast;
    }

    AST parsePrimary(Module* module, TokenVisitor& visitor) {
        AST ast = parsePrimaryNoSuffix(module, visitor);
        return parsePrimarySuffix(module, visitor, ast);
    }

    AST parsePostfix(Module* module, TokenVisitor& visitor) {
        AST ast = parsePrimary(module, visitor);
        while (!visitor.done()) {
            Token token = visitor.peek();
            switch (token.token.symbol) {
                case OperatorEllipsis:
                    visitor.read();
                    ast = module->add(ASTKind::Splat, token.pos, ast);
                    break;
                case OperatorIncr:
                    visitor.read();
                    ast = module->add(ASTKind::PostIncr, token.pos, ast);
                    break;
                case OperatorDecr:
                    visitor.read();
                    ast = module->add(ASTKind::PostDecr, token.pos, ast);
                    break;
                default:
                    return ast;
            }
        }
        return ast;
    }

    constexpr u32 NumPossibleBinaryOperators = LastKeyword + 1 - FirstOperator;
    u32 precedences[NumPossibleBinaryOperators];
    bool didInitPrecedences;

    inline bool isBinaryOperator(Symbol symbol) {
        constexpr u64 IsBinaryOperatorMask = 0ull
            | 1ull << (OperatorAdd - FirstOperator)
            | 1ull << (OperatorAddAssign - FirstOperator)
            | 1ull << (OperatorSub - FirstOperator)
            | 1ull << (OperatorSubAssign - FirstOperator)
            | 1ull << (OperatorMul - FirstOperator)
            | 1ull << (OperatorMulAssign - FirstOperator)
            | 1ull << (OperatorDiv - FirstOperator)
            | 1ull << (OperatorDivAssign - FirstOperator)
            | 1ull << (OperatorRem - FirstOperator)
            | 1ull << (OperatorRemAssign - FirstOperator)
            | 1ull << (OperatorExp - FirstOperator)
            | 1ull << (OperatorExpAssign - FirstOperator)
            | 1ull << (OperatorBitAnd - FirstOperator)
            | 1ull << (OperatorBitAndAssign - FirstOperator)
            | 1ull << (OperatorBitOr - FirstOperator)
            | 1ull << (OperatorBitOrAssign - FirstOperator)
            | 1ull << (OperatorBitXor - FirstOperator)
            | 1ull << (OperatorBitXorAssign - FirstOperator)
            | 1ull << (OperatorLeftShift - FirstOperator)
            | 1ull << (OperatorLeftShiftAssign - FirstOperator)
            | 1ull << (OperatorRightShift - FirstOperator)
            | 1ull << (OperatorRightShiftAssign - FirstOperator)
            | 1ull << (OperatorLeftRotate - FirstOperator)
            | 1ull << (OperatorLeftRotateAssign - FirstOperator)
            | 1ull << (OperatorRightRotate - FirstOperator)
            | 1ull << (OperatorRightRotateAssign - FirstOperator)
            | 1ull << (OperatorAssign - FirstOperator)
            | 1ull << (OperatorLess - FirstOperator)
            | 1ull << (OperatorLessEqual - FirstOperator)
            | 1ull << (OperatorGreater - FirstOperator)
            | 1ull << (OperatorGreaterEqual - FirstOperator)
            | 1ull << (OperatorEqual - FirstOperator)
            | 1ull << (OperatorNotEqual - FirstOperator)
            | 1ull << (OperatorRange - FirstOperator)
            | 1ull << (KeywordAs - FirstOperator)
            | 1ull << (KeywordIs - FirstOperator)
            | 1ull << (KeywordIsNot - FirstOperator)
            | 1ull << (KeywordIn - FirstOperator)
            | 1ull << (KeywordAnd - FirstOperator)
            | 1ull << (KeywordOr - FirstOperator)
            | 1ull << (KeywordIf - FirstOperator);
        return symbol.symbol >= FirstOperator && symbol.symbol <= LastKeyword && IsBinaryOperatorMask & (1ull << (symbol.symbol - FirstOperator));
    }

    u32 precedenceOf(Symbol op) {
        assert(isBinaryOperator(op));
        assert(didInitPrecedences);
        if (op.symbol < FirstOperator || op.symbol > LastKeyword)
            return 0;
        return precedences[op.symbol - FirstOperator];
    }

    enum Associativity {
        LeftAssociative,
        RightAssociative
    };

    Associativity associativityOf(Symbol op) {
        assert(isBinaryOperator(op));

        constexpr u64 RightAssociativityMask = 0ull
            | 1ull << (OperatorExp - FirstOperator)
            | 1ull << (OperatorAddAssign - FirstOperator)
            | 1ull << (OperatorSubAssign - FirstOperator)
            | 1ull << (OperatorMulAssign - FirstOperator)
            | 1ull << (OperatorDivAssign - FirstOperator)
            | 1ull << (OperatorRemAssign - FirstOperator)
            | 1ull << (OperatorExpAssign - FirstOperator)
            | 1ull << (OperatorBitAndAssign - FirstOperator)
            | 1ull << (OperatorBitOrAssign - FirstOperator)
            | 1ull << (OperatorBitXorAssign - FirstOperator)
            | 1ull << (OperatorLeftShiftAssign - FirstOperator)
            | 1ull << (OperatorRightShiftAssign - FirstOperator)
            | 1ull << (OperatorLeftRotateAssign - FirstOperator)
            | 1ull << (OperatorRightRotateAssign - FirstOperator)
            | 1ull << (OperatorAssign - FirstOperator);
        return (1ull << (op.symbol - FirstOperator)) & RightAssociativityMask ? RightAssociative : LeftAssociative;
    }

    AST parsePattern(Module* module, TokenVisitor& visitor) {
        // Patterns are just expressions...for now. This is kind of gross, it
        // requires that the pattern grammar is a subset of the expression
        // grammar, which means the expression grammar needs to support some
        // weird things like variable declarations in call expressions. We
        // validate and resolve this sort of thing in a later phase once we
        // resolve types.
        return parseExpressionOrJuxtaposition(module, visitor, false, false, false);
    }

    AST chainRelationalOperators(Module* module, Pos pos, AST lhs, ASTKind op, AST rhs) {
        AST left = lhs;
        while (left.kind() == ASTKind::And)
            left = left.child(1);

        if ((u32)left.kind() < (u32)ASTKind::Less || (u32)left.kind() > (u32)ASTKind::NotEqual)
            return module->add(op, pos, lhs, rhs);

        left = module->clone(left.child(1));
        AST compare = module->add(op, pos, left, rhs);
        return module->add(ASTKind::And, pos, lhs, compare);
    }

    AST makeBinary(Module* module, AST lhs, Token op, AST rhs, const AST& interior, const vec<AST>& extraChildren) {
        assert(isBinaryOperator(op.token));
        ASTKind kind;
        switch (op.token.symbol) {
            case OperatorAdd: kind = ASTKind::Add; break;
            case OperatorSub: kind = ASTKind::Sub; break;
            case OperatorDiv: kind = ASTKind::Div; break;
            case OperatorRem: kind = ASTKind::Rem; break;
            case OperatorBitOr: kind = ASTKind::BitOr; break;
            case OperatorBitXor: kind = ASTKind::BitXor; break;
            case OperatorBitAnd: kind = ASTKind::BitAnd; break;
            case OperatorLeftShift: kind = ASTKind::BitShl; break;
            case OperatorRightShift: kind = ASTKind::BitShr; break;
            case OperatorLeftRotate: kind = ASTKind::BitRol; break;
            case OperatorRightRotate: kind = ASTKind::BitRor; break;
            case OperatorAddAssign: kind = ASTKind::AddEq; break;
            case OperatorSubAssign: kind = ASTKind::SubEq; break;
            case OperatorMulAssign: kind = ASTKind::MulEq; break;
            case OperatorDivAssign: kind = ASTKind::DivEq; break;
            case OperatorRemAssign: kind = ASTKind::RemEq; break;
            case OperatorExpAssign: kind = ASTKind::ExpEq; break;
            case OperatorBitOrAssign: kind = ASTKind::BitOrEq; break;
            case OperatorBitXorAssign: kind = ASTKind::BitXorEq; break;
            case OperatorBitAndAssign: kind = ASTKind::BitAndEq; break;
            case OperatorLeftShiftAssign: kind = ASTKind::BitShlEq; break;
            case OperatorRightShiftAssign: kind = ASTKind::BitShrEq; break;
            case OperatorLeftRotateAssign: kind = ASTKind::BitRolEq; break;
            case OperatorRightRotateAssign: kind = ASTKind::BitRorEq; break;
            case OperatorAssign: kind = ASTKind::Assign; break;
            case OperatorLess: return chainRelationalOperators(module, op.pos, lhs, ASTKind::Less, rhs);
            case OperatorLessEqual: return chainRelationalOperators(module, op.pos, lhs, ASTKind::LessEq, rhs);
            case OperatorGreater: return chainRelationalOperators(module, op.pos, lhs, ASTKind::Greater, rhs);
            case OperatorGreaterEqual: return chainRelationalOperators(module, op.pos, lhs, ASTKind::GreaterEq, rhs);
            case OperatorEqual: return chainRelationalOperators(module, op.pos, lhs, ASTKind::Equal, rhs);
            case OperatorNotEqual: return chainRelationalOperators(module, op.pos, lhs, ASTKind::NotEqual, rhs);
            case OperatorRange: kind = ASTKind::Range; break;
            case KeywordAs:
                return module->add(ASTKind::Construct, op.pos, rhs, lhs);
            case KeywordIs: kind = ASTKind::Is; break;
            case KeywordIsNot:
                return module->add(ASTKind::Not, op.pos, module->add(ASTKind::Is, op.pos, lhs, rhs));
            case KeywordIn: kind = ASTKind::In; break;
            case KeywordAnd: kind = ASTKind::And; break;
            case KeywordOr: kind = ASTKind::Or; break;
            case OperatorMul:
            case OperatorExp:
                // To resolve type-based ambiguities, we parse both of these to
                // a common "Stars" node and encode each * or ** as a 1 or 2.
                return module->add(ASTKind::Stars, op.pos, lhs, rhs, extraChildren);
            case KeywordIf:
                // We should only be in this case if we had an inline else block.
                return module->add(ASTKind::Ternary, op.pos, interior, lhs, rhs);
            default:
                unreachable("Not a binary operator");
        }
        return module->add(kind, op.pos, lhs, rhs);
    }

    bool processPossiblePointerType(Module* module, AST& lhs, Token op, TokenVisitor& visitor, vec<AST>& extraChildren) {
        // This edge case handles the situation that a multiply or exponent operator
        // are being used postfix to indicate a pointer type, i.e. T* or T**.
        // In many cases, we'll parse pointer types as different expressions, and disambiguate
        // later: "T * x" is a multiplication for instance, but can become a pointer-typed variable
        // declaration if T is found to be a type. The problem is that not all occurrences of these
        // operators are otherwise valid expressions, consider "foo(T*, x)". In these cases, we check
        // for the erroneous case eagerly here, and turn the * or ** into a postfix operator before
        // continuing.

        static_assert(LastOperator < 64);

        while (op.token == OperatorMul || op.token == OperatorExp) {
            extraChildren.push(module->add(ASTKind::Unsigned, Constant::UnsignedConst(op.token == OperatorMul ? 1 : 2)));

            constexpr u64 followSetForPointerType = 0ull
                | 1ull << WhitespaceNewline // Can occur in i.e. type Ptr: T*
                | 1ull << PunctuatorComma // Can occur in parameter lists, i.e. foo(T*, x)
                | 1ull << PunctuatorRightParen // Can occur in parameter lists, i.e. foo(T*)
                | 1ull << PunctuatorDot // Can occur in static method invocations, i.e. T*.alignment()
                | 1ull << PunctuatorColon // Can occur in patterns, i.e. case T*:, or conditions, i.e. if x is T*:
                ;
            // We notably do not include binary operators in the follow set.
            // We could conceivably assume a postfix star is a pointer type by
            // default if it's followed by another binary operator, but this
            // would lead to confusing parsing for a common pattern like
            // 'T * -x'. This is consequential though, because it means T*
            // can't act like a primary expression in cases like 'x is T* or y'
            // which is a little confusing.
            Token next = visitor.peek();
            if (visitor.done()
                || (next.token.symbol <= LastOperator && (followSetForPointerType & (1ull << next.token.symbol)))) {
                u32 totalStars = 0;
                for (AST ast : extraChildren)
                    totalStars += ast.uintConst();
                for (u32 i = 0; i < totalStars; i ++)
                    lhs = module->add(ASTKind::PtrType, op.pos, lhs);
                if (op.token == PunctuatorDot) {
                    // Uniquely, dot means we have more to parse in this
                    // specific expression - we could be calling a method for
                    // example, like T*.size()
                    assert(lhs.kind() == ASTKind::PtrType);
                    lhs = parsePrimarySuffix(module, visitor, lhs);

                    // This also means we need to go around again, to handle
                    // things like T*.Vec().ElementType* where the field
                    // we are accessing is itself a type.
                    if (visitor.peek().token == OperatorMul || visitor.peek().token == OperatorExp) {
                        op = visitor.read();
                        continue;
                    }
                }
                return true; // None of the elements of the follow set are legal exprs, so we quit and bail in parseBinary.
            } else if (next.token == OperatorMul || next.token == OperatorExp)
                op = visitor.read(); // Consume the operator and go around again.
            else
                break; // We're done here.
        }
        return false;
    }

    pair<Token, bool> getBinaryOperator(Module* module, TokenVisitor& visitor, bool alreadyMultiline) {
        if (visitor.peek().token == WhitespaceNewline) {
            if (alreadyMultiline || visitor.peek2().token == WhitespaceIndent) {
                Token possibleOp = visitor.peekIgnoringWhitespace();
                if (isBinaryOperator(possibleOp.token) || possibleOp.token == KeywordElse) {
                    visitor.readIgnoringWhitespace(); // Consume newline and any interstitial indentation.
                    if (possibleOp.token == KeywordIs && visitor.peek2().token == KeywordNot) {
                        visitor.read();
                        return { { KeywordIsNot, possibleOp.pos }, false };
                    }
                    return { possibleOp, true };
                }
            }
        }
        if (visitor.peek().token == KeywordIs && visitor.peek2().token == KeywordNot) {
            return { { KeywordIsNot, visitor.read().pos }, false };
        }
        return { visitor.peek(), false };
    }

    AST parseBinary(Module* module, AST lhs, Token op, TokenVisitor& visitor, bool& didBail, u32& ifOffset, const vec<AST>& extraChildren, bool isMultiline, u32 minPrecedence = 0) {
        u32 operatorOffset = visitor.offset - 1;
        AST rhs = parsePostfix(module, visitor);

        if (rhs.kind() == ASTKind::List && (op.token == OperatorMul || op.token == OperatorExp)) {
            // It's invalid to actually multiply a list with anything, so this
            // must be a pointer type of some kind. Treat these as a cohesive
            // group as if constructed using an array-type or slice-type
            // construction.
            lhs = makeBinary(module, lhs, op, rhs, AST(), extraChildren);
            lhs = parsePrimarySuffix(module, visitor, lhs);
            auto possibleOp = getBinaryOperator(module, visitor, isMultiline);
            Token op = possibleOp.first;
            if (isBinaryOperator(op.token)) {
                visitor.read();
                return parseBinary(module, lhs, op, visitor, didBail, ifOffset, extraChildren, isMultiline || possibleOp.second);
            } else
                return lhs;
        }

        auto possibleNextOp = getBinaryOperator(module, visitor, isMultiline);
        Token nextOp = possibleNextOp.first;
        isMultiline = possibleNextOp.second || isMultiline;
        AST interior;
        vec<AST> nextExtraChildren;

        if (op.token == KeywordIf) {
            while (nextOp.token != KeywordElse) {
                if (!isBinaryOperator(nextOp.token))
                    goto parseBinary_ifBail;
                if (precedenceOf(nextOp.token) > precedenceOf(op.token)) {
                    visitor.read();
                    rhs = parseBinary(module, rhs, nextOp, visitor, didBail, ifOffset, nextExtraChildren, precedenceOf(KeywordIf));
                    if (visitor.done())
                        goto parseBinary_ifBail;
                    possibleNextOp = getBinaryOperator(module, visitor, isMultiline);
                    nextOp = possibleNextOp.first;
                    isMultiline = possibleNextOp.second || isMultiline;
                } else
                    goto parseBinary_ifBail; // If we see a lower-precedence operator than if, we should have already seen an else.
            }
            goto parseBinary_ifSuccess;
        parseBinary_ifBail:
            // Because of the nasty situation where `if` can be used as either a ternary expression,
            // or as an inline statement, we need to bail on binary operator parsing in the specific
            // case that we encounter an inline `if` with no `else`. This is to specifically enshrine
            // the desired precedence in the following scenario:
            //   foo = 1 if cond else 2
            //   foo = 1 if cond
            // In the first, it's more useful for the assignment to bind less tight than the ternary.
            // In the second, it's more useful if the assignment binds more tightly than the if, so we
            // don't just evaluate 1 to nothing.
            //
            // In this very specific scenario, we just return the lhs (the body of the inline if) and
            // backtrack to the point of the if operator. All very unfortunate, but probably not enough
            // backtracking to be terribly meaningful in real uses.

            didBail = true;
            ifOffset = operatorOffset;
            return lhs;
        parseBinary_ifSuccess:
            interior = rhs;
            visitor.read(); // Consume else.
            rhs = parsePostfix(module, visitor);
            nextOp = visitor.peek();
        }

        bool alreadyReadNextOp = false;
        if (nextOp.token == OperatorMul || nextOp.token == OperatorExp) {
            alreadyReadNextOp = true;
            visitor.read(); // nextOp is a binary operator, we need to consume it.
            if (processPossiblePointerType(module, rhs, nextOp, visitor, nextExtraChildren)) {
                // Note we pass extraChildren and not nextExtraChildren. If the
                // rhs can be determined to be a primary expression (i.e. a
                // pointer type), then nextExtraChildren "belongs" to that
                // expression. extraChildren always corresponds to op, and
                // nextExtraChildren always corresponds to nextOp. When we pass
                // nextOp to become the op of a callee, our nextExtraChildren
                // becomes their extraChildren.
                return makeBinary(module, lhs, op, rhs, interior, extraChildren); // Means rhs is a primary expression.
            }
        }
        if (!isBinaryOperator(nextOp.token) || precedenceOf(nextOp.token) < minPrecedence)
            return makeBinary(module, lhs, op, rhs, interior, extraChildren);

        if (!alreadyReadNextOp)
            visitor.read(); // nextOp is a binary operator, we need to consume it.
        u32 opPrecedence = precedenceOf(op.token), nextPrecedence = precedenceOf(nextOp.token);
        Associativity assoc = associativityOf(op.token);
        if (opPrecedence > nextPrecedence || (opPrecedence == nextPrecedence && assoc == LeftAssociative))
            return parseBinary(module, makeBinary(module, lhs, op, rhs, interior, extraChildren), nextOp, visitor, didBail, ifOffset, nextExtraChildren, isMultiline);
        else
            return makeBinary(module, lhs, op, parseBinary(module, rhs, nextOp, visitor, didBail, ifOffset, nextExtraChildren, isMultiline), interior, extraChildren);
    }

    void consumeNewlines(TokenVisitor& visitor) {
        while (visitor.peek().token == WhitespaceNewline)
            visitor.read();
    }

    AST parseBlock(Module* module, TokenVisitor& visitor) {
        vec<AST> items;
        if (visitor.done())
            return module->add(ASTKind::Do, visitor.last().pos, items);

        assert(visitor.peek().token == WhitespaceNewline);
        Pos pos = visitor.read().pos;
        if (visitor.peek().token != WhitespaceIndent) {
            error(module, pos, "Expected indented block.");
            return module->add(ASTKind::Do, pos, items);
        }
        if (visitor.isIgnoringWhitespace()) {
            error(module, pos, "Indented block not permitted within enclosed expression.")
                .note(module, visitor.whitespaceStack.back(), "Enclosed expression started here.");
        }
        visitor.read();
        while (!visitor.done() && visitor.peek().token != WhitespaceDedent) {
            consumeNewlines(visitor);
            if (visitor.done() || visitor.peek().token == WhitespaceDedent)
                break;
            items.push(parseStatement(module, visitor));
        }
        visitor.read();
        return module->add(ASTKind::Do, pos, items);
    }

    AST parseExpression(Module* module, TokenVisitor& visitor, u32 minPrecedence) {
        AST ast = parsePostfix(module, visitor);
        auto possibleOp = getBinaryOperator(module, visitor, false);
        Token op = possibleOp.first;
        if (isBinaryOperator(op.token) && precedenceOf(op.token) >= minPrecedence) {
            visitor.read();
            vec<AST> extraChildren;
            if (processPossiblePointerType(module, ast, op, visitor, extraChildren))
                return ast;

            bool didBail = false;
            u32 ifOffset = 0;
            u32 savedDedents = visitor.dedentsToIgnore, savedIndents = visitor.impliedIndents;
            ast = parseBinary(module, ast, op, visitor, didBail, ifOffset, extraChildren, possibleOp.second, minPrecedence);

            // TODO: Probably don't backtrack if we ran into a parse error exploring past the `if`.
            if UNLIKELY(didBail) {
                visitor.offset = ifOffset;
                visitor.dedentsToIgnore = savedDedents;
                visitor.impliedIndents = savedIndents;
            }
        }
        return ast;
    }

    AST parseParameterTuple(Module* module, TokenVisitor& visitor, bool typeLevel) {
        assert(visitor.peek().token == PunctuatorLeftParen);
        visitor.ignoreWhitespace(visitor.peek().pos);
        Token paren = visitor.read();

        vec<AST> parameters;
        while (!visitor.done() && visitor.peek().token != PunctuatorRightParen) {
            if (visitor.peek().token == KeywordConst) {
                Token token = visitor.read();
                Pos namePos = visitor.peek().pos;
                AST name = parseIdentifier(module, visitor);
                AST value = module->add(ASTKind::Missing);
                if (visitor.peek().token == PunctuatorColon) {
                    visitor.read();
                    value = parseExpression(module, visitor);
                }
                parameters.push(module->add(ASTKind::ConstVarDecl, namePos, name, value));
            } else if (visitor.peek().token == KeywordType) {
                Token token = visitor.read();
                Pos namePos = visitor.peek().pos;
                AST name = parseIdentifier(module, visitor);

                if (visitor.peek().token == PunctuatorLeftParen) {
                    error(module, visitor.peek().pos, "Type parameter list not permitted in type parameter declaration.");
                    visitor.readUpToAndIncluding(PunctuatorRightParen);
                }

                AST value = module->add(ASTKind::Missing);
                if (visitor.peek().token == PunctuatorColon) {
                    visitor.read();
                    value = parseExpression(module, visitor);
                }
                parameters.push(module->add(ASTKind::AliasDecl, namePos, name, module->add(ASTKind::Missing), value));
            } else {
                Token token = visitor.peek();
                AST expr = parseExpression(module, visitor);

                if (typeLevel) {
                    error(module, token.pos, "Expected constant or type parameter in type-level parameter list, found '", TokenFormat(module, token), "'.");
                    visitor.readUpTo(PunctuatorRightParen);
                    break;
                }

                if (expr.kind() == ASTKind::Stars) {
                    // For this to be valid, it must be a declaration of pointer type.

                    token = visitor.peek();
                    AST lhs = expr.child(0);
                    for (u32 i = 2; i < expr.arity(); i ++) for (u32 j = 0; j < expr.child(i).uintConst(); j ++)
                        lhs = module->add(ASTKind::PtrType, expr.pos(), lhs);

                    AST name = expr.child(1);
                    if (name.kind() != ASTKind::Ident)
                        error(module, expr.pos(), "Expected identifier in parameter declaration, found '", name, "'.");

                    AST init;
                    if (token.token == PunctuatorColon) {
                        visitor.read();
                        init = parseExpression(module, visitor);
                    } else
                        init = module->add(ASTKind::Missing);
                    parameters.push(module->add(ASTKind::VarDecl, expr.pos(), lhs, name, init));
                } else if (visitor.peek().token != PunctuatorRightParen && visitor.peek().token != PunctuatorComma && visitor.peek().token != PunctuatorColon) {
                    // Try to parse a juxtaposition.
                    Pos pos = visitor.peek().pos;
                    AST name = parsePrimaryNoSuffix(module, visitor);
                    if (name.kind() != ASTKind::Ident)
                        error(module, pos, "Expected identifier in parameter declaration, found '", name, "'.");
                    AST value = module->add(ASTKind::Missing);
                    if (visitor.peek().token == PunctuatorColon) {
                        visitor.read();
                        value = parseExpression(module, visitor);
                    }
                    parameters.push(module->add(ASTKind::VarDecl, pos, expr, name, value));
                } else if (expr.kind() == ASTKind::Ident) {
                    AST value = module->add(ASTKind::Missing);
                    Pos pos = visitor.peek().pos;
                    if (visitor.peek().token == PunctuatorColon) {
                        visitor.read();
                        value = parseExpression(module, visitor);
                    }
                    parameters.push(module->add(ASTKind::VarDecl, pos, module->add(ASTKind::Missing), expr, value));
                } else {
                    // We accept arbitrary exprs at parse time, but we expect
                    // anything that isn't a VarDecl to be a type expression
                    // by name resolution.
                    parameters.push(expr);
                }
            }

            if (visitor.peek().token == PunctuatorComma)
                visitor.read();
        }
        visitor.stopIgnoringWhitespace();
        if UNLIKELY(visitor.done())
            error(module, visitor.last(), "Unexpected end of file parsing parameter list.");
        else if UNLIKELY(visitor.peek().token != PunctuatorRightParen) {
            error(module, visitor.peek(), "Expected comma or closing parenthese ')' in parameter list, found '", TokenFormat(module, visitor.peek()), "'.");
            visitor.readUpToAndIncluding(PunctuatorRightParen);
        } else
            visitor.read();

        return module->add(ASTKind::Tuple, paren.pos, parameters);
    }

    AST parseRaisesDeclaration(Module* module, TokenVisitor& visitor) {
        assert(visitor.peek().token == KeywordRaises);
        Token raises = visitor.read();
        vec<AST> raisedTypes;
        raisedTypes.push(parseExpression(module, visitor)); // Types are part of the expression grammar.
        while (visitor.peek().token == PunctuatorComma) {
            visitor.read();
            raisedTypes.push(parseExpression(module, visitor));
        }
        return module->add(ASTKind::Tuple, raisedTypes);
    }

    AST parseBlockOrChain(Module* module, TokenVisitor& visitor, const char* statementKind, bool allowMissing) {
        // For a couple statement types, we allow them to be followed by any
        // of:
        //  - a colon and an inline statement,
        //
        //  - a colon and an indented block,
        //
        //  - or the first keyword of another statement which is terminated in
        //    the same way. This is the "chain" in "block or chain".
        //
        // This function handles detecting the chained case and returning the
        // overall interior block.

        constexpr u64 followSetForChain = 0ull
            | 1ull << (KeywordIf - FirstKeyword)
            | 1ull << (KeywordUnless - FirstKeyword)
            | 1ull << (KeywordWhile - FirstKeyword)
            | 1ull << (KeywordUntil - FirstKeyword)
            | 1ull << (KeywordFor - FirstKeyword)
            | 1ull << (KeywordMatch - FirstKeyword);

        auto token = visitor.peek();
        if (token.token.symbol >= FirstKeyword
            && token.token.symbol <= LastKeyword
            && (followSetForChain & (1ull << (token.token.symbol - FirstKeyword)))) {
            return parseStatement(module, visitor);
        }

        if (token.token != PunctuatorColon) {
            if (allowMissing)
                return module->add(ASTKind::Missing);
            error(module, token, "Expected colon ':' in ", statementKind, ", found '", TokenFormat(module, token), "'.");
            visitor.readUpToAndIncluding(WhitespaceNewline);
            return module->add(ASTKind::Missing);
        }

        visitor.read();
        if (visitor.done() || visitor.peek().token == WhitespaceNewline)
            return parseBlock(module, visitor);
        else
            return parseStatement(module, visitor);
    }

    AST parseExpressionOrJuxtaposition(Module* module, TokenVisitor& visitor, bool allowMultiple, bool allowFunction, bool allowInitializer) {
        AST ast = parseExpression(module, visitor);
        if (visitor.done())
            return ast;

        Token token = visitor.peek();

        if (token.token == PunctuatorColon || token.token == KeywordRaises || token.token == PunctuatorComma) {
            AST possibleDecl = ast;
            while (possibleDecl.kind() == ASTKind::OwnType || possibleDecl.kind() == ASTKind::UninitType)
                possibleDecl = ast.child(0);
            if (possibleDecl.kind() == ASTKind::Stars) {
                // In order for this to be legal, it must be a declaration with
                // a pointer type. So we do a bit of fixup. If the rhs of the
                // stars is a call, then it must be a function. If we're
                // followed by the 'raises' keyword, it's an error for it to not be
                // a function. Otherwise, it's a vardecl, and we assert that the
                // rhs is an identifier here.

                AST rhs = possibleDecl.child(1);
                if (rhs.kind() == ASTKind::Call) {
                    // It's a FunDecl.
                    vec<AST> arguments;
                    for (AST child : rhs.children(1))
                        arguments.push(child);
                    if (rhs.child(0).kind() != ASTKind::Ident)
                        error(module, rhs.pos(), "Expected identifier in function declaration, found '", rhs.child(0), "'.");
                    AST lhs = possibleDecl.child(0);
                    for (u32 i = 2; i < possibleDecl.arity(); i ++) for (u32 j = 0; j < possibleDecl.child(i).uintConst(); j ++)
                        lhs = module->add(ASTKind::PtrType, possibleDecl.pos(), lhs);
                    AST modifiers = ast;
                    while (modifiers.kind() != ASTKind::Stars) {
                        if (modifiers.kind() == ASTKind::OwnType)
                            lhs = module->add(ASTKind::OwnType, modifiers.pos(), lhs);
                        else
                            lhs = module->add(ASTKind::UninitType, modifiers.pos(), lhs);
                        modifiers = modifiers.child(0);
                    }
                    AST raises;
                    if (visitor.peek().token == KeywordRaises)
                        raises = parseRaisesDeclaration(module, visitor);
                    else
                        raises = module->add(ASTKind::Missing);
                    AST body = parseBlockOrChain(module, visitor, "function declaration", true);
                    return module->add(ASTKind::FunDecl, possibleDecl.pos(), lhs, rhs.child(0), module->add(ASTKind::Tuple, ast.pos(), arguments), raises, body);
                } else {
                    assert(token.token == PunctuatorColon || token.token == PunctuatorComma);
                    AST lhs = possibleDecl.child(0);
                    for (u32 i = 2; i < possibleDecl.arity(); i ++) for (u32 j = 0; j < possibleDecl.child(i).uintConst(); j ++)
                        lhs = module->add(ASTKind::PtrType, possibleDecl.pos(), lhs);
                    AST modifiers = ast;
                    while (modifiers.kind() != ASTKind::Stars) {
                        if (modifiers.kind() == ASTKind::OwnType)
                            lhs = module->add(ASTKind::OwnType, modifiers.pos(), lhs);
                        else
                            lhs = module->add(ASTKind::UninitType, modifiers.pos(), lhs);
                        modifiers = modifiers.child(0);
                    }

                    AST name = possibleDecl.child(1);
                    if (name.kind() != ASTKind::Ident)
                        error(module, possibleDecl.pos(), "Expected identifier in variable declaration, found '", name, "'.");

                    AST init;
                    if (token.token == PunctuatorColon) {
                        visitor.read();
                        if (visitor.peek().token == KeywordUninit
                            && (visitor.peek2().token == MetaNone || visitor.peek2().token == PunctuatorComma || visitor.peek2().token == WhitespaceNewline)) {
                            visitor.read();
                            init = module->add(ASTKind::Uninit);
                        } else
                            init = parseExpression(module, visitor);
                    } else
                        init = module->add(ASTKind::Missing);
                    if (visitor.peek().token != PunctuatorComma || !allowMultiple)
                        return module->add(ASTKind::VarDecl, possibleDecl.pos(), lhs, name, init);
                    vec<AST> decls;
                    decls.push(module->add(ASTKind::VarDecl, possibleDecl.pos(), lhs, name, init));
                    while (visitor.peek().token == PunctuatorComma) {
                        visitor.readIgnoringWhitespace();
                        Pos pos = visitor.peek().pos;
                        name = parseIdentifier(module, visitor);
                        if (visitor.peek().token == PunctuatorColon) {
                            visitor.read();
                            if (visitor.peek().token == KeywordUninit
                                && (visitor.peek2().token == MetaNone || visitor.peek2().token == PunctuatorComma || visitor.peek2().token == WhitespaceNewline)) {
                                visitor.read();
                                init = module->add(ASTKind::Uninit);
                            } else
                                init = parseExpression(module, visitor);
                        } else
                            init = module->add(ASTKind::Missing);
                        decls.push(module->add(ASTKind::VarDecl, pos, lhs, name, init));
                    }
                    return decls.size() == 1 ? decls[0] : module->add(ASTKind::Do, token.pos, decls);
                }
            }
        }

        // Check if the next token indicates that we shouldn't expect
        // another expression. Since we're already in statement land
        // (because a juxtaposition is a statement, it's specifically a
        // declaration like `int foo`), we don't need to worry about too
        // many weird cases like getting confused by the `in` in a
        // `for ... in ...` statement. We just need to check that we aren't
        // followed by an inline statement joiner, or a statement
        // terminator (which can only be a newline, EOF, or closing paren).
        constexpr u64 followSetForNonJuxtaposed = 0ull
            | 1ull << (KeywordIf - FirstKeyword)
            | 1ull << (KeywordUnless - FirstKeyword)
            | 1ull << (KeywordWhile - FirstKeyword)
            | 1ull << (KeywordUntil - FirstKeyword)
            | 1ull << (KeywordElse - FirstKeyword)
            | 1ull << (KeywordFor - FirstKeyword)
            | 1ull << (KeywordThen - FirstKeyword);
        if (token.token.symbol >= FirstKeyword && token.token.symbol <= LastKeyword
            && followSetForNonJuxtaposed & (1ull << (token.token.symbol - FirstKeyword)))
            return ast;
        if (token.token == WhitespaceNewline || token.token == PunctuatorRightParen || token.token == PunctuatorColon || token.token == PunctuatorComma)
            return ast;

        // Otherwise, we should consider this a juxtaposition (variable or
        // function decl really) and try to parse another expression. Really
        // it's invalid for this to be anything other than an identifier or
        // string.
        Pos namePos = visitor.peek().pos;
        AST name = parseIdentifier(module, visitor);
        if (visitor.peek().token == PunctuatorLeftParen && allowFunction) {
            // Function definition with explicit return type.
            AST parameters = parseParameterTuple(module, visitor, false);
            AST raises;
            if (visitor.peek().token == KeywordRaises)
                raises = parseRaisesDeclaration(module, visitor);
            else
                raises = module->add(ASTKind::Missing);
            AST body = parseBlockOrChain(module, visitor, "function declaration", true);
            return module->add(ASTKind::FunDecl, namePos, ast, name, parameters, raises, body);
        }

        vec<AST> decls;
        AST init;
        if (visitor.peek().token == PunctuatorColon && allowInitializer) {
            visitor.read();
            if (visitor.peek().token == KeywordUninit
                && (visitor.peek2().token == MetaNone || visitor.peek2().token == PunctuatorComma || visitor.peek2().token == WhitespaceNewline)) {
                visitor.read();
                init = module->add(ASTKind::Uninit);
            } else
                init = parseExpression(module, visitor);
        } else
            init = module->add(ASTKind::Missing);
        decls.push(module->add(ASTKind::VarDecl, namePos, ast, name, init));
        if (allowMultiple) while (visitor.peek().token == PunctuatorComma) {
            visitor.readIgnoringWhitespace();
            name = parseIdentifier(module, visitor);
            if (visitor.peek().token == PunctuatorColon && allowInitializer) {
                visitor.read();
                if (visitor.peek().token == KeywordUninit
                    && (visitor.peek2().token == MetaNone || visitor.peek2().token == PunctuatorComma || visitor.peek2().token == WhitespaceNewline)) {
                    visitor.read();
                    init = module->add(ASTKind::Uninit);
                } else
                    init = parseExpression(module, visitor);
            } else
                init = module->add(ASTKind::Missing);
            decls.push(module->add(ASTKind::VarDecl, namePos, ast, name, init));
        }

        return decls.size() == 1 ? decls[0] : module->add(ASTKind::Do, token.pos, decls);
    }

    void exploreBody(vec<AST>& contents, AST body) {
        if (body.kind() == ASTKind::Do)
            for (AST ast : body)
                exploreBody(contents, ast);
        else
            contents.push(body);
    }

    AST parsePrimaryStatement(Module* module, TokenVisitor& visitor);

    AST parseTypeDecl(Module* module, TokenVisitor& visitor, u32 expectedKeyword) {
        assert(visitor.peek().token == expectedKeyword);
        Token token = visitor.read();
        AST name = parseIdentifier(module, visitor);
        bool isCase = expectedKeyword == KeywordCase;

        AST parameters;
        if (visitor.peek().token == PunctuatorLeftParen) { // Generic type.
            parameters = parseParameterTuple(module, visitor, true);
        } else
            parameters = module->add(ASTKind::Missing);

        if (!parameters.missing() && isCase)
            error(module, token.pos, "Type parameter list not permitted on case type declaration.");

        if (visitor.peek().token != PunctuatorColon) // Stub type, or maybe unit.
            return module->add(isCase ? ASTKind::NamedCaseDecl : ASTKind::NamedDecl, token.pos, name, parameters, module->add(ASTKind::Missing));

        visitor.read();
        AST body;
        if (visitor.done() || visitor.peek().token == WhitespaceNewline)
            body = parseBlock(module, visitor);
        else
            body = parsePrimaryStatement(module, visitor);

        vec<AST> contents;
        exploreBody(contents, body);
        for (AST ast : contents) {
            if (ast.kind() == ASTKind::FunDecl)
                error(module, contents[0].pos(), "Function declarations are not permitted in type definitions.");
            else if (ast.kind() == ASTKind::Namespace)
                error(module, contents[0].pos(), "Namespace declarations are not permitted in type definitions.");
        }
        if (contents.size() == 1) switch (contents[0].kind()) {
            case ASTKind::VarDecl:
            case ASTKind::StructCaseDecl:
            case ASTKind::UnionCaseDecl:
            case ASTKind::NamedCaseDecl:
            case ASTKind::StructDecl:
            case ASTKind::UnionDecl:
            case ASTKind::NamedDecl:
            case ASTKind::ConstVarDecl:
            case ASTKind::ConstFunDecl:
                break;
            default:
                // If it's just some kind of expression, assume it's a type and we're defining
                // a struct with a single unnamed field.
                return module->add(isCase ? ASTKind::NamedCaseDecl : ASTKind::NamedDecl, token.pos, name, parameters, contents[0]);
        }
        bool sawField = false, sawCase = false;
        // TODO: Maybe this should be a tree-traversal? Not sure.
        for (AST ast : contents) switch (ast.kind()) {
            case ASTKind::VarDecl:
                sawField = true;
                break;
            case ASTKind::StructCaseDecl:
            case ASTKind::UnionCaseDecl:
            case ASTKind::NamedCaseDecl:
                sawCase = true;
                break;
            default:
                break;
        }
        ASTKind kind = sawCase ? ASTKind::UnionDecl : ASTKind::StructDecl;
        if (isCase)
            kind = sawCase ? ASTKind::UnionCaseDecl : ASTKind::StructCaseDecl;

        return module->add(kind, token.pos, name, parameters, contents);
    }

    AST parseImportGroup(Module* module, TokenVisitor& visitor, Pos pos) {
        AST main = parseIdentifier(module, visitor);
        bool isLocal = false;
        while (visitor.peek().token == OperatorDiv || visitor.peek().token == PunctuatorDot) {
            Token link = visitor.read();
            if (link.token == PunctuatorDot)
                isLocal = true;
            if (visitor.done())
                error(module, link, "Unexpected end of file in import path.");
            else if (visitor.peek().token == WhitespaceNewline)
                error(module, visitor.peek(), "Unexpected line break in import path.");
            else if (visitor.peek().token == OperatorMul) {
                visitor.read();
                AST subpath = module->add(ASTKind::Wildcard);
                main = module->add(ASTKind::GetField, link.pos, main, subpath);
            } else {
                AST subpath = parseIdentifier(module, visitor);
                main = module->add(ASTKind::GetField, link.pos, main, subpath);
            }
        }

        if (visitor.peek().token == KeywordAs) {
            auto token = visitor.read();
            if (visitor.done())
                error(module, token, "Unexpected end of file in import alias specifier.");
            else if (visitor.peek().token == WhitespaceNewline)
                error(module, token, "Unexpected line break in import alias specifier.");
            else {
                AST alias = parseIdentifier(module, visitor);
                main = module->add(ASTKind::As, token.pos, main, alias);
            }
        }

        return module->add(isLocal ? ASTKind::UseLocal : ASTKind::UseModule, pos, main);
    }

    AST parseUse(Module* module, TokenVisitor& visitor) {
        Token use = visitor.read();
        vec<AST> groups;
        groups.push(parseImportGroup(module, visitor, use.pos));
        while (visitor.peek().token == PunctuatorComma) {
            visitor.readIgnoringWhitespace();
            groups.push(parseImportGroup(module, visitor, use.pos));
        }
        return groups.size() == 1 ? groups[0] : module->add(ASTKind::Do, use.pos, groups);
    }

    AST parsePrimaryStatement(Module* module, TokenVisitor& visitor) {
        Token token = visitor.peek();
        switch (token.token.symbol) {
            case KeywordBreak:
                visitor.read();
                return module->add(ASTKind::Break, token.pos);
            case KeywordContinue:
                visitor.read();
                return module->add(ASTKind::Continue, token.pos);
            case KeywordReturn: {
                visitor.read();
                constexpr u64 followSetForEmptyReturn = 0ull
                    | 1ull << (KeywordIf - FirstKeyword)
                    | 1ull << (KeywordUnless - FirstKeyword)
                    | 1ull << (KeywordWhile - FirstKeyword)
                    | 1ull << (KeywordUntil - FirstKeyword)
                    | 1ull << (KeywordElse - FirstKeyword)
                    | 1ull << (KeywordFor - FirstKeyword)
                    | 1ull << (KeywordThen - FirstKeyword);
                Token next = visitor.peek();
                bool isEmptyReturn = next.token == WhitespaceNewline
                    || next.token == MetaNone
                    || (next.token.symbol >= FirstKeyword && next.token.symbol <= LastKeyword
                        && followSetForEmptyReturn & (1ull << (next.token.symbol - FirstKeyword)));
                if (isEmptyReturn)
                    return module->add(ASTKind::Return, token.pos, module->add(ASTKind::Missing));
                return module->add(ASTKind::Return, token.pos, parseExpression(module, visitor));
            }
            case KeywordRaise:
                visitor.read();
                return module->add(ASTKind::Raise, token.pos, parseExpression(module, visitor));
            case KeywordAlias: {
                visitor.read();
                vec<AST> decls;
                AST name = parseIdentifier(module, visitor);

                AST parameters;
                if (visitor.peek().token == PunctuatorLeftParen) { // Generic type alias.
                    parameters = parseParameterTuple(module, visitor, true);
                } else
                    parameters = module->add(ASTKind::Missing);

                if (visitor.peek().token != PunctuatorColon) {
                    error(module, visitor.peek(), "Expected colon ':' in alias declaration, found '", TokenFormat(module, visitor.peek()));
                    visitor.readUpTo(WhitespaceNewline);
                    return module->add(ASTKind::Missing);
                } else
                    visitor.read();
                AST init = parseExpression(module, visitor);

                decls.push(module->add(ASTKind::AliasDecl, token.pos, name, parameters, init));
                while (visitor.peek().token == PunctuatorComma) {
                    visitor.readIgnoringWhitespace();
                    name = parseIdentifier(module, visitor);

                    if (visitor.peek().token == PunctuatorLeftParen) { // Generic type alias.
                        parameters = parseParameterTuple(module, visitor, true);
                    } else
                        parameters = module->add(ASTKind::Missing);

                    if (visitor.peek().token != PunctuatorColon) {
                        error(module, visitor.peek(), "Expected colon ':' in alias declaration, found '", TokenFormat(module, visitor.peek()));
                        visitor.readUpTo(WhitespaceNewline);
                        return module->add(ASTKind::Missing);
                    } else
                        visitor.read();
                    init = parseExpression(module, visitor);
                    decls.push(module->add(ASTKind::AliasDecl, token.pos, name, parameters, init));
                }
                return decls.size() == 1 ? decls[0] : module->add(ASTKind::Do, token.pos, decls);
            }
            case KeywordVar:
            case KeywordConst: {
                visitor.read();
                ASTKind kind = token.token == KeywordVar ? ASTKind::VarDecl : ASTKind::ConstVarDecl;
                AST type = module->add(ASTKind::Missing);
                Pos namePos = visitor.peek().pos;

                // Since we only reserve one `const` keyword for variables and
                // functions, we can't disambiguate at parse time whether i.e.
                //
                //   const a(b): c
                //
                // is a destructuring assignment or a function definition. We
                // choose to always make it a function definition and preclude
                // destructuring assignment for constant declarations.
                AST pattern = kind == ASTKind::ConstVarDecl ? parseIdentifier(module, visitor) : parsePattern(module, visitor);

                if (kind == ASTKind::ConstVarDecl && visitor.peek().token == PunctuatorLeftParen) {
                    // Constant function.
                    kind = ASTKind::ConstFunDecl;
                    auto parenPos = visitor.read().pos;
                    visitor.ignoreWhitespace(parenPos);

                    if (pattern.kind() != ASTKind::Ident)
                        error(module, namePos, "Expected identifier in const function definition.");

                    vec<AST> params;
                    while (!visitor.done() && visitor.peek().token != PunctuatorRightParen) {
                        AST id = parseIdentifier(module, visitor);
                        AST value = module->add(ASTKind::Missing);
                        if (visitor.peek().token == PunctuatorColon) {
                            visitor.read();
                            value = parseExpression(module, visitor);
                        }
                        params.push(module->add(ASTKind::ConstVarDecl, id, value));
                        if (visitor.peek().token == PunctuatorComma)
                            visitor.read();
                    }

                    visitor.stopIgnoringWhitespace();
                    if UNLIKELY(visitor.done())
                        error(module, visitor.last(), "Unexpected end of file parsing parameter list.");
                    else if UNLIKELY(visitor.peek().token != PunctuatorRightParen) {
                        error(module, visitor.peek(), "Expected comma or closing parenthese ')' in parameter list, found '", TokenFormat(module, visitor.peek()), "'.");
                        visitor.readUpToAndIncluding(PunctuatorRightParen);
                    } else
                        visitor.read();

                    if (visitor.peek().token != PunctuatorColon)
                        error(module, visitor.peek(), "Expected colon ':' in const function definition, found '", TokenFormat(module, visitor.peek()), "'.");
                    else
                        return module->add(kind, namePos, pattern, module->add(ASTKind::Tuple, parenPos, params), parseBlockOrChain(module, visitor, "const function definition", false));
                    return module->add(ASTKind::Missing);
                }

                vec<AST> decls;
                AST init;
                if (visitor.peek().token == PunctuatorColon) {
                    visitor.read();
                    init = parseExpression(module, visitor);
                } else
                    init = module->add(ASTKind::Missing);
                if (kind == ASTKind::ConstVarDecl)
                    decls.push(module->add(kind, namePos, pattern, init));
                else
                    decls.push(module->add(kind, namePos, type, pattern, init));
                while (visitor.peek().token == PunctuatorComma) {
                    visitor.readIgnoringWhitespace();
                    namePos = visitor.peek().pos;
                    pattern = parsePattern(module, visitor);
                    if (visitor.peek().token == PunctuatorColon) {
                        visitor.read();
                        init = parseExpression(module, visitor);
                    } else
                        init = module->add(ASTKind::Missing);
                    if (kind == ASTKind::ConstVarDecl)
                        decls.push(module->add(kind, namePos, pattern, init));
                    else
                        decls.push(module->add(kind, namePos, type, pattern, init));
                }
                return decls.size() == 1 ? decls[0] : module->add(ASTKind::Do, token.pos, decls);
            }
            case KeywordFun: {
                visitor.read();
                Pos namePos = visitor.peek().pos;
                AST name = parseIdentifier(module, visitor);
                if (visitor.peek().token != PunctuatorLeftParen) {
                    error(module, visitor.peek(), "Expected opening parenthese '(' in function declaration, found '", TokenFormat(module, visitor.peek()));
                    visitor.readUpTo(WhitespaceNewline);
                    return module->add(ASTKind::Missing);
                }
                AST parameters = parseParameterTuple(module, visitor, false);
                AST raises;
                if (visitor.peek().token == KeywordRaises)
                    raises = parseRaisesDeclaration(module, visitor);
                else
                    raises = module->add(ASTKind::Missing);
                AST body;
                if (visitor.done() || visitor.peek().token == WhitespaceNewline)
                    body = module->add(ASTKind::Missing);
                else
                    body = parseBlockOrChain(module, visitor, "function declaration", false);
                return module->add(ASTKind::FunDecl, namePos, module->add(ASTKind::Missing), name, parameters, raises, body);
            }
            case KeywordIn: {
                visitor.read();
                vec<Pos, 4> namePositions;
                vec<AST, 4> names;
                namePositions.push(visitor.peek().pos);
                names.push(parseIdentifier(module, visitor));
                while (visitor.peek().token == PunctuatorDot) {
                    visitor.read();
                    namePositions.push(visitor.peek().pos);
                    names.push(parseIdentifier(module, visitor));
                }
                AST body;
                if (visitor.peek().token != PunctuatorColon)
                    error(module, visitor.peek(), "Expected colon ':' in namespace declaration, found '", TokenFormat(module, visitor.peek()), "'.");
                else
                    body = parseBlockOrChain(module, visitor, "namespace declaration", false);

                while (names.size())
                    body = module->add(ASTKind::Namespace, namePositions.pop(), names.pop(), body);
                return body;
            }
            case KeywordType:
                return parseTypeDecl(module, visitor, KeywordType);
            case KeywordCase:
                return parseTypeDecl(module, visitor, KeywordCase);
            case KeywordUse:
                return parseUse(module, visitor);
            default:
                return parseExpressionOrJuxtaposition(module, visitor, true, true, true);
        }
    }

    AST transformExpressionToPattern(Module* module, AST expression) {
        // No-op currently. But we might make this significant in the future.
        return expression;
    }

    enum class BindingKind {
        Error,
        In,                 // for x in xs
        RangeIncreasing,    // for 0 <= x < 10
        RangeDecreasing     // for 10 > x >= 0
    };

    struct BindingGroup {
        BindingKind kind;
        Pos pos;
        union {
            struct { ASTWord pattern, items; };
            struct { ASTWord low, var, high; bool lowInclusive, highInclusive; };
        };
        ASTWord breakIf, continueUnless;
        bool hasBreakIf, hasContinueUnless;
    };

    struct ForLoop {
        vec<BindingGroup, 2> bindings;
    };

    BindingGroup parseBindingGroup(Module* module, TokenVisitor& visitor) {
        AST lhs = parsePrimary(module, visitor);
        BindingGroup binding;
        Token next = visitor.peek();
        binding.pos = next.pos;
        if (next.token == KeywordIn) {
            AST pattern = transformExpressionToPattern(module, lhs);
            visitor.read();
            AST items = parseExpression(module, visitor);
            binding.pattern = pattern.asOperand();
            binding.items = items.asOperand();
            binding.kind = BindingKind::In;
        } else if (next.token == OperatorLess
            || next.token == OperatorGreater
            || next.token == OperatorLessEqual
            || next.token == OperatorGreaterEqual) {
            bool isDescending = next.token == OperatorGreater || next.token == OperatorGreaterEqual;
            bool firstInclusive = next.token == OperatorLessEqual || next.token == OperatorGreaterEqual;
            binding.kind = isDescending ? BindingKind::RangeDecreasing : BindingKind::RangeIncreasing;
            visitor.read();

            AST middle = parseExpression(module, visitor, precedenceOf(OperatorLess) + 1);

            AST rhs;
            bool secondInclusive = false;
            next = visitor.peek();
            if (next.token == OperatorLess
                || next.token == OperatorGreater
                || next.token == OperatorLessEqual
                || next.token == OperatorGreaterEqual) {
                secondInclusive = next.token == OperatorLessEqual || next.token == OperatorGreaterEqual;
                assert(isDescending == (next.token == OperatorGreater || next.token == OperatorGreaterEqual));
                visitor.read();

                rhs = parseExpression(module, visitor, precedenceOf(OperatorLess) + 1);
            } else {
                if (isDescending) {
                    secondInclusive = true;
                    rhs = module->add(ASTKind::Unsigned, Constant::UnsignedConst(0));
                } else {
                    rhs = middle;
                    middle = lhs;
                    secondInclusive = firstInclusive;
                    firstInclusive = true;
                    lhs = module->add(ASTKind::Unsigned, Constant::UnsignedConst(0));
                }
            }

            assert(lhs.kind() != ASTKind::VarDecl);
            assert(middle.kind() == ASTKind::VarDecl || middle.kind() == ASTKind::Ident);
            assert(rhs.kind() != ASTKind::VarDecl);
            binding.var = middle.asOperand();
            binding.low = isDescending ? rhs.asOperand() : lhs.asOperand();
            binding.high = isDescending ? lhs.asOperand() : rhs.asOperand();
            binding.lowInclusive = isDescending ? secondInclusive : firstInclusive;
            binding.highInclusive = isDescending ? firstInclusive : secondInclusive;
        } else {
            error(module, next, "Unexpected token '", TokenFormat(module, next), "' in for loop binding.");
            binding.kind = BindingKind::Error;
        }

        binding.hasBreakIf = binding.hasContinueUnless = false;

        next = visitor.peek();
        if (next.token == KeywordIf || next.token == KeywordUnless) {
            visitor.read();
            AST condition = parseExpression(module, visitor);
            condition = next.token == KeywordUnless ? module->add(ASTKind::Not, next.pos, condition) : condition;
            binding.continueUnless = condition.asOperand();
            binding.hasContinueUnless = true;
        }

        next = visitor.peek();
        if (next.token == KeywordUntil || next.token == KeywordWhile) {
            visitor.read();
            AST condition = parseExpression(module, visitor);
            condition = next.token == KeywordWhile ? module->add(ASTKind::Not, next.pos, condition) : condition;
            binding.breakIf = condition.asOperand();
            binding.hasBreakIf = true;
        }

        return binding;
    }

    AST parseForLoop(Module* module, TokenVisitor& visitor, Pos pos, AST preParsedBody, bool isPostfix) {
        vec<BindingGroup> bindings;
        bindings.push(parseBindingGroup(module, visitor));
        while (visitor.peek().token == PunctuatorComma) {
            visitor.readIgnoringWhitespace();
            bindings.push(parseBindingGroup(module, visitor));
        }

        AST body = preParsedBody;
        if (!isPostfix) {
            if (visitor.peek().token != PunctuatorColon) {
                error(module, visitor.peek(), "Expected colon ':' in for loop statement, found '", TokenFormat(module, visitor.peek()), "'.");
                visitor.readUpToAndIncluding(WhitespaceNewline);
                return module->add(ASTKind::Missing);
            }
            visitor.read();
            if (visitor.done() || visitor.peek().token == WhitespaceNewline)
                body = parseBlock(module, visitor);
            else
                body = parseStatement(module, visitor);
        }

        vec<AST> seq;

        // First, we set up all our iterators.

        for (const auto& [i, binding] : enumerate(bindings)) switch (binding.kind) {
            case BindingKind::Error:
                break;
            case BindingKind::In: {
                array<i8, 64> buffer;
                auto name = prints(buffer, "__iter", i);
                seq.push(module->add(ASTKind::VarDecl, binding.pos, module->add(ASTKind::Missing), Identifier(module->sym(name)),
                    module->add(ASTKind::CallMethod, binding.pos, Identifier(MethodIter), module->fromOperand(binding.items))));
                binding.items = seq.back().child(1).asOperand();
                break;
            }
            case BindingKind::RangeDecreasing: {
                array<i8, 64> buffer;
                auto name = prints(buffer, "__iter", i);
                AST init = module->fromOperand(binding.high);
                if (!binding.highInclusive)
                    init = module->add(ASTKind::Sub, binding.pos, init, Constant::UnsignedConst(1));
                seq.push(module->add(ASTKind::VarDecl, binding.pos, module->add(ASTKind::Missing), Identifier(module->sym(name)), init));
                binding.high = seq.back().child(1).asOperand();
                name = prints(buffer, "__end", i);
                seq.push(module->add(ASTKind::VarDecl, binding.pos, module->add(ASTKind::Missing), Identifier(module->sym(name)), module->fromOperand(binding.low)));
                binding.low = seq.back().child(1).asOperand();
                break;
            }
            case BindingKind::RangeIncreasing: {
                array<i8, 64> buffer;
                auto name = prints(buffer, "__iter", i);
                AST init = module->fromOperand(binding.low);
                if (!binding.lowInclusive)
                    init = module->add(ASTKind::Add, binding.pos, init, Constant::UnsignedConst(1));
                seq.push(module->add(ASTKind::VarDecl, binding.pos, module->add(ASTKind::Missing), Identifier(module->sym(name)), init));
                binding.low = seq.back().child(1).asOperand();
                name = prints(buffer, "__end", i);
                seq.push(module->add(ASTKind::VarDecl, binding.pos, module->add(ASTKind::Missing), Identifier(module->sym(name)), module->fromOperand(binding.high)));
                binding.high = seq.back().child(1).asOperand();
                break;
            }
        }

        // Next, we set up our top-level condition.

        vec<AST> conditions;
        for (const auto& binding : bindings) {
            switch (binding.kind) {
                case BindingKind::Error:
                    conditions.push(module->add(ASTKind::Bool, Constant::BoolConst(true)));
                    break;
                case BindingKind::In:
                    conditions.push(module->add(ASTKind::Not, binding.pos, module->add(ASTKind::CallMethod, binding.pos, Identifier(MethodDone), module->fromOperand(binding.items))));
                    break;
                case BindingKind::RangeDecreasing:
                    conditions.push(module->add(binding.lowInclusive ? ASTKind::GreaterEq : ASTKind::Greater, binding.pos, module->fromOperand(binding.high), module->fromOperand(binding.low)));
                    break;
                case BindingKind::RangeIncreasing:
                    conditions.push(module->add(binding.highInclusive ? ASTKind::LessEq : ASTKind::Less, binding.pos, module->fromOperand(binding.low), module->fromOperand(binding.high)));
                    break;
            }
        }
        assert(conditions.size() > 0);
        AST condition = conditions[0];
        for (u32 i = 1; i < conditions.size(); i ++)
            condition = module->add(ASTKind::And, pos, condition, conditions[i]);

        // Now we're going to build the actual body of the loop. We start by
        // introducing declarations for any named bindings we have. If any of
        // these bindings have an until or while clause, we break if that
        // condition fails immediately after initializing the corresponding
        // binding.

        vec<AST> bodyStmts;

        for (const auto& binding : bindings) {
            switch (binding.kind) {
                case BindingKind::Error:
                    break;
                case BindingKind::In:
                    bodyStmts.push(module->add(ASTKind::VarDecl, binding.pos, module->add(ASTKind::Missing), module->fromOperand(binding.pattern),
                        module->add(ASTKind::CallMethod, binding.pos, Identifier(MethodRead), module->fromOperand(binding.items))));
                    break;
                case BindingKind::RangeDecreasing:
                    bodyStmts.push(module->add(ASTKind::VarDecl, binding.pos, module->add(ASTKind::Missing), module->fromOperand(binding.var), module->fromOperand(binding.high)));
                    break;
                case BindingKind::RangeIncreasing:
                    bodyStmts.push(module->add(ASTKind::VarDecl, binding.pos, module->add(ASTKind::Missing), module->fromOperand(binding.var), module->fromOperand(binding.low)));
                    break;
            }
            if (binding.hasBreakIf)
                bodyStmts.push(module->add(ASTKind::If, binding.pos, module->fromOperand(binding.breakIf), module->add(ASTKind::Break, binding.pos)));
        }



        // Now we construct the body. This will be the original lexical body of
        // the loop, plus if statements wrapping it for each if or unless
        // clause in the binding groups, in reverse order.

        for (const auto& binding : reversed(bindings)) if (binding.hasContinueUnless)
            body = module->add(ASTKind::If, binding.pos, module->fromOperand(binding.continueUnless), body);

        bodyStmts.push(body);

        // Finally, we add the update step, bumping each binding group's
        // iterator for the next iteration.

        for (const auto& binding : bindings) switch (binding.kind) {
            case BindingKind::Error:
                break;
            case BindingKind::In:
                bodyStmts.push(module->add(ASTKind::Assign, binding.pos, module->fromOperand(binding.items),
                    module->add(ASTKind::CallMethod, binding.pos, Identifier(MethodNext), module->fromOperand(binding.items))));
                break;
            case BindingKind::RangeDecreasing:
                bodyStmts.push(module->add(ASTKind::PreDecr, binding.pos, module->fromOperand(binding.pattern)));
                break;
            case BindingKind::RangeIncreasing:
                bodyStmts.push(module->add(ASTKind::PreIncr, binding.pos, module->fromOperand(binding.pattern)));
                break;
        }

        // We construct the loop from the condition and body statements.

        AST loop = module->add(ASTKind::While, pos, condition, module->add(ASTKind::Do, pos, bodyStmts));

        // Then combine the loop with the initialization statements in a Do.

        return module->add(ASTKind::DoScoped, pos, seq, loop);
    }

    AST parseCase(Module* module, TokenVisitor& visitor) {
        if (visitor.peek().token != KeywordCase && visitor.peek().token != KeywordElse) {
            error(module, visitor.peek().pos, "Expected keyword 'case' or 'else' in match case, found '", TokenFormat(module, visitor.peek()), "'.");
            return module->add(ASTKind::Missing);
        }

        bool isElse = visitor.peek().token == KeywordElse;
        Pos pos = visitor.read().pos;
        AST pattern = isElse ? module->add(ASTKind::Missing) : parsePattern(module, visitor);

        if (visitor.peek().token != PunctuatorColon) {
            error(module, visitor.peek(), "Expected colon ':' in match case, found '", TokenFormat(module, visitor.peek()), "'.");
            visitor.readUpToAndIncluding(WhitespaceNewline);
            return module->add(ASTKind::Missing);
        }
        visitor.read();

        AST body;
        if (visitor.done() || visitor.peek().token == WhitespaceNewline)
            body = parseBlock(module, visitor);
        else
            body = parseStatement(module, visitor);

        return module->add(ASTKind::Case, pos, pattern, body);
    }

    AST parseSingleInlineStatement(Module* module, TokenVisitor& visitor) {
        u32 line = visitor.peek().pos.line;
        AST ast = parsePrimaryStatement(module, visitor);
        if (visitor.done() || visitor.peek().pos.line > line)
            return ast;
        while (true) {
            Token token = visitor.peek();
            switch (token.token.symbol) {
                case KeywordIf:
                    visitor.read();
                    ast = module->add(ASTKind::If, parseExpression(module, visitor), ast);
                    continue;
                case KeywordUnless:
                    visitor.read();
                    ast = module->add(ASTKind::If, module->add(ASTKind::Not, parseExpression(module, visitor)), ast);
                    continue;
                case KeywordWhile:
                    visitor.read();
                    ast = module->add(ASTKind::While, parseExpression(module, visitor), ast);
                    continue;
                case KeywordUntil:
                    visitor.read();
                    ast = module->add(ASTKind::While, module->add(ASTKind::Not, parseExpression(module, visitor)), ast);
                    continue;
                case KeywordFor:
                    // Postfix for-loop must be the last in the chain (other
                    // than a Then) to avoid a parsing ambiguity.
                    return parseForLoop(module, visitor, visitor.read().pos, ast, true);
                default:
                    return ast;
            }
        }
    }

    AST parseInlineStatement(Module* module, TokenVisitor& visitor) {
        // This function pretty much just handles the `then` operator, which has
        // lower precedence than all other inline statement operators. Since it's
        // (currently) just the one it doesn't feel worth it to make a whole other
        // precedence parser for inline statements.

        u32 line = visitor.peek().pos.line;
        AST ast = parseSingleInlineStatement(module, visitor);
        if (visitor.done() || visitor.peek().pos.line > line)
            return ast;

        while (visitor.peek().token == KeywordThen) {
            Pos pos = visitor.read().pos;
            ast = module->add(ASTKind::Then, pos, ast, parseSingleInlineStatement(module, visitor));
        }
        return ast;
    }

    void expectStatementTerminator(Module* module, TokenVisitor& visitor) {
        if (!visitor.done() && visitor.peek().token != WhitespaceNewline)
            error(module, visitor.peek(), "Expected newline after statement, found '", TokenFormat(module, visitor.peek()), "'.");
        if (!visitor.done())
            visitor.read();
    }

    AST parseStatement(Module* module, TokenVisitor& visitor) {
        consumeNewlines(visitor);
        Token token = visitor.peek();
        switch (token.token.symbol) {
            case KeywordIf: {
                Pos pos = visitor.read().pos;
                AST cond = parseExpression(module, visitor);
                AST body = parseBlockOrChain(module, visitor, "if statement", false);
                consumeNewlines(visitor);
                if (visitor.peek().token == KeywordElse) {
                    visitor.read();
                    AST elseBody = parseBlockOrChain(module, visitor, "if-else statement", false);
                    return module->add(ASTKind::IfElse, pos, cond, body, elseBody);
                } else
                    return module->add(ASTKind::If, pos, cond, body);
            }
            case KeywordUnless: {
                Pos pos = visitor.read().pos;
                AST cond = parseExpression(module, visitor);
                AST body = parseBlockOrChain(module, visitor, "unless statement", false);
                return module->add(ASTKind::If, pos, module->add(ASTKind::Not, cond), body);
            }
            case KeywordWhile: {
                Pos pos = visitor.read().pos;
                AST cond = parseExpression(module, visitor);
                AST body = parseBlockOrChain(module, visitor, "while statement", false);
                return module->add(ASTKind::While, pos, cond, body);
            }
            case KeywordUntil: {
                Pos pos = visitor.read().pos;
                AST cond = parseExpression(module, visitor);
                AST body = parseBlockOrChain(module, visitor, "until statement", false);
                return module->add(ASTKind::While, pos, module->add(ASTKind::Not, cond), body);
            }
            case KeywordFor:
                return parseForLoop(module, visitor, visitor.read().pos, {}, false);
            case KeywordMatch: {
                Pos pos = visitor.read().pos;
                AST cond = parseExpression(module, visitor);
                if (visitor.peek().token != PunctuatorColon) {
                    error(module, visitor.peek(), "Expected colon ':' in match statement, found '", TokenFormat(module, visitor.peek()), "'.");
                    visitor.readUpToAndIncluding(WhitespaceNewline);
                    return module->add(ASTKind::Missing);
                }
                visitor.read();

                vec<AST> cases;
                if (visitor.done())
                    return module->add(ASTKind::Match, pos, cond, cases);

                if (visitor.peek().token != WhitespaceNewline) {
                    error(module, visitor.peek(), "Expected newline after colon ':' in match statement, found '", TokenFormat(module, visitor.peek()), "'.");
                    visitor.readUpToAndIncluding(WhitespaceNewline);
                    return module->add(ASTKind::Missing);
                }
                visitor.read();

                if (visitor.peek().token != WhitespaceIndent) {
                    error(module, visitor.peek().pos, "Expected indented block.");
                    return module->add(ASTKind::Match, pos, cond, cases);
                }
                if (visitor.isIgnoringWhitespace()) {
                    error(module, pos, "Indented block not permitted within enclosed expression.")
                        .note(module, visitor.whitespaceStack.back(), "Enclosed expression started here.");
                }
                visitor.read();
                while (!visitor.done() && visitor.peek().token != WhitespaceDedent) {
                    consumeNewlines(visitor);
                    cases.push(parseCase(module, visitor));
                }
                visitor.read();
                return module->add(ASTKind::Match, pos, cond, cases);
            }
            case KeywordOn:
            case KeywordDefault:
                unreachable("TODO");
            default: {
                u32 line = visitor.peek().pos.line;
                AST result = parseInlineStatement(module, visitor);
                if (visitor.peek().pos.line > line)
                    return result;
                if (visitor.peek().token != WhitespaceNewline && visitor.peek().token != WhitespaceDedent && !visitor.done())
                    error(module, visitor.peek(), "Expected newline or end of file after statement, found '", TokenFormat(module, visitor.peek()), "'.");
                if (visitor.peek().token != WhitespaceDedent)
                    visitor.read();
                return result;
            }
        }
    }

    AST parseModuleName(Module* module, TokenVisitor& visitor) {
        AST name = parseIdentifier(module, visitor);
        while (!visitor.done() && visitor.peek().token == PunctuatorDot) {
            Pos pos = visitor.read().pos;
            name = module->add(ASTKind::GetField, pos, name, parseIdentifier(module, visitor));
        }
        return name;
    }

    AST parsePossibleExport(Module* module, TokenVisitor& visitor) {
        consumeNewlines(visitor);
        Token token = visitor.peek();
        if (token.token == KeywordExport) {
            visitor.read();
            AST decl = parseStatement(module, visitor);
            return module->add(ASTKind::Export, token.pos, decl);
        }
        return parseStatement(module, visitor);
    }

    AST parseTopLevel(Module* module, TokenVisitor& visitor) {
        vec<AST, 32> topLevel;
        module->setTempTopLevel(topLevel);
        while (!visitor.done()) {
            consumeNewlines(visitor);
            if (visitor.done())
                break;
            topLevel.push(parsePossibleExport(module, visitor));
        }
        return module->add(ASTKind::TopLevel, topLevel);
    }

    NOINLINE Artifact* parse(Artifact* artifact) {
        assert(artifact->kind == ArtifactKind::Tokens);

        if (!didInitPrecedences) {
            didInitPrecedences = true;
            precedences[OperatorAssign - FirstOperator] = 10;
            precedences[OperatorAddAssign - FirstOperator] = 10;
            precedences[OperatorSubAssign - FirstOperator] = 10;
            precedences[OperatorMulAssign - FirstOperator] = 10;
            precedences[OperatorDivAssign - FirstOperator] = 10;
            precedences[OperatorRemAssign - FirstOperator] = 10;
            precedences[OperatorBitAndAssign - FirstOperator] = 10;
            precedences[OperatorBitXorAssign - FirstOperator] = 10;
            precedences[OperatorBitOrAssign - FirstOperator] = 10;
            precedences[OperatorLeftShiftAssign - FirstOperator] = 10;
            precedences[OperatorRightShiftAssign - FirstOperator] = 10;
            precedences[OperatorLeftRotateAssign - FirstOperator] = 10;
            precedences[OperatorRightRotateAssign - FirstOperator] = 10;
            precedences[OperatorExpAssign - FirstOperator] = 10;
            precedences[KeywordIf - FirstOperator] = 20;
            precedences[KeywordOr - FirstOperator] = 30;
            precedences[KeywordAnd - FirstOperator] = 40;
            precedences[OperatorEqual - FirstOperator] = 50;
            precedences[OperatorNotEqual - FirstOperator] = 50;
            precedences[KeywordIs - FirstOperator] = 50;
            precedences[KeywordIsNot - FirstOperator] = 50;
            precedences[KeywordIn - FirstOperator] = 50;
            precedences[OperatorLess - FirstOperator] = 60;
            precedences[OperatorLessEqual - FirstOperator] = 60;
            precedences[OperatorGreater - FirstOperator] = 60;
            precedences[OperatorGreaterEqual - FirstOperator] = 60;
            precedences[OperatorRange - FirstOperator] = 70;
            precedences[OperatorBitOr - FirstOperator] = 80;
            precedences[OperatorBitXor - FirstOperator] = 90;
            precedences[OperatorBitAnd - FirstOperator] = 100;
            precedences[OperatorLeftShift - FirstOperator] = 110;
            precedences[OperatorRightShift - FirstOperator] = 110;
            precedences[OperatorLeftRotate - FirstOperator] = 110;
            precedences[OperatorRightRotate - FirstOperator] = 110;
            precedences[OperatorAdd - FirstOperator] = 120;
            precedences[OperatorSub - FirstOperator] = 120;
            precedences[OperatorMul - FirstOperator] = 130;
            precedences[OperatorDiv - FirstOperator] = 130;
            precedences[OperatorRem - FirstOperator] = 130;
            precedences[OperatorExp - FirstOperator] = 140;
            precedences[KeywordAs - FirstOperator] = 150;
        }

        Module* module = new Module(artifact->parent->compilation, artifact, artifact->as<Tokens>()->takeSource(), move(artifact->as<Tokens>()->lineOffsets));
        TokenVisitor visitor(module, artifact->as<Tokens>()->tokens);
        module->setTopLevel(parseTopLevel(module, visitor));

        artifact->update(ArtifactKind::ParsedAST, module);
        if UNLIKELY(config::printParseTree)
            module->print(module->compilation), println();
        return artifact;
    }

    inline pair<ASTKind, i32> kindFromSymbol(Module* module, Symbol symbol) {
        #define DEFINE_CASE(upper, lower, sym, isLeaf, arity) if (!isLeaf && module->str(symbol) == cstring(#sym)) return { ASTKind:: upper, (i32)arity };
        FOR_EACH_AST_KIND(DEFINE_CASE)
        #undef DEFINE_CASE

        unreachable("Unexpected symbol ", module->str(symbol));
    }

    NOINLINE AST parseSexp(Module* module, TokenVisitor& visitor) {
        Token token = visitor.peek();
        if (token.token == PunctuatorLeftParen) {
            visitor.read();
            Token head = visitor.read();
            pair<ASTKind, i32> pair = kindFromSymbol(module, head.token);
            ASTKind kind = pair.first; // TODO: Why does structured binding not work here?
            i32 arity = pair.second;

            vec<AST> children;
            while (!visitor.done() && visitor.peek().token != PunctuatorRightParen)
                children.push(parseSexp(module, visitor));
            assert(visitor.peek().token == PunctuatorRightParen);
            visitor.read();
            assert(children.size() == arity || arity == -1);

            return module->add(kind, head.pos, children);
        } else {
            auto text = module->str(token.token);
            assert(text.size());
            auto handleVar = [&](Symbol symbol) -> AST {
                if (module->str(symbol) == cstring("true"))
                    return module->add(ASTKind::Bool, Constant::BoolConst(true));
                if (module->str(symbol) == cstring("false"))
                    return module->add(ASTKind::Bool, Constant::BoolConst(false));
                if UNLIKELY(module->str(symbol) == cstring("missing"))
                    return module->add(ASTKind::Missing);
                if UNLIKELY(module->str(symbol) == cstring("uninit"))
                    return module->add(ASTKind::Uninit);
                if UNLIKELY(module->str(symbol) == cstring("wildcard"))
                    return module->add(ASTKind::Wildcard);
                return module->add(ASTKind::Ident, Identifier(symbol));
            };
            switch (text[0]) {
                case 'a' ... 'z':
                case 'A' ... 'Z':
                case '_':
                    visitor.read();
                    return handleVar(token.token);
                case '0' ... '9':
                    visitor.read();
                    for (i8 c : text) if (c == '.')
                        return module->add(ASTKind::Float, Constant::FloatConst(tofloat(text)));
                    return module->add(ASTKind::Unsigned, Constant::UnsignedConst(touint(text)));
                case '.':
                    // We know from above we aren't a reserved symbol like ., .., or ...; so
                    // we must be a decimal number that starts with the point.
                    visitor.read();
                    return module->add(ASTKind::Float, Constant::FloatConst(tofloat(text)));
                case '"':
                    visitor.read();
                    return module->add(ASTKind::String, Constant::StringConst(escapeAndUnquote(module, token.pos, text)));
                case '\'': {
                    visitor.read();
                    auto str = module->str(escapeAndUnquote(module, token.pos, text));
                    rune r;
                    utf8_decode(str.data(), str.size(), &r, 1);
                    return module->add(ASTKind::Char, Constant::CharConst(r.get()));
                }
                case '`':
                    visitor.read();
                    return handleVar(escapeAndUnquote(module, token.pos, text));
                case 128 ... 255: {
                        visitor.read();
                    rune first;
                    utf8_decode(text.data(), text.size(), &first, 1);
                    if (utf8_is_digit(first)) {
                        for (i8 c : text) if (c == '.')
                            return module->add(ASTKind::Float, Constant::FloatConst(tofloat(text)));
                        return module->add(ASTKind::Unsigned, Constant::UnsignedConst(touint(text)));
                    } else
                        return handleVar(token.token);
                    fallthrough;
                }
                default:
                    unreachable("Unexpected token '", module->str(token.token), "'");
            }
        }
    }

    Artifact* parseAsSexp(Artifact* artifact) {
        assert(artifact->kind == ArtifactKind::Tokens);

        Module* module = new Module(artifact->parent->compilation, artifact, artifact->as<Tokens>()->takeSource(), move(artifact->as<Tokens>()->lineOffsets));
        TokenVisitor visitor(module, artifact->as<Tokens>()->tokens);
        visitor.ignoreWhitespace(visitor.peek().pos); // Ignore whitespace in s-exp mode.

        vec<AST> topLevel;
        while (!visitor.done())
            topLevel.push(parseSexp(module, visitor));
        module->setTopLevel(module->add(ASTKind::TopLevel, Pos{ 0, 0 }, topLevel));
        artifact->update(ArtifactKind::ParsedAST, module);
        if UNLIKELY(config::printParseTree)
            module->print(module->compilation), println();
        return artifact;
    }
}