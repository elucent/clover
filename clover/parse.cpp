#include "clover/parse.h"
#include "clover/ast.h"
#include "clover/compilation.h"
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

        IndexedToken peek() const {
            if UNLIKELY(offset >= tokens.size())
                return IndexedToken(MetaNone, Pos(0, 0), 0);
            if UNLIKELY(impliedIndents && justParsedLine)
                return IndexedToken(WhitespaceIndent, tokens[offset].pos, offset);
            return IndexedToken(tokens[offset], offset);
        }

        IndexedToken peek2() const {
            if UNLIKELY(offset + 1 >= tokens.size())
                return IndexedToken(MetaNone, Pos(0, 0), 0);
            if UNLIKELY((impliedIndents && tokens[offset].token == WhitespaceNewline)
                || (impliedIndents > 1 && justParsedLine))
                return IndexedToken(WhitespaceIndent, tokens[offset + 1].pos, offset + 1);
            return IndexedToken(tokens[offset + 1], offset + 1);
        }

        IndexedToken peekIgnoringWhitespace() const {
            u32 i = offset;
            while (i < tokens.size() && tokens[i].token.symbol < 64ull && (1ull << tokens[i].token.symbol & SkipIfEnclosedMask))
                i ++;
            if (i >= tokens.size())
                return IndexedToken(MetaNone, Pos(0, 0), 0);
            return IndexedToken(tokens[i], i);
        }

        IndexedToken last() const {
            return IndexedToken(tokens[tokens.size() - 1], tokens.size() - 1);
        }

        IndexedToken readIgnoringWhitespace() {
            ignoreWhitespace(tokens[offset].pos);
            IndexedToken result = read();
            stopIgnoringWhitespace();
            return result;
        }

        IndexedToken read() {
            if UNLIKELY(offset >= tokens.size())
                return IndexedToken(MetaNone, Pos(0, 0), 0);
            if UNLIKELY(impliedIndents && justParsedLine && !isIgnoringWhitespace()) {
                impliedIndents --;
                return IndexedToken(WhitespaceIndent, tokens[offset].pos, offset);
            }
            justParsedLine = false;
            IndexedToken next = IndexedToken(tokens[offset], offset);
            offset ++;
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

    IndexedAST parsePrimaryNoSuffix(Module* module, TokenVisitor& visitor);
    IndexedAST parsePrimary(Module* module, TokenVisitor& visitor);
    IndexedAST parseExpression(Module* module, TokenVisitor& visitor, u32 minPrecedence = 0);
    IndexedAST parseExpressionOrJuxtaposition(Module* module, TokenVisitor& visitor, bool allowMultiple, bool allowFunction, bool allowInitializer);
    IndexedAST parseStatementInitial(Module* module, TokenVisitor& visitor);
    IndexedAST parseInlineStatement(Module* module, TokenVisitor& visitor);
    IndexedAST parseStatement(Module* module, TokenVisitor& visitor);

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

    IndexedAST parsePrimaryNoSuffix(Module* module, TokenVisitor& visitor) {
        auto token = visitor.peek();
        if (token.token.symbol < NumReservedSymbols) switch (token.token.symbol) {
            case PunctuatorLeftParen: {
                IndexedToken rparen;
                visitor.ignoreWhitespace(token.pos);
                auto lparen = visitor.read();
                if (visitor.peek().token == PunctuatorRightParen) {
                    visitor.stopIgnoringWhitespace();
                    auto rparen = visitor.read();
                    return module->addInitial(ASTKind::Tuple, origin(lparen, rparen)); // Empty tuple expression.
                }
                IndexedAST nextExpr = parseExpression(module, visitor);
                if (visitor.peek().token == PunctuatorComma) { // Tuple expression.
                    vec<IndexedAST> tup;
                    tup.push(nextExpr);
                    while (!visitor.done() && visitor.peek().token != PunctuatorRightParen) {
                        if UNLIKELY(visitor.peek().token != PunctuatorComma) {
                            error(module, visitor.peek(), "Expected comma in tuple expression, found '", TokenFormat(module, visitor.peek()), "'.");
                            visitor.stopIgnoringWhitespace();
                            visitor.readUpToAndIncluding(PunctuatorRightParen);
                            return module->addInitial(ASTKind::Tuple, origin(), tup);
                        }
                        visitor.read();
                        Pos beforePos = visitor.peek().pos;
                        tup.push(parseExpression(module, visitor));
                        if (visitor.peek().token == PunctuatorColon) {
                            Pos pos = visitor.read().pos;
                            if UNLIKELY(tup.back().kind() != ASTKind::Ident && tup.back().kind() != ASTKind::Missing)
                                error(module, beforePos, "Expected identifier in named tuple field, found '", tup.back(), "'.");
                            auto child = parseExpression(module, visitor);
                            tup.back() = module->addInitial(ASTKind::NamedParameter, origin(tup.back(), child), tup.back(), child);
                        }
                    }
                    visitor.stopIgnoringWhitespace();
                    if (visitor.done())
                        error(module, visitor.last(), "Unexpected end of file in tuple expression.");
                    else if (visitor.peek().token != PunctuatorRightParen) {
                        error(module, visitor.peek(), "Expected closing parenthese ')', found '", TokenFormat(module, visitor.peek()), "'.");
                        visitor.readUpToAndIncluding(PunctuatorRightParen);
                    } else
                        rparen = visitor.read();
                    return module->addInitial(ASTKind::Tuple, origin(lparen, rparen), tup);
                }
                visitor.stopIgnoringWhitespace();
                if (visitor.done())
                    error(module, visitor.last().pos, "Unexpected end of file in parenthetical expression.");
                else if (visitor.peek().token != PunctuatorRightParen) {
                    error(module, visitor.peek().pos, "Expected closing parenthese ')', found '", TokenFormat(module, visitor.peek()), "'.");
                    visitor.readUpToAndIncluding(PunctuatorRightParen);
                } else
                    rparen = visitor.read();
                return module->addInitial(ASTKind::Paren, origin(lparen, rparen), nextExpr);
            }
            case PunctuatorLeftBracket: {
                visitor.ignoreWhitespace(token.pos);
                auto lbrack = visitor.read();
                IndexedToken rbrack;
                if (visitor.peek().token == PunctuatorRightBracket) { // Empty list.
                    visitor.stopIgnoringWhitespace();
                    rbrack = visitor.read();
                    return module->addInitial(ASTKind::List, origin(lbrack, rbrack));
                }
                vec<IndexedAST> items;
                items.push(parseExpression(module, visitor));
                if (visitor.peek().token == PunctuatorComma) { // List expression.
                    while (!visitor.done() && visitor.peek().token != PunctuatorRightBracket) {
                        if UNLIKELY(visitor.peek().token != PunctuatorComma) {
                            error(module, visitor.peek(), "Expected comma in list expression, found '", TokenFormat(module, visitor.peek()), "'.");
                            visitor.stopIgnoringWhitespace();
                            visitor.readUpToAndIncluding(PunctuatorRightBracket);
                            return module->addInitial(ASTKind::List, origin(), items);
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
                } else
                    rbrack = visitor.read();
                return module->addInitial(ASTKind::List, origin(lbrack, rbrack), items);
            }
            case OperatorBitOr: {
                auto lpipe = visitor.read();
                IndexedToken rpipe;
                auto nextExpr = parsePrimary(module, visitor);
                if (visitor.done())
                    error(module, visitor.last().pos, "Unexpected end of file in length expression.");
                else if (visitor.peek().token != OperatorBitOr)
                    error(module, visitor.peek().pos, "Expected closing pipe '|', found '", TokenFormat(module, visitor.peek()), "'.");
                else
                    rpipe = visitor.read();
                return module->addInitial(ASTKind::Length, origin(lpipe, rpipe), nextExpr);
            }
            case OperatorIncr: {
                visitor.read();
                auto child = parsePrimary(module, visitor);
                return module->addInitial(ASTKind::PreIncr, origin(token, child), child);
            }
            case OperatorDecr: {
                visitor.read();
                auto child = parsePrimary(module, visitor);
                return module->addInitial(ASTKind::PreDecr, origin(token, child), child);
            }
            case OperatorAdd: {
                visitor.read();
                auto child = parsePrimary(module, visitor);
                return module->addInitial(ASTKind::Plus, origin(token, child), child);
            }
            case OperatorSub: {
                visitor.read();
                auto child = parsePrimary(module, visitor);
                return module->addInitial(ASTKind::Minus, origin(token, child), child);
            }
            case OperatorMul: {
                visitor.read();
                auto child = parsePrimary(module, visitor);
                return module->addInitial(ASTKind::Deref, origin(token, child), child);
            }
            case OperatorBitNot: {
                visitor.read();
                auto child = parsePrimary(module, visitor);
                return module->addInitial(ASTKind::BitNot, origin(token, child), child);
            }
            case KeywordNot: {
                visitor.read();
                auto child = parsePrimary(module, visitor);
                return module->addInitial(ASTKind::Not, origin(token, child), child);
            }
            case OperatorBitAnd: {
                visitor.read();
                auto child = parsePrimary(module, visitor);
                return module->addInitial(ASTKind::AddressOf, origin(token, child), child);
            }
            case KeywordTrue: {
                visitor.read();
                return module->add(ASTKind::Bool, Constant::BoolConst(true)).withOrigin(token);
            }
            case KeywordFalse: {
                visitor.read();
                return module->add(ASTKind::Bool, Constant::BoolConst(false)).withOrigin(token);
            }
            case KeywordNew: {
                visitor.read();
                auto child = parseExpression(module, visitor);
                return module->addInitial(ASTKind::New, origin(token, child), child);
            }
            case KeywordOwn: {
                visitor.read();
                IndexedAST child;
                if (visitor.peek().token == KeywordUninit || visitor.peek().token == KeywordOwn)
                    child = parsePrimaryNoSuffix(module, visitor); // Group modifiers together.
                else
                    child = parseExpression(module, visitor);
                return module->addInitial(ASTKind::OwnType, origin(token, child), child);
            }
            case KeywordUninit: {
                visitor.read();
                IndexedAST child;
                if (visitor.peek().token == KeywordUninit || visitor.peek().token == KeywordOwn)
                    child = parsePrimaryNoSuffix(module, visitor); // Group modifiers together.
                else
                    child = parseExpression(module, visitor);
                return module->addInitial(ASTKind::UninitType, origin(token, child), child);
            }
            case OperatorEllipsis: {
                visitor.read();
                return module->addInitial(ASTKind::Splat, origin(token), module->add(ASTKind::Missing).positionless());
            }
            default:
                error(module, token, "Unexpected token '", TokenFormat(module, token), "'.");
                visitor.read();
                return module->add(ASTKind::Missing).positionless();
        } else {
            const_slice<i8> text = module->str(token.token);
            assert(text.size() > 0);
            switch (text[0]) {
                case 'a' ... 'z':
                case 'A' ... 'Z':
                case '_':
                    visitor.read();
                    return module->add(ASTKind::Ident, Identifier(token.token)).withOrigin(token);
                case '0' ... '9':
                    visitor.read();
                    for (i8 c : text) if (c == '.')
                        return module->add(ASTKind::Float, Constant::FloatConst(tofloat(text))).withOrigin(token);
                    return module->add(ASTKind::Unsigned, Constant::UnsignedConst(touint(text))).withOrigin(token);
                case '.':
                    // We know from above we aren't a reserved symbol like ., .., or ...; so
                    // we must be a decimal number that starts with the point.
                    visitor.read();
                    return module->add(ASTKind::Float, Constant::FloatConst(tofloat(text))).withOrigin(token);
                case '"': {
                    visitor.read();
                    return module->add(ASTKind::String, Constant::StringConst(escapeAndUnquote(module, token.pos, text))).withOrigin(token);
                }
                case '\'': {
                    visitor.read();
                    auto str = module->str(escapeAndUnquote(module, token.pos, text));
                    rune r;
                    utf8_decode(str.data(), str.size(), &r, 1);
                    return module->add(ASTKind::Char, Constant::CharConst(r.get())).withOrigin(token);
                }
                case '`':
                    visitor.read();
                    return module->add(ASTKind::Ident, Identifier(escapeAndUnquote(module, token.pos, text))).withOrigin(token);
                case 128 ... 255: {
                        visitor.read();
                    rune first;
                    utf8_decode(text.data(), text.size(), &first, 1);
                    if (utf8_is_digit(first)) {
                        for (i8 c : text) if (c == '.')
                            return module->add(ASTKind::Float, Constant::FloatConst(tofloat(text))).withOrigin(token);
                        return module->add(ASTKind::Unsigned, Constant::UnsignedConst(toint(text))).withOrigin(token);
                    } else
                        return module->add(ASTKind::Ident, Identifier(token.token)).withOrigin(token);
                    fallthrough;
                }
                default:
                    error(module, token, "Unexpected token '", TokenFormat(module, token), "'.");
                    visitor.read();
                    return module->add(ASTKind::Missing).positionless();
            }
        }
    }

    IndexedAST parseIdentifier(Module* module, TokenVisitor& visitor) {
        auto token = visitor.read();
        const_slice<i8> text = module->str(token.token);
        assert(text.size() > 0);
        switch (text[0]) {
            case 'a' ... 'z':
            case 'A' ... 'Z':
            case '_':
                return module->add(ASTKind::Ident, Identifier(token.token)).withOrigin(token);
            case '`':
                return module->add(ASTKind::Ident, Identifier(escapeAndUnquote(module, token.pos, text))).withOrigin(token);
            case 128 ... 255: {
                rune first;
                utf8_decode(text.data(), text.size(), &first, 1);
                if (!utf8_is_digit(first))
                    return module->add(ASTKind::Ident, Identifier(token.token)).withOrigin(token);
                fallthrough;
            }
            default:
                error(module, token, "Unexpected token '", TokenFormat(module, token), "'.");
                return module->add(ASTKind::Missing).positionless();
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

    IndexedAST parsePrimarySuffix(Module* module, TokenVisitor& visitor, IndexedAST ast) {
        while (!visitor.done()) {
            // Tracks whether we've already seen a non-decimal constant. We
            // detect these here as a special case of integer coefficients,
            // i.e. 0x1234 would otherwise be parsed as 0 * x1234. We don't
            // want to allow this multiple times as in 0x1234x5678 though.

            bool allowNonDecimalConstant = true;
            auto token = visitor.peek();
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
                    auto lparen = visitor.read();
                    ASTKind callKind = ast.kind() == ASTKind::GetField ? ASTKind::CallMethod : ASTKind::Call;
                    vec<IndexedAST> arguments;
                    IndexedAST function = ast;
                    if (ast.kind() == ASTKind::GetField) {
                        arguments.push(ast.indexedChild(0));
                        function = ast.indexedChild(1);
                    }
                    if (visitor.peek().token == PunctuatorRightParen) {
                        visitor.stopIgnoringWhitespace();
                        auto rparen = visitor.read();
                        ast = module->addInitial(callKind, origin(function, rparen), function, arguments);
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
                        auto token = visitor.peek();
                        IndexedAST argument;
                        if (token.token == KeywordType) {
                            // Type parameter.
                            visitor.read();
                            IndexedAST ident = parseIdentifier(module, visitor);

                            if (visitor.peek().token == PunctuatorLeftParen) {
                                error(module, visitor.peek().pos, "Type parameter list not permitted in type parameter declaration.");
                                visitor.readUpToAndIncluding(PunctuatorRightParen);
                            }

                            IndexedAST init = module->add(ASTKind::Missing).positionless();
                            if (visitor.peek().token == PunctuatorColon) {
                                visitor.read();
                                init = parseExpression(module, visitor);
                            }
                            argument = module->addInitial(ASTKind::AliasDecl, origin(token, init.missing() ? ident : init), ident, Missing, init);
                        } else if (token.token == KeywordConst) {
                            // Const parameter.
                            visitor.read();
                            Pos namePos = visitor.peek().pos;
                            IndexedAST name = parseExpression(module, visitor);
                            IndexedAST type = module->add(ASTKind::Missing).positionless();
                            if (visitor.peek().token != PunctuatorRightParen && visitor.peek().token != PunctuatorComma)
                                type = name, namePos = visitor.peek().pos, name = parseIdentifier(module, visitor);
                            if (name.kind() != ASTKind::Ident && name.kind() != ASTKind::Missing)
                                error(module, namePos, "Expected identifier in const parameter declaration, found '", name, "'.");
                            IndexedAST init = module->add(ASTKind::Missing).positionless();
                            if (visitor.peek().token == PunctuatorColon) {
                                visitor.read();
                                init = parseExpression(module, visitor);
                            }
                            argument = module->addInitial(ASTKind::ConstVarDecl, origin(token, init.missing() ? name : init), type, name, init);
                        } else
                            argument = parseExpressionOrJuxtaposition(module, visitor, false, true, true);
                        if (visitor.peek().token == PunctuatorColon) {
                            Pos pos = visitor.read().pos;
                            if (argument.kind() != ASTKind::Ident && argument.kind() != ASTKind::Missing)
                                error(module, pos, "Expected identifier in parameter declaration, found '", argument, "'.");
                            argument = module->addInitial(ASTKind::NamedParameter, origin(), argument, parseExpression(module, visitor));
                        }
                        arguments.push(argument);
                        if (visitor.peek().token == PunctuatorComma)
                            visitor.read();
                    }
                    visitor.stopIgnoringWhitespace();
                    IndexedToken rparen;
                    if (visitor.done())
                        error(module, visitor.last(), "Unexpected end of file in call expression.");
                    else if (visitor.peek().token != PunctuatorRightParen) {
                        error(module, visitor.peek(), "Expected closing parenthese ')' in call, found '", TokenFormat(module, visitor.peek()), "'.");
                        visitor.readUpToAndIncluding(PunctuatorRightParen);
                    } else
                        rparen = visitor.read();
                    ast = module->addInitial(callKind, origin(function, rparen), function, arguments);
                    break;
                }
                case PunctuatorLeftBracket: { // Indexing
                    visitor.ignoreWhitespace(token.pos);
                    auto lbrack = visitor.read();
                    if (visitor.peek().token == PunctuatorColon) { // Slice access with no start
                        visitor.read();
                        if (visitor.peek().token == PunctuatorRightBracket) {
                            visitor.stopIgnoringWhitespace();
                            auto rbrack = visitor.read();
                            ast = module->addInitial(ASTKind::GetSlice, origin(ast, rbrack), ast, Missing, Missing);
                            break;
                        }
                        auto end = parseExpression(module, visitor);
                        visitor.stopIgnoringWhitespace();
                        IndexedToken rbrack;
                        if (visitor.peek().token != PunctuatorRightBracket) {
                            error(module, visitor.peek(), "Expected closing bracket ']', found '", TokenFormat(module, visitor.peek()), "'.");
                            visitor.readUpToAndIncluding(PunctuatorRightBracket);
                        } else
                            rbrack = visitor.read();
                        ast = module->addInitial(ASTKind::GetSlice, origin(ast, rbrack), ast, Missing, end);
                        break;
                    }
                    if (visitor.peek().token == PunctuatorRightBracket) {
                        visitor.stopIgnoringWhitespace();
                        auto rbrack = visitor.read();
                        ast = module->addInitial(ASTKind::SliceType, origin(ast, rbrack), ast);
                        break;
                    }
                    IndexedAST index = parseExpression(module, visitor);
                    switch (visitor.peek().token.symbol) {
                        case PunctuatorRightBracket:
                            ast = module->addInitial(ASTKind::GetIndex, origin(ast, visitor.peek()), ast, index);
                            break;
                        case PunctuatorComma: { // Multi-index access
                            vec<IndexedAST> indices;
                            indices.push(index);
                            while (!visitor.done() && visitor.peek().token != PunctuatorRightBracket) {
                                if (visitor.peek().token != PunctuatorComma)
                                    error(module, visitor.peek(), "Expected comma or closing bracket ']', found '", TokenFormat(module, visitor.peek()), "'.");
                                else
                                    visitor.read();
                                indices.push(parseExpression(module, visitor));
                            }
                            ast = module->addInitial(ASTKind::GetIndices, origin(ast, visitor.peek()), ast, indices);
                            break;
                        }
                        case PunctuatorColon: { // Slice access
                            visitor.read();
                            if (visitor.peek().token == PunctuatorRightBracket) { // No upper bound
                                ast = module->addInitial(ASTKind::GetSlice, origin(ast, visitor.peek()), ast, index, Missing);
                                break;
                            }
                            IndexedAST end = parseExpression(module, visitor);
                            ast = module->addInitial(ASTKind::GetSlice, origin(ast, visitor.peek()), ast, index, end);
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
                        vec<IndexedAST> fields;
                        visitor.ignoreWhitespace(visitor.peek().pos);
                        visitor.read();
                        while (!visitor.done() && visitor.peek().token != PunctuatorRightParen) {
                            Pos fieldPos = visitor.peek().pos;
                            IndexedAST field = parsePrimaryNoSuffix(module, visitor);
                            if (field.kind() != ASTKind::Ident && field.kind() != ASTKind::Missing)
                                error(module, fieldPos, "Expected identifier in field access, found '", field, "'.");
                            fields.push(field);
                            if (visitor.peek().token == PunctuatorComma)
                                visitor.read();
                        }
                        visitor.stopIgnoringWhitespace();
                        IndexedToken rparen;
                        if (visitor.done())
                            error(module, visitor.last(), "Unexpected end of file in multi-field access.");
                        else if (visitor.peek().token != PunctuatorRightParen) {
                            error(module, visitor.peek(), "Expected closing parenthese ')', found '", TokenFormat(module, visitor.peek()), "'.");
                            visitor.readUpToAndIncluding(PunctuatorRightParen);
                        } else
                            rparen = visitor.read();
                        ast = module->addInitial(ASTKind::GetFields, origin(ast, rparen), ast, fields);
                        break;
                    }
                    Pos fieldPos = visitor.peek().pos;
                    IndexedAST v = parsePrimaryNoSuffix(module, visitor);
                    if (v.kind() != ASTKind::Ident && v.kind() != ASTKind::Missing
                        && v.kind() != ASTKind::OwnType && v.kind() != ASTKind::UninitType)
                        error(module, fieldPos, "Expected identifier in field access, found '", v, "'.");
                    ast = module->addInitial(ASTKind::GetField, origin(ast, v), ast, v);
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
                                    ast = module->add(ASTKind::Unsigned, Constant::UnsignedConst(hextouint(multiplicand.drop(1)))).withOrigin(ast, token);
                                    break;
                                case 'b': // Binary literal.
                                    ast = module->add(ASTKind::Unsigned, Constant::UnsignedConst(binarytouint(multiplicand.drop(1)))).withOrigin(ast, token);
                                    break;
                                case 'o': // Octal literal.
                                    ast = module->add(ASTKind::Unsigned, Constant::UnsignedConst(octaltouint(multiplicand.drop(1)))).withOrigin(ast, token);
                                    break;
                                default:
                                    error(module, token.pos, "Invalid base for non-decimal integer constant '", multiplicand[0], "'.");
                            }
                        } else {
                            auto child = parseIdentifier(module, visitor);
                            ast = module->addInitial(ASTKind::Mul, origin(ast, child), ast, child);
                        }
                        break;
                    }
                    // Not a suffix, break the loop and return.
                    return ast;
            }
        }
        return ast;
    }

    IndexedAST parsePrimary(Module* module, TokenVisitor& visitor) {
        IndexedAST ast = parsePrimaryNoSuffix(module, visitor);
        return parsePrimarySuffix(module, visitor, ast);
    }

    IndexedAST parsePostfix(Module* module, TokenVisitor& visitor) {
        IndexedAST ast = parsePrimary(module, visitor);
        while (!visitor.done()) {
            auto token = visitor.peek();
            switch (token.token.symbol) {
                case OperatorEllipsis:
                    visitor.read();
                    ast = module->addInitial(ASTKind::Splat, origin(ast, token), ast);
                    break;
                case OperatorIncr:
                    visitor.read();
                    ast = module->addInitial(ASTKind::PostIncr, origin(ast, token), ast);
                    break;
                case OperatorDecr:
                    visitor.read();
                    ast = module->addInitial(ASTKind::PostDecr, origin(ast, token), ast);
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

    IndexedAST parsePattern(Module* module, TokenVisitor& visitor) {
        // Patterns are just expressions...for now. This is kind of gross, it
        // requires that the pattern grammar is a subset of the expression
        // grammar, which means the expression grammar needs to support some
        // weird things like variable declarations in call expressions. We
        // validate and resolve this sort of thing in a later phase once we
        // resolve types.
        return parseExpressionOrJuxtaposition(module, visitor, false, false, false);
    }

    IndexedAST chainRelationalOperators(Module* module, Pos pos, const IndexedAST& lhs, ASTKind op, const IndexedAST& rhs) {
        IndexedAST left = lhs;
        while (left.kind() == ASTKind::And)
            left = left.indexedChild(1);

        if ((u32)left.kind() < (u32)ASTKind::Less || (u32)left.kind() > (u32)ASTKind::NotEqual)
            return module->addInitial(op, origin(lhs, rhs), lhs, rhs);

        left = module->clone(left.child(1)).reconstituteOrigin(left.childOrigin(1).first);
        IndexedAST compare = module->addInitial(op, origin(lhs, rhs), left, rhs);
        return module->addInitial(ASTKind::And, origin(lhs, rhs), lhs, compare);
    }

    IndexedAST makeBinary(Module* module, const IndexedAST& lhs, const IndexedToken& op, const IndexedAST& rhs, const IndexedAST& interior, const vec<IndexedAST>& extraChildren) {
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
                return module->addInitial(ASTKind::Construct, origin(lhs, rhs), rhs, lhs);
            case KeywordIs: kind = ASTKind::Is; break;
            case KeywordIsNot:
                return module->addInitial(ASTKind::Not, origin(lhs, rhs), module->addInitial(ASTKind::Is, origin(lhs, rhs), lhs, rhs));
            case KeywordIn: kind = ASTKind::In; break;
            case KeywordAnd: kind = ASTKind::And; break;
            case KeywordOr: kind = ASTKind::Or; break;
            case OperatorMul:
            case OperatorExp:
                // To resolve type-based ambiguities, we parse both of these to
                // a common "Stars" node and encode each * or ** as a 1 or 2.
                return module->addInitial(ASTKind::Stars, origin(lhs, rhs), lhs, rhs, extraChildren);
            case KeywordIf:
                // We should only be in this case if we had an inline else block.
                return module->addInitial(ASTKind::Ternary, origin(lhs, rhs), interior, lhs, rhs);
            default:
                unreachable("Not a binary operator");
        }
        return module->addInitial(kind, origin(lhs, rhs), lhs, rhs);
    }

    bool processPossiblePointerType(Module* module, IndexedAST& lhs, IndexedToken op, TokenVisitor& visitor, vec<IndexedAST>& extraChildren) {
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
            extraChildren.push(module->add(ASTKind::Unsigned, Constant::UnsignedConst(op.token == OperatorMul ? 1 : 2)).withOrigin(op));

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
            IndexedToken next = visitor.peek();
            if (visitor.done()
                || (next.token.symbol <= LastOperator && (followSetForPointerType & (1ull << next.token.symbol)))) {
                for (const IndexedAST& ast : extraChildren) for (u32 i = 0; i < ast.uintConst(); i ++)
                    lhs = module->addInitial(ASTKind::PtrType, origin(lhs, ast), lhs);
                extraChildren.clear();
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

    pair<IndexedToken, bool> getBinaryOperator(Module* module, TokenVisitor& visitor, bool alreadyMultiline) {
        if (visitor.peek().token == WhitespaceNewline) {
            if (alreadyMultiline || visitor.peek2().token == WhitespaceIndent) {
                IndexedToken possibleOp = visitor.peekIgnoringWhitespace();
                if (isBinaryOperator(possibleOp.token) || possibleOp.token == KeywordElse) {
                    visitor.readIgnoringWhitespace(); // Consume newline and any interstitial indentation.
                    if (possibleOp.token == KeywordIs && visitor.peek2().token == KeywordNot) {
                        visitor.read();
                        return { { KeywordIsNot, possibleOp.pos, possibleOp.index }, false };
                    }
                    return { possibleOp, true };
                }
            }
        }
        if (visitor.peek().token == KeywordIs && visitor.peek2().token == KeywordNot) {
            auto is = visitor.read();
            return { { KeywordIsNot, is.pos, is.index }, false };
        }
        return { visitor.peek(), false };
    }

    IndexedAST parseBinary(Module* module, IndexedAST lhs, const IndexedToken& op, TokenVisitor& visitor, bool& didBail, u32& ifOffset, const vec<IndexedAST>& extraChildren, bool isMultiline, u32 minPrecedence = 0) {
        u32 operatorOffset = visitor.offset - 1;
        IndexedAST rhs = parsePostfix(module, visitor);

        if (rhs.kind() == ASTKind::List && (op.token == OperatorMul || op.token == OperatorExp)) {
            // It's invalid to actually multiply a list with anything, so this
            // must be a pointer type of some kind. Treat these as a cohesive
            // group as if constructed using an array-type or slice-type
            // construction.
            lhs = makeBinary(module, lhs, op, rhs, IndexedAST(), extraChildren);
            lhs = parsePrimarySuffix(module, visitor, lhs);
            auto possibleOp = getBinaryOperator(module, visitor, isMultiline);
            IndexedToken op = possibleOp.first;
            if (isBinaryOperator(op.token)) {
                visitor.read();
                return parseBinary(module, lhs, op, visitor, didBail, ifOffset, extraChildren, isMultiline || possibleOp.second);
            } else
                return lhs;
        }

        auto possibleNextOp = getBinaryOperator(module, visitor, isMultiline);
        IndexedToken nextOp = possibleNextOp.first;
        isMultiline = possibleNextOp.second || isMultiline;
        IndexedAST interior;
        vec<IndexedAST> nextExtraChildren;

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

    IndexedAST parseBlock(Module* module, TokenVisitor& visitor) {
        vec<IndexedAST> items;
        if (visitor.done())
            return module->addInitial(ASTKind::Do, origin(visitor.last()), items);

        assert(visitor.peek().token == WhitespaceNewline);
        auto newline = visitor.read();
        Pos pos = newline.pos;
        if (visitor.peek().token != WhitespaceIndent) {
            error(module, pos, "Expected indented block.");
            return module->addInitial(ASTKind::Do, origin(), items);
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

        // Kind of an inaccurate origin for blocks that very likely span
        // multiple lines, but since error reporting is single-line anyway,
        // there's no point worrying about the origins of the other statements.
        return module->addInitial(ASTKind::Do, origin(newline), items);
    }

    IndexedAST parseExpression(Module* module, TokenVisitor& visitor, u32 minPrecedence) {
        IndexedAST ast = parsePostfix(module, visitor);
        auto possibleOp = getBinaryOperator(module, visitor, false);
        IndexedToken op = possibleOp.first;
        if (isBinaryOperator(op.token) && precedenceOf(op.token) >= minPrecedence) {
            visitor.read();
            vec<IndexedAST> extraChildren;
            if (processPossiblePointerType(module, ast, op, visitor, extraChildren))
                return ast;

            bool didBail = false;
            u32 ifOffset = 0;
            u32 savedDedents = visitor.dedentsToIgnore, savedIndents = visitor.impliedIndents;
            ast = parseBinary(module, ast, op, visitor, didBail, ifOffset, extraChildren, possibleOp.second, minPrecedence);

            // TODO: Maybe don't backtrack if we ran into a parse error exploring past the `if`.
            if UNLIKELY(didBail) {
                visitor.offset = ifOffset;
                visitor.dedentsToIgnore = savedDedents;
                visitor.impliedIndents = savedIndents;
            }
        }
        return ast;
    }

    IndexedAST parseParameterTuple(Module* module, TokenVisitor& visitor, bool typeLevel) {
        assert(visitor.peek().token == PunctuatorLeftParen);
        visitor.ignoreWhitespace(visitor.peek().pos);
        auto paren = visitor.read();

        vec<IndexedAST> parameters;
        while (!visitor.done() && visitor.peek().token != PunctuatorRightParen) {
            if (visitor.peek().token == KeywordConst) {
                auto token = visitor.read();
                Pos namePos = visitor.peek().pos;
                IndexedAST name = parseIdentifier(module, visitor);
                IndexedAST value = module->add(ASTKind::Missing).positionless();
                if (visitor.peek().token == PunctuatorColon) {
                    visitor.read();
                    value = parseExpression(module, visitor);
                }
                parameters.push(module->addInitial(ASTKind::ConstVarDecl, origin(token, value.missing() ? name : value), name, value));
            } else if (visitor.peek().token == KeywordType) {
                auto token = visitor.read();
                IndexedAST name = parseIdentifier(module, visitor);

                if (visitor.peek().token == PunctuatorLeftParen) {
                    error(module, visitor.peek().pos, "Type parameter list not permitted in type parameter declaration.");
                    visitor.readUpToAndIncluding(PunctuatorRightParen);
                }

                IndexedAST value = module->add(ASTKind::Missing).positionless();
                if (visitor.peek().token == PunctuatorColon) {
                    visitor.read();
                    value = parseExpression(module, visitor);
                }
                parameters.push(module->addInitial(ASTKind::AliasDecl, origin(token, value.missing() ? name : value), name, Missing, value));
            } else {
                auto token = visitor.peek();
                IndexedAST expr = parseExpression(module, visitor);

                if (typeLevel) {
                    error(module, token.pos, "Expected constant or type parameter in type-level parameter list, found '", TokenFormat(module, token), "'.");
                    visitor.readUpTo(PunctuatorRightParen);
                    break;
                }

                if (expr.kind() == ASTKind::Stars) {
                    // For this to be valid, it must be a declaration of pointer type.

                    token = visitor.peek();
                    IndexedAST lhs = expr.indexedChild(0);
                    for (u32 i = 2; i < expr.arity(); i ++) for (u32 j = 0; j < expr.child(i).uintConst(); j ++)
                        lhs = module->addInitial(ASTKind::PtrType, origin(lhs, expr.indexedChild(i)), lhs);

                    IndexedAST name = expr.indexedChild(1);
                    if (name.kind() != ASTKind::Ident)
                        error(module, (AST)expr, "Expected identifier in parameter declaration, found '", name, "'.");

                    IndexedAST init = module->add(ASTKind::Missing).positionless();
                    if (token.token == PunctuatorColon) {
                        visitor.read();
                        init = parseExpression(module, visitor);
                    }
                    parameters.push(module->addInitial(ASTKind::VarDecl, origin(lhs, init.missing() ? name : init), lhs, name, init));
                } else if (visitor.peek().token != PunctuatorRightParen && visitor.peek().token != PunctuatorComma && visitor.peek().token != PunctuatorColon) {
                    // Try to parse a juxtaposition.
                    Pos pos = visitor.peek().pos;
                    IndexedAST name = parsePrimaryNoSuffix(module, visitor);
                    if (name.kind() != ASTKind::Ident)
                        error(module, pos, "Expected identifier in parameter declaration, found '", name, "'.");
                    IndexedAST value = module->add(ASTKind::Missing).positionless();
                    if (visitor.peek().token == PunctuatorColon) {
                        visitor.read();
                        value = parseExpression(module, visitor);
                    }
                    parameters.push(module->addInitial(ASTKind::VarDecl, origin(name, value.missing() ? name : value), expr, name, value));
                } else if (expr.kind() == ASTKind::Ident) {
                    IndexedAST value = module->add(ASTKind::Missing).positionless();
                    Pos pos = visitor.peek().pos;
                    if (visitor.peek().token == PunctuatorColon) {
                        visitor.read();
                        value = parseExpression(module, visitor);
                    }
                    parameters.push(module->addInitial(ASTKind::VarDecl, origin(expr, value.missing() ? expr : value), Missing, expr, value));
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
        IndexedToken rparen;
        visitor.stopIgnoringWhitespace();
        if UNLIKELY(visitor.done())
            error(module, visitor.last(), "Unexpected end of file parsing parameter list.");
        else if UNLIKELY(visitor.peek().token != PunctuatorRightParen) {
            error(module, visitor.peek(), "Expected comma or closing parenthese ')' in parameter list, found '", TokenFormat(module, visitor.peek()), "'.");
            visitor.readUpToAndIncluding(PunctuatorRightParen);
        } else
            rparen = visitor.read();

        return module->addInitial(ASTKind::Tuple, origin(paren, rparen), parameters);
    }

    IndexedAST parseRaisesDeclaration(Module* module, TokenVisitor& visitor) {
        assert(visitor.peek().token == KeywordRaises);
        auto raises = visitor.read();
        vec<IndexedAST> raisedTypes;
        raisedTypes.push(parseExpression(module, visitor)); // Types are part of the expression grammar.
        while (visitor.peek().token == PunctuatorComma) {
            visitor.read();
            raisedTypes.push(parseExpression(module, visitor));
        }
        return module->addInitial(ASTKind::Tuple, origin(raises, raisedTypes.back()), raisedTypes);
    }

    IndexedAST parseBlockOrChain(Module* module, TokenVisitor& visitor, const char* statementKind, bool allowMissing) {
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
                return module->add(ASTKind::Missing).positionless();
            error(module, token, "Expected colon ':' in ", statementKind, ", found '", TokenFormat(module, token), "'.");
            visitor.readUpToAndIncluding(WhitespaceNewline);
            return module->add(ASTKind::Missing).positionless();
        }

        visitor.read();
        if (visitor.done() || visitor.peek().token == WhitespaceNewline)
            return parseBlock(module, visitor);
        else
            return parseStatement(module, visitor);
    }

    IndexedAST parseExpressionOrJuxtaposition(Module* module, TokenVisitor& visitor, bool allowMultiple, bool allowFunction, bool allowInitializer) {
        IndexedAST ast = parseExpression(module, visitor);
        if (visitor.done())
            return ast;

        Token token = visitor.peek();

        if (token.token == PunctuatorColon || token.token == KeywordRaises || token.token == PunctuatorComma) {
            IndexedAST possibleDecl = ast;
            while (possibleDecl.kind() == ASTKind::OwnType || possibleDecl.kind() == ASTKind::UninitType)
                possibleDecl = ast.indexedChild(0);
            if (possibleDecl.kind() == ASTKind::Stars) {
                // In order for this to be legal, it must be a declaration with
                // a pointer type. So we do a bit of fixup. If the rhs of the
                // stars is a call, then it must be a function. If we're
                // followed by the 'raises' keyword, it's an error for it to not be
                // a function. Otherwise, it's a vardecl, and we assert that the
                // rhs is an identifier here.

                IndexedAST rhs = possibleDecl.indexedChild(1);
                if (rhs.kind() == ASTKind::Call) {
                    IndexedToken lparen, rparen;
                    if (rhs.first != IndexedToken::InvalidIndex && rhs.last != IndexedToken::InvalidIndex) {
                        lparen = { visitor.tokens[rhs.first], rhs.first };
                        // TODO: This probably computes the wrong position if the
                        // callee contains parentheses, like (foo.bar)(baz). But I
                        // don't really have the patience to write a parenthese
                        // matcher inline here right now and it's super minor.
                        while (lparen.token != PunctuatorLeftParen)
                            lparen = { visitor.tokens[lparen.index + 1], lparen.index + 1 };
                        rparen = { visitor.tokens[rhs.last], rhs.last };
                    }
                    // It's a FunDecl.
                    vec<IndexedAST> arguments;
                    for (u32 i = 1; i < rhs.arity(); i ++) {
                        IndexedAST child = rhs.indexedChild(i);
                        if (child.kind() == ASTKind::Ident) {
                            auto name = module->add(ASTKind::Ident, Identifier(child.symbol())).withOrigin(child);
                            arguments.push(module->addInitial(ASTKind::VarDecl, origin(name), Missing, name, Missing));
                        } else
                            arguments.push(child);
                    }
                    if (rhs.child(0).kind() != ASTKind::Ident)
                        error(module, (AST)rhs, "Expected identifier in function declaration, found '", rhs.child(0), "'.");
                    IndexedAST lhs = possibleDecl.indexedChild(0);
                    for (u32 i = 2; i < possibleDecl.arity(); i ++) for (u32 j = 0; j < possibleDecl.child(i).uintConst(); j ++)
                        lhs = module->addInitial(ASTKind::PtrType, origin(lhs, possibleDecl.indexedChild(i)), lhs);
                    IndexedAST modifiers = ast;
                    while (modifiers.kind() != ASTKind::Stars) {
                        if (modifiers.kind() == ASTKind::OwnType)
                            lhs = module->addInitial(ASTKind::OwnType, origin(modifiers, lhs), lhs);
                        else
                            lhs = module->addInitial(ASTKind::UninitType, origin(modifiers, lhs), lhs);
                        modifiers = modifiers.indexedChild(0);
                    }
                    IndexedAST raises = module->add(ASTKind::Missing).positionless();
                    if (visitor.peek().token == KeywordRaises)
                        raises = parseRaisesDeclaration(module, visitor);
                    IndexedAST body = parseBlockOrChain(module, visitor, "function declaration", true);
                    return module->addInitial(ASTKind::FunDecl, origin(lhs, body), lhs, rhs.indexedChild(0), module->addInitial(ASTKind::Tuple, origin(lparen, rparen), arguments), raises, body);
                } else {
                    assert(token.token == PunctuatorColon || token.token == PunctuatorComma);
                    IndexedAST lhs = possibleDecl.indexedChild(0);
                    for (u32 i = 2; i < possibleDecl.arity(); i ++) for (u32 j = 0; j < possibleDecl.child(i).uintConst(); j ++)
                        lhs = module->addInitial(ASTKind::PtrType, origin(lhs, possibleDecl.indexedChild(i)), lhs);
                    IndexedAST modifiers = ast;
                    while (modifiers.kind() != ASTKind::Stars) {
                        if (modifiers.kind() == ASTKind::OwnType)
                            lhs = module->addInitial(ASTKind::OwnType, origin(modifiers, lhs), lhs);
                        else
                            lhs = module->addInitial(ASTKind::UninitType, origin(modifiers, lhs), lhs);
                        modifiers = modifiers.indexedChild(0);
                    }

                    IndexedAST name = possibleDecl.indexedChild(1);
                    if (name.kind() != ASTKind::Ident)
                        error(module, (AST)possibleDecl, "Expected identifier in variable declaration, found '", name, "'.");

                    IndexedAST init = module->add(ASTKind::Missing).positionless();
                    if (token.token == PunctuatorColon) {
                        visitor.read();
                        if (visitor.peek().token == KeywordUninit
                            && (visitor.peek2().token == MetaNone || visitor.peek2().token == PunctuatorComma || visitor.peek2().token == WhitespaceNewline))
                            init = module->add(ASTKind::Uninit).withOrigin(visitor.read());
                        else
                            init = parseExpression(module, visitor);
                    }
                    if (visitor.peek().token != PunctuatorComma || !allowMultiple)
                        return module->addInitial(ASTKind::VarDecl, origin(lhs, init.missing() ? name : init), lhs, name, init);
                    vec<IndexedAST> decls;
                    decls.push(module->addInitial(ASTKind::VarDecl, origin(lhs, init.missing() ? name : init), lhs, name, init));
                    while (visitor.peek().token == PunctuatorComma) {
                        visitor.readIgnoringWhitespace();
                        Pos pos = visitor.peek().pos;
                        name = parseIdentifier(module, visitor);
                        if (visitor.peek().token == PunctuatorColon) {
                            visitor.read();
                            if (visitor.peek().token == KeywordUninit
                                && (visitor.peek2().token == MetaNone || visitor.peek2().token == PunctuatorComma || visitor.peek2().token == WhitespaceNewline))
                                init = module->add(ASTKind::Uninit).withOrigin(visitor.read());
                            else
                                init = parseExpression(module, visitor);
                        } else
                            init = module->add(ASTKind::Missing).positionless();
                        decls.push(module->addInitial(ASTKind::VarDecl, origin(lhs, init.missing() ? name : init), lhs, name, init));
                    }
                    return decls.size() == 1 ? decls[0] : module->addInitial(ASTKind::Do, origin(decls.front(), decls.back()), decls);
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
        if (token.token == WhitespaceNewline || token.token == WhitespaceDedent || token.token == PunctuatorRightParen || token.token == PunctuatorColon || token.token == PunctuatorComma)
            return ast;

        // Otherwise, we should consider this a juxtaposition (variable or
        // function decl really) and try to parse another expression. Really
        // it's invalid for this to be anything other than an identifier or
        // string.
        Pos namePos = visitor.peek().pos;
        IndexedAST name = parseIdentifier(module, visitor);
        if (visitor.peek().token == PunctuatorLeftParen && allowFunction) {
            // Function definition with explicit return type.
            IndexedAST parameters = parseParameterTuple(module, visitor, false);
            IndexedAST raises;
            if (visitor.peek().token == KeywordRaises)
                raises = parseRaisesDeclaration(module, visitor);
            else
                raises = module->add(ASTKind::Missing).positionless();
            IndexedAST body = parseBlockOrChain(module, visitor, "function declaration", true);
            return module->addInitial(ASTKind::FunDecl, origin(ast, body), ast, name, parameters, raises, body);
        }

        vec<IndexedAST> decls;
        IndexedAST init;
        if (visitor.peek().token == PunctuatorColon && allowInitializer) {
            visitor.read();
            if (visitor.peek().token == KeywordUninit
                && (visitor.peek2().token == MetaNone || visitor.peek2().token == PunctuatorComma || visitor.peek2().token == WhitespaceNewline))
                init = module->add(ASTKind::Uninit).withOrigin(visitor.read());
            else
                init = parseExpression(module, visitor);
        } else
            init = module->add(ASTKind::Missing).positionless();
        decls.push(module->addInitial(ASTKind::VarDecl, origin(ast, init.missing() ? name : init), ast, name, init));
        if (allowMultiple) while (visitor.peek().token == PunctuatorComma) {
            visitor.readIgnoringWhitespace();
            name = parseIdentifier(module, visitor);
            if (visitor.peek().token == PunctuatorColon && allowInitializer) {
                visitor.read();
                if (visitor.peek().token == KeywordUninit
                    && (visitor.peek2().token == MetaNone || visitor.peek2().token == PunctuatorComma || visitor.peek2().token == WhitespaceNewline))
                    init = module->add(ASTKind::Uninit).withOrigin(visitor.read());
                else
                    init = parseExpression(module, visitor);
            } else
                init = module->add(ASTKind::Missing).positionless();
            decls.push(module->addInitial(ASTKind::VarDecl, origin(ast, init.missing() ? name : init), ast, name, init));
        }

        return decls.size() == 1 ? decls[0] : module->addInitial(ASTKind::Do, origin(decls.front(), decls.back()), decls);
    }

    void exploreBody(vec<IndexedAST>& contents, IndexedAST body) {
        if (body.kind() == ASTKind::Do)
            for (u32 i = 0; i < body.arity(); i ++)
                exploreBody(contents, body.indexedChild(i));
        else
            contents.push(body);
    }

    IndexedAST parsePrimaryStatement(Module* module, TokenVisitor& visitor);

    IndexedAST parseTypeDecl(Module* module, TokenVisitor& visitor, u32 expectedKeyword) {
        assert(visitor.peek().token == expectedKeyword);
        auto token = visitor.read();
        IndexedAST name = parseIdentifier(module, visitor);
        bool isCase = expectedKeyword == KeywordCase;

        IndexedAST parameters;
        if (visitor.peek().token == PunctuatorLeftParen) { // Generic type.
            parameters = parseParameterTuple(module, visitor, true);
        } else
            parameters = module->add(ASTKind::Missing).positionless();

        if (!parameters.missing() && isCase)
            error(module, token.pos, "Type parameter list not permitted on case type declaration.");

        if (visitor.peek().token != PunctuatorColon) // Stub type, or maybe unit.
            return module->addInitial(isCase ? ASTKind::NamedCaseDecl : ASTKind::NamedDecl, origin(token, parameters.missing() ? name : parameters), name, parameters, Missing);

        auto colon = visitor.read();
        IndexedAST body;
        if (visitor.done() || visitor.peek().token == WhitespaceNewline)
            body = parseBlock(module, visitor);
        else
            body = parsePrimaryStatement(module, visitor);

        vec<IndexedAST> contents;
        exploreBody(contents, body);
        for (AST ast : contents) {
            if (ast.kind() == ASTKind::FunDecl)
                error(module, ast, "Function declarations are not permitted in type definitions.");
            else if (ast.kind() == ASTKind::Namespace)
                error(module, ast, "Namespace declarations are not permitted in type definitions.");
        }
        if (contents.size() == 0) {
            error(module, colon, "Expected at least one member declaration in type definition.");
            contents.push(module->add(ASTKind::Missing).positionless());
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
                return module->addInitial(isCase ? ASTKind::NamedCaseDecl : ASTKind::NamedDecl, origin(token, contents[0]), name, parameters, contents[0]);
        }
        bool sawField = false, sawCase = false;
        // TODO: Maybe this should be a tree-traversal? Not sure.
        for (const IndexedAST& ast : contents) switch (ast.kind()) {
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

        return module->addInitial(kind, origin(token, contents.back()), name, parameters, contents);
    }

    IndexedAST parseImportGroup(Module* module, TokenVisitor& visitor, const IndexedToken& token) {
        IndexedAST main = parseIdentifier(module, visitor);
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
                IndexedAST subpath = module->add(ASTKind::Wildcard).withOrigin(visitor.read());
                main = module->addInitial(ASTKind::GetField, origin(main, subpath), main, subpath);
            } else {
                IndexedAST subpath = parseIdentifier(module, visitor);
                main = module->addInitial(ASTKind::GetField, origin(main, subpath), main, subpath);
            }
        }

        if (visitor.peek().token == KeywordAs) {
            auto token = visitor.read();
            if (visitor.done())
                error(module, token, "Unexpected end of file in import alias specifier.");
            else if (visitor.peek().token == WhitespaceNewline)
                error(module, token, "Unexpected line break in import alias specifier.");
            else {
                IndexedAST alias = parseIdentifier(module, visitor);
                main = module->addInitial(ASTKind::As, origin(main, alias), main, alias);
            }
        }

        return module->addInitial(isLocal ? ASTKind::UseLocal : ASTKind::UseModule, origin(token, main), main);
    }

    IndexedAST parseUse(Module* module, TokenVisitor& visitor) {
        auto use = visitor.read();
        vec<IndexedAST> groups;
        groups.push(parseImportGroup(module, visitor, use));
        while (visitor.peek().token == PunctuatorComma) {
            visitor.readIgnoringWhitespace();
            groups.push(parseImportGroup(module, visitor, use));
        }
        return groups.size() == 1 ? groups[0] : module->addInitial(ASTKind::Do, origin(use, groups.back()), groups);
    }

    IndexedAST parsePrimaryStatement(Module* module, TokenVisitor& visitor) {
        auto token = visitor.peek();
        switch (token.token.symbol) {
            case KeywordBreak:
                return module->addInitial(ASTKind::Break, origin(visitor.read()));
            case KeywordContinue:
                return module->addInitial(ASTKind::Continue, origin(visitor.read()));
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
                    return module->addInitial(ASTKind::Return, origin(token), Missing);
                auto expr = parseExpression(module, visitor);
                return module->addInitial(ASTKind::Return, origin(token, expr), expr);
            }
            case KeywordRaise: {
                visitor.read();
                auto expr = parseExpression(module, visitor);
                return module->addInitial(ASTKind::Raise, origin(token, expr), expr);
            }
            case KeywordAlias: {
                visitor.read();
                vec<IndexedAST> decls;
                IndexedAST name = parseIdentifier(module, visitor);

                IndexedAST parameters;
                if (visitor.peek().token == PunctuatorLeftParen) { // Generic type alias.
                    parameters = parseParameterTuple(module, visitor, true);
                } else
                    parameters = module->add(ASTKind::Missing).positionless();

                if (visitor.peek().token != PunctuatorColon) {
                    error(module, visitor.peek(), "Expected colon ':' in alias declaration, found '", TokenFormat(module, visitor.peek()));
                    visitor.readUpTo(WhitespaceNewline);
                    return module->add(ASTKind::Missing).positionless();
                } else
                    visitor.read();
                IndexedAST init = parseExpression(module, visitor);

                decls.push(module->addInitial(ASTKind::AliasDecl, origin(name, init), name, parameters, init));
                while (visitor.peek().token == PunctuatorComma) {
                    visitor.readIgnoringWhitespace();
                    name = parseIdentifier(module, visitor);

                    if (visitor.peek().token == PunctuatorLeftParen) { // Generic type alias.
                        parameters = parseParameterTuple(module, visitor, true);
                    } else
                        parameters = module->add(ASTKind::Missing).positionless();

                    if (visitor.peek().token != PunctuatorColon) {
                        error(module, visitor.peek(), "Expected colon ':' in alias declaration, found '", TokenFormat(module, visitor.peek()));
                        visitor.readUpTo(WhitespaceNewline);
                        return module->add(ASTKind::Missing).positionless();
                    } else
                        visitor.read();
                    init = parseExpression(module, visitor);
                    decls.push(module->addInitial(ASTKind::AliasDecl, origin(name, init), name, parameters, init));
                }
                assert(decls.size() > 0);
                return decls.size() == 1 ? decls[0] : module->addInitial(ASTKind::Do, origin(token, decls.back()), decls);
            }
            case KeywordVar:
            case KeywordConst: {
                visitor.read();
                ASTKind kind = token.token == KeywordVar ? ASTKind::VarDecl : ASTKind::ConstVarDecl;
                IndexedAST type = module->add(ASTKind::Missing).positionless();
                Pos namePos = visitor.peek().pos;

                // Since we only reserve one `const` keyword for variables and
                // functions, we can't disambiguate at parse time whether i.e.
                //
                //   const a(b): c
                //
                // is a destructuring assignment or a function definition. We
                // choose to always make it a function definition and preclude
                // destructuring assignment for constant declarations.
                IndexedAST pattern = kind == ASTKind::ConstVarDecl ? parseIdentifier(module, visitor) : parsePattern(module, visitor);

                if (kind == ASTKind::ConstVarDecl && visitor.peek().token == PunctuatorLeftParen) {
                    // Constant function.
                    kind = ASTKind::ConstFunDecl;
                    auto lparen = visitor.read();
                    visitor.ignoreWhitespace(lparen.pos);

                    if (pattern.kind() != ASTKind::Ident)
                        error(module, namePos, "Expected identifier in const function definition.");

                    vec<IndexedAST> params;
                    while (!visitor.done() && visitor.peek().token != PunctuatorRightParen) {
                        IndexedAST id = parseIdentifier(module, visitor);
                        IndexedAST value = module->add(ASTKind::Missing).positionless();
                        if (visitor.peek().token == PunctuatorColon) {
                            visitor.read();
                            value = parseExpression(module, visitor);
                        }
                        params.push(module->addInitial(ASTKind::ConstVarDecl, origin(token, value.missing() ? id : value), id, value));
                        if (visitor.peek().token == PunctuatorComma)
                            visitor.read();
                    }

                    visitor.stopIgnoringWhitespace();
                    IndexedToken rparen;
                    if UNLIKELY(visitor.done())
                        error(module, visitor.last(), "Unexpected end of file parsing parameter list.");
                    else if UNLIKELY(visitor.peek().token != PunctuatorRightParen) {
                        error(module, visitor.peek(), "Expected comma or closing parenthese ')' in parameter list, found '", TokenFormat(module, visitor.peek()), "'.");
                        visitor.readUpToAndIncluding(PunctuatorRightParen);
                    } else
                        rparen = visitor.read();

                    if (visitor.peek().token != PunctuatorColon)
                        error(module, visitor.peek(), "Expected colon ':' in const function definition, found '", TokenFormat(module, visitor.peek()), "'.");
                    else {
                        auto paramTuple = module->addInitial(ASTKind::Tuple, origin(lparen, rparen), params);
                        auto body = parseBlockOrChain(module, visitor, "const function definition", false);
                        return module->addInitial(kind, origin(token, body), pattern, paramTuple, body);
                    }
                    return module->add(ASTKind::Missing).positionless();
                }

                vec<IndexedAST> decls;
                IndexedAST init;
                if (visitor.peek().token == PunctuatorColon) {
                    visitor.read();
                    if (visitor.peek().token == KeywordUninit
                        && (visitor.peek2().token == MetaNone || visitor.peek2().token == PunctuatorComma || visitor.peek2().token == WhitespaceNewline))
                        init = module->add(ASTKind::Uninit).withOrigin(visitor.read());
                    else
                        init = parseExpression(module, visitor);
                } else
                    init = module->add(ASTKind::Missing).positionless();
                if (kind == ASTKind::ConstVarDecl)
                    decls.push(module->addInitial(kind, origin(token, init.missing() ? pattern : init), pattern, init));
                else
                    decls.push(module->addInitial(kind, origin(token, init.missing() ? pattern : init), type, pattern, init));
                while (visitor.peek().token == PunctuatorComma) {
                    visitor.readIgnoringWhitespace();
                    namePos = visitor.peek().pos;
                    pattern = parsePattern(module, visitor);
                    if (visitor.peek().token == PunctuatorColon) {
                        visitor.read();
                        init = parseExpression(module, visitor);
                    } else
                        init = module->add(ASTKind::Missing).positionless();
                    if (kind == ASTKind::ConstVarDecl)
                        decls.push(module->addInitial(kind, origin(token, init.missing() ? pattern : init), pattern, init));
                    else
                        decls.push(module->addInitial(kind, origin(token, init.missing() ? pattern : init), type, pattern, init));
                }
                assert(decls.size() > 0);
                return decls.size() == 1 ? decls[0] : module->addInitial(ASTKind::Do, origin(token, decls.back()), decls);
            }
            case KeywordFun: {
                visitor.read();
                Pos namePos = visitor.peek().pos;
                IndexedAST name = parseIdentifier(module, visitor);
                if (visitor.peek().token != PunctuatorLeftParen) {
                    error(module, visitor.peek(), "Expected opening parenthese '(' in function declaration, found '", TokenFormat(module, visitor.peek()));
                    visitor.readUpTo(WhitespaceNewline);
                    return module->add(ASTKind::Missing).positionless();
                }
                IndexedAST parameters = parseParameterTuple(module, visitor, false);
                IndexedAST raises;
                if (visitor.peek().token == KeywordRaises)
                    raises = parseRaisesDeclaration(module, visitor);
                else
                    raises = module->add(ASTKind::Missing).positionless();
                IndexedAST body;
                if (visitor.done() || visitor.peek().token == WhitespaceNewline)
                    body = module->add(ASTKind::Missing).positionless();
                else
                    body = parseBlockOrChain(module, visitor, "function declaration", false);
                return module->addInitial(ASTKind::FunDecl, origin(token, body.missing() ? (raises.missing() ? parameters : raises) : body), Missing, name, parameters, raises, body);
            }
            case KeywordIn: {
                visitor.read();
                vec<IndexedAST, 4> names;
                names.push(parseIdentifier(module, visitor));
                while (visitor.peek().token == PunctuatorDot) {
                    visitor.read();
                    names.push(parseIdentifier(module, visitor));
                }
                IndexedAST body;
                if (visitor.peek().token != PunctuatorColon)
                    error(module, visitor.peek(), "Expected colon ':' in namespace declaration, found '", TokenFormat(module, visitor.peek()), "'.");
                else
                    body = parseBlockOrChain(module, visitor, "namespace declaration", false);

                while (names.size())
                    body = module->addInitial(ASTKind::Namespace, origin(token, body), names.pop(), body);
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

    IndexedAST transformExpressionToPattern(Module* module, IndexedAST expression) {
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
        u32 firstToken, lastToken;
        Pos pos;
        union {
            struct { ASTWord pattern, items; u32 patternPos, itemsPos; };
            struct { ASTWord low, var, high; u32 lowPos, varPos, highPos; bool lowInclusive, highInclusive; };
        };
        ASTWord breakIf, continueUnless;
        u32 breakIfPos, continueUnlessPos;
        bool hasBreakIf, hasContinueUnless;
    };

    struct ForLoop {
        vec<BindingGroup, 2> bindings;
    };

    BindingGroup parseBindingGroup(Module* module, TokenVisitor& visitor) {
        IndexedAST lhs = parsePrimary(module, visitor);
        BindingGroup binding;
        binding.firstToken = lhs.first;
        auto next = visitor.peek();
        binding.pos = next.pos;
        if (next.token == KeywordIn) {
            IndexedAST pattern = transformExpressionToPattern(module, lhs);
            visitor.read();
            IndexedAST items = parseExpression(module, visitor);
            binding.pattern = pattern.asOperand();
            binding.patternPos = pattern.first;
            binding.items = items.asOperand();
            binding.itemsPos = items.first;
            binding.kind = BindingKind::In;
            binding.lastToken = items.last;
        } else if (next.token == OperatorLess
            || next.token == OperatorGreater
            || next.token == OperatorLessEqual
            || next.token == OperatorGreaterEqual) {
            bool isDescending = next.token == OperatorGreater || next.token == OperatorGreaterEqual;
            bool firstInclusive = next.token == OperatorLessEqual || next.token == OperatorGreaterEqual;
            binding.kind = isDescending ? BindingKind::RangeDecreasing : BindingKind::RangeIncreasing;
            visitor.read();

            IndexedAST middle = parseExpression(module, visitor, precedenceOf(OperatorLess) + 1);
            binding.lastToken = middle.last;

            IndexedAST rhs;
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
                binding.lastToken = rhs.last;
            } else {
                if (isDescending) {
                    secondInclusive = true;
                    rhs = module->add(ASTKind::Unsigned, Constant::UnsignedConst(0)).positionless();
                } else {
                    rhs = middle;
                    middle = lhs;
                    secondInclusive = firstInclusive;
                    firstInclusive = true;
                    lhs = module->add(ASTKind::Unsigned, Constant::UnsignedConst(0)).positionless();
                }
            }

            assert(lhs.kind() != ASTKind::VarDecl);
            assert(middle.kind() == ASTKind::VarDecl || middle.kind() == ASTKind::Ident);
            assert(rhs.kind() != ASTKind::VarDecl);
            binding.var = middle.asOperand();
            binding.varPos = middle.first;
            binding.low = isDescending ? rhs.asOperand() : lhs.asOperand();
            binding.lowPos = isDescending ? rhs.first : lhs.first;
            binding.high = isDescending ? lhs.asOperand() : rhs.asOperand();
            binding.highPos = isDescending ? lhs.first : rhs.first;
            binding.lowInclusive = isDescending ? secondInclusive : firstInclusive;
            binding.highInclusive = isDescending ? firstInclusive : secondInclusive;
        } else {
            error(module, next, "Unexpected token '", TokenFormat(module, next), "' in for loop binding.");
            while (visitor.peek().token != WhitespaceNewline && visitor.peek().token != PunctuatorColon)
                visitor.read();
            binding.kind = BindingKind::Error;
        }

        binding.hasBreakIf = binding.hasContinueUnless = false;

        next = visitor.peek();
        if (next.token == KeywordIf || next.token == KeywordUnless) {
            visitor.read();
            IndexedAST condition = parseExpression(module, visitor);
            condition = next.token == KeywordUnless ? module->addInitial(ASTKind::Not, origin(condition), condition) : condition;
            binding.continueUnless = condition.asOperand();
            binding.continueUnlessPos = condition.first;
            binding.hasContinueUnless = true;
            binding.lastToken = condition.last;
        }

        next = visitor.peek();
        if (next.token == KeywordUntil || next.token == KeywordWhile) {
            visitor.read();
            IndexedAST condition = parseExpression(module, visitor);
            condition = next.token == KeywordWhile ? module->addInitial(ASTKind::Not, origin(condition), condition) : condition;
            binding.breakIf = condition.asOperand();
            binding.breakIfPos = condition.first;
            binding.hasBreakIf = true;
            binding.lastToken = condition.last;
        }

        return binding;
    }

    IndexedAST parseForLoop(Module* module, TokenVisitor& visitor, const IndexedToken& token, IndexedAST preParsedBody, bool isPostfix) {
        vec<BindingGroup> bindings;
        bindings.push(parseBindingGroup(module, visitor));
        while (visitor.peek().token == PunctuatorComma) {
            visitor.readIgnoringWhitespace();
            bindings.push(parseBindingGroup(module, visitor));
        }

        IndexedAST body = preParsedBody;
        if (!isPostfix) {
            if (visitor.peek().token != PunctuatorColon) {
                error(module, visitor.peek(), "Expected colon ':' in for loop statement, found '", TokenFormat(module, visitor.peek()), "'.");
                visitor.readUpToAndIncluding(WhitespaceNewline);
                return module->add(ASTKind::Missing).positionless();
            }
            visitor.read();
            if (visitor.done() || visitor.peek().token == WhitespaceNewline)
                body = parseBlock(module, visitor);
            else
                body = parseStatement(module, visitor);
        }

        vec<IndexedAST> seq;

        // First, we set up all our iterators.

        for (const auto& [i, binding] : enumerate(bindings)) switch (binding.kind) {
            case BindingKind::Error:
                break;
            case BindingKind::In: {
                array<i8, 64> buffer;
                auto name = prints(buffer, "__iter", i);
                seq.push(module->addInitial(ASTKind::VarDecl, origin(binding.firstToken, binding.lastToken), Missing, Identifier(module->sym(name)),
                    module->addInitial(ASTKind::CallMethod, origin(binding.firstToken, binding.lastToken), Identifier(MethodIter), module->fromOperand(binding.items).reconstituteOrigin(binding.itemsPos))));
                binding.items = seq.back().child(1).asOperand();
                break;
            }
            case BindingKind::RangeDecreasing: {
                array<i8, 64> buffer;
                auto name = prints(buffer, "__iter", i);
                IndexedAST init = module->fromOperand(binding.high).reconstituteOrigin(binding.highPos);
                if (!binding.highInclusive)
                    init = module->addInitial(ASTKind::Sub, origin(binding.firstToken, binding.lastToken), init, Constant::UnsignedConst(1));
                seq.push(module->addInitial(ASTKind::VarDecl, origin(binding.firstToken, binding.lastToken), Missing, Identifier(module->sym(name)), init));
                binding.high = seq.back().child(1).asOperand();
                name = prints(buffer, "__end", i);
                seq.push(module->addInitial(ASTKind::VarDecl, origin(binding.firstToken, binding.lastToken), Missing, Identifier(module->sym(name)), module->fromOperand(binding.low).reconstituteOrigin(binding.lowPos)));
                binding.low = seq.back().child(1).asOperand();
                break;
            }
            case BindingKind::RangeIncreasing: {
                array<i8, 64> buffer;
                auto name = prints(buffer, "__iter", i);
                IndexedAST init = module->fromOperand(binding.low).reconstituteOrigin(binding.lowPos);
                if (!binding.lowInclusive)
                    init = module->addInitial(ASTKind::Add, origin(binding.firstToken, binding.lastToken), init, Constant::UnsignedConst(1));
                seq.push(module->addInitial(ASTKind::VarDecl, origin(binding.firstToken, binding.lastToken), Missing, Identifier(module->sym(name)), init));
                binding.low = seq.back().child(1).asOperand();
                name = prints(buffer, "__end", i);
                seq.push(module->addInitial(ASTKind::VarDecl, origin(binding.firstToken, binding.lastToken), Missing, Identifier(module->sym(name)), module->fromOperand(binding.high).reconstituteOrigin(binding.highPos)));
                binding.high = seq.back().child(1).asOperand();
                break;
            }
        }

        // Next, we set up our top-level condition.

        vec<IndexedAST> conditions;
        for (const auto& binding : bindings) {
            switch (binding.kind) {
                case BindingKind::Error:
                    conditions.push(module->add(
                        ASTKind::Bool,
                        Constant::BoolConst(true)).withOrigin(binding.firstToken, binding.lastToken)
                    );
                    break;
                case BindingKind::In:
                    conditions.push(module->addInitial(
                        ASTKind::Not,
                        origin(binding.firstToken, binding.lastToken),
                        module->addInitial(
                            ASTKind::CallMethod,
                            origin(binding.firstToken, binding.lastToken),
                            Identifier(MethodDone),
                            module->fromOperand(binding.items).reconstituteOrigin(binding.itemsPos)
                        )
                    ));
                    break;
                case BindingKind::RangeDecreasing:
                    conditions.push(module->addInitial(
                        binding.lowInclusive ? ASTKind::GreaterEq : ASTKind::Greater,
                        origin(binding.firstToken, binding.lastToken),
                        module->fromOperand(binding.high).reconstituteOrigin(binding.highPos),
                        module->fromOperand(binding.low).reconstituteOrigin(binding.lowPos)
                    ));
                    break;
                case BindingKind::RangeIncreasing:
                    conditions.push(module->addInitial(
                        binding.highInclusive ? ASTKind::LessEq : ASTKind::Less,
                        origin(binding.firstToken, binding.lastToken),
                        module->fromOperand(binding.low).reconstituteOrigin(binding.lowPos),
                        module->fromOperand(binding.high).reconstituteOrigin(binding.highPos)
                    ));
                    break;
            }
        }
        assert(conditions.size() > 0);
        IndexedAST condition = conditions[0];
        for (u32 i = 1; i < conditions.size(); i ++)
            condition = module->addInitial(ASTKind::And, origin(condition, conditions[i]), condition, conditions[i]);

        // Now we're going to build the actual body of the loop. We start by
        // introducing declarations for any named bindings we have. If any of
        // these bindings have an until or while clause, we break if that
        // condition fails immediately after initializing the corresponding
        // binding.

        vec<IndexedAST> bodyStmts;

        for (const auto& binding : bindings) {
            switch (binding.kind) {
                case BindingKind::Error:
                    break;
                case BindingKind::In:
                    bodyStmts.push(module->addInitial(
                        ASTKind::VarDecl,
                        origin(binding.firstToken, binding.lastToken),
                        Missing,
                        module->fromOperand(binding.pattern).reconstituteOrigin(binding.patternPos),
                        module->addInitial(
                            ASTKind::CallMethod,
                            origin(binding.firstToken, binding.lastToken),
                            Identifier(MethodRead),
                            module->fromOperand(binding.items).reconstituteOrigin(binding.itemsPos)
                        )
                    ));
                    break;
                case BindingKind::RangeDecreasing:
                    bodyStmts.push(module->addInitial(
                        ASTKind::VarDecl,
                        origin(binding.firstToken, binding.lastToken),
                        Missing,
                        module->fromOperand(binding.var).reconstituteOrigin(binding.varPos),
                        module->fromOperand(binding.high).reconstituteOrigin(binding.highPos)
                    ));
                    break;
                case BindingKind::RangeIncreasing:
                    bodyStmts.push(module->addInitial(
                        ASTKind::VarDecl,
                        origin(binding.firstToken, binding.lastToken),
                        Missing,
                        module->fromOperand(binding.var).reconstituteOrigin(binding.varPos),
                        module->fromOperand(binding.low).reconstituteOrigin(binding.lowPos)
                    ));
                    break;
            }
            if (binding.hasBreakIf)
                bodyStmts.push(module->addInitial(ASTKind::If, origin(binding.firstToken, binding.lastToken), module->fromOperand(binding.breakIf).reconstituteOrigin(binding.breakIfPos), module->addInitial(ASTKind::Break, origin(binding.firstToken, binding.lastToken))));
        }

        // Now we construct the body. This will be the original lexical body of
        // the loop, plus if statements wrapping it for each if or unless
        // clause in the binding groups, in reverse order.

        for (const auto& binding : reversed(bindings)) if (binding.hasContinueUnless)
            body = module->addInitial(ASTKind::If, origin(binding.firstToken, binding.lastToken), module->fromOperand(binding.continueUnless).reconstituteOrigin(binding.continueUnlessPos), body);

        bodyStmts.push(body);

        // Finally, we add the update step, bumping each binding group's
        // iterator for the next iteration.

        for (const auto& binding : bindings) switch (binding.kind) {
            case BindingKind::Error:
                break;
            case BindingKind::In:
                bodyStmts.push(module->addInitial(
                    ASTKind::Assign,
                    origin(binding.firstToken, binding.lastToken),
                    module->fromOperand(binding.items).reconstituteOrigin(binding.itemsPos),
                    module->addInitial(
                        ASTKind::CallMethod,
                        origin(binding.firstToken, binding.lastToken),
                        Identifier(MethodNext),
                        module->fromOperand(binding.items).reconstituteOrigin(binding.itemsPos)
                    )
                ));
                break;
            case BindingKind::RangeDecreasing:
                bodyStmts.push(module->addInitial(
                    ASTKind::PreDecr,
                    origin(binding.firstToken, binding.lastToken),
                    module->fromOperand(binding.high).reconstituteOrigin(binding.highPos)
                ));
                break;
            case BindingKind::RangeIncreasing:
                bodyStmts.push(module->addInitial(
                    ASTKind::PreIncr,
                    origin(binding.firstToken, binding.lastToken),
                    module->fromOperand(binding.low).reconstituteOrigin(binding.lowPos)
                ));
                break;
        }

        // We construct the loop from the condition and body statements.

        IndexedAST bodyBlock;
        if (bodyStmts.size() > 0)
            bodyBlock = module->addInitial(ASTKind::Do, origin(token, bodyStmts.back()), bodyStmts);
        else
            bodyBlock = module->addInitial(ASTKind::Do, origin(token), bodyStmts);
        IndexedAST loop = module->addInitial(ASTKind::While, origin(token, bodyBlock), condition, bodyBlock);

        // Then combine the loop with the initialization statements in a Do.

        return module->addInitial(ASTKind::DoScoped, origin(token, loop), seq, loop);
    }

    IndexedAST parseCase(Module* module, TokenVisitor& visitor) {
        if (visitor.peek().token != KeywordCase && visitor.peek().token != KeywordElse) {
            error(module, visitor.peek().pos, "Expected keyword 'case' or 'else' in match case, found '", TokenFormat(module, visitor.peek()), "'.");
            return module->add(ASTKind::Missing).positionless();
        }

        bool isElse = visitor.peek().token == KeywordElse;
        auto token = visitor.read();
        IndexedAST pattern = isElse ? module->add(ASTKind::Missing).positionless() : parsePattern(module, visitor);

        if (visitor.peek().token != PunctuatorColon) {
            error(module, visitor.peek(), "Expected colon ':' in match case, found '", TokenFormat(module, visitor.peek()), "'.");
            visitor.readUpToAndIncluding(WhitespaceNewline);
            return module->add(ASTKind::Missing).positionless();
        }
        visitor.read();

        IndexedAST body;
        if (visitor.done() || visitor.peek().token == WhitespaceNewline)
            body = parseBlock(module, visitor);
        else
            body = parseStatement(module, visitor);

        return module->addInitial(ASTKind::Case, origin(token, body), pattern, body);
    }

    IndexedAST parseSingleInlineStatement(Module* module, TokenVisitor& visitor) {
        u32 line = visitor.peek().pos.line;
        IndexedAST ast = parsePrimaryStatement(module, visitor);
        if (visitor.done() || visitor.peek().pos.line > line)
            return ast;
        while (true) {
            auto token = visitor.peek();
            IndexedAST cond;
            switch (token.token.symbol) {
                case KeywordIf:
                    visitor.read();
                    cond = parseExpression(module, visitor);
                    ast = module->addInitial(ASTKind::If, origin(ast, cond), cond, ast);
                    continue;
                case KeywordUnless:
                    visitor.read();
                    cond = parseExpression(module, visitor);
                    ast = module->addInitial(ASTKind::If, origin(ast, cond), module->addInitial(ASTKind::Not, origin(cond), cond), ast);
                    continue;
                case KeywordWhile:
                    visitor.read();
                    cond = parseExpression(module, visitor);
                    ast = module->addInitial(ASTKind::While, origin(ast, cond), cond, ast);
                    continue;
                case KeywordUntil:
                    visitor.read();
                    cond = parseExpression(module, visitor);
                    ast = module->addInitial(ASTKind::While, origin(ast, cond), module->addInitial(ASTKind::Not, origin(cond), cond), ast);
                    continue;
                case KeywordFor:
                    // Postfix for-loop must be the last in the chain (other
                    // than a Then) to avoid a parsing ambiguity.
                    return parseForLoop(module, visitor, visitor.read(), ast, true);
                default:
                    return ast;
            }
        }
    }

    IndexedAST parseInlineStatement(Module* module, TokenVisitor& visitor) {
        // This function pretty much just handles the `then` operator, which has
        // lower precedence than all other inline statement operators. Since it's
        // (currently) just the one it doesn't feel worth it to make a whole other
        // precedence parser for inline statements.

        u32 line = visitor.peek().pos.line;
        IndexedAST ast = parseSingleInlineStatement(module, visitor);
        if (visitor.done() || visitor.peek().pos.line > line)
            return ast;

        while (visitor.peek().token == KeywordThen) {
            Pos pos = visitor.read().pos;
            auto next = parseSingleInlineStatement(module, visitor);
            ast = module->addInitial(ASTKind::Then, origin(ast, next), ast, next);
        }
        return ast;
    }

    void expectStatementTerminator(Module* module, TokenVisitor& visitor) {
        if (!visitor.done() && visitor.peek().token != WhitespaceNewline)
            error(module, visitor.peek(), "Expected newline after statement, found '", TokenFormat(module, visitor.peek()), "'.");
        if (!visitor.done())
            visitor.read();
    }

    IndexedAST parseStatement(Module* module, TokenVisitor& visitor) {
        consumeNewlines(visitor);
        auto token = visitor.peek();
        switch (token.token.symbol) {
            case KeywordIf: {
                Pos pos = visitor.read().pos;
                IndexedAST cond = parseExpression(module, visitor);
                IndexedAST body = parseBlockOrChain(module, visitor, "if statement", false);
                consumeNewlines(visitor);
                if (visitor.peek().token == KeywordElse) {
                    visitor.read();
                    IndexedAST elseBody = parseBlockOrChain(module, visitor, "if-else statement", false);
                    return module->addInitial(ASTKind::IfElse, origin(token, elseBody), cond, body, elseBody);
                } else
                    return module->addInitial(ASTKind::If, origin(token, body), cond, body);
            }
            case KeywordUnless: {
                Pos pos = visitor.read().pos;
                IndexedAST cond = parseExpression(module, visitor);
                IndexedAST body = parseBlockOrChain(module, visitor, "unless statement", false);
                return module->addInitial(ASTKind::If, origin(token, body), module->addInitial(ASTKind::Not, origin(cond), cond), body);
            }
            case KeywordWhile: {
                Pos pos = visitor.read().pos;
                IndexedAST cond = parseExpression(module, visitor);
                IndexedAST body = parseBlockOrChain(module, visitor, "while statement", false);
                return module->addInitial(ASTKind::While, origin(token, body), cond, body);
            }
            case KeywordUntil: {
                Pos pos = visitor.read().pos;
                IndexedAST cond = parseExpression(module, visitor);
                IndexedAST body = parseBlockOrChain(module, visitor, "until statement", false);
                return module->addInitial(ASTKind::While, origin(token, body), module->addInitial(ASTKind::Not, origin(cond), cond), body);
            }
            case KeywordFor:
                return parseForLoop(module, visitor, visitor.read(), {}, false);
            case KeywordMatch: {
                Pos pos = visitor.read().pos;
                IndexedAST cond = parseExpression(module, visitor);
                if (visitor.peek().token != PunctuatorColon) {
                    error(module, visitor.peek(), "Expected colon ':' in match statement, found '", TokenFormat(module, visitor.peek()), "'.");
                    visitor.readUpToAndIncluding(WhitespaceNewline);
                    return module->add(ASTKind::Missing).positionless();
                }
                visitor.read();

                vec<IndexedAST> cases;
                if (visitor.done())
                    return module->addInitial(ASTKind::Match, origin(token, cond), cond, cases);

                if (visitor.peek().token != WhitespaceNewline) {
                    error(module, visitor.peek(), "Expected newline after colon ':' in match statement, found '", TokenFormat(module, visitor.peek()), "'.");
                    visitor.readUpToAndIncluding(WhitespaceNewline);
                    return module->add(ASTKind::Missing).positionless();
                }
                visitor.read();

                if (visitor.peek().token != WhitespaceIndent) {
                    error(module, visitor.peek().pos, "Expected indented block.");
                    return module->addInitial(ASTKind::Match, origin(token, cond), cond, cases);
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
                return module->addInitial(ASTKind::Match, origin(token, cases.size() == 0 ? cond : cases.back()), cond, cases);
            }
            case KeywordOn:
            case KeywordDefault:
                unreachable("TODO");
            default: {
                u32 line = visitor.peek().pos.line;
                IndexedAST result = parseInlineStatement(module, visitor);
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

    IndexedAST parseModuleName(Module* module, TokenVisitor& visitor) {
        IndexedAST name = parseIdentifier(module, visitor);
        while (!visitor.done() && visitor.peek().token == PunctuatorDot) {
            Pos pos = visitor.read().pos;
            auto field = parseIdentifier(module, visitor);
            name = module->addInitial(ASTKind::GetField, origin(name, field), name, field);
        }
        return name;
    }

    IndexedAST parsePossibleExport(Module* module, TokenVisitor& visitor) {
        consumeNewlines(visitor);
        auto token = visitor.peek();
        if (token.token == KeywordExport) {
            visitor.read();
            IndexedAST decl = parseStatement(module, visitor);
            return module->addInitial(ASTKind::Export, origin(token, decl), decl);
        }
        return parseStatement(module, visitor);
    }

    IndexedAST parseTopLevel(Module* module, TokenVisitor& visitor) {
        vec<IndexedAST, 32> topLevel;
        module->setTempTopLevel(topLevel);
        while (!visitor.done()) {
            consumeNewlines(visitor);
            if (visitor.done())
                break;
            topLevel.push(parsePossibleExport(module, visitor));
        }
        if (topLevel.size() == 0)
            return module->addInitial(ASTKind::TopLevel, origin());
        return module->addInitial(ASTKind::TopLevel, origin(topLevel.front(), topLevel.back()), topLevel);
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

            return module->add(kind, children);
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
        module->setTopLevel(module->add(ASTKind::TopLevel, topLevel));
        artifact->update(ArtifactKind::ParsedAST, module);
        if UNLIKELY(config::printParseTree)
            module->print(module->compilation), println();
        return artifact;
    }
}