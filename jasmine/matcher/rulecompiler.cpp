#include "util/vec.h"
#include "util/hash.h"
#include "util/io.h"
#include "util/str.h"
#include "util/deque.h"
#include "jasmine/pass/reduce.h"

namespace jasmine {
    struct Source {
        const_slice<i8> source;

        ~Source() {
            delete[] source.data();
        }
    };

    Source* load(const_slice<i8> path) {
        auto file = file::open(path, file::READ);
        if (file == -1) {
            println("Couldn't open file ", path);
            process::exit(1);
        }
        array<i8, file::FDBUF_SIZE> buffer;
        vec<i8, file::FDBUF_SIZE> bytes;
        iword read = file::read(file, buffer);
        while (read > 0) {
            for (iword i = 0; i < read; i ++)
                bytes.push(buffer[i]);
            read = file::read(file, buffer);
        }
        file::close(file);
        return new Source { bytes.take_slice() };
    }

    enum class PatternType {
        Int, F32, F64, Var, Wildcard
    };

    struct TypeSystem {
        map<const_slice<i8>, AbstractType> abstractTypeMap;

        inline TypeSystem() {
            #define PUT_TYPE(upper, lower, ...) abstractTypeMap.put(cstring(#lower), AbstractType:: upper);
            FOR_EACH_TYPE_KIND(PUT_TYPE);
            #undef PUT_TYPE
            abstractTypeMap.put(cstring("union"), AbstractType::UNION);
            abstractTypeMap.put(cstring("struct"), AbstractType::STRUCT);
            abstractTypeMap.put(cstring("array"), AbstractType::ARRAY);
            abstractTypeMap.put(cstring("vector"), AbstractType::VECTOR);
            abstractTypeMap.put(cstring("function"), AbstractType::FUNCTION);
            abstractTypeMap.put(cstring("any_signed"), AbstractType::ANY_SIGNED);
            abstractTypeMap.put(cstring("any_unsigned"), AbstractType::ANY_UNSIGNED);
            abstractTypeMap.put(cstring("any_int"), AbstractType::ANY_INT);
            abstractTypeMap.put(cstring("any_float"), AbstractType::ANY_FLOAT);
            abstractTypeMap.put(cstring("any_number"), AbstractType::ANY_NUMBER);
            abstractTypeMap.put(cstring("sint"), AbstractType::ANY_SIGNED);
            abstractTypeMap.put(cstring("uint"), AbstractType::ANY_UNSIGNED);
            abstractTypeMap.put(cstring("int"), AbstractType::ANY_INT);
            abstractTypeMap.put(cstring("float"), AbstractType::ANY_FLOAT);
            abstractTypeMap.put(cstring("num"), AbstractType::ANY_NUMBER);
            abstractTypeMap.put(cstring("any"), AbstractType::ANY);
        }

        inline bool contains(const_slice<i8> name) {
            return abstractTypeMap.find(name) != abstractTypeMap.end();
        }

        inline AbstractType get(const_slice<i8> name) {
            auto it = abstractTypeMap.find(name);
            if (it == abstractTypeMap.end())
                unreachable("Unknown type name ", name);
            return it->value;
        }

        inline AbstractType intersect(AbstractType a, AbstractType b) {
            return AbstractType(u32(a) & u32(b));
        }

        inline const_slice<i8> nameOf(AbstractType type) {
            #define DEFINE_CASE(upper, lower, ...) case AbstractType:: upper: return cstring(#upper);
            switch (type) {
                FOR_EACH_TYPE_KIND(DEFINE_CASE)
                case AbstractType::STRUCT:
                    return cstring("STRUCT");
                case AbstractType::UNION:
                    return cstring("UNION");
                case AbstractType::ARRAY:
                    return cstring("ARRAY");
                case AbstractType::VECTOR:
                    return cstring("VECTOR");
                case AbstractType::FUNCTION:
                    return cstring("FUNCTION");
                case AbstractType::ANY_SIGNED:
                    return cstring("ANY_SIGNED");
                case AbstractType::ANY_UNSIGNED:
                    return cstring("ANY_UNSIGNED");
                case AbstractType::ANY_INT:
                    return cstring("ANY_INT");
                case AbstractType::ANY_FLOAT:
                    return cstring("ANY_FLOAT");
                case AbstractType::ANY_NUMBER:
                    return cstring("ANY_NUMBER");
                case AbstractType::ANY:
                    return cstring("ANY");
                default:
                    break;
            }
            #undef DEFINE_CASE
            if (!type)
                unreachable("Tried to get name of uninhabited type");
            u32 bits = (u32)type;
            vec<i8, 64> buffer;
            vec_ref<64> bufref(buffer);
            bool first = true;
            for (u32 i = 0; i < 32; i ++) if (bits & 1 << i) {
                if (!first)
                    format(bufref, '|');
                first = false;
                format(bufref, nameOf(AbstractType(bits & 1 << i)));
            }
            return buffer.take_slice(); // Memory leak, but probably doesn't matter for this tool.
        }
    };

    struct Env {
        TypeSystem* typeSystem;
        map<const_slice<i8>, AbstractType> types;
        map<const_slice<i8>, PatternType> vars;

        inline Env(TypeSystem* typeSystem_in): typeSystem(typeSystem_in) {}
    };

    struct AST {
        enum Kind {
            IntDecl, F32Decl, F64Decl, VarDecl, WildcardDecl,
            IntConst, F32Const, F64Const,
            IntUse, F32Use, F64Use, VarUse, WildcardUse,
            IntExpr, F32Expr, F64Expr, VarExpr, WildcardExpr,
            Rule, RuleParams, RuleResult,
            Typename, TypeConstraint,
            WhereClause
        };
        Kind kind;
        const_slice<i8> name;
        AST* type;
        Env* env;
        vec<AST*, 3> children;

        inline AST(Kind kind_in): kind(kind_in), env(nullptr) {}

        void write(file::fd io) {
            switch (kind) {
                case WhereClause:
                    ::write(io, name);
                    break;
                case IntDecl:
                case IntUse:
                case IntConst:
                    ::write(io, name, ":i");
                    break;
                case F32Decl:
                case F32Use:
                case F32Const:
                    ::write(io, name, ":f");
                    break;
                case F64Decl:
                case F64Use:
                case F64Const:
                    ::write(io, name, ":d");
                    break;
                case VarDecl:
                case VarUse:
                    ::write(io, name, ":v");
                    break;
                case WildcardDecl:
                case WildcardUse:
                    ::write(io, name, ":?");
                    break;
                case IntExpr:
                    ::write(io, '[', name, "]:i");
                    break;
                case F32Expr:
                    ::write(io, '[', name, "]:f");
                    break;
                case F64Expr:
                    ::write(io, '[', name, "]:d");
                    break;
                case VarExpr:
                    ::write(io, '[', name, "]:v");
                    break;
                case WildcardExpr:
                    ::write(io, '[', name, "]:?");
                    break;
                case Typename:
                    ::write(io, "$", name);
                    break;
                case TypeConstraint:
                    children[0]->write(io);
                    ::write(io, ":");
                    children[1]->write(io);
                    break;
                case RuleParams:
                case RuleResult:
                    ::write(io, '(', name, ' ');
                    type->write(io);
                    for (AST* other : children) {
                        ::write(io, ' ');
                        other->write(io);
                    }
                    ::write(io, ')');
                    break;
                case Rule:
                    ::write(io, name);
                    children[0]->write(io);
                    if (children[1]) {
                        ::write(io, " where ");
                        children[1]->write(io);
                    }
                    ::write(io, ": ");
                    children[2]->write(io);
                    ::write(io, "\n");
                    break;
            }
        }

        void print() {
            write(file::stdout);
        }
    };

    struct Parser {
        Source* source;
        u32 pos;
        u32 line, column;

        inline Parser(Source* source_in):
            source(source_in), pos(0), line(1), column(0) {}

        inline i8 peek() {
            if (pos >= source->source.size())
                return 0;
            return source->source[pos];
        }

        inline i8 read() {
            i8 result = peek();
            if (pos < source->source.size()) {
                pos ++;
                if (result == '\n')
                    line ++, column = 0;
                else if (result == '\t')
                    column += 4;
                else
                    column ++;
            }
            return result;
        }

        inline void expect(i8 ch) {
            if (read() != ch) {
                println("Expected ", ch, " at ", line, ":", column);
                crash();
            }
        }

        inline bool done() const {
            return pos >= source->source.size();
        }

        inline void skipws() {
            while (peek() == ' ' || peek() == '\n' || peek() == '\t' || peek() == '#') {
                if (peek() == '#')
                    while (peek() != '\n') read();
                read();
            }
        }

        inline bool isDelim(i8 ch) {
            return ch == ' ' || ch == '\n' || ch == '\t' || ch == ')' || ch == '(' || ch == '[' || ch == ']' || ch == ':';
        }

        inline bool isDigit(i8 ch) {
            return ch >= '0' && ch <= '9';
        }

        inline const_slice<i8> readName(bool allowEmpty = false) {
            const i8* start = &source->source[pos];
            u32 size = 0;
            while (!isDelim(peek()))
                read(), size ++;
            if (size == 0 && !allowEmpty)
                unreachable("Expected identifier at ", line, ":", column);
            return { start, size };
        }

        inline AST* parseTerminal(Env* env, bool isParams) {
            skipws();
            const_slice<i8> name;
            bool isCode = false, isConst = false;
            i8 base = 0, isNegative = false, startsWithZero = false, hasSeenDecimal = false;
            if (peek() == '[') {
                read();
                if (isParams)
                    unreachable("Expressions are not permitted in rule parameters.");
                const i8* start = &source->source[pos];
                while (peek() && peek() != ']')
                    read();
                name = { start, &source->source[pos] - start };
                expect(']');
                isCode = true;
            } else if (isDigit(peek()) || peek() == '-') {
                const i8* start = &source->source[pos];
                if (peek() == '-') {
                    isNegative = true;
                    read();
                }
                if (peek() == '0')
                    startsWithZero = true;
                while (isDigit(peek()) || peek() == '.' || peek() == 'x' || peek() == 'b') {
                    if (peek() == 'x' || peek() == 'b') {
                        if (base || hasSeenDecimal || !startsWithZero)
                            unreachable("Found unexpected base signifier ", peek(), " at ", line, ":", column);
                        base = peek();
                    }
                    if (peek() == '.') {
                        if (hasSeenDecimal || base)
                            unreachable("Found unexpected decimal point at ", line, ":", column);
                        hasSeenDecimal = true;
                    }
                    read();
                }
                name = { start, &source->source[pos] - start };
                isConst = true;
            } else {
                name = readName();
                auto it = env->vars.find(name);
                if (isParams) {
                    if (it != env->vars.end())
                        unreachable("Duplicate definition of variable ", name, " at ", line, ":", column);
                } else if (it == env->vars.end())
                    unreachable("Undefined variable ", name, " at ", line, ":", column);
            }
            skipws();
            expect(':');
            skipws();
            AST::Kind kind;
            PatternType type;
            switch (peek()) {
                case 'v': kind = (!isParams ? (isCode ? AST::VarExpr : AST::VarUse) : AST::VarDecl), type = PatternType::Var; break;
                case 'i': kind = (isConst ? AST::IntConst : (!isParams ? (isCode ? AST::IntExpr : AST::IntUse) : AST::IntDecl)), type = PatternType::Int; break;
                case 'f': kind = (isConst ? AST::F32Const : (!isParams ? (isCode ? AST::F32Expr : AST::F32Use) : AST::F32Decl)), type = PatternType::F32; break;
                case 'd': kind = (isConst ? AST::F64Const : (!isParams ? (isCode ? AST::F64Expr : AST::F64Use) : AST::F64Decl)), type = PatternType::F64; break;
                case '?': kind = (!isParams ? (isCode ? AST::WildcardExpr : AST::WildcardUse) : AST::WildcardDecl), type = PatternType::Wildcard; break;
                default: unreachable("Unexpected variable kind ", peek());
            }
            if (type == PatternType::Int && hasSeenDecimal)
                unreachable("Invalid type 'i' for constant ", name, " at ", line, ":", column);
            if (type != PatternType::Int && base)
                unreachable("Invalid type '", peek(), "' for constant ", name, " at ", line, ":", column);
            read();
            AST* param = new AST(kind);
            param->name = name;
            if (isParams && !isConst)
                env->vars.put(param->name, type);
            return param;
        }

        inline AST* parseType(Env* env, bool isParams) {
            skipws();
            AST* lhs = new AST(AST::Typename);
            lhs->name = readName();
            skipws();
            if (peek() == ':') {
                if (!isParams)
                    unreachable("Trying to constrain type variable definition outside rule pattern at ", line, ":", column);
                read();
                skipws();
                AST* rhs = new AST(AST::Typename);
                rhs->name = readName();
                if (!env->typeSystem->contains(rhs->name))
                    unreachable("Unrecognized type constraint ", rhs->name, " at ", line, ":", column);
                AbstractType constraint = env->typeSystem->get(rhs->name);
                AST* result = new AST(AST::TypeConstraint);
                result->children.push(lhs, rhs);
                if (env->types.contains(lhs->name))
                    env->types[lhs->name] = env->typeSystem->intersect(env->types[lhs->name], constraint);
                else
                    env->types.put(lhs->name, constraint);
                return result;
            } else {
                if (!env->typeSystem->contains(lhs->name))
                    env->types.put(lhs->name, AbstractType::ANY);
                return lhs;
            }
        }

        inline AST* parseInstruction(Env* env, bool isParams) {
            AST* node = new AST(isParams ? AST::RuleParams : AST::RuleResult);
            node->env = env;
            skipws();
            expect('(');
            skipws();
            const_slice<i8> opcode = readName();
            node->name = opcode;
            skipws();
            node->type = parseType(env, isParams);
            while (peek() && peek() != ')') {
                node->children.push(parseExpr(env, isParams));
                skipws();
            }
            expect(')');
            skipws();
            return node;
        }

        inline AST* parseExpr(Env* env, bool isParams) {
            skipws();
            switch (peek()) {
                case '(': return parseInstruction(env, isParams);
                default: return parseTerminal(env, isParams);
            }
        }

        inline AST* parse(TypeSystem* typeSystem) {
            AST* rule = new AST(AST::Rule);
            rule->env = new Env(typeSystem);
            skipws();
            if (peek() != '(')
                rule->name = readName();
            else
                rule->name = { nullptr, 0 };
            rule->children.push(parseInstruction(rule->env, true));
            AST* where = nullptr;

            if (peek() == 'w') {
                read();
                expect('h');
                expect('e');
                expect('r');
                expect('e');
                skipws();

                const char* start = &source->source[pos];
                where = new AST(AST::WhereClause);
                while (peek() && peek() != ':')
                    read();
                where->name = const_slice<i8>{ start, &source->source[pos] - start };
            }
            expect(':');
            rule->children.push(where);
            rule->children.push(parseExpr(rule->env, false));
            skipws();
            return rule;
        }
    };

    void parse(Parser& parser, vec<AST*>& rules, TypeSystem* typeSystem) {
        parser.skipws();
        while (!parser.done()) {
            rules.push(parser.parse(typeSystem));
            parser.skipws();
        }
    }

    struct Compiler {
        map<const_slice<i8>, Opcode> opcodeMap;
        vec<AST*>& rules;
        fd out;
        vec<vec<AST*>> rulesByOpcode;
        u32 anonymousRules = 0;
        u32 indentation = 0;

        inline Compiler(vec<AST*>& rules_in, const char* path):
            rules(rules_in) {
            if (path[0] == '-' && path[1] == 0)
                out = file::stdout;
            else
                out = file::open({ path, findc(path, 0) }, file::WRITE);
            if (out == -1)
                unreachable("Can't write to output file ", path);
            #define PUT_OPCODE(upper, lower) opcodeMap.put(cstring(#lower), Opcode:: upper);
            FOR_EACH_OPCODE(PUT_OPCODE)
            #undef PUT_OPCODE

            #define CREATE_LIST(...) rulesByOpcode.push({});
            FOR_EACH_OPCODE(CREATE_LIST)
            #undef CREATE_LIST
        }

        inline ~Compiler() {
            for (AST* ast : rules)
                delete ast;
            file::close(out);
        }

        struct RuleHandle {
            Opcode opcode;
            AST* rule;
            const_slice<i8> functionName;
        };

        inline void indent() {
            for (u32 i = 0; i < indentation; i ++)
                write(out, ' ');
        }

        inline void compileMatch(AST* rule, AST* pattern, i32 index) {
            indent(), writeln(out, "_operand = _uses[", index, "];");
            AbstractType type;
            switch (pattern->kind) {
                case AST::IntDecl:
                    indent(); writeln(out, "if (_operand.kind != Operand::IntConst) FAIL;");
                    indent(); writeln(out, pattern->name, " = _fn.intValueOf(_operand);");
                    break;
                case AST::F32Decl:
                    indent(); writeln(out, "if (_operand.kind != Operand::F32Const) FAIL;");
                    indent(); writeln(out, pattern->name, " = _fn.f32ValueOf(_operand);");
                    break;
                case AST::F64Decl:
                    indent(); writeln(out, "if (_operand.kind != Operand::F64Const) FAIL;");
                    indent(); writeln(out, pattern->name, " = _fn.f64ValueOf(_operand);");
                    break;
                case AST::VarDecl:
                    indent(); writeln(out, "if (_operand.kind != Operand::Var) FAIL;");
                    indent(); writeln(out, pattern->name, " = _operand.var;");
                    break;
                case AST::WildcardDecl:
                    indent(); writeln(out, pattern->name, " = _operand;");
                    break;
                case AST::IntConst:
                    indent(); writeln(out, "if (_operand.kind != Operand::IntConst || _fn.intValueOf(_operand) != ", pattern->name, ") FAIL;");
                    break;
                case AST::F32Const:
                    indent(); writeln(out, "if (_operand.kind != Operand::F32Const || _fn.f32ValueOf(_operand) != ", pattern->name, ") FAIL;");
                    break;
                case AST::F64Const:
                    indent(); writeln(out, "if (_operand.kind != Operand::F64Const || _fn.f64ValueOf(_operand) != ", pattern->name, ") FAIL;");
                    break;
                case AST::RuleParams:
                    indent(); writeln(out, "if (_operand.kind != Operand::Var) FAIL;");
                    indent(); writeln(out, "if (_defs[_operand.var] == -1) FAIL;");
                    indent(); writeln(out, "{");
                    indentation += 4;
                    indent(); writeln(out, "Node _node = _fn.node(_defs[_operand.var]);");
                    indent(); writeln(out, "if (_node.opcode() != Opcode::", OPCODE_NAMES_UPPER[(i32)opcodeMap[pattern->name]], ") FAIL;");

                    if (pattern->type->kind == AST::TypeConstraint) {
                        // Constraints are used to introduce type variables.
                        indent(), writeln(out, "if (!(u32(abstractType(_fn, _node.type())) & ", (u32)rule->env->typeSystem->get(pattern->type->children[1]->name), ")) FAIL;");
                        indent(), writeln(out, "auto ", pattern->type->children[0]->name, " = _node.type();");
                    } else {
                        assert(pattern->type->kind == AST::Typename);
                        if (rule->env->types.contains(pattern->type->name))
                            indent(), writeln(out, "if (_node.type() != ", pattern->type->name, ") FAIL;");
                        else
                            indent(), writeln(out, "if (!(u32(abstractType(_fn, _node.type())) & ", (u32)rule->env->typeSystem->get(pattern->type->name), ")) FAIL;");
                    }
                    indent(); writeln(out, "auto _uses = _node.uses();");
                    indent(); writeln(out, "if (_uses.size() != ", pattern->children.size(), ") FAIL;");
                    for (auto [i, pattern] : enumerate(pattern->children))
                        compileMatch(rule, pattern, i);
                    indentation -= 4;
                    indent(); writeln(out, "}");
                    break;
                default:
                    unreachable("Unexpected pattern");
            }
        }

        inline void compileTransform(AST* rule, AST* pattern) {
            switch (pattern->kind) {
                case AST::RuleResult: {
                    indent(); writeln(out, "{");
                    indentation += 4;

                    assert(pattern->type->kind == AST::Typename);
                    if (rule->env->typeSystem->contains(pattern->type->name) && rule->env->typeSystem->get(pattern->type->name) >= AbstractType::STRUCT)
                        unreachable("Result patterns can only have numeric or variable types");
                    for (AST* other : pattern->children)
                        compileTransform(rule, other);
                    indent(); writeln(out, "Operand _result = _fn.variable();");
                    indent(); write(out, "_fn.variableList[_result.var].type = ");
                    if (rule->env->typeSystem->contains(pattern->type->name))
                        write(out, "TypeKind::", rule->env->typeSystem->nameOf(rule->env->typeSystem->get(pattern->type->name)));
                    else
                        write(out, pattern->type->name);
                    writeln(out, ";");
                    indent(); write(out, "Node _newNode = _fn.addNode(Opcode::", OPCODE_NAMES_UPPER[(i32)opcodeMap[pattern->name]], ", ");
                    if (rule->env->typeSystem->contains(pattern->type->name))
                        write(out, "TypeKind::", rule->env->typeSystem->nameOf(rule->env->typeSystem->get(pattern->type->name)));
                    else
                        write(out, pattern->type->name);
                    writeln(out, ", _result, const_slice<Operand>{ &_operands[_operands.size() - ", pattern->children.size(), "], ", pattern->children.size(), "});");
                    indent(); writeln(out, "(*_ctx.defs).push(_newNode.index());");
                    indent(); writeln(out, "_fn.addNodeToInsertion(_newNode);");
                    indent(); writeln(out, "_operands.trim(", pattern->children.size(), ");");
                    indent(); writeln(out, "_operands.push(_result);");
                    indentation -= 4;
                    indent(); writeln(out, "}");
                    break;
                }
                case AST::VarUse:
                case AST::VarExpr:
                    indent(), writeln(out, "_operands.push(_fn.variableById(", pattern->name, "));");
                    break;
                case AST::IntUse:
                case AST::IntExpr:
                case AST::IntConst:
                    indent(), writeln(out, "_operands.push(_fn.intConst(", pattern->name, "));");
                    break;
                case AST::F32Use:
                case AST::F32Expr:
                case AST::F32Const:
                    indent(), writeln(out, "_operands.push(_fn.f32Const(", pattern->name, "));");
                    break;
                case AST::F64Use:
                case AST::F64Expr:
                case AST::F64Const:
                    indent(), writeln(out, "_operands.push(_fn.f64Const(", pattern->name, "));");
                    break;
                case AST::WildcardUse:
                case AST::WildcardExpr:
                    indent(), writeln(out, "_operands.push(", pattern->name, ");");
                    break;
                case AST::RuleParams:
                case AST::Rule:
                case AST::VarDecl:
                case AST::IntDecl:
                case AST::F32Decl:
                case AST::F64Decl:
                case AST::WildcardDecl:
                case AST::WhereClause:
                case AST::Typename:
                case AST::TypeConstraint:
                    unreachable("Found invalid pattern in result");
            }
        }

        inline RuleHandle compileRule(AST* rule) {
            Env* env = rule->env;
            Opcode opcode = opcodeMap[rule->children[0]->name];
            const_slice<i8> name = rule->name;
            if (name.size() == 0)
                name = tostring("_anonymous_rule_", anonymousRules ++);

            writeln(out, "    static MatchResult ", name, "(PassContext& _ctx, Function& _fn, Block _block, u32 _indexInBlock, Node _node) {");
            writeln(out, "        assert(_node.opcode() == Opcode::", OPCODE_NAMES_UPPER[(i32)opcode], ");");
            writeln(out, "        auto _uses = _node.uses();");
            writeln(out, "        auto& _defs = *_ctx.defs;");
            writeln(out, "        Operand _resultOperand;");
            writeln(out, "        if (_uses.size() != ", rule->children[0]->children.size(), ") FAIL;");
            indentation = 8;
            if (rule->children[0]->type->kind == AST::TypeConstraint) {
                // Constraints are used to introduce type variables.
                indent(), writeln(out, "if (!(u32(abstractType(_fn, _node.type())) & ", (u32)rule->env->typeSystem->get(rule->children[0]->type->children[1]->name), ")) FAIL;");
                indent(), writeln(out, "auto ", rule->children[0]->type->children[0]->name, " = _node.type();");
            } else {
                assert(rule->children[0]->type->kind == AST::Typename);
                if (rule->env->types.contains(rule->children[0]->type->name))
                    indent(), writeln(out, "if (_node.type() != ", rule->children[0]->type->name, ") FAIL;");
                else
                    indent(), writeln(out, "if (!(u32(abstractType(_fn, _node.type())) & ", (u32)rule->env->typeSystem->get(rule->children[0]->type->name), ")) FAIL;");
            }
            indent(); writeln(out, "Operand _operand;");
            for (const auto [k, v] : rule->env->vars) {
                indent();
                switch (v) {
                    case PatternType::Int: write(out, "i64 "); break;
                    case PatternType::F32: write(out, "f32 "); break;
                    case PatternType::F64: write(out, "f64 "); break;
                    case PatternType::Var: write(out, "i32 "); break;
                    case PatternType::Wildcard: write(out, "Operand "); break;
                }
                writeln(out, k, ";");
            }
            writeln(out, "        {");
            indentation += 4;
            for (auto [i, pattern] : enumerate(rule->children[0]->children))
                compileMatch(rule, pattern, i);
            indentation -= 4;
            writeln(out, "        }");

            if (rule->children[1])
                indent(), writeln(out, "if (!(", rule->children[1]->name, ")) FAIL;");

            // Now, we do the actual transform.
            bool hasNestedTransform = false;
            for (auto [i, a]: enumerate(rule->children[2]->children)) if (a->kind == AST::RuleResult)
                hasNestedTransform = true;
            switch (rule->children[2]->kind) {
                case AST::RuleResult: {
                    indent(), writeln(out, "vec<Operand, 16> _operands;");
                    assert(rule->children[2]->type->kind == AST::Typename);
                    indent(), write(out, "_operands.push(_node.operand(0));");
                    if (rule->env->typeSystem->contains(rule->children[2]->type->name) && rule->env->typeSystem->get(rule->children[2]->type->name) >= AbstractType::STRUCT)
                        unreachable("Result patterns can only have numeric or variable types");
                    if (hasNestedTransform)
                        indent(), writeln(out, "_fn.addInsertion(_block, _indexInBlock);");
                    for (auto [i, a]: enumerate(rule->children[2]->children))
                        compileTransform(rule, a);
                    indent(), write(out, "_fn.replaceNode(_node, Opcode:: ", OPCODE_NAMES_UPPER[(i32)opcodeMap[rule->children[2]->name]], ", ");
                    if (rule->env->typeSystem->contains(rule->children[2]->type->name))
                        write(out, "TypeKind::", rule->env->typeSystem->nameOf(rule->env->typeSystem->get(rule->children[2]->type->name)));
                    else
                        write(out, rule->children[2]->type->name);
                    writeln(out, ", _operands);");
                    writeln(out, "        PASS(_node);");
                    break;
                }
                case AST::VarUse:
                case AST::VarExpr:
                    indent(), writeln(out, "_fn.replaceNode(_node, Opcode::NOP, _node.type());");
                    writeln(out, "        _operand = _fn.variableById(", rule->children[2]->name, ");");
                    writeln(out, "        PASS(_operand);");
                    break;
                case AST::IntUse:
                case AST::IntExpr:
                case AST::IntConst:
                    indent(), writeln(out, "_fn.replaceNode(_node, Opcode::NOP, _node.type());");
                    writeln(out, "        _operand = _fn.intConst(", rule->children[2]->name, ");");
                    writeln(out, "        PASS(_operand);");
                    break;
                case AST::F32Use:
                case AST::F32Expr:
                case AST::F32Const:
                    indent(), writeln(out, "_fn.replaceNode(_node, Opcode::NOP, _node.type());");
                    writeln(out, "        _operand = _fn.f32Const(", rule->children[2]->name, ");");
                    writeln(out, "        PASS(_operand);");
                    break;
                case AST::F64Use:
                case AST::F64Expr:
                case AST::F64Const:
                    indent(), writeln(out, "_fn.replaceNode(_node, Opcode::NOP, _node.type());");
                    writeln(out, "        _operand = _fn.f64Const(", rule->children[2]->name, ");");
                    writeln(out, "        PASS(_operand);");
                    break;
                case AST::WildcardUse:
                case AST::WildcardExpr:
                    indent(), writeln(out, "_fn.replaceNode(_node, Opcode::NOP, _node.type());");
                    writeln(out, "        _operand = ", rule->children[2]->name, ";");
                    writeln(out, "        PASS(_operand);");
                    break;
                case AST::RuleParams:
                case AST::Rule:
                case AST::VarDecl:
                case AST::IntDecl:
                case AST::F32Decl:
                case AST::F64Decl:
                case AST::WildcardDecl:
                case AST::WhereClause:
                case AST::Typename:
                case AST::TypeConstraint:
                    unreachable("Found invalid pattern in result");
            }
            writeln(out, "    }");
            writeln(out);

            return { opcode, rule, name };
        }

        inline void compile() {
            writeln(out, "#include \"jasmine/pass/reduce.h\"");
            writeln(out, "namespace jasmine {");
            writeln(out, "    #define FAIL return MatchResult::fail()");
            writeln(out, "    #define PASS(n) return MatchResult::success(n)");

            vec<RuleHandle> ruleHandles;
            for (AST* ast : rules)
                ruleHandles.push(compileRule(ast));

            writeln(out, "    extern void populateDatabase(ReductionRules& rules) {");
            deque<AST*> bfs;
            for (RuleHandle handle : ruleHandles) {
                bfs.pushr(handle.rule->children[0]);
                u32 size = 0;
                vec<u8, 16> signature;
                while (bfs.size()) {
                    AST* rule = bfs.popl();
                    if (size >= TreeSignatureLength)
                        break;
                    // To achieve a kind of weak fuzzy matching, we add the negation of each TreeElement
                    // byte to our pattern. For a source instruction to match a pattern, we require that
                    // its data ANDed with the pattern data is zero. This lets us use the byte 0 as a
                    // hole, letting us match with anything in that position.
                    switch (rule->kind) {
                        case AST::RuleParams:
                            signature.push((u8)~signTreeElement(opcodeMap[rule->name])), size ++;
                            for (AST* ast : rule->children)
                                bfs.pushr(ast);
                            break;
                        case AST::F32Const:
                        case AST::F32Decl:
                            signature.push((u8)~signTreeElement(TreeElement::F32Const)), size ++;
                            break;
                        case AST::F64Const:
                        case AST::F64Decl:
                            signature.push((u8)~signTreeElement(TreeElement::F64Const)), size ++;
                            break;
                        case AST::IntConst:
                        case AST::IntDecl:
                            signature.push((u8)~signTreeElement(TreeElement::IntConst)), size ++;
                            break;
                        case AST::VarDecl: // Variables are basically wildcards that we don't clone.
                        case AST::WildcardDecl:
                            signature.push(0), size ++;
                            break;
                        default:
                            unreachable("Unexpected rule kind.");
                    }
                }
                while (signature.size() < TreeSignatureLength)
                    signature.push(0);
                while (signature.size() > TreeSignatureLength)
                    signature.pop();
                TreeSignature sig = ::load<TreeSignature>(signature.data());
                writeln(out, "         rules.add(Opcode::", OPCODE_NAMES_UPPER[(i32)handle.opcode], ", ", handle.functionName, ", { 0x", hex<u64>(sig.sig, 16), "ull }, \"", handle.functionName, "\");");
            }
            writeln(out, "    }");
            writeln(out, "}");
        }
    };
}

using namespace jasmine;

extern "C" i32 main(i32 argc, i8** argv) {
    if (argc < 3) {
        println("Usage: ", argv[0], " FILE... DEST");
        process::exit(1);
    }
    vec<AST*> rules;
    TypeSystem typeSystem;
    for (int i = 1; i < argc - 1; i ++) {
        Source* source = load({ argv[i], findc(argv[i], 0) });
        Parser parser(source);
        jasmine::parse(parser, rules, &typeSystem);
    }

    for (AST* ast : rules)
        ast->print();

    Compiler compiler(rules, argv[argc - 1]);
    compiler.compile();
    return 0;
}