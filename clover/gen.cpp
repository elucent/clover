#include "clover/gen.h"
#include "clover/ast.h"
#include "clover/jitruntime.h"
#include "clover/typecheck.h"
#include "jasmine/capi.h"
#include "jasmine/mod.h"

namespace clover {
    struct JasmineArtifact : public ArtifactData {
        JasmineModule module;
        JasmineArtifact(JasmineModule module_in): module(module_in) {}

        ~JasmineArtifact() {}

        maybe<const_slice<i8>> getSource() override {
            return none<const_slice<i8>>();
        }

        maybe<const_slice<u32>> getLineOffsets() override {
            return none<const_slice<u32>>();
        }

        void print(Compilation* compilation) override {
            jasmine_print_module(JasmineStdout, module);
        }

        u64 reportSize() override {
            // TODO: Report something meaningful here.
            return 0;
        }
    };

    struct GenerationContext {
        JasmineBuilder builder;
        Module* module;
        JasmineModule output;
        vec<JasmineType> types;
        maybe<JasmineAssembly> assembly;

        struct FunctionState {
            Function* function;
            JasmineFunction output;
            JasmineBlock block;
            u32 firstLocal;
        };
        vec<FunctionState> callStack;

        struct LoopState {
            NodeIndex loop;
            JasmineBlock continueTarget;
            JasmineBlock breakTarget;
        };
        vec<LoopState> loopStack;

        vec<JasmineOperand> locals;
        bool isUnreachable = false;

        u32 anonArrayId = 0;

        GenerationContext(JasmineBuilder builder_in, Module* module_in, JasmineModule output_in):
            builder(builder_in), module(module_in), output(output_in) {
            auto sys = module->types;
            types.expandTo(sys->typeList.size(), JASMINE_INVALID_TYPE);
        }

        JasmineEdge addEdge(JasmineBlock from, JasmineBlock to) {
            return jasmine_add_edge(callStack.back().output, from, to);
        }

        JasmineEdge addEdgeTo(JasmineBlock to) {
            return addEdge(jasmine_current_block(builder), to);
        }

        JasmineBlock addBlock() {
            return jasmine_add_block(callStack.back().output);
        }

        u32 nextAnonArrayId() {
            return anonArrayId ++;
        }

        TypeIndex assignTags(Type type, TypeIndex tag) {
            // Takes the next available tag, returns the next available tag
            // after assigning tags to each of the union cases.

            assert(type.is<TypeKind::Union>());
            auto unionType = type.as<TypeKind::Union>();
            for (u32 i = 0; i < unionType.count(); i ++) {
                Type caseType = unionType.caseType(i);
                if (caseType.is<TypeKind::Union>())
                    tag = assignTags(caseType, tag);
                else if (caseType.is<TypeKind::Struct>())
                    caseType.as<TypeKind::Struct>().setTypeTag(tag ++);
                else if (caseType.is<TypeKind::Named>())
                    caseType.as<TypeKind::Named>().setTypeTag(tag ++);
            }
            return tag;
        }

        void ensureTypeTag(Type type) {
            bool hasTag = true;
            if (type.is<TypeKind::Named>())
                hasTag = type.as<TypeKind::Named>().typeTag() != InvalidType;
            else if (type.is<TypeKind::Struct>())
                hasTag = type.as<TypeKind::Struct>().typeTag() != InvalidType;
            else if (type.is<TypeKind::Union>()) {
                assert(type.as<TypeKind::Union>().count() > 0);
                return ensureTypeTag(type.as<TypeKind::Union>().caseType(0));
            }
            if (!hasTag) {
                Type parent = parentType(type);
                while (isCase(parent))
                    parent = parentType(parent);
                assignTags(parent, 0);
            }
        }

        JasmineType lowerType(Type type) {
            if (type.index < ReservedTypes::NumReservedTypes) switch (u8(type.index)) {
                case U8: return JASMINE_TYPE_U8;
                case U16: return JASMINE_TYPE_U16;
                case U32: return JASMINE_TYPE_U32;
                case U64: return JASMINE_TYPE_U64;
                case I8: return JASMINE_TYPE_I8;
                case I16: return JASMINE_TYPE_I16;
                case I32: return JASMINE_TYPE_I32;
                case I64: return JASMINE_TYPE_I64;
                case F32: return JASMINE_TYPE_F32;
                case F64: return JASMINE_TYPE_F64;
                case Bool: return JASMINE_TYPE_BOOL;
                case Char: return JASMINE_TYPE_U32;
                case Void: return JASMINE_TYPE_VOID;
                case String: {
                    JasmineType types[2] = { JASMINE_TYPE_PTR, JASMINE_TYPE_U64 };
                    return jasmine_make_struct_type(output, types, 2);
                }
                default:
                    unreachable("Can't lower type ", type);
            }
            switch (type.kind()) {
                case TypeKind::Pointer:
                    return JASMINE_TYPE_PTR;
                case TypeKind::Slice: {
                    JasmineType types[2] = { JASMINE_TYPE_PTR, JASMINE_TYPE_U64 };
                    return jasmine_make_struct_type(output, types, 2);
                }
                case TypeKind::Array: {
                    JasmineType element = lower(type.as<TypeKind::Array>().elementType());
                    return jasmine_make_array_type(output, element, type.as<TypeKind::Array>().length());
                }
                case TypeKind::Tuple: {
                    vec<JasmineType> elements;
                    for (u32 i = 0; i < type.as<TypeKind::Tuple>().count(); i ++)
                        elements.push(lower(type.as<TypeKind::Tuple>().fieldType(i)));
                    return jasmine_make_struct_type(output, elements.data(), elements.size());
                }
                case TypeKind::Struct: {
                    vec<JasmineType> elements;
                    auto structType = type.as<TypeKind::Struct>();
                    if (structType.isCase()) {
                        ensureTypeTag(type);
                        elements.push(tagType());
                    }
                    for (u32 i = 0; i < structType.count(); i ++)
                        elements.push(lower(structType.fieldType(i)));
                    return jasmine_make_struct_type(output, elements.data(), elements.size());
                }
                case TypeKind::Named: {
                    auto namedType = type.as<TypeKind::Named>();
                    JasmineType element = lower(namedType.innerType());
                    if (namedType.isCase()) {
                        ensureTypeTag(type);
                        JasmineType elements[2];
                        elements[0] = tagType();
                        if (namedType.innerType() == Void)
                            return jasmine_make_struct_type(output, elements, 1);
                        elements[1] = element;
                        return jasmine_make_struct_type(output, elements, 2);
                    }
                    return jasmine_make_struct_type(output, &element, 1);
                }
                case TypeKind::Union: {
                    auto unionType = type.as<TypeKind::Union>();
                    vec<JasmineType> elements;
                    for (u32 i = 0; i < unionType.count(); i ++)
                        elements.push(lower(unionType.caseType(i)));
                    return jasmine_make_union_type(output, elements.data(), elements.size());
                }
                case TypeKind::Function: {
                    vec<JasmineType> elements;
                    for (u32 i = 0; i < type.as<TypeKind::Function>().parameterCount(); i ++)
                        elements.push(lower(type.as<TypeKind::Function>().parameterType(i)));
                    return jasmine_make_function_type(output, elements.data(), elements.size(), lower(type.as<TypeKind::Function>().returnType()));
                }
                default:
                    unreachable("Can't lower type ", type);
            }
        }

        JasmineType fpowType = JASMINE_INVALID_TYPE;
        JasmineType ipowType = JASMINE_INVALID_TYPE;
        JasmineType upowType = JASMINE_INVALID_TYPE;

        JasmineType getFpowType() {
            if (fpowType.index != JASMINE_INVALID_TYPE.index)
                return fpowType;
            JasmineType args[2] = { JASMINE_TYPE_F64, JASMINE_TYPE_F64 };
            return fpowType = jasmine_make_function_type(output, args, 2, JASMINE_TYPE_F64);
        }

        JasmineType getIpowType() {
            if (ipowType.index != JASMINE_INVALID_TYPE.index)
                return ipowType;
            JasmineType args[2] = { JASMINE_TYPE_I64, JASMINE_TYPE_I64 };
            return ipowType = jasmine_make_function_type(output, args, 2, JASMINE_TYPE_I64);
        }

        JasmineType getUpowType() {
            if (upowType.index != JASMINE_INVALID_TYPE.index)
                return upowType;
            JasmineType args[2] = { JASMINE_TYPE_U64, JASMINE_TYPE_U64 };
            return upowType = jasmine_make_function_type(output, args, 2, JASMINE_TYPE_U64);
        }

        JasmineType memoryAllocType = JASMINE_INVALID_TYPE;
        JasmineType memoryFreeType = JASMINE_INVALID_TYPE;

        JasmineType getMemoryAllocType() {
            if (memoryAllocType.index != JASMINE_INVALID_TYPE.index)
                return memoryAllocType;
            return memoryAllocType = jasmine_make_function_type(output, &JASMINE_TYPE_U64, 1, JASMINE_TYPE_PTR);
        }

        JasmineType getMemoryFreeType() {
            if (memoryFreeType.index != JASMINE_INVALID_TYPE.index)
                return memoryFreeType;
            return memoryFreeType = jasmine_make_function_type(output, &JASMINE_TYPE_PTR, 1, JASMINE_TYPE_VOID);
        }

        void enter(Function* function, JasmineFunction output) {
            if (callStack.size())
                callStack.back().block = jasmine_current_block(builder);
            callStack.push({ function, output, JASMINE_INVALID_BLOCK, locals.size() });
            locals.expandBy(function ? function->locals.size() : 0, JASMINE_INVALID_OPERAND);
            jasmine_builder_set_function(builder, output);
        }

        void leave() {
            auto state = callStack.pop();
            Function* function = state.function;
            JasmineFunction outputFunc = state.output;
            if (assembly)
                jasmine_compile_function_only(*assembly, outputFunc, 0, true);
            locals.shrinkBy(function ? function->locals.size() : 0);
            if (callStack.size() > 0) {
                jasmine_builder_set_function(builder, callStack.back().output);
                jasmine_builder_set_block(builder, callStack.back().block);
            }
            isUnreachable = false;
        }

        void beginLoop(AST loop, JasmineBlock continueTarget, JasmineBlock breakTarget) {
            loopStack.push({ loop.node, continueTarget, breakTarget });
        }

        JasmineBlock currentContinueTarget() {
            assert(loopStack.size());
            assert(!isUnreachable);
            return loopStack.back().continueTarget;
        }

        JasmineBlock currentBreakTarget() {
            assert(loopStack.size());
            assert(!isUnreachable);
            return loopStack.back().breakTarget;
        }

        void endLoop() {
            assert(loopStack.size());
            loopStack.pop();
        }

        JasmineType lower(Type t) {
            t = expand(t);
            types.expandTo(t.index + 1, JASMINE_INVALID_TYPE);
            if (types[t.index].index == JASMINE_INVALID_TYPE.index)
                return types[t.index] = lowerType(t);
            return types[t.index];
        }

        void setAssembly(JasmineAssembly as) {
            assembly = some<JasmineAssembly>(as);
        }

        Function* func() {
            return callStack.back().function;
        }

        JasmineOperand local(u32 i) {
            if (locals[callStack.back().firstLocal + i].bits == JASMINE_INVALID_OPERAND.bits) {
                #ifndef RELEASE
                    auto name = module->str(func()->locals[i].name);
                    i8 buffer[64];
                    auto fmt = prints({ buffer, 64 }, name, '_', i);
                    return locals[callStack.back().firstLocal + i] = jasmine_variable_with_name(jasmine_current_function(builder), fmt.data(), fmt.size());
                #else
                    return locals[callStack.back().firstLocal + i] = jasmine_variable(jasmine_current_function(builder));
                #endif
            }
            return locals[callStack.back().firstLocal + i];
        }

        JasmineOperand global(u32 i) {
            return staticref(module->str(module->globals[i].name));
        }

        JasmineValue globalValue(u32 i) {
            return staticrefValue(module->str(module->globals[i].name));
        }

        void setLocal(u32 i, JasmineOperand operand) {
            locals[callStack.back().firstLocal + i] = operand;
        }

        JasmineOperand imm(i64 i) {
            return jasmine_imm(jasmine_current_function(builder), i);
        }

        JasmineOperand f32imm(f32 f) {
            return jasmine_f32(jasmine_current_function(builder), f);
        }

        JasmineOperand f64imm(f64 f) {
            return jasmine_f64(jasmine_current_function(builder), f);
        }

        JasmineOperand sizeOf(JasmineType type) {
            return jasmine_sizeof(jasmine_current_function(builder), type);
        }

        JasmineOperand sizeOf(Type type) {
            return sizeOf(lower(type));
        }

        JasmineOperand temp() {
            return jasmine_variable(callStack.back().output);
        }

        JasmineOperand funcref(const_slice<i8> name) {
            return jasmine_function_ref(jasmine_current_function(builder), name.data(), name.size());
        }

        JasmineOperand dataref(const_slice<i8> name) {
            return jasmine_data_ref(jasmine_current_function(builder), name.data(), name.size());
        }

        JasmineOperand staticref(const_slice<i8> name) {
            return jasmine_static_ref(jasmine_current_function(builder), name.data(), name.size());
        }

        JasmineValue funcrefValue(const_slice<i8> name) {
            return jasmine_function_ref_value(output, name.data(), name.size());
        }

        JasmineValue datarefValue(const_slice<i8> name) {
            return jasmine_data_ref_value(output, name.data(), name.size());
        }

        JasmineValue staticrefValue(const_slice<i8> name) {
            return jasmine_static_ref_value(output, name.data(), name.size());
        }

        JasmineType sizeType() const {
            return JASMINE_TYPE_U64;
        }

        JasmineType tagType() const {
            return JASMINE_TYPE_U32;
        }

        Symbol defineAtom(Type atom) {
            ensureTypeTag(atom);

            i8 buffer[64];
            auto name = prints({ buffer, 64 }, ".atom.", module->str(atom.as<TypeKind::Named>().name()));
            jasmine_define_static(output, tagType(), name.data(), name.size(), jasmine_integer_value(output, tagType(), atom.as<TypeKind::Named>().typeTag()));
            auto sym = module->sym(name);
            atoms.put(atom.index, sym);
            return sym;
        }

        JasmineOperand atomRef(Type atom) {
            auto it = atoms.find(atom.index);
            if (it != atoms.end())
                return staticref(module->str(it->value));
            auto name = defineAtom(atom);
            return staticref(module->str(name));
        }

        JasmineValue atomRefValue(Type atom) {
            auto it = atoms.find(atom.index);
            if (it != atoms.end())
                return staticrefValue(module->str(it->value));
            auto name = defineAtom(atom);
            return staticrefValue(module->str(name));
        }

        map<TypeIndex, Symbol> atoms;

        JasmineOperand stringRef(Symbol string) {
            auto it = strings.find(string);
            if (it != strings.end())
                return dataref(module->str(it->value));
            auto str = module->str(string);
            i8 buffer[64];
            auto name = prints({ buffer, 64 }, ".str.", strings.size());
            jasmine_define_data(output, jasmine_make_array_type(output, JASMINE_TYPE_I8, str.size()), name.data(), name.size(), jasmine_i8_array_value(output, (const int8_t*)str.data(), str.size()));
            strings.put(string, module->sym(name));
            return dataref(name);
        }

        map<Symbol, Symbol> strings;

        slice<i8> mangleType(slice<i8> target, Type type) {
            type = expand(type);
            switch (type.kind()) {
                case TypeKind::Numeric:
                case TypeKind::Bool:
                case TypeKind::Char:
                case TypeKind::Void:
                case TypeKind::Any:
                case TypeKind::Bottom:
                    return format(target, type);
                case TypeKind::Pointer:
                    return format(mangleType(target, type.as<TypeKind::Pointer>().elementType()), "*");
                case TypeKind::Slice:
                    return format(mangleType(target, type.as<TypeKind::Slice>().elementType()), "[]");
                case TypeKind::Array:
                    return format(mangleType(target, type.as<TypeKind::Array>().elementType()), "[", type.as<TypeKind::Array>().length(), "]");
                case TypeKind::Tuple:
                    target = format(target, "(");
                    for (u32 i = 0; i < type.as<TypeKind::Tuple>().count(); i ++) {
                        if (i > 0) target = format(target, ",");
                        target = mangleType(target, type.as<TypeKind::Tuple>().fieldType(i));
                    }
                    return format(target, ")");
                case TypeKind::Function:
                    target = mangleType(target, type.as<TypeKind::Function>().returnType());
                    target = format(target, "(");
                    for (u32 i = 0; i < type.as<TypeKind::Function>().parameterCount(); i ++) {
                        if (i > 0) target = format(target, ",");
                        target = mangleType(target, type.as<TypeKind::Function>().parameterType(i));
                    }
                    return format(target, ")");
                case TypeKind::Struct:
                    return format(target, type.types->symbols->get(type.as<TypeKind::Struct>().name().symbol));
                case TypeKind::Named:
                    return format(target, type.types->symbols->get(type.as<TypeKind::Named>().name().symbol));
                case TypeKind::Union:
                    return format(target, type.types->symbols->get(type.as<TypeKind::Union>().name().symbol));
                default:
                    unreachable("Should not be mangling type ", type);
            }
        }

        Symbol mangledName(Module* module, Function* function) {
            if (function->mangledName != InvalidSymbol)
                return function->mangledName;

            AST decl = module->node(function->decl);
            auto name = decl.module->str(decl.child(1).varInfo(function).name);
            i8 buffer[1024];
            slice<i8> target = { buffer, 1024 };
            target = format(target, name);
            if (!module->noMangling) {
                target = format(target, "(");
                Type type = expand(decl.type());
                assert(type.is<TypeKind::Function>());
                for (u32 i = 0; i < type.as<TypeKind::Function>().parameterCount(); i ++) {
                    if (i > 0) target = format(target, ",");
                    target = mangleType(target, type.as<TypeKind::Function>().parameterType(i));
                }
                target = format(target, ")");
            }
            return function->mangledName = module->sym(const_slice<i8>{ buffer, 1024 - target.size() });
        }

        u32 expressionDepth = 0;
    };

    struct ExpressionDepthScope {
        GenerationContext& ctx;
        bool didActuallyRun;
        inline ExpressionDepthScope(GenerationContext& ctx_in, bool actuallyRun): ctx(ctx_in), didActuallyRun(actuallyRun) {
            if (actuallyRun)
                ctx.expressionDepth ++;
        }

        inline ~ExpressionDepthScope() {
            if (didActuallyRun)
                ctx.expressionDepth --;
        }
    };

    Type typeOf(AST ast) {
        assert(!ast.isLeaf());
        return expand(ast.type(ast.function()));
    }

    Type typeOf(Function* function, AST ast) {
        return expand(ast.type(function));
    }

    Type typeOf(AST ast, u32 i) {
        return expand(ast.child(i).type(ast.function()));
    }

    JasmineOperand generateGetSlice(GenerationContext& genCtx, JasmineBuilder builder, Type baseType, JasmineOperand aggregate, maybe<JasmineOperand> low, maybe<JasmineOperand> high, Type* sliceType = nullptr);

    JasmineOperand coerce(GenerationContext& genCtx, JasmineBuilder builder, Type destType, Type srcType, JasmineOperand operand) {
        if (destType.index == JASMINE_TYPE_VOID.index)
            return operand;
        if (destType != srcType) {
            JasmineOperand result;
            switch (destType.kind()) {
                case TypeKind::Void:
                    result = operand;
                    break;

                case TypeKind::Union: {
                    assert(isCase(srcType));
                    result = genCtx.temp();
                    auto loweredUnion = genCtx.lower(destType);
                    jasmine_append_var(builder, loweredUnion, result);
                    auto ptr = genCtx.temp();
                    jasmine_append_addr(builder, loweredUnion, ptr, result);
                    jasmine_append_store(builder, genCtx.lower(srcType), ptr, operand);
                    break;
                }

                case TypeKind::Pointer:
                    result = operand;
                    break;

                case TypeKind::Slice:
                    if (srcType.is<TypeKind::Slice>()) {
                        result = operand;
                        break;
                    }
                    return generateGetSlice(genCtx, builder, srcType, operand, none<JasmineOperand>(), none<JasmineOperand>());

                default:
                    result = genCtx.temp();
                    jasmine_append_convert(builder, genCtx.lower(destType), result, genCtx.lower(srcType), operand);
                    break;
            }
            return result;
        }
        return operand;
    }

    JasmineOperand generate(GenerationContext& genCtx, JasmineBuilder builder, AST ast, Type destType);

    JasmineOperand generateUnary(GenerationContext& genCtx, JasmineBuilder builder, AST ast, Type destType, void (*builder_func)(JasmineBuilder, JasmineType, JasmineOperand, JasmineOperand)) {
        JasmineType resultType = genCtx.lower(typeOf(ast));
        JasmineOperand value = generate(genCtx, builder, ast.child(0), typeOf(ast));
        JasmineOperand result = genCtx.temp();
        builder_func(builder, resultType, result, value);
        return coerce(genCtx, builder, destType, typeOf(ast), result);
    }

    JasmineOperand generateBinary(GenerationContext& genCtx, JasmineBuilder builder, AST ast, Type destType, void (*builder_func)(JasmineBuilder, JasmineType, JasmineOperand, JasmineOperand, JasmineOperand)) {
        JasmineType resultType = genCtx.lower(typeOf(ast));
        JasmineOperand lhs = generate(genCtx, builder, ast.child(0), typeOf(ast));
        JasmineOperand rhs = generate(genCtx, builder, ast.child(1), typeOf(ast));
        JasmineOperand result = genCtx.temp();
        builder_func(builder, resultType, result, lhs, rhs);
        return coerce(genCtx, builder, destType, typeOf(ast), result);
    }

    JasmineOperand generateCompare(GenerationContext& genCtx, JasmineBuilder builder, AST ast, Type destType, void (*builder_func)(JasmineBuilder, JasmineType, JasmineOperand, JasmineOperand, JasmineOperand)) {
        Type operandType = typeOf(ast, 0);
        JasmineType loweredType = genCtx.lower(operandType);
        JasmineOperand lhs = generate(genCtx, builder, ast.child(0), operandType);
        JasmineOperand rhs = generate(genCtx, builder, ast.child(1), operandType);
        JasmineOperand result = genCtx.temp();
        builder_func(builder, loweredType, result, lhs, rhs);
        assert(typeOf(ast) == ast.module->boolType());
        return result;
    }

    Type typeForExp(Type type) {
        auto numType = type.as<TypeKind::Numeric>();
        if (numType.isFloat())
            return type.types->get(F64);
        if (numType.isSigned())
            return type.types->get(I64);
        return type.types->get(U64);
    }

    struct CallExpInfo {
        Type type;
        JasmineType funcType;
        JasmineOperand callee;
    };

    CallExpInfo getCallExpInfo(GenerationContext& genCtx, Type expType) {
        auto numType = expType.as<TypeKind::Numeric>();
        if (numType.isFloat())
            return { expType.types->get(F64), genCtx.getFpowType(), genCtx.funcref(cstring("math.pow(f64)")) };
        if (numType.isSigned())
            return { expType.types->get(I64), genCtx.getIpowType(), genCtx.funcref(cstring("math.pow(i64)")) };
        return { expType.types->get(U64), genCtx.getUpowType(), genCtx.funcref(cstring("math.pow(u64)")) };
    }

    JasmineOperand generateExp(GenerationContext& genCtx, JasmineBuilder builder, AST lhs, AST rhs, Type expType, Type destType) {
        Module* module = lhs.module;

        auto result = genCtx.temp();
        CallExpInfo info = getCallExpInfo(genCtx, expType);
        JasmineOperand args[2];
        args[0] = generate(genCtx, builder, lhs, info.type);
        args[1] = generate(genCtx, builder, rhs, info.type);
        jasmine_append_call(builder, info.funcType, result, info.callee, args, 2);
        return coerce(genCtx, builder, destType, info.type, result);
    }

    JasmineOperand generateExpAssignment(GenerationContext& genCtx, JasmineBuilder builder, AST ast) {
        Type pointerType = typeOf(ast, 0);
        assert(pointerType.is<TypeKind::Pointer>());
        Type elementType = pointerType.as<TypeKind::Pointer>().elementType();
        JasmineType resultType = genCtx.lower(elementType);

        CallExpInfo info = getCallExpInfo(genCtx, elementType);
        Type varType = typeOf(ast.child(0), 0);
        bool lhsNeedsCoerce = varType != info.type;
        if (ast.child(0).kind() == ASTKind::AddressOf && ast.child(0).child(0).kind() == ASTKind::Local) {
             // In this specific (but probably extremely common case) we want to
            // avoid memory operations and turn this into a single instruction.
            JasmineOperand var = genCtx.local(ast.child(0).child(0).variable());
            JasmineOperand args[2];
            args[0] = var;
            args[1] = generate(genCtx, builder, ast.child(1), info.type);
            if (lhsNeedsCoerce) {
                args[0] = genCtx.temp();
                jasmine_append_convert(builder, genCtx.lower(info.type), args[0], genCtx.lower(varType), var);
            }
            jasmine_append_call(builder, info.funcType, args[0], info.callee, args, 2);
            if (lhsNeedsCoerce)
                jasmine_append_convert(builder, genCtx.lower(varType), var, genCtx.lower(info.type), args[0]);
        } else {
            JasmineOperand lhs = generate(genCtx, builder, ast.child(0), typeOf(ast, 0));
            JasmineOperand args[2];
            JasmineOperand varTemp = genCtx.temp();
            args[0] = varTemp;
            args[1] = generate(genCtx, builder, ast.child(1), elementType);
            jasmine_append_load(builder, resultType, args[0], lhs);
            if (lhsNeedsCoerce) {
                args[0] = genCtx.temp(); // New temp for wider size.
                jasmine_append_convert(builder, genCtx.lower(info.type), args[0], genCtx.lower(varType), varTemp);
            }
            jasmine_append_call(builder, info.funcType, args[0], info.callee, args, 2);
            if (lhsNeedsCoerce)
                jasmine_append_convert(builder, genCtx.lower(varType), varTemp, genCtx.lower(info.type), args[0]);
            jasmine_append_store(builder, resultType, lhs, varTemp);
        }
        return JASMINE_INVALID_OPERAND;
    }

    JasmineOperand generateCompoundAssignment(GenerationContext& genCtx, JasmineBuilder builder, AST ast, void (*builder_func)(JasmineBuilder, JasmineType, JasmineOperand, JasmineOperand, JasmineOperand)) {
        Type pointerType = typeOf(ast, 0);
        assert(pointerType.is<TypeKind::Pointer>());
        Type elementType = expand(pointerType.as<TypeKind::Pointer>().elementType());
        JasmineType resultType = genCtx.lower(elementType);

        if (ast.child(0).kind() == ASTKind::AddressOf && ast.child(0).child(0).kind() == ASTKind::Local) {
            // In this specific (but probably extremely common case) we want to
            // avoid memory operations and turn this into a single instruction.
            JasmineOperand lhs = genCtx.local(ast.child(0).child(0).variable());
            JasmineOperand rhs = generate(genCtx, builder, ast.child(1), elementType);
            builder_func(builder, resultType, lhs, lhs, rhs);
        } else {
            JasmineOperand lhs = generate(genCtx, builder, ast.child(0), typeOf(ast, 0));
            JasmineOperand rhs = generate(genCtx, builder, ast.child(1), elementType);
            JasmineOperand temp = genCtx.temp();
            jasmine_append_load(builder, resultType, temp, lhs);
            builder_func(builder, resultType, temp, temp, rhs);
            jasmine_append_store(builder, resultType, lhs, temp);
        }
        return JASMINE_INVALID_OPERAND;
    }

    JasmineOperand generateIncrDecr(GenerationContext& genCtx, JasmineBuilder builder, Type destType, AST ast) {
        auto binOp = ast.kind() == ASTKind::PreIncr || ast.kind() == ASTKind::PostIncr
            ? jasmine_append_add
            : jasmine_append_sub;
        bool isPre = ast.kind() == ASTKind::PreIncr || ast.kind() == ASTKind::PreDecr;

        if (ast.child(0).kind() == ASTKind::AddressOf && ast.child(0).child(0).kind() == ASTKind::Local) {
            // Again, in this common case, we avoid memory operations and
            // operate directly on the local.
            JasmineType varType = genCtx.lower(typeOf(ast.child(0), 0));
            JasmineOperand base = genCtx.local(ast.child(0).child(0).variable());
            if (isPre) {
                binOp(builder, varType, base, base, genCtx.imm(1));
                return coerce(genCtx, builder, destType, typeOf(ast.child(0), 0), base);
            } else {
                auto result = genCtx.temp();
                jasmine_append_mov(builder, varType, result, base);
                binOp(builder, varType, base, base, genCtx.imm(1));
                return coerce(genCtx, builder, destType, typeOf(ast.child(0), 0), result);
            }
        } else {
            Type ptrType = typeOf(ast, 0);
            Type elementType = expand(ptrType.as<TypeKind::Pointer>().elementType());
            JasmineType varType = genCtx.lower(elementType);
            JasmineOperand base = generate(genCtx, builder, ast.child(0), ptrType);
            JasmineOperand temp = genCtx.temp();
            jasmine_append_load(builder, varType, temp, base);
            if (isPre) {
                binOp(builder, varType, temp, temp, genCtx.imm(1));
                jasmine_append_store(builder, varType, base, temp);
                return coerce(genCtx, builder, destType, elementType, temp);
            } else {
                JasmineOperand result = genCtx.temp();
                jasmine_append_mov(builder, varType, result, temp);
                binOp(builder, varType, temp, temp, genCtx.imm(1));
                jasmine_append_store(builder, varType, base, temp);
                return coerce(genCtx, builder, destType, elementType, result);
            }
        }
    }

    ASTKind negateCondition(ASTKind kind) {
        switch (kind) {
            case ASTKind::Bool:
                return ASTKind::Bool;
            case ASTKind::Less:
                return ASTKind::GreaterEq;
            case ASTKind::LessEq:
                return ASTKind::Greater;
            case ASTKind::Equal:
                return ASTKind::NotEqual;
            case ASTKind::NotEqual:
                return ASTKind::Equal;
            case ASTKind::Greater:
                return ASTKind::LessEq;
            case ASTKind::GreaterEq:
                return ASTKind::Less;
            case ASTKind::And:
                return ASTKind::Or;
            case ASTKind::Or:
                return ASTKind::And;
            case ASTKind::Not:
                return ASTKind::Missing;
            default:
                return ASTKind::Local;
        }
    }

    void generateConditionalBranch(GenerationContext& genCtx, JasmineBuilder builder, AST cond, JasmineBlock ifTrue, JasmineBlock ifFalse, bool negate) {
        JasmineOperand lhs, rhs;
        Type type;
        JasmineOperand condition;
        ASTKind knownCondOp = ASTKind::Missing;
        switch (cond.kind()) {
            case ASTKind::Less:
            case ASTKind::LessEq:
            case ASTKind::Equal:
            case ASTKind::NotEqual:
            case ASTKind::Greater:
            case ASTKind::GreaterEq:
                knownCondOp = cond.kind();
                type = typeOf(cond, 0);
                lhs = generate(genCtx, builder, cond.child(0), typeOf(cond, 0));
                rhs = generate(genCtx, builder, cond.child(1), typeOf(cond, 0));
                break;
            case ASTKind::And:
            case ASTKind::Or:
                knownCondOp = cond.kind();
                type = genCtx.module->boolType();
                break;
            case ASTKind::Not:
                knownCondOp = cond.kind();
                type = cond.module->boolType();
                break;
            case ASTKind::Bool:
                knownCondOp = cond.kind();
                type = cond.module->boolType();
                break;
            default:
                condition = generate(genCtx, builder, cond, genCtx.module->boolType());
                type = genCtx.module->boolType();
                break;
        }

        if (negate) {
            ASTKind condOpBefore = knownCondOp;
            knownCondOp = negateCondition(knownCondOp);
            if (knownCondOp == ASTKind::Missing) {
                // This happens when we negate a Not node - in order words, we
                // just want to generate a normal branch.
                return generateConditionalBranch(genCtx, builder, cond.child(0), ifTrue, ifFalse, false);
            }
            if (knownCondOp == ASTKind::Local || knownCondOp == ASTKind::Bool) {
                // We can't negate a boolean by swapping the condition, so we
                // instead need to actually just swap the target blocks.
                swap(ifTrue, ifFalse);
            }
        }

        auto currentBlock = jasmine_current_block(builder);

        switch (knownCondOp) {
            case ASTKind::Bool:
                jasmine_append_br(builder, (negate ? !cond.boolConst() : cond.boolConst()) ? genCtx.addEdge(currentBlock, ifTrue) : genCtx.addEdge(currentBlock, ifFalse));
                break;
            case ASTKind::Less:
                jasmine_append_br_lt(builder, genCtx.lower(type), lhs, rhs, genCtx.addEdge(currentBlock, ifTrue), genCtx.addEdge(currentBlock, ifFalse));
                break;
            case ASTKind::LessEq:
                jasmine_append_br_le(builder, genCtx.lower(type), lhs, rhs, genCtx.addEdge(currentBlock, ifTrue), genCtx.addEdge(currentBlock, ifFalse));
                break;
            case ASTKind::Equal:
                jasmine_append_br_eq(builder, genCtx.lower(type), lhs, rhs, genCtx.addEdge(currentBlock, ifTrue), genCtx.addEdge(currentBlock, ifFalse));
                break;
            case ASTKind::NotEqual:
                jasmine_append_br_ne(builder, genCtx.lower(type), lhs, rhs, genCtx.addEdge(currentBlock, ifTrue), genCtx.addEdge(currentBlock, ifFalse));
                break;
            case ASTKind::Greater:
                jasmine_append_br_gt(builder, genCtx.lower(type), lhs, rhs, genCtx.addEdge(currentBlock, ifTrue), genCtx.addEdge(currentBlock, ifFalse));
                break;
            case ASTKind::GreaterEq:
                jasmine_append_br_ge(builder, genCtx.lower(type), lhs, rhs, genCtx.addEdge(currentBlock, ifTrue), genCtx.addEdge(currentBlock, ifFalse));
                break;
            case ASTKind::Not:
                generateConditionalBranch(genCtx, builder, cond.child(0), ifTrue, ifFalse, true);
                break;
            case ASTKind::And: {
                JasmineBlock continuation = genCtx.addBlock();
                generateConditionalBranch(genCtx, builder, cond.child(0), ifFalse, continuation, !negate);
                jasmine_builder_set_block(builder, continuation);
                generateConditionalBranch(genCtx, builder, cond.child(1), ifFalse, ifTrue, !negate);
                break;
            }
            case ASTKind::Or: {
                JasmineBlock continuation = genCtx.addBlock();
                generateConditionalBranch(genCtx, builder, cond.child(0), ifTrue, continuation, negate);
                jasmine_builder_set_block(builder, continuation);
                generateConditionalBranch(genCtx, builder, cond.child(1), ifTrue, ifFalse, negate);
                break;
            }
            default:
                jasmine_append_br_if(builder, genCtx.lower(type), condition, genCtx.addEdge(currentBlock, ifTrue), genCtx.addEdge(currentBlock, ifFalse));
                break;
        }
    }

    triple<Type, JasmineOperand, bool> materializeAggregateBase(GenerationContext& genCtx, JasmineBuilder builder, Type baseType, JasmineOperand base) {
        bool isPointer = baseType.is<TypeKind::Pointer>();
        Type aggregateType = isPointer ? expand(baseType.as<TypeKind::Pointer>().elementType()) : baseType;
        if (jasmine_is_static_ref(base)) { // Base is a global, implicitly a pointer.
            if (isPointer) { // Global pointer needs double dereference.
                JasmineOperand ptr = genCtx.temp();
                jasmine_append_load(builder, JASMINE_TYPE_PTR, ptr, base);
                base = ptr;
            }
            isPointer = true;
        }
        return { aggregateType, base, isPointer };
    }

    triple<JasmineType, Type, u32> computeFieldAccessTypes(GenerationContext& genCtx, Type aggregateType, u32 fieldId) {
        if (aggregateType.is<TypeKind::Struct>()) {
            auto structType = aggregateType.as<TypeKind::Struct>();
            auto fieldType = expand(structType.fieldType(fieldId));
            if (structType.isCase())
                fieldId ++; // Account for tag field.
            return { genCtx.lower(aggregateType), fieldType, fieldId };
        } else if (aggregateType.is<TypeKind::Named>()) {
            // Invalid in normal GetField, but fine here since we're just using
            // it as a codegen helper.
            auto namedType = aggregateType.as<TypeKind::Named>();
            auto fieldType = expand(namedType.innerType());
            if (namedType.isCase())
                fieldId ++;
            return { genCtx.lower(aggregateType), fieldType, fieldId };
        } else if (aggregateType.is<TypeKind::Tuple>()) {
            auto tupleType = aggregateType.as<TypeKind::Tuple>();
            auto fieldType = expand(tupleType.fieldType(fieldId));
            return { genCtx.lower(aggregateType), fieldType, fieldId };
        } else
            unreachable("Invalid type ", aggregateType, " for field access base.");
    }

    JasmineOperand generateGetTag(GenerationContext& genCtx, JasmineBuilder builder, Type baseType, JasmineOperand aggregate, Type destType) {
        const auto [aggregateType, base, isPointer] = materializeAggregateBase(genCtx, builder, baseType, aggregate);
        const auto [loweredAggregate, fieldType, _] = computeFieldAccessTypes(genCtx, aggregateType, 0);

        JasmineOperand result = genCtx.temp();
        if (isPointer) {
            jasmine_append_load_field(builder, loweredAggregate, result, base, 0);
            return coerce(genCtx, builder, destType, genCtx.module->u32Type(), result);
        }
        jasmine_append_get_field(builder, loweredAggregate, result, base, 0);
        return coerce(genCtx, builder, destType, genCtx.module->u32Type(), result);
    }

    JasmineOperand generateGetField(GenerationContext& genCtx, JasmineBuilder builder, Type baseType, JasmineOperand aggregate, u32 fieldId, Type destType) {
        const auto [aggregateType, base, isPointer] = materializeAggregateBase(genCtx, builder, baseType, aggregate);
        const auto [loweredAggregate, fieldType, id] = computeFieldAccessTypes(genCtx, aggregateType, fieldId);

        JasmineOperand result = genCtx.temp();
        if (isPointer) {
            jasmine_append_load_field(builder, loweredAggregate, result, base, id);
            return coerce(genCtx, builder, destType, fieldType, result);
        }
        jasmine_append_get_field(builder, loweredAggregate, result, base, id);
        return coerce(genCtx, builder, destType, fieldType, result);
    }

    JasmineOperand generateAddrField(GenerationContext& genCtx, JasmineBuilder builder, Type baseType, JasmineOperand aggregate, u32 fieldId) {
        const auto [aggregateType, base, isPointer] = materializeAggregateBase(genCtx, builder, baseType, aggregate);
        const auto [loweredAggregate, fieldType, id] = computeFieldAccessTypes(genCtx, aggregateType, fieldId);

        JasmineOperand result = genCtx.temp();
        if (isPointer) {
            jasmine_append_offset_field(builder, loweredAggregate, result, base, id);
            return result;
        }
        jasmine_append_addr_field(builder, loweredAggregate, result, base, id);
        return result;
    }

    Type getFieldType(Type baseType, u32 fieldId) {
        bool isPointer = baseType.is<TypeKind::Pointer>();
        Type aggregateType = isPointer ? expand(baseType.as<TypeKind::Pointer>().elementType()) : baseType;
        if (aggregateType.is<TypeKind::Struct>())
            return expand(aggregateType.as<TypeKind::Struct>().fieldType(fieldId));
        else if (aggregateType.is<TypeKind::Named>())
            return expand(aggregateType.as<TypeKind::Named>().innerType());
        else if (aggregateType.is<TypeKind::Tuple>())
            return expand(aggregateType.as<TypeKind::Tuple>().fieldType(fieldId));
        else
            unreachable("Invalid type ", aggregateType, " for field access base.");
    }

    void generateSetField(GenerationContext& genCtx, JasmineBuilder builder, Type baseType, JasmineOperand aggregate, u32 fieldId, JasmineOperand value) {
        const auto [aggregateType, base, isPointer] = materializeAggregateBase(genCtx, builder, baseType, aggregate);
        const auto [loweredAggregate, fieldType, id] = computeFieldAccessTypes(genCtx, aggregateType, fieldId);
        if (isPointer) {
            jasmine_append_store_field(builder, loweredAggregate, base, id, value);
            return;
        }
        jasmine_append_set_field(builder, loweredAggregate, base, id, value);
    }

    JasmineOperand generateLength(GenerationContext& genCtx, JasmineBuilder builder, Type baseType, JasmineOperand base, Type destType) {
        bool isPointer = baseType.is<TypeKind::Pointer>();
        Type aggregateType = isPointer ? expand(baseType.as<TypeKind::Pointer>().elementType()) : baseType;
        if (aggregateType.is<TypeKind::Array>())
            return genCtx.imm(aggregateType.as<TypeKind::Array>().length());
        assert(aggregateType.is<TypeKind::Slice>());

        if (jasmine_is_static_ref(base)) { // Base is a global, implicitly a pointer.
            if (isPointer) { // Global pointer needs double dereference.
                JasmineOperand ptr = genCtx.temp();
                jasmine_append_load(builder, JASMINE_TYPE_PTR, ptr, base);
                base = ptr;
            }
            isPointer = true;
        }
        JasmineOperand result = genCtx.temp();
        if (isPointer)
            jasmine_append_load_field(builder, genCtx.lower(baseType), result, base, 1);
        else
            jasmine_append_get_field(builder, genCtx.lower(baseType), result, base, 1);
        return coerce(genCtx, builder, destType, genCtx.module->u64Type(), result);
    }

    JasmineOperand generateGetIndex(GenerationContext& genCtx, JasmineBuilder builder, Type baseType, JasmineOperand aggregate, JasmineOperand index, Type destType) {
        const auto [aggregateType, base, isPointer] = materializeAggregateBase(genCtx, builder, baseType, aggregate);

        JasmineOperand result = genCtx.temp();
        if (aggregateType.is<TypeKind::Array>()) {
            auto elementType = expand(aggregateType.as<TypeKind::Array>().elementType());
            auto loweredElement = genCtx.lower(elementType);
            if (isPointer)
                jasmine_append_load_index(builder, loweredElement, result, base, JASMINE_TYPE_U64, index);
            else
                jasmine_append_get_index(builder, loweredElement, result, base, JASMINE_TYPE_U64, index);
            return coerce(genCtx, builder, destType, elementType, result);
        } else if (aggregateType.is<TypeKind::Slice>()) {
            auto elementType = expand(aggregateType.as<TypeKind::Slice>().elementType());
            auto loweredElement = genCtx.lower(elementType);
            JasmineOperand ptr = genCtx.temp();
            if (isPointer)
                jasmine_append_load_field(builder, genCtx.lower(aggregateType), ptr, base, 0);
            else
                jasmine_append_get_field(builder, genCtx.lower(aggregateType), ptr, base, 0);
            jasmine_append_load_index(builder, loweredElement, result, ptr, JASMINE_TYPE_U64, index);
            return coerce(genCtx, builder, destType, elementType, result);
        } else
            unreachable("Invalid base type ", baseType, " for indexed access.");
    }

    JasmineOperand generateAddrIndex(GenerationContext& genCtx, JasmineBuilder builder, Type baseType, JasmineOperand aggregate, JasmineOperand index) {
        const auto [aggregateType, base, isPointer] = materializeAggregateBase(genCtx, builder, baseType, aggregate);

        JasmineOperand result = genCtx.temp();
        if (aggregateType.is<TypeKind::Array>()) {
            auto loweredElement = genCtx.lower(expand(aggregateType.as<TypeKind::Array>().elementType()));
            if (isPointer)
                jasmine_append_offset_index(builder, loweredElement, result, base, JASMINE_TYPE_U64, index);
            else
                jasmine_append_addr_index(builder, loweredElement, result, base, JASMINE_TYPE_U64, index);
            return result;
        } else if (aggregateType.is<TypeKind::Slice>()) {
            auto loweredElement = genCtx.lower(expand(aggregateType.as<TypeKind::Slice>().elementType()));
            JasmineOperand ptr = genCtx.temp();
            if (isPointer)
                jasmine_append_load_field(builder, genCtx.lower(aggregateType), ptr, base, 0);
            else
                jasmine_append_get_field(builder, genCtx.lower(aggregateType), ptr, base, 0);
            jasmine_append_offset_index(builder, loweredElement, result, ptr, JASMINE_TYPE_U64, index);
            return result;
        } else
            unreachable("Invalid base type ", baseType, " for indexed access.");
    }

    void generateSetIndex(GenerationContext& genCtx, JasmineBuilder builder, Type baseType, JasmineOperand aggregate, JasmineOperand index, AST src) {
        const auto [aggregateType, base, isPointer] = materializeAggregateBase(genCtx, builder, baseType, aggregate);

        if (aggregateType.is<TypeKind::Array>()) {
            auto elementType = expand(aggregateType.as<TypeKind::Array>().elementType());
            auto loweredElement = genCtx.lower(elementType);
            JasmineOperand value = generate(genCtx, builder, src, elementType);
            if (isPointer)
                jasmine_append_store_index(builder, loweredElement, base, JASMINE_TYPE_U64, index, value);
            else
                jasmine_append_set_index(builder, loweredElement, base, JASMINE_TYPE_U64, index, value);
        } else if (aggregateType.is<TypeKind::Slice>()) {
            auto elementType = expand(aggregateType.as<TypeKind::Slice>().elementType());
            auto loweredElement = genCtx.lower(elementType);
            JasmineOperand value = generate(genCtx, builder, src, elementType);
            JasmineOperand ptr = genCtx.temp();
            if (isPointer)
                jasmine_append_load_field(builder, genCtx.lower(aggregateType), ptr, base, 0);
            else
                jasmine_append_get_field(builder, genCtx.lower(aggregateType), ptr, base, 0);
            jasmine_append_store_index(builder, loweredElement, ptr, JASMINE_TYPE_U64, index, value);
        } else
            unreachable("Invalid base type ", baseType, " for indexed access.");
    }

    JasmineOperand generateGetSlice(GenerationContext& genCtx, JasmineBuilder builder, Type baseType, JasmineOperand aggregate, maybe<JasmineOperand> low, maybe<JasmineOperand> high, Type* type) {
        const auto [aggregateType, base, isPointer] = materializeAggregateBase(genCtx, builder, baseType, aggregate);

        JasmineOperand addr, size;
        JasmineType loweredSlice;
        if (aggregateType.is<TypeKind::Array>()) {
            auto arrayType = aggregateType.as<TypeKind::Array>();
            auto elementType = expand(arrayType.elementType());
            auto loweredElement = genCtx.lower(elementType);
            if (type)
                *type = genCtx.module->sliceType(elementType);
            loweredSlice = genCtx.lower(genCtx.module->sliceType(elementType));

            if (!low && !high) {
                addr = genCtx.temp();
                if (isPointer)
                    jasmine_append_mov(builder, JASMINE_TYPE_PTR, addr, base);
                else
                    jasmine_append_addr(builder, JASMINE_TYPE_PTR, addr, base);
                size = jasmine_imm(jasmine_current_function(builder), arrayType.length());
            } else if (!low) {
                addr = genCtx.temp();
                if (isPointer)
                    jasmine_append_mov(builder, JASMINE_TYPE_PTR, addr, base);
                else
                    jasmine_append_addr(builder, JASMINE_TYPE_PTR, addr, base);
                size = *high;
            } else if (!high) {
                addr = genCtx.temp(), size = genCtx.temp();
                if (isPointer)
                    jasmine_append_offset_index(builder, loweredElement, addr, base, genCtx.sizeType(), *low);
                else
                    jasmine_append_addr_index(builder, loweredElement, addr, base, genCtx.sizeType(), *low);
                jasmine_append_sub(builder, genCtx.sizeType(), size, jasmine_imm(jasmine_current_function(builder), arrayType.length()), *low);
            } else {
                addr = genCtx.temp(), size = genCtx.temp();
                if (isPointer)
                    jasmine_append_offset_index(builder, loweredElement, addr, base, genCtx.sizeType(), *low);
                else
                    jasmine_append_addr_index(builder, loweredElement, addr, base, genCtx.sizeType(), *low);
                jasmine_append_sub(builder, genCtx.sizeType(), size, *high, *low);
            }
        } else if (aggregateType.is<TypeKind::Slice>()) {
            auto sliceType = aggregateType.as<TypeKind::Slice>();
            if (type)
                *type = sliceType;
            auto elementType = expand(sliceType.elementType());
            loweredSlice = genCtx.lower(sliceType);
            auto loweredElement = genCtx.lower(elementType);

            if (!low && !high) {
                addr = genCtx.temp(), size = genCtx.temp();
                if (isPointer) {
                    jasmine_append_load_field(builder, loweredSlice, addr, base, 0);
                    jasmine_append_load_field(builder, loweredSlice, size, base, 1);
                } else {
                    jasmine_append_get_field(builder, loweredSlice, addr, base, 0);
                    jasmine_append_get_field(builder, loweredSlice, size, base, 1);
                }
            } else if (!low) {
                addr = genCtx.temp();
                if (isPointer)
                    jasmine_append_load_field(builder, loweredSlice, addr, base, 0);
                else
                    jasmine_append_get_field(builder, loweredSlice, addr, base, 0);
                size = *high;
            } else if (!high) {
                addr = genCtx.temp(), size = genCtx.temp();
                if (isPointer) {
                    jasmine_append_load_field(builder, loweredSlice, addr, base, 0);
                    jasmine_append_load_field(builder, loweredSlice, size, base, 1);
                } else {
                    jasmine_append_get_field(builder, loweredSlice, addr, base, 0);
                    jasmine_append_get_field(builder, loweredSlice, size, base, 1);
                }
                jasmine_append_offset_index(builder, loweredElement, addr, addr, genCtx.sizeType(), *low);
                jasmine_append_sub(builder, genCtx.sizeType(), size, size, *low);
            } else {
                addr = genCtx.temp(), size = genCtx.temp();
                if (isPointer)
                    jasmine_append_load_field(builder, loweredSlice, addr, base, 0);
                else
                    jasmine_append_get_field(builder, loweredSlice, addr, base, 0);
                jasmine_append_offset_index(builder, loweredElement, addr, addr, genCtx.sizeType(), *low);
                jasmine_append_sub(builder, genCtx.sizeType(), size, *high, *low);
            }
        }

        auto result = genCtx.temp();
        jasmine_append_set_field(builder, loweredSlice, result, 0, addr);
        jasmine_append_set_field(builder, loweredSlice, result, 1, size);

        // TODO: This might need coercing, but it's unclear since GetSlice
        // should always return a slice.
        return result;
    }

    void generatePattern(GenerationContext& genCtx, JasmineBuilder builder, AST pattern, Type inputType, JasmineOperand input, JasmineBlock fail) {
        // Either this function branches to the provided failure block, or it
        // leaves control at the current tip of instruction generation where we
        // expect to enter the case body.

        if (pattern.missing())
            return;

        if UNLIKELY(config::jasmineASTComments) {
            i8 buffer[1024];
            slice<i8> output = { buffer, 1024 };
            for (u32 i = 0; i < genCtx.expressionDepth; i ++)
                output = format(output, ' ');
            output = format(output, "(pattern ", ASTWithDepth { pattern, 2 }, ")");
            jasmine_append_comment(builder, buffer, 1024 - output.size());
        }
        ExpressionDepthScope expressionDepth(genCtx, config::jasmineASTComments);

        auto getLValue = [&](AST pattern) -> JasmineOperand {
            assert(pattern.kind() == ASTKind::VarDecl);
            auto varType = genCtx.lower(typeOf(pattern));
            if (pattern.child(1).kind() == ASTKind::Local) {
                auto result = genCtx.temp();
                jasmine_append_addr(builder, varType, result, genCtx.local(pattern.child(1).variable()));
                return result;
            } else {
                assert(pattern.child(1).kind() == ASTKind::Global);
                auto varInfo = pattern.child(1).varInfo();
                auto name = pattern.module->str(varInfo.name);
                jasmine_define_static_uninit(genCtx.output, varType, name.data(), name.size());
                return genCtx.staticref(name);
            }
        };

        JasmineBlock continuation;
        switch (pattern.kind()) {
            case ASTKind::Bool: {
                continuation = genCtx.addBlock();
                jasmine_append_br_if(builder, genCtx.lower(inputType), input, genCtx.addEdgeTo(continuation), genCtx.addEdgeTo(fail));
                jasmine_builder_set_block(builder, continuation);
                break;
            }

            case ASTKind::Float: {
                continuation = genCtx.addBlock();
                auto imm = expand(inputType) == F32 ? genCtx.f32imm(pattern.floatConst()) : genCtx.f64imm(pattern.floatConst());
                jasmine_append_br_eq(builder, genCtx.lower(inputType), input, imm, genCtx.addEdgeTo(continuation), genCtx.addEdgeTo(fail));
                jasmine_builder_set_block(builder, continuation);
                break;
            }

            case ASTKind::Int:
                continuation = genCtx.addBlock();
                jasmine_append_br_eq(builder, genCtx.lower(inputType), input, genCtx.imm(pattern.intConst()), genCtx.addEdgeTo(continuation), genCtx.addEdgeTo(fail));
                jasmine_builder_set_block(builder, continuation);
                break;

            case ASTKind::Unsigned:
                continuation = genCtx.addBlock();
                jasmine_append_br_eq(builder, genCtx.lower(inputType), input, genCtx.imm(pattern.uintConst()), genCtx.addEdgeTo(continuation), genCtx.addEdgeTo(fail));
                jasmine_builder_set_block(builder, continuation);
                break;

            case ASTKind::Char:
                continuation = genCtx.addBlock();
                jasmine_append_br_eq(builder, genCtx.lower(inputType), input, genCtx.imm(pattern.charConst()), genCtx.addEdgeTo(continuation), genCtx.addEdgeTo(fail));
                jasmine_builder_set_block(builder, continuation);
                break;

            case ASTKind::VarDecl: {
                auto varType = genCtx.lower(typeOf(pattern));
                auto coercedInput = coerce(genCtx, builder, typeOf(pattern), inputType, input);
                if (pattern.child(1).kind() == ASTKind::Local)
                    jasmine_append_mov(builder, varType, genCtx.local(pattern.child(1).variable()), coercedInput);
                else {
                    assert(pattern.child(1).kind() == ASTKind::Global);
                    auto varInfo = pattern.child(1).varInfo();
                    auto name = pattern.module->str(varInfo.name);
                    jasmine_define_static_uninit(genCtx.output, varType, name.data(), name.size());
                    jasmine_append_store(builder, varType, genCtx.global(pattern.child(1).variable()), coercedInput);
                }
                break;
            }

            case ASTKind::Splat:
                unreachable("Should have handled splat in advance from parent pattern to avoid unnecessary copying.");

            case ASTKind::Construct: {
                // We know the input matches this pattern from earlier. But
                // how we handle it depends on exactly what the input type is.
                auto baseType = expand(inputType);
                if (baseType.is<TypeKind::Pointer>())
                    baseType = expand(baseType.as<TypeKind::Pointer>().elementType());
                switch (baseType.kind()) {
                    case TypeKind::Union: {
                        // Really we want to dispatch to one of our cases. We
                        // implement this by recursively calling
                        // generatePattern again once we've isolated the right
                        // subtype.

                        Type patternType = typeOf(pattern);
                        JasmineType loweredPattern = genCtx.lower(patternType);

                        u32 id;
                        if (patternType.is<TypeKind::Struct>())
                            id = patternType.as<TypeKind::Struct>().typeTag();
                        else if (patternType.is<TypeKind::Named>())
                            id = patternType.as<TypeKind::Named>().typeTag();
                        else
                            unreachable("Unions can only match against specific struct or named type members.");

                        JasmineOperand ptr = input;
                        if (!expand(inputType).is<TypeKind::Pointer>()) {
                            ptr = genCtx.temp();
                            jasmine_append_addr(builder, loweredPattern, ptr, input);
                        }
                        auto tag = genCtx.temp();
                        jasmine_append_load_field(builder, loweredPattern, tag, ptr, 0); // Tag is always the first field.
                        continuation = genCtx.addBlock();
                        jasmine_append_br_ne(builder, genCtx.tagType(), tag, genCtx.imm(id), genCtx.addEdgeTo(fail), genCtx.addEdgeTo(continuation));
                        jasmine_builder_set_block(builder, continuation);
                        generatePattern(genCtx, builder, pattern, genCtx.module->ptrType(patternType), ptr, fail);
                        break;
                    }

                    case TypeKind::Struct: {
                        // Either we statically match, or we don't do anything.
                        if (typeOf(pattern) != baseType) {
                            continuation = genCtx.addBlock();
                            jasmine_append_br(builder, genCtx.addEdgeTo(fail));
                            genCtx.isUnreachable = true;
                            break;
                        }

                        auto structType = baseType.as<TypeKind::Struct>();
                        for (u32 i = 0; i < structType.count(); i ++) {
                            if (pattern.child(i).kind() == ASTKind::Splat) {
                                auto splatType = typeOf(pattern.child(i));
                                if (splatType.is<TypeKind::Void>())
                                    break;
                                auto loweredStruct = genCtx.lower(structType);
                                auto loweredSplat = genCtx.lower(splatType);
                                JasmineOperand tuple = getLValue(pattern.child(i).child(0));
                                for (u32 j = i; j < structType.count(); j ++) {
                                    auto field = generateGetField(genCtx, builder, inputType, input, j, structType.fieldType(j));
                                    jasmine_append_store_field(builder, loweredSplat, tuple, j - i, field);
                                }
                                break;
                            }
                            auto field = generateGetField(genCtx, builder, inputType, input, i, structType.fieldType(i));
                            generatePattern(genCtx, builder, pattern.child(i), structType.fieldType(i), field, fail);
                        }
                        break;
                    }

                    case TypeKind::Named: {
                        // Either we statically match, or we don't do anything.
                        if (typeOf(pattern) != baseType) {
                            jasmine_append_br(builder, genCtx.addEdgeTo(fail));
                            genCtx.isUnreachable = true;
                            break;
                        }

                        auto namedType = baseType.as<TypeKind::Named>();
                        auto field = generateGetField(genCtx, builder, inputType, input, 0, namedType.innerType());
                        generatePattern(genCtx, builder, pattern.child(0), namedType.innerType(), field, fail);
                        break;
                    }

                    default:
                        unreachable("Invalid type ", baseType, " for constructor pattern.");
                }

                break;
            }

            case ASTKind::List: {
                auto sliceType = genCtx.lower(typeOf(pattern));
                auto minLength = pattern.arity();
                i64 maxLength = pattern.arity();
                for (u32 i = 0; i < pattern.arity(); i ++) if (pattern.child(i).kind() == ASTKind::Splat)
                    minLength --, maxLength = -1;
                auto baseType = inputType;
                if (baseType.is<TypeKind::Pointer>())
                    baseType = expand(baseType.as<TypeKind::Pointer>().elementType());
                Type elementType;
                JasmineOperand length;
                if (baseType.is<TypeKind::Array>()) {
                    auto arrayType = baseType.as<TypeKind::Array>();
                    elementType = expand(arrayType.elementType());
                    if (arrayType.length() < minLength || (arrayType.length() > maxLength && maxLength != -1)) {
                        jasmine_append_br(builder, genCtx.addEdgeTo(fail));
                        return;
                    }
                    length = genCtx.imm(arrayType.length());
                } else {
                    assert(baseType.is<TypeKind::Slice>());
                    elementType = expand(baseType.as<TypeKind::Slice>().elementType());

                    length = generateLength(genCtx, builder, inputType, input, genCtx.module->u64Type());
                    if (minLength > 0 && maxLength == -1) {
                        continuation = genCtx.addBlock();
                        jasmine_append_br_lt(builder, genCtx.sizeType(), length, genCtx.imm(minLength), genCtx.addEdgeTo(fail), genCtx.addEdgeTo(continuation));
                        jasmine_builder_set_block(builder, continuation);
                    } else if (minLength > 0) {
                        auto adj = genCtx.temp();
                        jasmine_append_sub(builder, genCtx.sizeType(), adj, length, genCtx.imm(minLength));
                        continuation = genCtx.addBlock();
                        jasmine_append_br_gt(builder, genCtx.sizeType(), adj, genCtx.imm(maxLength - minLength), genCtx.addEdgeTo(fail), genCtx.addEdgeTo(continuation));
                        jasmine_builder_set_block(builder, continuation);
                    } else if (maxLength != -1) {
                        continuation = genCtx.addBlock();
                        jasmine_append_br_gt(builder, genCtx.sizeType(), length, genCtx.imm(maxLength), genCtx.addEdgeTo(fail), genCtx.addEdgeTo(continuation));
                        jasmine_builder_set_block(builder, continuation);
                    }
                    // Otherwise, we have a minLength of 0, and no maxLength, so we evidently accept any input size.
                }

                bool hasDynamicIndex = false;
                JasmineOperand index;
                for (u32 i = 0; i < pattern.arity(); i ++) {
                    if (pattern.child(i).kind() == ASTKind::Splat) {
                        auto splat = pattern.child(i);
                        if (i == pattern.arity() - 1) {
                            auto value = generateGetSlice(genCtx, builder, inputType, input, some<JasmineOperand>(hasDynamicIndex ? index : genCtx.imm(i)), none<JasmineOperand>());
                            generatePattern(genCtx, builder, splat.child(0), typeOf(splat, 0), value, fail);
                            break;
                        } else {
                            assert(i < pattern.arity() - 1);
                            AST nextPattern = pattern.child(i + 1);
                            if (!hasDynamicIndex) {
                                hasDynamicIndex = true;
                                index = genCtx.temp();
                                jasmine_append_mov(builder, genCtx.sizeType(), index, genCtx.imm(i));
                            }
                            i ++;
                            auto start = genCtx.temp(), end = genCtx.temp();
                            jasmine_append_mov(builder, genCtx.sizeType(), start, index);

                            JasmineBlock continuation = genCtx.addBlock(), loopEnd = genCtx.addBlock(), loop = genCtx.addBlock();
                            jasmine_append_br_ge(builder, genCtx.sizeType(), index, length, genCtx.addEdgeTo(fail), genCtx.addEdgeTo(continuation));
                            jasmine_builder_set_block(builder, continuation);

                            auto field = generateGetIndex(genCtx, builder, inputType, input, index, elementType);
                            generatePattern(genCtx, builder, nextPattern, elementType, field, loop);
                            jasmine_append_br(builder, genCtx.addEdgeTo(loopEnd));
                            jasmine_builder_set_block(builder, loop);

                            jasmine_append_add(builder, genCtx.sizeType(), index, index, genCtx.imm(1));
                            continuation = genCtx.addBlock();
                            jasmine_append_br_ge(builder, genCtx.sizeType(), index, length, genCtx.addEdgeTo(fail), genCtx.addEdgeTo(continuation));
                            jasmine_builder_set_block(builder, continuation);
                            field = generateGetIndex(genCtx, builder, inputType, input, index, elementType);
                            generatePattern(genCtx, builder, nextPattern, elementType, field, loop);
                            jasmine_append_br(builder, genCtx.addEdgeTo(loopEnd));

                            jasmine_builder_set_block(builder, loopEnd);
                            jasmine_append_mov(builder, genCtx.sizeType(), end, index);
                            jasmine_append_add(builder, genCtx.sizeType(), index, index, genCtx.imm(1)); // Need one more advance since we also matched the next pattern.
                            auto value = generateGetSlice(genCtx, builder, inputType, input, some<JasmineOperand>(start), some<JasmineOperand>(end));
                            generatePattern(genCtx, builder, splat.child(0), typeOf(splat, 0), value, fail);
                            continue;
                        }
                    }
                    auto field = generateGetIndex(genCtx, builder, inputType, input, hasDynamicIndex ? index : genCtx.imm(i), elementType);
                    if (hasDynamicIndex)
                        jasmine_append_add(builder, genCtx.sizeType(), index, index, genCtx.imm(1));
                    auto subpattern = pattern.child(i);
                    generatePattern(genCtx, builder, subpattern, elementType, field, fail);
                }

                break;
            }

            case ASTKind::Tuple: {
                auto baseType = inputType;
                if (baseType.is<TypeKind::Pointer>())
                    baseType = expand(baseType.as<TypeKind::Pointer>().elementType());
                auto tupleType = baseType.as<TypeKind::Tuple>();
                for (u32 i = 0; i < tupleType.count(); i ++) {
                    if (pattern.child(i).kind() == ASTKind::Splat) {
                        auto loweredTuple = genCtx.lower(tupleType);
                        JasmineOperand tuple = getLValue(pattern.child(i).child(0));
                        for (u32 j = i; j < tupleType.count(); j ++) {
                            auto temp = genCtx.temp();
                            auto field = generateGetField(genCtx, builder, inputType, input, j, tupleType.fieldType(j));
                            jasmine_append_store_field(builder, loweredTuple, tuple, j - i, field);
                        }
                        break;
                    }
                    auto field = generateGetField(genCtx, builder, inputType, input, i, tupleType.fieldType(i));
                    generatePattern(genCtx, builder, pattern.child(i), tupleType.fieldType(i), field, fail);
                }

                break;
            }

            case ASTKind::TypeField:
            case ASTKind::Typename:
            case ASTKind::GlobalTypename:
            case ASTKind::FunType:
            case ASTKind::TupleType:
            case ASTKind::PtrType:
            case ASTKind::SliceType:
            case ASTKind::OwnType:
            case ASTKind::UninitType:
            case ASTKind::ArrayType: {
                Type patternType = evaluateType(genCtx.module, genCtx.func(), pattern);

                bool allCases = true;
                if (patternType.is<TypeKind::Tuple>()) {
                    bool allCases = true;
                    auto tup = patternType.as<TypeKind::Tuple>();
                    for (u32 i = 0; i < tup.count(); i ++) if (!isCase(expand(tup.fieldType(i)))) {
                        allCases = false;
                        break;
                    }
                } else if (!isCase(patternType))
                    allCases = false;

                if (!allCases)
                    return; // Simple type matching always passes, as long as typechecking didn't fail.

                auto baseType = expand(inputType);
                if (baseType.is<TypeKind::Pointer>())
                    baseType = expand(baseType.as<TypeKind::Pointer>().elementType());
                if (baseType.is<TypeKind::Tuple>()) {
                    bool nontrivial = false;
                    for (u32 i = 0; i < baseType.as<TypeKind::Tuple>().count(); i ++) {
                        Type fieldType = expand(baseType.as<TypeKind::Tuple>().fieldType(i));
                        if (fieldType.is<TypeKind::Pointer>())
                            fieldType = expand(fieldType.as<TypeKind::Pointer>().elementType());
                        if (fieldType.is<TypeKind::Union>()) {
                            nontrivial = true;
                            break;
                        }
                    }
                    if (!nontrivial)
                        return;
                } else if (!baseType.is<TypeKind::Union>())
                    return; // If the base isn't a union, typechecking is similarly tautological.

                JasmineType loweredPattern = genCtx.lower(patternType);

                if (patternType.is<TypeKind::Tuple>()) {
                    auto tup = patternType.as<TypeKind::Tuple>();
                    auto inputTuple = baseType.as<TypeKind::Tuple>();
                    for (u32 i = 0; i < tup.count(); i ++) {
                        auto tag = genCtx.temp();
                        auto fieldType = expand(inputTuple.fieldType(i));

                        auto baseFieldType = fieldType;
                        if (baseFieldType.is<TypeKind::Pointer>())
                            baseFieldType = expand(baseFieldType.as<TypeKind::Pointer>().elementType());

                        if (!baseFieldType.is<TypeKind::Union>())
                            continue; // Trivial type match.

                        auto field = fieldType.is<TypeKind::Pointer>()
                            ? generateGetField(genCtx, builder, baseType, input, i, fieldType)
                            : generateAddrField(genCtx, builder, baseType, input, i);
                        jasmine_append_load_field(builder, genCtx.lower(tup.fieldType(i)), tag, field, 0);

                        continuation = genCtx.addBlock();
                        Type patternFieldType = expand(tup.fieldType(i));
                        u32 id;
                        if (patternFieldType.is<TypeKind::Struct>())
                            id = patternFieldType.as<TypeKind::Struct>().typeTag();
                        else if (patternFieldType.is<TypeKind::Named>())
                            id = patternFieldType.as<TypeKind::Named>().typeTag();
                        else
                            unreachable("Unions can only match against specific struct or named type members.");
                        jasmine_append_br_ne(builder, genCtx.tagType(), tag, genCtx.imm(id), genCtx.addEdgeTo(fail), genCtx.addEdgeTo(continuation));
                        jasmine_builder_set_block(builder, continuation);
                    }
                } else {
                    u32 id;
                    if (patternType.is<TypeKind::Struct>())
                        id = patternType.as<TypeKind::Struct>().typeTag();
                    else if (patternType.is<TypeKind::Named>())
                        id = patternType.as<TypeKind::Named>().typeTag();
                    else
                        unreachable("Unions can only match against specific struct or named type members.");

                    JasmineOperand ptr = input;
                    if (!expand(inputType).is<TypeKind::Pointer>()) {
                        ptr = genCtx.temp();
                        jasmine_append_addr(builder, loweredPattern, ptr, input);
                    }
                    auto tag = genCtx.temp();
                    jasmine_append_load_field(builder, loweredPattern, tag, ptr, 0); // Tag is always the first field.
                    continuation = genCtx.addBlock();
                    jasmine_append_br_ne(builder, genCtx.tagType(), tag, genCtx.imm(id), genCtx.addEdgeTo(fail), genCtx.addEdgeTo(continuation));
                    jasmine_builder_set_block(builder, continuation);
                }
                break;
            }

            default:
                unreachable("TODO: Implement codegen for pattern ", pattern);
        }
    }

    void generateMatch(GenerationContext& genCtx, JasmineBuilder builder, AST match) {
        Type inputType = typeOf(match.function(), match.child(0));
        JasmineOperand input = generate(genCtx, builder, match.child(0), inputType);
        JasmineType loweredInput = genCtx.lower(inputType);

        JasmineBlock continuation = genCtx.addBlock();
        bool anyNonUnreachable = false;
        for (u32 i = 1; i < match.arity(); i ++) {
            AST matchCase = match.child(i);
            JasmineBlock fail = i == match.arity() - 1 ? continuation : genCtx.addBlock();

            generatePattern(genCtx, builder, matchCase.child(0), inputType, input, fail);
            generate(genCtx, builder, matchCase.child(1), genCtx.module->voidType());
            if (!genCtx.isUnreachable) {
                anyNonUnreachable = true;
                jasmine_append_br(builder, genCtx.addEdgeTo(continuation));
            }

            genCtx.isUnreachable = false;
            jasmine_builder_set_block(builder, fail);
        }
        if (!anyNonUnreachable)
            jasmine_append_trap(builder);
    }

    bool canInitializeStatically(AST init, Type destType) {
        switch (init.kind()) {
            case ASTKind::Int:
            case ASTKind::Bool:
            case ASTKind::Char:
            case ASTKind::Unsigned:
            case ASTKind::Float:
                return true; // Constants can always be embedded in static sections.
            case ASTKind::Global:
                return init.module->globals[init.variable()].kind == VariableInfo::Function;
            case ASTKind::AddressOf:
                return destType.is<TypeKind::Pointer>() && init.child(0).kind() == ASTKind::Global;
            case ASTKind::Paren:
                return canInitializeStatically(init.child(0), destType);
            case ASTKind::Construct: {
                Type type = typeOf(init);
                if (type.is<TypeKind::Struct>()) {
                    // TODO: Support more than just fieldwise initializers.
                    assert(init.arity() == type.as<TypeKind::Struct>().count());
                    for (auto [i, child] : enumerate(init))
                        if (!canInitializeStatically(child, type.as<TypeKind::Struct>().fieldType(i)))
                            return false;
                    return true;
                } else if (type.is<TypeKind::Named>()) {
                    if (type.as<TypeKind::Named>().innerType() == Void)
                        return true;
                    assert(init.arity() == 1);
                    if (!canInitializeStatically(init.child(0), type.as<TypeKind::Named>().innerType()))
                        return false;
                    return true;
                } else if (type.is<TypeKind::Union>())
                    unreachable("TODO: Implement union types.");
                else
                    return false; // We don't support other constructors/conversions at the moment.
            }
            case ASTKind::String:
                return true;
            case ASTKind::List: {
                auto elementType = typeOf(init).as<TypeKind::Array>().elementType();
                for (AST child : init)
                    if (!canInitializeStatically(child, elementType))
                        return false;
                return true;
            }
            case ASTKind::TypeField:
            case ASTKind::Typename:
            case ASTKind::GlobalTypename:
                return true; // It's either an atom or we broke something. We can catch it in the later call once we have a Function handy.
            default:
                return false;
        }
    }

    JasmineValue createStaticInitializer(GenerationContext& genCtx, AST init, Type destType);

    template<typename T>
    JasmineValue createIntArrayValue(GenerationContext& genCtx, AST init, Type elementType, JasmineValue(*func)(JasmineModule, const T*, size_t)) {
        vec<T, 8> initializers;
        for (u32 i = 0; i < init.arity(); i ++) {
            auto value = createStaticInitializer(genCtx, init.child(i), elementType);
            initializers.push(jasmine_get_integer_value(genCtx.output, value));
        }
        return func(genCtx.output, initializers.begin(), initializers.size());
    }

    JasmineValue createStaticInitializer(GenerationContext& genCtx, AST init, Type destType) {
        switch (init.kind()) {
            case ASTKind::Int:
                assert(destType.is<TypeKind::Numeric>());
                if (destType == init.module->f32Type())
                    return jasmine_f32_value(genCtx.output, init.intConst());
                else if (destType == init.module->f64Type())
                    return jasmine_f64_value(genCtx.output, init.intConst());
                return jasmine_integer_value(genCtx.output, genCtx.lower(destType), init.intConst());
            case ASTKind::Unsigned:
                assert(destType.is<TypeKind::Numeric>());
                if (destType == init.module->f32Type())
                    return jasmine_f32_value(genCtx.output, init.uintConst());
                else if (destType == init.module->f64Type())
                    return jasmine_f64_value(genCtx.output, init.uintConst());
                return jasmine_integer_value(genCtx.output, genCtx.lower(destType), i64(init.uintConst()));
            case ASTKind::Float:
                return destType == init.module->f32Type()
                    ? jasmine_f32_value(genCtx.output, init.floatConst())
                    : jasmine_f64_value(genCtx.output, init.floatConst());
            case ASTKind::Bool:
                return jasmine_integer_value(genCtx.output, genCtx.lower(destType), init.boolConst() ? 1 : 0);
            case ASTKind::Char:
                return jasmine_integer_value(genCtx.output, genCtx.lower(destType), init.charConst());
            case ASTKind::Global:
                assert(init.module->globals[init.variable()].kind == VariableInfo::Function);
                return genCtx.funcrefValue(init.module->str(init.module->globals[init.variable()].name));
            case ASTKind::AddressOf:
                assert(destType.is<TypeKind::Pointer>());
                assert(init.child(0).kind() == ASTKind::Global);
                return genCtx.globalValue(init.child(0).variable());
            case ASTKind::Paren:
                return createStaticInitializer(genCtx, init, destType);
            case ASTKind::Construct: {
                Type type = typeOf(init);
                if (type.is<TypeKind::Named>()) {
                    JasmineValue values[2];
                    bool isCase = type.as<TypeKind::Named>().isCase();
                    bool isVoid = type.as<TypeKind::Named>().innerType() == Void;
                    if (isCase) {
                        genCtx.lower(type); // Needed to ensure our type tag has been assigned.
                        values[0] = jasmine_integer_value(genCtx.output, genCtx.tagType(), type.as<TypeKind::Named>().typeTag());
                    }
                    if (!isVoid)
                        values[1] = createStaticInitializer(genCtx, init.child(1), type.as<TypeKind::Named>().innerType());
                    JasmineValue* start = isCase ? values : values + 1;
                    JasmineValue* end = isVoid ? values + 2 : values + 1;
                    return jasmine_struct_value(genCtx.output, genCtx.lower(type), start, end - start);
                } else if (type.is<TypeKind::Struct>()) {
                    auto structType = type.as<TypeKind::Struct>();
                    vec<JasmineValue, 8> initValues;
                    if (type.as<TypeKind::Struct>().isCase()) {
                        genCtx.lower(type); // Needed to ensure our type tag has been assigned.
                        initValues.push(jasmine_integer_value(genCtx.output, genCtx.tagType(), structType.typeTag()));
                    }
                    for (u32 i = 0; i < structType.count(); i ++)
                        initValues.push(createStaticInitializer(genCtx, init.child(i), structType.fieldType(i)));
                    return jasmine_struct_value(genCtx.output, genCtx.lower(structType), initValues.begin(), initValues.size());
                } else if (type.is<TypeKind::Union>())
                    unreachable("TODO: Implement union types.");
                else
                    unreachable("Found unsupported type ", type, " for static construction.");
            }
            case ASTKind::String: {
                auto text = init.module->str(init.stringConst());
                JasmineValue value = jasmine_i8_array_value(genCtx.output, (const int8_t*)text.data(), text.size());
                if (destType.is<TypeKind::Slice>()) {
                    array<i8, 64> buf;
                    auto name = prints(buf, ".array ", genCtx.nextAnonArrayId(), '.');
                    jasmine_define_static(genCtx.output, jasmine_make_array_type(genCtx.output, JASMINE_TYPE_I8, init.arity()), name.data(), name.size(), value);
                    array<JasmineValue, 2> values;
                    values[0] = jasmine_static_ref_value(genCtx.output, name.data(), name.size());
                    values[1] = jasmine_integer_value(genCtx.output, genCtx.sizeType(), text.size());
                    value = jasmine_struct_value(genCtx.output, genCtx.lower(destType), values.begin(), values.size());
                }
                return value;
            }

            case ASTKind::List: {
                Type elementType;
                if (destType.is<TypeKind::Slice>())
                    elementType = destType.as<TypeKind::Slice>().elementType();
                else
                    elementType = destType.as<TypeKind::Array>().elementType();
                Module* module = init.module;
                JasmineValue value;
                if (elementType == module->i8Type())
                    value = createIntArrayValue<int8_t>(genCtx, init, module->i8Type(), jasmine_i8_array_value);
                else if (elementType == module->u8Type())
                    value = createIntArrayValue<uint8_t>(genCtx, init, module->u8Type(), jasmine_u8_array_value);
                else if (elementType == module->i16Type())
                    value = createIntArrayValue<int16_t>(genCtx, init, module->i16Type(), jasmine_i16_array_value);
                else if (elementType == module->u16Type())
                    value = createIntArrayValue<uint16_t>(genCtx, init, module->u16Type(), jasmine_u16_array_value);
                else if (elementType == module->i32Type())
                    value = createIntArrayValue<int32_t>(genCtx, init, module->i32Type(), jasmine_i32_array_value);
                else if (elementType == module->u32Type())
                    value = createIntArrayValue<uint32_t>(genCtx, init, module->u32Type(), jasmine_u32_array_value);
                else if (elementType == module->i64Type())
                    value = createIntArrayValue<int64_t>(genCtx, init, module->i64Type(), jasmine_i64_array_value);
                else if (elementType == module->u64Type())
                    value = createIntArrayValue<uint64_t>(genCtx, init, module->u64Type(), jasmine_u64_array_value);
                else if (elementType == module->f32Type()) {
                    vec<f32, 8> initializers;
                    for (u32 i = 0; i < init.arity(); i ++) {
                        auto value = createStaticInitializer(genCtx, init.child(i), elementType);
                        initializers.push(jasmine_get_f32_value(genCtx.output, value));
                    }
                    value = jasmine_f32_array_value(genCtx.output, initializers.begin(), initializers.size());
                } else if (elementType == module->f64Type()) {
                    vec<f64, 8> initializers;
                    for (u32 i = 0; i < init.arity(); i ++) {
                        auto value = createStaticInitializer(genCtx, init.child(i), elementType);
                        initializers.push(jasmine_get_f64_value(genCtx.output, value));
                    }
                    value = jasmine_f64_array_value(genCtx.output, initializers.begin(), initializers.size());
                } else {
                    vec<JasmineValue, 8> initializers;
                    for (u32 i = 0; i < init.arity(); i ++)
                        initializers.push(createStaticInitializer(genCtx, init.child(i), elementType));
                    value = jasmine_value_array_value(genCtx.output, genCtx.lower(elementType), initializers.begin(), initializers.size());
                }
                if (destType.is<TypeKind::Slice>()) {
                    array<i8, 64> buf;
                    auto name = prints(buf, ".array ", genCtx.nextAnonArrayId(), '.');
                    jasmine_define_static(genCtx.output, jasmine_make_array_type(genCtx.output, genCtx.lower(elementType), init.arity()), name.data(), name.size(), value);
                    array<JasmineValue, 2> values;
                    values[0] = jasmine_static_ref_value(genCtx.output, name.data(), name.size());
                    values[1] = jasmine_integer_value(genCtx.output, genCtx.sizeType(), init.arity());
                    value = jasmine_struct_value(genCtx.output, genCtx.lower(destType), values.begin(), values.size());
                }
                return value;
            }

            case ASTKind::TypeField:
            case ASTKind::Typename:
            case ASTKind::GlobalTypename: {
                Type type = evaluateType(genCtx.module, genCtx.func(), init);
                if (isAtom(type)) {
                    genCtx.ensureTypeTag(type);
                    if (destType.is<TypeKind::Pointer>())
                        return genCtx.atomRefValue(type); // It's a static/data ref, so natively a pointer.
                    auto val = genCtx.temp();
                    return jasmine_integer_value(genCtx.output, genCtx.tagType(), type.as<TypeKind::Named>().typeTag());
                }
                unreachable("Non-atom typenames are not permitted in value position.");
            }

            default:
                unreachable("Tried to create static initializer value from invalid node ", init);
        }
    }

    JasmineOperand generate(GenerationContext& genCtx, JasmineBuilder builder, AST ast, Type destType) {
        JasmineType resultType;
        JasmineOperand value, lhs, rhs, result;
        Module* module = ast.module;

        if (genCtx.isUnreachable)
            return JASMINE_INVALID_OPERAND;

        bool printComments = false;
        if UNLIKELY(config::jasmineASTComments && !ast.isLeaf() && ast.kind() != ASTKind::TopLevel && ast.kind() != ASTKind::Do) {
            i8 buffer[1024];
            slice<i8> output = { buffer, 1024 };
            for (u32 i = 0; i < genCtx.expressionDepth; i ++)
                output = format(output, ' ');
            output = format(output, ASTWithDepth { ast, 2 });
            jasmine_append_comment(builder, buffer, 1024 - output.size());
            printComments = true;
        }
        ExpressionDepthScope expressionDepth(genCtx, printComments);

        switch (ast.kind()) {
            case ASTKind::Bool:
                return genCtx.imm(ast.boolConst() ? 1 : 0);

            case ASTKind::Char:
                return genCtx.imm(ast.charConst());

            case ASTKind::Unsigned:
                assert(destType.is<TypeKind::Numeric>());
                if (destType == module->f32Type())
                    return genCtx.f32imm(ast.uintConst());
                else if (destType == module->f64Type())
                    return genCtx.f64imm(ast.uintConst());
                return genCtx.imm(bitcast<i64>(ast.uintConst()));

            case ASTKind::Int:
                assert(destType.is<TypeKind::Numeric>());
                if (destType == module->f32Type())
                    return genCtx.f32imm(ast.intConst());
                else if (destType == module->f64Type())
                    return genCtx.f64imm(ast.intConst());
                return genCtx.imm(ast.intConst());

            case ASTKind::Float:
                if (destType == F32)
                    return genCtx.f32imm(ast.floatConst());
                else
                    return genCtx.f64imm(ast.floatConst());

            case ASTKind::Local: {
                const auto& varInfo = ast.varInfo(genCtx.func());
                if (genCtx.module->node(varInfo.decl).kind() == ASTKind::FunDecl) {
                    // Function with known def - we should reference its symbol directly.
                    auto name = genCtx.module->str(genCtx.mangledName(module, module->node(varInfo.decl).function()));
                    return coerce(genCtx, builder, destType, ast.module->types->get(varInfo.type), genCtx.funcref(name));
                }
                return coerce(genCtx, builder, destType, ast.module->types->get(varInfo.type), genCtx.local(ast.variable()));
            }

            case ASTKind::Global: {
                const auto& varInfo = ast.varInfo(genCtx.func());
                auto name = genCtx.module->str(varInfo.name);
                auto loweredType = genCtx.lower(ast.module->types->get(varInfo.type));
                if (genCtx.module->node(varInfo.decl).kind() == ASTKind::FunDecl) {
                    // Function with known def - we should reference its symbol directly.
                    auto name = genCtx.module->str(genCtx.mangledName(module, module->node(varInfo.decl).function()));
                    return coerce(genCtx, builder, destType, ast.module->types->get(varInfo.type), genCtx.funcref(name));
                }
                auto dest = genCtx.temp();
                jasmine_append_load(builder, loweredType, dest, genCtx.staticref(name));
                return coerce(genCtx, builder, destType, ast.module->types->get(varInfo.type), dest);
            }

            case ASTKind::Paren:
                return generate(genCtx, builder, ast.child(0), destType);

            case ASTKind::Add:
                return generateBinary(genCtx, builder, ast, destType, jasmine_append_add);
            case ASTKind::Sub:
                return generateBinary(genCtx, builder, ast, destType, jasmine_append_sub);
            case ASTKind::Mul:
                return generateBinary(genCtx, builder, ast, destType, jasmine_append_mul);
            case ASTKind::Div:
                return generateBinary(genCtx, builder, ast, destType, jasmine_append_div);
            case ASTKind::Rem:
                return generateBinary(genCtx, builder, ast, destType, jasmine_append_rem);
            case ASTKind::Exp:
                return generateExp(genCtx, builder, ast.child(0), ast.child(1), typeOf(ast), destType);
            case ASTKind::Plus:
                return generate(genCtx, builder, ast.child(0), destType);
            case ASTKind::Minus:
                return generateUnary(genCtx, builder, ast, destType, jasmine_append_neg);
            case ASTKind::BitAnd:
                return generateBinary(genCtx, builder, ast, destType, jasmine_append_and);
            case ASTKind::BitOr:
                return generateBinary(genCtx, builder, ast, destType, jasmine_append_or);
            case ASTKind::BitXor:
                return generateBinary(genCtx, builder, ast, destType, jasmine_append_xor);
            case ASTKind::BitShl:
                return generateBinary(genCtx, builder, ast, destType, jasmine_append_shl);
            case ASTKind::BitShr:
                return generateBinary(genCtx, builder, ast, destType, jasmine_append_shr);
            case ASTKind::BitRol:
                return generateBinary(genCtx, builder, ast, destType, jasmine_append_rol);
            case ASTKind::BitRor:
                return generateBinary(genCtx, builder, ast, destType, jasmine_append_ror);
            case ASTKind::BitNot:
                return generateUnary(genCtx, builder, ast, destType, jasmine_append_not);
            case ASTKind::Less:
                return generateCompare(genCtx, builder, ast, destType, jasmine_append_is_lt);
            case ASTKind::LessEq:
                return generateCompare(genCtx, builder, ast, destType, jasmine_append_is_le);
            case ASTKind::Greater:
                return generateCompare(genCtx, builder, ast, destType, jasmine_append_is_gt);
            case ASTKind::GreaterEq:
                return generateCompare(genCtx, builder, ast, destType, jasmine_append_is_ge);
            case ASTKind::Equal:
                return generateCompare(genCtx, builder, ast, destType, jasmine_append_is_eq);
            case ASTKind::NotEqual:
                return generateCompare(genCtx, builder, ast, destType, jasmine_append_is_ne);
            case ASTKind::AddEq:
                return generateCompoundAssignment(genCtx, builder, ast, jasmine_append_add);
            case ASTKind::SubEq:
                return generateCompoundAssignment(genCtx, builder, ast, jasmine_append_sub);
            case ASTKind::MulEq:
                return generateCompoundAssignment(genCtx, builder, ast, jasmine_append_mul);
            case ASTKind::DivEq:
                return generateCompoundAssignment(genCtx, builder, ast, jasmine_append_div);
            case ASTKind::RemEq:
                return generateCompoundAssignment(genCtx, builder, ast, jasmine_append_rem);
            case ASTKind::ExpEq:
                return generateExpAssignment(genCtx, builder, ast);
            case ASTKind::BitAndEq:
                return generateCompoundAssignment(genCtx, builder, ast, jasmine_append_and);
            case ASTKind::BitOrEq:
                return generateCompoundAssignment(genCtx, builder, ast, jasmine_append_or);
            case ASTKind::BitXorEq:
                return generateCompoundAssignment(genCtx, builder, ast, jasmine_append_xor);
            case ASTKind::BitShlEq:
                return generateCompoundAssignment(genCtx, builder, ast, jasmine_append_shl);
            case ASTKind::BitShrEq:
                return generateCompoundAssignment(genCtx, builder, ast, jasmine_append_shr);
            case ASTKind::BitRolEq:
                return generateCompoundAssignment(genCtx, builder, ast, jasmine_append_rol);
            case ASTKind::BitRorEq:
                return generateCompoundAssignment(genCtx, builder, ast, jasmine_append_ror);
            case ASTKind::PreIncr:
            case ASTKind::PreDecr:
            case ASTKind::PostIncr:
            case ASTKind::PostDecr:
                return generateIncrDecr(genCtx, builder, destType, ast);

            case ASTKind::And: {
                auto result = genCtx.temp();
                auto lhs = generate(genCtx, builder, ast.child(0), module->boolType());
                jasmine_append_mov(builder, JASMINE_TYPE_BOOL, result, lhs);

                auto ifTrue = genCtx.addBlock(), continuation = genCtx.addBlock();
                jasmine_append_br_if(builder, JASMINE_TYPE_BOOL, result, genCtx.addEdgeTo(ifTrue), genCtx.addEdgeTo(continuation));

                jasmine_builder_set_block(builder, ifTrue);
                auto rhs = generate(genCtx, builder, ast.child(1), module->boolType());
                jasmine_append_mov(builder, JASMINE_TYPE_BOOL, result, rhs);
                jasmine_append_br(builder, genCtx.addEdgeTo(continuation));

                jasmine_builder_set_block(builder, continuation);
                return result;
            }

            case ASTKind::Or: {
                auto result = genCtx.temp();
                auto lhs = generate(genCtx, builder, ast.child(0), module->boolType());
                jasmine_append_mov(builder, JASMINE_TYPE_BOOL, result, lhs);

                auto ifFalse = genCtx.addBlock(), continuation = genCtx.addBlock();
                jasmine_append_br_if_not(builder, JASMINE_TYPE_BOOL, result, genCtx.addEdgeTo(ifFalse), genCtx.addEdgeTo(continuation));

                jasmine_builder_set_block(builder, ifFalse);
                auto rhs = generate(genCtx, builder, ast.child(1), module->boolType());
                jasmine_append_mov(builder, JASMINE_TYPE_BOOL, result, rhs);
                jasmine_append_br(builder, genCtx.addEdgeTo(continuation));

                jasmine_builder_set_block(builder, continuation);
                return result;
            }

            case ASTKind::Call: {
                JasmineOperand callee;
                Type calleeType;
                if (ast.child(0).kind() == ASTKind::ResolvedOverload) {
                    auto name = module->str(genCtx.mangledName(module, ast.child(0).overloadDecl().function()));
                    calleeType = typeOf(ast.child(0).overloadDecl());
                    callee = genCtx.funcref(name);
                } else {
                    calleeType = typeOf(ast, 0);
                    callee = generate(genCtx, builder, ast.child(0), calleeType);
                }
                const auto& funType = calleeType.as<TypeKind::Function>();
                vec<JasmineOperand> args;
                for (auto [i, arg] : enumerate(ast.children(1)))
                    args.push(generate(genCtx, builder, arg, expand(funType.parameterType(i))));
                if (expand(funType.returnType()) == Void) {
                    jasmine_append_call_void(builder, genCtx.lower(calleeType), callee, args.data(), args.size());
                    return JASMINE_INVALID_OPERAND;
                } else {
                    JasmineOperand result = genCtx.temp();
                    jasmine_append_call(builder, genCtx.lower(calleeType), result, callee, args.data(), args.size());
                    return coerce(genCtx, builder, destType, expand(funType.returnType()), result);
                }
            }

            case ASTKind::New: {
                auto type = typeOf(ast);
                auto elementType = type.as<TypeKind::Pointer>().elementType();
                if (!ast.child(0).missing())
                    value = generate(genCtx, builder, ast.child(0), typeOf(ast, 0));
                auto size = genCtx.sizeOf(elementType);
                result = genCtx.temp();
                jasmine_append_call(builder, genCtx.getMemoryAllocType(), result, genCtx.funcref(cstring("memory.alloc")), &size, 1);
                if (!ast.child(0).missing())
                    jasmine_append_store(builder, genCtx.lower(elementType), result, value);
                return result;
            }

            case ASTKind::NewArray: {
                auto type = typeOf(ast);
                auto elementType = type.as<TypeKind::Slice>().elementType();
                value = generate(genCtx, builder, ast.child(0), module->u64Type());
                auto size = genCtx.temp(), ptr = genCtx.temp(), result = genCtx.temp();

                jasmine_append_mul(builder, genCtx.sizeType(), size, value, genCtx.sizeOf(elementType));
                jasmine_append_call(builder, genCtx.getMemoryAllocType(), ptr, genCtx.funcref(cstring("memory.alloc")), &size, 1);
                jasmine_append_set_field(builder, genCtx.lower(type), result, 0, ptr);
                jasmine_append_set_field(builder, genCtx.lower(type), result, 1, value);
                return result;
            }

            case ASTKind::Del: {
                value = generate(genCtx, builder, ast.child(0), typeOf(ast, 0));
                jasmine_append_call_void(builder, genCtx.getMemoryFreeType(), genCtx.funcref(cstring("memory.free")), &value, 1);
                return JASMINE_INVALID_OPERAND;
            }

            case ASTKind::String: {
                auto sym = ast.stringConst();
                auto value = genCtx.stringRef(sym);
                result = genCtx.temp();
                jasmine_append_load(builder, jasmine_make_array_type(genCtx.output, JASMINE_TYPE_I8, module->str(sym).size()), result, value);
                if (destType.is<TypeKind::Slice>()) {
                    auto tmp = result;
                    auto addr = genCtx.temp();
                    result = genCtx.temp();
                    auto loweredSlice = genCtx.lower(destType);
                    jasmine_append_addr(builder, JASMINE_TYPE_PTR, addr, tmp);
                    jasmine_append_set_field(builder, loweredSlice, result, 0, addr);
                    jasmine_append_set_field(builder, loweredSlice, result, 1, genCtx.imm(module->str(sym).size()));
                }
                return result;
            }

            case ASTKind::Tuple: {
                auto baseType = typeOf(ast);
                auto tupleType = baseType.as<TypeKind::Tuple>();
                auto loweredTuple = genCtx.lower(tupleType);
                auto result = genCtx.temp();
                for (u32 i = 0; i < tupleType.count(); i ++) {
                    auto init = generate(genCtx, builder, ast.child(i), tupleType.fieldType(i));
                    jasmine_append_set_field(builder, loweredTuple, result, i, init);
                }
                return result;
            }

            case ASTKind::List: {
                u32 minLength = 0;
                bool isDynamic = false;
                for (AST child : ast) {
                    if (child.kind() == ASTKind::Splat)
                        isDynamic = true;
                    else
                        minLength ++;
                }
                assert(!isDynamic); // TODO: Dynamic array constructors.

                result = genCtx.temp();
                auto baseType = typeOf(ast);
                Type elementType;
                if (baseType.is<TypeKind::Array>())
                    elementType = baseType.as<TypeKind::Array>().elementType();
                else
                    elementType = baseType.as<TypeKind::Slice>().elementType();
                auto loweredElement = genCtx.lower(elementType);

                jasmine_append_var(builder, jasmine_make_array_type(genCtx.output, loweredElement, minLength), result);
                for (auto [i, child] : enumerate(ast)) {
                    auto element = generate(genCtx, builder, child, elementType);
                    jasmine_append_set_index(builder, loweredElement, result, genCtx.sizeType(), genCtx.imm(i), element);
                }

                if (baseType.is<TypeKind::Slice>()) {
                    auto tmp = result;
                    auto addr = genCtx.temp();
                    result = genCtx.temp();
                    auto loweredSlice = genCtx.lower(destType);
                    jasmine_append_addr(builder, JASMINE_TYPE_PTR, addr, tmp);
                    jasmine_append_set_field(builder, loweredSlice, result, 0, addr);
                    jasmine_append_set_field(builder, loweredSlice, result, 1, genCtx.imm(minLength));
                }

                return coerce(genCtx, builder, destType, baseType, result);
            }

            case ASTKind::Construct: {
                auto type = typeOf(ast);
                if (type.is<TypeKind::Numeric>()) {
                    JasmineOperand result = genCtx.temp();
                    value = generate(genCtx, builder, ast.child(0), typeOf(ast, 0));
                    jasmine_append_convert(builder, genCtx.lower(type), result, genCtx.lower(typeOf(ast, 0)), value);
                    return coerce(genCtx, builder, destType, type, result);
                } else if (type.is<TypeKind::Struct>()) {
                    auto structType = type.as<TypeKind::Struct>();
                    auto loweredStruct = genCtx.lower(type);
                    result = genCtx.temp();
                    if (structType.isCase())
                        jasmine_append_set_field(builder, loweredStruct, result, 0, genCtx.imm(structType.typeTag()));
                    for (u32 i = 0; i < structType.count(); i ++) {
                        JasmineOperand field = generate(genCtx, builder, ast.child(i), structType.fieldType(i));
                        jasmine_append_set_field(builder, loweredStruct, result, structType.isCase() ? i + 1 : i, field);
                    }
                    return result;
                } else if (type.is<TypeKind::Named>()) {
                    auto namedType = type.as<TypeKind::Named>();
                    auto loweredStruct = genCtx.lower(type);
                    result = genCtx.temp();
                    JasmineOperand field = generate(genCtx, builder, ast.child(0), namedType.innerType());
                    if (namedType.isCase())
                        jasmine_append_set_field(builder, loweredStruct, result, 0, genCtx.imm(namedType.typeTag()));
                    jasmine_append_set_field(builder, loweredStruct, result, namedType.isCase() ? 1 : 0, field);
                    return result;
                } else if (type.is<TypeKind::Pointer>()) {
                    return generate(genCtx, builder, ast.child(0), typeOf(ast, 0));
                } else if (type.is<TypeKind::Slice>()) {
                    return generate(genCtx, builder, ast.child(0), typeOf(ast, 0));
                } else
                    unreachable("Invalid construction ", ast);
            }

            case ASTKind::Return: {
                if (ast.child(0).missing()) {
                    genCtx.isUnreachable = true;
                    jasmine_append_ret_void(builder);
                    return JASMINE_INVALID_OPERAND;
                }
                Type returnType = expand(typeOf(ast.module->node(ast.function()->decl)).as<TypeKind::Function>().returnType());
                value = generate(genCtx, builder, ast.child(0), returnType);
                genCtx.isUnreachable = true;
                if (returnType == ast.module->voidType())
                    jasmine_append_ret_void(builder);
                else
                    jasmine_append_ret(builder, genCtx.lower(returnType), value);
                return JASMINE_INVALID_OPERAND;
            }

            case ASTKind::VarDecl:
                if (ast.child(1).kind() == ASTKind::Local) {
                    // Local variable declaration, a simple move.
                    if (ast.child(2).missing())
                        jasmine_append_var(builder, genCtx.lower(typeOf(ast)), genCtx.local(ast.child(1).variable()));
                    else {
                        value = generate(genCtx, builder, ast.child(2), typeOf(ast));
                        jasmine_append_mov(builder, genCtx.lower(typeOf(ast)), genCtx.local(ast.child(1).variable()), value);
                    }
                } else if (ast.child(1).missing()) {
                    // This is a stub variable, almost certainly a parameter.
                    // We don't need to generate anything.
                } else {
                    assert(ast.child(1).kind() == ASTKind::Global);
                    auto varInfo = ast.child(1).varInfo();
                    auto name = ast.module->str(varInfo.name);
                    if (!ast.child(2).missing() && canInitializeStatically(ast.child(2), typeOf(ast)))
                        jasmine_define_static(genCtx.output, genCtx.lower(typeOf(ast)), name.data(), name.size(), createStaticInitializer(genCtx, ast.child(2), typeOf(ast)));
                    else {
                        jasmine_define_static_uninit(genCtx.output, genCtx.lower(typeOf(ast)), name.data(), name.size());
                        if (!ast.child(2).missing()) {
                            value = generate(genCtx, builder, ast.child(2), typeOf(ast));
                            jasmine_append_store(builder, genCtx.lower(typeOf(ast)), genCtx.global(ast.child(1).variable()), value);
                        }
                    }
                }
                return JASMINE_INVALID_OPERAND;

            case ASTKind::FunDecl: {
                if (ast.child(4).kind() != ASTKind::Missing) {
                    auto name = module->str(genCtx.mangledName(module, ast.function()));
                    auto funType = typeOf(ast).as<TypeKind::Function>();
                    JasmineFunction func = jasmine_create_function(genCtx.output, name.data(), name.size(), genCtx.lower(funType.returnType()));
                    genCtx.enter(ast.function(), func);
                    for (AST param : ast.child(2)) {
                        assert(param.kind() == ASTKind::VarDecl);
                        auto paramType = typeOf(ast.function(), param);
                        auto paramName = ast.module->str(param.child(1).varInfo(ast.function()).name);
                        auto paramOperand = jasmine_add_parameter(func, genCtx.lower(paramType), paramName.data(), paramName.size());
                        genCtx.setLocal(param.child(1).variable(), paramOperand);
                    }
                    auto entrypoint = jasmine_add_block(func);
                    jasmine_set_entrypoint(func, entrypoint);
                    jasmine_builder_set_block(builder, entrypoint);
                    generate(genCtx, builder, ast.child(4), ast.module->voidType());
                    genCtx.leave();
                }
                return JASMINE_INVALID_OPERAND;
            }

            case ASTKind::AliasDecl:
            case ASTKind::StructDecl:
            case ASTKind::NamedDecl:
            case ASTKind::UnionDecl:
            case ASTKind::StructCaseDecl:
            case ASTKind::NamedCaseDecl:
            case ASTKind::UnionCaseDecl:
                return JASMINE_INVALID_OPERAND; // Nothing to be done...yet.

            case ASTKind::If: {
                auto ifTrue = genCtx.addBlock(), continuation = genCtx.addBlock();
                generateConditionalBranch(genCtx, builder, ast.child(0), ifTrue, continuation, false);

                jasmine_builder_set_block(builder, ifTrue);
                generate(genCtx, builder, ast.child(1), genCtx.module->voidType());
                if (!genCtx.isUnreachable)
                    jasmine_append_br(builder, genCtx.addEdge(jasmine_current_block(builder), continuation));

                genCtx.isUnreachable = false;
                jasmine_builder_set_block(builder, continuation);
                return JASMINE_INVALID_OPERAND;
            }

            case ASTKind::IfElse:
            case ASTKind::Ternary: {
                auto ifTrue = genCtx.addBlock(), ifFalse = genCtx.addBlock();
                JasmineBlock continuation = JASMINE_INVALID_BLOCK;
                JasmineOperand result;
                Type resultType = typeOf(ast);
                bool hasResult = ast.kind() == ASTKind::Ternary;
                if (hasResult)
                    result = genCtx.temp();
                generateConditionalBranch(genCtx, builder, ast.child(0), ifTrue, ifFalse, false);

                bool leftUnreachable = true, rightUnreachable = true;

                jasmine_builder_set_block(builder, ifTrue);
                auto ifTrueResult = generate(genCtx, builder, ast.child(1), resultType);
                if (hasResult && !genCtx.isUnreachable)
                    jasmine_append_mov(builder, genCtx.lower(resultType), result, ifTrueResult);
                if (!genCtx.isUnreachable) {
                    leftUnreachable = false;
                    if (continuation.index == JASMINE_INVALID_BLOCK.index)
                        continuation = genCtx.addBlock();
                    jasmine_append_br(builder, genCtx.addEdge(jasmine_current_block(builder), continuation));
                }

                genCtx.isUnreachable = false;
                jasmine_builder_set_block(builder, ifFalse);
                auto ifFalseResult = generate(genCtx, builder, ast.child(2), resultType);
                if (hasResult && !genCtx.isUnreachable)
                    jasmine_append_mov(builder, genCtx.lower(resultType), result, ifFalseResult);
                if (!genCtx.isUnreachable) {
                    rightUnreachable = false;
                    if (continuation.index == JASMINE_INVALID_BLOCK.index)
                        continuation = genCtx.addBlock();
                    jasmine_append_br(builder, genCtx.addEdge(jasmine_current_block(builder), continuation));
                }

                genCtx.isUnreachable = leftUnreachable && rightUnreachable;
                if (genCtx.isUnreachable)
                    return JASMINE_INVALID_OPERAND;
                jasmine_builder_set_block(builder, continuation);
                return coerce(genCtx, builder, destType, resultType, result);
            }

            case ASTKind::While: {
                auto body = genCtx.addBlock(), end = genCtx.addBlock(), continuation = genCtx.addBlock();
                generateConditionalBranch(genCtx, builder, ast.child(0), body, continuation, false);

                genCtx.beginLoop(ast, end, continuation);
                jasmine_builder_set_block(builder, body);
                generate(genCtx, builder, ast.child(1), genCtx.module->voidType());
                if (!genCtx.isUnreachable)
                    jasmine_append_br(builder, genCtx.addEdge(jasmine_current_block(builder), end));
                genCtx.endLoop();

                // We put the backwards branch in its own separate block so
                // that continue statements always have a valid target to
                // branch to even though we invert the loop.
                genCtx.isUnreachable = false;
                jasmine_builder_set_block(builder, end);
                generateConditionalBranch(genCtx, builder, ast.child(0), body, continuation, false);

                jasmine_builder_set_block(builder, continuation);
                return JASMINE_INVALID_OPERAND;
            }

            case ASTKind::Break: {
                jasmine_append_br(builder, genCtx.addEdge(jasmine_current_block(builder), genCtx.currentBreakTarget()));
                genCtx.isUnreachable = true;
                return JASMINE_INVALID_OPERAND;
            }

            case ASTKind::Continue: {
                jasmine_append_br(builder, genCtx.addEdge(jasmine_current_block(builder), genCtx.currentContinueTarget()));
                genCtx.isUnreachable = true;
                return JASMINE_INVALID_OPERAND;
            }

            case ASTKind::Assign:
                if (ast.child(0).kind() == ASTKind::Local) {
                    auto src = generate(genCtx, builder, ast.child(1), typeOf(ast, 0));
                    jasmine_append_mov(builder, genCtx.lower(typeOf(ast, 0)), genCtx.local(ast.child(0).variable()), src);
                } else if (ast.child(0).kind() == ASTKind::Global) {
                    auto src = generate(genCtx, builder, ast.child(1), typeOf(ast, 0));
                    jasmine_append_store(builder, genCtx.lower(typeOf(ast, 0)), genCtx.global(ast.child(0).variable()), src);
                } else
                    unreachable("Assignment during the codegen phase should only operate on local and global variables.");
                return JASMINE_INVALID_OPERAND;

            case ASTKind::GetField:
            case ASTKind::SetField:
            case ASTKind::AddrField: {
                Type baseType = typeOf(ast, 0);
                auto base = ast.child(0).kind() == ASTKind::Global
                    ? genCtx.global(ast.child(0).variable())
                    : generate(genCtx, builder, ast.child(0), baseType);
                switch (ast.kind()) {
                    case ASTKind::GetField:
                        return generateGetField(genCtx, builder, baseType, base, ast.child(1).fieldId(), destType);
                    case ASTKind::AddrField:
                        return generateAddrField(genCtx, builder, baseType, base, ast.child(1).fieldId());
                    case ASTKind::SetField:
                        value = generate(genCtx, builder, ast.child(2), getFieldType(baseType, ast.child(1).fieldId()));
                        generateSetField(genCtx, builder, baseType, base, ast.child(1).fieldId(), value);
                        return JASMINE_INVALID_OPERAND;
                    default:
                        unreachable("Invalid field access kind.");
                }
            }

            case ASTKind::GetFields:
            case ASTKind::AddrFields: {
                Type baseType = typeOf(ast, 0);
                auto base = ast.child(0).kind() == ASTKind::Global
                    ? genCtx.global(ast.child(0).variable())
                    : generate(genCtx, builder, ast.child(0), baseType);

                Type resultType = typeOf(ast);
                auto loweredResult = genCtx.lower(resultType);
                auto result = genCtx.temp();
                for (u32 i = 1; i < ast.arity(); i ++) {
                    auto id = ast.child(i).fieldId();
                    if (ast.kind() == ASTKind::GetFields)
                        value = generateGetField(genCtx, builder, baseType, base, id, resultType.as<TypeKind::Tuple>().fieldType(i - 1));
                    else
                        value = generateAddrField(genCtx, builder, baseType, base, id);
                    jasmine_append_set_field(builder, loweredResult, result, i - 1, value);
                }
                return result;
            }

            case ASTKind::SetFields: {
                Type baseType = typeOf(ast, 0);
                auto base = ast.child(0).kind() == ASTKind::Global
                    ? genCtx.global(ast.child(0).variable())
                    : generate(genCtx, builder, ast.child(0), baseType);

                AST src = ast.child(ast.arity() - 1);
                Type inputType = typeOf(src);
                auto input = src.kind() == ASTKind::Global
                    ? genCtx.global(src.variable())
                    : generate(genCtx, builder, src, baseType);

                for (u32 i = 1; i < ast.arity() - 1; i ++) {
                    auto dstId = ast.child(i).fieldId();
                    auto srcId = i - 1;
                    value = generateGetField(genCtx, builder, inputType, input, srcId, inputType.as<TypeKind::Tuple>().fieldType(srcId));
                    generateSetField(genCtx, builder, baseType, base, dstId, value);
                }
            }

            case ASTKind::GetSlice: {
                Type baseType = typeOf(ast, 0);
                auto base = ast.child(0).kind() == ASTKind::Global
                    ? genCtx.global(ast.child(0).variable())
                    : generate(genCtx, builder, ast.child(0), baseType);
                maybe<JasmineOperand> low, high;
                if (!ast.child(1).missing())
                    low = some<JasmineOperand>(generate(genCtx, builder, ast.child(1), module->u64Type()));
                if (!ast.child(2).missing())
                    high = some<JasmineOperand>(generate(genCtx, builder, ast.child(2), module->u64Type()));
                return generateGetSlice(genCtx, builder, baseType, base, low, high);
            }

            case ASTKind::SetSlice: {
                Type baseType = typeOf(ast, 0);
                auto base = ast.child(0).kind() == ASTKind::Global
                    ? genCtx.global(ast.child(0).variable())
                    : generate(genCtx, builder, ast.child(0), baseType);
                maybe<JasmineOperand> low, high;
                if (!ast.child(1).missing())
                    low = some<JasmineOperand>(generate(genCtx, builder, ast.child(1), module->u64Type()));
                if (!ast.child(2).missing())
                    high = some<JasmineOperand>(generate(genCtx, builder, ast.child(2), module->u64Type()));
                Type sliceType;
                auto dest = generateGetSlice(genCtx, builder, baseType, base, low, high, &sliceType);

                assert(sliceType.is<TypeKind::Slice>());
                auto loweredSlice = genCtx.lower(sliceType);
                auto loweredElement = genCtx.lower(expand(sliceType.as<TypeKind::Slice>().elementType()));
                auto src = generate(genCtx, builder, ast.child(3), sliceType);
                auto destPtr = genCtx.temp(), srcPtr = genCtx.temp();
                auto destLength = genCtx.temp(), srcLength = genCtx.temp();
                jasmine_append_get_field(builder, loweredSlice, destLength, dest, 1);
                jasmine_append_get_field(builder, loweredSlice, srcLength, src, 1);

                auto fail = genCtx.addBlock(), continuation = genCtx.addBlock();
                jasmine_append_br_ne(builder, genCtx.sizeType(), destLength, srcLength, genCtx.addEdgeTo(fail), genCtx.addEdgeTo(continuation));

                jasmine_builder_set_block(builder, fail);
                jasmine_append_trap(builder);

                jasmine_builder_set_block(builder, continuation);
                auto i = genCtx.temp();
                auto forward = genCtx.addBlock(), backward = genCtx.addBlock(), end = genCtx.addBlock();

                jasmine_append_get_field(builder, loweredSlice, destPtr, dest, 0);
                jasmine_append_get_field(builder, loweredSlice, srcPtr, src, 0);
                jasmine_append_br_lt(builder, JASMINE_TYPE_PTR, destPtr, srcPtr, genCtx.addEdgeTo(backward), genCtx.addEdgeTo(forward));

                jasmine_builder_set_block(builder, backward);
                auto backwardLoop = genCtx.addBlock();
                jasmine_append_mov(builder, genCtx.sizeType(), i, genCtx.imm(0));
                jasmine_append_br_ge(builder, genCtx.sizeType(), i, destLength, genCtx.addEdgeTo(end), genCtx.addEdgeTo(backwardLoop));

                jasmine_builder_set_block(builder, backwardLoop);
                auto tmp = genCtx.temp();
                jasmine_append_load_index(builder, loweredElement, tmp, srcPtr, genCtx.sizeType(), i);
                jasmine_append_store_index(builder, loweredElement, destPtr, genCtx.sizeType(), i, tmp);
                jasmine_append_add(builder, genCtx.sizeType(), i, i, genCtx.imm(1));
                jasmine_append_br_ge(builder, genCtx.sizeType(), i, destLength, genCtx.addEdgeTo(end), genCtx.addEdgeTo(backwardLoop));

                jasmine_builder_set_block(builder, forward);
                auto forwardLoop = genCtx.addBlock();
                jasmine_append_sub(builder, genCtx.sizeType(), i, destLength, genCtx.imm(1));
                jasmine_append_br_ge(builder, genCtx.sizeType(), i, destLength, genCtx.addEdgeTo(end), genCtx.addEdgeTo(forwardLoop));

                jasmine_builder_set_block(builder, forwardLoop);
                jasmine_append_load_index(builder, loweredElement, tmp, srcPtr, genCtx.sizeType(), i);
                jasmine_append_store_index(builder, loweredElement, destPtr, genCtx.sizeType(), i, tmp);
                jasmine_append_sub(builder, genCtx.sizeType(), i, i, genCtx.imm(1));
                jasmine_append_br_ge(builder, genCtx.sizeType(), i, destLength, genCtx.addEdgeTo(end), genCtx.addEdgeTo(forwardLoop));

                jasmine_builder_set_block(builder, end);
                return JASMINE_INVALID_OPERAND;
            }

            case ASTKind::Length: {
                Type baseType = typeOf(ast, 0);
                auto base = ast.child(0).kind() == ASTKind::Global
                    ? genCtx.global(ast.child(0).variable())
                    : generate(genCtx, builder, ast.child(0), baseType);
                return coerce(genCtx, builder, destType, typeOf(ast), generateLength(genCtx, builder, baseType, base, typeOf(ast)));
            }

            case ASTKind::GetIndex:
            case ASTKind::SetIndex:
            case ASTKind::AddrIndex: {
                Type baseType = typeOf(ast, 0);
                auto base = ast.child(0).kind() == ASTKind::Global
                    ? genCtx.global(ast.child(0).variable())
                    : generate(genCtx, builder, ast.child(0), baseType);
                auto index = generate(genCtx, builder, ast.child(1), ast.module->u64Type());
                switch (ast.kind()) {
                    case ASTKind::GetIndex:
                        return generateGetIndex(genCtx, builder, baseType, base, index, destType);
                    case ASTKind::AddrIndex:
                        return generateAddrIndex(genCtx, builder, baseType, base, index);
                    case ASTKind::SetIndex:
                        generateSetIndex(genCtx, builder, baseType, base, index, ast.child(2));
                        return JASMINE_INVALID_OPERAND;
                    default:
                        unreachable("Invalid field access kind.");
                }
            }

            case ASTKind::Deref: {
                auto ptrType = typeOf(ast, 0);
                auto resultType = ptrType.as<TypeKind::Pointer>().elementType();
                auto src = generate(genCtx, builder, ast.child(0), ptrType);
                auto dest = genCtx.temp();
                jasmine_append_load(builder, genCtx.lower(resultType), dest, src);
                return coerce(genCtx, builder, destType, resultType, dest);
            }

            case ASTKind::AddressOf: {
                auto resultType = expand(typeOf(ast).as<TypeKind::Pointer>().elementType());
                auto dest = genCtx.temp();
                if (ast.child(0).kind() == ASTKind::Local)
                    jasmine_append_addr(builder, JASMINE_TYPE_PTR, dest, genCtx.local(ast.child(0).variable()));
                else if (ast.child(0).kind() == ASTKind::Global)
                    jasmine_append_mov(builder, JASMINE_TYPE_PTR, dest, genCtx.global(ast.child(0).variable()));
                else
                    unreachable("Expected local or global variable in address-of expression ", ast);
                return dest;
            }

            case ASTKind::Store: {
                auto ptrType = expand(typeOf(ast, 0));
                auto resultType = expand(ptrType.as<TypeKind::Pointer>().elementType());
                auto dest = generate(genCtx, builder, ast.child(0), ptrType);
                auto src = generate(genCtx, builder, ast.child(1), typeOf(ast, 1));
                jasmine_append_store(builder, genCtx.lower(resultType), dest, src);
                return JASMINE_INVALID_OPERAND;
            }

            case ASTKind::Then:
                generate(genCtx, builder, ast.child(0), destType);
                generate(genCtx, builder, ast.child(1), destType);
                return JASMINE_INVALID_OPERAND;

            case ASTKind::Match:
                generateMatch(genCtx, builder, ast);
                return JASMINE_INVALID_OPERAND;

            case ASTKind::TypeField:
            case ASTKind::Typename:
            case ASTKind::GlobalTypename: {
                Type type = evaluateType(module, genCtx.func(), ast);
                if (isAtom(type)) {
                    auto impl = genCtx.atomRef(type);
                    if (destType.is<TypeKind::Pointer>())
                        return impl; // It's a static/data ref, so natively a pointer.
                    auto val = genCtx.temp();
                    jasmine_append_load(builder, genCtx.lower(type), val, impl);
                    return coerce(genCtx, builder, destType, type, val);
                }
                unreachable("Non-atom typenames are not permitted in value position.");
            }


            case ASTKind::Do:
            case ASTKind::TopLevel: {
                JasmineOperand result;
                for (AST child : ast)
                    result = generate(genCtx, builder, child, destType);
                return JASMINE_INVALID_OPERAND;
            }
            default:
                unreachable("Could not lower AST node '", ast, "' to Jasmine IR.");
        }
    }

    struct AssemblyArtifact : public ArtifactData {
        JasmineAssembly assembly;
        AssemblyArtifact(JasmineAssembly assembly_in): assembly(assembly_in) {}

        ~AssemblyArtifact() {
            jasmine_destroy_assembly(assembly);
        }

        maybe<const_slice<i8>> getSource() override {
            return none<const_slice<i8>>();
        }

        maybe<const_slice<u32>> getLineOffsets() override {
            return none<const_slice<u32>>();
        }

        void print(Compilation* compilation) override {
            println("<binary object>");
        }

        u64 reportSize() override {
            // TODO: Report something meaningful here.
            return 0;
        }
    };

    NOINLINE Artifact* generateJasmine(Artifact* artifact, u32 optimizationLevel) {
        assert(artifact->kind == ArtifactKind::FinalizedAST);

        Module* module = artifact->as<Module>();
        auto artifactName = module->str(artifact->name);
        JasmineModule output = jasmine_create_module(artifactName.data(), artifactName.size());
        JasmineFunction topLevel = jasmine_create_function(output, "<toplevel>", 10, JASMINE_TYPE_VOID);
        JasmineBlock topLevelEntrypoint = jasmine_add_block(topLevel);
        jasmine_set_entrypoint(topLevel, topLevelEntrypoint);

        JasmineBuilder builder = jasmine_create_builder(output);
        GenerationContext genCtx(builder, module, output);
        if (optimizationLevel == 0)
            genCtx.setAssembly(jasmine_create_assembly(output));
        genCtx.enter(nullptr, topLevel);
        jasmine_builder_set_block(builder, topLevelEntrypoint);
        auto result = generate(genCtx, builder, module->getTopLevel(), module->getTopLevel().type());
        jasmine_append_ret_void(builder);
        genCtx.leave();
        if (optimizationLevel == 0) {
            jasmine_compile_module_only(*genCtx.assembly, output, optimizationLevel, true);
            artifact->update(new AssemblyArtifact(*genCtx.assembly));
            return artifact;
        }
        artifact->update(new JasmineArtifact(output));
        return artifact;
    }

    NOINLINE Artifact* emitAssembly(Artifact* artifact, u32 optimizationLevel) {
        assert(artifact->kind == ArtifactKind::JasmineIR);

        JasmineArtifact* jasmineIR = artifact->as<JasmineArtifact>();
        JasmineModule module = jasmineIR->module;
        JasmineAssembly assembly = jasmine_create_assembly(module);
        jasmine_compile(assembly, module, optimizationLevel, true);

        artifact->update(new AssemblyArtifact(assembly));
        return artifact;
    }

    void writeELF(Artifact* artifact, const_slice<i8> path) {
        assert(artifact->kind == ArtifactKind::Assembly);

        JasmineAssembly assembly = artifact->as<AssemblyArtifact>()->assembly;
        jasmine_write_relocatable_elf_object(assembly, path.data(), path.size());
    }

    JasmineAssembly getAssembly(Artifact* artifact) {
        return artifact->as<AssemblyArtifact>()->assembly;
    }

    JasmineAssembly createEntrypoint(Compilation* compilation) {
        Assembly* as = new Assembly(compilation->symbols);
        #ifdef RT_AMD64
            #ifdef RT_LINUX
                using ASM = AMD64LinuxAssembler;

                ASM::global(*as, as->symtab["_start"]);
                ASM::pop64(*as, GP(ASM::RDI));
                ASM::mov64(*as, GP(ASM::RSI), GP(ASM::RSP));
                ASM::lai64(*as, GP(ASM::RDX), Mem(ASM::RSP, 8), GP(ASM::RDI));
                ASM::and64(*as, GP(ASM::RSP), GP(ASM::RSP), Imm(-16));
                ASM::sub64(*as, GP(ASM::RSP), GP(ASM::RSP), Imm(8));
                ASM::push64(*as, GP(ASM::RDI));
                ASM::push64(*as, GP(ASM::RSI));
                ASM::push64(*as, GP(ASM::RDX));
                ASM::call(*as, Func(as->symtab[".clrt.init"]));
                ASM::pop64(*as, GP(ASM::RDI));
                ASM::pop64(*as, GP(ASM::RSI));
                ASM::pop64(*as, GP(ASM::RDX));
                ASM::add64(*as, GP(ASM::RSP), GP(ASM::RSP), Imm(8));
                ASM::call(*as, Func(as->symtab["<toplevel>"]));
                ASM::call(*as, Func(as->symtab[".clrt.deinit"]));
            #endif
        #else
            #error "No assembler found for current platform."
        #endif
        return JasmineAssembly { .handle = as };
    }

    JasmineExecutable load(Artifact* artifact) {
        assert(artifact->kind == ArtifactKind::Assembly);

        JITRuntimeShims* shims = artifact->parent->compilation->ensureJITRuntimeShims();

        AssemblyArtifact* assemblyArtifact = artifact->as<AssemblyArtifact>();
        JasmineAssembly assemblies[2];
        assemblies[0] = assemblyArtifact->assembly;
        assemblies[1] = JasmineAssembly { .handle = shims->assembly };
        JasmineExecutable executable = jasmine_link_assemblies(assemblies, 2);
        jasmine_load_executable(executable);
        return executable;
    }
}