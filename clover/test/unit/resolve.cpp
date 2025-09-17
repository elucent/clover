#include "clover/test/unit/helpers.h"

TEST(resolve_primitive_types) {
    auto artifact = RESOLVE(R"(
i8 a
i16 b
i32 c
i64 d
u8 e
u16 f
u32 g
u64 h
f32 i
f64 j
void k
bool l
char m
)");
    Module* module = artifact.as<Module>();
    auto topLevel = module->getTopLevel();
    ASSERT(topLevel.child(0).type() == I8);
    ASSERT(topLevel.child(1).type() == I16);
    ASSERT(topLevel.child(2).type() == I32);
    ASSERT(topLevel.child(3).type() == I64);
    ASSERT(topLevel.child(4).type() == U8);
    ASSERT(topLevel.child(5).type() == U16);
    ASSERT(topLevel.child(6).type() == U32);
    ASSERT(topLevel.child(7).type() == U64);
    ASSERT(topLevel.child(8).type() == F32);
    ASSERT(topLevel.child(9).type() == F64);
    ASSERT(topLevel.child(10).type() == Void);
    ASSERT(topLevel.child(11).type() == Bool);
    ASSERT(topLevel.child(12).type() == Char);
}

TEST(resolve_alias_reference) {
    auto artifact = RESOLVE(R"(
alias Int: i32
Int
)");
    Module* module = artifact.as<Module>();
    auto topLevel = artifact.as<Module>()->getTopLevel();
    auto int32 = topLevel.child(1);
    ASSERT(int32.kind() == ASTKind::GlobalTypename);
    ASSERT(int32.type(module) == I32);
}

TEST(resolve_global_var_decl) {
    auto artifact = RESOLVE(R"(
i32 x: 1
i32 y
if x:
    f32 z: 2
var w: 3
)");
    Module* module = artifact.as<Module>();
    auto topLevel = module->getTopLevel();

    auto x = topLevel.child(0);
    ASSERT(x.child(0).kind() == ASTKind::GlobalTypename);
    ASSERT(x.child(1).kind() == ASTKind::Global);
    ASSERT(x.type() == I32);

    auto y = topLevel.child(1);
    ASSERT(y.child(0).kind() == ASTKind::GlobalTypename);
    ASSERT(y.child(1).kind() == ASTKind::Global);
    ASSERT(y.type() == I32);

    auto z = topLevel.child(2).child(1).child(0);
    ASSERT(z.child(0).kind() == ASTKind::GlobalTypename);
    ASSERT(z.child(1).kind() == ASTKind::Global);
    ASSERT(z.type() == F32);

    auto w = topLevel.child(3);
    ASSERT(w.child(0).missing());
    ASSERT(w.child(1).kind() == ASTKind::Global);
    ASSERT(w.type().is<TypeKind::Var>());
}

TEST(resolve_ptr_decl) {
    auto artifact = RESOLVE(R"(
i32* x
i32[2]** y
)");
    Module* module = artifact.as<Module>();
    auto topLevel = module->getTopLevel();
    auto x = topLevel.child(0);
    ASSERT(x.kind() == ASTKind::VarDecl);
    ASSERT(x.child(0).kind() == ASTKind::PtrType);
    ASSERT(x.child(0).type() == module->ptrType(I32));
    ASSERT(x.type() == module->ptrType(I32));

    auto y = topLevel.child(1);
    ASSERT(y.kind() == ASTKind::VarDecl);
    ASSERT(y.child(0).kind() == ASTKind::PtrType);
    ASSERT(y.type() == module->ptrType(module->ptrType(module->arrayType(I32, 2u))));
}

TEST(resolve_ptr_cast) {
    auto artifact = RESOLVE(R"(
i32* x: i32*(42)
i32* y: x.i32*()
)");
    Module* module = artifact.as<Module>();
    auto topLevel = module->getTopLevel();
    auto x = topLevel.child(0);
    ASSERT(x.kind() == ASTKind::VarDecl);
    ASSERT(x.child(2).kind() == ASTKind::Construct);
    ASSERT(x.child(2).arity() == 1);
    ASSERT(x.child(2).child(0).uintConst() == 42);
    ASSERT(x.child(2).type() == module->ptrType(I32));
    ASSERT(x.type() == module->ptrType(I32));
    ASSERT(x.child(2).type<TypeKind::Pointer>().elementType() == I32);

    auto y = topLevel.child(1);
    ASSERT(y.kind() == ASTKind::VarDecl);
    ASSERT(y.child(2).kind() == ASTKind::Construct);
    ASSERT(y.child(2).arity() == 1);
    ASSERT(y.child(2).child(0).varInfo().name == module->sym("x"));
    ASSERT(y.child(2).type() == module->ptrType(I32));
    ASSERT(y.type() == module->ptrType(I32));
    ASSERT(y.child(2).type<TypeKind::Pointer>().elementType() == I32);
}

TEST(resolve_ptr_fundecl) {
    auto artifact = RESOLVE(R"(
i32* f(i32 x)
i32* g(i32 x: 1, i32 y: 2)
i32* h(i32* x): 1
i32* i(i32* x): x
i32* j(i32* x):
    x + 4
)");
}

TEST(resolve_ptr_slice_array) {
    auto artifact = RESOLVE(R"(
i32*[] ptr_slice
i32*[4] ptr_array
i32*[]* ptr_slice_ptr
i32*[4]*[] ptr_array_ptr_slice
)");
    Module* module = artifact.as<Module>();
    auto topLevel = module->getTopLevel();
    auto ptr_slice = topLevel.child(0);
    ASSERT(ptr_slice.child(0).kind() == ASTKind::SliceType);
    ASSERT(ptr_slice.child(0).child(0).kind() == ASTKind::PtrType);
    ASSERT(ptr_slice.child(0).child(0).type() == module->ptrType(I32));
    ASSERT(ptr_slice.child(0).type() == module->sliceType(module->ptrType(I32)));
    ASSERT(ptr_slice.type() == module->sliceType(module->ptrType(I32)));
    ASSERT(ptr_slice.type<TypeKind::Slice>().elementType() == module->ptrType(I32));

    auto ptr_array = topLevel.child(1);
    ASSERT(ptr_array.child(0).kind() == ASTKind::ArrayType);
    ASSERT(ptr_array.child(0).child(0).kind() == ASTKind::PtrType);
    ASSERT(ptr_array.type() == module->arrayType(module->ptrType(I32), 4u));
    ASSERT(ptr_array.type<TypeKind::Array>().elementType() == module->ptrType(I32));
    ASSERT(ptr_array.type<TypeKind::Array>().length() == 4);

    auto ptr_slice_ptr = topLevel.child(2);
    ASSERT(ptr_slice_ptr.child(0).kind() == ASTKind::PtrType);
    ASSERT(ptr_slice_ptr.child(0).child(0).kind() == ASTKind::SliceType);
    ASSERT(ptr_slice_ptr.child(0).child(0).child(0).kind() == ASTKind::PtrType);
    ASSERT(ptr_slice_ptr.type() == module->ptrType(module->sliceType(module->ptrType(I32))));

    auto ptr_array_ptr_slice = topLevel.child(3);
    ASSERT(ptr_array_ptr_slice.child(0).kind() == ASTKind::SliceType);
    ASSERT(ptr_array_ptr_slice.child(0).child(0).kind() == ASTKind::PtrType);
    ASSERT(ptr_array_ptr_slice.child(0).child(0).child(0).kind() == ASTKind::ArrayType);
    ASSERT(ptr_array_ptr_slice.child(0).child(0).child(0).child(0).kind() == ASTKind::PtrType);
    ASSERT(ptr_array_ptr_slice.type() == module->sliceType(module->ptrType(module->arrayType(module->ptrType(I32), 4u))));
}

TEST(resolve_slice_type) {
    auto artifact = RESOLVE(R"(
i32[] slice
i32[][] double_slice
)");
    Module* module = artifact.as<Module>();
    auto topLevel = module->getTopLevel();
    auto slice = topLevel.child(0);
    ASSERT(slice.child(0).kind() == ASTKind::SliceType);
    ASSERT(slice.child(0).type() == module->sliceType(I32));
    ASSERT(slice.type() == module->sliceType(I32));
    ASSERT(slice.type<TypeKind::Slice>().elementType() == I32);

    auto double_slice = topLevel.child(1);
    ASSERT(double_slice.child(0).kind() == ASTKind::SliceType);
    ASSERT(double_slice.child(0).child(0).kind() == ASTKind::SliceType);
    ASSERT(double_slice.child(0).type() == module->sliceType(module->sliceType(I32)));
}

TEST(resolve_slice_ctor) {
    auto artifact = RESOLVE(R"(
i32 slice
i32[] foo: i32[](slice)
i32[] bar: foo[0].i32[]()
)");
    Module* module = artifact.as<Module>();
    auto topLevel = module->getTopLevel();
    auto foo = topLevel.child(1);
    ASSERT(foo.child(2).kind() == ASTKind::Construct);
    ASSERT(foo.child(2).arity() == 1);
    ASSERT(foo.child(2).type() == module->sliceType(I32));
    ASSERT(foo.child(2).child(0).kind() == ASTKind::Global);
    ASSERT(foo.child(2).child(0).varInfo().name == module->sym("slice"));

    auto bar = topLevel.child(2);
    ASSERT(bar.child(2).kind() == ASTKind::Construct);
    ASSERT(bar.child(2).arity() == 1);
    ASSERT(bar.child(2).type() == module->sliceType(I32));
    ASSERT(bar.child(2).child(0).kind() == ASTKind::GetIndex);
    ASSERT(bar.child(2).child(0).child(0).kind() == ASTKind::Global);
    ASSERT(bar.child(2).child(0).child(0).varInfo().name == module->sym("foo"));
}

TEST(resolve_array_type) {
    auto artifact = RESOLVE(R"(
i32[4] array
i32[4][3] matrix
i32[][4] array_of_slices
i32[4][] slice_of_arrays
)");
    Module* module = artifact.as<Module>();
    auto topLevel = module->getTopLevel();
    auto array = topLevel.child(0);
    ASSERT(array.child(0).kind() == ASTKind::ArrayType);
    ASSERT(array.child(0).type() == module->arrayType(I32, 4u));
    ASSERT(array.type() == module->arrayType(I32, 4u));
    ASSERT(array.type<TypeKind::Array>().elementType() == I32);
    ASSERT(array.type<TypeKind::Array>().length() == 4);

    auto matrix = topLevel.child(1);
    ASSERT(matrix.child(0).kind() == ASTKind::ArrayType);
    ASSERT(matrix.child(0).child(0).kind() == ASTKind::ArrayType);
    ASSERT(matrix.type() == module->arrayType(module->arrayType(I32, 4u), 3u));

    auto array_of_slices = topLevel.child(2);
    ASSERT(array_of_slices.child(0).kind() == ASTKind::ArrayType);
    ASSERT(array_of_slices.child(0).child(0).kind() == ASTKind::SliceType);
    ASSERT(array_of_slices.type() == module->arrayType(module->sliceType(I32), 4u));

    auto slice_of_arrays = topLevel.child(3);
    ASSERT(slice_of_arrays.child(0).kind() == ASTKind::SliceType);
    ASSERT(slice_of_arrays.child(0).child(0).kind() == ASTKind::ArrayType);
    ASSERT(slice_of_arrays.type() == module->sliceType(module->arrayType(I32, 4u)));
}

TEST(resolve_array_ctor) {
    auto artifact = RESOLVE(R"(
i32[][4] array_of_slices
i32[4][] slice_of_arrays
var slice_element: slice_of_arrays[3].i32[4]()
var array_element: array_of_slices.i32[][3]()[0].i32[]()
)");
    Module* module = artifact.as<Module>();
    auto topLevel = module->getTopLevel();
    auto slice_element = topLevel.child(2);
    ASSERT(slice_element.child(2).kind() == ASTKind::Construct);
    ASSERT(slice_element.child(2).arity() == 1);
    ASSERT(slice_element.child(2).type() == module->arrayType(I32, 4u));
    ASSERT(slice_element.child(2).child(0).kind() == ASTKind::GetIndex);
    ASSERT(slice_element.child(2).child(0).child(0).kind() == ASTKind::Global);
    ASSERT(slice_element.child(2).child(0).child(0).varInfo().name == module->sym("slice_of_arrays"));

    auto array_element = topLevel.child(3);
    ASSERT(array_element.child(2).kind() == ASTKind::Construct);
    ASSERT(array_element.child(2).arity() == 1);
    ASSERT(array_element.child(2).type() == module->sliceType(I32));
    auto array_element_arg = array_element.child(2).child(0);
    ASSERT(array_element_arg.kind() == ASTKind::GetIndex);
    ASSERT(array_element_arg.child(0).kind() == ASTKind::Construct);
    ASSERT(array_element_arg.child(0).type() == module->arrayType(module->sliceType(I32), 3u));
    ASSERT(array_element_arg.child(0).child(0).kind() == ASTKind::Global);
    ASSERT(array_element_arg.child(0).child(0).varInfo().name == module->sym("array_of_slices"));
}

TEST(resolve_tuple_type) {
    auto artifact = RESOLVE(R"(
(i32) singleton
(i32, i32) pair
(i32, (i32, i32)) nested_pair
((i32, i32), (i32, i32)) nested_pairs
(i32, f32, char) heterogeneous_triple
)");
    Module* module = artifact.as<Module>();
    auto topLevel = module->getTopLevel();
    auto singleton = topLevel.child(0);
    ASSERT(singleton.child(0).kind() == ASTKind::TupleType);
    ASSERT(singleton.type() == module->tupleType(I32));

    auto pair = topLevel.child(1);
    ASSERT(pair.child(0).kind() == ASTKind::TupleType);
    ASSERT(pair.type() == module->tupleType(I32, I32));

    auto nested_pair = topLevel.child(2);
    ASSERT(nested_pair.child(0).kind() == ASTKind::TupleType);
    ASSERT(nested_pair.type() == module->tupleType(I32, module->tupleType(I32, I32)));

    auto nested_pairs = topLevel.child(3);
    ASSERT(nested_pairs.child(0).kind() == ASTKind::TupleType);
    ASSERT(nested_pairs.type() == module->tupleType(module->tupleType(I32, I32), module->tupleType(I32, I32)));

    auto heterogeneous_triple = topLevel.child(4);
    ASSERT(heterogeneous_triple.child(0).kind() == ASTKind::TupleType);
    ASSERT(heterogeneous_triple.type() == module->tupleType(I32, F32, Char));
}

TEST(resolve_tuple_ctor) {
    auto artifact = RESOLVE(R"(
(i32, i32) pair1: (1, 2).(i32, i32)()
(i32, i32) pair2: (i32, i32)(pair1)
)");
    Module* module = artifact.as<Module>();
    auto topLevel = module->getTopLevel();
    auto pair1 = topLevel.child(0);
    ASSERT(pair1.child(0).kind() == ASTKind::TupleType);
    ASSERT(pair1.type() == module->tupleType(I32, I32));
    ASSERT(pair1.child(2).kind() == ASTKind::Construct);
    ASSERT(pair1.child(2).type() == module->tupleType(I32, I32));
    ASSERT(pair1.child(2).child(0).kind() == ASTKind::Tuple);

    auto pair2 = topLevel.child(1);
    ASSERT(pair2.child(0).kind() == ASTKind::TupleType);
    ASSERT(pair2.type() == module->tupleType(I32, I32));
    ASSERT(pair2.child(2).kind() == ASTKind::Construct);
    ASSERT(pair2.child(2).type() == module->tupleType(I32, I32));
    ASSERT(pair2.child(2).child(0).kind() == ASTKind::Global);
    ASSERT(pair2.child(2).child(0).varInfo().name == module->sym("pair1"));
}

TEST(resolve_function_type) {
    auto artifact = RESOLVE(R"(
i32(i32) one_param
i32(i32, i32) two_params
i32(i32, f32) different_params
i32[](i32[]) slice_param
i32(void) no_param
)");
    Module* module = artifact.as<Module>();
    auto topLevel = module->getTopLevel();

    auto one_param = topLevel.child(0);
    ASSERT(one_param.child(0).kind() == ASTKind::FunType);
    ASSERT(one_param.type() == module->funType(I32, I32));

    auto two_params = topLevel.child(1);
    ASSERT(two_params.child(0).kind() == ASTKind::FunType);
    ASSERT(two_params.type() == module->funType(I32, I32, I32));

    auto different_params = topLevel.child(2);
    ASSERT(different_params.child(0).kind() == ASTKind::FunType);
    ASSERT(different_params.type() == module->funType(I32, I32, F32));

    auto slice_param = topLevel.child(3);
    ASSERT(slice_param.child(0).kind() == ASTKind::FunType);
    ASSERT(slice_param.type() == module->funType(module->sliceType(I32), module->sliceType(I32)));

    auto no_param = topLevel.child(4);
    ASSERT(no_param.child(0).kind() == ASTKind::FunType);
    ASSERT(no_param.type() == module->funType(I32));
}

TEST(resolve_constructor_methods) {
    auto artifact = RESOLVE(R"(
var x: 42
x.i32()
x.i32*()
x.i32(i32)()
x.i32(i32)*(i32)(42)
x.i32(i32)*[42](42)
)");
    Module* module = artifact.as<Module>();
    auto topLevel = module->getTopLevel();

    auto first = topLevel.child(1);
    ASSERT(first.kind() == ASTKind::Construct);
    ASSERT(first.type() == I32);

    auto second = topLevel.child(2);
    ASSERT(second.kind() == ASTKind::Construct);
    ASSERT(second.type() == module->ptrType(I32));

    auto third = topLevel.child(3);
    ASSERT(third.kind() == ASTKind::Construct);
    ASSERT(third.type() == module->funType(I32, I32));

    auto fourth = topLevel.child(4);
    ASSERT(fourth.kind() == ASTKind::Construct);
    ASSERT(fourth.type() == module->funType(module->ptrType(module->funType(I32, I32)), I32));
    ASSERT(fourth.child(1).kind() == ASTKind::Unsigned);

    auto fifth = topLevel.child(5);
    ASSERT(fifth.kind() == ASTKind::Construct);
    ASSERT(fifth.type() == module->arrayType(module->ptrType(module->funType(I32, I32)), 42u));
    ASSERT(fifth.child(1).kind() == ASTKind::Unsigned);
}

TEST(resolve_named_type) {
    auto artifact = RESOLVE(R"(
type Foo: i32
type Bar
type Baz:
    Bar
)");
    Module* module = artifact.as<Module>();
    auto topLevel = module->getTopLevel();

    auto Foo = topLevel.child(0);
    ASSERT(Foo.kind() == ASTKind::NamedDecl);
    auto FooType = Foo.type();
    ASSERT(FooType.is<TypeKind::Named>());
    ASSERT_EQUAL(FooType.as<TypeKind::Named>().innerType(), I32);

    auto Bar = topLevel.child(1);
    ASSERT(Bar.kind() == ASTKind::NamedDecl);
    auto BarType = Bar.type();
    ASSERT(BarType.is<TypeKind::Named>());
    ASSERT_EQUAL(BarType.as<TypeKind::Named>().innerType(), Void);

    auto Baz = topLevel.child(2);
    ASSERT(Baz.kind() == ASTKind::NamedDecl);
    auto BazType = Baz.type();
    ASSERT(BazType.is<TypeKind::Named>());
    ASSERT_EQUAL(BazType.as<TypeKind::Named>().innerType(), BarType);
}

TEST(resolve_struct_type) {
    auto artifact = RESOLVE(R"(
type Foo: i32 x
type Bar:
    i32 x, y
    f32 z
    Foo w
)");
    Module* module = artifact.as<Module>();
    auto topLevel = module->getTopLevel();

    auto Foo = topLevel.child(0);
    ASSERT(Foo.kind() == ASTKind::StructDecl);
    auto FooType = Foo.type();
    ASSERT(FooType.is<TypeKind::Struct>());
    ASSERT_EQUAL(FooType.as<TypeKind::Struct>().count(), 1);
    ASSERT(FooType.as<TypeKind::Struct>().fieldName(0) == module->sym("x"));
    ASSERT_EQUAL(FooType.as<TypeKind::Struct>().fieldType(0), I32);

    auto Bar = topLevel.child(1);
    ASSERT(Bar.kind() == ASTKind::StructDecl);
    auto BarType = Bar.type();
    ASSERT(BarType.is<TypeKind::Struct>());
    ASSERT_EQUAL(BarType.as<TypeKind::Struct>().count(), 4);
    ASSERT(BarType.as<TypeKind::Struct>().fieldName(0) == module->sym("x"));
    ASSERT_EQUAL(BarType.as<TypeKind::Struct>().fieldType(0), I32);
    ASSERT(BarType.as<TypeKind::Struct>().fieldName(1) == module->sym("y"));
    ASSERT_EQUAL(BarType.as<TypeKind::Struct>().fieldType(1), I32);
    ASSERT(BarType.as<TypeKind::Struct>().fieldName(2) == module->sym("z"));
    ASSERT_EQUAL(BarType.as<TypeKind::Struct>().fieldType(2), F32);
    ASSERT(BarType.as<TypeKind::Struct>().fieldName(3) == module->sym("w"));
    ASSERT_EQUAL(BarType.as<TypeKind::Struct>().fieldType(3), FooType);
}

TEST(resolve_union_type) {
    auto artifact = RESOLVE(R"(
type Foo:
    case Bar
    case Baz: i32
    case Quux: i32 x
    case Xyzzy:
        case A
        case B
)");
    Module* module = artifact.as<Module>();
    auto topLevel = module->getTopLevel();

    auto Foo = topLevel.child(0);
    ASSERT(Foo.kind() == ASTKind::UnionDecl);
    auto FooType = Foo.type();
    ASSERT(FooType.is<TypeKind::Union>());
    ASSERT_EQUAL(FooType.as<TypeKind::Union>().count(), 4);

    auto Bar = Foo.child(1);
    ASSERT(Bar.kind() == ASTKind::NamedCaseDecl);
    auto BarType = Bar.type();
    ASSERT(BarType.is<TypeKind::Named>());
    ASSERT(BarType.as<TypeKind::Named>().isCase());
    ASSERT_EQUAL(BarType.as<TypeKind::Named>().innerType(), Void);
    ASSERT_EQUAL(FooType.as<TypeKind::Union>().caseType(0), BarType);

    auto Baz = Foo.child(2);
    ASSERT(Baz.kind() == ASTKind::NamedCaseDecl);
    auto BazType = Baz.type();
    ASSERT(BazType.is<TypeKind::Named>());
    ASSERT(BazType.as<TypeKind::Named>().isCase());
    ASSERT_EQUAL(BazType.as<TypeKind::Named>().innerType(), I32);
    ASSERT_EQUAL(FooType.as<TypeKind::Union>().caseType(1), BazType);

    auto Quux = Foo.child(3);
    ASSERT(Quux.kind() == ASTKind::StructCaseDecl);
    auto QuuxType = Quux.type();
    ASSERT(QuuxType.is<TypeKind::Struct>());
    ASSERT(QuuxType.as<TypeKind::Struct>().isCase());
    ASSERT_EQUAL(QuuxType.as<TypeKind::Struct>().count(), 1);
    ASSERT_EQUAL(QuuxType.as<TypeKind::Struct>().fieldType(0), I32);
    ASSERT_EQUAL(FooType.as<TypeKind::Union>().caseType(2), QuuxType);

    auto Xyzzy = Foo.child(4);
    ASSERT(Xyzzy.kind() == ASTKind::UnionCaseDecl);
    auto XyzzyType = Xyzzy.type();
    ASSERT(XyzzyType.is<TypeKind::Union>());
    ASSERT(XyzzyType.as<TypeKind::Union>().isCase());
    ASSERT_EQUAL(XyzzyType.as<TypeKind::Union>().count(), 2);
    auto A = Xyzzy.child(1);
    ASSERT(A.kind() == ASTKind::NamedCaseDecl);
    auto AType = A.type();
    ASSERT(AType.is<TypeKind::Named>());
    ASSERT(AType.as<TypeKind::Named>().isCase());
    ASSERT_EQUAL(AType.as<TypeKind::Named>().innerType(), Void);
    ASSERT_EQUAL(XyzzyType.as<TypeKind::Union>().caseType(0), AType);
    auto B = Xyzzy.child(2);
    ASSERT(B.kind() == ASTKind::NamedCaseDecl);
    auto BType = B.type();
    ASSERT(BType.is<TypeKind::Named>());
    ASSERT(BType.as<TypeKind::Named>().isCase());
    ASSERT_EQUAL(BType.as<TypeKind::Named>().innerType(), Void);
    ASSERT_EQUAL(XyzzyType.as<TypeKind::Union>().caseType(1), BType);
    ASSERT_EQUAL(FooType.as<TypeKind::Union>().caseType(3), XyzzyType);
}

TEST(resolve_nested_type_decl) {
    auto artifact = RESOLVE(R"(
type Foo:
    type Bar: i32
    i32 x
type Baz:
    case Quux: i32

Foo.Bar x
Baz.Quux y
)");

    Module* module = artifact.as<Module>();
    auto topLevel = module->getTopLevel();

    auto FooBar = topLevel.child(0).child(1).type();
    auto BazQuux = topLevel.child(1).child(1).type();

    auto x = topLevel.child(2);
    ASSERT(x.child(0).kind() == ASTKind::TypeField);
    ASSERT_EQUAL(x.type(), FooBar);

    auto y = topLevel.child(3);
    ASSERT(y.child(0).kind() == ASTKind::TypeField);
    ASSERT_EQUAL(y.type(), BazQuux);
}

TEST(resolve_recursive_struct_type) {
    auto artifact = RESOLVE(R"(
type Recursive:
    Recursive* ptr
type Tree:
    i32 value
    Tree* left, right
)");
}

TEST(resolve_numeric_cast) {
    auto artifact = RESOLVE(R"(
var x: i32(f32(i64(f64(42))))
)");

    Module* module = artifact.as<Module>();
    auto topLevel = module->getTopLevel();

    auto x = topLevel.child(0);
    auto init = x.child(2);
    ASSERT(init.kind() == ASTKind::Construct);
    ASSERT(init.type() == I32);
    ASSERT(init.child(0).kind() == ASTKind::Construct);
    ASSERT(init.child(0).type() == F32);
    ASSERT(init.child(0).child(0).kind() == ASTKind::Construct);
    ASSERT(init.child(0).child(0).type() == I64);
    ASSERT(init.child(0).child(0).child(0).kind() == ASTKind::Construct);
    ASSERT(init.child(0).child(0).child(0).type() == F64);
}

TEST(resolve_nested_set_field) {
    auto artifact = RESOLVE(R"(
type Foo: i32 x
type Bar: Foo foo
var bar: Bar(Foo(42))

bar.foo.x = 42
)");

    Module* module = artifact.as<Module>();
    auto topLevel = module->getTopLevel();

    auto setField = topLevel.child(3);
    ASSERT(setField.kind() == ASTKind::SetField);
    auto barFoo = setField.child(0);
    ASSERT(barFoo.kind() == ASTKind::AddrField);
    ASSERT(setField.child(1).kind() == ASTKind::Ident);
    ASSERT(setField.child(2).kind() == ASTKind::Unsigned);
    ASSERT(setField.child(2).uintConst() == 42);
}

TEST(resolve_nested_addr_field) {
    auto artifact = RESOLVE(R"(
type Foo: i32 x
type Bar: Foo foo
var bar: Bar(Foo(42))

&bar.foo.x
)");

    Module* module = artifact.as<Module>();
    auto topLevel = module->getTopLevel();

    auto addrField = topLevel.child(3);
    ASSERT(addrField.kind() == ASTKind::AddrField);
    auto barFoo = addrField.child(0);
    ASSERT(barFoo.kind() == ASTKind::AddrField);
    ASSERT(addrField.child(1).kind() == ASTKind::Ident);
}

TEST(resolve_nested_set_index) {
    auto artifact = RESOLVE(R"(
var arr: [[1, 2], [3, 4]]
arr[0][1] = 2
)");

    Module* module = artifact.as<Module>();
    auto topLevel = module->getTopLevel();

    auto setIndex = topLevel.child(1);
    ASSERT(setIndex.kind() == ASTKind::SetIndex);
    auto base = setIndex.child(0);
    ASSERT(base.kind() == ASTKind::AddrIndex);
    ASSERT(setIndex.child(1).kind() == ASTKind::Unsigned);
    ASSERT(setIndex.child(1).uintConst() == 1);
    ASSERT(setIndex.child(2).kind() == ASTKind::Unsigned);
    ASSERT(setIndex.child(2).uintConst() == 2);
}

TEST(resolve_nested_addr_index) {
    auto artifact = RESOLVE(R"(
var arr: [[1, 2], [3, 4]]
&arr[0][1]
)");

    Module* module = artifact.as<Module>();
    auto topLevel = module->getTopLevel();

    auto addrIndex = topLevel.child(1);
    ASSERT(addrIndex.kind() == ASTKind::AddrIndex);
    auto base = addrIndex.child(0);
    ASSERT(base.kind() == ASTKind::AddrIndex);
    ASSERT(addrIndex.child(1).kind() == ASTKind::Unsigned);
    ASSERT(addrIndex.child(1).uintConst() == 1);
}

TEST(resolve_bare_typename_parameter) {
    auto artifact = RESOLVE(R"(
i32 foo(i32)
i64 bar(i64): 42
x baz(x)

alias x: i32
)");

    Module* module = artifact.as<Module>();
    auto topLevel = module->getTopLevel();

    auto foo = topLevel.child(0);
    auto fooParam = foo.child(2).child(0);
    ASSERT(fooParam.kind() == ASTKind::VarDecl);
    ASSERT(fooParam.child(0).kind() == ASTKind::GlobalTypename);
    ASSERT(fooParam.child(1).missing());
    ASSERT(fooParam.type(module) == module->i32Type());
    ASSERT(foo.type().as<TypeKind::Function>().parameterType(0) == module->i32Type());

    auto bar = topLevel.child(1);
    auto barParam = bar.child(2).child(0);
    ASSERT(barParam.kind() == ASTKind::VarDecl);
    ASSERT(barParam.child(0).kind() == ASTKind::GlobalTypename);
    ASSERT(barParam.child(1).missing());
    ASSERT(barParam.type(module) == module->i64Type());
    ASSERT(bar.type().as<TypeKind::Function>().parameterType(0) == module->i64Type());

    auto baz = topLevel.child(2);
    auto bazParam = baz.child(2).child(0);
    ASSERT(bazParam.kind() == ASTKind::VarDecl);
    ASSERT(bazParam.child(0).kind() == ASTKind::GlobalTypename);
    ASSERT(bazParam.child(1).missing());
    ASSERT(bazParam.type(module) == module->i32Type());
    ASSERT(baz.type().as<TypeKind::Function>().parameterType(0) == module->i32Type());
}

TEST(resolve_new_expression) {
    auto artifact = RESOLVE(R"(
var x: [1, 2, 3]
new 1
new 1 + 2
new x[0]
new i32[4]
new i32[x]
new i32
new x
)");

    Module* module = artifact.as<Module>();
    auto topLevel = module->getTopLevel();

    auto first = topLevel.child(1);
    ASSERT(first.kind() == ASTKind::New);
    ASSERT(first.child(0).kind() == ASTKind::Unsigned);

    auto second = topLevel.child(2);
    ASSERT(second.kind() == ASTKind::New);
    ASSERT(second.child(0).kind() == ASTKind::Add);

    auto third = topLevel.child(3);
    ASSERT(third.kind() == ASTKind::New);
    ASSERT(third.child(0).kind() == ASTKind::GetIndex);

    auto fourth = topLevel.child(4);
    ASSERT(fourth.kind() == ASTKind::NewArray);
    ASSERT(fourth.type() == module->sliceType(Own | Uninit, I32));
    ASSERT(fourth.child(0).kind() == ASTKind::Unsigned);

    auto fifth = topLevel.child(5);
    ASSERT(fifth.kind() == ASTKind::NewArray);
    ASSERT(fifth.type() == module->sliceType(Own | Uninit, I32));
    ASSERT(fifth.child(0).kind() == ASTKind::Global);

    auto sixth = topLevel.child(6);
    ASSERT(sixth.kind() == ASTKind::New);
    ASSERT(sixth.type() == module->ptrType(Own | Uninit, I32));
    ASSERT(sixth.child(0).missing());

    auto seventh = topLevel.child(7);
    ASSERT(seventh.kind() == ASTKind::New);
    ASSERT(seventh.child(0).kind() == ASTKind::Global);
}

TEST(resolve_type_import) {
    auto artifact = RESOLVE(R"(
type Foo:
    case Bar: i32
    case Baz
use Foo.Bar
Bar bar: Bar(42)
)");
}

TEST(resolve_type_wildcard) {
    auto artifact = RESOLVE(R"(
type Foo:
    case Bar: i32
    case Baz
use Foo.*
Bar bar: Bar(42)
Baz baz: Baz()
)");
}

TEST(resolve_out_of_order_pointer_function) {
    auto artifact = RESOLVE(R"(
var x: foo(&42)
i32* foo(i32* p): p
)");
}

TEST(resolve_method_style_own_uninit_cast) {
    auto artifact = RESOLVE(R"(
var x: 42

var a: x.own i32*()
var b: x.own uninit i32*()
var c: x.own i32[]()
var d: x.uninit own i32[]()
var e: x.own uninit i32*[](i32)[]()
var f: x.own uninit i32*(uninit own i32*)(own i32[])*()
)");

    auto module = artifact.as<Module>();
    auto topLevel = module->getTopLevel();

    auto a = topLevel.child(1).child(2);
    ASSERT_EQUAL(a.type(), module->ptrType(Own, I32));

    auto b = topLevel.child(2).child(2);
    ASSERT_EQUAL(b.type(), module->ptrType(Own | Uninit, I32));

    auto c = topLevel.child(3).child(2);
    ASSERT_EQUAL(c.type(), module->sliceType(Own, I32));

    auto d = topLevel.child(4).child(2);
    ASSERT_EQUAL(d.type(), module->sliceType(Own | Uninit, I32));

    auto e = topLevel.child(5).child(2);
    ASSERT_EQUAL(e.type(), module->sliceType(Own | Uninit, module->funType(module->sliceType(module->ptrType(I32)), I32)));

    auto f = topLevel.child(6).child(2);
    ASSERT_EQUAL(f.type(), module->ptrType(Own | Uninit, module->funType(module->funType(module->ptrType(I32), module->ptrType(Own | Uninit, I32)), module->sliceType(Own, I32))));
}