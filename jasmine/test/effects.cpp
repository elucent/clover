#include "jasmine/test/common.h"

TEST(tiny_int_set_empty_all) {
    ASSERT_EQUAL(TinyIntSet().intersects(TinyIntSet()), false);
    ASSERT_EQUAL(TinyIntSet().intersects(TinyIntSet::all()), false);
    ASSERT_EQUAL(TinyIntSet::all().intersects(TinyIntSet()), false);
    ASSERT_EQUAL(TinyIntSet::all().intersects(TinyIntSet::all()), true);

    TinyIntSet a;
    a.add(TinyIntSet::all());
    ASSERT_EQUAL(TinyIntSet().intersects(a), false);
    ASSERT_EQUAL(TinyIntSet::all().intersects(a), true);

    TinyIntSet b = TinyIntSet::all();
    b.add(TinyIntSet());
    ASSERT_EQUAL(TinyIntSet().intersects(b), false);
    ASSERT_EQUAL(TinyIntSet::all().intersects(b), true);
}

TEST(tiny_int_set_singleton) {
    for (u32 i = 0; i < 15; i ++) for (u32 j = 0; j < 15; j ++)
        ASSERT_EQUAL(TinyIntSet(i).intersects(TinyIntSet(j)), i == j);
    for (u32 i = 0; i < 15; i ++) for (u32 j = 0; j < 15; j ++)
        ASSERT_EQUAL(TinyIntSet(0x7ff0 + i).intersects(TinyIntSet(0x7ff0 + j)), i == j);
}

TEST(tiny_int_set_bits_only) {
    for (u32 i = 0; i < 15; i ++) for (u32 j = 0; j < 15; j ++) {
        TinyIntSet s;
        s.add(i);
        s.add(j);
        for (u32 k = 0; k < 15; k ++) for (u32 l = 0; l < 15; l ++) {
            TinyIntSet t;
            t.add(k);
            t.add(l);
            ASSERT_EQUAL(s.intersects(t), i == k || j == k || i == l || j == l);
        }
    }
    for (u32 i = 0; i < 15; i ++) for (u32 j = 0; j < 15; j ++) {
        TinyIntSet s;
        s.add(0x7ff0 + i);
        s.add(0x7ff0 + j);
        for (u32 k = 0; k < 15; k ++) for (u32 l = 0; l < 15; l ++) {
            TinyIntSet t;
            t.add(0x7ff0 + k);
            t.add(0x7ff0 + l);
            ASSERT_EQUAL(s.intersects(t), i == k || j == k || i == l || j == l);
        }
    }
}

TEST(tiny_int_set_bits_to_range) {
    for (u32 i = 0; i < 64; i ++) for (u32 j = 0; j < 64; j ++) {
        TinyIntSet s;
        for (u32 x = min(i, j); x < max(i, j); x ++) s.add(x);
        for (u32 k = 0; k < 64; k ++) for (u32 l = 0; l < 64; l ++) {
            TinyIntSet t;
            for (u32 y = min(k, l); y < max(k, l); y ++) t.add(y);
            ASSERT_EQUAL(s.intersects(t), i != j && k != l && !(min(k, l) >= max(i, j) || min(i, j) >= max(k, l)));
        }
    }
    for (u32 i = 0; i < 64; i ++) for (u32 j = 0; j < 64; j ++) {
        TinyIntSet s;
        for (u32 x = min(i, j); x < max(i, j); x ++) s.add(0x7fb0 + x);
        for (u32 k = 0; k < 64; k ++) for (u32 l = 0; l < 64; l ++) {
            TinyIntSet t;
            for (u32 y = min(k, l); y < max(k, l); y ++) t.add(0x7fb0 + y);
            ASSERT_EQUAL(s.intersects(t), i != j && k != l && !(min(k, l) >= max(i, j) || min(i, j) >= max(k, l)));
        }
    }
}

DEFINE_TEST(load_after_store, 42, i32, I32,
    auto bb0 = fn.addBlock();
    bb0.addNode(Opcode::MOV, I32, fn.variable("x"), fn.intConst(20));
    bb0.addNode(Opcode::ADDR, PTR, fn.variable("p"), fn.variable("x"));
    bb0.addNode(Opcode::LOAD, I32, fn.variable("y"), fn.variable("p"));
    bb0.addNode(Opcode::ADD, I32, fn.variable("w"), fn.variable("y"), fn.intConst(2));
    bb0.addNode(Opcode::STORE, I32, fn.variable("p"), fn.variable("w"));
    bb0.addNode(Opcode::LOAD, I32, fn.variable("z"), fn.variable("p"));
    bb0.addNode(Opcode::ADD, I32, fn.variable("y"), fn.variable("y"), fn.variable("z"));
    bb0.addNode(Opcode::RET, I32, fn.variable("y"));
);

DEFINE_TEST(reference_local_struct, 126, i32, I32,
    TypeIndex tripleType = StructBuilder(I32, I32, I32).build(fn.typeContext());
    auto bb0 = fn.addBlock();
    bb0.addNode(Opcode::VAR, tripleType, fn.variable("v"));
    bb0.addNode(Opcode::ADDR_FIELD, tripleType, fn.variable("p0"), fn.variable("v"), fn.intConst(0));
    bb0.addNode(Opcode::ADDR_FIELD, tripleType, fn.variable("p1"), fn.variable("v"), fn.intConst(1));
    bb0.addNode(Opcode::ADDR_FIELD, tripleType, fn.variable("p2"), fn.variable("v"), fn.intConst(2));
    bb0.addNode(Opcode::STORE, I32, fn.variable("p0"), fn.intConst(42));
    bb0.addNode(Opcode::STORE, I32, fn.variable("p1"), fn.intConst(42));
    bb0.addNode(Opcode::STORE, I32, fn.variable("p2"), fn.intConst(42));
    bb0.addNode(Opcode::GET_FIELD, tripleType, fn.variable("x"), fn.variable("v"), fn.intConst(0));
    bb0.addNode(Opcode::GET_FIELD, tripleType, fn.variable("y"), fn.variable("v"), fn.intConst(0));
    bb0.addNode(Opcode::GET_FIELD, tripleType, fn.variable("z"), fn.variable("v"), fn.intConst(0));
    bb0.addNode(Opcode::ADD, I32, fn.variable("x"), fn.variable("x"), fn.variable("y"));
    bb0.addNode(Opcode::ADD, I32, fn.variable("x"), fn.variable("x"), fn.variable("z"));
    bb0.addNode(Opcode::RET, I32, fn.variable("x"));
);

DEFINE_TEST(reference_struct_across_branch, 42, i32, I32,
    TypeIndex pairType = StructBuilder(I32, I32).build(fn.typeContext());
    auto bb0 = fn.addBlock(), bb1 = fn.addBlock(), bb2 = fn.addBlock(), bb3 = fn.addBlock();
    bb0.addNode(Opcode::SET_FIELD, pairType, fn.variable("p"), fn.intConst(0), fn.intConst(0));
    bb0.addNode(Opcode::SET_FIELD, pairType, fn.variable("p"), fn.intConst(1), fn.intConst(0));
    bb0.addNode(Opcode::BR_IF, I32, fn.intConst(1), fn.branch(bb0, bb1), fn.branch(bb0, bb2));
    bb1.addNode(Opcode::ADDR_FIELD, pairType, fn.variable("a"), fn.variable("p"), fn.intConst(0));
    bb1.addNode(Opcode::BR, VOID, fn.branch(bb1, bb3));
    bb2.addNode(Opcode::ADDR_FIELD, pairType, fn.variable("a"), fn.variable("p"), fn.intConst(1));
    bb2.addNode(Opcode::BR, VOID, fn.branch(bb2, bb3));
    bb3.addNode(Opcode::STORE, I32, fn.variable("a"), fn.intConst(42));
    bb3.addNode(Opcode::GET_FIELD, pairType, fn.variable("x"), fn.variable("p"), fn.intConst(0));
    bb3.addNode(Opcode::RET, I32, fn.variable("x"));
);

DEFINE_SIMPLE_TEST(pointer_escape_through_call, 0, i32, I32,
    Module mod("test");
    {
        Function& fn = mod.defineFunction(VOID, "inc");
        fn.addParameters(PTR, "p");
        auto bb0 = fn.addBlock();
        bb0.addNode(Opcode::LOAD, I32, fn.variable("x"), fn.variable("p"));
        bb0.addNode(Opcode::ADD, I32, fn.variable("x"), fn.variable("x"), fn.intConst(1));
        bb0.addNode(Opcode::STORE, I32, fn.variable("p"), fn.variable("x"));
        bb0.addNode(Opcode::RET, VOID);
    }
    {
        Function& fn = mod.defineFunction(I32, "calls_inc");
        TypeIndex funcType = FunctionBuilder(VOID, PTR).build(fn.typeContext());
        auto bb0 = fn.addBlock();
        bb0.addNode(Opcode::MOV, I32, fn.variable("x"), fn.intConst(41));
        bb0.addNode(Opcode::ADDR, PTR, fn.variable("p"), fn.variable("x"));
        bb0.addNode(Opcode::CALL_VOID, funcType, fn.func("inc"), fn.variable("p"));
        bb0.addNode(Opcode::RET, I32, fn.variable("x"));
    }

    COMPILE(linked);
    auto calls_inc = linked.lookup<i32()>("calls_inc");
    ASSERT_EQUAL(calls_inc(), 42);
);

DEFINE_SIMPLE_TEST(pointer_escape_through_parameter, 0, i32, I32,
    Module mod("test");
    Function& fn = mod.defineFunction(VOID, "store");
    fn.addParameters(PTR, "p", PTR, "q");
    auto bb0 = fn.addBlock();
    bb0.addNode(Opcode::STORE, PTR, fn.variable("q"), fn.variable("p"));
    bb0.addNode(Opcode::RET, VOID);

    COMPILE(linked);
    auto store = linked.lookup<void(i32*, i32**)>("store");
    i32 p = 42;
    i32* q = nullptr;
    store(&p, &q);
    ASSERT_EQUAL(*q, 42);
);
