#include "jasmine/test/common.h"

FOR_EACH_INT(DEFINE_SIMPLE_TEST, reduce_commute_add, 0,
    Module mod("test");
    Function& fn = mod.defineFunction(type, "add");
    fn.addParameters(type, "x", type, "y");
    auto bb0 = fn.addBlock();
    bb0.addNode(Opcode::ADD, type, fn.variable("x"), fn.intConst(1), fn.variable("x"));
    bb0.addNode(Opcode::ADD, type, fn.variable("y"), fn.intConst(2), fn.variable("y"));
    bb0.addNode(Opcode::ADD, type, fn.variable("x"), fn.variable("x"), fn.variable("y"));
    bb0.addNode(Opcode::RET, type, fn.variable("x"));
    COMPILE(linked);
    auto func = linked.lookup<ctype(ctype, ctype)>("add");
    ASSERT(func(4, 5) == 12);
);

FOR_EACH_INT(DEFINE_SIMPLE_TEST, reduce_combine_add, 0,
    Module mod("test");
    Function& fn = mod.defineFunction(type, "add");
    fn.addParameters(type, "x", type, "y");
    auto bb0 = fn.addBlock();
    bb0.addNode(Opcode::ADD, type, fn.variable("x"), fn.intConst(1), fn.variable("x"));
    bb0.addNode(Opcode::ADD, type, fn.variable("y"), fn.intConst(2), fn.variable("y"));
    bb0.addNode(Opcode::ADD, type, fn.variable("x"), fn.variable("x"), fn.variable("y"));
    bb0.addNode(Opcode::RET, type, fn.variable("x"));
    COMPILE(linked);
    auto func = linked.lookup<ctype(ctype, ctype)>("add");
    ASSERT(func(4, 5) == 12);
);

FOR_EACH_INT(DEFINE_SIMPLE_TEST, reduce_add_zero, 0,
    Module mod("test");
    Function& fn = mod.defineFunction(type, "add");
    fn.addParameters(type, "x");
    auto bb0 = fn.addBlock();
    bb0.addNode(Opcode::ADD, type, fn.variable("x"), fn.variable("x"), fn.intConst(0));
    bb0.addNode(Opcode::ADD, type, fn.variable("x"), fn.variable("x"), fn.intConst(0));
    bb0.addNode(Opcode::ADD, type, fn.variable("x"), fn.variable("x"), fn.intConst(0));
    bb0.addNode(Opcode::RET, type, fn.variable("x"));
    COMPILE(linked);
    auto func = linked.lookup<ctype(ctype)>("add");
    ASSERT(func(42) == 42);
);

FOR_EACH_SINT(DEFINE_SIMPLE_TEST, reduce_neg_combine, 0,
    Module mod("test");
    Function& fn = mod.defineFunction(type, "neg");
    fn.addParameters(type, "x");
    auto bb0 = fn.addBlock();
    bb0.addNode(Opcode::NEG, type, fn.variable("x"), fn.variable("x"));
    bb0.addNode(Opcode::NEG, type, fn.variable("x"), fn.variable("x"));
    bb0.addNode(Opcode::RET, type, fn.variable("x"));
    COMPILE(linked);
    auto func = linked.lookup<ctype(ctype)>("neg");
    ASSERT(func(42) == 42);
);

FOR_EACH_INT(DEFINE_SIMPLE_TEST, reduce_sub_zero, 0,
    Module mod("test");
    Function& fn = mod.defineFunction(type, "sub");
    fn.addParameters(type, "x");
    auto bb0 = fn.addBlock();
    bb0.addNode(Opcode::SUB, type, fn.variable("x"), fn.variable("x"), fn.intConst(0));
    bb0.addNode(Opcode::SUB, type, fn.variable("x"), fn.variable("x"), fn.intConst(0));
    bb0.addNode(Opcode::SUB, type, fn.variable("x"), fn.variable("x"), fn.intConst(0));
    bb0.addNode(Opcode::RET, type, fn.variable("x"));
    COMPILE(linked);
    auto func = linked.lookup<ctype(ctype)>("sub");
    ASSERT(func(42) == 42);
);

FOR_EACH_INT(DEFINE_SIMPLE_TEST, reduce_sub_from_zero, 0,
    Module mod("test");
    Function& fn = mod.defineFunction(type, "sub");
    fn.addParameters(type, "x");
    auto bb0 = fn.addBlock();
    bb0.addNode(Opcode::SUB, type, fn.variable("x"), fn.intConst(0), fn.variable("x"));
    bb0.addNode(Opcode::SUB, type, fn.variable("x"), fn.intConst(0), fn.variable("x"));
    bb0.addNode(Opcode::RET, type, fn.variable("x"));
    COMPILE(linked);
    auto func = linked.lookup<ctype(ctype)>("sub");
    ASSERT(func(42) == 42);
);

FOR_EACH_INT(DEFINE_SIMPLE_TEST, reduce_sub_combine, 0,
    Module mod("test");
    Function& fn = mod.defineFunction(type, "sub");
    fn.addParameters(type, "x", type, "y");
    auto bb0 = fn.addBlock();
    bb0.addNode(Opcode::SUB, type, fn.variable("x"), fn.variable("x"), fn.intConst(1));
    bb0.addNode(Opcode::SUB, type, fn.variable("y"), fn.variable("y"), fn.intConst(2));
    bb0.addNode(Opcode::SUB, type, fn.variable("x"), fn.variable("x"), fn.variable("y"));
    bb0.addNode(Opcode::RET, type, fn.variable("x"));
    COMPILE(linked);
    auto func = linked.lookup<ctype(ctype, ctype)>("sub");
    ASSERT(func(4, 5) == 0);
);
