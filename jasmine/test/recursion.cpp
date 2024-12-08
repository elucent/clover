#include "jasmine/test/common.h"

FOR_EACH_INT(DEFINE_SIMPLE_TEST, factorial, 0, 
    Module mod("test");
    Function& fn = mod.defineFunction(type, "factorial");
    fn.addParameters(type, "x");
    auto bb0 = fn.addBlock(), bb1 = fn.addBlock(), bb2 = fn.addBlock();
    bb0.addNode(Opcode::BR_EQ, type, fn.variable("x"), fn.intConst(0), fn.branch(fn.addEdge(bb0, bb1)), fn.branch(fn.addEdge(bb0, bb2)));
    bb1.addNode(Opcode::RET, type, fn.intConst(1));
    bb2.addNode(Opcode::SUB, type, fn.variable("y"), fn.variable("x"), fn.intConst(1));
    bb2.addNode(Opcode::CALL, FunctionBuilder(type, type).build(fn.typeContext()), fn.variable("y"), fn.func("factorial"), fn.variable("y"));
    bb2.addNode(Opcode::MUL, type, fn.variable("x"), fn.variable("x"), fn.variable("y"));
    bb2.addNode(Opcode::RET, type, fn.variable("x"));

    COMPILE(linked);
    auto factorial = linked.lookup<ctype(ctype)>("factorial");
    ASSERT(factorial(0) == 1);
    ASSERT(factorial(1) == 1);
    ASSERT(factorial(2) == 2);
    ASSERT(factorial(5) == 120);
);

FOR_EACH_INT(DEFINE_SIMPLE_TEST, fibonacci, 0,
    Module mod("test");
    Function& fn = mod.defineFunction(type, "fibonacci");
    fn.addParameters(type, "x");
    auto bb0 = fn.addBlock(), bb1 = fn.addBlock(), bb2 = fn.addBlock();
    bb0.addNode(Opcode::BR_LT, type, fn.variable("x"), fn.intConst(2), fn.branch(fn.addEdge(bb0, bb1)), fn.branch(fn.addEdge(bb0, bb2)));
    bb1.addNode(Opcode::RET, type, fn.variable("x"));
    bb2.addNode(Opcode::SUB, type, fn.variable("y"), fn.variable("x"), fn.intConst(1));
    bb2.addNode(Opcode::CALL, FunctionBuilder(type, type).build(fn.typeContext()), fn.variable("y"), fn.func("fibonacci"), fn.variable("y"));
    bb2.addNode(Opcode::SUB, type, fn.variable("z"), fn.variable("x"), fn.intConst(2));
    bb2.addNode(Opcode::CALL, FunctionBuilder(type, type).build(fn.typeContext()), fn.variable("z"), fn.func("fibonacci"), fn.variable("z"));
    bb2.addNode(Opcode::ADD, type, fn.variable("y"), fn.variable("y"), fn.variable("z"));
    bb2.addNode(Opcode::RET, type, fn.variable("y"));

    COMPILE(linked);
    auto fibonacci = linked.lookup<ctype(ctype)>("fibonacci");
    ASSERT(fibonacci(0) == 0);
    ASSERT(fibonacci(1) == 1);
    ASSERT(fibonacci(2) == 1);
    ASSERT(fibonacci(10) == 55);
);