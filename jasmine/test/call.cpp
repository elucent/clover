#include "jasmine/test/common.h"

FOR_EACH_INT(DEFINE_SIMPLE_TEST, call_identity, 0, 
    Module mod("test");
    Function& id = mod.defineFunction(type, "identity");
    id.addParameters(type, "x");
    auto id_bb0 = id.addBlock();
    id_bb0.addNode(Opcode::RET, type, id.variable("x"));
    
    Function& caller = mod.defineFunction(type, "caller");
    caller.addParameters(type, "x");
    auto call_bb0 = caller.addBlock();
    call_bb0.addNode(Opcode::CALL, FunctionBuilder(type, type).build(caller.typeContext()), caller.variable("x"), caller.func("identity"), caller.variable("x"));
    call_bb0.addNode(Opcode::RET, type, caller.variable("x"));

    COMPILE(linked);
    auto func = linked.lookup<ctype(ctype)>("caller");
    ASSERT(func(42) == 42);
);