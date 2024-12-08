#include "jasmine/test/common.h"

FOR_EACH_INT(DEFINE_TEST, load_static_scalar, 42,
    mod.values.defStatic(mod.syms["global"], mod.values.makeInt((TypeKind)type, 42));
    auto bb0 = fn.addBlock();
    bb0.addNode(Opcode::LOAD, type, fn.variable("x"), fn.stat(cstring("global")));
    bb0.addNode(Opcode::RET, type, fn.variable("x"));
);

FOR_EACH_INT(DEFINE_TEST, addr_static_scalar, 42,
    mod.values.defStatic(mod.syms["global"], mod.values.makeInt((TypeKind)type, 42));
    auto bb0 = fn.addBlock();
    bb0.addNode(Opcode::MOV, PTR, fn.variable("p"), fn.stat(cstring("global")));
    bb0.addNode(Opcode::LOAD, type, fn.variable("x"), fn.variable("p"));
    bb0.addNode(Opcode::RET, type, fn.variable("x"));
);

FOR_EACH_INT(DEFINE_TEST, modify_static_scalar, 84,
    mod.values.defStatic(mod.syms["global"], mod.values.makeInt((TypeKind)type, 42));
    auto bb0 = fn.addBlock();
    bb0.addNode(Opcode::LOAD, type, fn.variable("x"), fn.stat(cstring("global")));
    bb0.addNode(Opcode::LOAD, type, fn.variable("y"), fn.stat(cstring("global")));
    bb0.addNode(Opcode::ADD, type, fn.variable("x"), fn.variable("x"), fn.variable("y"));
    bb0.addNode(Opcode::STORE, type, fn.stat(cstring("global")), fn.variable("x"));
    bb0.addNode(Opcode::LOAD, type, fn.variable("x"), fn.stat(cstring("global")));
    bb0.addNode(Opcode::RET, type, fn.variable("x"));
);