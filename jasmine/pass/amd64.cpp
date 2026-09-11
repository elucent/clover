#include "jasmine/pass/amd64.h"

#ifndef RT_AMD64
#error "Shouldn't be including jasmine/pass/amd64.h on non-amd64 platform."
#endif

#include "asm/arch/amd64.h"

namespace jasmine::amd64 {
    constexpr static RegSet GPS = AMD64Assembler::gps(), FPS = AMD64Assembler::fps();

    RegSet
        NULLARY_OP[1] = { {} },
        UNARY_INT_OP[1] = { GPS },
        BINARY_INT_OP[2] = { GPS, GPS },
        TERNARY_INT_OP[3] = { GPS, GPS, GPS },
        DIV_INT_OP[3] = { AMD64Assembler::RAX, GPS, GPS },
        SHIFT_INT_OP[3] = { GPS, GPS, AMD64Assembler::RCX },
        INT_ISBOUNDS_OP[4] = { GPS, GPS, GPS, GPS },
        INT_BRANCH_OP[4] = { GPS, GPS, {}, {} },
        INT_BRBOUNDS_OP[5] = { GPS, GPS, GPS, {}, {} },
        UNARY_FLOAT_OP[1] = { FPS },
        BINARY_FLOAT_OP[2] = { FPS, FPS },
        TERNARY_FLOAT_OP[3] = { FPS, FPS, FPS },
        FLOAT_COMPARE_OP[3] = { GPS, FPS, FPS },
        FLOAT_BRANCH_OP[4] = { FPS, FPS, {}, {} },
        FLOAT_ISBOUNDS_OP[4] = { GPS, FPS, FPS, FPS },
        FLOAT_BRBOUNDS_OP[5] = { FPS, FPS, FPS, {}, {} },
        BOOL_BRANCH_OP[3] = { GPS, {}, {} },
        CONVERT_INT_OP[3] = { GPS, {}, GPS },
        CONVERT_FLOAT_OP[3] = { FPS, {}, FPS },
        CONVERT_INT_TO_FLOAT_OP[3] = { FPS, {}, GPS },
        CONVERT_FLOAT_TO_INT_OP[3] = { GPS, {}, FPS },
        LOAD_FLOAT_OP[2] = { FPS, GPS },
        STORE_FLOAT_OP[2] = { GPS, FPS },
        INT_LOAD_FIELD_OP[3] = { GPS, GPS, {} },
        INT_STORE_FIELD_OP[3] = { GPS, {}, GPS },
        INT_LOAD_INDEX_OP[4] = { GPS, GPS, {}, GPS },
        INT_STORE_INDEX_OP[4] = { GPS, {}, GPS, GPS },
        INT_GET_FIELD_OP[3] = { GPS, {}, {} },
        INT_SET_FIELD_OP[3] = { {}, {}, GPS },
        INT_GET_INDEX_OP[4] = { GPS, {}, {}, GPS },
        INT_SET_INDEX_OP[4] = { {}, {}, GPS, GPS },
        FLOAT_LOAD_FIELD_OP[3] = { FPS, GPS, {} },
        FLOAT_STORE_FIELD_OP[3] = { GPS, {}, FPS },
        FLOAT_LOAD_INDEX_OP[4] = { FPS, GPS, {}, GPS },
        FLOAT_STORE_INDEX_OP[4] = { GPS, {}, GPS, FPS },
        FLOAT_GET_FIELD_OP[3] = { FPS, {}, {} },
        FLOAT_SET_FIELD_OP[3] = { {}, {}, FPS },
        FLOAT_GET_INDEX_OP[4] = { FPS, {}, {}, GPS },
        FLOAT_SET_INDEX_OP[4] = { {}, {}, GPS, FPS };
}