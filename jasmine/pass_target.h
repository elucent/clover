#ifndef BASIL_JASMINE_PASS_TARGET_H
#define BASIL_JASMINE_PASS_TARGET_H

#include "jasmine/pass.h"

MODULE(jasmine)

template<typename Target>
void regalloc(PassInfo& info, Function& fn) {
    //
}

template<typename Target>
void preisel(PassInfo& info, Function& fn) {
    //
}

template<typename Target>
void lower(PassInfo& info, Function& fn, Assembly& as) {
    //
}

ENDMODULE()

#endif