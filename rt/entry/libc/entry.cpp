#include "rt/sys.h"
#include "stdlib.h"

extern "C" void lib_init();
extern "C" void lib_deinit();

struct Initer {
    inline Initer() {
        lib_init();
    }
};

static Initer initer;
