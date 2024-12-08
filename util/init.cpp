#include "util/init.h"
#include "util/io.h"
#include "util/elumalloc.h"

struct AtExitList {
    void(*func)();
    AtExitList* next;

    void* operator new(size_t n) {
        return bitcast<AtExitList*>(elumalloc::allocate(n));
    }
};

static AtExitList* exit_funcs;

extern "C" void lib_init() {
    io_init();
    thread_init();
    exit_funcs = nullptr;
}

extern "C" void lib_deinit() {
    while (exit_funcs) {
        (exit_funcs->func)();
        exit_funcs = exit_funcs->next;
    }
    thread_deinit();
    io_deinit();
}

extern "C" void thread_init() {
    //
}

extern "C" void thread_deinit() {
    //
}

void atexit(void(*func)()) {
    exit_funcs = new AtExitList{ func, exit_funcs };
}