#include "lib/init.h"
#include "lib/io.h"
#include "lib/malloc.h"
#include "lib/gc.h"

static u8 arena_buf[sizeof(arena)];

extern "C" void lib_init() {
    gc_init();
    io_init();
    arena::instance = new(arena_buf) arena();
}

extern "C" void lib_deinit() {
    io_deinit();
    gc_deinit();
    arena::instance->~arena();
}