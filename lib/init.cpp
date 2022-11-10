#include "lib/init.h"
#include "lib/io.h"
#include "lib/malloc.h"
#include "lib/gc.h"

static u8 arena_buf[sizeof(arena)];
static u8 alloc_buf[sizeof(allocator)];

extern "C" void lib_init() {
    gc_init();
    io_init();
    arena::instance = new(arena_buf) arena();
    allocator::instance = new(alloc_buf) allocator();
}

extern "C" void lib_deinit() {
    io_deinit();
    gc_deinit();
    arena::instance->~arena();
    allocator::instance->~allocator();
}