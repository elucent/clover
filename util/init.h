#ifndef UTILITY_INIT_H
#define UTILITY_INIT_H

extern "C" void lib_init();
extern "C" void lib_deinit();

extern "C" void thread_init();
extern "C" void thread_deinit();

void atexit(void(*func)());

#endif