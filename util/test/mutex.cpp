#include "util/thread.h"
#include "util/test/harness.h"

TEST(simple_thread) {
    SKIP_IF_USING_LIBC;
    thread<void(i32)> intPrinter([](i32 i) { println(i); }, 42);
}

TEST(simple_thread_on_heap) {
    SKIP_IF_USING_LIBC;
    thread<void(i32)>* intPrinter = new thread<void(i32)>([](i32 i) { println(i); }, 42);
    delete intPrinter;
}

TEST(two_threads_at_once) {
    SKIP_IF_USING_LIBC;
    thread<void(i32)> a([](i32 i) { println(i); }, 42);
    thread<void(i32)> b([](i32 i) { println(i); }, 43);
}

TEST(four_threads_at_once) {
    SKIP_IF_USING_LIBC;
    thread<void(i32)> a([](i32 i) { println(i); }, 42);
    thread<void(i32)> b([](i32 i) { println(i); }, 43);
    thread<void(i32)> c([](i32 i) { println(i); }, 44);
    thread<void(i32)> d([](i32 i) { println(i); }, 45);
}

// TEST(simple_mutex) {
//     i32 x;
//     mutex xGuard;

//     vec<thread<void(i32*, mutex*)>*> threads;
//     for (u32 i = 0; i < 2; i ++) {
//         threads.push(new thread<void(i32*, mutex*)>([](i32* data, mutex* lock) {
//             lock->acquire();
//             *data ++;
//             lock->release();
//         }, &x, &xGuard));
//     }
//     for (u32 i = 0; i < threads.size(); i ++)
//         delete threads[i];

//     ASSERT(x == 128);
// }