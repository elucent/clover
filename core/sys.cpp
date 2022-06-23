#include "core/sys.h"
#include "lib/io.h"
#include "lib/init.h"

extern i32 main(i32, i8**);

struct context {
    iptr gp_regs[16];
    void (*rip)();
};

struct thread {
    slice<page> stack;
    context ctx;
};

extern "C" iptr pclone(i32(*function)(void*), void* stack, void* arg);

static iptr waiting_queue_size;
static thread** waiting_queue_base;
static thread** waiting_queue_start;
static thread** waiting_queue_end;

static inline thread** wrap_waiting_queue(thread** ptr) {
    return waiting_queue_base + (ptr - waiting_queue_base & waiting_queue_size - 1);
}

static void grow_waiting_queue() {
    iptr new_size = waiting_queue_size * 2;
    thread** new_base = (thread**)mreq(new_size * sizeof(thread*) / PAGESIZE).ptr;
    thread** new_end = new_base;
    while (waiting_queue_start != waiting_queue_end) {
        *new_end = *waiting_queue_start;
        waiting_queue_start = wrap_waiting_queue(waiting_queue_start + 1);
        new_end ++;
    }
    mfree({ (page*)waiting_queue_base, waiting_queue_size * (iptr)sizeof(thread*) / PAGESIZE });
    waiting_queue_base = waiting_queue_start = new_base;
    waiting_queue_end = new_end;
    waiting_queue_size = new_size;
}

static void enqueue_thread(thread* t) {
    thread** next = wrap_waiting_queue(waiting_queue_end + 1);
    if (next == waiting_queue_start) {
        grow_waiting_queue();
        next = waiting_queue_end + 1;
    }
    *waiting_queue_end = t;
    waiting_queue_end = next;
}

static thread* dequeue_thread() {
    if (waiting_queue_start == waiting_queue_end) return nullptr; // empty queue
    thread* result = *waiting_queue_start;
    waiting_queue_start = wrap_waiting_queue(waiting_queue_start + 1);
    return result;
}

static void thread_enclosure(void (*function)(void*), void* data) {
    function(data);
}

static thread* thread_heap;
static thread* thread_heap_top;
static thread* thread_free_list;

static thread* create_thread(void (*function)(void*), void* data) {
    thread* t;
    if (thread_free_list) {
        t = thread_free_list;
        thread_free_list = *(thread**)thread_free_list;
    }
    else {
        if (thread_heap_top == thread_heap + PAGESIZE)
            ; // allocate more space?
        t = thread_heap_top;
        thread_heap_top ++;
    }

    t->stack = mreq(1);
    t->ctx.gp_regs[5] /* rbp */ = (iptr)t->stack.ptr;
    t->ctx.gp_regs[4] /* rsp */ = (iptr)t->stack.ptr;
    t->ctx.rip = (void(*)())function;
    t->ctx.gp_regs[6] /* rdi */ = (iptr)data;
    return t;
}

static void destroy_thread(thread* t) {
    mfree(t->stack);
    *(thread**)t = thread_free_list;
    thread_free_list = (thread*)t;
}

extern "C" void spawn(void (*function)(void*), void* data) {
    enqueue_thread(create_thread(function, data));
}

static constexpr iptr N_OS_THREADS = 4;
iptr os_tids[N_OS_THREADS];
context thread_contexts[N_OS_THREADS];
thread* active_threads[N_OS_THREADS];
page* os_stacks[N_OS_THREADS];
iptr alive[N_OS_THREADS];

static iptr os_thread_lock, thread_queue_lock, os_io_lock;

static void sig_cont(i32 sig) {
    return;
}

extern "C" iptr _tgid = 0;
extern "C" iptr getpid();
extern "C" iptr gettid();
extern "C" void pyield(iptr tid);
extern "C" void psignal(iptr sig, iptr tid);
extern "C" void pswitch(context* cur, context* next);
extern "C" void phandle(iptr sig, void* action);

static i32 os_thread_run(void* id_in) {
    iptr id = (iptr)id_in;
    os_tids[id] = gettid();

    while (true) {
        plock(&os_thread_lock);
        write_string(stdout, "thread woke ", 12);
        write_int(stdout, id);
        write_byte(stdout, '\n');
        if (active_threads[id]) {
            thread* next = active_threads[id];
            write_string(stdout, "thread found work ", 18);
            write_int(stdout, id);
            write_byte(stdout, '\n');
            punlock(&os_thread_lock);
            pswitch(thread_contexts + id, &next->ctx);
        }
        else {
            write_string(stdout, "thread waiting ", 15);
            write_int(stdout, id);
            write_byte(stdout, '\n');
            punlock(&os_thread_lock);
        }
        psignal(10, os_tids[0]); // signal main thread after we finish a task or fail to find one
        pyield(os_tids[id]);
    }

    psignal(10, os_tids[0]); // signal main thread
    pexit(0);
    return 0;
}

static constexpr bool threaded = false;

extern "C" void _start_impl(i32 argc, i8** argv) {
    lib_init();

    if (threaded) {
        waiting_queue_size = 4 * PAGESIZE / sizeof(thread*);
        waiting_queue_base = waiting_queue_start = waiting_queue_end = (thread**)mreq(4).ptr;
        thread_heap = thread_heap_top = (thread*)mreq(sizeof(thread)).ptr;
        thread_free_list = nullptr;

        _tgid = getpid();
        os_thread_lock = 0;
        thread_queue_lock = 0;
        os_io_lock = 0;

        spawn((void(*)(void*))main, nullptr);

        os_tids[0] = gettid();
        active_threads[0] = nullptr; // scheduler thread never has a task
        for (iptr i = 1; i < N_OS_THREADS; i ++) alive[i] = true;
        for (iptr i = 1; i < N_OS_THREADS; i ++) {
            iptr stack = (iptr)mreq(256).ptr;
            os_stacks[i] = (page*)stack;
            os_tids[i] = -1;
            pclone(os_thread_run, (void*)((u8*)stack + 256 * PAGESIZE), (void*)i);
        }
        
        bool still_running = true;
        while (still_running) {
            write_string(stdout, "main thread woke!\n", 18);
            plock(&thread_queue_lock);
            still_running = waiting_queue_start != waiting_queue_end;
            for (iptr i = 1; i < N_OS_THREADS; i ++) {
                if (active_threads[i]) still_running = true;
                else {
                    active_threads[i] = dequeue_thread();
                    if (active_threads[i]) psignal(10, os_tids[i]);
                }
            }
            write_string(stdout, "main thread sleeping\n", 22);
            punlock(&thread_queue_lock);
            if (still_running) pyield(os_tids[0]);
        }
    }
    else {
        main(argc, argv);
    }

    mfree({ (page*)thread_heap, sizeof(thread) });
    pexit(0);
}

extern "C" void __cxa_pure_virtual() {}
extern "C" void __cxa_atexit() {}
extern "C" void __cxx_global_var_init() {}
extern "C" void __dso_handle() {}