#include "rt/linux/common.h"
#include "asm/prctl.h"
#include "asm/ldt.h"
#include "asm/signal.h"
#include "sched.h"

namespace process {
    using namespace memory;

    enum Syscall {
        SigAction = 13,
        SigProcmask = 14,
        SigReturn = 15,
        SchedYield = 24,
        Pause = 34,
        GetPid = 39,
        Clone = 56,
        Fork = 57,
        Execve = 59,
        Exit = 60,
        Wait4 = 61,
        ArchPrctl = 158,
        GetTid = 186,
        TgKill = 234,
        Clone3 = 435,
    };

    struct Channel {
        i8 mutex, start, end;
        iword buffer[CHANNEL_BUFFER_SIZE / sizeof(iword) - 1];

        inline void lock();
        inline void unlock();
        inline bool isempty() const;
        inline bool isfull() const;
        inline iword dequeue();
        inline bool enqueue(iword msg);
    };

    struct Thread {
        Channel channels[0];
        iword alignToChannelSize[CHANNEL_BUFFER_SIZE / sizeof(iword) - 8];
        iword pad;
        bool running;
        iword id;
        iword num_channels;
        i32 native_tid;
        u32 page_count;
        i8* base;
        i8* stack;
        i8* tls;
    };

    struct CloneArgs {
        u64 flags;
        i32* pidfd;
        i32* child_tid;
        i32* parent_tid;
        u64 exit_signal;
        i8* stack;
        u64 stack_size;
        i8* tls;
    };

    static_assert(sizeof(Thread) == sizeof(Channel));
    extern i8 thread_table_lock ASMLABEL("process.thread_table_lock");
    extern Thread* thread_table[65536] ASMLABEL("process.thread_table");
    extern thread main_thread ASMLABEL("process.main_thread");
    extern u32 thread_count ASMLABEL("process.thread_count");
    extern iword clone_impl(void* args, iword size, void* stack) ASMLABEL("process.clone_impl");
    extern i32 pid ASMLABEL("process.pid");
    extern "C" iptr _initial_sp ASMLABEL("memory.initial_sp");
    extern void sig_return() ASMLABEL("process.sig_return");
    extern void exit_and_unmap_stack(slice<page> pages, iword code) ASMLABEL("process.exit_and_unmap_stack");

    void sigaction(i32 sig, void* handler) ASMLABEL("process.sigaction");
    void sigaction(i32 sig, void* handler) {
        struct sigaction action;
        memory::fill(&action.sa_mask, 0, sizeof(sigset_t));
        action.sa_handler = (__sighandler_t)handler;
        action.sa_flags = SA_RESTORER|SA_RESTART;
        action.sa_restorer = &sig_return;
        os_syscall(SigAction, sig, (iword)&action, 0, 8);
    }

    extern "C" void lib_deinit();
    extern "C" void thread_deinit();

    void exit(iword code) ASMLABEL("process.exit");
    void exit(iword code) {
        Thread* t = (Thread*)tls() - 1;
        thread_deinit();
        lock(&thread_table_lock);
        thread_count --;
        if (!thread_count)
            lib_deinit();
        thread_table[t->id] = nullptr;
        unlock(&thread_table_lock);
        exit_and_unmap_stack({ (page*)t->base, t->page_count }, 0);
    }

    void setup_main_thread() ASMLABEL("process.setup_main_thread");
    void setup_main_thread() {
        lock(&thread_table_lock);
        main_thread = 0;
        pid = os_syscall(GetPid);
        assert(!thread_table[main_thread]);

        slice<page> memory = memory::map(2);
        if (!memory.data()) {
            unlock(&thread_table_lock);
            main_thread = -1;
            return;
        }

        slice<page> header_space = { &memory[0], 1 };
        Thread* thread_header = (Thread*)((i8*)&header_space.last() + PAGESIZE - sizeof(Channel));
        slice<page> tls_space = { &memory[1], 1 };
        thread_header->running = true;
        thread_header->id = main_thread;
        thread_header->num_channels = 0;
        thread_header->page_count = 2;
        thread_header->native_tid = os_syscall(GetTid);
        thread_header->base = (i8*)header_space.data();
        thread_header->stack = (i8*)_initial_sp;
        thread_header->tls = (i8*)tls_space.data();
        os_syscall(ArchPrctl, ARCH_SET_FS, (iword)&thread_header->tls);
        thread_table[main_thread] = thread_header;
        thread_count ++;
        unlock(&thread_table_lock);
    }

    thread current() ASMLABEL("process.current");
    thread current() {
        return ((Thread*)tls() - 1)->id;
    }

    thread spawn(void (*task)(iword), iword parameter, iword stack, iword tls, iword channels) ASMLABEL("process.spawn");
    thread spawn(void (*task)(iword), iword parameter, iword stack, iword tls, iword channels) {
        lock(&thread_table_lock);
        thread result = -1;
        for (iword i = 0; i < 65536; i ++) {
            if (!thread_table[i]) {
                result = i;
                break;
            }
        }
        if (result == -1) {
            unlock(&thread_table_lock);
            return result;
        }

        iword header_pages = (sizeof(Thread) + channels * sizeof(Channel) + PAGESIZE - 1) / PAGESIZE;
        tls += 1; // At least one page needed for runtime purposes.
        iword pages = stack + tls + header_pages;
        slice<page> memory = memory::map(pages);
        if (!memory.data()) {
            unlock(&thread_table_lock);
            return result;
        }
        slice<page> header_space = { &memory[0], header_pages };
        Thread* thread_header = (Thread*)((i8*)&header_space.last() + PAGESIZE - sizeof(Channel));
        slice<page> tls_space = { &memory[header_pages], tls };
        slice<page> stack_space = { &memory[tls + header_pages], stack };
        thread_header->running = true;
        thread_header->id = result;
        thread_header->num_channels = channels;
        thread_header->page_count = pages;
        thread_header->base = (i8*)header_space.data();
        thread_header->stack = (i8*)stack_space.data();
        thread_header->tls = (i8*)tls_space.data();

        CloneArgs args;
        args.flags = CLONE_VM | CLONE_FS | CLONE_FILES | CLONE_SIGHAND | CLONE_THREAD | CLONE_SYSVSEM | CLONE_SETTLS | CLONE_PARENT_SETTID | CLONE_CHILD_CLEARTID;
        args.pidfd = nullptr;
        args.child_tid = &thread_header->native_tid;
        args.parent_tid = &thread_header->native_tid;
        args.exit_signal = 0;
        args.stack = thread_header->stack;
        args.stack_size = stack * PAGESIZE;
        args.tls = (i8*)&thread_header->tls;
        thread_table[result] = thread_header;

        iword* stack_words = (iword*)(args.stack + stack * PAGESIZE);
        stack_words[-1] = (iword)task;
        stack_words[-2] = parameter;
        stack_words[-3] = result;
        iword* tls_words = (iword*)tls_space.data();
        tls_words[0] = 42;
        thread_count ++;
        unlock(&thread_table_lock);
        iword clone_result = clone_impl(&args, sizeof(args), stack_words);
        if (clone_result == -1) {
            lock(&thread_table_lock);
            memory::unmap(memory);
            thread_table[result] = nullptr;
            unlock(&thread_table_lock);
            return -1;
        }
        assert(clone_result); // Should be in parent
        return result;
    }

    inline static Channel& get_channel(iword channel) {
        return ((Channel*)tls())[-channel - 2];
    }

    inline static Channel& get_channel(thread th, iword channel) {
        return ((Channel*)thread_table[th]->tls)[-channel - 2];
    }

    inline void Channel::lock() {
        process::lock(&this->mutex);
    }

    inline void Channel::unlock() {
        process::unlock(&this->mutex);
    }

    inline bool Channel::isempty() const {
        return start == end;
    }

    inline bool Channel::isfull() const {
        return end == (start == 1 ? CHANNEL_BUFFER_SIZE - 1 : start - 1);
    }

    inline iword Channel::dequeue() {
        iword i = buffer[i32(start)];
        start ++;
        if UNLIKELY(start >= CHANNEL_BUFFER_SIZE)
            start = 1;
        return i;
    }

    inline bool Channel::enqueue(iword msg) {
        if (isfull())
            return false;
        buffer[i32(end ++)] = msg;
        if UNLIKELY(end >= CHANNEL_BUFFER_SIZE)
            end = 1;
        return true;
    }

    bool send(thread t, iword channel, iword message) ASMLABEL("process.send");
    bool send(thread t, iword channel, iword message) {
        Channel& chan = get_channel(t, channel);
        chan.lock();
        bool result = chan.enqueue(message);
        chan.unlock();
        return result;
    }

    bool isempty(iword channel) ASMLABEL("process.isempty");
    bool isempty(iword channel) {
        Channel& chan = get_channel(channel);
        chan.lock();
        bool result = chan.isempty();
        chan.unlock();
        return result;
    }

    iword receive(iword channel) ASMLABEL("process.receive");
    iword receive(iword channel) {
        Channel& chan = get_channel(channel);
        chan.lock();
        while (chan.isempty()) {
            chan.unlock();
            os_syscall(SchedYield);
            chan.lock();
        }
        iword result = chan.dequeue();
        chan.unlock();
        return result;
    }

    void await(thread t) ASMLABEL("process.await");
    void await(thread t) {
        lock(&thread_table_lock);
        while (thread_table[t]) {
            unlock(&thread_table_lock);
            // os_syscall(SchedYield);
            lock(&thread_table_lock);
        }
        unlock(&thread_table_lock);
    }

    void sleep() ASMLABEL("process.sleep");
    void sleep() {
        lock(&thread_table_lock);
        Thread& t = ((Thread*)tls())[-1];
        t.running = false;
        unlock(&thread_table_lock);
        os_syscall(Pause);
        lock(&thread_table_lock);
        t.running = true;
        unlock(&thread_table_lock);
    }

    void wake(thread t) ASMLABEL("process.wake");
    void wake(thread t) {
        lock(&thread_table_lock);
        if (!thread_table[t]) {
            unlock(&thread_table_lock);
            return;
        }
        i32 tid = thread_table[t]->native_tid;
        os_syscall(TgKill, pid, tid, SIGRTMIN + 1);
        unlock(&thread_table_lock);
    }

    bool running(thread t) ASMLABEL("process.running");
    bool running(thread t) {
        lock(&thread_table_lock);
        if (!thread_table[t]) {
            unlock(&thread_table_lock);
            return false;
        }
        bool result = thread_table[t]->running;
        unlock(&thread_table_lock);
        return result;
    }

    extern const i8** envp ASMLABEL(".ENVP");

    i32 exec(const_slice<i8> path, const_slice<const_slice<i8>> argv) ASMLABEL("process.exec");
    i32 exec(const_slice<i8> path, const_slice<const_slice<i8>> argv) {
        constexpr u32 pathBufferSize = 16384u;
        i8 pathBuffer[pathBufferSize];
        if (path.size() >= pathBufferSize - 1)
            return -1;
        memory::copy(pathBuffer, path.data(), path.size());
        pathBuffer[path.size()] = '\0';

        constexpr u32 argBufferSize = 32768u;
        constexpr u32 argvBufferSize = 16384u / sizeof(const i8*);
        i8 argBuffer[argBufferSize];
        const i8* argvPointers[argvBufferSize];
        if (argv.size() >= argvBufferSize - 2)
            return -1;

        argvPointers[0] = pathBuffer;
        u32 offset = 0;
        for (u32 i = 0; i < argv.size(); i ++) {
            if (offset + argv[i].size() + 1 >= argBufferSize)
                return -1;
            memory::copy(argBuffer + offset, argv[i].data(), argv[i].size());
            argBuffer[offset + argv[i].size()] = '\0';
            argvPointers[i + 1] = argBuffer + offset;
            offset += argv[i].size() + 1;
        }
        argvPointers[argv.size() + 1] = nullptr;

        iword forkResult = os_syscall(Fork);
        if (!forkResult) {
            // In child.
            os_syscall(Execve, (iword)pathBuffer, (iword)argvPointers, (iword)envp);
            return -1; // Shouldn't return.
        } else {
            // In parent.
            i32 result;
            os_syscall(Wait4, forkResult, (iword)&result, 0, 0);
            return result;
        }
    }
}