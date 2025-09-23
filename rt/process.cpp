#include "rt/sys.h"

#ifdef RT_LINUX
#include "sched.h"
#include "signal.h"
#include "unistd.h"
#include "sys/wait.h"
#endif

namespace process {
    using namespace memory;

    extern "C" void lib_init();
    extern "C" void lib_deinit();

    extern "C" void thread_init();
    extern "C" void thread_deinit();

    static i8* const* envp;
    static i8* const* argv;

    void init(i32 argc, i8** argv, i8** envp) ASMLABEL("process.init");
    void init(i32 argc, i8** argv, i8** envp) {
        process::envp = (i8* const*)envp;
        process::argv = (i8* const*)argv;
        lib_init();
    }

    void exit(i32 code) ASMLABEL("process.exit");
    void exit(i32 code) {
        thread_deinit();
        lib_deinit();

        #ifdef RT_LINUX
            _exit(code);
        #else
            #error "Unimplemented syscall for platform: exit"
        #endif
    }

    bool try_lock(i8* mutex) ASMLABEL("process.try_lock");
    bool try_lock(i8* mutex) {
        bool result;

        #ifdef RT_GCC_COMPATIBLE
            // TODO: Relax sequential consistency maybe.
            result = __atomic_test_and_set(mutex, __ATOMIC_SEQ_CST);
        #elif defined(RT_AMD64)
            asm volatile (
                "\tlock btsq $0, 0(%1)\n"
                "\tsetnc %0\n"
                : "=r"(result)
                : "r"(mutex)
                :
            );
        #else
            #error "Unimplemented syscall for platform: try_lock"
        #endif

        return result;
    }

    void lock(i8* mutex) ASMLABEL("process.lock");
    void lock(i8* mutex) {
        #ifdef RT_GCC_COMPATIBLE
            while (!try_lock(mutex)) {}
        #elif defined(RT_AMD64)
            asm volatile (
                "0:\n"
                "\tlock btsq $0, 0(%0)\n"
                "\tjc 1f\n"
                "\tjmp 2f\n"
                "1:\n"
                "\ttestq $1, 0(%0)\n"
                "\tjz 0b\n"
                "\tjmp 1b\n"
                "2:\n"
                :
                : "r"(mutex)
                :
            );
        #else
            #error "Unimplemented syscall for platform: lock"
        #endif
    }

    void unlock(i8* mutex) ASMLABEL("process.unlock");
    void unlock(i8* mutex) {
        #ifdef RT_GCC_COMPATIBLE
            i8 val = 0;
            __atomic_store(mutex, &val, __ATOMIC_SEQ_CST);
        #elif defined(RT_AMD64)
            asm volatile (
                "\tmovb $0, 0(%0)\n"
                :
                : "r"(mutex)
                :
            );
        #else
            #error "Unimplemented syscall for platform: unlock"
        #endif
    }

    thread_local i8 tls_array[65536];

    void* tls() ASMLABEL("process.tls");
    void* tls() {
        return tls_array;
    }

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
        i8* argvPointers[argvBufferSize];
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

        #ifdef RT_LINUX
            i32 forkResult = fork();
            if (!forkResult) {
                // In child.
                execve(pathBuffer, argvPointers, envp);
                return -1; // Shouldn't return.
            } else {
                // In parent.
                i32 result;
                waitpid(forkResult, &result, 0);
                return result;
            }
        #else
            #error "Unimplemented syscall for platform: fork"
        #endif
    }
}