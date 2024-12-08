#ifndef UTILITY_THREAD_H
#define UTILITY_THREAD_H

#include "rt/def.h"
#include "util/deque.h"
#include "util/ptrhax.h"

template<typename T>
struct thread;

template<typename T, typename... Args>
struct thread<T(Args...)> {
    process::thread tid;

    template<u32 N, typename U, typename... Args2>
    struct DataTuple : public DataTuple<N - 1, Args2...> {
        using Type = U;
        Type item;

        template<u32 M>
        struct NthType;

        template<>
        struct NthType<0> {
            using Type = U;
        };

        template<u32 M>
        struct NthType {
            using Type = typename DataTuple<N - 1, Args2...>::template NthType<M - 1>::Type;
        };

        template<u32 M>
        auto get() -> typename NthType<M>::Type& {
            return DataTuple<N - 1, Args2...>::template get<M - 1>();
        }

        template<>
        auto get<0>() -> typename NthType<0>::Type& {
            return item;
        }

        template<u32 M>
        auto get() const -> const typename NthType<M>::Type& {
            return DataTuple<N - 1, Args2...>::template get<M - 1>();
        }

        template<>
        auto get<0>() const -> const typename NthType<0>::Type& {
            return item;
        }
    };

    template<typename U>
    struct DataTuple<1, U> {
        using Type = U;
        Type item;

        template<u32 M>
        struct NthType;

        template<>
        struct NthType<0> {
            using Type = U;
        };

        template<u32 M>
        auto get() -> U&;

        template<>
        auto get<0>() -> U& {
            return item;
        }

        template<u32 M>
        auto get() const -> const U&;

        template<>
        auto get<0>() const -> const U& {
            return item;
        }
    };  
    
    template<typename U, U... Indices>
    struct IntegerSeq
    {
      static constexpr u32 size() noexcept { return sizeof...(Indices); }
    };

    template<u32... Indices>
    static T apply(T(*func)(Args...), DataTuple<sizeof...(Args), Args...>& data, IntegerSeq<u32, Indices...>) {
        return func(data.template get<Indices>()...);
    }

    static void task(iword parameters) {
        i8* buffer = bitcast<i8*>(parameters);

        T(*func)(Args...);
        memory::copy(&func, buffer, sizeof(func));
        buffer += sizeof(func);

        DataTuple<sizeof...(Args), Args...> data;
        readParameters<sizeof...(Args)>(data, buffer);
        delete[] buffer;

        apply(func, data, __make_integer_seq<IntegerSeq, u32, sizeof...(Args)>{});
    }

    template<typename>
    inline static constexpr u32 sizeOfParameters() {
        return 0;
    }

    template<typename, typename U, typename... Args2>
    inline static constexpr u32 sizeOfParameters() {
        return sizeof(U) + sizeOfParameters<int, Args2...>();
    }

    template<u32 N>
    inline static void readParameters(DataTuple<sizeof...(Args), Args...>& data, const i8* buffer) {
        auto& ref = data.template get<sizeof...(Args) - N>();
        using U = remove_reference<decltype(ref)>;
        memory::copy(&ref, buffer, sizeof(U));
        buffer += sizeof(U);
        readParameters<N - 1>(data, buffer);
    }

    template<>
    inline static void readParameters<0>(DataTuple<sizeof...(Args), Args...>&, const i8*) {}

    inline static void writeParameters(i8*) {}

    template<typename U, typename... Args2>
    inline static void writeParameters(i8* buffer, const U& t, const Args2&... args) {
        memory::copy(buffer, &t, sizeof(U));
        writeParameters(buffer + sizeof(U), args...);
    }

    inline thread(T(*func)(Args...), Args... args) {
        i8* originalBuffer = new i8[sizeof(func) + sizeOfParameters<Args...>()];
        i8* buffer = originalBuffer;

        uptr funcPtr = bitcast<uptr>(func);
        memory::copy(buffer, &funcPtr, sizeof(uptr));

        writeParameters(buffer + sizeof(func), args...);
        tid = process::spawn(task, bitcast<iword>(originalBuffer), 16, 1, 0);
    }

    inline ~thread() {
        process::await(tid);
    }
};

struct mutex {
    struct Locks {
        i8 lock, queue_lock;
    };
    ptr_tuple<deque<process::thread>*, Locks> data;

    inline mutex(): 
        data(nullptr, Locks { 0, 0 }) {}
    
    inline ~mutex() {
        assert(!data.getPtr() || !data.getPtr()->size());
        delete data.getPtr();
    }

    mutex(const mutex&): mutex() {}

    mutex(mutex&& other) = delete;
    mutex& operator=(const mutex&) = delete;
    mutex& operator=(mutex&&) = delete;

    inline deque<process::thread>& ensureQueue() {
        if UNLIKELY(!data.getPtr())
            data.setPtr(new deque<process::thread>());
        return *data.getPtr();
    }

    void acquire() {
        if (process::try_lock(&(data.refExtra().lock)))
            return;
        process::thread current = process::current();
        process::lock(&(data.refExtra().queue_lock));
        ensureQueue().pushr(current);
        process::unlock(&(data.refExtra().queue_lock));
        process::sleep();
    }

    void release() {
        process::lock(&(data.refExtra().queue_lock));
        process::thread next_waiter = ensureQueue().popl();
        process::unlock(&(data.refExtra().queue_lock));
        process::unlock(&(data.refExtra().lock));
        process::wake(next_waiter);
    }
};

template<typename T, u32 N>
struct channel {
    u8 buffer[sizeof(T) * N];
    u32 start, end;
    mutex m;
};

#endif