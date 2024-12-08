#ifndef UTILITY_FUNC_H
#define UTILITY_FUNC_H

#include "rt/def.h"
#include "util/rc.h"

template <typename T>
struct func_interface;

template<typename Result, typename... Args>
struct func_interface<Result(Args...)> {
    virtual ~func_interface() {}
    virtual auto operator()(Args... args) -> Result = 0;
};

template <typename T, typename U>
struct func_impl;

template<typename T>
struct Copy {
    const T& ref;
};

template<typename Result, typename... Args, typename Func>
struct func_impl<Result(Args...), Func> : public func_interface<Result(Args...)> {
    Func func;

    inline func_impl(Func&& func_in):
        func(move(func_in)) {}

    inline func_impl(Copy<Func> func_in):
        func(func_in.ref) {}

    auto operator()(Args... args) -> Result override {
        return func(forward<decltype(args)>(args)...);
    }
};

template<typename T>
struct func;

struct Valid { using Type = u32; };

template<typename T>
struct IsNotSelf { using Type = Valid; };

template<typename T>
struct IsNotSelf<func<T>> { using Type = void; };

template<typename Result, typename... Args>
struct func<Result(Args...)> {
    rc<func_interface<Result(Args...)>> callable;

    inline func(func&& func_in):
        callable(move(func_in.callable)) {}

    inline func(const func& func_in):
        callable(func_in.callable) {}

    inline func& operator=(func&& other) {
        callable = move(other.callable);
    }

    inline func& operator=(const func& other) {
        callable = other.callable;
    }
    
    template<typename Func, typename = typename IsNotSelf<remove_reference<Func>>::Type::Type>
    inline func(Func&& func_in):
        callable(rc<func_impl<Result(Args...), Func>>(forward<decltype(func_in)>(func_in)).operator rc<func_interface<Result(Args...)>>()) {}

    template<typename Func, typename = typename IsNotSelf<remove_reference<Func>>::Type::Type>
    inline func(const Func& func_in):
        callable(rc<func_impl<Result(Args...), Func>>(Copy { func_in }).operator rc<func_interface<Result(Args...)>>()) {}

    inline virtual ~func() {}

    virtual auto operator()(Args... args) -> Result {
        return (*callable)(forward<decltype(args)>(args)...);
    }
};

template<typename Container, typename Func>
Container apply(const Container& container, Func&& func) {
    Container result = container;
    for (auto& x : result)
        x = func(x);
    return result;
}

template<typename Container, typename Func>
Container& apply_mut(Container& container, Func&& func) {
    for (auto& x : container)
        x = func(x);
    return container;
}

template<typename Container, typename Func>
void foreach(const Container& container, Func&& func) {
    for (const auto& x : container)
        func(x);
}

template<typename Container, typename Func, typename T>
T fold(const Container& container, Func&& func, T init) {
    for (const auto& x : container)
        init = func(init, x);
    return init;
}

template<typename Container, typename Func, typename T>
T foldr(const Container& container, Func&& func, T init) {
    for (const auto& x : container)
        init = func(x, init);
    return init;
}

template<typename Container, typename Func>
auto reduce(const Container& container, Func&& func) -> decltype(func(*container.begin(), *container.begin())) {
    auto it = container.begin();
    assert(it != container.end());
    auto it2 = it;
    auto iterable = span(++ it2, container.end());
    return fold(iterable, forward<Func>(func), *it);
}

template<typename Container>
auto sum(const Container& container) {
    return reduce(container, [](const auto& x, const auto& y) -> auto { return x + y; });
}

template<typename Container>
auto product(const Container& container) {
    return reduce(container, [](const auto& x, const auto& y) -> auto { return x * y; });
}

#endif