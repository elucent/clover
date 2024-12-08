#include "util/test/harness.h"
#include "asm/arch/amd64.h"

using Assembler = AMD64LinuxAssembler;

struct ScopedState {
    void* state;
    ~ScopedState() {
        Assembler::finish_placing_parameters(state);
    }
};

struct ParameterRunner {
    void* state;
    inline ParameterRunner() {
        state = Assembler::start_placing_parameters();
    }

    inline ~ParameterRunner() {
        Assembler::finish_placing_parameters(state);
    }

    template<typename Func>
    inline void operator<<(Func&& func) const {
        func(state);
    }
};
#define parameters ParameterRunner() << [&](void* state)

TEST(linux_amd64_pass_one_integer) {
    parameters {
        ASSERT(Assembler::place_scalar_parameter(state, Repr::Scalar(Size::BITS8)) == GP(Assembler::RDI));
    };
    parameters {
        ASSERT(Assembler::place_scalar_parameter(state, Repr::Scalar(Size::BITS16)) == GP(Assembler::RDI));
    };
    parameters {
        ASSERT(Assembler::place_scalar_parameter(state, Repr::Scalar(Size::BITS32)) == GP(Assembler::RDI));
    };
    parameters {
        ASSERT(Assembler::place_scalar_parameter(state, Repr::Scalar(Size::BITS64)) == GP(Assembler::RDI));
    };
}

TEST(linux_amd64_pass_two_integers) {
    parameters {
        ASSERT(Assembler::place_scalar_parameter(state, Repr::Scalar(Size::BITS8)) == GP(Assembler::RDI));
        ASSERT(Assembler::place_scalar_parameter(state, Repr::Scalar(Size::BITS8)) == GP(Assembler::RSI));
    };
    parameters {
        ASSERT(Assembler::place_scalar_parameter(state, Repr::Scalar(Size::BITS16)) == GP(Assembler::RDI));
        ASSERT(Assembler::place_scalar_parameter(state, Repr::Scalar(Size::BITS16)) == GP(Assembler::RSI));
    };
    parameters {
        ASSERT(Assembler::place_scalar_parameter(state, Repr::Scalar(Size::BITS32)) == GP(Assembler::RDI));
        ASSERT(Assembler::place_scalar_parameter(state, Repr::Scalar(Size::BITS32)) == GP(Assembler::RSI));
    };
    parameters {
        ASSERT(Assembler::place_scalar_parameter(state, Repr::Scalar(Size::BITS64)) == GP(Assembler::RDI));
        ASSERT(Assembler::place_scalar_parameter(state, Repr::Scalar(Size::BITS64)) == GP(Assembler::RSI));
    };
}