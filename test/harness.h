#ifndef BASIL_TEST_HARNESS_H
#define BASIL_TEST_HARNESS_H

#include "core/def.h"
#include "lib/vec.h"

extern const i8* current_test_name;

struct OverallResultList {
    i32 n_tests = 0, n_passed = 0;
    vec<pair<const i8*, const i8*>> failing_tests;

    inline void pass() {
        n_tests ++;
        n_passed ++;
    }

    inline void fail(const i8* object, const i8* failed) {
        n_tests ++;
        failing_tests.push({ object, failed });
    }

    inline void summarize() {
        bool failing = n_passed < n_tests;
        println("[RESULT] Passed ", failing ? "\e[0;91m" : "\e[0;92m", n_passed, " / ", n_tests, "\e[0m");
        for (auto failure : failing_tests) {
            println("[FAILED]     ", failure.first, ":", failure.second);
        }
    }
};

struct TestResults {
    vec<void(*)()> results;

    inline TestResults(const i8* name) {
        current_test_name = name;
        print("[TEST]       ", current_test_name, " ... ");
    }

    inline void finish(const i8* obj, OverallResultList& summary) {
        if (results.size()) {
            println("\e[0;91m✗\e[0m");
            summary.fail(obj, current_test_name);
        }
        else {
            println("\e[0;92m✓\e[0m");
            summary.pass();
        }
        for (auto result : results) result();
    }
};

#define TEST(name) extern "C" void test_##name##_fn(TestResults& results)
#define ASSERT(expr, ...) do { if (!(expr)) results.results.push([](){ println("[FAILED] " __FILE__ " line " TOSTRING(__LINE__) ": ", __VA_ARGS__); }); } while (false)

inline void setup_harness(int argc, char** argv) {
    //
}

#endif