#ifndef BASIL_TEST_HARNESS_H
#define BASIL_TEST_HARNESS_H

#include "rt/def.h"
#include "util/config.h"
#include "util/vec.h"
#include "util/hash.h"
#include "util/sort.h"
#include "util/io.h"
#include "util/func.h"

extern const i8* current_test_name;

#ifdef RT_LIBC_COMPATIBLE
#define SKIP_IF_USING_LIBC return
#else
#define SKIP_IF_USING_LIBC do {} while (false)
#endif

inline bool lexicographic_less(const i8* const& a, const i8* const& b) {
    auto len_a = findc(a, 0), len_b = findc(b, 0);
    auto len = min<iword>(len_a, len_b);
    auto compare = memory::compare(a, b, len_a);
    if (compare < 0) return true;
    if (compare == 0)
        return len_a < len_b;
    return false;
}

struct OverallResultList {
    i32 n_tests = 0, n_passed = 0, total_tests = 0;
    vec<const i8*> failing_tests;

    inline void pass() {
        n_tests ++;
        n_passed ++;
        if (config::printTestNames)
            println(BOLDGREEN "✓ " RESET);
    }

    inline void fail(const i8* failed) {
        n_tests ++;
        failing_tests.push(failed);
        if (config::printTestNames)
            println(BOLDRED "✘ " RESET);
    }

    inline void summarize() {
        bool failing = n_passed < n_tests;
        println("[RESULT] Passed ", failing ? "\e[0;91m" : "\e[0;92m", n_passed, " / ", n_tests, "\e[0m");
        sort(failing_tests.begin(), failing_tests.end(), lexicographic_less);
        for (auto failure : failing_tests) {
            println("[" BOLDRED "FAILED" RESET "]     ", failure);
        }
    }
};

struct TestResults {
    vec<func<void()>> results;

    inline TestResults() {}

    inline TestResults(OverallResultList& summary, const i8* name) {
        current_test_name = name;
        print("[TEST]   (", summary.n_tests + 1, " / ", summary.total_tests);
        if (summary.n_passed < summary.n_tests)
            print(", ", summary.n_tests - summary.n_passed, " failures) ", current_test_name);
        else
            print(") ", current_test_name);
        if (config::printTestNames)
            print(" ... ");
        else
            print("\33[2K\r");
        flush(file::stdout);
    }

    inline void finish(OverallResultList& summary) {
        if (results.size())
            summary.fail(current_test_name);
        else
            summary.pass();
        for (auto& result : results) result();
    }
};

extern map<const i8*, void(*)(TestResults&)> test_map;
extern vec<entry<const i8*, void(*)(TestResults&)>> test_list;

#define TEST(name) \
    NOINLINE extern "C" __attribute((used)) void test_##name##_fn(TestResults& results)
#define ASSERT(...) do { if (!(__VA_ARGS__)) { results.results.push([=](){ println("[" BOLDRED "FAILED" RESET "] " __FILE__ ":" TOSTRING(__LINE__) ": ", #__VA_ARGS__); }); if (config::crashOnTestFailure) { results.results.pop()(); crash(); } return; } } while (false)
#define ASSERT_WITH_MESSAGE(expr, ...) do { if (!(expr)) { results.results.push([=](){ println("[" BOLDRED "FAILED" RESET "] " __FILE__ ":" TOSTRING(__LINE__) ": ", __VA_ARGS__); }); if (config::crashOnTestFailure) { results.results.pop()(); crash(); } return; } } while (false)
#define ASSERT_EQUAL(exp1, exp2) do { auto r1 = (exp1); auto r2 = (exp2); if (!(r1 == r2)) { results.results.push([=](){ println("[" BOLDRED "FAILED" RESET "] "  __FILE__ ":" TOSTRING(__LINE__) ": ", r1, " " GRAY, #exp1, RESET " != ", r2, " " GRAY, #exp2, RESET ""); }); if (config::crashOnTestFailure) { results.results.pop()(); crash(); } return; } } while (false)
#define EPSILON 0.00000001
#define ASSERT_FLOAT_EQUAL(exp1, exp2) do { auto r1 = (exp1); auto r2 = (exp2); if (abs(r1 - r2) >= EPSILON) { results.results.push([=](){ println("[" BOLDRED "FAILED" RESET "] "  __FILE__ ":" TOSTRING(__LINE__) ": ", r1, " " GRAY, #exp1, RESET " != ", r2, " " GRAY, #exp2, RESET ""); }); if (config::crashOnTestFailure) { results.results.pop()(); crash(); } return; } } while (false)

inline void setup_harness(i32 argc, char** argv) {
    new (&test_map) decltype(test_map)();
    new (&test_list) decltype(test_list)();
}

inline void sort_tests() {
    sort(test_list.begin(), test_list.end(), [](const entry<const i8*, void(*)(TestResults&)>& lhs, const entry<const i8*, void(*)(TestResults&)>& rhs) -> bool { return lexicographic_less(lhs.key, rhs.key); });
}

inline void parse_arguments_in_harness(i32 argc, char** argv) {
    parseOptions(argc, argv);
    bool sawRealTestSpecifier = false;
    for (i32 i = 1; i < argc; i ++) {
        i32 globPos = -1;
        if (argv[i][0] == '-') {
            if (findc(argv[i], 0) >= 6 && !memory::compare(argv[i], "--list", 6)) {
                for (const auto& test : test_map)
                    println(test.key);
                process::exit(0);
            }
            continue;
        }
        for (i32 j = 0; argv[i][j]; j ++) {
            sawRealTestSpecifier = true;
            if (argv[i][j] == '*') {
                if (globPos == -1)
                    globPos = j;
                else {
                    println("[ERROR]  Invalid test selection: only one wildcard (*) is allowed in the test list currently.");
                    process::exit(1);
                }
            }
        }
        if (globPos != -1) {
            const_slice<i8> prefix = { argv[i], globPos };
            const_slice<i8> suffix = { argv[i] + globPos + 1, findc(argv[i], 0) - globPos - 1 };
            for (const auto& p : test_map) {
                const_slice<i8> name = { p.key, findc(p.key, 0) };
                if (name.size() < prefix.size() + suffix.size())
                    continue;
                for (i32 i = 0; i < prefix.size(); i ++)
                    if (name[i] != prefix[i])
                        goto fail;
                for (i32 i = 1; i <= suffix.size(); i ++) {
                    if (name[-i] != suffix[-i])
                        goto fail;
                }
                test_list.push(p);
                fail:;
            }
        } else {
            sawRealTestSpecifier = true;
            auto it = test_map.find(argv[i]);
            if (it != test_map.end())
                test_list.push(*it);
        }
    }
    if (!sawRealTestSpecifier)
        for (const auto& p : test_map) test_list.push(p);
}

#endif