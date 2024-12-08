#include "util/config.h"
#include "util/hash.h"
#include "util/vec.h"
#include "util/io.h"
#include "util/str.h"

namespace config {
#define DEFINE_OPTION(name, optname, type, ...) type name;
    FOR_EACH_OPTION(DEFINE_OPTION)
#undef DEFINE_OPTION
};

template<typename T>
void parseOption(T& opt, const i8* str);

bool string_equal(const i8* a, const i8* b) {
    return findc(a, 0) == findc(b, 0) && !memory::compare(a, b, findc(a, 0));
}

template<>
void parseOption<bool>(bool& opt, const i8* str) {
    while (*str != '=') {
        str ++;
        if (!*str) {
            opt = true;
            return;
        }
    }
    str ++;
    if (!memory::compare(str, "1", 2) || !memory::compare(str, "true", 5) || !memory::compare(str, "yes", 4))
        opt = true;
    else if (!memory::compare(str, "0", 2) || !memory::compare(str, "false", 6) || !memory::compare(str, "no", 3))
        opt = false;
}

template<>
void parseOption<i32>(i32& opt, const i8* str) {
    while (*str != '=') {
        str ++;
        if (!*str)
            panic("Expected time unit as a string.");
    }
    str ++;
    opt = toint(cstring(str));
}

template<>
void parseOption<const_slice<i8>>(const_slice<i8>& opt, const i8* str) {
    while (*str != '=') {
        str ++;
        if (!*str)
            panic("Expected string value.");
    }
    str ++;
    opt = cstring(str);
}

template<>
void parseOption<PassTimeUnit>(PassTimeUnit& opt, const i8* str) {
    while (*str != '=') {
        str ++;
        if (!*str)
            panic("Expected time unit as a string.");
    }
    str ++;
    if (string_equal(str, "s") || string_equal(str, "seconds"))
        opt = PassTimeUnit::Seconds;
    if (string_equal(str, "ms") || string_equal(str, "millis") || string_equal(str, "milliseconds"))
        opt = PassTimeUnit::Milliseconds;
    if (string_equal(str, "us") || string_equal(str, "micros") || string_equal(str, "microseconds"))
        opt = PassTimeUnit::Microseconds;
    if (string_equal(str, "ns") || string_equal(str, "nanos") || string_equal(str, "nanoseconds"))
        opt = PassTimeUnit::Nanoseconds;
    if (string_equal(str, "ticks"))
        opt = PassTimeUnit::Ticks;
}

void parseOptions(i32& argc, i8** argv) {
    #define DEFINE_DEFAULT_OPTION(name, optname, type, value, ...) config:: name = value;
    FOR_EACH_OPTION(DEFINE_DEFAULT_OPTION)
    #undef DEFINE_DEFAULT_OPTION

    for (i32 i = 1; i < argc; i ++) if (argv[i][0] == '-' && argv[i][1] == '-') {
        #define CHECK_FOR_OPTION(name, optname, type, ...) if (findc("--" #optname, 0) <= findc(argv[i], 0) && !memory::compare(argv[i], "--" #optname, findc("--" #optname, 0))) parseOption<type>(config:: name, argv[i]);
        FOR_EACH_OPTION(CHECK_FOR_OPTION)
        #undef CHECK_FOR_OPTION
    }

    map<const i8*, vec<void(*)()>> optionsByCategory;
    #define ADD_CATEGORY_LIST(cat) optionsByCategory.put(#cat, {});
    FOR_EACH_CATEGORY(ADD_CATEGORY_LIST)
    #undef ADD_CATEGORY_LIST

    #define ADD_OPTION_TO_CATEGORY(name, optname, type, value, category, desc) \
        optionsByCategory[#category].push([]() { println("  --" BOLDCYAN #optname RESET ": ", desc); });
    FOR_EACH_OPTION(ADD_OPTION_TO_CATEGORY)
    #undef ADD_OPTION_TO_CATEGORY

    if (config::listOptions || config::help) {
        for (const auto& p : optionsByCategory) {
            println(p.key, " options:");
            for (const auto& f : p.value) f();
            println();
        }
        process::exit(0);
    }
    
    i8** writer = argv + 1;
    i8** reader = writer;
    for (i32 i = 1; i < argc; i ++) {
        ++ reader;
        if (argv[i][0] == '-' && argv[i][1] == '-') {
            #define CHECK_FOR_OPTION(name, optname, type, ...) if (findc("--" #optname, 0) <= findc(argv[i], 0) && !memory::compare(argv[i], "--" #optname, findc("--" #optname, 0))) continue;
            FOR_EACH_OPTION(CHECK_FOR_OPTION)
            #undef CHECK_FOR_OPTION
        }
        *writer ++ = reader[-1];
    }
    argc = writer - argv;
}