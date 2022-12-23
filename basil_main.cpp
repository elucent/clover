#include "basil/basil.h"

using namespace basil;

bool matches(const i8* arg, const i8* cmp, i32 n) {
    return cidx(arg, '\0') == n && !mcmp(arg, cmp, n);
}

template<typename... Args>
i32 errorArgs(i32 code, const Args&... args) {
    println(args...);
    return code;
}

i32 parseArgs(i32 argc, i8** argv) {
    new (&Options::sourceFiles) vec<const i8*>;
    new (&Options::directSource) vec<const i8*>;

    Options::mode = Options::RUN;
    for (i32 i = 1; i < argc; i ++) {
        if (argv[i][0] == '-' && argv[i][1] == '-') { // Word arguments
            if (matches(argv[i], "--read", 6))
                Options::mode = Options::READ;
            else if (matches(argv[i], "--compile", 9))
                Options::mode = Options::COMPILE;
            else if (matches(argv[i], "--eval", 6))
                Options::mode = Options::EVAL;
            else if (matches(argv[i], "--repl", 6))
                Options::mode = Options::REPL;
            else if (matches(argv[i], "--verbose", 9))
                Options::flags |= Options::VERBOSE;
            else return errorArgs(1, "Unknown command line argument: ", argv[i]);
        }
        else if (argv[i][0] == '-') for (i32 j = 1; j < cidx(argv[i], '\0'); j ++) {
            switch (argv[i][j]) {
                case 'c':
                    Options::mode = Options::COMPILE;
                    break;
                case 'i':
                    Options::mode = Options::REPL;
                    break;
                case 'e':
                    Options::mode = Options::EVAL;
                    break;
                case 'v':
                    Options::flags |= Options::VERBOSE;
                    break;
                default:
                    return errorArgs(2, "Unknown command line flag: ", argv[i][j]);
            }
        }
        else (Options::mode == Options::EVAL ? Options::directSource : Options::sourceFiles).push(argv[i]);
    }

    return 0;
}

i32 main(i32 argc, i8** argv) {
    Basil basil;
    basilInstance = &basil;

    if (i32 code = parseArgs(argc, argv))
        return code;
    
    if (Options::mode == Options::REPL)
        repl();
    else
        compileModule({ argv[1], cidx(argv[1], '\0') });
}