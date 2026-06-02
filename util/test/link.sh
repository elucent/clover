#!/usr/bin/env bash
SRC=$1
HDR=${1%.cpp}.h
echo "
#include \"util/test/harness.h\"
#include \"util/io.h\"
" > $HDR
echo "
#include \"$HDR\"
i32 main(i32 argc, i8** argv, i8** envp) {
    process::init(argc, argv, envp);
    auto exec = cstring(argv[0]);
    setup_harness(argc, argv);
    vec<const_slice<i8>> originalArgv;
    bool spawned = false;
    for (i32 i = 1; i < argc; i ++) {
        originalArgv.push(cstring(argv[i]));
        if (originalArgv.back() == cstring(\"--spawned\"))
            spawned = true;
    }
    OverallResultList summary;
    summary.was_spawned = spawned;" > $SRC
for obj in ${@:2:$#-1}; do
    TESTS="$(nm --no-sort --defined-only --no-demangle $obj | sed 's/0[0-9a-fA-F]*\s\?//' | sed 's/[a-zA-Z]\s//' | grep '^test_.*_fn' | paste -sd ' ' -)"
    for test in $TESTS; do
        trimmed=${test%_fn}
        trimmed=${trimmed#test_}
        echo "extern \"C\" void $test(TestResults&);" >> $HDR
        echo "    if (!spawned) test_map.put(cstring(\"$trimmed\"), $test);" >> $SRC
    done
done
echo "
    parse_arguments_in_harness(argc, argv);
    summary.total_tests = test_list.size();
    sort_tests();
    if (test_list.size() == 1) {
        TestResults results(summary, test_list[0].key);
        test_list[0].value(results);
        results.finish(summary);
        return 0;
    } else for (const auto& test : test_list) {
        vec<const_slice<i8>> subArgv;
        subArgv.push(cstring(\"--spawned\"));
        for (auto str : originalArgv) {
            bool isOption = true;
            for (u32 i = 1; i < argc; i ++) {
                if (cstring(argv[i]) == str)
                    isOption = false;
            }
            if (isOption)
                subArgv.push(str);
        }
        subArgv.push(test.key);
        TestResults results(summary, test.key);
        auto result = process::exec(exec, subArgv);
        if (result != 0)
            results.results.push([](){});
        results.finish(summary);
    }
    summary.summarize();
    process::deinit();
    return 0;
}" >> $SRC