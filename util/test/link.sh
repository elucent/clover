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
    setup_harness(argc, argv);
    OverallResultList summary;" > $SRC
for obj in ${@:2:$#-1}; do
    TESTS="$(nm --no-sort --defined-only --no-demangle $obj | sed 's/0[0-9a-fA-F]*\s\?//' | sed 's/[a-zA-Z]\s//' | grep '^test_.*_fn' | paste -sd ' ' -)"
    for test in $TESTS; do
        trimmed=${test%_fn}
        trimmed=${trimmed#test_}
        echo "extern \"C\" void $test(TestResults&);" >> $HDR
        echo "    test_map.put(cstring(\"$trimmed\"), $test);" >> $SRC
    done
done
echo "
    parse_arguments_in_harness(argc, argv);
    summary.total_tests = test_list.size();
    sort_tests();
    for (const auto& test : test_list) {
        TestResults results(summary, test.key);
        test.value(results);
        results.finish(summary);
    }
    summary.summarize();
    process::deinit();
    return 0;
}" >> $SRC