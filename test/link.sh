#!/usr/bin/env bash
SRC=$1
HDR=${1%.cpp}.h
echo "
#include \"test/harness.h\"
#include \"lib/io.h\"
" > $HDR
echo "
#include \"$HDR\"
int main(int argc, char** argv) {
    setup_harness(argc, argv);
    OverallResultList summary;" > $SRC
for obj in ${@:2:$#-1}; do
    TESTS="$(nm --quiet --no-sort --defined-only --no-demangle $obj | sed 's/0[0-9a-fA-F]*\s\?//' | sed 's/[a-zA-Z]\s//' | grep '^test_.*_fn' | paste -sd ' ' -)"
    echo "    println(\"[SOURCE] ${obj%.o}\"\":\");" >> $SRC
    for test in $TESTS; do
        trimmed=${test%_fn}
        trimmed=${trimmed#test_}
        echo "extern \"C\" void $test(TestResults&);" >> $HDR
        echo "    TestResults ${test}_results(\"$trimmed\");" >> $SRC
        echo "    $test(${test}_results);" >> $SRC
        echo "    ${test}_results.finish(\"${obj%.o}\", summary);" >> $SRC
    done
done
echo '    summary.summarize();
}' >> $1