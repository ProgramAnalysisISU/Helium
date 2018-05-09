#!/bin/bash

# calculate the buildrate
# using helium --snippet-db-folder=snippet --conf="code-selection=function" --print="ci,br" /path/to/benchmark
if [[ $# -eq 0 || $1 == '-h' ]]; then
    echo 'Usage: ';
    echo 'ast-build.sh /path/to/folder/contains/benchmarks'
    exit 0;
fi


# echo "benchmark, file count, func count, compile success, compile error, build rate, time(s)"
for folder in $1/*; do
    echo $folder 1>&2;
    if [ -d $folder ]; then
        helium --snippet-db-folder=$folder/snippets --conf="code-selection=function" $folder/
    fi
    # cd - >/dev/null
done
