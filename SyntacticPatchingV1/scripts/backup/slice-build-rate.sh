#!/bin/bash

# slice build rate experiment
# this experiment is decided by the given slice data folder.

if [ $# == 0 ]; then
    echo "Do slice buildrate experimetn."
    echo ""
    echo "Usage: <this-script> /path/to/slice/folders /path/to/benchmarks"
    exit 1
fi


# $1: path to slice folders
# $2: path to benchmark folders

for folder in $1/*; do
    benchmark=${folder##*/}
    # echo $benchmark
    if [ -d $2/$benchmark ]; then
        # echo "=== found $benchmark"
        echo  "===" $benchmark " ..." 1>&2
        helium --slice=$folder --snippet-db-folder=$2/$benchmark/snippets $2/$benchmark
    fi
done
