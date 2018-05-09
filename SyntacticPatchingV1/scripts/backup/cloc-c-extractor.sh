#!/bin/bash

if [[ $# -eq 0 || $1 == '-h' ]]; then
    echo 'Usage: ';
    echo 'cloc-c-extractor.sh /path/to/folder/contains/benchmarks'
    exit 0;
fi


echo "benchmark, loc(C), size"
for folder in $1/*; do
    echo -n $folder
    echo -n ","
    cloc --csv --quiet $folder | grep C | awk -F , -v ORS='' '{s+=$5} END {print s}'
    echo -n ","
    du -sh $folder | awk '{print $1}'
done
