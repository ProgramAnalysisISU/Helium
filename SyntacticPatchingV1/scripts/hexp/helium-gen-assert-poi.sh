#!/bin/bash


bench=${1##*/}
if [ -z $bench ]; then
    bench=${1%/}
    bench=${bench##*/}
fi
if [ -z $bench ]; then
    >&2 echo "Cannot get benchmark name from $1."
    exit 1;
fi

for file in $1/src/*.c; do
    >&2 echo $file
    helium-gen-assert-poi.awk $file\
        | awk -v bench="$bench" -v file=${file##*/}\
              '{print bench "," file "," $0}'
done
