#!/bin/bash

# genrate POI file

# The parameter is the top level folder of the benchmark
# benchmarkname/src/a.c

## IO:
# stderr: process message
# stdout: csv file



loc=0
for file in $1/src/*.c; do
    # >&2 echo $file
    loc=$loc+$(expr $(cat $file | wc -l))
done

# echo $loc
loc=$(($loc))
>&2 echo "Total loc: $loc"

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
    # echo $file
    thisloc=$(cat $file | wc -l)
    # calculate the percentage
    # assume generate 1000 poi for the benchmark
    # echo $thisloc
    number="$thisloc * 1000 / $loc"
    number=$(echo "$number" | bc)
    >&2 echo "Generating $number of poi for $file .."
    # | libpng-1.2.5              | pngrutil.c            |  1014 | stmt | null-deref     |                                                         |                           |
    # echo "shuf -i 1-$thisloc -n $number"
    for line in $(shuf -i 1-$thisloc -n $number); do
        echo "${bench},${file##*/},$line,stmt"
    done
done
