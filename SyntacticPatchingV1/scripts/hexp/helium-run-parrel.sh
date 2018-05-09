#!/bin/bash

function observe_data {
    echo "======== observing data =========="
    for bench in $1/*; do
        dump_file=$bench/helium_dump.txt
        # echo $dump_file
        if [ -f $dump_file ]; then
            success=`cat $dump_file | grep success | wc -l`
            error=`cat $dump_file | grep error | wc -l`
            reach=`cat $dump_file | grep reach | wc -l`
            noreach=`cat $dump_file | grep no | wc -l`
            echo "$success $error $reach $noreach ==== $bench"
        fi
    done
}


if [ $# != 2 ]; then
    echo "Usage: this.sh /path/to/folder 100"
    exit 1
fi

ps -ef | grep "helium -s"
ps -ef | grep "helium -s" | awk '{print $2}' | xargs kill


background_jobs=""

for bench in $1/*; do
    echo "processing " $bench " .."
    simple_name=${bench##*/}
    poi_file=/tmp/poi/$simple_name.poi.txt
    if ! [ -f $poi_file ]; then
        echo "NO POI File: $poi_file"
        continue
    fi

    # processing
    cd $bench
    rm -f helium_log.txt helium_dump.txt helium_dump_compile_error.txt
    # CONFIG
    # when testing build rate, use test-number=1
    # do NOT use run-test=false, that is buggy
    helium -s snippets/ cpped/ --print='ci,ce,col' --conf="test-number=1" -c src --poi=$poi_file >/dev/null 2>&1 &
    cd ..
done

echo "peirodically observe the data .."
for (( i=0; i<$2; i++)); do
    sleep 1
    echo $i/$2
    observe_data $1
    # also, I want to check how many procedure left.
    ps -ef | grep 'helium -s'
    left_procedure=`ps -ef | grep 'helium -s' | wc -l`
    left_procedure=`expr $left_procedure - 1`
    echo "left procedure: $left_procedure"
done

ps -ef | grep "helium -s"
ps -ef | grep "helium -s" | awk '{print $2}' | xargs kill

echo "======= Final data ======="
observe_data $1

echo "=== End ==="
