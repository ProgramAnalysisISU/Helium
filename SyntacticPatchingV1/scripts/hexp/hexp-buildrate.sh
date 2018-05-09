#!/bin/bash

# build rate test script

# $1: /path/to/folder/containing/benchmarks
# $2: /path/to/headered/pois
# Output: helium-result.txt

# Also, use tee to run the script!

# 2>&1 | tee -ai log.txt



for name in $1/*; do
    echo "============ " ${name##*/} | tee -a helium-result.txt
    ## --print-compile-error=true\
    timeout 30m helium\
            --run-test=false\
            --instrument-io=false\
            --segment-per-poi-limit=100 --valid-poi-limit=100\
            $name --poi-file=$2/${name##*/}.csv\
            2>&1 | tee -a helium-result.txt
    RETVAL=$?

    # remove the temp folder to avoid run out of disk space
    # comment this out if you want to debug the generated code
    rm -rf /tmp/helium*

    
    # return code for timeout
    if [ $RETVAL == 124 ]; then
        echo "====== Benchmark $name Timeout" | tee -a helium-result.txt
    # return code for C-c
    elif [ $RETVAL == 130 ]; then
        echo "====== Interrupted with C-c. Enter to continue next benchmark, C-c to stop."
        read text
        continue
        # break
    fi

    ## use one C-c to back to terminal, ask for input
    ## another C-c will end the program
done
