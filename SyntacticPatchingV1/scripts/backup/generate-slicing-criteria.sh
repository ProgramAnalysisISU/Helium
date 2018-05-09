#!/bin/bash

# generate input.txt for code surfer slicing plugin

# algorithm description
# scan all files in the target benchmark directory
# for each file, get its loc
# randomly assign N lines to be in the slicing creteria, append them to "input.txt"

if [ $# == 0  -o $# == 1 ]; then
    echo "Generate input.txt as slicing creteria."
    echo ""
    echo "Each line contains the absolute file name and a line number, as the slicing criteria."
    echo ""
    echo "Usage: <this-script> /path/to/benchmark/folder <num-per-file>"
    echo "output: input.txt"
    exit 1;
fi

# $0: the first component, the executable
# $1: the first argument, path to benchmark
# $2: 100: 100 random locations per file
if [ -n $2 ]; then
    perfile=$2
    echo "Set the criteria per file to $perfile"
else
    perfile=10
    echo "Criteria per file defaults to $perfile"
fi

echo "Erasing input.txt ..."
rm -rf input.txt

echo "generating ..."
for folder in $1/*; do
    echo -n "."
    all_c_files=`find $folder -type f -name *.c`
    for file in $all_c_files; do
        # echo $file;
        total_loc=`cat $file | wc -l`
        # echo "shuf -i 1-$total_loc -n $perfile"
        randlocs=`shuf -i 1-$total_loc -n $perfile`
        abs_filename=`realpath $file`;
        for loc in $randlocs; do
            echo $abs_filename":"$loc >> input.txt
        done
    done
done
echo ""

echo "outputed to input.txt"
