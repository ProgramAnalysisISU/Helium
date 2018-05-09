#!/bin/bash

# run slicing plugin for a bunch of benchmarks

# remove the result.txt everytime, because the write to it is appending

if [ $# == 0 ]; then
    echo "Run slicing plugin of code surfer on a bunch of benchmarks."
    echo ""
    echo "Usage: <this-script> /path/to/folder/of/benchmarks"
    echo ""
    echo "Need to ensure there's an input.txt file in each benchmark"
    echo "Output will be the result.txt file in each benchmark."
    echo "result.txt will be overwrote."
    echo "Result will also write in ./tmp/ folder with the path-like filename"
fi

mkdir tmp

for folder in $1/*; do
    echo $folder
    rm -rf $folder/result.txt
    # create the "EOF" empty file, so that I don't need to press C-d everytime it presents STK>
    csurf -nogui -l /home/hebi/github/Helium/lib/csurf/plugin $folder/myproj <EOF
    filename=`echo $folder | sed -e 's/\//-/g' -e 's/\.//g' -e 's/^-//g'`
    echo $filename
    cp $folder/result.txt  tmp/$filename.txt
done

    
