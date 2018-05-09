#!/bin/bash

# generate input.txt for code surfer slicing plugin

# algorithm description
# scan all files in the target benchmark directory
# for each file, get its loc
# randomly assign N lines to be in the slicing creteria, append them to "input.txt"

if [ $# == 0 -o $# == 1 ]; then
     echo "Generate for many benchmarks. Generate input.txt as slicing creteria."
     echo ""
     echo "Each line contains the absolute file name and a line number, as the slicing criteria."
     echo "See each "
     echo ""
     echo "Usage: <this-script> /path/to/folder/containing/many/benchmarks <num-per-file>"
     echo "output: <individual-benchmark>/input.txt"
     echo "Side effect: will overwrite input.txt for the current directory."
     exit 1;
   fi

     for folder in $1/*; do
         if [ -d $folder ]; then
             generate-slicing-criteria.sh $folder $2
             mv input.txt $folder
         fi
     done
     
