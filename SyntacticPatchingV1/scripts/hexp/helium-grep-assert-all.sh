#!/bin/bash

for name in ~/github/200g/preprocessed/*; do
    for file in $name/src/*.c; do
        # >&2 echo $file
        cat $file | awk '/\<assert\>/ {print}'
    done
done
