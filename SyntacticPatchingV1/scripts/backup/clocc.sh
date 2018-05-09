#!/bin/bash

if [ $# != 1 ]; then
    echo "Usage: clocc.sh /path/to/folder/or/archive"
    exit 1;
fi

cloc --include-lang="C,C/C++ Header" $1 | grep SUM | awk '{print $5}'
