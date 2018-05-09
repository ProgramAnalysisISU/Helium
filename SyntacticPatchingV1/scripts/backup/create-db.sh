#!/bin/bash


if [[ $# -eq 0 || $1 == '-h' ]]; then
    echo 'Usage: ';
    echo 'create-db.sh /path/to/folder/contains/benchmarks'
    echo 'create  snippet database for all benchmarks in ./benchmark/ folder';
    exit 0;
fi

# create  snippet database for all benchmarks in ./benchmark/ folder
# usage: bash create_db.sh

for folder in $1/*; do
    echo $folder;
    cd $folder;
    helium --create-snippet-db .
    cd -
done
