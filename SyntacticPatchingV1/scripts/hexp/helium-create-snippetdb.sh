#!/bin/bash

CC=${CC=clang}

type $CC >/dev/null 2>&1 || { echo >&2 "I require $CC but it's not installed.  Aborting."; exit 1; }

# create snippet db for all the benchmarks in argument

if [[ $# -eq 0 || $1 == '-h' ]]; then
    echo "create snippet db for all the benchmarks in argument";
    echo 'Usage: ';
    echo 'this.sh /path/to/folder/contains/benchmarks'
    exit 0;
fi

rm -rf helium

# $1: a path
function get_simple_name() {
    name=${1##*/}
    if [[ $name == *.tar.gz ]]; then
        name=${name%.tar.gz}
    elif [[ $name == *.zip ]]; then
        name=${name%.zip}
    elif [[ $name == *.tgz ]]; then
        name=${name%.tgz}
    fi
    echo $name
}

# $1: name of archive
# $2: name of destination folder
function uncompress() {
    if [[ -d $1 ]]; then
        cp -r $1 $2
    elif [[ $1 == *.tar.gz ]]; then
        tar zxvf $1 -C $2 > /dev/null 2>&1
    elif [[ $1 == *.zip ]]; then
        unzip $1 -d $2 > /dev/null 2>&1
    elif [[ $1 == *.tgz ]]; then
        tar xvf $1 -C $2 > /dev/null 2>&1
    else
        continue
    fi
}

# 0. gather all benchmarks and their meta data
# 0.1 uncompress benchmarks

# 1. put all benchmarks into a folder
#    this script runs against that folder
for bench in $1/*; do
    # 2. for each benchmark, create a dummy project: XXX-cpped/cpped/
    echo "== processing " $bench " ..."
    # this bench is the relative path to .tar.gz
    name=`get_simple_name $bench`
    target_dir="helium/$name"
    mkdir -p $target_dir
    mkdir $target_dir/orig
    mkdir $target_dir/src
    mkdir $target_dir/tmp
    mkdir $target_dir/cpped

    uncompress $bench $target_dir/orig
    
    # 3. find all .c and .h files into that folder
    #    FIXME do not consider same name for now
    #    FIXME might have many main function ...
    find "$target_dir/orig" -name "*.[c|h]" -exec cp "{}" $target_dir/src/ \;
    dir_back=$(pwd)
    cd $target_dir
    echo "== Compiler Preprocessing .."
    for file in src/*; do
        # 4. use cc -E -nostdinc to process, rename back to the same name
        simple_name=${file##*/}
        # echo $simple_name
        cd src
        # this will determine what the line marker looks like
        # FIXME NOW The include files are buggy if not found!
        # so we'd better process it from original location
        # cpp will terminate for unfound local header files
        # Maybe just leave it, because the error I found for apache is just the file is missing, not loation problem
        # $CC -E -nostdinc "$simple_name" > "../tmp/$simple_name" 2>/dev/null

        # the reason not to use -nostdinc:
        # the stardard include file might define some flags
        # if the program checks the flag for conditional compilation, and if we nostdinc, the program will end up contains extra definition in cpped, thus in snippet-db
        # FIXME some issue that it replace common things, e.g. bool to _Bool
        $CC -E -include $HELIUM_HOME/lib/config/config.h "$simple_name" > "../tmp/$simple_name" 2>/dev/null
        cd ../tmp
        # 5. use helium-process-cpped.py to remove extra line marker staffs, rename back
        helium-process-cpped.py "$simple_name" > "../cpped/$simple_name"
        cd ..
    done
    # 6. create snippetdb, the name should be renamed to XXX-cpped/snippet/
    # helium --create-snippet-db helium/$bench/src
    # mv snippets helium/$bench/
    echo "== Creating snippet db ..."
    echo "== using timeout 30 min ..."
    timeout 30m time helium --create-snippet-db cpped > /dev/null 2>&1
    if [ $? == 124 ]; then
        echo "== EE: Timed out"
    fi
    cd $dir_back
done    

# build rate & search efficiency
# 1. define a txt file for POI selection!
# 2. do not include test, run context search, for each context, record build rate.
#    Limit the procedure to 4
# 3. also, list the search times

## Try to find problems! Otherwise others would assume everything works fine, and expect results

# dynamic debugging
# template implementation
# more general transfer,invariant,pre-condition stop point
