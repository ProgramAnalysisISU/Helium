#!/bin/bash



# This file is going to be used like this:

# script benchmark.tar.gz
# script benchmark/

# The generated file will be ./helium-output,
# It will contain the benchmark folders
# containing src,orig,cpped

# If you want to preprocess all benchmarks in a folder, write a script like this:
# for bench in $1/*; do
#     helium-preprocess.sh $bench
# done


# Setting timeout
# for name in *.zip; do timeout 30m helium-preprocess.sh $name; [ $? == 0 ] && echo "$name " >> result.txt; done
    


CC=${CC=clang}

type $CC >/dev/null 2>&1 || { echo >&2 "$CC is not installed. Abort."; exit 1; }

# create snippet db for all the benchmarks in argument

# if [[ $# -eq 0 || $1 == '-h' ]]; then
#     echo "create snippet db for all the benchmarks in argument";
#     echo 'Usage: ';
#     echo 'this.sh /path/to/folder/contains/benchmarks'
#     exit 0;
# fi

# $1: name of archive
# $2: name of destination folder
function uncompress {
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

function get_simple_name {
    name=$1
    if [[ $name == */ ]]; then
        name=${name%%/}
    fi
    name=${name##*/}
    if [[ $name == *.tar.gz ]]; then
        name=${name%.tar.gz}
    elif [[ $name == *.zip ]]; then
        name=${name%.zip}
    elif [[ $name == *.tgz ]]; then
        name=${name%.tgz}
    fi
    echo $name
}


for BenchPath in $*; do
    if [[ ! -a $BenchPath ]]; then
        echo "Benchmark $BenchPath does not exists"
        exit 1;
    fi
    echo "Processing $BenchPath .."
    SimpleName=$(get_simple_name $BenchPath)
    OutputFolder="helium-output/$SimpleName"

    echo "Outputing to $OutputFolder .."

    rm -rf $OutputFolder
    mkdir -p $OutputFolder
    for folder in orig src tmp cpped; do
        mkdir $OutputFolder/$folder
    done

    # copy or uncompress the code into "orig" folder
    uncompress $BenchPath $OutputFolder/orig

    # copy all .c and .h files into "src" folder
    # FIXME do not consider same name for now
    find "$OutputFolder/orig" -name "*.[c|h]" -exec cp "{}" $OutputFolder/src/ \;

    CurrentFolder=$(pwd)

    cd $OutputFolder
    echo "Compiler Preprocessing .."
    for file in src/*; do
        # 4. use cc -E -nostdinc to process, rename back to the same name
        simple_name=${file##*/}
        # inside src folder, preprocess all files and put the result into "tmp" folder
        cd src
        $CC -E -include $HELIUM_HOME/lib/config/config.h "$simple_name" > "../tmp/$simple_name" 2>/dev/null
        cd ../tmp
        # 5. use helium-process-cpped.py to remove extra line marker staffs, rename back
        # in "tmp" folder, call the preprocess script, and put the result into "cpped" folder, as final result
        helium-process-cpped.py "$simple_name" > "../cpped/$simple_name"
        cd ..
    done
    # 6. create snippetdb, the name should be renamed to XXX-cpped/snippet/
    echo "Creating snippet db ..."
    time helium --create-snippet-db cpped > /dev/null # 2>&1
    cd $CurrentFolder

    
done
