#!/bin/bash

# $1: project folder
# $2: output directory
# output: $2/project_name.tag
function generate_one_tag () {
    # helium --ctags $1
    output_name=$2/${1##*/}.tag
    # cmd="ctags -f $output_name --languages=c,c++ -n --c-kinds=+x --exclude=heium_result -R $1"
    cmd="helium --create-tagfile $1 -o $output_name";
    echo $cmd
    echo -n generating $output_name ...
    echo -ne '\t'
    $cmd >/dev/null 2>&1
    if [ $? == 0 ]; then
        echo OK
    else
        echo Fail
    fi
}

# $1: benchmarks folder: containing many benchmarks in the top level
# output: in tagfiles/benchmark_name.tag
function generate_tags () {
    # generate tags for all files
    confirm=
    while [[ $confirm != 'n' && $confirm != 'y' ]];
    do
        read -p "Are you sure to generate tags? (y or n) " confirm
    done
    if [[ $confirm == 'y' ]]; then
        echo "yes"
        rm -rf tagfiles
        mkdir tagfiles
        for fname in $1/*
        do
            generate_one_tag $fname tagfiles
        done
    fi
}

# generate_tags $1

function print_help {
    echo 'create-tag.sh <target-benchmarkS-folder>'
    echo '    <target-benchmarks-folder> is the top folder contains many benchmarks.'
    echo '    will generate "tagfile/" folder, and store the tag file as <project-name>.tag'
}

# exit 0
if [[ -d $1 ]]; then
    generate_tags $1
else
    print_help
fi
       

# for fname in $1/*
# do
#     project_name=${fname##*/}
#     cmd="helium --print-segment-info $fname --tagfile tagfiles/$project_name.tag"
#     echo $cmd
#     # $cmd >/dev/null 2>&1
#     $cmd
# done
