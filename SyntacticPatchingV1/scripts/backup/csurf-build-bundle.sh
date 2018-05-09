#!/bin/bash

# build a bunch of project with code surfer

if [ $# == 0 ]; then
    echo "Build a bunch of benchmarks, using code surfer."
    echo ""
    echo "Will try to use different methods to build."
    echo "Methods tried (in order):"
    echo "1. If Makefile presents in root directory of benchmark, issue make"
    echo "2. If configure presents and executable, issue ./configure; make"
    echo "3. If configure.ac presents, issue autoconf; ./configure; make"
    exit 1;
fi

prefix="csurf hook-bulid myproj "
prefix=""
for folder in $1/*; do
    cd $folder;
    echo -n "====== processing " $folder "... "
    make clean >/dev/null 2>&1
    autoconf >/dev/null 2>&1
    ./configure >/dev/null 2>&1
    rm -rf myproj.*
    csurf hook-build myproj make >/dev/null 2>&1
    echo $?
    
    # if [ -f "Makefile" ]; then
    #     $prefix make 2>/dev/null
    # elif [ -f "configure" ]; then
    #     ./configure
    #     $prefix make 2>/dev/null
    # elif [ -f "configure.ac" ]; then
    #     autoconf
    #     ./configure
    #     $prefix make 2>/dev/null
    # fi
    cd -;
done
