#!/bin/bash

# simply invoke this in a benchmark directory

benchmark=`pwd`
benchmark=${benchmark##*/}
# command=""
# echo $command
# bash -c $command

helium -s snippets -c src\
       --print='ci,ce,col,ana,io,csv'\
       --debug='ce'\
       --whole-poi=/tmp/poi/poi.org -b $benchmark\
       cpped\
       --conf='instrument-null=true'\
       $*
##############################
# for buffer overflow, bugbench
##############################

# --conf='instrument-strlen=true, test-number=30'\

##############################
# for double-free
##############################
# --conf='instrument-null=true'\

##############################
# for build rate testing
##############################

# --conf='test-number=1'\
