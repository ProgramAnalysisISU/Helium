#!/bin/bash

# copy picture
# written to /tmp/helium-BHpcn0/out.dot

rm -r out
mkdir out
idx=0
cat $1 | awk '/written/ {print $3}' |
    while read line; do
        idx=$((idx+1));
        if (( $idx < 10 )); then
            filename=0$idx
        else
            filename=$idx
        fi
        cmd="cp $line out/$filename.dot";
        echo $cmd;
        $cmd;
        dot -Tpng -o out/$filename.png out/$filename.dot;
    done
# cat $1 | awk '/written/ {print $3}' | while read line; do idx=$((idx+1)); cmd="cp $line out/$idx.dot"; echo $cmd; done
rm out/*.dot
