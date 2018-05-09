#!/bin/awk -f

BEGIN {
    print "beginning of process"
    ok=0;
    input=0
    output=0
}

/\[SourceManager\] IOData/ {
    # print "IOData"
    ok=0;
    # $4: input
    # $8: output
    if ($4 > 0 && $8 > 0) {
        ok=1;
        input=$4
        output=$8
        # print input,output
    }
}

/\[main\] Output written to/ {
    # print "output"
    # $5: the path
    path=$5
    # get id from path
    match(path, /sel-([[:digit:]]+)\.json/)
    # id="unset"
    if (RSTART>0) {
        id=substr(path, RSTART+4, RLENGTH-4-5)
    }
    
    if (ok) {
        print id,input, output, path
    }
}
