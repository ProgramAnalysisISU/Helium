#!/usr/bin/awk -f

# This script will parse the script and return one with only the last copy of POI instrument


BEGIN {
    # print "beginning"
    in_prefix=1;
    temp="";
    res="";
}


# the cut off position might be bad.
# so do not use the last one, instead, use the one before last
/HELIUM_POI_INSTRUMENT\>/ {res=temp; temp=""; temp=$0; in_prefix=0;}
!/HELIUM_POI_INSTRUMENT\>/ {
    if (in_prefix) {
        print
    } else {
        temp=temp "\n" $0
    }
}



END {
    # print "The end"
    if (length(res)==0 && length(temp!=0)) {
        print temp
    } else {
        print res
    }
}
