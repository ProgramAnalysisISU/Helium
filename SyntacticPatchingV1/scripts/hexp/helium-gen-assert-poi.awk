#!/usr/bin/awk -f

# This script will extract the linum and condition for each assertion statement.
# The condition may be invalid though.

/\<assert\>/ {
    text=$0
    # print "orig: " text
    text = gensub("assert\\((.*)\\)", "\\1", "g", text);
    # text = gensub("assert\\\(", "hello&", "g")
    # print gensub("\\((a>8)\\)", "&\\1", "g", "assert(a>8)")
    # print "1: " text
    text = gensub("([[:alpha:]]+[[:alpha:][:digit:]\\[\\]_->.]*)", "output_int_\\1", "g", text)
    text = gensub("output_int_NULL", "0", "g", text)
    # print "2: " text

    # to be a POI file, need benchmark name and file name as prefix in each line
    print NR ",stmt,," text
}
