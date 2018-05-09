#!/usr/bin/env python3

"""
Usage:
thisscript gzip.cpp.c
"""

import sys
import re
import os
from subprocess import Popen

def remove_extra(filename):
    "Remove everything between # xxx 1 and # gzip.c 2"
    output = False
    count = 0
    for line in open(filename):
        # print(line)
        # line = "# 3 \"fdsl\""
        # line = "# 1 \"builtin\" 3"
        # line = "# 1 \"gzip.c\" 2"
        pattern = (r"^#\s*\d+\s+"
                   r"\"([\w\.\-\_<>]+)\"" # first capturing group, the file name
                   # r"\s*(\d*)"  # optional flags
        )
        match = re.search(pattern, line)
        if output:
            if not line.startswith('#') and len(line) != 1:
                print(line, end='')
        if match:
            # print(line)
            count += 1
            _filename = match.group(1) # file name in line marker in the pre-processed c file
            # if filename == "gzip.c":
            #     print(_filename, file=sys.stderr)
            # flag = match.group(2) # the flag associated with above line
            # print(filename + ":" + flag)
            # print("0: " + g0)
            # print("1: " + g1)
            # if flag == '2' and _filename == filename:
            #     output = True
            # if flag == '1':
            #     output = False
            # do not use flag. Everytime the line marker is the same as file name, turn on output
            # otherwise turn off it
            # CAUTION the filename should be the same, otherwise will output empty
            # e.g. gzip.h > gzip.cpp.h, need to mv back to original filename
            if _filename == filename:
                output = True
            else:
                output = False
                

def remove_extra2(filename):
    output=False
    for line in open(filename):
        if line.startswith('#'):
            line_marker = line.split()
            if len(line_marker) < 3:
                # assert False
                continue
            _file = line_marker[2]
            if _file[0] != '"' or _file[-1] != '"':
                # print(_file)
                # assert False
                continue
            _file = _file[1:-1]
            if _file == filename:
                # print out the line marker, for 
                print(line, end='')
                output = True
            else:
                output = False
        else:
            # if output and len(line) != 1:
            if output:
                # output even empty line, because I want to keep the line number precise
                print(line, end='')

if __name__ == "__main__":
    """
    The script accept a benchmark folder.
    1. It copy all the source files, into a new folder called "cpp-output".
    2. It then call compiler preprocessor on each file, with "cc -E -nostdin" command,
       redirect the stdout to a new file, then replace the original file.
    3. After that, replace the #xxx by calling remove_extra function.
    """
    assert len(sys.argv) > 1
    # Popen("rm -r cpp-output")
    # Popen("cp -r " + sys.argv[1] + " cpp-output")
    # for root, dirs, files in os.walk(sys.argv[1]):
    #     for f in files:
    #         Popen("cc -E -nostdinc " + f + " > " + f + ".tmp")
    #         Popen("mv " + f + ".tmp " + f)
    # remove_extra(f)
    # remove_extra(sys.argv[1])
    remove_extra2(sys.argv[1])
