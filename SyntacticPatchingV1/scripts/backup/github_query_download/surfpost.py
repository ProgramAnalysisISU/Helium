#!/usr/bin/env python3

import sys
import argparse
import xml.etree.ElementTree as ET
from subprocess import check_output

# for root,dirs,files in os.walk(sys.argv[1]):

# Filename: xxx; Line: xxx; OutputIndex: xx
def parse_statement():
    f = open('index.txt')
    for l in f:
        (filename, line, index) = l.split(';')
        filename = filename.split(':')[1].strip()
        line = line.split(':')[1].strip()
        index = index.split(':')[1].strip()+'.txt'
        # print(filename+'\t'+line)
        f2 = open(index)
        s = set()
        for ll in f2:
            ll = ll.strip()
            if ll not in s:
                s.add(ll)
                # print(ll)
        print(len(s)-1)

d = {}
def preprocess(filename):
    li = []
    s = check_output('src2srcml --position ' + filename, shell=True)
    root = ET.fromstring(s)
    functions = root.findall('{http://www.sdml.info/srcML/src}function')
    for f in functions:
        nameElement = f.find('{http://www.sdml.info/srcML/src}name')
        line = nameElement.get('{http://www.sdml.info/srcML/position}line')
        line = int(line)
        name = nameElement.text
        li.append(line)
    def getFunctionId(line):
        result = 0
        for i in li:
            if i < line:
                result = i
            else:
                break
        return filename+':'+str(result)
    d[filename] = getFunctionId

def getFunctionId(filename, line):
    return d[filename](line)
# Filename: xxx; Line: xxx; OutputIndex: xx
def parse_function():
    f = open('index.txt')
    for l in f:
        s=set()
        (filename, line, index) = l.split(';')
        filename = filename.split(':')[1].strip()
        line = line.split(':')[1].strip()
        index = index.split(':')[1].strip()+'.txt'
        # print(filename+'\t'+line)

        f2 = open(index)
        for ll in f2:
            # /xxx/xxx/xxx.c 222
            (ff, num) = ll.split()
            num = int(num)
            if ff not in d.keys():
                preprocess(ff)
            # functionId: /xxx/xx/xxx:222
            functionId = getFunctionId(ff,num)
            if functionId not in s:
                s.add(functionId)
        print(len(s))
parser = argparse.ArgumentParser()
parser.add_argument('-s', '--statement', help='Slice in terms of Statement Level', action='store_true')
parser.add_argument('-f', '--function', help='How many function are the slices belong', action='store_true')
args = parser.parse_args()

if len(sys.argv) == 1:
    parser.print_help()
    exit(1)
if args.statement:
    parse_statement()
if args.function:
    parse_function()
