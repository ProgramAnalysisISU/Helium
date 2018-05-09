#!/usr/bin/env python3
# get url of projects containing 500+ asserts
import sys

if len(sys.argv) == 1:
    print("usage: ./url.py boa-output.txt > url.txt")
    exit(1)
f = open(sys.argv[1])
s=set()
d = dict()
d2 = dict()
n=0
for line in f:
    part = line.split('[');
    name = part[1][:-1]
    url = part[2][:-1]
    number = line.split('=')[-1].strip()
    if name not in s:
        d[name] = 0
        d2[name] = url
        s.add(name)
        # print(n)
        n=0
        # print(name+'\t'+url, end='\t')
    n += int(number)
    d[name] =n

# print(d)
for k in sorted(d, key=d.get, reverse=False):
    if (d[k]>5000):
        # print(k+':'+ d2[k] +':'+str(d[k]))
        print(d2[k])

    # print('\t'.join([name, url, number]))
