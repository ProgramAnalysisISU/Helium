#!/usr/bin/env python3

'''
format:

asserts[Project Name][url][java file path][functionName] = <number>
'''

import argparse,re

parser = argparse.ArgumentParser()
parser.add_argument('-f', '--file', help='boa output file name', required=True)
args = parser.parse_args()
d={}
count=0
for line in open(args.file):
    # line.strip().split('[')
    m = re.findall('\[([^\]]*)\]', line)
    projectName = m[0]
    url = m[1]
    number = int(line.split('=')[-1].strip())
    if projectName not in d:
        d[projectName] = 0
    d[projectName] += number

# print (d)
l = sorted(d, key=d.get, reverse=True)
count=0
for key in l:
    print(key+'\t'+str(d[key]))
    count+=1
    if count == 30:
        break
