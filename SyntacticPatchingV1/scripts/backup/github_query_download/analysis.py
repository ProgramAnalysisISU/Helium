#!/usr/bin/env python3

'''
count loc of programs
'''
import os,sys,re
import util
from subprocess import call,DEVNULL,PIPE,Popen,check_output

def countLine(proj):
    count=0
    assertCount=0
    for root,dirs,files in os.walk(proj):
        for f in files:
            if not f.endswith('.c'): continue
            try:
                for line in open(root+'/' + f):
                    count+=1
                    if re.search('assert', line, re.IGNORECASE):
                        assertCount+=1
            except UnicodeDecodeError:
                try:
                    for line in open(root+'/'+f, encoding='latin1'):
                        count+=1
                        if re.search('assert', line, re.IGNORECASE):
                            assertCount+=1
                except UnicodeDecodeError:
                    print('exception for '+root+'/'+f, file=sys.stderr)
            except FileNotFoundError:
                print('FileNotFoundError for '+root+'/'+f, file=sys.stderr)
    return (count, assertCount)

def loc(path):
    d = {}
    d2 = {}
    for root,dirs,files in os.walk(path):
        if root != path: continue
        for proj in dirs:
            (count, assertCount) = countLine(root+'/'+proj)
            d[proj] = count
            d2[proj] = assertCount
    return (d,d2)

'''
Input: tmp/
Output:
{
    'linux': 43242
    'git': 34212
}
'''
def size(path):
    # 4776	tmp/flinux
    # 354800	tmp/godot
    d = {}
    f = open('size.txt', 'w')
    if call('du -s '+path+'/*', stdout=open('size.txt', 'w'), shell=True) != 0:
        print('Error executing du -s', file=stderr)
        exit(1)
    f.close()
    f = open('size.txt', 'r')
    for line in f:
        line = line.replace(path+'/', '')
        d[line.split()[1]] = int(line.split()[0])
    return d

'''
Input: repos.txt
Ouptut:
{
    'linux': 'https://github.com/linux/linux'
}
'''
def url(filename):
    d = {}
    f = open(filename,'r')
    for line in f:
        url = line.split()[0]
        name = util.getId(url)
        d[name] = url
    return d

# def countAssert(path):
#     d = {}
#     for root,dirs,files in os.walk(path):
#         if root != path: continue
#         for proj in dirs:
#             grepCommand = 'grep -i assert -R '+root+'/'+proj+'/**/*.c'
#             grep = Popen(grepCommand, stdout=PIPE, shell=True)
#             output = check_output(['wc', '-l'], stdin=grep.stdout)
#             d[proj] = int(output.strip())
#     return d
