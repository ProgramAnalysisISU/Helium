#!/usr/bin/env python3
from urllib import request
import json

import argparse
import sys

from threading import Thread
from download import Download

import analysis
import util

filename = 'repos.txt'

def query(size):
    url = 'https://api.github.com'
    api = '/search/repositories'
    query = 'language:c&stars:>10&per_page='+size
    response = request.urlopen(url+api+"?q="+query)
    s = response.read().decode('utf8')
    j = json.loads(s)
    f = open(filename, 'w')
    for item in j['items']:
        f.write(item['clone_url'] + "\t" + str(item['stargazers_count']))
        f.write('\n')
    f.close()
    print('result saved to '+filename)

def download(directory):
    Download(filename, t='git', directory=directory).download()

'''
output format:
[
    {
        'name': 'linux-linux'
        'url': 'http://github.com/linux/linux'
        'loc': 34353
        'size': 1245
        'assert': 1152
    }
]
'''
def analyze(folder='tmp'):
    result = []
    print("doing loc analysis")
    (locDict, assertDict) = analysis.loc(folder)
    print('doing size analysis')
    sizeDict = analysis.size(folder)
    # print('doing assert analysis')
    # assertDict = analysis.countAssert('tmp')
    # print('extracting urls')
    # urlDict = analysis.url(filename)
    for k in locDict.keys():
        d = {}
        d['name'] = k
        # d['url'] = urlDict[k]
        d['loc'] = locDict[k]
        d['size'] = sizeDict[k]
        d['assert'] = assertDict[k]
        result.append(d)
    s = json.dumps(result)
    print(s)

parser = argparse.ArgumentParser()
parser.add_argument('-q', '--query', help='How many result to query github api')
parser.add_argument('-d', '--download', help='Directory to download repos')
parser.add_argument('-a', '--analyze', help='analyze repos')
args = parser.parse_args()

if len(sys.argv)==1:
    parser.print_help()
if (args.query):
    query(args.query)
if (args.download):
    download(args.download)
if (args.analyze):
    analyze(args.analyze)
