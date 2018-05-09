#!/usr/bin/env python3

from urllib import request
import json
import os
from threading import Thread
import argparse

token = '995b6274688ce47a82ce72cd09bb7866eca1e460'
token = '9b47dfbca3a74d8ff098b7d966329843d69fbba4'
url = 'https://api.github.com'

def get_repo():
    url = 'https://api.github.com'
    api = '/search/repositories'
    query_str = ['language:C',
                 # 'sort=stars',
                 # 'order=desc',
                 # 'stars:>10',
                 # 'stars:1..384',
                 # 'stars:1..64',
                 'stars:>500'
    ]
    query = url + api + '?q=' + '+'.join(query_str) + '&per_page=100'
    token = '995b6274688ce47a82ce72cd09bb7866eca1e460'
    objs = []
    for i in range(1,11):
        print ("page ", i, "...")
        paged_query = query + '&page=' + str(i);
        print (paged_query)
        req = request.Request(paged_query)
        req.add_header("Authorization", "token " + token)
        response = request.urlopen(req)
        s = response.read().decode('utf8')
        j = json.loads(s);

        for item in j['items']:
            obj={}
            obj['name'] = item['name']
            obj['full_name'] = item['full_name']
            obj['stars'] = item['stargazers_count']
            obj['language'] = item['language']
            obj['size'] = item['size']
            objs.append(obj)
    return objs

def get_language(obj):
    api = '/repos/'
    # j=json.load(open('all.json'))
    ct=0
    for item in obj:
        # only if this item does not have
        if 'languages' not in item:
            print ('getting language for ', item['name'], ', star: ', item['stars'])
            ct+=1
            # this is for rate limit
            if ct>2800: break
            query = url + api + item['full_name'] + '/languages'
            req = request.Request(query)
            req.add_header("Authorization", "token " + token)
            response = request.urlopen(req)
            s = response.read().decode('utf8')
            langj = json.loads(s);
            item['languages'] = langj

            
# obj = json.load(open('all.json'))
# get_language(obj)
    

def get_rate_limit():
    query = url + '/rate_limit'
    req = request.Request(query)
    req.add_header("Authorization", "token " + token)
    response = request.urlopen(req)
    s = response.read().decode('utf8')
    print(s)

def connect():
    repo=[]
    s=set()
    for i in range(1,7):
        j=json.load(open('result-' + str(i) + '.json'))
        for item in j:
            # remove duplicate
            if item['name'] in s: continue
            s.add(item['name'])
            repo.append(item)
        # repo.extend(j)
    json.dump(repo, open('all.json', 'w'), indent=2)
    return repo

# repo=connect()

# if __name__ == '__main__':
#     # TODO multi-thread
#     search("C", 1);


# all_with_lang = json.load(open('all_with_lang.json'))

def lang_rate(item):
    langs = item['languages']
    sumloc = 0
    cloc = item['languages']['C']
    for lang in langs:
        sumloc += langs[lang]
    return cloc/sumloc
        
def filter_bench(repos):
    """
    compute the percentage of C in the language list
    """
    ret = []
    for item in repos:
        langs = item['languages']
        if 'Objective-C' in langs:
            continue
        if 'C++' in langs:
            continue
        if 'Ruby' in langs:
            continue
        if 'Java' in langs:
            continue
        if 'JavaScript' in langs:
            continue
        if 'C' not in langs:
            continue
        if lang_rate(item) < 0.7: continue
        # 100k LOC
        if langs['C'] > 100000: continue
        # 1M
        if item['size'] > 1000: continue
        # print(item['name'])
        ret.append(item)
    return ret

if __name__ == '__hebi__':
    print ('hello')
    # obj=filter_bench(all_with_lang)
    # json.dump(obj, open('use.json', 'w'), indent=2)
    # print(len(obj))

class DownloadThread(Thread):
    def __init__(self, url, filename):
        Thread.__init__(self)
        self.url = url
        self.filename = filename
    def run(self):
        if not os.path.isfile(self.filename):
            try:
                request.urlretrieve(self.url, self.filename)
                print ('finished downloading ' + self.filename)
            except KeyboardInterrupt:
                print ('Keyboard Interrupt. Exiting')
                exit(1)
            except:
                print ('Other errors. Ignored')

download_prefix = 'https://github.com/'
download_suffix = '/archive/master.zip'
def download(repos, output_folder):
    """
    Download errors:
    - zamaudio/intelmetool
    - rentzsch/stressdrive
    - mulle-nat/mulle-concurrent
    - petermichaux/royal-scheme
    - bradfa/flashbench
    - dparrish/libcli
    """
    print ('Downloading total of ' + str(len(repos)) + ' repos')
    for repo in repos:
        fullname = repo['full_name']
        print('downloading ', fullname, '...')
        filename = fullname.replace('/', '--')
        url = download_prefix + fullname + download_suffix
        # filename = 'data/' + filename + '.zip'
        filename = output_folder + '/' + filename + '.zip'
        # if not os.path.isfile(filename):
        #     try:
        #         request.urlretrieve(url, filename)
        #     except KeyboardInterrupt:
        #         print ('Keyboard Interrupt. Exiting')
        #     except:
        #         print ('Other errors. Ignored')
        DownloadThread(url, filename).start();
        
if __name__ == '__hebi__':
    # the one I'm going to use in experiment
    used_repos = json.load(open('use.json'))
    # download those files
    download(used_repos)
    # filter out those use more headers


if __name__ == '__main__':
    parser = argparse.ArgumentParser(description="Query and Download benchmarks")
    parser.add_argument('--action', choices=['query', 'download', 'show-rate-limit'], help='what action to perform', required=True)
    parser.add_argument('--json-file', help='json file to store or load')
    parser.add_argument('--output-folder', help='output folder', default='data')
    args = parser.parse_args()
    if (args.action == 'query'):
        # query
        # write json-file
        # if json file already exist, show warning and exit
        if (os.path.exists(args.json_file)):
            print(args.json_file + ' exists. Exiting')
            exit(1)
        # query and write json file
        repos = get_repo()
        get_language(repos)
        repos = filter_bench(repos)
        json.dump(objs, open(args.json_file, 'w'), indent=2)
    elif (args.action == 'show-rate-limit'):
        get_rate_limit()
    else:
        # download
        if not os.path.exists(args.json_file):
            print(args.json_file + ' does not exist. Exiting')
            exit(1)
        if os.path.exists(args.output_folder):
            print('output folder ' + args.output_folder + ' exists. Remove it to continue.')
            exit(1)
        os.makedirs(args.output_folder)
        repos = json.load(open(args.json_file))
        download(repos, args.output_folder)
