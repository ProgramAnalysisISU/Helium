#!/usr/bin/env python3

'''
Input: https://github.com/xxx/yyy[.git]
Output: xxx-yyy
'''

def getId(url):
    return url.replace('https://github.com/','').replace('.git','').replace('/','-')
