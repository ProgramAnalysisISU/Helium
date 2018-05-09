#!/usr/bin/env python3
# get valid svn command for the urls
import sys

# http://sourceforge.net/projects/jmanage
# TO
# svn checkout svn://svn.code.sf.net/p/x10/code/trunk x10-code
# svn checkout svn://svn.code.sf.net/p/jnode/svn/trunk jnode-svn
# svn checkout svn://svn.code.sf.net/p/ungrid/code/ ungrid-code

from urllib.request import urlopen
from html.parser import HTMLParser
from subprocess import call, DEVNULL
from threading import Thread

checkout_prefix = "svn checkout svn://svn.code.sf.net/p/"
ls_prefix = checkout_prefix.replace("checkout", "ls")

def func(id):
    for suffix in ["/svn/trunk", "/code/trunk", "/svn/trunk", "/code", "/svn"]:
        ls_command = ls_prefix + id + suffix
        if call(ls_command, shell=True, stdout=DEVNULL, stderr=DEVNULL) == 0:
            # Note: create tmp directory first
            return checkout_prefix + id + suffix + " " + "svn/" + id
    return ""

class myThread(Thread):
    def __init__(self, id):
        Thread.__init__(self)
        self.id = id;
    def run(self):
        command = func(self.id)
        # print(self.id + "\t" + command)
        if command: print(command)

if (len(sys.argv)<2):
    print("usage: ./svncommand.py url.txt")
    exit(1)
f = open(sys.argv[1])
for line in f:
    name = line.split('/')[-1].strip()
    # print(name)
    # command = func(name)
    # print(command)
    t = myThread(name)
    t.start()
