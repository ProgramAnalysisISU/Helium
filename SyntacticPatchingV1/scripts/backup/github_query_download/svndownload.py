#!/usr/bin/env python3
import sys

from threading import Thread
from subprocess import call, DEVNULL

class myThread(Thread):
    def __init__(self, command):
        Thread.__init__(self)
        self.command = command
    def run(self):
        print("running "+self.command)
        if call(self.command, stdout=DEVNULL, stderr=DEVNULL, shell=True) != 0:
            print(self.command + " Failed")

if len(sys.argv) < 2:
    print("usage: ./svndownload.py svncommand.txt")
    exit(1)
f = open(sys.argv[1])
for line in f:
    line = line.strip()
    t = myThread(line)
    t.start()
