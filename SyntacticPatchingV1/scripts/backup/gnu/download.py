#!/usr/bin/env python3

from threading import Thread
from subprocess import call, DEVNULL
import sys

class DownloadThread(Thread):
    def __init__(self, command):
        Thread.__init__(self)
        self.command = command
    def run(self):
        print('[running]\t' + self.command)
        if call(self.command, stderr=DEVNULL, stdout=DEVNULL, shell=True) != 0:
            print('[fail]\t' + self.command)
        else:
            print('[success]\t' + self.command)

if __name__ == '__main__':
    directory = 'out'
    with open(sys.argv[1]) as f:
        for line in f:
            if line.startswith("http"):
                name = line.split('/')[-1].split('.')[0];
                command = 'git clone '+line.strip()+' ' + directory + '/'+name
                DownloadThread(command).start()
                # print(command)
