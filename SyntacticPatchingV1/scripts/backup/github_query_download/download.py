#!/usr/bin/env python3

'''
download a list of repo.
The Repo.txt file format:
    http://github.com/... xxx xxx
    http://svn.com/... xxx xxx
It will extract the first component as the url.
Whether --git should be provided
'''

from threading import Thread
from subprocess import call, DEVNULL
import util
import sys

class Download:
    def __init__(self, filename, t, directory):
        self.filename = filename
        self.directory = directory
        if t == 'git':
            self.prefix = t + ' clone '
        else:
            print('only support git')
            exit(1)
    def download(self):
        f = open(self.filename, 'r')
        for line in f:
            # the first component of the line should be the git url
            url = line.split()[0]
            name = util.getId(url)
            command = self.prefix+url+' '+self.directory + '/' + name
            DownloadThread(command).start()
        f.close()


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
    parser = argparse.ArgumentParser()
    parser.add_argument('-f', '--file', help='Url file path', required=True)
    parser.add_argument('-t', '--type', help='git or svn', choices=['git', 'svn'], required=True)
    args = parser.parse_args()

    Download(args.file, args.type)
