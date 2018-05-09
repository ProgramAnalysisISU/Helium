#!/usr/bin/env python3

from threading import Thread
import os
import subprocess

class ThreadExecutor(Thread):
    def __init__(self, cmd):
        Thread.__init__(self)
        self.cmd = cmd
    def run(self):
        subprocess.run(self.cmd)

def preprocess(indir, outdir):
    """
    1. preprocess
    2. create include.json
    3. create snippets.json
    """
    for item in os.listdir(indir):
        print ('preprocessing for ' + os.path.join(indir, item) + ' ..')
        benchmark = os.path.join(indir, item)
        outputdir = os.path.join(outdir, item)
        cmd = 'helium --preprocess ' + benchmark + ' -o ' + outputdir
        subprocess.run(cmd, shell=True)
        cmd = 'helium --create-include-dep ' + benchmark\
              + ' -o ' + os.path.join(outputdir, 'include.json')
        subprocess.run(cmd, shell=True)
        cmd = 'helium --create-snippet ' + outputdir\
              + ' --include-dep ' + os.path.join(outputdir, 'include.json')\
              + ' -o ' + os.path.join(outputdir, 'snippets.json')
        subprocess.run(cmd, shell=True)

def create_selection(indir, outdir):
    """
    Create random selection for all cache folder
    """
    for item in os.listdir(indir):
        print ('creating selection for ' + os.path.join(indir, item) + ' ..')
        benchmark=os.path.join(indir, item)
        outputdir=os.path.join(outdir, item)
        if os.path.isdir(benchmark):
            cmd = 'helium --create-sel --sel-num 100 --sel-tok 20 '\
                  + benchmark + ' -o ' + outputdir
            subprocess.run(cmd, shell=True)
            
def unzip(indir, outdir):
    """
    unzip all zip files and put into unzip folder with the same name
    """
    if os.path.exists(outdir):
        print('Output folder exists. Remove to continue')
        exit(1)
    for root,dirs,files in os.walk(indir):
        for f in files:
            if f.endswith('.zip'):
                # extract
                tmpfolder=outdir+'/tmp-'+f
                targetfolder = outdir + '/' + f[:-4]
                zipfile = root + '/' + f
                os.makedirs(tmpfolder)
                print ('unzipping ' + zipfile + ' ..')
                subprocess.run('unzip -q ' + zipfile + ' -d ' + tmpfolder, shell=True)
                subprocess.run('mv ' + tmpfolder + '/* ' + targetfolder, shell=True)
                # this folder is empty
                os.rmdir(tmpfolder)
