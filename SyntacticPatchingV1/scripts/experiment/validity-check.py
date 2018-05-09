#!/usr/bin/env python3

from subprocess import run, DEVNULL
import os
import shutil

bench_dir = '/home/hebi/data/unziped'

def check_header():
    valid = open('valid.txt', 'w')
    invalid = open('invalid.txt', 'w')
    ct=0
    for bench in os.listdir(bench_dir):
        ct+=1
        print(ct)
        # if ct > 10: break
        # print (bench)
        simplebench=bench
        bench = bench_dir + '/' + bench
        if os.path.isdir(bench):
            completed = run(['helium', '--check-headers-bench', bench], stdout=DEVNULL)
            if completed.returncode == 1:
                # print ('\t', simplebench,'is not supported')
                invalid.write(simplebench)
                invalid.write('\n')
                invalid.flush()
            else:
                # print (simplebench, 'is supported')
                # print (simplebench)
                valid.write(simplebench)
                valid.write('\n')
                valid.flush()

def check_cache():
    valid = open('valid.txt', 'w')
    invalid = open('invalid.txt', 'w')
    ct=0
    for bench in os.listdir(bench_dir):
        ct+=1
        # if ct > 10: break
        # print (bench)
        simplebench=bench
        print(ct, simplebench)
        bench = bench_dir + '/' + bench
        if os.path.isdir(bench):
            completed = run(['helium', '--check-cache-valid', bench], stdout=DEVNULL)
            if completed.returncode == 1:
                # print ('\t', simplebench,'is not supported')
                invalid.write(simplebench)
                invalid.write('\n')
                invalid.flush()
            elif completed.returncode == 0:
                # print (simplebench, 'is supported')
                # print (simplebench)
                valid.write(simplebench)
                valid.write('\n')
                valid.flush()
            else:
                print ('return code is ', completed.returncode)


    
def remove_invalid():
    with open('invalid.txt') as f:
        for line in f:
            print ('removing ',bench_dir + '/' + line.strip())
            # os.removedirs(bench_dir + '/' + line.strip())
            shutil.rmtree(bench_dir + '/' + line.strip())

if __name__ == '__hebi__':
    check_header()
    check_cache()
    # remove_invalid()
