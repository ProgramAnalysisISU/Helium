#!/usr/bin/env python3

"""
./iclones-process.py --run-simple /path/to/unzipped/benchmarks
"""

import json
import argparse
import subprocess
import os
import shutil

"""
    [
    {
    type: 2,
    IDs: [343,451,12]
    },
    {
    }
    ]
    """

        
        


def iclone_result_to_selection(iclone_result_file, output_dir):
    """
    outputdir/by-bench
    outputdir/by-ID
    """
    # iclone_result_file = "/home/hebi/.helium.d/iclones-result-javaout.txt"
    ID=1
    sels = []
    with open(iclone_result_file) as f:
        vaild = False
        for line in f:
            if (line.startswith("-- Type")):
                t = int(line.split()[2])
                if (t == 1):
                    # type 1 is omitted
                    valid = False
                    pass
                else:
                    valid = True
            if (line.startswith("File:")):
                filename = line.split()[1]
            if (line.startswith("Position:")):
                start,end = line.split()[1].split(",")
                start = int(start)
                end = int(end)
                if valid:
                    # add to result
                    lines = [{"line": i} for i in range(start,end+1)]
                    # sel.append({"file": filename, "sel": lines})
                    obj={"file": filename, "sel": lines}
                    obj["ID"]=ID
                    # dump each into a seperate file
                    # the file should be put into the corresponding folder
                    # by bench
                    
                    sels.append(obj)
                    # with open(selfile, "w") as jsonfile:
                    #     json.dump([obj], jsonfile, indent=2)
                    ID+=1
    # get the path name
    # the common prefix will be the path
    # then we can get the benchmark name
    bybench_dir = output_dir + '/' + 'by-bench'
    os.mkdir(bybench_dir)
    byid_dir = output_dir + '/' + 'by-ID'
    os.mkdir(byid_dir)
    files = []
    for item in sels:
        files.append(item['file'])
    prefix = os.path.commonpath(files)
    for item in sels:
        bench = os.path.relpath(item['file'], prefix)
        bench = bench[:bench.find('/')]
        # FIXME examine bench
        benchdir = os.path.join(bybench_dir, bench)
        if not os.path.exists(benchdir):
            os.mkdir(benchdir)
        by_bench_file = os.path.join(benchdir, str(item['ID'])+'.json')
        with open(by_bench_file, 'w') as jsonfile:
            json.dump([item], jsonfile, indent=2)
        by_id_file = os.path.join(byid_dir, str(item['ID'])+'.json')
        with open(by_id_file, 'w') as jsonfile:
            json.dump([item], jsonfile, indent=2)


def iclone_result_to_pairs(iclone_result_file, output_dir):
    # iclone_result_file = "/home/hebi/.helium.d/iclones-result-javaout.txt"
    ID=1
    ret=[]
    with open(iclone_result_file) as f:
        vaild = False
        # sel = []
        tmp={}
        for line in f:
            if (line.startswith("-- Type")):
                # dump current tmp
                if tmp:
                    ret.append(tmp)
                    tmp={}
                t = int(line.split()[2])
                if (t == 1):
                    # type 1 is omitted
                    valid = False
                    pass
                else:
                    valid = True
                    tmp['type']=t
                    tmp['IDs']=[]
            if (line.startswith("Position:")):
                if valid:
                    tmp['IDs'].append(ID)
                    ID+=1
        if tmp:
            ret.append(tmp)
    # dump ret
    with open(os.path.join(output_dir, "pairs.json"), "w") as f:
        json.dump(ret, f, indent=2)


def run_iclones(indir, outdir):
    cmd = 'iclones -language c++ -outformat rcf -input '\
          + indir + ' -output ' + outdir
    print('==>', cmd)
    subprocess.run(cmd, shell=True)
def run_rcfreader(indir, outdir):
    cmd = 'rcfreader ' + indir + ' > ' + outdir
    print('==>', cmd)
    subprocess.run(cmd, shell=True)
def run_parse_result(result_file, outdir):
    iclone_result_to_selection(result_file, outdir)
    iclone_result_to_pairs(result_file, outdir)
def create_iclones_selection(indir, outdir):
    if os.path.exists(outdir): shutil.rmtree(outdir)
    os.mkdir(outdir);
    rcffile = os.path.join(outdir, 'iclones.rcf')
    resultfile = os.path.join(outdir, 'iclones-result.txt')
    run_iclones(indir, rcffile)
    run_rcfreader(rcffile, resultfile)
    run_parse_result(resultfile, outdir)

if __name__ == '__main__':
    # simply call --run-all and parse
    # input: benchS/<many benchmarks>
    # output: /path/to/seldir
    parser = argparse.ArgumentParser(description='Process iclones experiment')
    # the selection output is going to be
    # selection/
    # -- by-bench
    # -- -- sl-master
    # -- -- -- 8.json
    # -- -- -- 10.json
    # -- -- hello-master
    # -- by-ID
    # -- -- 1.json
    # -- -- 2.json
    # -- pairs.json
    parser.add_argument('--run-iclones', action='store_true',
                        help='input /path/to/benchS output file.rcf')
    parser.add_argument('--read-rcf', action='store_true',
                        help='input /path/to/file.rcf output /path/to/result.txt')
    parser.add_argument('--parse-result', action='store_true',
                        help='input /path/to/result.txt output /path/to/seldir')

    parser.add_argument('--run-all', action='store_true',
                        help='input: /path/to/benchS output: /path/to/seldir')
    parser.add_argument('--run-helium', action='store_true',
                        help='input: /path/to/benchS seldir: /path/to/seldir output: /output/folder')
    parser.add_argument('--run-simple', help='/path/to/benchS')
    
    parser.add_argument('--input')
    parser.add_argument('--seldir')
    parser.add_argument('--output')
    args = parser.parse_args()
    if args.run_iclones:
        run_iclones(args.input, args.output)
    elif args.read_rcf:
        run_rcfreader(args.input, args.output)
    elif args.parse_result:
        run_parse_result(args.input, args.output)
    elif args.run_all:
        rcffile = '/tmp/helium-iclones-result.rcf'
        resultfile = '/tmp/helium-iclones-result.txt'
        run_iclones(args.input, rcffile)
        run_rcfreader(rcffile, resultfile)
        run_parse_result(resultfile, args.output)
    elif args.run_helium:
        if not args.seldir:
            print('Require seldir to be set')
            exit(1)
        run_helium(args.input, args.seldir, args.output)

def gen_result_json():
    result_file = 'ioidpath.txt'
    ret = {}
    with open(result_file) as f:
        for line in f:
            [ID,Input,Output,path] = line.split()
            obj = {}
            obj['ID'] = ID
            obj['input'] = Input
            obj['output'] = Output
            obj['path'] = path
            ret[ID] = obj
    with open('ioidpath.json', 'w') as f:
        json.dump(ret, f, indent=2)
    # return ret
            
def compare():
    """
    compare for the pairs of clones, if the IO data is the same
    The output should be:
    - how many in total
    - how many have same amount of input/output
    - how many match the type of variable
    - how many match all IO data

    The output present format:
    
    """
    # res = gen_result_json()
    res = json.load(open('ioidpath.json'))
    pairs = json.load(open('pairs.json'))
    classID = 0
    ret = []
    for pair in pairs:
        objs = []
        for ID in pair['IDs']:
            if str(ID) in res:
                obj = res[str(ID)]
                objs.append(obj)
        # now everything is in objs, compare them
        print ("One class: ")
        classObj = {}
        classObj['classID'] = classID
        classID+=1
        classObj['type'] = pair['type']
        pairsObj = []
        for i in range(len(objs)):
            for j in range(i+1,len(objs)):
                pairObj = {}
                pairObj['ID1'] = objs[i]['ID']
                pairObj['ID2'] = objs[j]['ID']
                # compare i and j
                print ("comparing " + objs[i]['ID'] + ' and ' + objs[j]['ID'] + ' ...')
                [clone,iodata]=compare_file(objs[i]['path'], objs[j]['path'])
                pairObj['clone'] = clone
                pairObj['iodata'] = iodata
                pairsObj.append(pairObj)
                # if (clone):
                #     print ("is clone")
                # else:
                #     print ("is NOT clone")
        if pairsObj:
            classObj['pairs'] = pairsObj
            ret.append(classObj)
    # dump ret into file
    with open('clone_compare_result.json', 'w') as f:
        json.dump(ret, f, indent=2)
    return ret
        # for obj in objs:
        #     print (obj['input'] + '/' + obj['output'])

def compare_file(file1, file2):
    """
    comparing file1 and file2
    The format is
    input: int a=8
    output: char c=e
    """
    with open(file1) as f1, open(file2) as f2:
        f1_inputs = []
        f1_outputs = []
        f2_inputs = []
        f2_outputs = []
        # get input from f1
        # get output from f1
        # get input from f2
        # get output from f2
        for line in f1:
            t=line.split()[1] # type
            k=line.split()[2].split('=')[0] # key
            v=line.split('=')[1] # value
            if line.startswith('input'):
                f1_inputs.append({'type': t, 'key': k, 'value': v})
            if line.startswith('output'):
                f1_outputs.append({'type': t, 'key': k, 'value': v})
        for line in f2:
            t=line.split()[1] # type
            k=line.split()[2].split('=')[0] # key
            v=line.split('=')[1] # value
            if line.startswith('input'):
                f2_inputs.append({'type': t, 'key': k, 'value': v})
            if line.startswith('output'):
                f2_outputs.append({'type': t, 'key': k, 'value': v})
        # comparing f1 and f2
        # now only compare in alphabetical order
        # i'll do a arbitrary combination later
        if not len(f1_inputs) == len(f2_inputs): return [False, 'length']
        if not len(f1_outputs) == len(f2_outputs): return [False, 'length']

        f1i = [f1_inputs[i]['value'] for i in range(len(f1_inputs))]
        f2i = [f2_inputs[i]['value'] for i in range(len(f2_inputs))]
        f1o = [f1_outputs[i]['value'] for i in range(len(f1_outputs))]
        f2o = [f2_outputs[i]['value'] for i in range(len(f2_outputs))]

        if not sorted(f1i) == sorted(f2i):
            # return [False, [f1i, f2i, f1o, f2o]]
            return [False, [f1_inputs, f2_inputs]]
        if not sorted(f1o) == sorted(f2o):
            return [False, [f1_outputs, f2_outputs]]
        
        # for i in range(len(f1_inputs)):
        #     if not f1_inputs[i]['value'] == f2_inputs[i]['value']: return False
        # for i in range(len(f1_outputs)):
        #     if not f1_outputs[i]['value'] == f2_outputs[i]['value']: return False
        return [True, '']

def gen_table():
    """
    generate table for clone compare results
    """
    res = json.load(open('clone_compare_result.json'))
    ret=''
    line = ['classID', 'type', 'ID1', 'ID2', 'clone']
    ret += ','.join(line) + '\n'
    for classObj in res:
        for pair in classObj['pairs']:
            line=[]
            line.append(classObj['classID'])
            line.append(classObj['type'])
            line.append(pair['ID1'])
            line.append(pair['ID2'])
            line.append(pair['clone'])
            ret += ','.join([str(i) for i in line]) + "\n"
    # write line to table
    with open('compare_result.csv', 'w') as f:
        f.write(ret)


def extract_segment():
    """
    extract segment for each ID, from wholesel.json
    """
    wholejson = json.load(open('wholesel.json'))
    for segment in wholejson:
        filename=segment['file']
        firstline=segment['sel'][0]['line']
        lastline=segment['sel'][-1]['line']
        ID=segment['ID']
        linum=0
        output=''
        with open(filename) as f:
            for line in f:
                if linum>=firstline and linum <= lastline:
                    output+=line
                linum+=1
        with open('output/'+str(ID)+'.txt', 'w') as f:
            f.write(output)
    
def analyze_whole():
    j=json.load(open("wholesel.json"))
    len(j)
if __name__ == '__hebi__':
    # process()
    analyze_whole()
    get_pairs()
    gen_result_json()
    compare_res = compare()
    extract_segment()
    gen_table()
    pass
