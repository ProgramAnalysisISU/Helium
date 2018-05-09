#!/usr/bin/env python3

# this file parse the testing trace and gives out a csv file.

# the file are separated into each segment, by the line of:
# [1;36mProcessing query with the new node

# for each section, the following fields (lines) are of interest

# [0m[1;35mSegment Meta:
# [0m	Segment Size: 24
# 	Procedure Number: 1
# 	Branch Number: 0
# 	Loop Number: 0
# 	Compile: true
# [1;35mTest Meta:[0m
# 	Number of input variables: 1
# 	Number of tests: 20
# 	Test Generation Time: 0.000 seconds
# 	Total Testing Time: 0.081 seconds
# 	Number of Pass Test: 10
# 	Number of Fail Test: 10

# so the plan is:
# separate the file into several segments
# each segment forms a line in csv

# each line contains:
# Procedure Number, Branch Number, Loop Number,
# Compile, #var, #tests, #gen time, #total time,
# #pass, #fail

import sys

# if (os.path.exists(filename)):
#     print("file " + filename + " does not exist.")
#     sys.exit(0)



def dump_header():
    "doc"
    print("seg_size, proc_num, branch_num, loop_num, compilible,", end="")
    print("var_num, test_num, gen_time, total_time,", end="")
    print("stmt_cov, branch_cov,", end="")
    print("pass_num, fail_num,", end="")
    print("covered_num, bug_triggered_num")

class Data(object):
    "doc"
    seg_size = 0
    proc_num = 0
    branch_num = 0
    loop_num = 0
    compilible = False
    var_num = 0
    test_num = 0
    gen_time = ""
    total_time = ""
    stmt_cov = ""
    branch_cov = ""
    pass_num = 0
    fail_num = 0
    total_reach_poi = 0
    total_fail_poi = 0
    def __init__(self):
        pass
    def dump(self):
        "doc"
        # if not self.compilible:
        #     return
        print(self.seg_size, end=",")
        print(self.proc_num, end=",")
        print(self.branch_num, end=",")
        print(self.loop_num, end=",")
        print(self.compilible, end=",")
        print(self.var_num, end=",")
        print(self.test_num, end=",")
        print(self.gen_time, end=",")
        print(self.total_time, end=",")
        print(self.stmt_cov, end=",")
        print(self.branch_cov, end=",")
        print(self.pass_num, end=",")
        print(self.fail_num, end=",")
        print(self.total_reach_poi, end=",")
        print(self.total_fail_poi)


if __name__ == '__main__':
    if len(sys.argv) == 1:
        print("Provide a file to parse")
        sys.exit(0)

    FILENAME = sys.argv[1]

    data = Data()
    dump_header()
    for line in open(FILENAME):
        if line.find("Processing query") is not -1:
            # print out
            data.dump()
            # start new segment
            data = Data()
        if line.find("Segment Size") is not -1:
            data.seg_size = int(line.split(':')[1])
        if line.find("Procedure Number") is not -1:
            data.proc_num = int(line.split(':')[1])
        if line.find("Branch Number") is not -1:
            data.branch_num = int(line.split(':')[1])
        if line.find("Loop Number") is not -1:
            data.loop_num = int(line.split(':')[1])
        if line.find("Compile") is not -1:
            # print (line.split(':')[1].strip())
            comp = line.split(':')[1].strip()
            if comp == 'true':
                data.compilible = True
            else:
                data.compilible = False
        if line.find("Number of input") is not -1:
            data.var_num = int(line.split(':')[1])
        if line.find("Number of tests") is not -1:
            data.test_num = int(line.split(':')[1])
        if line.find("Test Generation Time") is not -1:
            data.gen_time = line.split(':')[1].strip()
        if line.find("Total Testing Time") is not -1:
            data.total_time = line.split(':')[1].strip()
        if line.find("Stmt Coverage") is not -1:
            data.stmt_cov = line.split(':')[1].strip()
        if line.find("Branch Coverage") is not -1:
            data.branch_cov = line.split(':')[1].strip()
        if line.find("Number of Pass Test") is not -1:
            data.pass_num = int(line.split(':')[1])
        if line.find("Number of Fail Test") is not -1:
            data.fail_num = int(line.split(':')[1])
        if line.find("Total reach poi") is not -1:
            data.total_reach_poi = int(line.split(':')[1])
        if line.find("Total fail poi") is not -1:
            data.total_fail_poi = int(line.split(':')[1])
