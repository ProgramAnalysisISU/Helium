#!/usr/bin/env python3
# parse the output file from HELIUM

# HELIUM_INPUT_SPEC
# int_iname.strlen = 151
# addr_iname = 0x61300000de80
# isnull_iname = 0
# HELIUM_INPUT_SPEC_END
# HELIUM_POI_INSTRUMENT
# int_ifname.size = 1024
# addr_ifname = 0x6059e0
# int_iname.strlen = 151
# addr_iname = 0x61300000de80
# isnull_iname = 0
# HELIUM_POI_INSTRUMENT_END
# HELIUM_AFTER_POI
# HELIUM_TEST_SUCCESS

# The table looks like:

# |       | input  |        |        | output  |         |         | code |
# |-------+--------+--------+--------+---------+---------+---------+------|
# |       | input1 | input2 | input3 | output1 | output2 | output3 |      |
# |-------+--------+--------+--------+---------+---------+---------+------|
# | test1 |        |        |        |         |         |         |      |
# | test2 |        |        |        |         |         |         |      |
# | test3 |        |        |        |         |         |         |      |



import sys

class Data(object):
    "Data"
    stage = 0
    output_dict = dict()
    input_dict = dict()
    status_code = 0
    reach_code = 1
    def __init__(self):
        # (HEBI: 1. self 2. reinit the dict)
        self.output_dict = dict()
        self.input_dict = dict()
    def add(self, key, value):
        "Add key and value pair"
        if self.stage == 1:
            # input
            self.input_dict[key] = value
        elif self.stage == 2:
            self.output_dict[key] = value
        else:
            pass
    def start_input(self):
        "set the stage to input"
        self.stage = 1
    def start_output(self):
        "Set the stage to output"
        self.stage = 2
    def clear_output(self):
        "Clear the output dictionary"
        self.output_dict.clear()
    def set_status_code(self, code):
        "Set status code"
        self.status_code = code
    def set_reach_code(self, code):
        "set reach code"
        self.reach_code = code
    def clear(self):
        "clear all dictionary"
        self.input_dict.clear()
        self.output_dict.clear()

def get_header(inputs, outputs):
    "get the header of the table"
    lst = []
    # the input_ and output_ prefix will be added
    for i in inputs:
        lst.append('input_' + i)
    for output in outputs:
        lst.append('output_' + output)
    lst.append("reach_code")
    lst.append("status_code")
    return ",".join(lst)

def get_row(row, inputs, outputs):
    "get the row in string"
    i_d = row.input_dict
    o_d = row.output_dict
    lst = []
    for i in inputs:
        if i in i_d:
            lst.append(i_d[i])
        else:
            lst.append("NA")
    for output in outputs:
        if output in o_d:
            lst.append(o_d[output])
        else:
            lst.append("NA")
    lst.append(str(row.reach_code))
    lst.append(str(row.status_code))
    return ",".join(lst)
class Table(object):
    "Table"
    rows = []
    def add_row(self, row):
        "Add one row of data"
        self.rows.append(row)
    def gather_all_input(self):
        "Gather all inputs"
        inputs = set()
        for row in self.rows:
            keys = row.input_dict.keys()
            for key in keys:
                inputs.add(key)
        return inputs
    def gather_all_output(self):
        "Gather all outputs"
        outputs = set()
        for row in self.rows:
            keys = row.output_dict.keys()
            for key in keys:
                outputs.add(key)
        return outputs
    def to_csv(self):
        "Output To CSV"
        # gather all the input and output of rows
        # if a field is not there, put NA
        # output CSV File
        inputs = self.gather_all_input()
        outputs = self.gather_all_output()
        ret = ""
        ret += get_header(inputs, outputs)
        for row in self.rows:
            ret += "\n"
            ret += get_row(row, inputs, outputs)
        return ret

def parse(filename):
    "parse the file"
    data = Data()
    table = Table()

    for line in open(filename):
        if line.find("HELIUM_INPUT_SPEC_END") is not -1:
            # end of input spec
            data.set_reach_code(3)
        elif line.find("HELIUM_INPUT_SPEC") is not -1:
            # start of input spec
            data.start_input()
            data.set_reach_code(2)
        elif line.find("HELIUM_POI_INSTRUMENT_END") is not -1:
            # end of output instrument
            # start of POI
            data.set_reach_code(5)
        elif line.find("HELIUM_POI_INSTRUMENT") is not -1:
            # start of output instrument
            data.start_output()
            data.set_reach_code(4)
        elif line.find("HELIUM_AFTER_POI") is not -1:
            # POI has passed peacefully
            data.set_reach_code(6)
        elif line.find("HELIUM_TEST_SUCCESS") is not -1:
            # record the return code
            data.set_status_code(0)
            table.add_row(data)
            # start new row
            data = Data()
        elif line.find("HELIUM_TEST_FAILURE") is not -1:
            data.set_status_code(1)
            table.add_row(data)
            data = Data()
        elif line.find("=") is not -1:
            key = line.split('=')[0].strip()
            value = line.split('=')[1].strip()
            data.add(key, value)
    return table

if __name__ == "__main__":
    if len(sys.argv) is 1:
        print("Provide a file to parse")
        sys.exit(0)
    FILENAME = sys.argv[1]
    TABLE = parse(FILENAME)
    CSV = TABLE.to_csv()
    print(CSV)
