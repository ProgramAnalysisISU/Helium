#!/usr/bin/env python3

"""Compare analyze for output csv file.
"""
import logging
from argparse import ArgumentParser

logger = logging.getLogger(__name__)

def parse(file):
    """
    :param file: csv file name
    :return string as result
    """
    result = ''
    with open(file) as f:
        content = f.read().strip()
    lines = content.split('\n')
    header = lines.pop(0)
    if len(lines) == 0:
        logger.warning('no data')
        return ''
    variables = header.split(',')
    var_number = len(variables)
    var_list = []
    # header
    var_set = set()
    for var in variables:
        var = var.strip()
        if var in var_set:
            var = var+'_after'
        var_set.add(var)
        var_list.append(Variable(var))
    # data
    for line in lines:
        # if 'NA' in line: continue
        values = line.split(',')
        for idx,v in enumerate(values):
            # if v == 'NA': break
            var_list[idx].add(v)
    # single property
    for i in range(var_number):
        var_list[i].analyze_property()
        result += var_list[i].property
    # relation
    for i in range(var_number):
        for j in range(i+1, var_number):
            result += check_relation(var_list[i], var_list[j])
    return result.strip()

def check_equal_to(var1, var2):
    valid_count = 0
    for i in range(len(var1.data)):
        if 'NA' in var1.data or 'NA' in var2.data: continue
        if var1.data[i] != var2.data[i]: return False
        valid_count += 1
    if valid_count > 5:
        return True
    else:
        return False
def check_less_than(var1, var2):
    valid_count = 0
    for i in range(len(var1.data)):
        if 'NA' in var1.data or 'NA' in var2.data: continue
        if var1.data[i] > var2.data[i]: return False
        valid_count += 1
    if valid_count > 5:
        return True
    else:
        return False
def check_relation(var1, var2):
    result = ''
    if len(var1.data) != len(var2.data):
        logger.error('csv data not consistant')
        exit(1)
    if check_equal_to(var1, var2):
        result += var1.name + ' = ' + var2.name + '\n'
    elif check_less_than(var1, var2):
        result += var1.name + ' <= ' + var2.name + '\n'
    elif check_less_than(var2, var1):
        result += var1.name + ' >= ' + var2.name + '\n'
    return result


class Variable:
    def __init__(self, name):
        self.name = name
        self.data = []
        self.property = ''
    def add(self, value):
        # FIXME double?
        if value == 'NA':
            self.data.append('NA')
        else:
            self.data.append(int(value))
    def print_property(self):
        pass
    def analyze_property(self):
        if not self.__single_value():
            self.__boundary()
    def __single_value(self):
        if len(set(self.data)) == 1:
            self.property += self.name + ' = ' + str(self.data[0]) + '\n'
            return True
        return False
    def __boundary(self):
        if [d for d in self.data if d != 'NA']:
            self.property += self.name + '.max = ' + str(max([d for d in self.data if d != 'NA'])) + '\n'
            self.property += self.name + '.min = ' + str(min([d for d in self.data if d != 'NA'])) + '\n'


if __name__ == '__main__':
    parser = ArgumentParser()
    parser.add_argument('-f', '--file', help='csv file to analyze', required=True)
    args = parser.parse_args()

    print(parse(args.file))
