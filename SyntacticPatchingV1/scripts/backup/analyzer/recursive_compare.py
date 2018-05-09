#!/usr/bin/env python3
import logging
from helium.analyzer.util import parse_csv

logger = logging.getLogger(__name__)

def compare(csv1,csv2):
    data1 = parse_csv(csv1)
    data2 = parse_csv(csv2)
    if '5678' not in data1 or '5678' not in data2: return False
    data1 = data1['5678']
    data2 = data2['5678']
    return minus_check(data1, data2)

def minus_check(data1, data2):
    new_data1 = []
    new_data2 = []
    if len(data1) != len(data2):
        return False
    for i in range(len(data1)):
        if data1[i] != 'NA' and data2[i] != 'NA':
            new_data1.append(int(data1[i]))
            new_data2.append(int(data2[i]))
    if len(new_data1) < 3:
        return False
    minus = new_data1[0]-new_data2[0]
    for i in range(len(new_data1)):
        if new_data1[i]-new_data2[i] != minus:
            return False
    return True
