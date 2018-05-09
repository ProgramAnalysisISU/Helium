#!/usr/bin/env python3

"""Analyzer util functions
"""
import logging

logger = logging.getLogger(__name__)


def parse_csv(csv_file):
    """
    header1,header2
    1,2
    3,4
    ...
    :return
    {
    'header1': [1,3],
    'header2': [2,4],
    ...
    }
    """
    result = {}
    with open(csv_file) as f:
        # I assume no empty lines
        lines = f.readlines()
        if len(lines) == 0:
            logger.warning('no data')
    header_line = lines.pop(0)
    headers = header_line.split(',')
    for h in headers:
        result[h.strip()] = []
    for line in lines:
        data = line.split(',')
        for idx,h in enumerate(headers):
            result[h.strip()].append(data[idx].strip())
    return result
