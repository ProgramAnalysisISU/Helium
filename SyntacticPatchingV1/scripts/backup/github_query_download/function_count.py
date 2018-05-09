#!/usr/bin/env python3

import sys,os
import xml.etree.ElementTree as ET
from subprocess import check_output

count=0

for root,dirs,files in os.walk(sys.argv[1]):
    for f in files:
        if f.endswith('.c'):
            s = check_output('src2srcml --position ' + root + '/' + f, shell=True)
            # s = check_output('src2srcml --position ' + filename, shell=True)
            r = ET.fromstring(s)
            functions = r.findall('{http://www.sdml.info/srcML/src}function')
            count += len(functions)

print(count)
