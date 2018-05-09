#!/usr/bin/env python3

# sys.argv[1] should be a c file
# will output to a tmp file

# stdout will be the result
# python might crash
# need to set up the server container, dns for srcml-server-container

import sys
import socket

# assume the host is srcml-server-container
host = 'srcml-server-container'
port = 5678

s = socket.socket()
# print ('connecting server ..', file=sys.stderr)



# s.connect(('localhost', 5678))
# s.connect((sys.argv[1], 5678))
s.connect(('srcml-server-container', port))

# print ('connected. Sending file ..', file=sys.stderr)

if sys.argv[1] == '-':
    # use stdin
    # read from stdin
    # send via binary
    in_str = sys.stdin.read()
    with open('/tmp/helium-tmp.txt', 'w') as f:
        f.write(in_str)
    with open('/tmp/helium-tmp.txt', 'rb') as f:
        s.sendfile(f)
else:
    with open(sys.argv[1], 'rb') as f:
        s.sendfile(f)
# print ('Sent file. Shutting down send edge ..', file=sys.stderr)
s.shutdown(socket.SHUT_WR)
# s.close()
# print ('receiving result ..', file=sys.stderr)
data = s.recv(1024)
# write to output.xml
with open('/tmp/helium-output-tmp.xml', 'wb') as f:
    while data:
        f.write(data)
        data = s.recv(1024)
# print ('finished. Closing socket', file=sys.stderr)
s.close()

with open('/tmp/helium-output-tmp.xml') as f:
    print(f.read())
