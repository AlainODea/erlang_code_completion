#!/usr/bin/env python
import socket
import sys
import os
tm_line_number = int(os.getenv('TM_LINE_NUMBER'))
tm_line_index = int(os.getenv('TM_LINE_INDEX'))
module = os.getenv('TM_FILEPATH').split('/').pop().split('.').pop(0)
s = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
s.connect(('127.0.0.1',2345))
s.send(module + os.linesep)
line = 1
for line in sys.stdin:
    if line == tm_line_number:
        s.send(line[0:tm_line_index].rstrip() + os.linesep)
        break
    else:
        s.send(line.rstrip() + os.linesep)
s.send('\f' + os.linesep)
print s.recv(4096)