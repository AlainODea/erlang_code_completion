#!/usr/bin/env python
import socket
import sys
import os
import subprocess
tm_line_number = int(os.getenv('TM_LINE_NUMBER'))
tm_line_index = int(os.getenv('TM_LINE_INDEX'))
module = os.getenv('TM_FILEPATH').split('/').pop().split('.').pop(0)
s = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
try:
    s.connect(('127.0.0.1',2345))
    s.send(module + os.linesep)
    line_num = 1
    for line in sys.stdin:
        if line_num == tm_line_number:
            s.send(line[0:tm_line_index].rstrip() + os.linesep)
            break
        line_num = line_num + 1
    s.send('\f' + os.linesep)
    sys.stdout.write(s.recv(4096).rstrip())
    sys.stdout.flush()
except socket.error:
    os.system('mkdir -p /Users/alain/Documents/Erlang/completion/ebin')
    os.system('erlc -o /Users/alain/Documents/Erlang/completion/ebin /Users/alain/Documents/Erlang/completion/src/*.erl')
    os.system('erl -noshell -detached -pa /Users/alain/Documents/Erlang/completion/ebin -s tm_complete_server')