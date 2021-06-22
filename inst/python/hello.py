#!/usr/bin/env python3

import sys
arg = sys.argv
message = 'Hello World! ' + arg[1] + ' ' +  arg[2]
print(message)

f = open(arg[2]+"/testpython.csv", "w")
f.write(message)
f.close()
# From here onward we will have the Python script for Habiuts
