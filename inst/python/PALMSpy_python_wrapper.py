#!/usr/bin/env python3

import sys
arg = sys.argv
print("Python function to use: ",  arg[0])
print("GPS folder: ",  arg[1])
print("Acc folder: ",  arg[2])
print("Output folder: ",  arg[3])
if (len(arg) > 3):
  print("Config file: ",  arg[4])
  
message = "PALMSpy analyses successful!"
f = open(arg[3]+"/testpython.csv", "w") # write file to output directory
f.write(message)
f.close()
