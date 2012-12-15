import sys
import subprocess
import commands

def execute (cmd):
    return commands.getoutput(cmd).rstrip('\n')

progname = sys.argv.pop(0)

if len(sys.argv) < 3:
    sys.exit ("usage: " + progname + " command translator [args]")

command = sys.argv.pop(0)
translator = sys.argv.pop(0)

newargs = command
for arg in sys.argv:
    out = execute ("echo " + arg + " | " + translator)
    newargs += (" " + out)

print (execute (newargs))
                   
