import sys
import commands

def execute (cmd):
    return commands.getoutput(cmd).rstrip('\n')

progname = sys.argv.pop(0)

if len(sys.argv) < 3:
    sys.exit ("usage: " + progname + " command translator [args]")

command = sys.argv.pop(0).replace(':',' ')
translator = sys.argv.pop(0).replace(':',' ')

newargs = command
for arg in sys.argv:
    out = execute ("echo " + arg + " | " + translator)
    newargs += (" " + out)

print "[translated to " + newargs + "]"
print (execute (newargs))
                   
