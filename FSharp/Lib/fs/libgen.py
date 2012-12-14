import os
import re

regex=r'\$PARALLEL\$'

par_dir="par"
par_sub=".Parallel."

seq_dir="seq"
seq_sub="."

for fname in os.listdir('.'):
    if fname.endswith(".fs"):
        in_handle = open (fname)
        seq_handle = open (seq_dir + "/" + fname,'w')
        par_handle = open (par_dir + "/" + fname,'w')
        for line in in_handle:
            seq_handle.write (re.sub (regex,seq_sub,line))
            par_handle.write (re.sub (regex,par_sub,line))
