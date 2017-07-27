# SMYT (Show me your trees): Generates Haskell code for displaying trees.
# Note: The script is meant for testing purposes only.
# Author: Maciej Bendkowski <maciej.bendkowski@tcs.uj.edu.pl>
import subprocess
import argparse

# output specification
output = dict()

def datatype (n):
    return "Node" + str(n)

def args (deg):
    ret = ""
    for idx in range(0,deg):
        ret = ret + ' x' + str(idx)
    return ret

def rhs (deg):
    if deg == 0:
        return "\"()\""
    else:
        ret = "\"(\""
        for idx in range(0,deg):
            ret = ret + ' ++ show x' + str(idx)
        ret = ret + " ++ \")\""
        return ret

with open('input.deg', 'r') as f:
    lines = f.readlines()
    number_of_degrees = int(lines[0].strip())
    for idx in range(0,number_of_degrees):
        line = lines[idx+1].split()
        deg = int(line[0].strip()) # degree
        assert output.has_key(deg) == False, \
                "duplicate degree"

        if len(line) > 1:
            freq = float(line[1].strip()) # frequency
            assert 0.0 <= freq and freq <= 1.0, \
                    "incorrect frequencies, expected [0,1]"

            output[deg] = freq
        else:
            output[deg] = -1.0

print("")
print ("instance Show Tree where")
for deg, freq in output.iteritems():
    if deg == 0:
        print ("   show " + datatype(deg) + " = " + rhs(deg))
    else:
        print ("   show (" + datatype(deg) + args(deg) + ") = " + rhs(deg))
