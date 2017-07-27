import sys

#
## Helper methods
#

def node (n):
    return "Node" + str(n)

def trees (n):
    constrs = ""
    for x in range(0,n):
        constrs = constrs + " Tree"

    return constrs

def frequency (freq):
    if freq < 0.0:
        return ""
    else:
        return " [" + str(freq) + "]"

#
## Read the node degrees
#

deg_map = {}

with open('input.deg', 'r') as f:
    lines = f.readlines()
    number_of_degrees = int(lines[0].strip())
    for idx in range(0,number_of_degrees):
        line = lines[idx+1].split()
        deg = int(line[0].strip()) # degree
        assert deg_map.has_key(deg) == False, \
                "duplicate degree"

        if len(line) > 1:
            freq = float(line[1].strip()) # frequency
            assert 0.0 <= freq and freq <= 1.0, \
                    "incorrect frequencies, expected [0,1]"

            deg_map[deg] = freq
        else:
            deg_map[deg] = -1.0

#
## Print output grammar
#

idx = 0
n = len(deg_map.keys())
with open('output.txt', 'w+') as f:
    f.write("Tree = ")
    for deg, freq in deg_map.iteritems():
        if idx == 0:
            constr = ""
        else:
            constr = "| "

        constr = constr + node(deg) + trees(deg) + frequency(freq)
        if idx != n - 1:
            constr = constr + "\n"

        f.write(constr)
        idx = idx + 1

    f.write(".\n")
