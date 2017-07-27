# SMYT (Show me your tillings): Generates Haskell code for displaying tilings.
# Note: The script is meant for testing purposes only.
# Author: Maciej Bendkowski <maciej.bendkowski@tcs.uj.edu.pl>
import subprocess
import argparse

parser = argparse.ArgumentParser(description='Generates Haskell code for displaying tilings')
parser.add_argument('specification', nargs=1, help='Boltzmann brain specification file.')
args = parser.parse_args()

# input specification file
input_file = args.specification[0]

# output specification
output = dict()

def get_tile (tile):
    return tile[4:] # hack...

def get_position (pos):
    return pos.split('_')[0][3:] # hack...

def add (eq, constr, pos, tile):
    if not (eq in output):
        output[eq] = dict()

    output[eq][constr] = (pos,tile)

def parse_constructor (line):
    constr_name = line.split(' ')[0].strip()
    tile_name = line.split(' ')[1].strip()
    position = get_position(constr_name)
    tile = get_tile(tile_name)
    add (eq, constr_name, position, tile)

with open(input_file, 'r') as f:

    data = f.readlines()
    eq = "Error"

    for line in data:
        if line.startswith(' '):
            # constructor case
            line = line.split('|')[1].strip()
            parse_constructor(line)
        else:
            # definition case
            eq = line.split('=')[0].strip()
            line = line.split('=')[1].strip()
            if len(line.split('|')) == 2:
                # there is a terminal...
                term = line.split('|')[0].strip()
                terminal_name = term.split(' ')[0].strip()
                add (eq, terminal_name, -1, -1)

                # ... and a constructor
                line = line.split('|')[1].strip()
                parse_constructor(line)
            else:
                # no terminal
                parse_constructor(line)

print("")
for datatype, constrs in output.iteritems():
    print ("instance Show " + datatype + " where")
    for constr, (position,tile) in constrs.iteritems():
        if position < 0 or tile < 0 or datatype.startswith('Tile'):
            print ("   show " + constr + " = \"\"")
        else:
            print ("   show (" + constr + " _ x) = \"" + str(position)
                    + " " + str(tile) + "\\n\" ++ show x")
    print("")
