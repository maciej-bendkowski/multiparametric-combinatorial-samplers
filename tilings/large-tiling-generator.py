# Large tiling input generator for given width and tiles 2 x w.
# Note: The script is meant for testing purposes only.
# Author: Maciej Bendkowski <maciej.bendkowski@tcs.uj.edu.pl>
import argparse
from itertools import chain, combinations
import sys

parser = argparse.ArgumentParser(description='Large tiling input generator')

parser.add_argument('target_width', metavar='W',
        type=int, nargs=1, help='Target tiling width')

parser.add_argument('max_tile_width', metavar='w',
        type=int, nargs=1, help='Max. tile width')

# parse input
args = parser.parse_args()
target_width = args.target_width[0]
max_tile_width = args.max_tile_width[0]

sys.stderr.write("Target tiling width: " + str(target_width) +"\n")
sys.stderr.write("Max. tile width: " + str(max_tile_width) + "\n")

def tile_numer(n):
    return sum(map(lambda x: 2**x, range(1,n+1)))

def powerset(iterable):
    xs = list(iterable)
    return list(chain.from_iterable(combinations(xs,n) for n in range(len(xs)+1)))

def print_tile(width, top):

    # figure out the tile height
    if len(top) == 0:
        height = 1
    else:
        height = 2

    # print the tile dimensions
    print(str(height) + " " + str(width))

    # print top
    if len(top) != 0:
        for idx in range(0,width):
            if idx in top:
                sys.stdout.write('+')
            else:
                sys.stdout.write('.')
        sys.stdout.write('\n')

    # print base
    for x in map(lambda x: '+', range(0,width)):
        sys.stdout.write('+')
    sys.stdout.write('\n')

sys.stderr.write("Generating...\n")
print (str(tile_numer(max_tile_width)) +" "+ str(target_width))

for k in range(1,max_tile_width+1):
    subsets = powerset(range(0,k))
    for s in subsets:
        print_tile(k,s)

sys.stderr.write("Done.\n")
