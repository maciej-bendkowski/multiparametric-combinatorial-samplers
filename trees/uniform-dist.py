# Generates trees.py input consisting of a uniform degree distribution.
# Note: The script is meant for testing purposes only.
# Author: Maciej Bendkowski <maciej.bendkowski@tcs.uj.edu.pl>
import argparse

parser = argparse.ArgumentParser(description='Generates trees.py input consisting of uniform degree')
parser.add_argument('max_degree', type=int, nargs=1, help='Max. node degree.')
args = parser.parse_args()

max_degree = args.max_degree[0]
freq = 1.0/max_degree

print(max_degree)
for k in range(max_degree):
    if k < 2:
        print(str(k))
    else:
        print(str(k) + " " + str(freq))
