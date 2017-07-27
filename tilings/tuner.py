# Tuning script for Boltzmann brain and Paganini.
# Note: The script is meant for testing purposes only.
# Author: Maciej Bendkowski <maciej.bendkowski@tcs.uj.edu.pl>
import subprocess
import argparse

parser = argparse.ArgumentParser(description='Tuning script for bb and paganini.py')
parser.add_argument('specification', nargs="+", help='Boltzmann brain specification file.')
args = parser.parse_args()

def gen_paganini_spec (spec, tmpFile):
    arg = ("bb", "--force", "-g", spec)
    with open(tmpFile, 'w') as specFile:
        popen = subprocess.Popen(arg, stdout=specFile)
        popen.communicate()

def gen_bb_spec (tmpFile, paganiniOut, prec, solver):
    arg = ("python2", "../paganini.py", tmpFile, prec, solver)
    with open(paganiniOut, 'w') as pgFile:
        popen = subprocess.Popen(arg, stdout=pgFile)
        popen.communicate()

def gen_module(spec, tmpFile):
    arg = ("bb", "--force", "--with-io", "-m" "Sampler", "-t", tmpFile, spec)
    with open('Sampler.hs', 'w') as moduleFile:
        popen = subprocess.Popen(arg, stdout=moduleFile)
        popen.communicate()

paganini_input = 'paganini.pg'
bb_params = 'bb.param'

print ("Generating Paganini specification...")
gen_paganini_spec(args.specification[0], paganini_input)

print ("Calculating parameters...")
gen_bb_spec(paganini_input, bb_params, '1e-20', 'SCS')

print ("Generating Haskell module...")
gen_module(args.specification[0], bb_params)

print ("Finished.")
