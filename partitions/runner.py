# Note: This script is meant for testing purposes only.
# Author: Maciej Bendkowski <maciej.bendkowski@tcs.uj.edu.pl>
import subprocess
import argparse
import sys
import os

current_task = 0

def pipe (arg, output='', mode='w'):
    """ Run a script and pipe the std to given output """
    if output == '':
        popen = subprocess.Popen(arg)
        popen.communicate()
    else:
        with open(output, mode) as f:
            popen = subprocess.Popen(arg, stdout=f)
            popen.communicate()

def read(arg, input_file):
    """ Run a script and pipe the stdin to given input """
    with open(input_file, 'r') as f:
        popen = subprocess.Popen(arg, stdin=f)
        popen.communicate()

def log(message):
    sys.stderr.write(message + '\n')

def progress(total, status=''):
    """ see: https://gist.github.com/vladignatyev/06860ec2040cb497f0f3 """
    global current_task
    current_task += 1
    count = current_task

    bar_len = 60
    filled_len = int(round(bar_len * count / float(total)))

    percents = round(100.0 * count / float(total), 1)
    bar = '=' * filled_len + '-' * (bar_len - filled_len)

    sys.stdout.write('[%s] %s%s ...%s\r' % (bar, percents, '%', status))
    sys.stdout.flush()  # As suggested by Rom Ruben

# parse input
parser = argparse.ArgumentParser(description='Runner')
parser.add_argument('-m', '--mode', dest='mode',
        required=True, help='Runner mode (expected generate or clean).')

args = parser.parse_args()
if args.mode == "generate":

    tasks = 3
    progress(tasks, 'Generating input.viz...')
    pipe(['python2','partitions.py'],'input.viz')

    progress(tasks, 'Running visualisation script...')
    read(['python2','../tilings/viz_tilings.py','bose.eps','Spectral'],'input.viz')

    progress(tasks, 'Generating PNG file...')
    pipe(['convert','-density','300','bose.eps','bose.png'])
    exit(0)

if args.mode == "clean":

    pipe(['rm','input.viz'])
    pipe(['rm','bose.eps'])
    pipe(['rm','bose.png'])
    exit(0)

log("Illegal runner mode: expected generate or clean.")
exit(1)
