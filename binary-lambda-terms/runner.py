# Note: This script is meant for testing purposes only.
# Author: Maciej Bendkowski <maciej.bendkowski@tcs.uj.edu.pl>
import subprocess
import argparse
import sys
import os

current_task = 0

def pipe (arg, output=''):
    """ Run a script and pipe the std to given output """
    if output == '':
        popen = subprocess.Popen(arg)
        popen.communicate()
    else:
        with open(output, 'w') as f:
            popen = subprocess.Popen(arg, stdout=f)
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
        required=True, help='Runner mode (either compile, generate or clean).')

args = parser.parse_args()
if args.mode == 'compile':

    tasks = 5
    progress(tasks,"Generating paganini specification...")
    pipe(['bb','--force','-s','specification.in'],'paganini.pg')

    progress(tasks,"Calculating tuning parameters...")
    pipe(['paganini','-i','paganini.pg','-p','1.0e-9'],'bb.param')

    progress(tasks,"Sampler generation...")
    if not os.path.isdir("blt/src"):
        os.mkdir('blt/src')
    pipe(['bb','--force','-p','bb.param','specification.in'],'blt/src/Sampler.hs')

    os.chdir('blt/')
    progress(tasks,"Compilation... May take some time...")
    pipe(['stack','install'])
    os.chdir('..')

    progress(tasks,"Done.")
    exit(0)

if args.mode == "generate":

    (lb, ub) = (1000, 4000)
    log("Generating a random lambda term of size [" + str(lb) + ", " + str(ub) + "]...")
    pipe(['blt',str(lb),str(ub)])
    exit(0)

if args.mode == "clean":

    pipe(['rm','bb.param'])
    pipe(['rm','paganini.pg'])
    exit(0)

log("Illegal runner mode: expected compile, generate or clean.")
exit(1)
