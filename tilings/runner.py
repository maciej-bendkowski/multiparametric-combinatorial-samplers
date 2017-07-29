# Note: This script is meant for testing purposes only.
# Author: Maciej Bendkowski <maciej.bendkowski@tcs.uj.edu.pl>
import subprocess
import argparse
import sys
import os

current_task = 0

def pipe (arg, output='', mode='w'):
    """ Run a script and pipe the stdout to given output """
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
        required=True, help='Runner mode (either compile, generate or clean).')

args = parser.parse_args()
if args.mode == 'compile':

    tasks = 7

    progress(tasks,"Construct the tiling...")
    pipe(['python2','tiling.py'])

    progress(tasks,"Generating paganini specification...")
    pipe(['bb','--force','-g','output.txt'],'paganini.pg')

    progress(tasks,"Calculating tuning parameters...")
    pipe(['paganini','-i','paganini.pg','-p','1.0e-20','-s','SCS', '-t', 'rational'],'bb.param')

    progress(tasks,"Sampler generation...")
    pipe(['bb','--force','--with-io','-m','Sampler','-t','bb.param','output.txt'],'tiling-generator/src/Sampler.hs')

    progress(tasks,"Generating string representation functions...")
    pipe(['python2','smyt.py','output.txt'],'tiling-generator/src/Sampler.hs','a')

    os.chdir('tiling-generator/')
    progress(tasks,"Compilation... May take some time...")
    pipe(['stack','install'])
    os.chdir('..')

    progress(tasks,"Done.")
    exit(0)

if args.mode == "generate":

    (lb, ub) = (500, 520)
    for idx in range(1,9):
        log("Sampling tile " + str(idx) + " ... (size window [" + str(lb)
                + ", " + str(ub) + "])")

        pipe(['tiling-generator',str(lb),str(ub)], 'tiling.viz')
        pipe(['sed', '$ d','tiling.viz'], 'tiling_clean.viz')
        pipe(['cat','input.tile'], 'final_tiling.viz')
        pipe(['awk','END {print NR}','tiling_clean.viz'], 'final_tiling.viz','a')
        pipe(['cat','tiling_clean.viz'], 'final_tiling.viz','a')
        read(['python2','viz_tilings.py','output'+str(idx)+'.eps'], 'final_tiling.viz')

    log("Generating an EPS file...")
    pipe(['convert','-density','1200','-quality','100','output*.eps','+append','tiling.eps'])

    log("Done.")
    exit(0)

if args.mode == "clean":

    pipe(['rm','bb.param'])
    pipe(['rm','paganini.pg'])
    pipe(['rm','output.txt'])
    pipe(['rm','tiling.viz'])
    pipe(['rm','tiling_clean.viz'])
    pipe(['rm','final_tiling.viz'])
    pipe(['rm','output1.eps'])
    pipe(['rm','output2.eps'])
    pipe(['rm','output3.eps'])
    pipe(['rm','output4.eps'])
    pipe(['rm','output5.eps'])
    pipe(['rm','output6.eps'])
    pipe(['rm','output7.eps'])
    pipe(['rm','output8.eps'])
    pipe(['rm','tiling.eps'])
    exit(0)

log("Illegal runner mode: expected compile, generate or clean.")
exit(1)
