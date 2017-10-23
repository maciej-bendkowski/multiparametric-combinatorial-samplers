__author__    = "Sergey Dovgal"
__copyright__ = "Copyright (C) 2017 Sergey Dovgal"
__license__   = "Public Domain"
__version__   = "1.0"

flag_debug        = True
N_COLORS          = 10
MAX_SUBSTITUTIONS = 10
EXPECTATIONS      = [20] * 10

assert len(EXPECTATIONS) == N_COLORS

#
##
### IMPORTS
##
#

#-- Stdio manipulations, read, write and stderr
try:
    import sys
    import os
except:
    print "Something went wrong, cannot import package 'sys' or 'os'"
    exit(1)

if sys.version_info.major > 2:
    sys.stderr.write('You are using Python 3. Please use Python 2.\n')
    exit(1)

#-- Better hints at non-installed packages
list_of_noninstalled_packages = []

try:
    import cvxpy
except:
    list_of_noninstalled_packages += ['cvxpy']

try:
    import numpy as np
except:
    list_of_noninstalled_packages += ['numpy']

if len(list_of_noninstalled_packages) > 0:
    sys.stderr.write("""It seems that you need to install some packages.
Please be patient and type into your command line
    pip2 install """ + ' '.join(list_of_noninstalled_packages) + """
If you have only Python2 installed, you can also try
    pip install """ + ' '.join(list_of_noninstalled_packages) + """
Good luck!
""")
    sys.exit(1)

# --- technical inclusions
from numpy import log, exp
np.set_printoptions(precision=14)

sys.stderr.write("""
-----------------------------------------------------------------------
Generate weighted partitions with predefined frequencies of each colour
-----------------------------------------------------------------------
""")

#
## ERROR MESSAGES
#

class bcolors:
    HEADER = '\033[95m'
    OKBLUE = '\033[94m'
    OKGREEN = '\033[92m'
    WARNING = '\033[93m'
    FAIL = '\033[91m'
    ENDC = '\033[0m'
    BOLD = '\033[1m'
    UNDERLINE = '\033[4m'

welcome_message = bcolors.BOLD + """Welcome to permutations.py!
""" + bcolors.ENDC

usage_message = bcolors.UNDERLINE + """
usage:""" + bcolors.ENDC + """ python2 permutations.py
"""

#
##
###  PARSING COMMAND LINE ARGUMENTS
##
#

if not flag_debug:
    pass
else:
    precision = 1e-10

def basis_vector(idx, dim):
    assert idx > 0
    if idx == dim:
        return [0] * dim
    result = [0] * dim
    result[idx-1] = 1
    return result

#
##
### MAIN CODE
##
#

sys.stderr.write("Composing optimization problem... ")

## System of generating functions for weighted partitions
## ======================================================
## Number of colors = 5

n_colors = N_COLORS
n_substitutions = MAX_SUBSTITUTIONS
expectations = np.array(EXPECTATIONS)
#expectations = 200 * np.array([
#    0.5, 0.3, 0.1, 0.07, 0.03
#])


# Target function B for Bose-Einstein
B = cvxpy.Variable()

# Marking variables
u = cvxpy.Variable(n_colors)

# Multisets of colors
m = cvxpy.Variable(n_substitutions)

objective = cvxpy.Minimize( B - expectations * u )

constraints = [
    B >= np.array([
        1.0 / (j + 1)
        for j in xrange(n_substitutions)
    ]) * m
] + [
    cvxpy.log(m[idx] + 1) >= - sum([
        cvxpy.log(1 - cvxpy.exp((idx+1) * u_j))
        for u_j in u
    ])
for idx in range(n_substitutions)
]

prob = cvxpy.Problem(objective, constraints)

sys.stderr.write("Done!\n")

sys.stderr.write("Solving the problem.\n")

old_stdout = sys.stdout
sys.stdout = sys.stderr

result = prob.solve(solver=cvxpy.SCS, verbose=True, eps=1e-20)
#result = prob.solve(solver=cvxpy.ECOS, verbose=True, feastol=precision)

sys.stderr.write("Solved.\n")

sys.stdout = old_stdout

##
## Generate symmetric polynomials and branching probabilities
## for each substitution step
##

if flag_debug:
    np.set_printoptions(precision = 7)

s = np.array([
    [
        exp((subs_idx+1) * u_j.value) / (1. - exp((subs_idx+1) * u_j.value))
        for u_j in u
    ]
    for subs_idx in range(n_substitutions)
])

parts = np.zeros([n_substitutions,n_colors,n_colors])
sums = np.zeros([n_substitutions,n_colors,n_colors])
for subs_idx in range(n_substitutions):
    parts[subs_idx][0] = s[subs_idx]
    for n_iter in range(n_colors):
        sums[subs_idx][n_iter][-1] = parts[subs_idx][n_iter][-1]
        for idx in reversed(range(n_colors)):
            if (idx < n_colors - 1) and (idx > n_iter-1):
                sums[subs_idx][n_iter][idx] = \
                    sums[subs_idx][n_iter][idx+1] + \
                    parts[subs_idx][n_iter][idx]
            if idx > n_iter:
                parts[subs_idx][n_iter + 1][idx] = \
                    sums[subs_idx][n_iter][idx] * \
                    s[subs_idx][idx - n_iter - 1] ** (subs_idx + 1)

# branching probabilities correspond to which symmetric polynomial to choose
branching_probabilities = np.array([
    np.sum(part, axis = 1) / np.sum(part)
    for part in parts
])

##
## Generate random variable according to array of weights
##

def random_weighted_choice(weights):
    n_elements = len(weights)
    return np.random.choice(n_elements, p=weights/sum(weights))

##
## Sample one row of Young tableau using Boltzmann
## Gamma A(z^k)
##

elements = range(n_colors)
def sample_from_nth_substitution(subs_idx):
    """
    Example, consider 5 possible colors.
    The 'parts' array looks like
    [
        [s1       s2            s3      s4                    s5      ]
        [0        s1(s2+...+s5) ...     s3(s4+s5)             s4 s5   ]
        [0        0             s1(...) s2(s3(s4+s5)+s4 s5)   s3 s4 s5]
        [0        0             0       s1(...)               s2...s5 ]
        [0        0             0       0                     s1...s5 ]
    ]
    The sum of each row is symmetric polynomial.

    I.    Sample how many colors we will have
    II.   Start from the row number equal to number of colors sampled at step I.
              Sample the index of the first color.
    III.  On each consequent step, the row index is decreased by 1.
              The sampled color on previous step determines the slice of the row,
              starting from 'left' to the end of the row.
    IV.   Several final steps will all be in the last column meaning that
              the length of the slice is 1 and no freedom of choice is allowed.
    """
    sample_colors = np.random.choice(
            elements,
            p=branching_probabilities[subs_idx]
    )
    color_indices = [0] * (sample_colors+1)
    left = sample_colors
    for idx in range(sample_colors+1):
        row = sample_colors - idx
        nth_idx = random_weighted_choice(parts[subs_idx][row][left:])
        color_indices[idx] = left - row + nth_idx
        assert color_indices[idx] >= 0
        assert color_indices[idx] < n_colors
        left += nth_idx

    # color_indices contains the indices of colors
    # sample geometric r.v. for each color index

    result = [0] * n_colors

    for idx in range(sample_colors+1):
        color_idx = color_indices[idx]
        result[color_idx] = (subs_idx+1) * np.random.geometric(1. - exp((subs_idx+1) * u[color_idx].value))
    return result

##
## First step of Boltzmann generation
## Determine the number of rows in Young tableau
##

def get_number_of_rows():
    """
    Warning: the answer can be zero with low but positive probability!
    The user should repeat the call until the number of rows is positive.
    """
    partial_sums = np.zeros(n_substitutions+1)
    for idx in range(n_substitutions):
        partial_sums[idx+1] = partial_sums[idx] + m[idx].value / (idx + 1.0)

    partial_sums -= partial_sums[-1]
    partial_sums = exp(partial_sums)

    # Artificially truncate because I don't want 0 rows
    partial_sums[0] = 0.

    elements = range(n_substitutions)
    probabilities = np.zeros(n_substitutions)
    for idx in range(n_substitutions):
        probabilities[idx] = partial_sums[idx+1] - partial_sums[idx]

    return np.random.choice(elements, p=probabilities)

n_rows = 0
while n_rows == 0:
    n_rows = get_number_of_rows()

def positive_poisson(param):
    """
    Positive Poisson is ordinary Poisson conditioned to be positive.
    This is achieved by rejection sampling.
    """
    result = 0
    while result == 0:
        result = np.random.poisson(lam=param)
    return result

def sample_tableau():
    """
    Boltzmann sampler for multicoloured Young tableaux
    """
    tableau = []

    for subs_idx in range(n_rows):
        if subs_idx < n_rows - 1:
            # 2a. `subs_idx` is NOT maximal.
            n_certain_rows = np.random.poisson(m[subs_idx].value)
        else:
            # 2a. `subs_idx` IS maximal.
            n_certain_rows = positive_poisson(m[subs_idx].value)
        for idx in range(n_certain_rows):
            tableau += [ sample_from_nth_substitution(subs_idx) ]
    return np.array(tableau)

def lex_cmp(a, b):
    # If the sum is different, sort by sum
    s_a = sum(a)
    s_b = sum(b)
    if s_a < s_b:
        return -1
    if s_a > s_b:
        return 1

    # find index of the first non matching element
    nonmatching = np.where( (a>b) != (a<b) )[0]
    if len(nonmatching) == 0:
        return 0
    idx = nonmatching[0]

    if a[idx] < b[idx]:
        return -1
    if a[idx] > b[idx]:
        return 1

tableau = sample_tableau()
tableau = np.array(sorted(tableau, cmp=lex_cmp))

##
## Create visualization output
## Start from the lowest row and put corresponding tiles
## Format:
##      N_TILES WIDTH
##      1 1             // repeat
##      +               // n_colors times
##      N_PLACEMENTS
##      POS_X TILE_IDX

width = sum(tableau[-1])
n_placements = np.sum(tableau)

placements = []

sys.stdout.write(str(n_colors) + ' ' + str(width) + '\n')
for idx in range(n_colors):
    sys.stdout.write('1 1\n')
    sys.stdout.write('+\n')
sys.stdout.write(str(n_placements) + '\n')
for row in reversed(tableau):
    position = 0
    for color in xrange(n_colors):
        for it in xrange(row[color]):
            placements += [ (position, color) ]
            position += 1

for (pos, color) in reversed(placements):
    sys.stdout.write(str(pos) + ' ' + str(color) + '\n')

##
## DONE.
##
