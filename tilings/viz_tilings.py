__author__    = "Sergey Dovgal"
__copyright__ = "Copyright (C) 2017 Sergey Dovgal"
__license__   = "Public Domain"
__version__   = "0.29559"
#
##
### VIZUALIZE TILINGS
##
#

### Global configuration for easier plotting first

step = 50  # The size of a square inside the tile
lw   = 0.1    # Linewidth of tile border

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
    exit(0)

if sys.version_info.major > 2:
    sys.stderr.write('You are using Python 3. Please use Python 2.\n')
    exit(0)

#-- Better hints at non-installed packages
list_of_noninstalled_packages = []

try:
    from stat import S_ISFIFO
except:
    list_of_noninstalled_packages += ['stat']

try:
    import matplotlib.pyplot as plt
    import matplotlib.patches as patches
    import matplotlib.cm as colormap
    from matplotlib.colors import LinearSegmentedColormap
    from matplotlib.path import Path
except:
    list_of_noninstalled_packages += ['matplotlib']

try:
    import colorsys
except:
    list_of_noninstalled_packages += ['colorsys']

try:
    import numpy as np
except:
    list_of_noninstalled_packages += ['numpy']

if len(list_of_noninstalled_packages) > 0:
    sys.stderr.write("""It seems that you need to install some packages.
Please be patient and type into your command line
    pip2 install """ + ' '.join(list_of_noninstalled_packages) + """
If you have only Python2 installed, you can also try
    pip2 install """ + ' '.join(list_of_noninstalled_packages) + """
Good luck!
""")

#-- Now we can assume that all the packages are installed and imported

#
## Random colormap definition.
#

def rand_cmap(nlabels):
    # Generate soft pastel colors, by limiting the RGB spectrum
    # Part of code reused thanks to https://github.com/delestro/rand_cmap
    # only soft colors are used
    low = 0.4
    high = 0.9
    randRGBcolors = [(np.random.uniform(low=low, high=high),
                      np.random.uniform(low=low, high=high),
                      np.random.uniform(low=low, high=high)) for i in xrange(nlabels)]

    random_colormap = LinearSegmentedColormap.from_list('new_map', randRGBcolors, N=nlabels)

    return random_colormap

#
## Which symbol is used for the purpose of representing the tile
#

# Flag defined for testing the program
flag_debug = False

# Which symbol for occupied squares of a tile
symbol_yes = '+'
# Which symbol for non-occupied square of a tile
symbol_no  = '.'

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

welcome_message = bcolors.BOLD + """Welcome to viz_tilings.py!
""" + bcolors.ENDC

usage_message = bcolors.UNDERLINE + """
usage:""" + bcolors.ENDC + """ cat input.txt | python2 viz_tilings.py output.eps
""" + bcolors.UNDERLINE + """usage:""" + bcolors.ENDC + """ python2 viz_tilings.py output.eps < input.txt
"""

hint_message = """
help:  python2 viz_tilings.py -h
"""

input_message = """
The program reads all the data from stdin and creates a picture, so after
you compose your  favourite 'input.txt'  in  the format  described  below,
you can use the program to  convert the tiling  description from sequence
of placements into actual '.eps' picture.

We assume that the sequence of tiles forms a tilings without holes.
If this condition is not met, overlaps and undefined behaviour occur.
"""

example_message = """
Input file consists of
- <number of tiles> <width of the stripe>
- description of the tiles started with <height> and <width> of each tile
- number of tiles to be placed
- sequence of placements in the format
    <pos_x> <tile_idx>

""" + bcolors.BOLD + """Example of a valid input file:""" + bcolors.ENDC + """
2 5
2 1
+
+
1 2
++
7
0 0
1 1
3 1
1 0
2 0
3 1
4 0

Resulting tiling from the example (using numbers in the order of placement
starting from 0 and finishing by 6):
            [6]
   [3][4]   [6]
[0][3][4][5  5]
[0][1  1][2  2]
---------
 0  1  2  3  4 <- positions

In the above example, first line defines the number of tiles and width
The second line gives dimension of the first tile, and so on.
These tiles are dominoes 2x1 and 1x2.
After the second tile, we put 7 tiles in a certain sequence.
The second number in each line marks the left-bottom point in which we place
the next tile. They will stack one above the other.

Each tile is given in ASCII format using the symbols '.' and '+' like this:
4 3 <- height and width
.+.
+++
++.
+..
We assume that the tiles are convex and there are no empty rows or empty columns.
"""

short_message \
    = welcome_message \
    + usage_message \
    + hint_message

help_message \
    = welcome_message \
    + usage_message \
    + input_message \
    + example_message

#
## DETECT IF THE USER REQUIRES ANY KIND OF HELP MESSAGE
#

colorscheme = 'terrain'

if len(sys.argv) < 2:
    sys.stderr.write(short_message)
    exit(1)
if (len(sys.argv) == 2) and (sys.argv[1] == '-h'):
    sys.stderr.write(help_message)
    exit(1)
if (len(sys.argv) == 3):
    colorscheme = sys.argv[2]

filename = sys.argv[1]

#
## Technical definitions for code development
#

def display_tile(tile, file=None):
    for row in tile:
        for column in row:
            if column == 1:
                if file == None:
                    print symbol_yes,
                else:
                    file.write(symbol_yes)
            else:
                if file == None:
                    print symbol_no,
                else:
                    file.write(symbol_no)
        if file == None:
            print ''
        else:
            f.write('\n')

def create_empty_tile(height, width):
    return [[0]*width for _ in range(height)]

#
## Read the tiles
#

tiles = []
tiles_areas = []
total_area = 0.0
with sys.stdin as f:
    lines = f.readlines()
    [number_of_tiles, total_width,] = map(int, lines[0].split(' '))
    frequencies = np.zeros(number_of_tiles)
    line_idx = 1
    for idx in xrange(number_of_tiles):
        current_area = 0
        [n_rows, n_columns] = map(int, lines[line_idx].split(' '))
        line_idx += 1
        current_tile = create_empty_tile(n_rows, n_columns)
        for row in xrange(n_rows):
            for column in xrange(n_columns):
                if lines[line_idx][column] == symbol_yes:
                    current_tile[row][column] = 1
                    current_area += 1
                else:
                    current_tile[row][column] = 0
            line_idx += 1
        tiles += [current_tile]
        tiles_areas += [current_area]
    n_placements = int(lines[line_idx].split(' ')[0])
    line_idx += 1
    sequence = []
    for idx in xrange(n_placements):
        [pos_x, tile_idx] = map(int, lines[line_idx].split(' '))
        sequence += [(tile_idx, pos_x)]
        line_idx += 1
        frequencies[tile_idx] += tiles_areas[tile_idx]
        total_area += tiles_areas[tile_idx]
    # reverse the order of placaments
    sequence = list(reversed(sequence))

frequencies = frequencies / total_area


if flag_debug:
    for tile_ind in xrange(number_of_tiles):
        print "Tile " + str(tile_ind)
        display_tile(tiles[tile_ind])

#
## TWO-DIMENSIONAL LATTICE GEOMETRY
#

def there_is_a_square(shiftx, shifty, tile, square_pos):
    tile_height = len(tile)
    tile_width = len(tile[0])
    if square_pos[0] + shiftx < 0:
        return False
    if square_pos[1] + shifty < 0:
        return False
    if square_pos[0] + shiftx >= tile_height:
        return False
    if square_pos[1] + shifty >= tile_width:
        return False
    if tile[square_pos[0]+shiftx][square_pos[1]+shifty] == 0:
        return False
    return True

## def compute_contour(tile, step = 1, posx = 0, posy = 0):
##     """
##     I assume that the tile is convex and there are no empty rows and columns.
##     Otherwise the user will receive an assertion error. Ok, in fact I don't check for
##     convexity. But I don't want to say this officially.
##     """
##     contour = []
##     tile_height = len(tile)
##     tile_width = len(tile[0])
##
##     # Let's find the starting point
##     starting_point = None
##     for i in xrange(tile_height):
##         if tile[i][0] == 1:
##             starting_point = (i, 0)
##     assert starting_point != None, "Some of the tiles doesn't match goodness assumption"
##     contour += [(\
##                 (starting_point[1] + posx)*step,\
##                 (starting_point[0] + posy)*step)\
##                ]
##
##     # Turn around until we are not in the starting point again
##     flag_finished = False
##     square_pos = starting_point
##     point_pos = starting_point
##     #
##     ## ORIENTATION. There are four possible orientation attached to the current square.
##     ## We encode them by (shiftx, shifty)
##     ## Initial orientation is always (1, 0)
##     ## We go counter-clockwise, i.e. in positive direction
##     #
##     orient = (0, -1)
##     while not flag_finished:
##         # Look forward to next orientation
##         # The next one is rotation of the previous direction by 90 degrees
##         # This corresponds to multiplication by matrix [[0,1],[-1,0]]
##         # simply said, (x, y) -> (-y, x)
##         inc_orient = (-orient[1], orient[0])
##         # Try to go in the direction corresponding to orientation
##         if there_is_a_square(orient[0], orient[1], tile, square_pos):
##             # Don't change the orientation
##             # Move point accordingly
##             point_pos = (point_pos[0] + orient[0], point_pos[1] + orient[1])
##             # Move square accordingly
##             square_pos = (square_pos[0] + orient[0], square_pos[1] + orient[1])
##         elif there_is_a_square(inc_orient[0], inc_orient[1], tile, square_pos):
##             # Try to go to the next direction, but don't change the main direction
##             point_pos = (point_pos[0] + inc_orient[0], point_pos[1] + inc_orient[1])
##             # Move square accordingly
##             square_pos = (square_pos[0] + inc_orient[0], square_pos[1] + inc_orient[1])
##         else:
##             # Everything else fails, we need to update the orientation and move the point
##             orient = (inc_orient[0], inc_orient[1])
##             point_pos = (point_pos[0] + orient[0], point_pos[1] + orient[1])
##
##         # Finally add a new point to the sequence
##         contour += [(\
##                      (point_pos[1] + posx)*step,\
##                      (point_pos[0] + posy)*step)\
##                    ]
##         if point_pos == starting_point:
##             flag_finished = True
##     contour += [(\
##                 (starting_point[1] + posx)*step,\
##                 (starting_point[0] + posy)*step)\
##                ]
##     return contour

def compute_contour(tile, step = 1, posx = 0, posy = 0):
    """
    I assume that the tile is convex and there are no empty rows and columns
    """
    posy +=1
    contour = []
    tile_height = len(tile)
    tile_width = len(tile[0])

    # Let's find the starting point
    starting_point = None
    for i in xrange(tile_height):
        if tile[i][0] == 1:
            starting_point = (i, 0)
    assert starting_point != None, "Some tile doesn't match goodness assumption"
    contour += [(\
                (starting_point[1] + posx)*step,\
                (starting_point[0] + posy)*step)\
               ]

    # Turn around until we are not in the starting point again
    flag_finished = False
    square_pos = starting_point
    point_pos = starting_point
    #
    ## ORIENTATION. There are four possible orientation attached to the current square.
    ## We encode them by (shiftx, shifty)
    ## Initial orientation is always (0, -1)
    ## We go counter-clockwise, i.e. in positive direction
    #
    orient = (1, 0)
    while not flag_finished:
        # Look forward to next orientation
        # This corresponds to multiplication by matrix [[0,1],[-1,0]]
        # i.e. (x, y) -> (-y, x)
        # inc_orient = (-orient[1], orient[0])
        # prev_orient = (orient[1], -orient[0])
        diag = (orient[0] + orient[1], -orient[0] + orient[1])
        # Try to go in the direction corresponding to orientation
        if there_is_a_square(diag[0], diag[1], tile, square_pos):
            # Move along diagonal
            square_pos = (square_pos[0] + diag[0], square_pos[1] + diag[1])
            orient     = (orient[1], -orient[0])
            point_pos = (point_pos[0] + orient[0], point_pos[1] + orient[1])
        elif there_is_a_square(orient[0], orient[1], tile, square_pos):
            # Try to go to the next direction, but don't change the main direction
            point_pos = (point_pos[0] + orient[0], point_pos[1] + orient[1])
            # Move square accordingly
            square_pos = (square_pos[0] + orient[0], square_pos[1] + orient[1])
        else:
            # Everything else fails, we need to update the orientation and move the point
            orient = (-orient[1], orient[0])
            point_pos = (point_pos[0] + orient[0], point_pos[1] + orient[1])

        # Finally add a new point to the sequence
        """
        assert\
            (point_pos[0] >= 0) and\
            (point_pos[1] >= 0) and\
            (point_pos[0] <= tile_height) and\
            (point_pos[1] <= tile_width),\
            "\nAlgorithm computing the contour fell out of desired region which is totally unexpected.\nPlease write an email to vit.north@gmail.com.\n Point: " + \
            str(point_pos[0]) + ' ' + str(point_pos[1])
        """
        contour += [(\
                     (point_pos[1] + posx)*step,\
                     (point_pos[0] + posy)*step)\
                   ]
        if point_pos == starting_point:
            flag_finished = True
    """
    contour += [(\
                (starting_point[1] + posx)*step,\
                (starting_point[0] + posy)*step)\
               ]
    """
    return contour

if flag_debug:
    display_tile(tiles[-1])
    print len(compute_contour(tiles[-1]))
    print compute_contour(tiles[-1])

#
## The contour is composed. Now we can use contour primitives
## from 'matplotlib patches' library to depict the border and
## fill the interior.
#

def add_tile(tile, ax, posx, posy, step=0.2, color=None):
    tile_height = len(tile)
    tile_width  = len(tile[0])
    contour     = compute_contour(tile, step, posx, posy)
    codes       = [Path.MOVETO] + [Path.LINETO]*(len(contour)-1)
    path        = Path(contour, codes)
    patch       = patches.PathPatch(path, facecolor=color, lw=lw)
    ax.add_patch(patch)

def compute_lower_height_profile(tile):
    width = len(tile[0])
    height = len(tile)
    result = [0]*width
    for col in xrange(width):
        for row in range(height - 1,-1,-1): # decreasing 'for'
            if tile[row][col] == 0:
                result[col] += 1
            else:
                break
    return result

def compute_higher_height_profile(tile):
    width = len(tile[0])
    height = len(tile)
    result = [0]*width
    for col in xrange(width):
        for row in range(0,height):
            if tile[row][col] == 0:
                result[col] += 1
            else:
                break
        result[col] = height - result[col]
    return result

lower_profiles  = []
higher_profiles = []

for tile_idx in xrange(number_of_tiles):
    lower_profiles  += [compute_lower_height_profile(tiles[tile_idx]) ]
    higher_profiles += [compute_higher_height_profile(tiles[tile_idx])]

def if_fits_give_profile(small_tile, large_tile_profile, pos):
    """
    In standart format, like
        'small_tile == [[0,0], [0,1]]'
    small tile corresponds to
    ..
    .*
    'large_tile' is represented in border format.

    For example, assume that total_height = 3.
    Then
        'large_tile == [0,1,2,0,1]'
    corresponds to
    .....
    ..*..
    .**.*
    'pos' is the index of horizontal position from 0 to W-1.

    [*] returns None if the tile doesn't fit
    [*] returns new profile if the tile fits
    """
    small_height = len(small_tile)
    small_width = len(small_tile[0])
    large_width = len(large_tile_profile)

    if pos + small_width > large_width:
        return None

    new_profile = list(large_tile_profile)

    lower_tile_profile = compute_lower_height_profile(small_tile)
    higher_tile_profile = compute_higher_height_profile(small_tile)

    for idx in range(small_width):
        if lower_tile_profile[0] - lower_tile_profile[idx]\
            != large_tile_profile[pos] - large_tile_profile[pos + idx]:
                return None

    for idx in range(small_width):
        new_profile[idx + pos] += higher_tile_profile[idx] - lower_tile_profile[idx]

    return new_profile

def get_tile_positions_from_sequence(tiles, sequence, width):
    results = [] # each element is a triple (tile_idx, x, y)
    profile = [0] * width

    # sequence is a list of tuples (n_tile, position)
    for new_tile in sequence:
        tile_idx = new_tile[0]
        pos_x    = new_tile[1]
        pos_y    = profile[ pos_x ] - lower_profiles[ tile_idx ][0]
        results += [ (tile_idx, pos_x, pos_y) ]
        profile  = if_fits_give_profile(tiles[tile_idx], profile, pos_x)
        assert profile != None, \
        "Sorry, your tiling is not beautiful enough to display,\
        maybe it has some holes, or maybe some tiles don't fit into the desired width."
    height = max(profile)
    for column in profile:
        if column < height:
            sys.stderr.write("Warning: final tiling is not rectangular!\n")
            break
    return (results, height)

#
## Create the picture and save it into 'filename'
#
(coordinates, height) = get_tile_positions_from_sequence(tiles, sequence, total_width)

# Revert all the tiles for display purposes
for tile_ind in xrange(number_of_tiles):
    tiles[tile_ind] = list(reversed(tiles[tile_ind]))

fig1 = plt.figure()
ax1 = fig1.add_subplot(111, aspect='equal')
ax1.set_xlim(0, step * total_width)
ax1.set_ylim(0, step * height)
ax1.set_xticks([])
ax1.set_yticks([])
#cmap = colormap.get_cmap(colorscheme)
cmap = plt.get_cmap(colorscheme)
#cmap = rand_cmap(100)

brightness = 1.0

tile_colors = np.linspace(.1,.9, number_of_tiles)

tile_colors = [
    (brightness * x,
     brightness * y,
     brightness * z,
     t)
    for (x,y,z,t) in map(cmap, tile_colors)
]

for put_tile in coordinates:
    (tile_idx, pos_x, pos_y) = put_tile
    add_tile(tiles[tile_idx],
             ax1,
             pos_x,
             pos_y,
             step=step,
             color=tile_colors[tile_idx])

np.set_printoptions(precision=3)

# uncomment to zero the array
# array_np = np.asarray(frequencies)
# low_values_indices = array_np < 2.0  # Where values are low
# array_np[low_values_indices] = 0  # All low values set to 0
fig1.savefig(filename, bbox_inches='tight')

#frequencies = np.load("statistics.npy")
# np.save("statistics",frequencies)
#print frequencies

#print np.count_nonzero(frequencies), "nonzero elements"
# fig1.savefig(filename, format='svg', dpi=1200, bbox_inches='tight')
