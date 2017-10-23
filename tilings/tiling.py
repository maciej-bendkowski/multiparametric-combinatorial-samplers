### Collect tiling.py
from collections import deque

import time

# Flag defined for testing the program
flag_debug = False
flag_progress = True

symbol_yes = '+'
symbol_no  = '.'

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
    #print ''

def create_empty_tile(height, width):
    return [[0]*width for _ in range(height)]

#
## Read the tiles
#

tiles = []
tiles_areas = []
max_height = -1
with open('input.tile', 'r') as f:
    lines = f.readlines()
    [number_of_tiles, total_width] = map(int, lines[0].split())
    line_idx = 1
    for idx in xrange(number_of_tiles):
        current_area = 0
        [n_rows, n_columns] = map(int, lines[line_idx].split())
        max_height = max(max_height, n_rows)
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
    # max_height += 1

if flag_debug:
    print tiles
    map(display_tile, tiles)
    print "Max Height =", max_height

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

if flag_debug:
    display_tile(tiles[-1])

if flag_debug:
    print compute_lower_height_profile(tiles[-1])

if flag_debug:
    print compute_higher_height_profile(tiles[-1])

# another technical definition

def display_profile(profile, file = None):
    result = create_empty_tile(max_height, total_width)
    width = len(profile)
    for idx in xrange(len(profile)):
        for h in xrange(profile[idx]):
            result[max_height - h - 1][idx] = 1
    display_tile(result, file = file)

def prune_profile(profile):
    min_index = min(profile)
    for idx in xrange(len(profile)):
        profile[idx] -= min_index

def if_fits_give_profile(small_tile, large_tile_profile, pos):
    """
    Small tile in standard
        'small_tile == [[0,0], [0,1]]'
    format corresponds to
    ..
    .*
    Large tile on border format for simplicity. Assume that total_height = 3.
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

    prune_profile(new_profile)

    if max(new_profile) > max_height:
        return None

    return new_profile

if flag_debug:
    display_tile(tiles[-1])

    print "\nTrying to fit into position 1:"
    display_profile(if_fits_give_profile(tiles[-1], test_profile, 1))
    print "Resulting profile:"
    print if_fits_give_profile(tiles[-1], test_profile, 1)

base = max_height + 1

def convert_array_to_base_representation(array):
    base_repr = 0
    array_length = len(array)
    for idx in xrange(array_length):
        base_repr += array[idx] * base**idx
    return base_repr

def convert_base_representation_to_array(base_repr):
    result = [0] * total_width
    idx = 0
    while base_repr > 0:
        digit = base_repr % base
        base_repr /= base
        result[idx] = digit
        idx += 1
    return result

if flag_debug:
    print convert_array_to_base_representation([0,1,1])

if flag_debug:
    test_profile = [1,2,1,2]
    print test_profile
    print convert_base_representation_to_array(\
          convert_array_to_base_representation(test_profile))
    test_profile = if_fits_give_profile(tiles[-1], test_profile, 1)
    print test_profile
    print convert_base_representation_to_array(\
          convert_array_to_base_representation(test_profile))

#
## Construct the (transposed) array of dependencies

index_size = base**total_width

import sys

"""
Each element of dependency array is a pair
- the tile index that brings dependency
- the position to insert the tile
If profiles are not dependent, then None.
"""
dependency_array = [[None] * index_size for _ in range(index_size)]

"""
not_display_indices = []

for profile_repr in xrange(0, index_size):
    profile = convert_base_representation_to_array(profile_repr)
    if min(profile) > 0:
        not_display_indices += [profile_repr]
    """

if flag_progress:
    print 'Eliminating unused transitions...'

for profile_repr in xrange(0, index_size):
    profile = convert_base_representation_to_array(profile_repr)
    for tile_idx in xrange(number_of_tiles):
        for pos in xrange(total_width):
            to_profile = if_fits_give_profile(tiles[tile_idx], profile, pos)
            if to_profile == None:
                continue
            to_profile_repr = convert_array_to_base_representation(to_profile)
            # assert dependency_array[to_profile_repr][profile_repr] == None
            dependency_array[to_profile_repr][profile_repr] = (tile_idx, pos)

    # display_profile(profile)
    # print "==========="

list_include_rows = [False] * index_size
bfs_queue = deque([0])
while len(bfs_queue) > 0:
    current_idx = bfs_queue.popleft()
    list_include_rows[current_idx] = True
    for next_idx in range(index_size):
        if (dependency_array[next_idx][current_idx] != None)\
        and (not list_include_rows[next_idx]):
            bfs_queue.append(next_idx)
            list_include_rows[next_idx] = True

if flag_debug:
    print list_include_rows

if flag_progress:
    print 'done!'

global_counter = 0

"""
not_display_indices = []
with open('output.txt', 'w+') as f:
    f.write('{- Compiled tiling grammar from '+ str('input.tile') + '\n')
    for tile_ind in xrange(number_of_tiles):
        f.write("Tile " + str(tile_ind) + '\n')
        display_tile(tiles[tile_ind], f)
    f.write('[*] Excessive search window:\n')
    display_tile(create_empty_tile(max_height, total_width), f)
    f.write('[*] Number of variables = ' + str(number_of_tiles) + '\n')
    f.write('[*] Number of functions = ' + str(index_size - len(not_display_indices)))
    f.write(' -}\n')
    for profile_ind in xrange(index_size):
        if profile_ind in not_display_indices:
            continue
        f.write('Tiling')
        f.write(str(profile_ind))
        f.write(' = ')
        if profile_ind == 0:
            f.write('Eps (0) | ')
        first_object_in_grammar = True
        for to_profile_ind in xrange(index_size):
            if to_profile_ind in not_display_indices:
                continue
            if dependency_array[profile_ind][to_profile_ind] == None:
                continue
            if first_object_in_grammar:
                first_object_in_grammar = False
            else:
                f.write(' | ')
            (tile_idx, pos) = dependency_array[profile_ind][to_profile_ind]
            f.write('Tile' + str(tile_idx) + ' ')
            f.write('Tiling' + str(to_profile_ind) + ' ')
            f.write('Pos' + str(pos) + ' (0)')
        f.write('.\n')
    f.close()
"""

flag_debug = False

tiling_n_adjacent_profiles = [0] * index_size

with open('output.txt', 'w+') as f:
    if flag_debug:
        f.write('{- Compiled tiling grammar from '+ str('input.tile') + '\n')
        for tile_ind in xrange(number_of_tiles):
            f.write("Tile " + str(tile_ind) + '\n')
            display_tile(tiles[tile_ind], f)
        f.write('[*] Excessive search window:\n')
        display_tile(create_empty_tile(max_height, total_width), f)
        f.write('[*] Number of variables = ' + str(number_of_tiles) + '\n')
        f.write('[*] Number of functions = ' + str(sum(list_include_rows)) + ' -}\n')
    # Write bb generator parameters
    f.write('@module     Sampler\n')
    f.write('@withIO     y\n')
    f.write('@withShow   n\n')
    for profile_ind in xrange(index_size):
        if not list_include_rows[profile_ind]:
            continue
        f.write('Tiling')
        f.write(str(profile_ind))
        f.write(' = ')
        if profile_ind == 0:
            f.write('Eps (1) | ')
        first_object_in_grammar = True
        for to_profile_ind in xrange(index_size):
            if not list_include_rows[to_profile_ind]:
                continue
            if dependency_array[profile_ind][to_profile_ind] == None:
                continue
            if first_object_in_grammar:
                first_object_in_grammar = False
            else:
                f.write('\n | ')
            (tile_idx, pos) = dependency_array[profile_ind][to_profile_ind]
            f.write('Pos' + str(pos) + '_x' + str(global_counter) + 'x ')
            global_counter += 1
            f.write('Tile' + str(tile_idx) + ' ')
            f.write('Tiling' + str(to_profile_ind) + ' (0)')
            tiling_n_adjacent_profiles[profile_ind] += 1
        f.write('.\n')
    for tile_idx in xrange(number_of_tiles):
        f.write('Tile' + str(tile_idx) + ' = ' + 'T' + str(tile_idx))
        f.write(' (' + str(tiles_areas[tile_idx]) + ')')
        f.write(' [' + str(1.0 / number_of_tiles) + ']')
        f.write('.\n')

    if flag_debug:
        f.write('{- \n')
        for tiling_idx in xrange(index_size):
            if not list_include_rows[tiling_idx]:
                continue
            f.write('Tiling' + str(tiling_idx) + '\n')
            display_profile(convert_base_representation_to_array(tiling_idx), f)
        f.write(' -}\n')

    f.close()

print('Grammar constructed.')

import sys

sys.stderr.write("Number of functions: " + str(sum(list_include_rows)) + "\n")

"""
print "Constructing Paganini specification..."

with open('paganini_start.pg', 'w+') as f:
    f.write(str(number_of_tiles) + " " + str(sum(list_include_rows)) + "\n")

    for profile_ind in xrange(index_size):
        if not list_include_rows[profile_ind]:
            continue
        f.write(str(tiling_n_adjacent_profiles[profile_ind]) + "\n")
        for to_profile in xrange(index_size):
            if dependency_array[profile_ind][to_profile] == None:
                continue
            for i in range(to_profile):
                f.write("0 ")
            f.write("1 ")
            for i in range(to_profile + 2, )
            f.write("\n")
    f.close()
"""
