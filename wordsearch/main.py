#! /usr/bin/env python
"""
author: Leonard Mbuli <mail@mandla.me>

creation date: 17 July 2014
update date: 2 November 2014

"""
import logging
import random

from word import *
from utils import *
from grid import *

logging.basicConfig(level=logging.DEBUG,
                    format='%(message)s')

def randomly_place(words, grid):
    '''
        takes a list of words and places them randowly on the grid
    '''
    for text in words:
        Direction = namedtuple('Direction', 'dir min_y max_y min_x max_x')
        directions = [
                Direction("EAST",
                    0, len(grid), 0, len(grid[0]) - (len(text) - 1)),
                Direction("WEST",
                    0, len(grid), len(text) - 1, len(grid[0])),
                Direction("SOUTH",
                    0, len(grid) - (len(text) - 1), 0, len(grid[0])),
                Direction("NORTH",
                    len(text) - 1, len(grid), 0, len(grid[0])),
                Direction("SOUTHEAST",
                    0, len(grid) - (len(text) - 1), 0, len(grid[0]) - (len(text) - 1)),
                Direction("NORTHWEST",
                    len(text) - 1, len(grid), len(text) - 1, len(grid[0])),
                Direction("SOUTHWEST",
                    0, len(grid) - (len(text) - 1), len(text) - 1, len(grid[-1])),
                Direction("NORTHEAST",
                    len(text) - 1, len(grid), 0, len(grid[0]) - (len(text) - 1))]
        dir_index = int(random.uniform(0, len(directions)))
        try:
            y = int(random.uniform(directions[dir_index].min_y,
                directions[dir_index].max_y))
            x = int(random.uniform(directions[dir_index].min_x,
                directions[dir_index].max_x))
            word = Word(text, directions[dir_index].dir, Point(y, x), grid)
            place(word, grid)
        except IndexError as e :
            logging.debug("x:{0} y:{1} word: {2} dir: {3}".format(x, y, text,
                directions[dir_index].dir))
            raise e


if __name__ == '__main__':
    grid = Grid(10, 10)
    # randomly_place(["word", "igama", "leonard", "python"], grid)
    python = Word("python", "NORTHEAST", Point(5, 2), grid)
    leonard = Word("leonard", "NORTHWEST", Point(6, 7), grid)
    grid.place(python)
    grid.place(leonard)
    print(grid)
    print(grid._check_collision(python, leonard))
    print(grid._boundaries(leonard))
    print(grid.words)
