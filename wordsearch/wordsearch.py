#! /usr/bin/env python
"""
author: Leonard Mbuli <mail@mandla.me>

creation date: 17 July 2014
update date: 10 September 2014

"""
import logging
import random
from collections import namedtuple

logging.basicConfig(level=logging.DEBUG,
                    format='%(message)s')

class Word():
    def __init__(self, text, direction, start, grid):
        self.text = text
        self.direction = direction
        self.start = start
        self.points = self._calculate_points();

    def _calculate_points(self):
        row_incr, col_incr = self._increments()
        points = []
        row = self.start.row
        col = self.start.col
        points.append((row, col))
        for letter in self.text:
            row += row_incr
            col += col_incr
            if (row < 0 or col < 0 or row > len(grid) or col > len(grid[0])):
                raise IndexError(self.text + " is not completely inside grid")
            points.append((row, col))
        return points

    def _increments(self):
        col_incr = 0
        row_incr = 0
        if self.direction == 'EAST':
            col_incr = 1
        elif self.direction == 'WEST':
            col_incr = -1
        elif self.direction == 'SOUTH':
            row_incr = 1
        elif self.direction == 'NORTH':
            row_incr = -1
        elif self.direction == 'SOUTHEAST':
            row_incr = 1
            col_incr = 1
        elif self.direction == 'SOUTHWEST':
            row_incr = 1
            col_incr = -1
        elif self.direction == 'NORTHEAST':
            row_incr = -1
            col_incr = 1
        elif self.direction == 'NORTHWEST':
            row_incr = -1
            col_incr = -1
        return row_incr, col_incr


def make_grid(x, y):
    '''
        input: size of the grid
        returns: a list of strings representing the row of the grid
    '''
    bg = []
    alphabet = "__________________________"
    for r in range(1, y + 1):
        row = []
        for c in range(1, x + 1):
            aindex = int(random.uniform(0, 26))
            row.append(alphabet[aindex])
        bg.append(row)
    return bg


def place(word, y, x, direction, grid):
    '''
        tries to place the word on position x and y
    '''
    # The word must be stripped of hyphens and numbers
    # x and y must be on the grid and the length must be less than grid
    # The word must be lowercase
    for i, letter in enumerate(word):
        if direction == 'EAST':
            if (x + len(word)) > len(grid[y]):
                raise IndexError("columns out of range")
                break
            grid[y][x + i] = letter
        if direction == 'WEST':
            if (x - (len(word) - 1)) < 0:
                raise IndexError("columns out of range")
                break
            grid[y][x - i] = letter
        if direction == 'SOUTH':
            if (y + len(word)) > len(grid[y]):
                raise IndexError("columns out of range")
                break
            grid[y + i][x] = letter
        if direction == 'NORTH':
            if (y - (len(word) - 1)) < 0:
                raise IndexError("columns out of range")
                break
            grid[y - i][x] = letter
        if direction == 'SOUTHEAST':
            if (x + len(word)) > len(grid[y]) or (y + len(word)) > len(grid[y]):
                raise IndexError("columns out of range")
                break
            grid[y + i][x + i] = letter
        if direction == 'NORTHWEST':
            if (x - (len(word) - 1)) < 0 or (y - (len(word) - 1)) < 0:
                raise IndexError("columns out of range")
                break
            grid[y - i][x - i] = letter
        if direction == 'SOUTHWEST':
            if (x - (len(word) - 1)) < 0 or (y + (len(word) - 1)) > len(grid[y]):
                raise IndexError("columns out of range")
                break
            grid[y + i][x - i] = letter
        if direction == 'NORTHEAST':
            if (x + len(word)) > len(grid[y]) or (y - (len(word) - 1)) < 0:
                raise IndexError("columns out of range")
                break
            grid[y - i][x + i] = letter


def randomly_place(words, grid):
    '''
        takes a list of words and places them randowly on the grid
    '''
    for word in words:
        Direction = namedtuple('Direction', 'dir min_y max_y min_x max_x')
        directions = [
                Direction("EAST",
                    0, len(grid), 0, len(grid[0]) - (len(word) - 1)),
                Direction("WEST",
                    0, len(grid), len(word) - 1, len(grid[0])),
                Direction("SOUTH",
                    0, len(grid) - (len(word) - 1), 0, len(grid[0])),
                Direction("NORTH",
                    len(word) - 1, len(grid), 0, len(grid[0])),
                Direction("SOUTHEAST",
                    0, len(grid) - (len(word) - 1), 0, len(grid[0]) - (len(word) - 1)),
                Direction("NORTHWEST",
                    len(word) - 1, len(grid), len(word) - 1, len(grid[0])),
                Direction("SOUTHWEST",
                    0, len(grid) - (len(word) - 1), len(word) - 1, len(grid[-1])),
                Direction("NORTHEAST",
                    len(word) - 1, len(grid), 0, len(grid[0]) - (len(word) - 1))]
        dir_index = int(random.uniform(0, len(directions)))
        try:
            y = int(random.uniform(directions[dir_index].min_y,
                directions[dir_index].max_y))
            x = int(random.uniform(directions[dir_index].min_x,
                directions[dir_index].max_x))
            place(word, y, x, directions[dir_index].dir, grid)
        except IndexError:
            logging.debug("x:{0} y:{1}".format(x, y))


if __name__ == '__main__':
    grid = make_grid(10, 10)
    # randomly_place(["word", "igama", "leonard", "python"], grid)
    # for row in grid:
    #     logging.info(" ".join(row))

    Point = namedtuple("Point", "row col")
    word = Word("WORD", "SOUTHEAST", Point(0, 0), grid)
    logging.info(word.points)
