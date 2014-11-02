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

Point = namedtuple("Point", "row col")

class Word():
    def __init__(self, text, direction, start, grid):
        # The word must be stripped of hyphens and numbers
        # x and y must be on the grid and the length must be less than grid
        # The word must be lowercase
        self.text = text
        self.direction = direction
        self.start = start
        self.points = self._calculate_points();

    def _calculate_points(self):
        row_incr, col_incr = self._increments()
        points = []
        row = self.start.row
        col = self.start.col
        points.append(Point(row, col))
        for letter in self.text[1:]:
            row += row_incr
            col += col_incr
            if (row < 0 or col < 0 or row > len(grid) or col > len(grid[0])):
                logging.debug("row:{0} col:{1}".format(row, col))
                raise IndexError(self.text + " is not completely inside grid")
            points.append(Point(row, col))
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


def place(word, grid):
    '''
        tries to place the word on position x and y
    '''
    for i, point in enumerate(word.points):
        grid[point.row][point.col] = word.text[i]


def check_intersection(first, second):
    for p1 in first.points:
        for p2 in second.points:
            if p1 == p2:
                return True


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
    grid = make_grid(10, 10)
    # randomly_place(["word", "igama", "leonard", "python"], grid)
    python = Word("python", "NORTHEAST", Point(5, 2), grid)
    leonard = Word("leonard", "NORTHWEST", Point(6, 7), grid)
    place(python, grid)
    place(leonard, grid)
    print(check_intersection(python, leonard))
    for row in grid:
        logging.info(" ".join(row))
