#! /usr/bin/env python
"""
author: Leonard Mbuli <mail@mandla.me>

creation date: 17 July 2014
update date: 14 December 2014

"""
from __future__ import print_function
from __future__ import unicode_literals

import random

from copy import copy
from collections import namedtuple

from .point import Point
from .word import Word


class Grid():
    def __init__(self, rows, cols):# {{{
        '''
            create a grid with the given dimensions, and empty list of words
        '''
        self.rows = rows
        self.cols = cols
        self.words = []
        self._grid = self.init_grid(rows, cols)# }}}

    def init_grid(self, y, x):# {{{
        '''
            input: size of the grid
            returns: a list of strings representing the row of the grid
        '''
        bg = []
        alphabet = "abcdefghijklmnopqrstuvwxyz"
        for r in range(1, y + 1):
            row = []
            for c in range(1, x + 1):
                aindex = int(random.uniform(0, 26))
                row.append(alphabet[aindex])
            bg.append(row)
        return bg# }}}

    def place(self, *args, **kwargs):# {{{
        '''
            Tries to place the given string(s) on the grid in random directions
            and starting positions

            takes a list of strings to place
            and optionally, list of directions
        '''
        if 'directions' in kwargs:
            directions = kwargs['directions']
        else:
            directions = ['EAST', 'WEST', 'SOUTH', 'NORTH', 'NORTHEAST',
                    'NORTHWEST', 'SOUTHEAST', 'SOUTHWEST']
        not_placed = []

        for arg in args:
            dindex = int(random.uniform(0, len(directions)))
            word = Word(arg, directions[dindex], self)
            possible_starts = self.possible_starts(word)
            if possible_starts:
                sindex = int(random.uniform(0, len(possible_starts)))
                word.start = possible_starts[sindex]
                for i, point in enumerate(word.points):
                    self._grid[point.row][point.col] = word.text[i]
                    if word not in self.words:
                        self.words.append(word)
            else:
                not_placed.append(arg)

        return not_placed# }}}

    def possible_starts(self, word):# {{{
        '''
            find all the possible starts for the given word on the grid where
            it will not have a collision but may have an intersection.
        '''
        bounds = self.boundaries(word)
        within_bounds = []
        points = []
        collision_points = []
        for row in range(bounds.min_y, bounds.max_y):
            for col in range(bounds.min_x, bounds.max_x):
                point = Point(row, col)
                within_bounds.append(point)
        # Get collision points,
        for point in within_bounds:
            word.start = point
            for grid_word in self.words:
                if self.collision(word, grid_word):
                    collision_points.append(point)
        # filter points collision points
        for point in within_bounds:
            if point not in collision_points:
                points.append(point)
        return points# }}}

    def boundaries(self, word):# {{{
        '''
            Find all the positions which make sure the word is within the grid
        '''
        Bound = namedtuple('Bound', 'direction min_y max_y min_x max_x')
        bounds = {
                'EAST':
                Bound('EAST', 0,
                    len(self._grid), 0, len(self._grid[0]) - (len(word) - 1)),
                'WEST':
                Bound("WEST",
                    0, len(self._grid), len(word) - 1, len(self._grid[0])),
                'SOUTH':
                Bound("SOUTH", 0,
                    len(self._grid) - (len(word) - 1), 0, len(self._grid[0])),
                'NORTH':
                Bound("NORTH",
                    len(word) - 1, len(self._grid), 0, len(self._grid[0])),
                "SOUTHEAST":
                Bound("SOUTHEAST",
                    0, len(self._grid) - (len(word) - 1), 0,
                    len(self._grid[0]) - (len(word) - 1)),
                "NORTHWEST":
                Bound("NORTHWEST",
                    len(word) - 1, len(self._grid), len(word) - 1,
                    len(self._grid[0])),
                "SOUTHWEST":
                Bound("SOUTHWEST",
                    0, len(self._grid) - (len(word) - 1), len(word) - 1,
                    len(self._grid[-1])),
                "NORTHEAST":
                Bound("NORTHEAST",
                    len(word) - 1, len(self._grid), 0,
                    len(self._grid[0]) - (len(word) - 1))
                }
        return bounds[word.direction]# }}}

    def collision(self, first, second):# {{{
        '''
            Check if two words collide and cannot intersect
        '''
        for i, p1 in enumerate(first.points):
            for j, p2 in enumerate(second.points):
                if p1 == p2 and first.text[i] != second.text[j]:
                    return True
        return False# }}}

    def __str__(self):# {{{
        '''
            join up the grid so it can be shown
        '''
        grid_str = ""
        for row in self._grid:
            grid_str += " ".join(row)
            grid_str += "\n"
        return grid_str[:-1]# }}}
