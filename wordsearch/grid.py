#! /usr/bin/env python
"""
author: Leonard Mbuli <mail@mandla.me>

creation date: 17 July 2014
update date: 2 November 2014

"""
import random
from copy import copy
from utils import *
from word import *
from collections import namedtuple


class Grid():
    def __init__(self, rows, cols):
        self.rows = rows
        self.cols = cols
        self.words = []
        self.grid = self._init_grid(rows, cols)

    def _init_grid(self, y, x):
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
        return bg

    def place(self, *args):
        '''
            tries to place the word on position x and y
        '''
        directions = ['EAST', 'WEST', 'SOUTH', 'NORTH', 'NORTHEAST',
                'NORTHWEST', 'SOUTHEAST', 'SOUTHWEST']

        for arg in args:
            dindex = int(random.uniform(0, len(directions)))
            word = Word(arg, directions[dindex], self)
            possible_starts = self._possible_starts(word)
            if possible_starts:
                sindex = int(random.uniform(0, len(possible_starts)))
                word.start = possible_starts[sindex]
                for i, point in enumerate(word.points):
                    self.grid[point.row][point.col] = word.text[i]
                    if word not in self.words:
                        self.words.append(word)

    def _possible_starts(self, word):
        bounds = self._boundaries(word)
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
                if self._check_collision(word, grid_word):
                    collision_points.append(point)
        # filter points collision points
        for point in within_bounds:
            if point not in collision_points:
                points.append(point)
        return points

    def _boundaries(self, word):
        Bound = namedtuple('Bound', 'direction min_y max_y min_x max_x')
        if word.direction == "EAST":
            return Bound("EAST",
                    0, len(self.grid), 0, len(self.grid[0]) - (len(word) - 1))
        elif word.direction == "WEST":
            return Bound("WEST",
                    0, len(self.grid), len(word) - 1, len(self.grid[0]))
        elif word.direction == "SOUTH":
            return Bound("SOUTH",
                    0, len(self.grid) - (len(word) - 1), 0, len(self.grid[0]))
        elif word.direction == "NORTH":
            return Bound("NORTH",
                    len(word) - 1, len(self.grid), 0, len(self.grid[0]))
        elif word.direction == "SOUTHEAST":
            return Bound("SOUTHEAST",
                    0, len(self.grid) - (len(word) - 1), 0,
                    len(self.grid[0]) - (len(word) - 1))
        elif word.direction == "NORTHWEST":
            return Bound("NORTHWEST",
                    len(word) - 1, len(self.grid), len(word) - 1,
                    len(self.grid[0]))
        elif word.direction == "SOUTHWEST":
            return Bound("SOUTHWEST",
                    0, len(self.grid) - (len(word) - 1), len(word) - 1,
                    len(self.grid[-1]))
        elif word.direction == "NORTHEAST":
            return Bound("NORTHEAST",
                    len(word) - 1, len(self.grid), 0,
                    len(self.grid[0]) - (len(word) - 1))

    def _check_collision(self, first, second):
        '''
            Check if two words collide and cannot intersect
        '''
        for i, p1 in enumerate(first.points):
            for j, p2 in enumerate(second.points):
                if p1 == p2 and first.text[i] != second.text[j]:
                    return True
        return False


    def __str__(self):
        '''
            join up the grid so it can be shown
        '''
        grid_str = ""
        for row in self.grid:
            grid_str += " ".join(row)
            grid_str += "\n"
        return grid_str[:-1]
