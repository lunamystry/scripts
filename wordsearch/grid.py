#! /usr/bin/env python
"""
author: Leonard Mbuli <mail@mandla.me>

creation date: 17 July 2014
update date: 2 November 2014

"""
import random
from utils import *


class Grid():
    def __init__(self, rows, cols):
        self.rows = rows
        self.cols = cols
        self.words = []
        self.grid = self._init_grid(rows, cols)

    def place(self, word):
        '''
            tries to place the word on position x and y
        '''
        for i, point in enumerate(word.points):
            self.grid[point.row][point.col] = word.text[i]
        self.words.append(word)

    def _init_grid(self, y, x):
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

    def _check_intersection(self, first, second):
        '''
            Check if two words intersect
        '''
        for p1 in first.points:
            for p2 in second.points:
                if p1 == p2:
                    return True

    def __str__(self):
        '''
            join up the grid so it can be shown
        '''
        grid_str = ""
        for row in self.grid:
            grid_str += " ".join(row)
            grid_str += "\n"
        return grid_str[:-1]
