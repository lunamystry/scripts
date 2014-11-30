#! /usr/bin/env python
"""
author: Leonard Mbuli <mail@mandla.me>

creation date: 17 July 2014
update date: 5 November 2014

"""
from utils import *
import re
import string
import logging


class Word():
    def __init__(self, text, direction, grid):
        self.text = re.sub("["+string.punctuation+"\d]", "", text.lower(), 0, 0)
        self.direction = direction
        self.grid = grid

    @property
    def start(self):
        return self._start

    @start.setter
    def start(self, value):
        self._start = value
        self.points = self._calculate_points()

    def _calculate_points(self):
        row_incr, col_incr = self._increments()
        points = []
        row = self.start.row
        col = self.start.col
        points.append(Point(row, col))
        for letter in self.text[1:]:
            row += row_incr
            col += col_incr
            if (row < 0 or col < 0 or row > self.grid.rows or col > self.grid.cols):
                logging.debug("row:{0} col:{1}".format(row, col))
                raise IndexError("'" +self.text + "' is not completely inside grid")
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

    def __str__(self):
        return self.text

    def __repr__(self):
        return self.text

    def __len__(self):
        return len(self.text)
