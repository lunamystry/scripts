#! /usr/bin/env python
"""
author: Leonard Mbuli <mail@mandla.me>

creation date: 13 December 2014

"""
from puzzle import *

def test_init_place():
    g = grid.Grid(20, 20)
    assert(len(g) == 20)

