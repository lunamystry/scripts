#! /usr/bin/env python
"""
author: Leonard Mbuli <mail@mandla.me>

creation date: 17 July 2014
update date: 2 November 2014

"""
import logging
from grid import *

logging.basicConfig(level=logging.DEBUG,
                    format='%(message)s')

if __name__ == '__main__':
    grid = Grid(20, 20)
    grid.place(*['leonard',
        'mandla',
        'phoebie',
        'book',
        'classic',
        'homecoming',
        'breast',
        'sibling',
        'war',
        'sand',
        'diary',
        'door',
        'cocky',
        'perfect',
        'nothing',
        'coven'])
    print(grid)
