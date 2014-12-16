#! /usr/bin/env python
"""
author: Leonard Mbuli <mail@mandla.me>

creation date: 13 December 2014

"""
from puzzle.grid import Grid

def test_place_returns_list_if_successful():
    '''
        Can I place a word on the grid
    '''
    g = Grid(5, 5)
    res = g.place('monty')
    assert(res == [])

def test_place_returns_list_of_words_not_placed():
    '''
        What if some words don't fit into the grid?
    '''
    g = Grid(5, 5)
    res = g.place('monty', 'python')
    assert(res == ['python'])
