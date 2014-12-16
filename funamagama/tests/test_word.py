#! /usr/bin/env python
"""
author: Leonard Mbuli <mail@mandla.me>

creation date: 16 December 2014

"""
import pytest

from generate.word import Word


def test_words_cant_be_less_than_2():
    '''
        If words are less than 2 characters you have letters
    '''
    with pytest.raises(ValueError):
        Word('a', None, None)
