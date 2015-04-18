'''

    test_searchreplace
    tests for the searchreplace.py script

    author: Mandla Mbuli <lm.mbuli@gmail.com>

    created: April 2015

    python 3
'''

from searchreplace import *
import os
import unittest

DIRS = ['test_recursively/directory/thing', 'test_directory/directory2']

class SearchReplace(unittest.TestCase):
    '''testing the searchreplace script'''

    def setUp(self):
        '''Create the directories for testing'''
        for folder in DIRS:
            if not os.path.isdir(folder):
                os.makedirs(folder)

    def test_replace_recursively_in_directories(self):
        '''Should be able to search and replace in directories'''
        assert 1 == 1

    def tearDown(self):
        '''Remove the directories used in testing'''
        for folder in DIRS:
            os.removedirs(folder)


if __name__ == '__main__':
    unittest.main()
