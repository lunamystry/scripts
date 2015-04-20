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

IGNORE = ('.ignore')
DIRS = ('top/sub1/subsub1', 'top/sub1/subsub2', 'top/sub2/.ignore/ignore1')

class SearchReplace(unittest.TestCase):
    '''testing the searchreplace script'''

    def setUp(self):
        '''Create the directories for testing'''
        for folder in DIRS:
            if not os.path.isdir(folder):
                os.makedirs(folder)

    def test_find_filenames_recursively_in_directories(self):
        '''Should be able to search and replace in directories'''
        filenames = find_filenames('top', None, True, None)
        self.assertEquals(filenames, DIRS)

    def tearDown(self):
        '''Remove the directories used in testing'''
        for folder in DIRS:
            os.removedirs(folder)


if __name__ == '__main__':
    unittest.main()
