'''

    test_searchreplace
    tests for the searchreplace.py script

    author: Mandla Mbuli <lm.mbuli@gmail.com>

    created: April 2015

    python 3
'''

from searchreplace import *
from collections import namedtuple
import os
import unittest

Dir = namedtuple('Dir', 'name filenames')

IGNORE = ('.ignore')
DIRS = (Dir('top/sub1/subsub1', ['file.txt']),
        Dir('top/sub1/subsub2', ['file.txt']),
        Dir('top/sub2/.ignore/ignore1', ['ignored.txt']))

class SearchReplace(unittest.TestCase):
    '''testing the searchreplace script'''

    def setUp(self):
        '''Create the directories for testing'''
        for name, filenames in DIRS:
            if not os.path.isdir(name):
                os.makedirs(name)
            for filename in filenames:
                fname = os.path.join(name,filename)
                if not os.path.exists(fname):
                    open(fname, 'w').close()

    def test_find_filenames_recursively_in_directories(self):
        '''Should be able to search and replace in directories'''
        filenames = find_filenames('top', None, True, 'ignore')
        print(filenames)
    #    self.assertEquals(filenames, list(DIRS))

    # def tearDown(self):
    #     '''Remove the directories used in testing'''
    #     for folder in DIRS:
    #         os.removedirs(folder)


if __name__ == '__main__':
    unittest.main()
