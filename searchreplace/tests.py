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
import shutil

Dir = namedtuple('Dir', 'name filenames')

IGNORE = ('.ignore')
DIRS = (Dir('top/sub1/subsub1', ['file1.1.txt', 'file1.2.txt']),
        Dir('top/sub1/subsub2', ['file2.txt']),
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
        filenames = find_filenames('top', None, True, None)
        expected = [os.path.join(directory.name, fname)
                    for directory in DIRS
                    for fname in directory.filenames]
        self.assertEqual(sorted(expected), sorted(filenames))


    def tearDown(self):
        '''Remove the directories used in testing'''
        for folder in DIRS:
            shutil.rmtree(folder.name)


if __name__ == '__main__':
    unittest.main()
