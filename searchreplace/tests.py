'''

    test_searchreplace
    tests for the searchreplace.py script

    author: Mandla Mbuli <lm.mbuli@gmail.com>

    created: April 2015

    python 2 or 3
'''

from searchreplace import *
from collections import namedtuple
import os
import re
import unittest
import shutil

Dir = namedtuple('Dir', 'name filenames')

DIRS = (Dir('top', ['file.ctop', 'ignore.top']),
        Dir('top/text', ['file1.txt', 'file2.txt']),
        Dir('top/sub1/python', ['file1.py', 'file2.py', 'ignore.py']),
        Dir('top/sub2/.ignore/ignore1', ['ignored.ignore']))

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

    def test_find_filenames_non_recursive(self):
        filenames = find_filenames('top', is_recursive=False)
        expected = ['top/file.ctop', 'top/ignore.top']
        self.assertEqual(sorted(expected), sorted(filenames))

    def test_find_filenames_non_recursive_with_ignored(self):
        ignore_str=r'ignore'
        filenames = find_filenames('top',
                                    is_recursive=False,
                                    ignore_str=ignore_str)
        expected = ['top/file.ctop']
        self.assertEqual(sorted(expected), sorted(filenames))

    def test_find_filenames_recursively_in_directories(self):
        filenames = find_filenames('top', extension='')
        expected = [os.path.join(directory.name, fname)
                    for directory in DIRS
                    for fname in directory.filenames]
        self.assertEqual(sorted(expected), sorted(filenames))

    def test_find_filenames_recursively_in_directories_without_ignored(self):
        ignore_str=r'ignore'
        filenames = find_filenames('top', ignore_str=ignore_str)
        expected = [os.path.join(directory.name, fname)
                    for directory in DIRS
                    for fname in directory.filenames
                    if not re.search(ignore_str, fname)]
        self.assertEqual(sorted(expected), sorted(filenames))

    def test_find_only_files_with_extension(self):
        filenames = find_filenames('top', extension='.py')
        expected = ['top/sub1/python/file1.py',
                    'top/sub1/python/file2.py',
                    'top/sub1/python/ignore.py']
        self.assertEqual(sorted(expected), sorted(filenames))

    def test_find_only_files_with_extension_non_recursive(self):
        filenames = find_filenames('top', extension='.top', is_recursive=False)
        expected = ['top/ignore.top']
        self.assertEqual(sorted(expected), sorted(filenames))

    def tearDown(self):
        '''Remove the directories used in testing'''
        for folder in DIRS:
            if os.path.isdir(folder.name):
                shutil.rmtree(folder.name)


if __name__ == '__main__':
    unittest.main()
