#! /usr/bin/env python
'''
    simple script to count the number of lines in a python file, done for the 
    Gauteng Python User Group

    author: Mandla Mbuli <mail@mandla.me>
    date: 7 Apr 2015
'''
from __future__ import print_function
import os
import time


def main():
    '''The main function, go C++'''
    time.sleep(10)
    for root, folders, filenames in os.walk('.'):
        for filename in filenames:
            if filename.endswith('.py'):
                with open(os.path.join(root, filename)) as f:
                    print(len(f.readlines()), end='\tin ')
                    print(os.path.join(root, filename))

if __name__ == '__main__':
    main()
