#! /usr/bin/env python
'''
    searchreplace

    author: Leonard Mandla Mbuli <lm.mbuli@gmail.com>

    creation date: 22 September 2012
    updated: April 2015

    python 3
'''
import fileinput
import os
import sys
import glob
import argparse
import textwrap

IGNORE_LIST = ('.git','.svn', '.bzr', '_cabal')

def main():
    '''Does everything'''
    parser = argparse.ArgumentParser(
        formatter_class=argparse.RawDescriptionHelpFormatter,
        description=textwrap.dedent(__doc__)
        )
    parser.add_argument('searchterm',
                        help='The term to search for')
    parser.add_argument('replaceterm',
                        help='The term to replace the search term with')
    parser.add_argument('-d', '--directory', action='store', default='.',
                        help='The directory to search within, default is \
                                current directory')
    parser.add_argument('-e', '--extension', action='store', default='',
                        help='Search only for files matching the extension')
    parser.add_argument('-f', '--filename', action='store',
                        help='Search and replace only within specified file')
    parser.add_argument('-r', '--recursive', action='store_true',
                        help='When searching in a directory, where to decend \
                        into sub directories')
    parser.add_argument('-o', '--omit',
                        action='store',
                        help='dont search in directory')
    parser.add_argument('-c', '--confirm',
                        action='store_true',
                        help='ask before you replace')
    args = parser.parse_args()
    print('replace "{}" with "{}"'.format(args.replaceterm, args.searchterm))

    if args.filename is None:
        filenames = find_filenames(args.directory,
                                   args.extension,
                                   args.recursive)
        for filename in filenames:
            search_replace(args.searchterm, args.replaceterm, filename)
    else:
        search_replace(args.searchterm, args.replaceterm, args.filename)


def find_filenames(directory, extension=None, is_recursive=True, ignore_list=None):
    '''searches either the provided directory for filenames'''
    if extension and not extension.startswith('.'):
        extension = '.' + extension
    if ignore_list is None:
        ignore_list = IGNORE_LIST

    filenames = []
    if is_recursive:
        for root, _, names in os.walk(directory):
            for filename in names:
                filenames.append(os.path.join(root, filename))
    else:
        for filename in glob.glob(directory+'/*'+extension):
            if os.path.isfile(filename):
                filenames.append(filename)

    return filenames


def search_replace(searchterm, replaceterm, filename):
    if os.path.isfile(filename):
        for line in fileinput.input(filename, inplace=True):
            line = line.replace(searchterm, replaceterm)
            sys.stdout.write(line)


if __name__ == '__main__':
    main()
