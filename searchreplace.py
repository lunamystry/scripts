#! /usr/bin/env python
descr = """
    searchreplace

    author: Leonard Mandla Mbuli <lm.mbuli@gmail.com>

    creation date: 22 September 2012

    python 3
"""

import fileinput
import re
import os
import sys
import glob
import argparse
import textwrap


def main():
    """ Does everything """
    parser = argparse.ArgumentParser(
        formatter_class=argparse.RawDescriptionHelpFormatter,
        description=textwrap.dedent(descr)
        )
    parser.add_argument('searchterm',
                        help="The term to search for")
    parser.add_argument('replaceterm',
                        help="The term to replace the search term with")
    parser.add_argument('-d', '--directory', action='store', default=".",
                        help="The directory to search within, default is \
                                current directory")
    parser.add_argument('-e', '--extension', action='store', default="",
                        help="Search only for files matching the extension")
    parser.add_argument('-f', '--filename', action='store',
                        help="Search and replace only within specified file")
    parser.add_argument('-r', '--recursive', action='store_true',
                        help="When searching in a directory, where to decend \
                        into sub directories")
    args = parser.parse_args()
    print("SEARCH: "+args.searchterm)
    print("REPLACE: "+args.replaceterm)

    if args.filename is None:
        filenames = find_filenames(args.directory,
                                   args.extension,
                                   args.recursive)
        for filename in filenames:
            search_replace(args.searchterm, args.replaceterm, filename)
    else:
        search_replace(args.searchterm, args.replaceterm, args.filename)


def find_filenames(directory, extension, is_recursive):
    "searches either the provided directory for filenames "
    paths = glob.glob(directory+"/*")
    filenames = []
    for path in paths:
        if os.path.isdir(path) and is_recursive:
            filenames += find_filenames(path, extension, is_recursive)
        else:
            filenames.append(path)

    if extension != "":
        filenames = [filename for filename in filenames
                     if os.path.splitext(filename)[1] == "."+extension]

    return filenames


def search_replace(searchterm, replaceterm, filename):
    if os.path.isfile(filename):
        replacements = 0
        for line in fileinput.input(filename, inplace=True):
            if re.match(r''+searchterm, line) is not None:
                replacements = replacements + 1
                line = line.replace(searchterm, replaceterm)
                sys.stdout.write(line)
            else:
                sys.stdout.write(line)
        print("IN: "+filename)

if __name__ == '__main__':
    main()
