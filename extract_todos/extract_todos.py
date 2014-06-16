#! /usr/bin/env python
descr = """
    extract todos

    author: Leonard Mandla Mbuli <lm.mbuli@gmail.com>

    creation date: 11 August 2013

    python 3
"""

import fileinput
import re
import os
import sys
import glob
import argparse
import textwrap
import logging

logging.root.setLevel(logging.INFO)
formatter = logging.Formatter('[%(levelname)s] %(message)s')
handler = logging.StreamHandler()
handler.setFormatter(formatter)
logging.root.addHandler(handler)

def parse_args():
    """ Does everything """
    parser = argparse.ArgumentParser(
        formatter_class=argparse.RawDescriptionHelpFormatter,
        description=textwrap.dedent(descr)
        )
    parser.add_argument('-d', '--directory', action='store', default=".",
                        help="""The directory to search within, default is
                              current directory""")
    parser.add_argument('-e', '--extension', action='store', default="",
                        help="Search only for files matching the extension")
    parser.add_argument('-f', '--filename', action='store',
                        help="Search only within specified file")
    parser.add_argument('-o', '--outputfilename', action='store', default="todos.out",
                        help="write output in this file, default: todos.out")
    parser.add_argument('-r', '--recursive', action='store_true',
                        help="""When searching in a directory,
                                where to decend into sub directories""")
    args = parser.parse_args()
    filenames = find_filenames(args.directory, args.extension, args.recursive)
    if filenames:
        for filename in filenames:
            search_replace(filename, args.outputfilename)

def find_filenames(directory, extension, is_recursive):
    "searches either the provided directory for filenames "
    logging.info("searching...: "+directory)
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

    if filenames:
        logging.info("found: "+str(' '.join(filenames)))
        return filenames
    else:
        logging.info(":-( No files found in "+directory)


def search_replace(filename, outputfile="todos.out"):
    if os.path.isfile(filename):
        logging.info("searching in: "+filename)
        num_found = 0
        file = open(filename, 'r')
        out_file = open(outputfile, 'a')
        for line_num, line in enumerate(file.readlines()):
            if re.search(r'TODO', line):
                num_found = num_found + 1
                out_file.write(filename+" line: "+str(line_num + 1)+"\n")
                logging.info("\tFound one :-)")


if __name__ == '__main__':
    parse_args()
