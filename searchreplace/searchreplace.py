#! /usr/bin/env python
'''
    searchreplace

    author: Mandla Mbuli <mail@mandla.me>

    creation date: 22 September 2012
    updated: June 2015

    python 2 or 3
'''
from __future__ import print_function
import argparse
import glob
import os
import re
import sys
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
                        help=('The directory to search within, default is'
                              'current directory'))
    parser.add_argument('-e', '--extension', action='store', default='',
                        help='Search only for files matching the extension')
    parser.add_argument('-f', '--filename', action='store',
                        help='Search and replace only within specified file')
    parser.add_argument('-r', '--recursive', action='store_true',
                        help=('When searching in a directory, where to decend'
                              'into sub directories'))
    parser.add_argument('-i', '--ignore', action='store',
                        help='dont search in directory')
    parser.add_argument('-c', '--confirm', action='store_true',
                        help='ask before you replace')
    args = parser.parse_args()
    print('replace "%s" with "%s"' % (args.searchterm, args.replaceterm))

    if args.filename is None:
        filenames = find_filenames(args.directory,
                                   args.extension,
                                   args.recursive,
                                   args.ignore)
        for filename in filenames:
            search_replace(args.searchterm, args.replaceterm, filename, args.confirm)
    else:
        search_replace(args.searchterm, args.replaceterm, args.filename, args.confirm)


def find_filenames(directory, extension='', is_recursive=True, ignore_str=None):
    '''searches either the provided directory for filenames'''
    # Add '.' to avoid .ctop vs .top if extesion=top
    if extension and not extension.startswith('.'):
        extension = '.' + extension

    if ignore_str is None:
        ignore_str = r'|'.join(IGNORE_LIST)
    else:
        ignore_str += '|' + r'|'.join(IGNORE_LIST)

    ignore_rgx = re.compile(ignore_str)

    if is_recursive:
        filenames = [os.path.join(root, filename)
                     for root, _, filenames in os.walk(directory)
                     for filename in filenames
                     if not ignore_rgx.search(filename) and
                     filename.endswith(extension)]
    else:
        # glob.glob (vs os.listdir) gives me the filename with dir for free
        filenames = [filename for filename in glob.glob(directory+'/*')
                     if os.path.isfile(filename) and
                     not ignore_rgx.search(filename) and
                     filename.endswith(extension)]
    return filenames


def should_replace(searchterm, line_info):
    '''Should the line be done'''
    if re.search(searchterm, line_info['line']):
        info = '{lineno} "{filename}": {line}'.format(**line_info)
        print(info, end='')
        ans = input('\tReplace? [Y/n] ')
        if ans.lower() == 'n':
            return False
        else:
            return True
    return False


def search_replace(searchterm, replaceterm, filename, confirm=False):
    if not os.path.isfile(filename):
        return

    with open(filename, 'r') as original_file:
        backup_filename = ".%s.bak" % filename
        with open(backup_filename, 'w') as backup_file:
            lineno = 0
            for line in original_file:
                lineno += 1
                line_info = {'lineno': lineno,
                            'filename': filename,
                            'line': line}
                if confirm and should_replace(searchterm, line_info):
                    line = line.replace(searchterm, replaceterm)
                backup_file.write(line)
    os.rename(backup_filename, filename)


if __name__ == '__main__':
    main()
