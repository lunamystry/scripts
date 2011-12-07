#! /usr/bin/env python

""" 
newFile: Create a new file from templates

author: Leonard Mandla Mbuli <mandla.mbuli@live.co.za>

creation date: 08 July 2011

"""

import subprocess
from optparse import OptionParser
from datetime import date

# Definitions and configs
TEMPLATE_DIR = "$HOME/Templates/"
VERBOSE = False
today = date.today().strftime("%A, %d %B %Y")
default_words = {'<name>':"Leonard Mandla Mbuli" 
    , '<creation date>':today
    , '<last updated>':today
    , '<current version>':'0.1'} 

def newFile(filename, ext):
    """ Create a new python bash script file with the given file name
      The filename is either the full path or the relative path where you want the 
      template""" 
    global TEMPLATE_DIR
    if filename != None:
      cmd = "cp -iv "+TEMPLATE_DIR+"*."+ext+" "+filename
    else:
      cmd = "cp -iv "+TEMPLATE_DIR+"*."+ext+" new_file."+ext
    #TODO: check for an error from the cp command
    p = subprocess.Popen(cmd, shell=True, stdout=subprocess.PIPE)
    p.stdout.read().strip()
    touchFile(filename)

def touchFile(filename):
    """This will update the date and names inside the file with the new filename
    TODO: I need another function to make all the comment sections look the same"""
    f = open(filename, 'r+')
    for line in f.readlines():
      f.write(replace_all(line, default_words))
    f.close()

def replace_all(text, dic):
    """replaces multiple occurences of word defined in a map/dic in text"""
    for i, j in dic.iteritems():
      text = text.replace(i, j)
    return text

def newCppHeader(filename):
    """ This will copy the cpp header file into the new directory, changing the 
    date of creation"""
    global TEMPLATE_DIR

def controller():
    """Select which file to create"""
    global VERBOSE
    #Create instance of OptionParser Module, included in Standard Library
    p = OptionParser(description = 'This is a program to manage templates',
        prog = 'template_manager',
        version = 'template manager 0.1',
        usage = '%prog [option]')
    p.add_option('--usage', '-u', action="store_true",
        help='gets disk usage of home dir')
    p.add_option('--verbose', '-v', action='store_true',
        help='prints verbosely', default=False)
    p.add_option('--bash', '-b', action='store_true',
        help='new bash script', default=False)
    p.add_option('--cppsrc', '-c', action='store_true',
        help='new cpp file', default=False)
    p.add_option('--cppheader', '-p', action='store_true',
        help='new cpp header file', default=False)
    p.add_option('--file', '-f', action='store',
        help='specify file name', dest="filename")

    #Option Handling  passes correct parameters to runBash
    options, arguments = p.parse_args()
    if options.verbose:
      VERBOSE = True
    if options.bash:
      newFile(options.filename, 'sh')
    elif options.cppsrc:
      newFile(options.filename, 'cc')
    else:
      p.print_help()

def main(): 
    """The main function"""
    controller()

if __name__ == '__main__':
    main()
