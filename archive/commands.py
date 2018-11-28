#! /usr/bin/env python
"""
commands: A script to help me manager and remember commands which
          I find useful but don't use often enough to stick in my memory.
          The commands are at the moment stored in a directory. I want
          this script to allow me to do the following:
            -list all commands
            -list commands by tag
            -have the tag system work without the script
             (ie store in a directory)
             I want it to work like gmail's tags

Commands to add
===============
  uname -a  # add command
  lscpi  # check hardware

author: Leonard Mandla Mbuli <mandla.mbuli@live.co.za>

creation date: 08 July 2011

"""
import argparse
import os

COMMANDS_DIR = os.path.join(os.getenv("MYBIN"), "Commands/")


def main():
    """ The main function"""
    des = "A script to help manage commands that I don't use often enough\
           to remember"
    parser = argparse.ArgumentParser(description=des)
    parser.add_argument('-t', '--tag', nargs='*', dest='tags', default=None,
                        type=str,
                        help="list commands that have a specified tag")
    parser.add_argument('-a', '--add', nargs='?', dest='command', default=None,
                        type=str,
                        help="Add a specified single command to the directory")
    parser.add_argument('-d', '--delete', nargs=1, dest='tag', default=None,
                        help="Delete a specified tag")
    parser.add_argument('-i', '--interactive', action='store_true',
                        help="run script in interactive mode")

    args = parser.parse_args()

    if(args.interactive):
        run_interactive()
    elif(args.command is not None):
        add_new_command(args.command, args.tags)
    else:
        list_by_tag(args.tags)


def run_interactive():
    """ Asks you what you wanna do"""
    pass


def list_by_tag(tags):
    """ Function which will list commands by tag if tags is not empty and
        list available tags if tags is empty."""
    if(tags is None):
        print("No tag specified")
    else:
        print("tags: "+" ".join(tags))


def add_new_command(command, tags):
    """adds a new command, also creates a new tag or adds it
    to the specified tag"""
    command_filename = COMMANDS_DIR+command.split()[0]
    print(command_filename)
    if os.isfile(command_filename):
        print("Filename exists")
    else:
        f = open(command_filename, 'w')

    if(tags is None):
        print(command+"-> No tag specified")
    else:
        print(command+"-> tags: "+" ".join(tags))

if __name__ == '__main__':
    main()
