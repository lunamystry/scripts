#! /usr/bin/env python
"""
odisplay: A script to switch on another display on and off

author: Leonard Mbuli <mandla.mbuli@live.co.za>

creation date: 08 July 2011

"""
import argparse
import logging
import subprocess
import os

logging.basicConfig(level=logging.INFO,
                    format='%(levelname)s: %(message)s')


def parse_args():
    """ The main function"""
    des = "A script to switch other display on and off"
    parser = argparse.ArgumentParser(description=des)
    parser.add_argument('mode',
                        default='on',
                        nargs='?',
                        type=str,
                        metavar='mode',
                        help="[on/off] switch monitor on or off")
    parser.add_argument('--above',
                        action='store_true',
                        help="The other display is on the above")
    parser.add_argument('--below',
                        action='store_true',
                        help="The other display is on the below")
    parser.add_argument('--right',
                        action='store_true',
                        help="The other display is on the right")
    parser.add_argument('--left',
                        action='store_true',
                        help="The other display is on the left")
    parser.add_argument('-s',
                        '--size',
                        default='1280x1024',
                        nargs='?',
                        type=str,
                        help="size of the other monitor")

    args = parser.parse_args()
    if(args.mode == 'on'):
        if args.below:
           switch_on('below', args.size)
        elif args.left:
           switch_on('left', args.size)
        elif args.above:
           switch_on('above', args.size)
        else:
           switch_on('right', args.size)
    elif(args.mode == 'off'):
        switch_off()
    else:
        parser.print_help()


def switch_on(position, size):
    """ Switches on the other display"""
    logging.info("Switching on")
    cmd = """xrandr --output LVDS1 --mode 1280x800;
             xrandr --output VGA1 --auto --right-of LVDS1"""
    if position == "left" or position == "right":
        position = position + "-of"

    cmd = cmd.replace("--auto", "--mode " + size)
    cmd = cmd.replace("right-of", position)
    run_command(cmd)
    logging.info("odisplay size: " + size)
    logging.info("odiplay position: " + position)
    logging.info("VGA is probably on")


def switch_off():
    """ Switches off the other display"""
    logging.info("Switching off")
    cmd = "xrandr --output VGA1 --off"
    run_command(cmd)
    logging.info("VGA is probably off")


def run_command(command):
    process = subprocess.Popen(command,
                               shell=True,
                               stdout=subprocess.PIPE,
                               stderr=subprocess.PIPE)
    return process.communicate()


if __name__ == "__main__":
    parse_args()
