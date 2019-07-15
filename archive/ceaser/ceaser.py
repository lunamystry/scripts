#! /usr/bin/env python
"""
A simple implementation of the ceaser Cypher

author: Leonard Mandla Mbuli
created: 7 March 2015

Interface
---------

    ceaser.py TEXT --shift=3
    ceaser.py --help

"""
import argparse
import logging
import string


logging.basicConfig(level=logging.INFO,
                    format='%(levelname)s: %(message)s')

def parse_args():
    '''This is the function which will parse command line arguments'''
    des = "A script to switch other display on and off"
    parser = argparse.ArgumentParser(description=des)
    parser.add_argument('text',
                        type=str,
                        help='the text word that you want to encrypt')
    parser.add_argument('--shift',
                        default=3,
                        type=int,
                        help='the shift for the encryption')

    args = parser.parse_args()
    cipher_text = ceaser(args.text, args.shift)
    decrypted_text = ceaser(cipher_text, -args.shift)
    logging.info(repr("%s -> %s -> %s" % (args.text, cipher_text, decrypted_text)))


def ceaser(text, shift, alphabet=None):
    '''Perfom the classic Ceaser cypher using either the given alphabet or the
    lowercase, whitespace, digits and punctuations from the standard library
    string. If encryption uses a positive shift, decryption should use a
    negative shift.'''
    cipher = ""
    if alphabet is None:
        alphabet = string.printable
    for char in text:
        cipher += alphabet[(alphabet.index(char) - shift) % len(alphabet)]
    return cipher


if __name__ == '__main__':
    parse_args()
