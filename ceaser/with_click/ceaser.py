#! /usr/bin/env python
"""A script that perfoms a classic Ceaser shift substitution cypher on a given
text"""
import string
import click
import logging


logging.basicConfig(level=logging.INFO,
                    format='%(levelname)s: %(message)s')


def ceaser(text, shift, alphabet=None):
    """Perfom the classic Ceaser cypher using either the given alphabet or the
    lowercase, whitespace, digits and punctuations from the standard library
    string. If encryption uses a positive shift, decryption should use a
    negative shift."""
    cipher = ""
    if alphabet is None:
        alphabet = string.printable
    for char in text:
        cipher += alphabet[(alphabet.index(char) - shift) % len(alphabet)]
    return cipher


def get_content(filename):
    '''reads a file and returns its content as a sigle large string'''
    print(filename)


# pylint: disable=too-many-arguments
@click.command()
@click.option('--shift', type=int, default=3, help='Shift to use for the encryption')
@click.argument('text')
def main(text, shift):
    """run the encryption and decryption using the given args"""
    cipher_text = ceaser(text, shift)
    decrypted_text = ceaser(cipher_text, -shift)
    logging.info(repr("%s -> %s -> %s" % (text, cipher_text, decrypted_text)))


if __name__ == '__main__':
    # pylint: disable=no-value-for-parameter
    main()  # values will be provided by click
