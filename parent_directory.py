#! /usr/bin/env python3.6
"""
Script to download files from the... uhm... somewhere

author: Mandla Mbuli <mandla.mbuli@live.co.za>

creation date: 11 May 2018
python 3

non-standard dependencies: lxml
"""

import lxml.html
import urllib.request, urllib.error, urllib.parse
import logging
import argparse

logging.basicConfig(level=logging.INFO, format='%(message)s')

url = "http://dept.ee.wits.ac.za/~cheng/ELEN4017/tuts/"
save_path = "/home/leny/University/networkfun/tutorials/"

def download():
    # fetch the page
    res = urllib.request.urlopen(url)

    page = str(res.read())
    # parse the response into an xml tree
    tree = lxml.html.fromstring(page)

    # construct a namespace dictionary to pass to the xpath() call
    # this lets us use regular expressions in the xpath
    ns = {'re': 'http://exslt.org/regular-expressions'}

    # iterate over all <a> tags whose href ends in ".pdf" (case-insensitive)
    for node in tree.xpath('//a[re:test(@href, "\.pdf$", "i")]', namespaces=ns):
        filename = node.attrib['href']
        file_url = urllib.parse.urljoin(url, filename)

        logging.info("Processing: " + file_url)

        remote_file = urllib.request.urlopen(file_url)
        local_file = open(save_path + filename, 'wb') # binary open file
        local_file.write(remote_file.read())
        local_file.close()

        logging.info("Saved file: " + save_path + filename)


if __name__ == '__main__':
    parser = argparse.ArgumentParser(
            prog=__file__,
            formatter_class=argparse.RawDescriptionHelpFormatter,
            description=__doc__
            )

    parser.add_argument('download_from', help='The URL to download from')
    parser.add_argument('save_to', help='The directory to save to')
    parser.add_argument('-f', '--filter', help='Regex used for filtering')

    args = parser.parse_args()
    logging.info(args)
    # download()
