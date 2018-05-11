#! /usr/bin/env python3.6
"""
Script to download files from the... uhm... somewhere

author: Mandla Mbuli <mandla.mbuli@live.co.za>

creation date: 11 May 2018
python 3.6

non-standard dependencies: lxml, aiohttp
"""

import lxml.html
import urllib.request
import urllib.error
import urllib.parse
import logging
import argparse
import os
import threading
import aiohttp
from collections import namedtuple

logging.basicConfig(level=logging.INFO, format='%(message)s')
# construct a namespace dictionary to pass to the xpath() call
# this lets us use regular expressions in the xpath
ns = {'re': 'http://exslt.org/regular-expressions'}

Link = namedtuple('Link', 'url name')


def file_links(url, reg, ns=ns):
    logging.info(f'getting the links from: {url}')
    # fetch the page
    res = urllib.request.urlopen(url)

    page = str(res.read())

    # parse the response into an xml tree
    tree = lxml.html.fromstring(page)

    regex = fr'//a[re:test(@href, "{reg}", "i")]'
    nodes = tree.xpath(regex, namespaces=ns)

    def get_link(node):
        filename = node.attrib['href']
        file_url = urllib.parse.urljoin(url, filename)

        return Link(file_url, filename)

    return map(get_link, nodes)


def download(links, save_path):
    if (not os.path.isdir(save_path)):
        os.makedirs(save_path)

    def download_file(link):
        logging.info(f'Downloading: {link.name}...')
        remote_file = urllib.request.urlopen(link.url)
        with open(link.name, 'wb') as local_file:
            logging.info(f'Saving: {link.name}...')
            local_file.write(remote_file.read())

        logging.info(f'Saved: {link.name}')

    return map(download_file, links)


if __name__ == '__main__':
    parser = argparse.ArgumentParser(
            prog=__file__,
            formatter_class=argparse.RawDescriptionHelpFormatter,
            description=__doc__
            )

    parser.add_argument('download_from', help='The URL to download from')
    parser.add_argument('save_to', help='The directory to save to')
    parser.add_argument('--has', help='Regex used for filtering. Files matching are kept')

    args = parser.parse_args()
    links = file_links(url, args.has)
    download(links, args.save_to)
