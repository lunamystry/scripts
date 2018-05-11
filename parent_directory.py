#! /usr/bin/env python3.6
"""
Script to download files from the... uhm... somewhere

author: Mandla Mbuli <mandla.mbuli@live.co.za>

creation date: 11 May 2018
python 3

non-standard dependencies: lxml
"""

import lxml.html
import urllib.request
import urllib.error
import urllib.parse
import logging
import argparse
import os
import threading

logging.basicConfig(level=logging.INFO, format='%(message)s')
# construct a namespace dictionary to pass to the xpath() call
# this lets us use regular expressions in the xpath
ns = {'re': 'http://exslt.org/regular-expressions'}



def file_links(url, reg, ns=ns):
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

        return file_url, filename


    print(list(map(get_link, nodes)))

    return links


def download(url, save_path, reg):
    links = file_links(url, reg)
    if (not os.path.isdir(save_path)):
        os.makedirs(save_path)


    # for node in tree.xpath(reg, namespaces=ns):
    #     filename = node.attrib['href']
    #     file_url = urllib.parse.urljoin(url, filename)
    #     local_filename = os.path.join(save_path, filename)

    #     logging.info(f'Processing: {file_url}')

    #     remote_file = urllib.request.urlopen(file_url)
    #     local_file = open(local_filename, 'wb') # binary open file
    #     local_file.write(remote_file.read())
    #     local_file.close()

    #     logging.info(f'Saved: {local_filename}')


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
    download(args.download_from, args.save_to, args.has)
