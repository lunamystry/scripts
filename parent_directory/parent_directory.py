#! /usr/bin/env python3.6
"""
Script to download files from the... uhm... somewhere

author: Mandla Mbuli <mandla.mbuli@live.co.za>

creation date: 11 May 2018
python 3.6

non-standard dependencies: lxml, aiohttp, aiofiles
"""

from collections import namedtuple
import aiofiles
import aiohttp
import argparse
import async_timeout
import asyncio
import logging
import lxml.html
import os
import csv
from functools import partial

import urllib.request
import urllib.parse

logging.basicConfig(level=logging.INFO, format='%(message)s')
# construct a namespace dictionary to pass to the xpath() call
# this lets us use regular expressions in the xpath
ns = {'re': 'http://exslt.org/regular-expressions'}
DEFAULT_TIMEOUT = 10*60;

Link = namedtuple('Link', 'url name')
Input = namedtuple('Input', 'download_from save_to has')


def file_links(inputs, ns=ns):
    url = inputs.download_from
    reg = inputs.has

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

    return list(map(get_link, nodes))


async def download(links, save_path, loop):
    if (not os.path.isdir(save_path)):
        os.makedirs(save_path)

    print(f'there are {len(links)} links to download.')

    async def download_file(link, save_path, session):
        logging.info(f'Downloading: {link.name}...')
        local_name = os.path.join(save_path, os.path.basename(link.name))

        with async_timeout.timeout(DEFAULT_TIMEOUT):
            async with session.get(link.url) as response:
                async with aiofiles.open(local_name, 'wb') as fd:
                    logging.info(f'Saving: {local_name}...')
                    while True:
                        chunk = await response.content.read(1024)
                        if not chunk:
                            break
                        await fd.write(chunk)

                logging.info(f'Saved: {link.name}')
                return await response.release()

    async with aiohttp.ClientSession(loop=loop) as session:
        for link in links:
            await download_file(link, save_path, session)



async def get_inputs(input_filename):
    if not os.path.exists(input_filename):
        raise ValueError('Input file does not exist')

    lines = []

    with open(input_filename, newline='') as csvfile:
        reader = csv.DictReader(csvfile)
        for row in reader:
            i = Input(row['download_from'].strip()
                 , row['save_to'].strip()
                 , row['has'].strip()
                 )
            lines.append(i)

    return lines


async def main(args, loop):

    if args.input:
        inputs = await get_inputs(args.input)
        logging.info(f'Found {len(inputs)} input(s) in {args.input}');
    else:
        inputs = [Input(args.download_from, args.save_to, args.has)]

    links_list = list(map(file_links, inputs))
    for links,args in zip(links_list, inputs):
        await download(links, args.save_to, loop)

if __name__ == '__main__':
    parser = argparse.ArgumentParser(
            prog=__file__,
            formatter_class=argparse.RawDescriptionHelpFormatter,
            description=__doc__
            )

    parser.add_argument('download_from', help='The URL to download from', nargs='?', default=None)
    parser.add_argument('save_to', help='The directory to save to', nargs='?', default=None)
    parser.add_argument('--has', help='Regex used for filtering. Files matching are kept')
    parser.add_argument('-i', '--input', help='File to get inputs', nargs='?', default=None)

    args = parser.parse_args()
    if ((args.download_from is None or args.save_to is None) and args.input is None):
        parser.error('You must either give inputs or both download_from and save_to')

    loop = asyncio.get_event_loop()
    loop.run_until_complete(main(args, loop))
