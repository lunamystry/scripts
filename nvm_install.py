#! /usr/bin/env python
"""
    nvm install 

    author: Leonard Mandla Mbuli <lm.mbuli@gmail.com>

    creation date: 12 February 2015
"""
import urllib2
import logging

logging.basicConfig(level=logging.INFO,
                    format='%(levelname)s: %(message)s')


def download(url, filename):
    sdk_download = urllib2.urlopen(url)
    f = open(filename, 'wb')
    meta = sdk_download.info()
    file_size = int(meta.getheaders("Content-Length")[0])
    logging.info("Downloading: %s Bytes: %s" % (filename, file_size))

    file_size_dl = 0
    block_sz = 8192
    while True:
        buffer = sdk_download.read(block_sz)
        if not buffer:
            break

        file_size_dl += len(buffer)
        f.write(buffer)
        status = r"%10d  [%3.2f%%]" % (file_size_dl, file_size_dl * 100. / file_size)
        status = status + chr(8)*(len(status)+1)
        logging.info(status)

    f.close()


if __name__ == '__main__':
    pass
