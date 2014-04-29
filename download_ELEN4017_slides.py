#! /usr/bin/env python
"""
Simple script to download m files from the Data course page
Because procrastination is more fun

author: Leonard Mandla Mbuli <mandla.mbuli@live.co.za>

creation date: 15 April 2013
python 3

non-standard dependencies: lxml
"""

import lxml.html
import urllib.request, urllib.error, urllib.parse

url = "http://dept.ee.wits.ac.za/~cheng/ELEN4017/lecture_notes/"
save_path = "/home/leny/University/networkfun/lecture_notes/"

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
    print("Processing: " + file_url)
    remote_file = urllib.request.urlopen(file_url)
    local_file = open(save_path + filename, 'wb') # binary open file
    local_file.write(remote_file.read())
    local_file.close()
    print("Saved file: " + save_path + filename)
