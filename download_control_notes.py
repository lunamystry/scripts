#! /usr/bin/env python
"""
A simple script to download notes from the Control 1 website

author: Leonard Mandla Mbuli <mandla.mbuli@live.co.za>

creation date: 22 October 2011
python 3
"""

import urllib.request
import logging

logging.basicConfig(level=logging.INFO,
                    format='%(message)s')

src_url = "http://dept.ee.wits.ac.za/~nyandoro/ControlI/"
save_path = "/home/leny/University/Control_1/lecture_slides/"

for lecture_num in range(1,10):
    filename = "Control%20I%20-%20Lecture%20" + str(lecture_num) + ".pdf"
    notes_url = src_url + filename
    logging.info("url: " + notes_url)
    logging.info("remote filename: " + filename)
    notes = urllib.request.urlopen(notes_url)
    local_url = save_path + filename.replace("%20","_")
    notes_file = open(local_url, 'wb') # binary open file
    logging.info("local url: " + local_url)
    logging.info("--")
    notes_file.write(notes.read())
    notes_file.close()
