#! /usr/bin/env python
""" 
A simple script to download notes from the Control 1 website

author: Leonard Mandla Mbuli <mandla.mbuli@live.co.za>

creation date: 22 October 2011
python 3
"""

import urllib.request

vanwyk_url = "http://dept.ee.wits.ac.za/~vanwyk/ELEN3016_2011/Notes/"
save_path = "/var/data/lunamystry/University/Second_sememster/ControlI/Lecture_Notes_2011/"

for lecture_num in range(1,19):
    filename = "Control%20I%20-%20Lecture%20" + str(lecture_num) + "%20(2011).pdf"
    print(filename)
    notes_url = vanwyk_url + filename
    print(notes_url)
    notes = urllib.request.urlopen(notes_url)
    notes_file = open(save_path + filename, 'wb') # binary open file
    notes_file.write(notes.read())
    notes_file.close()
