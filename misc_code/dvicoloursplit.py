#!/usr/bin/env python

# python program to take a dvi file, and split it into colour
# and black and white parts

# output files are called split_bw_xxx.ps and split_colour_xxx.ps

# Copyright (C) Jeremy Sanders 2001

##     This program is free software; you can redistribute it and/or modify
##     it under the terms of the GNU General Public License as published by
##     the Free Software Foundation; either version 2 of the License, or
##     (at your option) any later version.

##     This program is distributed in the hope that it will be useful,
##     but WITHOUT ANY WARRANTY; without even the implied warranty of
##     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
##     GNU General Public License for more details.

##     You should have received a copy of the GNU General Public License
##     along with this program; if not, write to the Free Software
##     Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

import os, sys, string

def usage():
    print 'Usage: ' + sys.argv[0] + ' [-s] file.dvi'
    print ' Takes a dvi file and output colour and black and white'
    print '  sections as postscript files'
    print ' -s option chooses simplex rather than duplex output'
    sys.exit(1)

# function takes file and returns whether it is a colour image or not
# checks triplet to see whether bytes are equal
def iscolourppm(filename):
    file = open(filename, 'r')

    lineno = 0

    for line in file.readlines():

        if line[0:1] == '#':
            continue

        if lineno == 0:
            if line != 'P3\n':
                print "Not a PPM file"
                sys.exit(1)
                
        lineno = lineno + 1
        nums = string.split(line)
                
        # skip non triplets
        if len(nums) % 3 != 0:
            continue

        for i in range(0, len(nums), 3):
            a = nums[i]
            b = nums[i+1]
            c = nums[i+2]
            if a != b or a != c or b != c:
                # is colour
                return 1

    # is black and white
    return 0

# main part of code
# useless argument handling - should use getopt
if len(sys.argv) == 1:   # no arguments
    usage()

if sys.argv[1] == '-s':
    doublesided = 0
    if len(sys.argv) != 3:
        usage()
    infile = sys.argv[2]
else:
    doublesided = 1
    if len(sys.argv) != 2:
        usage()
    infile = sys.argv[1]

tempfile = 'temp.ppm'
tempps = 'temp.ps'

colourpages={}

# iterate through pages, and find which are colour
page = 1
while 1:
    ret = os.system('dvips -p =' + str(page) + ' -l =' + str(page) +
                    ' -o ' + tempps + ' ' + infile )
    if ret != 0:
        break

    ret = os.system('gs -sDEVICE=ppm -dBATCH -dNOPAUSE'
                    ' -sPAPERSIZE=a4 -dSAFE -r20 -sOutputFile=' + tempfile +
                    ' ' + tempps)

    iscolour = iscolourppm(tempfile)
    colourpages[page] = iscolour
    page = page + 1

lastpage = page - 1

if doublesided:
    # mark matching pages as colour, too
    for page in range(1,lastpage+1):
        if colourpages[page]:
            if (page % 2 == 0) and (page > 1):  # even
                colourpages[page-1] = 1
                
            if (page % 2 != 0) and (page < lastpage): # odd
                colourpages[page+1] = 1

# now make black and white and colour output files
seqno = 1

lastcolourchangepage = 1
lastcolour = colourpages[1]

# generate output when the page becomes colour or black and white...
for page in range(1,lastpage+2):
    if (page == lastpage + 1) or (colourpages[page] != lastcolour):
        if lastcolour != 0:   # colour page
            name = 'split_colour_' + ('%03u' % seqno) + '.ps'
        else:
            name = 'split_bw_' + ('%03u' % seqno) + '.ps'
        seqno = seqno + 1

        option = ''
# add if offset between colour and black and white
#        if lastcolour != 0: # colour page
#            option = '1.8mm,-2.25mm '

        os.system('dvips -p =' + str(lastcolourchangepage) +
                  ' -l =' + str(page-1) + ' -o ' + name +
                  ' ' + option + infile)
        lastcolourchangepage = page
        if page <= lastpage:
            lastcolour = colourpages[page]
