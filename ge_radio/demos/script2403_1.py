######################################################################
#                                                                    #
# Use Case Script for reducing NGC 2403 HI line data                 #
# This is the first script of a set of three.  This script reads four#
# VLA archive files and stores them as a CASA measurement set.  It   #
# also lists the contents of the newly created measurement set.      #
#                                                                    #
#                                                                    #
# Original version GvM 2007-11-30 (Beta)                             #
#                                                                    #
######################################################################

import time
import os


# some comments on this script
#
#  o this is a non-interactive script - no interaction is required
#
#  o this script assigns values to global parameters, followed by a
#    single function call, e,g,:
#      default('listobs')
#      msfile='n2403.ms'
#      listobs()
#    note that some other scripts use local parameters instead:
#      listobs(msfile='n2403.ms')
# 



#=====================================================================
#
# Set up some useful variables - these will be set during the script
# also, but if you want to restart the script in the middle here
# they are in one place:

pathname=os.environ.get('AIPSPATH').split()[0]

prefix='n2403'
msfile = prefix + '.ms'

#
#=====================================================================
#
# Get to path to the CASA home and stip off the name
pathname=os.environ.get('AIPSPATH').split()[0]


# Clean up old versions of files to be created in this script

os.system('rm -rf '+prefix+'.ms*')

#=====================================================================
# Data Import
#=====================================================================
#
# Import the data from VLA archive files to MS
#
print "--importvla--"
print ""
print "Use importvla to read 4 VLA archive files and write the data"
print "into a Measurement Set (MS).  This will take a while ..."
print ""

# Set up the MS filename and save as new global variable
msfile = prefix + '.ms'

print "MS will be called "+msfile

default('importvla')

archivefiles=['AS649_1','AS649_2','AS649_3','AS649_4']
vis = msfile
importvla()

print ""


#=====================================================================
# List a summary of the MS
#=====================================================================
#
#
print "--listobs--"
print ""
print "Use listobs to print verbose summary to logger"
print "see the logger window for the listobs output"

# Don't default this one and make use of the previous setting of
# vis.  Remember, the variables are GLOBAL!

# You may wish to see more detailed information, in this case
# use the verbose = True option
#
verbose = True

listobs()

# The listobs output will be displayed in your logger window and in
# the casapy.log file


print ""
print "This marks the end of the first script"
