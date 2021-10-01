######################################################################
#                                                                    #
# Use Case Script for further reducing NGC 2403 HI line data         #
# This is the third script of a set of three.  This script subtracts #
# the continuum, and images/cleans the resulting line-free data.     #
# Finally, zero-th and first moment maps are made.                   #
#                                                                    #
#                                                                    #
# Original version GvM 2008-01-16                                    #
#                                                                    #
######################################################################

import time
import os

# 
#=====================================================================
#
# This script has some interactive commands: scriptmode = True
# if you are running it and want it to stop during interactive parts.

scriptmode = False

#=====================================================================
#
# Set up some useful variables - these will be set during the script
# also, but if you want to restart the script in the middle here
# they are in one place:

# The prefix to use for the input file

prefix='n2403'

# Set up the MS filename and save as new global variable

msfile    = prefix + '.ms'
splitfile = 'split2403.ms'

# Clean up old verions of files to be created in this script

os.system('rm -rf '+'split2403.ms*')
os.system('rm -rf '+prefix+'dirty'+'*')
os.system('rm -rf '+prefix+'cl'+'*')

#=====================================================================
# Split
#=====================================================================
#
# selects target source data (throws away all calibrator data).

print ""
print "--split--"
print ""
print " We are done with the calibrator data so we use split to select"
print " source data only (field '0').  Moreover, split's default is to"
print " move data in the 'corrected' datacolumn to the regular data"
print " column.  This frees up the 'corrected' datacolumn for the output"
print " of uvcontsub (which we will be running next)"
print ""

default('split')

vis       = msfile
outputvis = splitfile
field     = '0'

split()

print ""
# Pause script if you are running in scriptmode
if scriptmode:
    user_check=raw_input('split finished, hit Return to continue with uvcontsub\n')

#=====================================================================
# Continuum subtraction
#=====================================================================
#
# subtract continuum to form a line-only data set

print "--uvcontsub--"
print ""
print " fit a continuum using line-free regions on both ends of the spectrum"
print " and subtract this continuum.  Inspection of the earlier data cube"
print " shows that a good choice for line-free channels at either end are"
print " channels 21-30 and 92-111.  We use the parameter fitspw to specify"
print " the range of channels to base the fit on"
print ""
print " Note that no output file is needed; the results are stored in the"
print " 'corrected' data column"
print ""
print ""
print " have patience - this will take a while ..."
print ""

default('uvcontsub')

vis      = splitfile
field    = '0'
fitspw   = '0:21~30;92~111' 
fitorder = 1

uvcontsub()

print ""
# Pause script if you are running in scriptmode
if scriptmode:
    user_check=raw_input('done with uvcontsub, hit Return to continue script\n')

#=====================================================================
# image three channels to check uvcontsub result and determine rms
#=====================================================================
#
# image three channels to check result of uvcontsub

print ""
print "--clean--"
print ""
print " for now, we will image just three channels: 31, 61, and 91.  The first"
print " and last are line-free but were not part of the line-free range in"
print " uvcontsub and their rms noise is therefore indicative of the noise in"
print " the channels with line emission.  The middle channel is an example of"
print " a channel containing line emission without continuum"
print ""
print " Note by setting niter=0 we are just imaging; not cleaning"
print ""

# image output

outname = 'n2403dirty'

# remove possible previous clean output

os.system('rm -rf '+outname+'*')

default('clean')

vis       = splitfile
imagename = outname
field     = '0'
spw       = '0:31~91^30'
mode      = 'channel'
nchan     = 3
start     = 31
width     = 30
niter     = 0
imsize    = [512,512]
cell      = ['4arcsec','4arcsec']
weighting = 'briggs'
robust    = 0.0

clean()

print ""
# Pause script if you are running in scriptmode
if scriptmode:
    user_check=raw_input('clean ready, hit Return to view results\n')

#=====================================================================
# view result of clean
#=====================================================================
#
# view three test channels

print "--viewer--"
print ""
print " display continuum-free channels"
print ""

default('viewer')

infile = outname+'.image'

viewer()

print ""
# Pause script if you are running in scriptmode
if scriptmode:
    user_check=raw_input('clean ready, hit Return to determine rms noise\n')

##=====================================================================
## Determine the rms in a line-free channel (here: 0)
##=====================================================================
##
## use imaging tool (ia) to do this
#
#print "--im tool--"
#print ""
#print " there isn't a task yet to determine statistics over a specified area"
#print " in a data cube.  So for now we we will use the ia tool to do this"
#print ""
#print "Keep in mind that all coordinates are 0-relative, and that the first"
#print "channel (0) is channel 31 of the original data set"
#print ""
#
#ia.open(outname+'.image')
#box = ia.setboxregion([0,0,0,0],[511,511,0,0],frac=False)
#fullstat=ia.statistics(region=box)
#ia.close()
#
#rms_mJy=1000*fullstat['rms'][0]
#rms_mJy

#=====================================================================
# display image header
#=====================================================================
#
# 

print "--imhead--"
print ""
print " We will need to specify a subset of this cube, so let's explore the"
print " structure of the image cube first.  Use the task imhead for this"
print ""

default('imhead')

imagename = outname+'.image'

imhead()

print ""
print " Look in the log window for the output of imhead.  You will see that"
print " the axis order is RA, Dec, Stokes, Frequency.  Keep this in mind"
print " when specifying a subset of the image cube below"
print ""


#=====================================================================
# Determine the rms in a line-free channel (here: 0)
#=====================================================================
#
# use the task imstat to do this

print "--imstat--"
print ""
print "Keep in mind that all coordinates are 0-relative, and that the first"
print "channel (0) is channel 31 of the original data set"
print ""

default('imstat')

imagename = outname+'.image'
box       = '0,0,511,511'
chans     = '0'

xstat     = imstat()
rmsjy     = xstat['sigma'][0]
rmsmjy    = 1000 * rmsjy

print "rms = ", rmsmjy, "mJy"


print ""
# Pause script if you are running in scriptmode
if scriptmode:
    user_check=raw_input('hit Return to image and clean all channels\n')

#=====================================================================
# image and clean all channels
#=====================================================================
#

print ""
print "--clean--"
print ""
print "we will image all channels of interest, and are requesting"
print "cleaning by setting niter to a large number.  The value of"
print "threshold is based on the rms determined earlier using imstat"
print ""

# image output

outname = 'n2403cl'

# remove possible previous clean output

os.system('rm -rf '+outname+'*')

default('clean')

vis       = splitfile
imagename = outname
field     = '0'
spw       = '0:5~112'
mode      = 'channel'
nchan     = 55
start     = 32
width     = 1
niter     = 100000
threshold = 2.0*rmsmjy
psfmode   = 'clark'
mask      = [0,0,511,511]
imsize    = [512,512]
cell      = ['4arcsec','4arcsec']
weighting = 'briggs'
robust    = 0.0

clean()

print ""
# Pause script if you are running in scriptmode
if scriptmode:
    user_check=raw_input('clean ready, hit Return to view results\n')

#=====================================================================
# view result of clean
#=====================================================================
#
# view all channels

print "--viewer--"
print ""
print " display continuum-free channels"
print ""

default('viewer')

infile = outname+'.image'

viewer()

print ""
# Pause script if you are running in scriptmode
if scriptmode:
    user_check=raw_input('when done viewing, hit Return to continue\n')


#=====================================================================
# make a total HI map
#=====================================================================
#
# use the task immoments to do this

print ""
print "--immoments--"
print ""
print "as the final step we determine the 0'th and first moment maps, aka"
print "the total HI map and the velocity field.  For want of a better method"
print "we exclude pixel values falling in the interval given by excludepix."
print "Note the use of the rms determined earlier in imstat"
print ""

default('immoments')

imagename  = outname+'.image'
moments    = [0,1]
axis       = 3
excludepix = [-100, 3*rmsjy]
outfile    = outname + 'mom'

immoments()

#=====================================================================
# view result of immoments; first total HI, then velocity field
#=====================================================================
#
# view 

print "--viewer--"
print ""
print " display continuum-free channels"
print ""

default('viewer')

infile = outfile + '.integrated'

viewer()

infile = outfile + '.weighted_coord'

viewer()






