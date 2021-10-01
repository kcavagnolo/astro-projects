######################################################################
#                                                                    #
# Use Case Script for imaging the NGC 2403 HI line data              #
# This script must be run AFTER running the main NGC2403 script      #
#    ngc2403_tutorial_regression.py                                  #
# as it expects flagged, calibrated, and split data.                 #
# You can tune the deconvolution parameters in the clean section.    #
# This script reports results but does not do regression             #
#                                                                    #
# Imaging only     STM 2008-07-28   From ngc2403_tutorial_regression #
#                                                                    #
######################################################################

import time
import os
import pickle

# 
#=====================================================================
#
# This script has some interactive commands: scriptmode = True
# if you are running it and want it to stop during interactive parts.

scriptmode = False

# Enable benchmarking?
benchmarking = True

#=====================================================================
#
# Set up some useful variables

pathname=os.environ.get('AIPSPATH').split()[0]

prefix='n2403.tutorial'

# Sets a shorthand for fixed input script/regression files
scriptprefix='ngc2403_tutorial_regression'

# From the ngc2403_tutorial_regression script
msfile = prefix + '.ms'
splitfile = prefix + '.split.ms'

# Prefix for output from this script
imprefix='n2403.imaging'

outname = imprefix + '.final.clean'
momfile = outname + '.mom'

#
#=====================================================================
# Clean up old versions of files to be created in this script

os.system('rm -rf '+imprefix+'.*')

if benchmarking:
    startTime=time.time()
    startProc=time.clock()

#=====================================================================
# IMAGING
# Now image and clean all channels
#=====================================================================
#

print ""
print "--clean--"
print ""
print " we will image all channels of interest, and are requesting"
print " cleaning by setting niter to a large number and using a threshold"
print ""

default('clean')

outname = imprefix + '.final.clean'

vis       = splitfile
imagename = outname
field     = '0'
spw       = '0'
mode      = 'channel'
nchan     = 64
start     = 32
width     = 1
niter     = 100000

threshold = 0.6
print " using clean threshold = "+str(threshold)+" mJy"
print ""

psfmode   = 'clark'
print " using psfmode = "+psfmode
print ""

#imagermode = ''
imagermode = 'csclean'
print " using imagermode = "+imagermode
print ""
if ( imagermode=='csclean' ):
    cyclefactor = 1.5
    cyclespeedup = -1
    print "   cyclefactor  = "+str(cyclefactor)
    print "   cyclespeedup = "+str(cyclespeedup)

mask      = [0,0,511,511]
imsize    = [512,512]
cell      = ['4arcsec','4arcsec']
weighting = 'briggs'
robust    = 0.0

saveinputs('clean',imprefix+'.saved.clean')

clean()

if benchmarking:
    clean2time=time.time()

print ""

#=====================================================================
# view result of clean
#=====================================================================
#
# view all channels if you are running in scriptmode
if scriptmode:
    print "--viewer (clean cube)--"
    print ""
    print " display continuum-free channels"
    print ""
    
    default('viewer')

    infile = outname+'.image'

    viewer()

    user_check=raw_input('when done viewing, Close and hit Return to continue\n')

    print ""

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
# Statistics on clean image cube and moments
#=====================================================================
#
print '--imstat (clean cube)--'
default('imstat')

imagename = outname+'.image'

# determine the stats in entire cube

srcstat = imstat()

print "Found image max = "+str(srcstat['max'][0])+" Jy"

# determine the stats in a off-source box

offbox = '10,384,195,505'
box = offbox

offstat = imstat()

print "Found off-source image rms = "+str(offstat['sigma'][0])+" Jy"

# determine the stats in line-free channels (here: 0-3)

cenbox = '128,128,384,384'
offlinechan = '0,1,2,3'

box = cenbox
chans = offlinechan

offlinestat = imstat()

print "Found off-line image rms = "+str(offlinestat['sigma'][0])+" Jy"
offline_rms = offlinestat['sigma'][0]

if benchmarking:
    stat2time=time.time()

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
print "we exclude pixel values falling in the interval given by excludepix at"
print "  cutoff = "+str(3*offline_rms)+" Jy"
print "Note the use of the rms determined earlier in imstat"
print ""

default('immoments')

momfile    = outname + '.mom'

imagename  = outname + '.image'
moments    = [0,1]
planes     = '4~56'
axis       = 3
excludepix = [-100, 3.0*offline_rms]
outfile    = momfile

saveinputs('immoments',imprefix+'.saved.immoments')

immoments()

if benchmarking:
    moments2time=time.time()

#=====================================================================
# view result of immoments; first total HI, then velocity field
#=====================================================================
#
# view if in scriptmode

if scriptmode:
    print "--viewer--"
    print ""

    default('viewer')

    infile = outfile + '.integrated'

    viewer()

    print " display moment 0 of continuum-free channels"
    print ""
    user_check=raw_input('when done viewing, Close and hit Return to continue\n')

    infile = outfile + '.weighted_coord'

    viewer()

    print ""
    print " display moment 1 of continuum-free channels"
    print ""
    user_check=raw_input('when done viewing, Close and hit Return to continue\n')

    print ""


#=====================================================================
# Statistics on moment images
#=====================================================================
#
print '--imstat (moment images)--'
default('imstat')

imagename = momfile+'.integrated'

momzerostat=imstat()

try:
    print "Found moment 0 max = "+str(momzerostat['max'][0])
    print "Found moment 0 rms = "+str(momzerostat['rms'][0])
except:
    pass

imagename = momfile+'.weighted_coord'

momonestat=imstat()

try:
    print "Found moment 1 median = "+str(momonestat['median'][0])
except:
    pass

if benchmarking:
    momstat2time=time.time()

#=====================================================================
# DONE
#=====================================================================
if benchmarking:
    endProc=time.clock()
    endTime=time.time()

print ""
print "Completed imaging of NGC2403 data"
print ""

#
##########################################################################
# Calculate regression values
##########################################################################
print '--Calculate Regression Results--'
print ''
#
# Save these stats
#
try:
    srcmax = srcstat['max'][0]
except:
    srcmax = 0.0

try:
    offrms = offstat['sigma'][0]
except:
    offrms = 0.0

try:
    offlinerms = offlinestat['sigma'][0]
except:
    offlinerms = 0.0
    
try:
    momzero_max = momzerostat['max'][0]
except:
    momzero_max = 0.0

try:
    momzero_rms = momzerostat['rms'][0]
except:
    momzero_rms = 0.0

try:
    momone_median = momonestat['median'][0]
except:
    momone_median = 0.0

#
##########################################################################
# Fill results
# 
# Some date and version info
import datetime
datestring=datetime.datetime.isoformat(datetime.datetime.today())

myvers = casalog.version()
myuser = os.getenv('USER')
myhost = os.getenv('HOST')
mycwd = os.getcwd()
myos = os.uname()

results = {}

results['clean_image_max'] = {}
results['clean_image_max']['name'] = 'Clean image max'
results['clean_image_max']['value'] = srcmax

results['clean_image_offsrc_rms'] = {}
results['clean_image_offsrc_rms']['name'] = 'Clean image off-src rms'
results['clean_image_offsrc_rms']['value'] = offrms

results['clean_image_offline_rms'] = {}
results['clean_image_offline_rms']['name'] = 'Clean image off-line rms'
results['clean_image_offline_rms']['value'] = offlinerms

results['clean_momentzero_max'] = {}
results['clean_momentzero_max']['name'] = 'Moment 0 image max'
results['clean_momentzero_max']['value'] = momzero_max

results['clean_momentzero_rms'] = {}
results['clean_momentzero_rms']['name'] = 'Moment 0 image rms'
results['clean_momentzero_rms']['value'] = momzero_rms

results['clean_momentone_median'] = {}
results['clean_momentone_median']['name'] = 'Moment 1 image median'
results['clean_momentone_median']['value'] = momone_median

#
##########################################################################
#
# Timing
#
if benchmarking:
    walltime = (endTime - startTime)
    cputime = (endProc - startProc)

    imstages = {}
    nimstages = 4
    imstages[0] = ['clean',(clean2time-startTime)]
    imstages[1] = ['stat',(stat2time-clean2time)]
    imstages[2] = ['moments',(moments2time-stat2time)]
    imstages[3] = ['momstat',(momstat2time-moments2time)]
    
#
##########################################################################
#
# Now print out results
# The following writes a logfile for posterity
#
##########################################################################
#
outfile='out.'+prefix+'.'+datestring+'.log'
logfile=open(outfile,'w')

# Print version info to outfile
print >>logfile,'Regression = NGC2403 Imaging'
print >>logfile,'Running '+myvers+' on host '+myhost
print >>logfile,'at '+datestring
print >>logfile,''

# Print out comparison:
print >>logfile,'---'
print >>logfile,'Found new values:'
print >>logfile,'---'

res = {}
resultlist = ['clean_image_max',
              'clean_image_offsrc_rms','clean_image_offline_rms',
              'clean_momentzero_max','clean_momentzero_rms','clean_momentone_median']

for keys in resultlist:
    res = results[keys]
    print '--%30s : %12.6f ' % ( res['name'], res['value'] )
    print >>logfile,'--%30s : %12.6f ' % ( res['name'], res['value'] )

#
##########################################################################
# Print benchmarking etc.

if benchmarking:
    print ''
    print 'Total wall clock time was: %10.3f ' % walltime
    print 'Total CPU        time was: %10.3f ' % cputime
    print ''
    print '* Breakdown:                              *'

    print >>logfile,''
    print >>logfile,'********* Benchmarking *************************'
    print >>logfile,'*                                              *'
    print >>logfile,'Total wall clock time was: %10.3f ' % walltime
    print >>logfile,'Total CPU        time was: %10.3f ' % cputime
    print >>logfile,'* Breakdown:                                   *'

    for i in range(nimstages):
        print '* %16s * time was: %10.3f ' % tuple(imstages[i])
        print >>logfile,'* %16s * time was: %10.3f ' % tuple(imstages[i])
    
    print >>logfile,'************************************************'

logfile.close()

print "Done with NGC2403 Imaging"
#
##########################################################################
