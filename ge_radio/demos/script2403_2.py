######################################################################
#                                                                    #
# Use Case Script for reducing NGC 2403 HI line data                 #
# This is the second script of a set of three.  This script allows   #
# the user to inspect, flag, and calibrate the data.  In order to    #
# assess the quality maps will be made and displayed                 #
#                                                                    #
#                                                                    #
# Original version GvM 2007-11-30 (Beta)                             #
# This version reads data from 4 VLA archive files                   #
#                                                                    #
######################################################################

import time
import os

# comments on this script:
#
#  o this is an interactive script - in some cases a carriage return
#    is required to proceed.  This allows the user to flag or interact
#    with the plotter or viewer
#
#  o this script assigns values to global parameters, followed by a
#    single function call, e,g,:
#      default('listobs')
#      msfile='n2403.ms'
#      listobs()
#    note that some other scripts use local parameters instead:
#      listobs(msfile='n2403.ms')
# 

scriptmode = True

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


# Clean up old verions of files to be created in this script

os.system('rm -rf '+prefix+'.psf'+'*')
os.system('rm -rf '+prefix+'.flux'+'*')
os.system('rm -rf '+prefix+'.residual'+'*')
os.system('rm -rf '+prefix+'.model'+'*')
os.system('rm -rf '+prefix+'.?cal'+'*')

#=====================================================================
# Undo any flagging that may still be lying around
#=====================================================================
#
print ""
print "--flagmanager--"
print ""
print "undo any previous flagging"
print ""

default('flagmanager')

vis         = msfile
mode        = 'restore'
versionname = 'Original'

flagmanager()

#=====================================================================
# Fill the model column for flux density calibrators
#=====================================================================
#
print "--setjy--"
print ""
print "find the flux of the flux calibrators, and write it to the"
print "column labeled MODEL_DATA"

default('setjy')

vis=msfile
field='1,3,4'     # note: field 1 is the source NGC 2403, and field 2 is
                  # the phase calibrator 0841+708'
spw='0:5~112'     # use spectral window 0 (which is the only one).  In that
                  # window, use channels 5 - 112 (ignoring edge channels)

setjy()

print ""


#=====================================================================
# Plot the calibrator visibilities
#=====================================================================
#

print "--plotxy--"
print ""
print "plots all calibrator visibilities"

default('plotxy')

vis=msfile
spw='0:5~112'
field='1~4'
width='108'

plotxy()

print ""
print "Showing all visiblities for all calibrators in one plot"
print "no points absolutely require flagging, so let's continue"
print ""
print "you can leave the plotter running, so don't click quit\n"

    
#=====================================================================
# Plot the source visibilities
#=====================================================================
#
print "--plotxy--"
print ""
print "plots source visibilities, just RR for now"

default('plotxy')

vis=msfile
selectdata=True
correlation='RR'
spw='0:5~112'
field='0'
width='108'

plotxy()

print ""
print "Showing NGC 2403, RR"
print "clearly, there is one bad time interval"
print "click Mark Region, then draw a rectangle around the bad data"
print "click Locate to list the points, and Flag to flag them.  After"
print "Flag, the plot will be redrawn with the appropriate scale.  Do"
print "more flagging if necessary"
print ""
print "when happy with flagging, don't click quit"
print ""

# Pause script if you are running in scriptmode

if scriptmode:
    user_check=raw_input('happy with flagging? Hit Return to continue script\n')

print ""

correlation='LL'
plotxy()
print ""
print "Now showing NGC 2403, LL"
print "one bad point is visible in the upper right corner"
print "click Mark Region, then draw a rectangle around it"
print "click Locate to list the point, then Flag to flag them"
print "the plot will be redrawn with an appropriate scale"
print "do more flagging if necessary"
print ""

# Pause script if you are running in scriptmode

if scriptmode:
    user_check=raw_input('happy with flagging? Hit Return to continue script\n')

print "It is a good idea to save the flagging at certain times"
print "First, list the flag versions using flagmanager"

#=====================================================================
# Save flagging done up to this point
#=====================================================================
#
print ""
print "--flagmanager--"
print ""
print "first, list the current flag versions"
print ""

# first we list the current flagging tables

mode='list'

flagmanager()

print ""
print "then, we save the flagging we just did"

# then we save the flagging we just did

mode='save'
versionname='afterplotxy'
comment='flags after running plotxy'
merge='replace'

flagmanager()

# and now we list one more time to show the changes made

print ""
print "then, list one more time to show the changes"


mode='list'

flagmanager()

print "now we will determine the antenna gains"
print ""

#=====================================================================
# Determine antenna gains
#=====================================================================
#
print "--gaincal--"
print ""
print "creates user defined table containing gain solutions"
print "once for each calibrator since uv ranges are different."
print "Note: append is False first, then True"
print ""

default('gaincal')
vis        = msfile
caltable   = prefix + '.gcal'
field      = '1'
spw        = '0:5~112'
selectdata = True
uvrange    = '0~40klambda'
solint     = 'inf'
combine    = ''
append     = False

print "starting field 1"
print ""

gaincal()

field='2'
uvrange='0~20klambda'
append=True

print "starting field 2"
print ""

gaincal()


field='3'
uvrange='0~50klambda'

print "starting field 3"
print ""

gaincal()

field='4'
uvrange=''

print "starting field 4"
print ""

gaincal()


print ""
print "next, we will plot some of the antenna gains"
print ""

#=====================================================================
# Plot antenna gains
#=====================================================================
#
print "--plotcal--"
print ""
print "first we plot the amplitude gains for antennas 9 - 12"

default('plotcal')
caltable= prefix + '.gcal'
xaxis='time'
yaxis='amp'
field='2'
antenna='9~12'

plotcal()

print ""
# Pause script if you are running in scriptmode
if scriptmode:
    user_check=raw_input('Return to continue script\n')

print "then, we plot R and L just for antenna 9 in separate plots on"
print "the same page.  Note use of the subplot parameter"

yaxis='phase'
antenna='9'
poln='R'
subplot=211
plotcal()
poln='L'
subplot=212
plotcal()

print ""
print "Next, we will determine the flux of 0841+708"

print ""
# Pause script if you are running in scriptmode
if scriptmode:
    user_check=raw_input('Return to continue script\n')


#=====================================================================
# Bootstrap flux of 0841+708
#=====================================================================
#
print "--fluxscale--"
print ""
print "determines flux based on gains and flux calibrator fluxes"
print "see Log window for flux value found"
default('fluxscale')
vis=msfile
caltable= prefix + '.gcal'
fluxtable=prefix + '.fcal'
reference='1,3~4'
transfer='2'

fluxscale()

print ""
print "Next, we will determine bandpasses for the three bandpass calibrators"

print ""


#=====================================================================
# Solves for bandpass, writes it to table
#=====================================================================
#
print "--bandpass--"
print ""
print "determine bandpass"

default('bandpass')

vis      = msfile
caltable = prefix + '.bcal'
field    = '1,3~4'
solint   = 'inf'
combine  = ''
solnorm  = True

bandpass()

print "Next we plot bandpass solutions for a few selected antennas;"
print ""

#=====================================================================
# Plot bandpass
#=====================================================================
#
print "--plotcal--"
print ""
print "First we plot solutions for antennas 9-12 for field 1 only"

default('plotcal')

vis=msfile
caltable= prefix + '.bcal'
xaxis               =     'chan'
yaxis               =      'amp'
field               =        '1'
antenna             =     '9~12'
plotrange           = [-1, -1, 0.9, 1.15]

plotcal()

print ""
# Pause script if you are running in scriptmode
if scriptmode:
    user_check=raw_input('Return to continue script\n')
print ""

field='1,3~4'
antenna='25'
iteration='field'

plotcal()

print "we iterate over all three fields, just for antenna 25 using"
print "the iteration parameter"
print ""

print "Make sure to click Next to go through the fields before hitting return"
print ""
print "note the galactic absorption in the first two fields"
print "only field 4 (1331+305) is free of absorption"
print "for now, we will use only bandpass solutions for field 4"
print ""
# Pause script if you are running in scriptmode
if scriptmode:
    user_check=raw_input('Return to run applycal\n')


#=====================================================================
#Apply calibration - results go to corrected_data column
#=====================================================================
print "--applycal--"
print ""
print "applies supplied calibration tables (gain and bandpass) and"
print "writes corrected data to column labeled CORRECTED_DATA.  This"
print "may take a while ..."

default('applycal')

vis=msfile
spw='0:5~112'
gaintable=[prefix+'.fcal',prefix+'.bcal']
gainfield=['1', '4']

applycal()

print ""
print "Next, we will Fourier transform the corrected data using clean"

#=====================================================================
# Image a few channels
#=====================================================================
#
print "--clean--"
print ""
print "Fourier transforms data in column labeled CORRECTED_DATA"
print "and creates an output image and beam"
print ""
print "For now, we won't clean, and to make things go faster we image"
print "two channels only"
print ""

default('clean')

vis       = msfile
imagename = prefix
field     = '0'
spw       = '0:5~112'
mode      = 'channel'
nchan     = 2
start     = 50
width     = 5
niter     = 0
imsize    = [512,512]
cell      = ['4arcsec','4arcsec']


clean()



#=====================================================================
# Inspect the result of clean
#=====================================================================
#
print "--viewer--"
print ""
print "allows viewing both visibilities and images. Here: image"
print ""
print "Here we inspect the two channels we just created"
print ""

default('viewer')

infile=prefix+'.image'

viewer()

print "note that HI emission is visible, but that there is a structure"
print "superposed suggesting bad data.  Maybe using the channel average for"
print "flagging was not enough.  Sometimes data can be bad in individual"
print "channels but averaged out if the phases are random"
print ""
print "please exit viewer when done"
print ""
print "next, we will run plotxy again but now using only one channel"

print ""
# Pause script if you are running in scriptmode
if scriptmode:
    user_check=raw_input('Return to continue script\n')


#=====================================================================
# Use plotxy again, but now on channel 50 only 
#=====================================================================
#
print "--plotxy  --"
print ""
print "plots visibility data"
print ""

execfile('plotxy.last')

spw='0:50'
correlation=''
width='1'

plotxy()

print "Note that bad data not flagged previously now show up.  Don't"
print "flag these points interactively; instead, use the Locate button"
print "to see what's going on.  Turns out, there are some bad time stamps"
print "in both RR and LL, and a few in just LL.  In addition, antenna 0"
print "(LL) is suspect throughout the observation"
print ""
print "When done playing around with the CASA plotter, we will use the"
print "non-interactive task flagdata to flag these data"
print ""
print "first: the bad timeranges:"
print ""
# Pause script if you are running in scriptmode
if scriptmode:
    user_check=raw_input('Return to continue script\n')

#=====================================================================
# Flag data non-interactively
#=====================================================================
#
print "--flagdata--"
print ""
print "flags data based on a number of criteria"
print ""
print "first we flag the narrow time range around 03:53 for both correlations"
print ""

default('flagdata')

vis=msfile
spw='0'
timerange='03:52:44~03:52:46'
mode='manualflag'

flagdata()

print ""
print "then, we flag antenna 0 for correlation LL over the whole time range"
print ""


antenna='0'
correlation='LL'
timerange=''

flagdata()

print ""
print " done with flagdata, run clean again to see if things improved"
print " this time, image over the full range of channels with line emission"
print ""

# Pause script if you are running in scriptmode
if scriptmode:
    user_check=raw_input('Return to continue script\n')

# first delete previously made images

os.system('rm -rf '+prefix+'.image')
os.system('rm -rf '+prefix+'.residual')
os.system('rm -rf '+prefix+'.model')

#=====================================================================
# Image selected channels after latest flagging
#=====================================================================
#
print "--clean--"
print ""

execfile('clean.last')

spw      = '0:5~112'
nchan    = 55
start    = 32
width    = 1

clean()

print ""
print "clean finished; now run the viewer again to see if things look better"
print ""


#=====================================================================
# Inspect to see if the latest flagging made things better
#=====================================================================
#
print "--viewer--"
print ""

execfile('viewer.last')

viewer()

print ""
print "This marks the end of the second script.  Thanks for hanging in"
print "there.  The next script performs continuum subtraction in the uv"
print "plane and imaging with cleaning"
