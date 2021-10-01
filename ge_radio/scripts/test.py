##########################################################################
#                                                                        #
# Created by KWC 01/2009                                                 #
#                                                                        #
#    Input Data           Process          Output Data                   #
#                                                                        #
# AM0962_2          --> importvla  -->  <prefix>.ms   +                  #
#   (P-band,               |            <prefix>.ms.flagversions         #
#    2 SPW,                v                                             #
#    A-config)          listobs    -->  casapy.log                       #
#                          |                                             #
#                          v                                             #
#                        setjy                                           #
#                          |                                             #
#                          v                                             #
#                       gaincal    -->  <prefix>.gcal                    #
# fluxscale
# bandpass
# applycal
# clean
# split
# --selfcal--
#   widefield
#   gaincal
#   applycal
# --iterate--
# exportfits
# imstat
# immoments
#
##########################################################################

TODO:
    need to dump info to log file
    need to iterate over spw, corr combos correctly
    make arrays of:
        field name, ID combos

# Setup some useful env variables
import time
import os
benchmarking = T
scriptmode = T
pathname = os.environ.get('AIPSPATH').split()[0]

# Set up some useful variables
myvers = casalog.version()
myuser = os.getenv('USER')
myhost = os.getenv('HOST')
mycwd  = os.getcwd()
myos   = os.uname()
prefix = raw_input('Enter file prefix to use (e.g. night2): ')
afiles = raw_input('Enter archive files to use (e.g. \'AM0962_1\',\'AM0962_2\', etc): ')
oband  = raw_input('Enter name of observed band (e.g. P): ')
msfile = prefix + '.ms'
print "MS will be called "+msfile

# Clean up old versions of files to be created in this script
os.system('rm -rf '+prefix+'.ms*')

# Start benchmarking
if benchmarking:
    startTime = time.time()
    startProc = time.clock()

# Import the data from VLA archive files to MS
print "# STATUS: Importing data"
print "# Using importvla to read VLA archive files and"
print "# write the data into a Measurement Set (MS)."
print ""
default('importvla')
importvla(archivefiles=[afiles], vis=msfile, bandname=oband)
saveinputs('importvla', prefix+'_'+taskname+'.save')
print "# STATUS: Import complete"
print ""

# List a summary of the MS
print "# STATUS: Listing MS info"
print "# Using listobs to print verbose summary to logger;"
print "# check logger window for output"
listobs(vis=msfile, verbose=True)
print "# STATUS: Complete"
print ""

# make a figure of the vis, ant config, and uv coverage
print "# STATUS: Plotting array info"
print "# Making figure of:"
print "#     visibility versus channel"
print "#     antenna array configuration"
print "#     uv coverage"
default('plotxy')
plotxy(vis=msfile, xaxis='channel', datacolumn='data', field='0', subplot=211, plotcolor='', plotsymbol='go', interactive=False, selectplot=True, markersize=2.5, title=msfile+' Visibility')
plotxy(vis=msfile, xaxis='x', field='0', subplot=223, plotsymbol='r^', interactive=False, selectplot=True, title='Antenna Configuration')
plotxy(vis=msfile, xaxis='u', yaxis='v', field='0', subplot=224, plotsymbol='b', interactive=False, figfile=prefix+'_vis.png', selectplot=True, title='UV Coverage')
print "# STATUS: Complete"
print ""

# Determine reference antenna
print "# STATUS: Opening "+prefix+"_vis.png"
os.system('eog '+prefix+'_vis.png')
print "# Find antennas near center of array"
user_check = raw_input('When done, hit Return to continue script...')
print ""

# Check ref ant for good data
print "# STATUS: Checking possible reference antennas for good observing"
print "# Need to select antenna that collected good data in all scans, spw, corr"
trefant = raw_input('Enter first possible refant (e.g. VA06): ')
while trefant != "":
    plotxy(x=time, y=amp, selectdata=T, antenna=trefant)
    plotxy(x=channel, y=amp, selectdata=T, antenna=trefant)
    trefant = raw_input('Enter first possible refant (e.g. VA06): ')
refant = raw_input('Which antenna shall be the reference antenna (e.g. EA28): ')
print ""

# Clean up old verions of files to be created in this script
print "# STATUS: Removing old files with prefix \'"+prefix+"\'"
print ""
print "*** WARNING WARNING WARNING ***"
print "*** WARNING WARNING WARNING ***"
print ""
delold = raw_input('Really? Delete old files? (yes/no): ')
print ""
print "*** WARNING WARNING WARNING ***"
print "*** WARNING WARNING WARNING ***"
print ""
if delold == "yes":
    os.system('rm -rf '+prefix+'.psf'+'*')
    os.system('rm -rf '+prefix+'.flux'+'*')
    os.system('rm -rf '+prefix+'.residual'+'*')
    os.system('rm -rf '+prefix+'.model'+'*')
    os.system('rm -rf '+prefix+'.?cal'+'*')
else:
    print "# You have chosen to leave old files lying around."
    print ""

# Undo any flagging that may still be lying around
print "# STATUS: Removing old flagging"
print "# Undoing any previous flagging with flagmanager"
default('flagmanager')
flagmanager(vis=msfile, mode='restore', versionname='Original')
print "# STATUS: Complete"
print ""

# Fill the model column for flux density calibrators
print "# STATUS: Setting flux density"
print "# Find the flux of the flux calibrator, and write it to the"
print "# MS column labeled MODEL_DATA"
default('setjy')
calfield = str(raw_input('Enter field ID of gain calibrator (e.g. 0): '))
setjy(vis=msfile, field=calfield, spw='')
print "# Record Stokes\' coeffs from logger window."
print "# Check cal manual, are they correct?"
print ""

# Data flagging
print "# STATUS: 3D flagging using viewer"
print ""
print "*** NB NB NB***"
print "*** NB NB NB***"
print ""
print "Right now only flag calibrator data,"
print "source data flagging comes after calibration."
print ""
print "Be sure to write down bad time intervals, antenna(e), channels, scans, etc."
print "The flagdata tool will run after viewer to aide in"
print "flagging data which is too tedious to flag in viewer." 
print ""
print "*** NB NB NB***"
print "*** NB NB NB***"
print ""
print "# STATUS: Starting viewer for data editing..."
print "# Be sure to go through each combination of:"
print "#     calibrator field"
print "#     correlation"
print "#     spw"
print "# These fields are set under \'MS and Visbility Selection\'"
viewer(infile=msfile, displaytype='raster')
print "# Set X-axis to Baseline"
print "# Set Y-axis to Time"
print "# Set Animation axis to Channel"
print "# Now press \'play\' and watch for bad channels."
print "# Flag channels, antenna(e), time, etc."
print "# When happy, under \'Flagging Options\' hit \'Save edits\'."
print "# Now, change the correlation/spw/field combination under \'Display Axes\' and repeat."
print "# Close viewer when all done"
print ""

# Save flags
print "# STATUS: Saving flags"
print "It's a good idea to save flags often, leaving restore points."
vname = raw_input("Enter flag version name (e.g. afterviewer): ")
fcom  = raw_input("Enter comments for flag version (e.g. \'hooray, flags\'): ")
flagmanager(vis=msfile, mode='save', versionname=vname, comment=fcom)
print "# STATUS: Complete"
print ""

# More data flagging
print "# STATUS: 2D flagging using plotxy"
print ""
print "*** NB NB NB***"
print "*** NB NB NB***"
print ""
print "Right now only flag calibrator data,"
print "source data flagging comes after calibration."
print ""
print "Be sure to write down bad time intervals, antenna(e), channels, scans, etc."
print "The flagdata tool will run after viewer to aide in"
print "flagging data which is too tedious to flag in viewer." 
print ""
print "Flags do not need to be coherent! If data in one spw or correlation or"
print "channel are not flagged in another, that is okay. E.g. if one antenna"
print "is dead and it gets completely removed, it does not need to be removed"
print "in all spw, corr, or time. For imaging, the flags are cross-checked,"
print "so if one corr RR is flagged in one spw but not the other, data in"
print "both spw will be removed."
print ""
print "*** NB NB NB***"
print "*** NB NB NB***"
print ""
print "# STATUS: Starting plotxy for more data editing..."
print "# Click \'Mark Region\', then draw a rectangle around the bad data."
print "# Click \'Locate\' to list the points and \'Flag\' to flag them."
print "# After \'Flag\', the plot will be redrawn with the appropriate scale."
print ""
calid = raw_input("Enter a calibrator field ID - leave blank to end: ")
while calid != "":
    uspw = raw_input("Enter a spw - leave blank to move on: ")
    while uspw != "":
        ucorr = raw_input("Enter a corr - leave blank to move on: ")
        while ucorr != "":
            plotxy(vis=msfile, field=calid, yaxis='amp', xaxis='time', selectdata=T, correlation=ucorr, spw=uspw)
            plotxy(vis=msfile, field=calid, yaxis='amp', xaxis='channel', selectdata=T, correlation=ucorr, spw=uspw)
            ucorr = raw_input("Enter a corr - leave blank to move on: ")
            uspw = raw_input("Enter a spw - leave blank to move on: ")
            calid = raw_input("Enter a calibrator field ID - leave blank to end: ")
print "# STATUS: Complete"
print ""

# Save flags
print "# STATUS: Saving flags"
print "It's a good idea to save flags often, leaving restore points."
vname = raw_input("Enter flag version name (e.g. afterviewer): ")
fcom  = raw_input("Enter comments for flag version (e.g. \'hooray, flags\'): ")
flagmanager(vis=msfile, mode='save', versionname=vname, comments=fcom)
print "# STATUS: Complete"
print ""

#########################################################
#########################################################
#########################################################
#########################################################
#########################################################
#########################################################

flagdata(vis="night4.ms", mode="manualflag", antenna="VA27", spw="1",
correlation="LL", field="0", uvrange="",
timerange="2008/12/23/10:37:44~2008/12/23/10:37:46", scan="", feed="",
array="", clipexpr="ABS RR", clipminmax=[], clipcolumn="DATA",
clipoutside=True, quackinterval=0.0, autocorr=False, unflag=False,
algorithm="timemed", column="DATA", expr="ABS RR", thr=5.0, window=10)

save flags
get good spw from user
get minsnr from user

gaincal(vis=msfile, caltable=prefix+".gcal", field="0,1", spw="",
selectdata=False, timerange="", uvrange="", antenna="", scan="",
msselect="", solint="inf", combine="", preavg=-1.0, refant=myrefant,
minsnr=0.0, solnorm=False, gaintype="G", calmode="ap", append=False,
splinetime=3600.0, npointaver=3, phasewrap=180.0, gaintable=[''],
gainfield=[''], interp=['linear'], spwmap=[], gaincurve=False,
opacity=0.0, parang=False)

run listcal for check of amp and phase
run plotcal

fluxscale(vis=msfile, caltable=prefix+".gcal",
fluxtable=prefix+".fcal", reference=refid, transfer="", append=False,
refspwmap=[-1])

record spw flux dens for calibs and check vs. cal man
get fillgaps size

bandpass(vis=msfile, caltable=prefix+".bcal", field=refid, spw="",
selectdata=False, timerange="", uvrange="", antenna="", scan="",
msselect="", solint="inf", combine="", refant=myrefant, solnorm=False,
bandtype="B", append=False, fillgaps=0, degamp=3, degphase=3,
visnorm=False, maskcenter=0, maskedge=0, gaintable=[prefix+".fcal"],
gainfield=[refid], interp=['linear'], spwmap=[], gaincurve=False,
opacity=0.0, parang=False)

run plotcal

** order of params **
field = [phase_id, src_id]
gainfield = [phase_id, '*']
because there should be N entries in the fcal table for each phase calib
and only one entry in the bcal table

applycal(vis=msfile, field="1, 2", spw="", selectdata=False,
timerange="", uvrange="", antenna="", scan="", msselect="",
gaintable=[prefix+'.fcal', prefix+'.bcal'], gainfield=['1', '*'],
interp=['linear', 'nearest'], spwmap=[], gaincurve=False, opacity=0.0,
parang=False, calwt=True)

applycal(vis=msfile, field="0", spw="", selectdata=False,
timerange="", uvrange="", antenna="", scan="", msselect="",
gaintable=[prefix+'.fcal', prefix+'.bcal'], gainfield=['0', '*'],
interp=['linear', 'nearest'], spwmap=[], gaincurve=False, opacity=0.0,
parang=False, calwt=True)

run plotxy on corrected data per field for chan vs amp, baseline vs amp, time vs amp
flag bad data

*****split src field*****

+ P-band, A-config primary beam -> 9000", 150', 2.5deg
+ Want to oversample resolution by factor of 3-5
+ 4.5-6.0" resolution
5x: 0.9-1.2"/cell ==> 10000-7500 pix/side
4x: 1.125-1.5"/cell ==> 8000-6000 pix/side
3x: 1.5-2"/cell ==> 6000-4500 pix/side
1.25"/cell => 7200 pix/side => takes 35 min

# make low res image of HUGE field with widefield
# run self-cal once
# select bright sources for outlier file to feed widefield
# make outlier file

--------------------------------
--------------------------------
       SELF-CALIBRATION
--------------------------------
--------------------------------

widefield(vis="night2.NGC1316.split.ms", imagename=['selfcal'],
                  outlierfile="", field="0", spw="0~1:2~28",
                  selectdata=False, timerange="", uvrange="",
                  antenna="", scan="", mode="mfs", niter=200,
                  gain=0.1, threshold=0.0, psfmode="clark",
                  ftmachine="wproject", facets=1, wprojplanes=400,
                  multiscale=[], negcomponent=-1, interactive=False,
                  mask=[], nchan=-11, start=0, width=1, imsize=[7200,
                  7200],cell=['1.25arcsec','1.25arcsec'],
                  phasecenter=['0'], restfreq="", stokes="I",
                  weighting="briggs", robust=0.0, npixels=0,
                  noise="1.0Jy", cyclefactor=1.5, cyclespeedup=-1,
                  npercycle=100, uvtaper=False, outertaper=[],
                  innertaper=[], restoringbeam=[''])

gaincal(vis="night2.NGC1316.split.ms", caltable="selfcal.gcal",
                field="0", spw="0~1:2~28", selectdata=False,
                timerange="", uvrange="", antenna="", scan="",
                msselect="", solint="60s", combine="", preavg=-1.0,
                refant="VA06", minsnr=0.0, solnorm=False,
                gaintype="G", calmode="p", append=False,
                splinetime=3600.0, npointaver=3, phasewrap=180.0,
                gaintable=[''], gainfield=[''], interp=['linear'],
                spwmap=[], gaincurve=False, opacity=0.0, parang=False)

plotcal

applycal(vis="night2.NGC1316.split.ms", field="0", spw="0~1:2~28",
                 selectdata=False, timerange="", uvrange="",
                 antenna="", scan="", msselect="",
                 gaintable=['selfcal.gcal'], gainfield=['*'],
                 interp=['nearest'], spwmap=[], gaincurve=False,
                 opacity=0.0, parang=False, calwt=True)

--------------------------------
--------------------------------

When satisfied, run gaincal w/ mode='ap'

gaincal(vis="night2.NGC1316.split.ms", caltable="selfcal.gcal",
                    field="0", spw="0~1:2~28", selectdata=False,
                    timerange="", uvrange="", antenna="", scan="",
                    msselect="", solint="60s", combine="",
                    preavg=-1.0, refant="VA06", minsnr=0.0,
                    solnorm=False, gaintype="G", calmode="ap",
                    append=False, splinetime=3600.0, npointaver=3,
                    phasewrap=180.0, gaintable=[''], gainfield=[''],
                    interp=['linear'], spwmap=[], gaincurve=False,
                    opacity=0.0, parang=False)

applycal(vis="night2.NGC1316.split.ms", field="0", spw="0~1:2~28",
                     selectdata=False, timerange="", uvrange="",
                     antenna="", scan="", msselect="",
                     gaintable=['selfcal.gcal'], gainfield=['*'],
                     interp=['nearest'], spwmap=[], gaincurve=False,
                     opacity=0.0, parang=False, calwt=True)

exportfits
imstats
immoments
