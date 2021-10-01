# FITLD: Loads the data file into AIPS. 
# INDXR: Indexes the tables in the data files loaded in AIPS. 
# LISTR: Lists the sources observed as tabulated in the data files. 
# SETJY: Sets the absolute flux level of the flux calibrator. 
# TVFLG: Plot baseline vs time
# UVFLG: Used for flagging the antennae and base lines identified from the output of TVFLG. 
# CALIB: Calibrate the observed flux with the theoretical flux. 
# SNPLT: Plots the SN table produced by CALIB. 
# EXTDES: Destroys the extension tables (to avoid confusion we need to destroy improper tables)) 
# GETJY: Obtains the flux level of the phase calibrator. using the SN tables obtained from CALIB. 
# CLCAL: Actually applies the calibration. 
# UVPLT: Plots the UV data in various ways. The next step is not taken until the results of UVPLT is clean and satisfactory. 
# BPASS: Creates the band pass table. 
# POSSM: Displays the results of the band pass table. Used to identify the bad channels. 
# SPLAT: Applies the bandpass and creates the band pass averaged `FITS' file for the whole observation. 
# SPLIT: Splits the original data into individual sources 
# IMAGR: Creates the map.

# Setup some useful env variables
import os
scriptmode = T

# User input
gfits  = raw_input('Enter name of FITS files: ')
gflags = raw_input('Enter name of flags file: ')
prefix = raw_input('Enter MS table prefix: ')

# define some variables
msfile = prefix + '.ms'
btable = prefix + '.bcal'
gtable = prefix + '.gcal'
ftable = prefix + '.fcal'
splitms = prefix + '.src.split.ms'
imname = prefix + '.cleanimg'

# import data
print '## STATUS: Importing GMRT data'
default('importgmrt')
importgmrt(fitsfile=gfits,
           flagfile=gflags,
           vis=msfile)
print "## STATUS: Import complete"

# look at contents of ms and available antennas
listobs(msfile, verbose=True)
fluxcal = raw_input('Enter flux cal fields: ')
phascal = raw_input('Enter phase cal fields: ')

# Set the flux density of calibrator
print '## STATUS: Flux density of flux calibrator'
default('setjy')
setjy(vis=msfile, field=fluxcal)
print "## STATUS: Setjy complete"

# Auto flag as best one can
print '## STATUS: Autoflagging... get a beer'
default('flagdata')
flagdata(vis=msfile, mode='rfi', clipexpr='RR', flag_level=2)
flagdata(vis=msfile, mode='rfi', clipexpr='LL', flag_level=2)
flagdata(vis=msfile, mode='rfi', clipexpr='RR', flag_level=2)
flagdata(vis=msfile, mode='rfi', clipexpr='LL', flag_level=2)
flagdata(vis=msfile, mode='rfi', clipexpr='RR', flag_level=2)
flagdata(vis=msfile, mode='rfi', clipexpr='LL', flag_level=2)
print "## STATUS: Flagging complete"

# By hand flagging
# Pick reference antenna, RFI free channels
plotms()
viewer()

!!refant, goodchans

# Calibrate the bandpass
print '## STATUS: Calibrating bandpass'
os.system('rm -rf *bcal*')
default('bandpass')
bandpass(vis=msfile,
         caltable=btable,
         field=fluxcal,
         spw='',
         selectdata=F,
         solint='inf',
         combine='scan',
         minsnr=1.0,
         refant='0',
         bandtype='B',
         gaintable='',
         gainfield='',
         gaincurve=F)
print "## STATUS: Bandpass complete"

# Plot the bandpass solutions
default('plotcal')
plotcal(caltable = btable,
        field = fluxcal,
        yaxis = 'amp',
        iteration='antenna',
        showgui = True)
plotcal(caltable = btable,
        field = fluxcal,
        yaxis = 'phase',
        iteration='antenna',
        showgui = True)

# Calibrate the time-dependent gains
# Run only on the flux calibrator(s) and phase calibrator(s)
print '## STATUS: Calibrating gains...'
os.system('rm -rf *gcal*')
default('gaincal')
gaincal(vis = msfile,
        caltable = gtable,
        gaintable = btable,
        gainfield = '',
        interp = 'nearest',
        field = fluxcal+phascal,
        spw = goodchans,
        selectdata = False,
        gaincurve = False,
        opacity = 0.0,
        gaintype = 'G',
        solint = 'inf',
        combine = '',
        calmode = 'ap',
        minsnr = 1.0,
        refant = '0')
print "## STATUS: Gain calibration complete"

# Bootstrap the flux scale for all calibrators
print "## STATUS: Setting flux scale..."
os.system('rm -rf *fcal*')
default('fluxscale')
fluxscale(vis = msfile,
          fluxtable = ftable,
          caltable = gtable,
          reference = <fluxed cals>,         
          transfer = <unfluxed cals>)
print "## STATUS: Flux scaling complete"

# inspect the gain solutions
default('plotcal')
plotcal(caltable = ftable,
        field = fluxcal+phascal,
        yaxis = 'amp',
        iteration='antenna',
        showgui = True)
default('plotcal')
plotcal(caltable = ftable,
        field = fluxcal+phascal,
        yaxis = 'phase',
        iteration='antenna',
        showgui = True)

# Correct the calibrators using themselves
# and transfer from phase to itself and the targets
default('applycal')
applycal(vis = msfile,
         gaintable = [ftable,btable],
         gainfield = [<reffield?>,'*'],
         interp = ['linear','nearest'],
         spwmap = [],
         spw = '',
         selectdata = False,
         gaincurve = False,
         opacity = 0.0,
         field = <sources and phascals>)

# apply to itself
default('applycal')
applycal(vis = msfile,
         gaintable = [ftable,btable],
         gainfield = [fluxcal,'*'],
         interp = ['linear','nearest'],
         spwmap = [],
         spw = '',
         selectdata = False,
         gaincurve = False,
         opacity = 0.0,
         field = fluxcal)

# dirty image
default('clean')
clean(vis = msfile,
      imagename = imname,
      field = srcs,
      spw = goodchans,
      mode = 'channel',
      nchan = 1,
      start = 5,
      width = 20,
      niter = 0,
      imsize = XXX,
      cell = 'Xarcsecs')
