# This csh scripts demonstrates on how to use WVT binning to create temperature
# maps. The procedure outlined in here is not restricted to temperatures, but
# can be used for all types of spectral fitting parameters in sherpa. The
# modifications that have to be done should be self-explanatory.

# The sherpa script that I am using requires a background spectrum of the 
# name mark_bgd.pi (Markevitch background file). In my case this background
# file was identified by the CIAO script "acis_bkgrnd_lookup", reprocessed
# to match the gain file used in the observation and had the sky coordinates
# reprojected. Since all of this is hardwired into my set of routines, I 
# have not included the script to create the file. Please follow the 
# CIAO guidelines to create your background file.
# If you already have your background file, or the region on the chip 
# containing your local background, the essential piece of code you need 
# in order to create the spectrum is simply the following:
# dmextract infile="markevitch_bgfile.fits[bin pi]" outfile=mark_bgd.pi

# 0) Set some variable that you need later
# Final event2 file
setenv EVT2FILE         evt2.fits
# Mask, 2d image specifying the area for analysis (1 for good and 0 for bad)
setenv MASKFILE         mask.fits
# Sky coordinates to derive an image from, must match the mask file
setenv XMIN             3400
setenv XMAX             4500
setenv YMIN             3700
setenv YMAX             4800

# 1) Copy the ciao.csh file to the current directory and execute
source ciao.csh -o

# 2) Copy the ciao script acis_set_ardlib to the primary directory and execute
chmod +x acis_set_ardlib
punlearn ardlib
./acis_set_ardlib

# 4) Copy the counts image into the tempmap directory, e.g.
dmcopy "../${EVT2FILE}[bin x=${XMIN}:${XMAX}:1,y=${YMIN}:${YMAX}:1]" cts.fits clobber=yes verbose=2

# 5) Start IDL batch file wvt_temperaturemap
idl wvt_temperaturemap
# This script should do the following:
# a) Adaptively bin the image, with the specified number of counts per bin.
# b) Extract an event list for each bin from the evt2 file that you specified. 
#    The evt2 sublists will be named "evt2.fits.XXX". In addition, each file 
#    will have an ascii file "evt2.fits.XXX.BACKSCAL" associated with it, 
#    which contains the normalization parameter BACKSCAL for sherpa.
# c) A file called nbins.dat which simply contains the number of bins. This is
#    needed for the csh file to know when to stop iterating later.


# 6) Start the csh-script tempmap_generic.csh. If you decide to only use one
#    rmf and arf file for the whole chip, i.e. you want to neglect the spatial
#    dependence of the instrument response, create a file called "use_one_rmf":
touch use_one_rmf
# If this is not the case, the script will create a new rmf file for each bin.
# For a large number of bins, this will be very time consuming!
./tempmap_generic.csh


# 7) Evaluate the output to create a temperature map
idl wvt_evaltemperaturemap








