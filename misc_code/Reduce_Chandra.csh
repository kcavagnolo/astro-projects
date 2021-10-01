#!/bin/tcsh -f

# Reduce_Chandra IX (RUNALL v 6.0.2)
# D.S.Hudson 05-08-29


# 		     History
# 05-08-29 Changed placed where Chandra_Tools are stored.  Also allow bias files to be in secondary
#	   or supporting
#
# 05-08-12 Fixed bug that streaked non-vf mode would never produce a new level-1 event file.  Actually it produced
#          it, but the name never got changed.  This bug was introduced when I changed when the badpixel filtering
#          was done.  Basically, on destreaked non-vf mode there was no need to reprocess the data, therefore
#          "tmp/${obj}_dstrk_clean_evt1.fits" was never changed to "tmp/${obj}_new_evt1.fits" 
#
#
# 05-08-03 Fixed a bug in the line:
#	   fdelrow infile=${obj}_${CCD}_evt2_goodgti.fits+${GTI[$j]} firstrow=${frow} nrows=${drow} confirm=N proceed=Y
#          it gives an error if you put
#          fdelrow infile=${obj}_${CCD}_evt2_goodgti.fits+${GTI[$j]} firstrow=${frow} nrows=${drow} N Y
#
# 05-08-02 Fixed an error where I was not filtering badpixels before destreaking
#	   this is wrong.  The badpixels should be filtered before destreaking
#          I don't know if this used to be the other way and it changed, or
#          if it was always this way and I misunderstood.  The VF processing
#          definitely comes after...that's why Markevitch, removes bit 23
#          creates a new column of zeros..destreaks and then moves the old
#          column back.  I will just run acis_process_events with check_vf_pha=no
#	   and then re-run it with yes.
#
# 05-07-27 For a single Chip you should be able to do dmcopy "evt2.fits[gti]" gti.fits
#	   but apparently it doesn't work (maybe because of Chips configuration)
#	   changed line to dmcopy "evt2.fits[gti${Chips}]" gti.fits
#	   Fixed a minor error for the OOTs in the single chip mode.
#	   I forgot to change to ../event_1
#
# 05-07-26 Fixed problem with GTIs
# 	   Originally I deleted the GTI extensions so that the
# 	   single one would be appended which would be for all Chips.  
# 	   This created problems because it was labeled as the I0 GTI 
# 	   (for the obs I was doing).  It changed all the EXPOSUR# LIVTIME# 
#	   ONTIME# keywords to 0.  This created problems when making 
#	   e.g. WMAPs which gave problems with mkacisrmf.  When you crank 
#          up the verbosity mkacisrmf gives a weighting factor of "inf" 
#	   for the CHIPS that have zero exposure time in the WMAP.
# 	   Here's the solution:
# 	   (1) Don't delete the GTI extensions
# 	   (2) filter with the new GTI
# 	   All the old GTIs will have the same number or MORE rows than the new GTI.
#          This is important because ftools lets you remove rows, but you can't add rows.
# 	   (3) Remove the rows from the GTI extensions that have more rows than the new GTI
# 	   (4) Remove the cols START and STOP from the GTI extensions
# 	   (5) ADD the the cols START and STOP from the new GTI file
# 	   (6) filter the file with itself.  This will give all Chips the 
#	       same livetime, exposure time, and ontime
#
# 	   I also made some minor adjustments.
#	   The uncleaned gti event file is the only one processed with the new gti
#	   then the point sources are removed from it to make the "clean" one
#          I was doing this in parallel.  Mainly because my original code didn't
#	   have an uncleaned gti events file...I created it in Reprocess_events	
#	   (at Perlman's request) I also realized it was useful.  So I added it in
#  	   this code as well.
#
#	   I also removed some comments about old sections that had been commented
#	   out because of software updates (e.g. tgain).  I also removed the creation
#	   of the cxc-evt2 file, which I was using simply to find the number of
#	   GTI extensions.  I don't know what I was thinking.  Any events file
#	   should have them.  So now I extract that info from my evt2 file.
#	   
#
# 05-07-25 Fixed OOT code so works the way I like.  Strange behavior when you
#  	   filter the chips.  It changes all the LIVETIME, ONTIME, and EXPOSURE
#	   keywords, but it doesn't change the GTI.  I reset them manually.
#	   Unlike the background I set ONTIME and EXPOSURE differently due to
#	   dead time correction.  This is consistent with the source events.
#          When I filter out the sources, this step is done by ciao (i.e.
#	   the gti is summed and put in the ONTIME keyword.  Then the EXPOSURE
#	   and LIVETIME keywords are set by multiplying the ONTIME by the
#	   DEADTIME correction.  
#
#
# 05-07-24 Made minor adjustments (e.g. rm TMPJNKtempl TMPJNKdata TMPJNKfits2)
#          Fixed some errors in the OOT code.  There was a problem extracting
#	   the exposure time since it was in exponential form.  Also I had
#	   extracted some perl code and forgot to change the @{ccd} to ${ccd}
#	   Changed how lightcurve filtering is done for multiple chips
#	   For a I0123 obs, I filter the eventfile through every gti this creates
#	   four identical GTIs which has only goodtimes for all chips.  Then
#	   I read this into lcclean in order to create a single gti for the entire
#	   observation...this is the method recommended by Markevitch.  The advantage
# 	   is your count rate is higher so you can use smaller bins and catch shorter
#          flares
#
#
# 05-07-22 Fixed a problem with how GTI filtering is done.  If you use
#          dmcopy "evt2.fits[@GTI_I0.fits]" evt2_gti.fits it filters all
#          chips.  The way around this is to add the extensions and then
#          filter the file with itself.  Also changed script to include
#	   un-point source filtered gti file.
#
# 05-07-21 Changed GTI filtering to use dmcopy.  Reduces number events to
#	   those within the GTI
#
# 05-07-20 Added Proper OOTs using Markevitch's Scripts make_readout_bg
#          I will leave the old OOT script in to deal with situations of
#          pile-up.  I have adopted Markevitch's script to accept 4 exposure
#	   times.  This allows me to create an OOT/Read-out Artifact File
#	   that has "pre-normalized" times 
#
# 05-03-16 Fixed error in OOT destreaking (Yes, I am analysing a
#	   cluster with a core so bright it appears to create OOTs)
#
# 05-03-10 Changed the default assumption that badpix production
#          works now.  That is, I now have obsverations from
#	   later than DS 7.4.0 so I can't assume DEFAULT or
#	   a given file means the new hotpixel software didn't
#	   work.  As of CIAO 3.2.1 the new hotpixel software
#	   seems to be fine, so I don't think this will be 
#	   a problem.
#
# 05-02-11 Added corrections of CIAO 3.2.1...now Badpixfiles
# 	   should be generated correctly....
#
# 05-01-31 Added section to remove acis-afterglow detection
#	  when new badpixel file is created
#
# 05-01-13 Fixed a minor error where the wrong calibration gain
#	   file was accessed.  The mistake was introduced when I 
#          added the acis after-glow detection when using "DEFAULT" 
# 	   bad pixels
#
#	 
# 05-01-11 Since there is a problem with the new CIAO 3.2 hot pixel
#	  software, I changed the back to acis after-glow detection
#	  when using "DEFAULT" bad pixels.
#
# 04-12-16 Updated to include new CIAO 3.2 hot pixel software
#
# 04-12-14 Updated script with new background files from Markevitch
#
# 04-10-26 Rewrote script to store files as done in XMM-Newton
#	   Scripts.  This makes the script less flexible but
#	   much more robust.
#
# 04-07-31 Added option to include Closed background
#       Also change to gain correction in acis_process_events
#
#
# 04-04-13 Reduce_Chandra (RUNALL v 2.0)
# 	Significantly changed processing
#	and files (Background, BADPIX, etc) used
#	Follows suggesetions by Markvetich's COOKBOOK
#	for Background.  Data only processed, No images
#	created.  More correct and versatile than 
#	RUNALL v 1.0 
#
# 04-03-26 RUNALL v1.0 written
#	Combined and updated scripts written by
#	D.S.Hudson & E.R.Tittley into single script
#	Processes Chandra data and creates images


if ( $#argv != 10 ) then
 cat <<EOF 

usage 
	Reduce_Chandra  \
	 object \
	 path_to_rawdata \
	 output_directory \
	 Background.region \
	 CCD.region \
	 CCDs \
	 BADPIX \
	 OOT-Destreak \
	 FI-Destreak \
	 Closed_BKG

This tool follows closely the COOKBOOK by Markevitch 
(http://cxc.harvard.edu/cal/Acis/Cal_prods/bkgrnd/acisbg/COOKBOOK)
and the Chandra diffuse emission thread.
(http://cxc.harvard.edu/ciao/threads/diffuse_emission/)


object 			is the name of the object

path 			is the path to the primary 
			and secondary event directories 
			obtained from CXC (e.g.)

output_directory 	is the directory where the 
			output will be put (Directories 
			are created there!)

Background.region 	is a region free of extended 
			emission that is used to 
			determine the background normalization.
			The background is normalized to the observation
			in the (9.5-12 keV).
			Draw a box around the entire Chip if
			you want to use all the emission.

CCD.region		is used to reduce the the size 
			of the image files but limiting
			them to the region of the selected CCDs.
			Basically draw a big box around all the 
			CCDs you want to include in analysis.  It
			need not be precise, but don't cut out emission
			err on the side of being to big if you are
			unsure

CCDs			Are the CCDs used for analysis.  The format is
			"0 1 2 3" for I0123, "7" for S3, etc.  The quotes
			are very important as it needs to be parsed as a
			single input value.  It is best to run the S Chips
			separately as the program may crash b/c of limited
			Background files.  You can always merge the images
			later.
			
BADPIX	 		The BAPIX file to be used in conjuction with
			the BADPIX used with the Background from Markevitch.
			CREATE  = Create a Badpix for the period of the obs.
			DEFAULT = Use the BADPIX which was included with the obs.
			NONE    = No additional BADPIX file
			<file>  = Use this file.
			CREATE  is recommened for obs before DS 7.4.0 (7.6.0 for VFAINT MODE).
			DEFAULT is recommened for obs DS 7.4.0 or after (7.6.0 for VFAINT MODE).
			Use dmkeypar evt1.fits ASCDSVER echo+ to determine which
			your set is.  Note the naming convention changed so that
			R4CU5UPD14 = DS 6.0.0

OOT-Destreak		Is there a bright source with Out-of-Time events AND Pileup? Destreaking 
			is needed for bright sources that have significant so-called Out-of-Time 
			events.  "Y" => do it, anything else means don't do it.  
			See http://cxc.harvard.edu/ciao/threads/acisreadcorr/1.gif
			for an example.  Provide a point source free region called 
			"destreak.reg", which is used to produce to fill in the 
			destreaked region and a file called "destreak.txt" that contains 
			ONLY the dx and dy (i.e. 5 20 ).
			See http://cxc.harvard.edu/ciao/threads/acisreadcorr/ for info.

FI-Destreak		Should the image be destreaked?  "Y" => do it, anything else 
			means don't do it.  Should be done for the S4 (CCD 8) Chip.
			
Closed_BKG		"Y" => include a closed background event file.  Only valid for
			EPOCH "D" ( after Dec-2000 ).  If you are not sure of the EPOCH
			of your observation, this script will determine it and reset this
			flag to "N" if you do not have an EPOCH "D" observation.
			The closed background is useful for observations that have strange
			local backgrounds (High NH, soft excess, etc...)


e.g.
Reduce_Chandra.csh \
 "A262" \
 "raw_data/2215/" \
 "PASS_01" \
 "bkg.reg" \
 "CHIP_I0123.reg" \
 "0 1 2 3" \
 "CREATE" \
 "N" \
 "Y" \
 "Y"

EOF
 exit
endif


cat <<EOF




		  Reduce_Chandra (RUNALL v 2.6.2)
		  05-08-29




#############################################################
##################   W A R N I N G !!!!! ####################
#############################################################
# This is a very powerful tool that is very easy to misuse! #
# USE WITH CAUTION.  It is an automated tool for running all#
# the scripts at once. It assumes that there are NO problems#
# along the way.  It DOES create  intermediate products, so #
# please check them; especially the light curves, the point #
# sources, and after-glow events (for VFAINT obs).	    #		
#							    #
#							    #
# The author assumes no responsibility for this tool or its #
# misuse.						    #
# Comments, suggestions, and improvements are welcome.	    #
#############################################################

Danny Hudson
dhudson@astro.uni-bonn.de
2005-AUG-29


EOF

###################### VARIABLES ################################
#								#
# obj        = Object  						#
# pathe      = path to cxc data (primary/secondary events)	#
# top	     = path to output directories and data		#
# bkgreg     = ds9 region used to determine bkg normalization	#
# ccdreg     = ds9 region to reduce memory usage by only taking	#
#	       relevant space.					#
# Chips      = The CCDs used 0 1 2 3 etc....			#
# badpixfile = File used (with Markevitch's) to determine	#
#	       badpix to exclude				#
# OOT	     = Flag to determine if there is a bright source    #
#	       causing Out-of-Time events			#
# destreak   = A flag to determine if a obs should be 		#
#	       "destreaked"					#
#								#
#								#
# local_tools = Path to local tools and files from M Markevitch #
#		A Vikhlinin & D.S.Hudson			#
# filter_bits = The bits that should be filtered by dmcopy      #
#		when making a new event-2 file			#
# local_dir   = The absolute path to the local directory	#
# first_char  = The character preceding "/" in an input file.   #
#		If it is empty then the absolute path must	#
#		have been given, otherwise it was a relative	#
#		path.						#
# gzip_check  = All the files in a folder that contain bpix,    #
#		evt1, asol1, etc. That end in gz.  If it is NOT #
#		empty then the files must be unzipped		#



				#### I. SET THE VARIABLES ####
set obj 	=   $1
set pathe 	=   $2
set top 	=   $3
set bkgreg 	=   $4
set ccdreg 	=   $5
set Chips 	= ( $6 )
set badpixfile 	=   $7
set OOT		=   $8
set destreak 	=   $9 
set dopar       =  $10

				### Ia. MAKE THE LOG FILE ####
set local_dir = `pwd`
set firstchar = `echo $top | awk -F "/" '{print$1}'`
if ( $#firstchar != 0 ) set top = ${local_dir}/${top}

if (! -d ${top} ) then
 cat <<EOF
 
 ${top} does not appear to exist!
 Exiting...
 
EOF
 exit
endif

if (! -d $top/CXCRI) mkdir ${top}/CXCRI
echo "OBJECT = ${obj}" > ${top}/CXCRI/CXC.log
echo "CHIPS = ${Chips}" >> ${top}/CXCRI/CXC.log
echo "DESTREAK = ${destreak}" >> ${top}/CXCRI/CXC.log
echo "OOT = ${OOT}" >> ${top}/CXCRI/CXC.log
echo "MAIN_DIR = ${top}" >> ${top}/CXCRI/CXC.log


				### Ib SET TOOLS and BITS ####

#set local_tools  = /upc76_2/dhudson/Chandra_Tools/
set local_tools  = /aibn156_a/dhudson/Chandra_Tools/
set local_bkg = /aibn156_a/XRAY/Chandra_BKG/
#			    1         2	        3 
#		  01234567890123456789012345678901
#set filterbits = "00000x000000000000000xx0xxxxxxxx"
set filterbits = "00000x00000000000000000000000000"



################################################# A Note on bits ########################################################
# Bit   Filter														#
#															#
#  0 	CHIPX - CHIPY invalid coordinates										#
#  1 	PHA problems after CTI correction (see acis_status_bits.letter.pdf)						#
#  2 	PHA value >  4095 ( 1+ of 9 pixels in 3x3 island)								#
#  3 	The sum of the PHA values of the nine pixels of a 3x3-pixel event island that are >= the split threshold	#
#  4	Badpixel (bpix file)												#
#  5	Next to a Badpix (FAINT) or within 2 of a badpix (VFAINT) Markevitch recommends NOT filtering these		#
#  6	"bad" or if the pixel is not on the active region of a sub array						#
#  7	bias value of pixel unknown											#
#  8 	bias parity error												#
#  9	Overclock value unknown												#
# 10	The overclock value associated with an event is not in the nominally-expected range of overclock values.	#	
# 11	The mean PHA value of the four corner pixels of a 3x3-pixel event island < -4095.				#
# 12	Bits 12 and 13 are used to indicate the number of pixels that were included in the computation of the mean	# 
# 13	PHA value of the four corner pixels of a 3x3-pixel event island. (see acis_status_bits.letter.pdf)		#
# 14	The mean value of the four corner pixels of a 3x3-pixel event island = 4095.					#
# 15	Streak														#
# 16	cosmic-ray afterglow (1)											#
# 17	cosmic-ray afterglow (2)											#
# 18	cosmic-ray afterglow (4)											#
# 19	cosmic-ray afterglow (8) 1110 = 14, 0001 = 1 (# of frames in which event has been reported)			#
# 20	The CTI-adjustment algorithm (if used) did not converge.							#
# 21	ununsed														#
# 22	ununsed														#
# 23    potential cosmic-ray background event for vf mode								#
# 24-31 unused														#
#########################################################################################################################


				#### II. CHECK THE INPUTS ####

				#### II. A Check which Chips ####
				####	   are being used.   ####
if ( $#Chips > 1 ) then
 set i = 1
 while ( $i <= $#Chips )
  if ( ${Chips[$i]} > 4 ) then

   cat <<EOF
 
 	At this time I will require that the S (CCD 4-9) chips be analyzed seperately.
	This is only a moderate inconvenience and any images can be combined
	later.  It is also makes good sense in terms of handling the background,
	etc... (As they vary between FI and BI) 
	Exiting...

EOF
   exit
  endif
 @ i++
 end
endif

if ( $Chips[1] == 7 ||  $Chips[1] == 5 || $Chips[1] == 9 ) then
 if ( $destreak == "Y" ) then
 
  cat <<EOF
  
 	WARNING!!!
	You are destreaking a BI Chip.  This is not a good idea!!!
	
EOF
 endif
endif

if ( $Chips[1] == 8  ) then
 if ( $destreak == "N" ) then
 
  cat <<EOF
  
 	WARNING!!!
	You are NOT destreaking the S4 Chip(CCD 8) .  This is not a good idea!!!
	
EOF
 endif
endif
				#### II. B.  CHECK THAT THE ####
				#### 	     DIRECTORIES    ####
				####	     EXIST          ####

cat <<EOF

	Checking to make sure the 
	files and directories exist...

EOF


# SET LOCAL DIRECTORY
echo "OBS_DIR = ${local_dir}" >> ${top}/CXCRI/CXC.log

# Determine if relative or absolute paths were given
# If they are absolute they should begin with a backslash
set firstchar = `echo $pathe | awk -F "/" '{print$1}'`
if ( $#firstchar != 0 ) set pathe = ${local_dir}/${pathe}

if (! -d ${pathe} ) then
 cat <<EOF
 
 The directory ${pathe} does not appear to exist!
  Exiting...

EOF
 exit
endif

if (! -d ${pathe}/primary/ ) then
 cat <<EOF
 
 The primary directory does not appear to be in directory ${pathe}!
 Exiting...
 
EOF
 exit
endif

if (! -d ${pathe}/secondary/ ) then
 cat <<EOF
 
 The secondary directory does not appear to be in directory ${pathe}!
 Exiting...
 
EOF
 exit
endif

if ( ${badpixfile} == "CREATE" && ! -d ${pathe}/supporting/ ) then
 cat <<EOF
 
 The supporting directory does not appear to be in directory ${pathe}.
 Hopefully bias files are in secondary folder....
 Continuing...
 
EOF
endif

echo "RAW_DIR = ${pathe}" >> ${top}/CXCRI/CXC.log


set firstchar = `echo $bkgreg | awk -F "/" '{print$1}'`
if ( $#firstchar != 0 ) set bkgreg = ${local_dir}/${bkgreg}

if (! -f ${bkgreg} ) then
 cat <<EOF
 
 The file ${bkgreg} does not appear to exist!
 Exiting...
 
EOF
 exit
endif
echo "BKG_REG = ${bkgreg}" >> ${top}/CXCRI/CXC.log


set firstchar = `echo $ccdreg | awk -F "/" '{print$1}'`
if ( $#firstchar != 0 ) set ccdreg = ${local_dir}/${ccdreg}

if (! -f ${ccdreg} ) then
 cat <<EOF
 
 The file ${ccdreg} does not appear to exist!
 Exiting...
 
EOF
 exit
endif
echo "CCD_REG = ${ccdreg}" >> ${top}/CXCRI/CXC.log


if ( $badpixfile != "CALDB" && $badpixfile != "NONE") then
 if ( ${badpixfile} == "DEFAULT" ||  ${badpixfile} == "CREATE" ) then
  set gzip_check = `ls ${pathe}/primary/*bpix* | grep .gz`
  if ($#gzip_check == 1) gunzip ${pathe}/primary/*bpix*
  set bpix = `ls ${pathe}/primary/*bpix*`
  if (! -f ${bpix} ) then
   cat <<EOF
 
	The bad pixel file does not appear to be in the primary folder!
	Exiting...
EOF
   exit
  endif
 else
  set firstchar = `echo $badpixfile | awk -F "/" '{print$1}'`
  if ( $#firstchar != 0 ) set badpixfile = ${local_dir}/${badpixfile}

  if (! -f ${badpixfile} ) then

    cat <<EOF
 
  The file ${badpixfile} does not appear to exist!
  Exiting...
  
EOF
   exit
  endif
 endif
endif
#echo "BADPIXFILE = ${badpixfile}" >> ${top}/CXCRI/CXC.log

if ( ${OOT} == "Y" ) then
 set dstrk_reg = ${local_dir}/destreak.reg
 if (! -f ${local_dir}/destreak.reg ) then
  cat <<EOF
 
  The file destreak.reg does not appear to exist
  and the destreak flag has been set to "Y".
  Exiting...
  
EOF
  exit
 endif
endif

if ( ${OOT} == "Y" ) then
 set dstrkrad = ${local_dir}/destreak.txt
 if (! -f ${local_dir}/destreak.txt ) then
  cat <<EOF
 
  The file destreak.txt does not appear to exist
  and the destreak flag has been set to "Y".
  Exiting...
  
EOF
  exit
 endif
endif
  


				#### II. C.  DETERMINE THE FILES ####
				####         NEEDED AND UNZIP    ####
				####         THEM IF NEEDED      ####
cat <<EOF


	Finding the appropriate primary 
	and secondary files and unzipping 
	them if needed...


EOF
				# Start with primary events #
				# We need badpix, asol, and #
				# evt2 (for the GTI only)   #
set gzip_check = `ls ${pathe}/primary/*evt2* | grep .gz`
if ($#gzip_check == 1) gunzip ${pathe}/primary/*evt2*


				# There may be more that one   #
				# aspect file, we need to join #
				# them with commas as a single #
				# variable.		       #
set gzip_check = ( `ls ${pathe}/primary/*asol1* | grep .gz` )
if ($#gzip_check > 0) gunzip ${pathe}/primary/*asol1*
set ASOL = ( `ls ${pathe}/primary/*asol1*` )
set asol = ${ASOL[1]}
set i = 2
while ($i <= $#ASOL)
 set asol = "$asol,${ASOL[$i]}"
 if (! -f ${ASOL[$i]} ) then
  cat <<EOF
 
The aspect file does not appear to be in the primary folder!
Exiting...
EOF
  exit
 endif

 @ i++
end
echo "ASOL = ${asol}" >> ${top}/CXCRI/CXC.log



				# Now for the secondary events.#
				# We need evt1 and flt1	       #
set gzip_check = `ls ${pathe}/secondary/*evt1* | grep .gz`
if ($#gzip_check == 1) gunzip ${pathe}/secondary/*evt1*
set gzip_check = `ls ${pathe}/secondary/*flt1* | grep .gz`
if ($#gzip_check == 1) gunzip ${pathe}/secondary/*flt1*
set gzip_check = `ls ${pathe}/secondary/*msk1* | grep .gz`
if ($#gzip_check == 1) gunzip ${pathe}/secondary/*msk1*


set evt1 = `ls ${pathe}/secondary/*evt1*`
if (! -f ${evt1} ) then
 cat <<EOF
 
The event-1 file does not appear to be in the secondary folder!
Exiting...
EOF
 exit
endif
set flt1 = `ls ${pathe}/secondary/*flt1*`
if (! -f ${flt1} ) then
 cat <<EOF
 
The filter file does not appear to be in the secondary folder!
Exiting...
EOF
 exit
endif
echo "FILTER = ${flt1}" >> ${top}/CXCRI/CXC.log

set evt2 = `ls ${pathe}/primary/*evt2*`
if (! -f ${evt2} ) then
 cat <<EOF
 
The event2 file does not appear to be in the primary folder!
Exiting...
EOF
 exit
endif

set msk1 = `ls ${pathe}/secondary/*msk1*`
if (! -f ${msk1} ) then
 cat <<EOF
 
The mask file does not appear to be in the secondary folder!
Exiting...
EOF
 exit
endif

if (${badpixfile} == "CREATE" ) then 
 
 set bias0 = ( `ls ${pathe}/secondary/*bias0.fits*` )
 if ( $#bias0 == 0 ) then
  cat <<EOF
  
  The bias files do nor appear to be in the secondary folder...
  Checking supporting folder.
  
EOF

  set biasdir = ${pathe}/supporting
  if ( ! -d ${pathe}/supporting/ ) then
   cat <<EOF
 
 The bias files do not appear to be in the secondary or supporting folder!
 Exiting...
EOF
   exit
  endif

 else
  set biasdir = ${pathe}/secondary
 endif
 set gzip_check = `ls ${biasdir}/*bias0* | grep .gz`
 if ($#gzip_check > 0) gunzip ${biasdir}/*bias0*
 set bias0 = ( `ls ${biasdir}/*bias0.fits` )
 if ( $#bias0 == 0 ) then
  cat <<EOF
 
The bias files do not appear to be in the secondary or supporting folder!
Exiting...
EOF
  exit
 endif
 set gzip_check = `ls ${biasdir}/*pbk0* | grep .gz`
 if ($#gzip_check > 0) gunzip ${biasdir}/*pbk0*
 set pbk0 = ( `ls ${biasdir}/*pbk0.fits` )
 if ( $#pbk0 == 0 ) then
  cat <<EOF
 
The pbk file do not appear to be in the secondary or supporting folder!
Exiting...
EOF
  exit
 endif
endif

cat <<EOF

Done!

EOF

				#### III. SETUP THE DIRECTORY ####
				####	  STRUCTURE	      ####
if (! -d ${top}/data/ )            mkdir ${top}/data/  
if (! -d ${top}/Images/ )          mkdir ${top}/Images/  
if (! -d ${top}/Regions/ )         mkdir ${top}/Regions/
if (! -d ${top}/data/BKG/ )        mkdir ${top}/data/BKG/  
if (! -d ${top}/data/event_1/ )    mkdir ${top}/data/event_1/
if (! -d ${top}/data/event_1/tmp ) mkdir ${top}/data/event_1/tmp
if (! -d ${top}/data/event_2/ )    mkdir ${top}/data/event_2/
if (! -d ${top}/data/event_2/tmp ) mkdir ${top}/data/event_2/tmp



				#### IV. NOW SOME PRELIMINARIES ####

# For which period was the observation taken?
#- Period A is from first light until 1999-09-16. t=-100 C (No CTI correction)

#- Period B is 1999-09-17 to 2000-01-28: t=-110C (Needs to be CTI corrected by PSU scripts)

# - Period C is 2000-01-29 to 2000-11-30: t=-120C

#- Period D is 2000-12-01 to 2003-12-31 (at least): t=-120C, the particle
#The C/D changeover date is rather arbitrary; both the C and D datasets are
#applicable around the end of 2000.
punlearn dmkeypar
set DATE = `dmkeypar $evt1 "DATE-OBS" echo=yes | awk -F "-" '{print$1$2$3}' | awk -F "T" '{print$1}'`
if ( $DATE < 19990916 ) then
 set EPOCH = "A"
 cat <<EOF
 	
	Your observation appears to be from Period - A.  NO CTI correction will be applied.

EOF
set Bbadpix =  "${local_bkg}badpix_AB_251103.fits"
else if ( $DATE < 20000128 ) then

 set EPOCH = "B"
 cat  <<EOF
 	
	Your observation appears to be from Period - B.  I will look for a PSU corrected event - 1 file,
	but this may cause problems as this type is not supported, yet.  Make sure to use the PSU
	corrector for t = -110 C!
	
EOF
set Bbadpix =  "${local_bkg}badpix_AB_251103.fits"
else if ( $DATE < 20001201 ) then

 set EPOCH = "C"
 cat  <<EOF
 	
	Your observation appears to be from Period - C.  
	A CTI correction will be applied.
	
EOF
set Bbadpix = "${local_bkg}badpix_C_200803.fits"
else 

 set EPOCH = "D"
 cat  <<EOF
 	
	Your observation appears to be from Period - D.  
	A CTI correction will be applied.
	
EOF
set Bbadpix = "${local_bkg}badpix_D_041104.fits"
endif
punlearn dmkeypar
set DATAMODE = `dmkeypar $evt1 "DATAMODE" echo=yes`
if ( $DATAMODE == "VFAINT" ) then 
 set vf = yes
else
 set vf = no
endif
set CCDTEMP = `dmkeypar $evt1 "FP_TEMP" echo=yes`
set CCDTEMP = `echo "scale=0;($CCDTEMP-273-0.5)/1" | bc -l`
set CHIPPOINTING = `fdump ${evt1}+1 STDOUT - - prdata=no page =no | grep "DSVAL1  " | awk '{print$3}' | awk -F ":" '{print$2}'`
if ( $CHIPPOINTING <= 3 ) then
 set POINT = "i"
else if ( $CHIPPOINTING == 7 ) then
 set POINT = "s"
else
 echo "Ooops seems to be a problem determining the pointing...exiting."
 exit
endif
cat <<EOF

	OBSERVATION 		: ${obj} 
	EPOCH 			: ${EPOCH} 
	MODE 			: ${DATAMODE}
	POINTING		: "${POINT}"
	Focal Plane temperature	: ~${CCDTEMP} C.
	

	
EOF

echo "EPOCH = ${EPOCH}" >> ${top}/CXCRI/CXC.log
echo "MODE = ${DATAMODE}" >> ${top}/CXCRI/CXC.log
echo "POINTING = ${POINT}" >> ${top}/CXCRI/CXC.log
echo "CCD_TEMP = ${CCDTEMP}" >> ${top}/CXCRI/CXC.log

cat <<EOF

	
	Attempting to determine the correct 
	background file to use...
	I will use Markevitch's backgrounds 
	as they have been CTI corrected, while 
	not all the CALDB's have.  
	
EOF
#Caveat Emptor:
#	All Markevtich's Backgrounds are in FAINT mode.

# Now I need to find the correct Background file
# This is tedious, but if we cheat using
# acis_bkgrnd_lookup we can match that to 
# The correct file
set i = 2
set ccdid = ${Chips[1]}
while ( $i <= $#Chips )
 set ccdid = "${ccdid},${Chips[$i]}"
 @ i++
end



#set prefix = "${local_bkg}acis${POINT}_${EPOCH}_"
set prefile = acis${POINT}_${EPOCH}_

# I need to cd to local_tools now, because they have
# numbers in the directory names now that can &*^(&^%
# my grep logic.
cd ${local_bkg}
if ( $Chips[1] == 7 ) then
 if ( $#Chips == 1 ) then
   set BKGSUF = `ls ${prefile}7_bg_evt*  | awk -F "bg_evt_" '{print$2}'`
   set BKG = "${local_bkg}${prefile}7_bg_evt_${BKGSUF}"
 else
  set BKGFILE = `ls ${prefile}* | awk -F "bg_evt_" '{print$1}' | grep $Chips[2]`
  set BKGSUF = `ls ${prefix}* | awk -F "bg_evt_" '{print$2}'`
  set BKG = "${local_bkg}${BKGFILE}bg_evt_${BKGSUF}"
 endif
# Apparently there is some problem with grepping "0"
# so I need to take care of that case specifically.
# Which is why the line:
# "set BKGFILE = ${BKGFILE[1]}" 
# is needed, luckily
# if the Chip(1) is 0 that file is always first.
else
 set BKGFILE = `ls ${prefile}* | awk -F "bg_evt_" '{print$1}' | grep $Chips[1]`
 set BKGFILE = ${BKGFILE[1]}
 set BKGSUF = `ls ${prefile}* | awk -F "bg_evt_" '{print$2}'`
 set BKGSUF = ${BKGSUF[1]}
 set BKG = "${local_bkg}${BKGFILE}bg_evt_${BKGSUF}"
endif
cd ${local_dir}

cat <<EOF

	Using $BKG 
	as the background for your observation.
	
EOF

# Check on Particle Background
if ( ${dopar} == "Y" ) then
 if ( ${EPOCH} != "D" ) then
  cat <<EOF
  
  	Since your observation is before
	EPOCH "D", there are no blank sky
	files...
	Continuing with Closed_BKG flag
	reset to "N"

EOF

  set dopar = "N"
  echo "PARTICLE_BKG = ${dopar}" >> ${top}/CXCRI/CXC.log
 else
  set parbkg = "${local_bkg}acis_D_0123567_stowed_evt_041104.fits"
  echo "PARTICLE_BKG = ${parbkg}" >> ${top}/CXCRI/CXC.log
 endif
endif

   
########################################	Make_new_evt2 version 1.1	##################################



cat <<EOF


#############################################################
		Make_new_evt2 v 2.0

EOF

				#### V. SET UP CHIP NOMENCLATURE	     ####
				####    THIS STEP IS ONLY FOR NOMENCLATURE   ####
				####    IT SIMPLY CREATES AN OUTPUT VARIABLE ####
				####    SO THE FILES WILL BE I0123 or S3, or ####
				####    WHATEVER			     ####
				####    I ADDED BADPIXFILTER		     ####
set FIRST_CHIP = ( "I0" "I1" "I2" "I3" "S0" "S1" "S2" "S3" "S4" "S5" )
set OTHER_CHIP  = ( 0 1 2 3 0 1 2 3 4 5 )
set j = 1
while ($j <= $#Chips)
 # Setting up prefixes for out files
 if ($j == 1) then
  set ccd = ${Chips[$j]}
  set k = `echo "scale=0;$ccd+1" | bc -l`	# This is needed since the #
  						# chips are labeled from   #
						# zero, and tcsh indexes   #
						# from one 		   #
  set CCD = ${FIRST_CHIP[$k]}
  set ccdid = "${ccd}"
 else
  set ccd = ${Chips[$j]}
  set k = `echo "scale=0;$ccd+1" | bc -l`	# This is needed since the #
  						# chips are labeled from   #
						# zero, and tcsh indexes   #
						# from one 		   #
  set CCD = "${CCD}${OTHER_CHIP[$k]}"
  set ccdid = "${ccdid},${ccd}"
 endif
 @ j++
end
echo "CCDLIST = ${CCD}" >> ${top}/CXCRI/CXC.log
echo "CCDID = ${ccdid}" >> ${top}/CXCRI/CXC.log

if ( -d tmp ) then
 rm -rf tmp/
endif
mkdir tmp


if ( $badpixfile == "CREATE" ) then
 # FIND CALDB BADPIX
 cat <<EOF
 
	Creating a new Bad-pixel file.  
	
EOF

 set badpix = ${top}/data/event_2/${obj}_badpix_used.fits 
 set pbkfile = `ls ${biasdir}/*pbk0.fits`
 
 set biasfiles = ( `ls ${biasdir}/*bias0.fits` )
 set cntr = 2
 set biaslist = ${biasfiles[1]}
 while ( ${cntr} <= $#biasfiles )
  set biaslist = ${biaslist},${biasfiles[$cntr]}
  @ cntr++
 end
 cat << EOF
  	
	Removing acis_afterglow_detection...
	resetting bits 16-19 to zero....
	
EOF

 # Note from the Chandra web-site the 15-12 is intentional eventhough we are
 # resetting bits 16-19 
 
 # http://cxc.harvard.edu/ciao/threads/acisdetectafterglow/
 #  The tool dmtcalc is used to reset the status bits.

 # The expression selects the afterglow bits (status=X15F,status=X14F,status=X13F,status=X12F) 
 # and sets them to "0" (False), while retaining the original value of all other status bits (status=status). 
 # The tool counts the bits in the opposite direction (left-to-right vs. right-to-left), so the numbers 15-12 
 # in the expression correspond to bits 16-19. 

 punlearn dmtcalc
 dmtcalc \
  infile=${evt1} \
  outfile=${top}/data/event_1/acis_reset_evt1.fit \
  expression="status=status,status=X15F,status=X14F,status=X13F,status=X12F" \
  clobber=yes
  
 set evt1 = ${top}/data/event_1/acis_reset_evt1.fit
  
 cat <<EOF

	Creating A New Badpixel File....
	
EOF

 punlearn acis_run_hotpix
 acis_run_hotpix \
  infile=${evt1} \
  outfile=${badpix} \
  badpixfile=${bpix} \
  biasfile="${biaslist}" \
  maskfile=${msk1} \
  pbkfile=${pbkfile} \
  clobber=yes

else if ( ${badpixfile} == "DEFAULT" ) then
 set badpix = ${bpix}
 set badpixfile = ${bpix}
else if ( ${badpixfile} != "NONE" ) then
 set badpix = ${badpixfile}
endif

if ( ${badpixfile} != "NONE" ) then
 echo "BADPIXFILE = ${badpix}" >> ${top}/CXCRI/CXC.log
else
 echo "BADPIXFILE = NONE"      >> ${top}/CXCRI/CXC.log
endif

# SET THE ARDLIB TO THE BADPIXFILE
punlearn ardlib
foreach j ( $Chips )
 pset \
  ardlib AXAF_ACIS${j}_BADPIX_FILE = \
      "${badpix}[BADPIX${j}]"
end


cd ${top}/data/event_1

if ( ${badpixfile} != "NONE" ) then
 cat <<EOF

	Combining BADPIX from 
	${badpix} 
	with those used for Markevitch's background....
	
EOF
 # Now we need to combine the latest 
 # Badpix file from the CALDB or user
 # input and the badpix used in the 
 # background from Markevitch 

 # Unfortunately this is a huge pain in the ass 


 # First we need to determine how many extensions there are because
 # We can only merge one extension at a time
 set NEXTS = (`fstruct ${badpix} | grep BINTABLE | awk '{print$1}'`)

 # Now we make each extension a separate file and
 # merge them ( the j subscript comes from from the fact that
 #	       the zeroeth extension is "1" in dmcopy)
 set j = 1
 foreach i ( $NEXTS )
  @ j++
  dmcopy infile="${badpix}[$j]" outfile="/tmp/badpix_${i}.fits" clobber=yes
  dmcopy infile="${Bbadpix}[$j]" outfile="/tmp/Bbadpix_${i}.fits" clobber=yes
  set offset = `fstatistic /tmp/badpix_${i}.fits+1 COMPONENT - | grep "The number of points used in calculation is" | awk '{print$9}'`
  fcalc /tmp/Bbadpix_${i}.fits /tmp/Bbadpix_${i}_temp.fits clname = COMPONENT expr = "COMPONENT + ${offset}" clobber=yes
  mv /tmp/Bbadpix_${i}_temp.fits /tmp/Bbadpix_${i}.fits 
  punlearn dmmerge
  fmerge \
   infiles="/tmp/badpix_${i}.fits /tmp/Bbadpix_${i}.fits" outfile="/tmp/total_badpix_${i}.fits" columns=- clobber=yes >& error.log
 end

 # Now we need to build the fits and ascii file
 # first copy the first extension and then add the others
 # to it. 

 foreach i ( $NEXTS )
  if ($i == 1) then
   cp /tmp/total_badpix_1.fits ${obj}_badpix.fits
  else
   fappend "/tmp/total_badpix_${i}.fits+1" ${obj}_badpix.fits
  endif

  set CHIPX = ( `fdump ${obj}_badpix.fits+${i} STDOUT "CHIPX" - prhead=no prdata=yes showcol = no showunit=no showrow=no page=no` )
  set CHIPY = ( `fdump ${obj}_badpix.fits+${i} STDOUT "CHIPY" - prhead=no prdata=yes showcol = no showunit=no showrow=no page=no` )
  set j = 1
  while ( $j < $#CHIPX )
   set k = `echo "scale=0;$j + 1" | bc -l`
   set NODE = `echo "scale = 0;$i - 1" | bc -l`
   if ($i == 1 && $j == 1) then
    echo "$NODE ${CHIPX[$j]} ${CHIPX[$k]} ${CHIPY[$j]} ${CHIPY[$k]}" | awk '{printf"%g\t%g\t%g\t%g\t%g\n",$1,$2,$3,$4,$5}' > ${obj}_badpix
   else
    echo "$NODE ${CHIPX[$j]} ${CHIPX[$k]} ${CHIPY[$j]} ${CHIPY[$k]}" | awk '{printf"%g\t%g\t%g\t%g\t%g\n",$1,$2,$3,$4,$5}' >> ${obj}_badpix
   endif
   @ j++
   @ j++
  end
 end
echo "' ' ' '                         / Configuration control block--------------------" > /tmp/header.txt
echo "ORIGIN  = 'ASC     '" >> /tmp/header.txt
echo "CREATOR = 'acis_badpix_ard.pro - Version 4.5' /" >> /tmp/header.txt
echo "CHECKSUM= 'kN5AmN5AkN5AkN5A'   / HDU checksum updated 2000-09-28T16:05:47" >> /tmp/header.txt
echo "DATASUM = '##########'         / Data unit checksum written in ASCII" >> /tmp/header.txt
echo " ' ' ' '                          / Time information block-------------------------" >> /tmp/header.txt
echo "DATE    = '2000-09-28T16:01:11' / Date and time of file creation (UTC)" >> /tmp/header.txt
echo "  ' ' ' '                         / Observation information block------------------" >> /tmp/header.txt
echo "MISSION = 'AXAF    '           / Mission is AXAF" >> /tmp/header.txt
echo "TELESCOP= 'CHANDRA '           / Telescope is CHANDRA" >> /tmp/header.txt
echo "INSTRUME= 'ACIS    '           / HRC, ACIS, EPHIN, S/C subsystems" >> /tmp/header.txt
echo "TITLE   = 'The bad pixels & columns of the 10 ACIS CCDs' /" >> /tmp/header.txt

 fmodhead ${obj}_badpix.fits+0 /tmp/header.txt
else
 cp $Bbadpix ${obj}_badpix.fits
 set ASCII_badpix = `echo $Bbadpix | awk -F ".fits" '{printf $1}'`
 cp $ASCII_badpix ${obj}_badpix
endif

# acis_process_events doesn't like the badpix files I create, so I'll just use one
# and then filter using Vihlkin's badpixfilter script.







############5. Streaks in chip S4 (and occasionally in other FI chips)###########
#										#
# I ran 'destreak' (oroginally by John Houck, now part of CIAO) with default	#
# settings on all FI chip data (as of ver. 200803 datasets) to remove those	#
# bright soft streaks along chipx.  It removes most but not all of the		#
# streaks, so use S4 data below E<2 keV with care. Note that it should be run	#
# before the bad pixel filtering, and setting status to 0, because the		#
# `destreak' result depends on the input file and it does not consider events	#
# with status!=0 (e.g., if the VF status bit was set). I simply rename the	#
# status column before running destreak, then rename it back.			#
#										#
#################################################################################

#################################################################################
#										#
# I recommend that you don't use evt2 files and instead create your own clean	#
# file starting from evt1, keeping the adjacent pixels/columns in and not	#
# filteting by status=0.  The only useful thing in evt2 files is the GTI	#
# extension, which can be used as input for lc_clean.				#
#										#
# The total area of the detector covered by hot pixels is very small, so	#
# different pixel filtering is not a concern, unless a bright hot pixel is	#
# left in the user's data. Bad columns are more important and ideally, the	#
# same bad pixel and column list should be applied to the data and the		#
# background, and also used for making the exposure maps. I hope that files	#
# badpix*.fits work with the standard exposure map generation tools. To apply	#
# the ASCII badpix* lists to the event file, there is a tool badpixfilter (by	#
# A. Vikhlinin):								#
#										#
#################################################################################


cat <<EOF

	Processing events with updated
	BADPIX file, CTI correction, and 
	apply_tgain correction.
	This may take some time...

EOF
if ( $destreak == "Y" ) then
 if ( $vf == "yes" ) then 
  cat <<EOF

	Since you want the events de-streaked, and you have a 
	very faint observation, I will process the events without 
	the vf-flag.  De-streak and then apply the vf-flag.

EOF
 endif
 punlearn acis_process_events
 acis_process_events \
  infile       = "${evt1}" \
  outfile      = "tmp/${obj}_streaked_evt1.fits" \
  acaofffile   = ${asol} \
  eventdef     = ")stdlev1" \
  check_vf_pha = no \
  rand_pha     = yes \
  apply_cti    = yes \
  apply_tgain  = yes \
  clobber      = yes \
  badpixfile   = ${badpix} \
  gainfile     = CALDB >& /tmp/error.log

 # Get rid of the silly warning
 set errorcheck = `less /tmp/error.log | grep "ERROR"`
 if ($#errorcheck > 0 ) echo $errorcheck


 punlearn dmcopy
 dmcopy \
  infile  = "tmp/${obj}_streaked_evt1.fits[EVENTS][grade=0,2,3,4,6][status=${filterbits}]" \
  outfile = "tmp/${obj}_streaked_filtered_evt1.fits" \
  clobber = yes

   cat <<EOF
 
	Destreaking the filtered image....

EOF

 punlearn destreak
 destreak verbose=0 infile="tmp/${obj}_streaked_filtered_evt1.fits" outfile="tmp/${obj}_dstrk_evt1.fits" ccd_id="" filter=no clobber=yes
 punlearn dmcopy
 dmcopy \
  "tmp/${obj}_dstrk_evt1.fits[EVENTS][status=xxxxxxxxxxxxxxxx0xxxxxxxxxxxxxxx]" "tmp/${obj}_dstrk_clean_evt1.fits" clobber=yes
 punlearn dmcopy
 dmcopy "tmp/${obj}_dstrk_evt1.fits[EVENTS][status=00000000000000001000000000000000]" "tmp/${obj}_streaked_evt1.fits" clobber=yes


 cat <<EOF
 
	The image has been destreaked.  Check 
	data/event_1/tmp/${obj}_streaked_evt1.fits
	to see if any significant emission has been
	removed.

EOF
 if ( $vf == "yes" ) then

  cat <<EOF
  	
	Reprocessing the data for Cosmic
	Ray afterglow...

EOF
  punlearn acis_process_events
  acis_process_events \
   infile       = "tmp/${obj}_dstrk_clean_evt1.fits" \
   outfile      = "tmp/${obj}_new_evt1.fits" \
   acaofffile   = ${asol} \
   check_vf_pha = ${vf} \
   clobber      = yes \
   doevtgrade   = no \
   calculate_pi = no \
   apply_tgain  = no \
   apply_cti    = no \
   badpixfile   = NONE \
   stop         = none >& /tmp/error.log

  # Get rid of the silly warning
  set errorcheck = `less /tmp/error.log | grep "ERROR"`
  if ($#errorcheck > 0 ) echo $errorcheck
 else
  cp tmp/${obj}_dstrk_clean_evt1.fits tmp/${obj}_new_evt1.fits
 endif
else

 punlearn acis_process_events
 acis_process_events \
  infile       = "${evt1}" \
  outfile      = "tmp/${obj}_new_evt1.fits" \
  acaofffile   = ${asol} \
  eventdef     = ")stdlev1" \
  check_vf_pha = ${vf} \
  rand_pha     = yes \
  apply_cti    = yes \
  apply_tgain  = yes \
  clobber      = yes \
  badpixfile   = ${badpix} \
  gainfile     = CALDB >& /tmp/error.log
  
  # Get rid of the silly warning
  set errorcheck = `less /tmp/error.log | grep "ERROR"`
  if ($#errorcheck > 0 ) echo $errorcheck
 

endif

# Move the badpix file to the event_2 folder
# It is need for further processing
if ( $badpixfile != "CREATE" ) then
 cp -f ${badpix} ${top}/data/event_2/${obj}_badpix_used.fits 

 if ( $DATAMODE == "VFAINT" ) then 
  cat <<EOF

	As of CIAO 3.2 the no afterglow correction is needed.
	However you are not creating a new badpixel file, so I
	assume  you have a new file from DS 7.4.0 or higher OR
	you have already created a badpixfile...
	
EOF


 endif
endif
if ( $DATAMODE == "VFAINT" ) then 


 cat <<EOF

	As of CIAO 3.2 the no afterglow correction is needed.
	Therefore we will skip it and avoid the possible
	accidental subtraction it can cause.

	If you have a very bright source, source
	photons may have been accidently removed.
	Please check these files!
	They are: 
		${top}/data/event_1/tmp/acis_23bad.fits
		${top}/data/event_1/tmp/${obj}_new_evt1.fits
	
EOF

 # Create files to check "Clean ACIS Background in VFAINT Mode" Did I reject source photons?
 punlearn dmcopy
 dmcopy \
  infile  = "tmp/${obj}_new_evt1.fits[EVENTS][status=xxxxxxxx1xxxxxxxxxxxxxxxxxxxxxxx]" \
  outfile= "tmp/acis_23bad.fits" \
  clobber = yes


endif

cat <<EOF
	Filtering with status = ${filterbits}
	i.e. leaving the pixels and columns next to
	bad pixels in columns in... (see 
	http://cxc.harvard.edu/ciao3.2/data_products_guide/acis_status_bits.html 
	and Markevitvh's cookbook for information on filter bits)
	

EOF

punlearn dmcopy
dmcopy \
 infile  = "tmp/${obj}_new_evt1.fits[EVENTS][grade=0,2,3,4,6][status=${filterbits}]" \
 outfile = "tmp/${obj}_flt_evt1.fits" \
 clobber = yes

# Moving on assuming we didn't reject any real source photons.
cat <<EOF

	Creating New Event-2 files
EOF

punlearn dmcopy
dmcopy \
 infile  = "tmp/${obj}_flt_evt1.fits[EVENTS][@${flt1}][cols -phas]" \
 outfile = "../event_2/${obj}_evt2.fits" \
 clobber = yes

cat <<EOF

 Done!

EOF


cat <<EOF

	Using Vikhlinin's badpixfilter to filter bad pixels for both background and observation


EOF
cp -f ${local_tools}badpixfilter .

if ( -f "../event_2/${obj}_evt2_clean.fits" ) rm -f "../event_2/${obj}_evt2_clean.fits"
if ( -f "../event_2/${obj}_raw_bkg.fits" ) rm -f "../event_2/${obj}_raw_bkg.fits"

./badpixfilter "../event_2/${obj}_evt2.fits" "../event_2/${obj}_evt2_clean.fits" ${obj}_badpix
./badpixfilter "$BKG" "../event_2/${obj}_raw_bkg.fits" ${obj}_badpix
if ( $dopar != "N" ) then
 if ( -f "../event_2/${obj}_raw_parbkg.fits" ) rm -f "../event_2/${obj}_raw_parbkg.fits"
  ./badpixfilter "${parbkg}" "../event_2/${obj}_raw_parbkg.fits" ${obj}_badpix
 endif
endif
echo "BADPIXLIST = ${top}/data/event_1/${obj}_badpix" >> ${top}/CXCRI/CXC.log

cat <<EOF

Done!

EOF
##########################	NOTES ON FILTERING	#################################
#		6. VFAINT mode								#
#											#
#For observations performed in VF mode, one can reduce the background by		#
#applying the A. Vikhlinin's filter (see the ACIS cal page, section			#
#"Background", then "VF mode cleaning"). acis_process_events now has an			#
#option 'check_vf_pha=yes' emulating the Vikhlinin's algorithm and filling in		#
#a certain status bit. The period D files are composed of VF mode			#
#observations and have this bit filled. One can run acis_process_events with		#
#this option on their data and then filter both the data and the background		#
#files:											#
#											#
#fcopy in_evt.fits"[events][status==bxxxxxxxx0xxxxxxxxxxxxxxxxxxxxxxx]" out_evt.fits	#
#											#
#As of ver. 271103, there are no other nonzero status bits left in the bg		#
#event files, so one can just filter by status==b0.					#
#											#
# Bad pixels and columns are excluded using the respective bad pixel tables		#
# for each time period, included in this directory.  Unlike in the archival		#
# evt2 files, I do not exclude the columns and pixels adjacent to the true bad		#
# columns/pixels. The status bits that would flag these adjacent events are		#
# reset to 0, so further status filtering won't have any effect (this is to		#
# make the background files usable for both FAINT and VFAINT data).			#
#											#
# For period D, all observations included in the datasets were performed in		#
# VFAINT mode, and the respective status bit in the background datasets			#
# is filled (so the datasets can be filteded by this bit for use with a			#
# VFAINT-mode observation, or used as is for a FAINT observation).			#
#########################################################################################
if ( $DATAMODE == "VFAINT" ) then 
 cat <<EOF

	As you are reducing a VFAINT Observation,
	I will re-process the background events with the vf-flag...

EOF
 mv "../event_2/${obj}_raw_bkg.fits" "../event_2/tmp/${obj}_raw-faint_bkg.fits"
 cat <<EOF
 
 	Filtering the Background data with status = 0 
	(since Markevtich resets all the bits to zero
	in his background sets, this will simply filter 
	VF bit (should be bit 23, but looks like bit 9)
	As I understand it, this will only affect EPOCH
	D. )
	
EOF
 fcopy "../event_2/tmp/${obj}_raw-faint_bkg.fits[events][status==b0]" "../event_2/${obj}_raw_bkg.fits"
 if ( $dopar != "N" ) then
  mv "../event_2/${obj}_raw_parbkg.fits" "../event_2/tmp/${obj}_raw-faint_parbkg.fits"
  fcopy "../event_2/tmp/${obj}_raw-faint_parbkg.fits[events][status==b0]" "../event_2/${obj}_raw_parbkg.fits"
 endif
 
endif

#######################################		Destreaking if Needed		##################################
# To avoid confusion with cosmic ray events 
# and events generated in bad pixels and bad 
# columns, this tool should be applied after 
# all standard filtering steps.


# De-streaking the OOTs
if ( $OOT == "Y" ) then

 cat <<EOF
  
  	Destreaking the Out-of-Time data....

EOF

 mv "../event_2/${obj}_evt2_clean.fits" "../event_2/tmp/${obj}_evt2_strk.fits" 
 punlearn dmstat
 set PEAKX = `dmstat "../event_2/tmp/${obj}_evt2_strk.fits[bin sky=4]"  centroid=no sigma=no | \
               grep "max" | \
	       awk -F "(" '{print$2}' | \
	       awk '{print$1}'`

 set PEAKY = `dmstat "../event_2/tmp/${obj}_evt2_strk.fits[bin sky=4]"  centroid=no sigma=no | \
               grep "max" | \
	       awk -F "(" '{print$2}' | \
	       awk '{print$2}'`


 punlearn dmextract
 dmextract infile="../event_2/tmp/${obj}_evt2_strk.fits[sky=region(${dstrk_reg})][bin pi]" outfile="../event_2/tmp/${obj}_bkg.pi"
 set dx = ( `less ${local_dir}/destreak.txt` )
 punlearn acisreadcorr
 acisreadcorr \
  infile="../event_2/tmp/${obj}_evt2_strk.fits" \
  outfile="../event_2/${obj}_evt2_clean.fits" \
  aspect=${asol} \
  x=${PEAKX} y=${PEAKY} \
  dx=${dx[1]} dy=${dx[2]} \
  bkg="../event_2/tmp/${obj}_bkg.pi"

endif 


cat <<EOF

	Extracting events from the desired CCDs.

EOF


# Event_2 file only the appropriate chips 
punlearn dmcopy
dmcopy \
 infile="../event_2/${obj}_evt2_clean.fits[ccd_id=${ccdid}][sky=region(${ccdreg})]" \
 outfile="../event_2/${obj}_${CCD}_evt2.fits" clobber=yes

# Event_2 file in the 0.3 - 10.0 keV only I0123 chips for making Image
punlearn dmcopy
 dmcopy \
 infile="../event_2/${obj}_${CCD}_evt2.fits[energy=300:10000]" \
 outfile="../event_2/${obj}_${CCD}_evt2_0.3-10.fits" clobber=yes

cat <<EOF

Done!

EOF


cat <<EOF

	Reprojecting the background into Sky coordinates!

EOF
# Reproject the background into sky coordinates
punlearn reproject_events
reproject_events \
 infile = "../event_2/${obj}_raw_bkg.fits[cols -time]" \
 outfile = "../event_2/${obj}_BKG_evt2.fits" \
 match = "../event_2/${obj}_evt2_clean.fits" \
 aspect="${asol}" \
 random=0 \
 clobber=yes
cat <<EOF

Done!

EOF

if ( $dopar != "N" ) then
 cat <<EOF

	Reprojecting the closed background into Sky coordinates!

EOF
 
 # First we need to figure out which CHIPS we need
 # otherwise reproject_events will fail
 
 # We'll just use the background file
 # to determine the Chips we need (It is
 # important to bring the extra chips, i.e.
 # this chips the user didn't specify in case
 # later he/she wants to check background
 # of something else with these chips.
 
 set PARCHIPS = `echo $BKG | awk -F "_D_" '{print$2}' | awk -F "_bg_evt_271103.fits" '{print$1}'`
 set POSSCHIPS = ( 0 1 2 3 5 6 7 )
 set i = 1
 set j = 1
 while ( $j <= $#POSSCHIPS )
  set CHK = `echo ${PARCHIPS} | grep ${POSSCHIPS[$j]}`
  if ( $#CHK > 0 ) then
   if  ( $i == 1 ) then
    set parchips =  ${POSSCHIPS[$j]}
   else
    set parchips = "${parchips},${POSSCHIPS[$j]}"
   endif
   @ i ++
  endif
  @ j++
 end

 punlearn dmcopy
 dmcopy \
  infile="../event_2/${obj}_raw_parbkg.fits[ccd_id=${parchips}]" \
  outfile="../event_2/${obj}_raw_parbkg_${PARCHIPS}.fits" clobber=yes

 punlearn reproject_events
 reproject_events \
  infile = "../event_2/${obj}_raw_parbkg_${PARCHIPS}.fits[cols -time]" \
  outfile = "../event_2/${obj}_PARBKG_evt2.fits" \
  match = "../event_2/${obj}_evt2_clean.fits" \
  aspect="${asol}" \
  random=0 \
  clobber=yes >& /dev/null

 cat <<EOF

Done!

EOF

endif

 
cat <<EOF

	Cleaning up the background

EOF
# Event_2 file only appropriate chips 
punlearn dmcopy
dmcopy \
 infile="../event_2/${obj}_BKG_evt2.fits[ccd_id=${ccdid}][sky=region(${ccdreg})]" \
 outfile="../event_2/${obj}_${CCD}_BKG_evt2.fits" clobber=yes 

# Event_2 file in the 0.3 - 10.0 keV only appropriate chips for making Image
punlearn dmcopy
dmcopy \
 infile="../event_2/${obj}_${CCD}_BKG_evt2.fits[energy=300:10000]" \
 outfile="../event_2/${obj}_${CCD}_BKG_evt2_0.3-10.fits" clobber=yes

if ( $dopar != "N" ) then
 # Event_2 file only appropriate chips 
 punlearn dmcopy
 dmcopy \
  infile="../event_2/${obj}_PARBKG_evt2.fits[ccd_id=${ccdid}][sky=region(${ccdreg})]" \
  outfile="../event_2/${obj}_${CCD}_PARBKG_evt2.fits" clobber=yes 

 # Event_2 file in the 0.3 - 10.0 keV only appropriate chips for making Image
 punlearn dmcopy
 dmcopy \
  infile="../event_2/${obj}_${CCD}_PARBKG_evt2.fits[energy=300:10000]" \
  outfile="../event_2/${obj}_${CCD}_PARBKG_evt2_0.3-10.fits" clobber=yes

endif

cat <<EOF

Done!

EOF

cd ../event_2/




########################################	PNTSRC_DETECT version 1.0	##################################

cat <<EOF


#############################################################
		PNTSRC_DETECT v 1.0



EOF

set SCALES = ( "1.0 2.0 4.0 8.0 16.0" ) # The size of the wavelet scale for source detection
if (! -d srcs) mkdir srcs

cat <<EOF

	Running wavdetect.....This may take awhile....

EOF
punlearn wavdetect
wavdetect \
 infile="${obj}_${CCD}_evt2.fits" \
 outfile="srcs/${obj}_srclist.fits" scellfile="srcs/${obj}_scell.fits" imagefile="srcs/${obj}_image.fits" defnbkgfile="srcs/${obj}_normback.fits" \
 scales="${SCALES}" clobber=yes regfile="srcs/Pntsrcs.reg"

cat <<EOF

Done!

EOF


cat <<EOF
 
 	Making a Region file for the Chips - point sources
 
EOF

# Now I need to build a region file which removes the point sources
cp $ccdreg ${top}/Regions/${CCD}_nopntsrcs.reg
set pntsrcs = ( `less srcs/Pntsrcs.reg | grep ellipse` )
set j = 1
while ( $j <= $#pntsrcs )
 set nanchk = `echo ${pntsrcs[$j]} | grep "nan"`
 if ( $#nanchk == 0 ) then
  echo "-${pntsrcs[$j]}" >> ${top}/Regions/${CCD}_nopntsrcs.reg
 else
  cat <<EOF
  
     ${pntsrcs[$j]} 
     was rejected because it had 
     a nan parameter.  You may wish to check for
     a real point source there.
     
EOF
 endif
 @ j++
end
set REGION = "${top}/Regions/${CCD}_nopntsrcs.reg"
cp ${ccdreg} ${top}/Regions/${CCD}.reg

# Now to create the lc_clean.par file
# This is tricky
# I need to tbuild the lc_clean.par file for each CHIP they want to analyze


#We can build the lc_clean.par file
# from pieces I'll keep in my home directory










########################################	CLEAN_EVT2 version 1.0		##################################

cat <<EOF


#############################################################
		CLEAN_EVT2 v 1.0


EOF
cat <<EOF

	Cleaning point sources from the event file

EOF
# Event_2 file only appropriate chips cleaned for 
# point sources for spectra and normalizing background
punlearn dmcopy
dmcopy \
 infile="${obj}_${CCD}_evt2.fits[sky=region(${REGION})]" \
 outfile="${obj}_${CCD}_evt2_clean.fits" clobber=yes


cat <<EOF

	Extracting the source GTI...

EOF
# Filter through all the standard GTIs, so all the Chips have the same interval
if ( $#Chips > 1 ) then
 set j = 1
 while ( $j <= $#Chips )
  set k = ${Chips[$j]}
  dmcopy "${obj}_${CCD}_evt2_clean.fits[gti${k}]" tmp/gti${k}.fits clobber=yes
  dmcopy "${obj}_${CCD}_evt2_clean.fits[@tmp/gti${k}.fits]" ${obj}_${CCD}_evt2_clean_NSGTI.fits clobber=yes
  @ j++
 end
 set k = ${Chips[1]}
 dmcopy "${obj}_${CCD}_evt2_clean_NSGTI.fits[gti${k}]" tmp/${obj}_STDGTI.fits clobber=yes
else
 dmcopy "${obj}_${CCD}_evt2_clean.fits[gti${Chips}]" tmp/${obj}_STDGTI.fits clobber=yes
endif

cat <<EOF
 
	Making an appropriate lc_clean.par file ...


EOF

#set j = 1
#while ( $j <= $#Chips )
#if ( $#Chips > 1 ) then
# set k = ${Chips[$j]}
set c = `echo "scale=0;$k+1" | bc -l`	# This is needed since the chips are labeled from zero, tcsh indexes from 1
set lccleaninfile = "${obj}_${CCD}_evt2_clean.fits"
set lcgtifile = "tmp/${obj}_STDGTI.fits"
set lccleangtifile = "${obj}_clean_GTI.fits"
set uncleanlc = "tmp/${obj}_${CCD}.lc"
set cleanlc = "tmp/${obj}_${CCD}_clean.lc"
set parfile = "lc_clean_${CCD}.par"
#else
# set lccleaninfile = "${obj}_${CCD}_evt2_clean.fits"
# set lcgtifile = "tmp/${obj}_STDGTI.fits"
# set lccleangtifile = "${obj}_clean_GTI.fits"
# set uncleanlc = "${obj}_${CCD}.lc"
# set cleanlc = "${obj}_${CCD}_clean.lc"
# set parfile = "lc_clean.par"
#endif

echo "num_datafiles=1" > ${parfile}
if (${Chips[1]} == 7 ) then
 cat <<EOF

	This Chip appears to be the S3 Chip.  
	I will make an appropriate lc_clean.par 
	file.  

EOF
 echo  "file1=${lccleaninfile}[events][energy>2500&&energy<7000&&ccd_id==7]" >> ${parfile}  
 echo  "binsize=1037.12" >> ${parfile}



else if (${Chips[1]} == 5 ) then
 cat <<EOF

	This Chip appears to be the S1 Chip.  
	I will make an appropriate lc_clean.par 
	file.  

EOF
 echo  "file1=${lccleaninfile}[events][energy>2500&&energy<7000&&ccd_id==5]" >> ${parfile}  
 echo  "binsize=1037.12" >> ${parfile}

else if (${Chips[1]} == 8 ) then
 cat <<EOF

	This Chip appears to be the S4 Chip.  
	I will make an appropriate lc_clean.par 
	file.  

EOF
 echo  "file1=${lccleaninfile}[events][energy>300&&energy<12000&&ccd_id==8]" >> ${parfile}  
 echo  "binsize=1037.12" >> ${parfile}


else
 cat <<EOF

	This Chip appears to NOT be the S3 or the S1 Chip.  
	I will make an appropriate lc_clean.par 
	file.  

EOF
 echo  "file1=${lccleaninfile}[events][energy>300&&energy<12000&&ccd_id<7]" >> ${parfile}
 if ( $#Chips > 1 ) then
  echo  "binsize=259.28" >> ${parfile}
 else
  echo  "binsize=1037.12" >> ${parfile}
 endif
endif
echo "gti_file=${lcgtifile}              - input uncleaned GTI file" >> ${parfile} 
echo "clean_gti_file= ${lccleangtifile}    - (optional) output cleaned GTI file"  >> ${parfile}
echo "lc_dirty=${uncleanlc}                   - (optional) files for lightcurves" >> ${parfile}
echo "lc_clean=${cleanlc}"							   >> ${parfile}

less ${local_tools}lc_clean_end.par >> ${parfile}

cp ${local_tools}lc_clean ./


 cat <<EOF

	"Running Vihklinin's lc_par script for the data"

EOF
 # NOTES FROM VIKHLININ
 #For best results, use the same cleaning setup as that used for making the
 #background files. The background observations were cleaned separately for BI
 #and FI chips, excluding ASCA grades 1,5,7, excluding hot pixels (see
 #below). The energy band to use is 0.3-12 kev for FI chips and 2.5-7 kev for
 #S3 chip (2.5-6 kev for S1). I used
 #binsize=259.28    for several FI chips and 0.3-12 kev band, or
 #binsize=1037.12   for S3 and 2.5-7 kev band, and
 lc_clean ${parfile}




# Okay this is confusing.  Originally I deleted the GTI extensions so that the
# single one would be appended which would be for all Chips.  This created problems
# Because it was labeled as the I0 GTI (for the obs I was doing).  It changed all the
# EXPOSUR# LIVTIME# ONTIME# keywords to 0.  This created problems when making e.g. WMAPs
# which gave problems with mkacisrmf.  
# When you crank up the verbosity mkacisrmf gives a weighting factor of "inf" for the CHIPS that
# have zero exposure time in the WMAP.
#
# Anyway, I found a work-around by editing the header
# of the WMAP to the correct LIVTIME#s, EXPOSUR#s, and ONTIME#.  There doesn't seem to be
# any problems with the spectral files, but I don't want to take a chance.  So I figure I
# need those GTIs (it's better to have them).
# Here's the problem
# If I don't delete the GTI extensions when I filter on the new GTI, I keep all the extensions
# But they have different total times.  This annoys me, because the GTI I read in was a
# combination of all the standard GTIs, so there were no intervals where one CHIP has
# a good time and the other has a bad time.  Therefore all the Chips should have the same
# time.  (they differences are small < 3 seconds, but still annoying).  Also the number
# of events is the same no matter what the filtering (i.e. if I delete the standard GTIs
# and filter OR if I don't delete them and filter...I get the same number of events).
# I tried various things to aleviate this
#
# Trying to make the GTI have an extension for each CHIP (won't let me append the GTI file)
#
# Appending the GTI file to the events file and changing header keywords (doesn't work)
#	plus the gti on the events file has different keywords than the gti file itself
#
# I tried deleting the GTI extension and keywords (this created problems WMAPs)
#
# I tried deleting the GTI extension and fixing the keywords (this created problems with the WMAPs)
#
# So I came up with this:
# (1) Don't delete the GTI extensions
# (2) filter with the new GTI
# All the old GTIs will have the same number or MORE rows than the new GTI.  This is important
# because ftools lets you remove rows, but you can't add rows.
# (3) Remove the rows from the GTI extensions that have more rows than the new GTI
# (4) Remove the cols START and STOP from the GTI extensions
# (5) ADD the the cols START and STOP from the new GTI file
# (6) filter the file with itself.  This will give all Chips the same livetime, exposure time, and ontime
# Problem solved


 cat <<EOF

	Updating the event file GTI

EOF

punlearn dmcopy
dmcopy "${obj}_${CCD}_evt2.fits[@${lccleangtifile}]" "${obj}_${CCD}_evt2_goodgti.fits" clobber=yes

set GTI = ( `fstruct "${obj}_${CCD}_evt2_goodgti.fits" | grep BINTABLE | grep GTI | awk '{printf"%g\n",$1}'` )

set GROWS = `fkeyprint ${lccleangtifile}+1 NAXIS2 | grep number | awk '{print$3}'`
set j = 1
while ( $j <= $#GTI )
 set NROWS = `fkeyprint ${obj}_${CCD}_evt2_goodgti.fits+${GTI[$j]} NAXIS2 | grep number | awk '{print$3}'`
 if ( $NROWS > $GROWS ) then
  set frow = `echo "scale=0;${GROWS}+1" | bc -l`
  set drow = `echo "scale=0;${NROWS}-${GROWS}" | bc -l`
  fdelrow infile=${obj}_${CCD}_evt2_goodgti.fits+${GTI[$j]} firstrow=${frow} nrows=${drow} confirm=N proceed=Y
 endif 
 if ( $NROWS < $GROWS ) then
  cat << EOF
  
  	Somehow the new GTI extension has fewer rows than the old one...
	This shouldn't be possible, so there is probably a problem.
	I will continue without editing the GTI extension...
	
EOF
 else
  fdelcol infile=${obj}_${CCD}_evt2_goodgti.fits+${GTI[$j]} colname=START confirm=N proceed=Y
  fdelcol infile=${obj}_${CCD}_evt2_goodgti.fits+${GTI[$j]} colname=STOP confirm=N proceed=Y
  faddcol infile=${obj}_${CCD}_evt2_goodgti.fits+${GTI[$j]} colfile=${lccleangtifile}+1 colname="START STOP"
 endif
 @ j++
end
punlearn dmcopy
dmcopy "${obj}_${CCD}_evt2_goodgti.fits[@${obj}_${CCD}_evt2_goodgti.fits]" temp.fits clobber=yes
mv temp.fits ${obj}_${CCD}_evt2_goodgti.fits

punlearn dmcopy
dmcopy "${obj}_${CCD}_evt2_goodgti.fits[sky=region(${REGION})]"  ${obj}_${CCD}_evt2_clean_goodgti.fits clobber=yes



cat <<EOF

	Extracting a good GTI clean event list from 0.3-10.0 keV

EOF
# Event_2 file in the 0.3 - 10.0 keV only the appropraite chips
# cleaned for point sources for source free images
punlearn dmcopy
dmcopy \
 infile="${obj}_${CCD}_evt2_clean_goodgti.fits[energy=300:10000]" \
 outfile="${obj}_${CCD}_evt2_clean_goodgti_0.3-10.fits" clobber=yes
cat <<EOF

	Extracting a background with the same point source regions removed

EOF
# Event_2 file only appropriate chips cleaned for point 
# sources for spectra and normalizing background
punlearn dmcopy
dmcopy \
 infile="${obj}_${CCD}_BKG_evt2.fits[sky=region(${REGION})]" \
 outfile="${obj}_${CCD}_BKG_evt2_clean.fits" clobber=yes

if ( ${dopar} != "N" ) then

 punlearn dmcopy
 dmcopy \
  infile="${obj}_${CCD}_PARBKG_evt2.fits[sky=region(${REGION})]" \
  outfile="${obj}_${CCD}_PARBKG_evt2_clean.fits" clobber=yes

endif


cat <<EOF

	Extracting a "clean" background event list from 0.3-10.0 keV

EOF
# Event_2 file in the 0.3 - 10.0 keV only appropriate chips cleaned for point sources for source free images
punlearn dmcopy
dmcopy \
 infile="${obj}_${CCD}_BKG_evt2_clean.fits[energy=300:10000]" \
 outfile="${obj}_${CCD}_BKG_evt2_clean_0.3-10.fits" clobber=yes

if ( ${dopar} != "N" ) then
 punlearn dmcopy
 dmcopy \
  infile="${obj}_${CCD}_PARBKG_evt2_clean.fits[energy=300:10000]" \
  outfile="${obj}_${CCD}_PARBKG_evt2_clean_0.3-10.fits" clobber=yes

endif
cd ../BKG/


########################################	BKG_NORM version 1.0		##################################

cat <<EOF



#############################################################
		BKG_NORM v 1.0



EOF
set srcfile = "../event_2/${obj}_${CCD}_evt2_clean_goodgti.fits"

if ( $dopar != "N" ) then
 set TIMES = 2
else
 set TIMES = 1
endif

set N = 1
while ( $N <= ${TIMES} )
 if ( $N == 1 ) then
  set bkgfile = "../event_2/${obj}_${CCD}_BKG_evt2_clean.fits"
 else 
  set bkgfile = "../event_2/${obj}_${CCD}_PARBKG_evt2_clean.fits"
  set PAREXP = `fdump ${bkgfile}+0 STDOUT - - prdata=no page=no | grep EXPOSURE | awk '{printf"%10.10f\n",$2}'`
  fparkey value=${PAREXP} fitsfile=${bkgfile}+1 keyword=EXPOSURE
 endif
 
 if ( $N == 1 ) then
  punlearn dmextract
  dmextract "${srcfile}[EVENTS][energy>9500&&energy<12000][bin pi][sky=region(${bkgreg})]" src.pi clobber=yes
  set bkgpha = "bkg.pi"
 else
  set bkgpha = "parbkg.pi"
 endif
 punlearn dmextract
 dmextract "${bkgfile}[EVENTS][energy>9500&&energy<12000][bin pi][sky=region(${bkgreg})]" ${bkgpha} clobber=yes
 set COUNTS = `fstatistic src.pi COUNTS - | grep "The sum of the selected column is" | awk '{printf"%10.0f\n",$8}'`
 set BCOUNTS = `fstatistic ${bkgpha} COUNTS - | grep "The sum of the selected column is" | awk '{printf"%10.0f\n",$8}'`
 set n = `echo "scale=0;j+1" | bc -l`  
 set TIME = `fdump src.pi STDOUT - - prdata=no page=no | grep "EXPOSURE" | awk '{printf"%10.10f\n",$2}'` # I checked this with matlab and it is correct
 set BTIME = `fdump ${bkgpha} STDOUT - - prdata=no page=no | grep EXPOSURE | awk '{printf"%10.10f\n",$2}'`
 set RATE = `echo "scale=10;${COUNTS}/${TIME}" | bc -l`
 set BRATE = `echo "scale=10;${BCOUNTS}/${BTIME}" | bc -l`
 if ( $N == 1 ) then
  cat <<EOF
  
  
	The source appears to have a 
	deadtime corrected exposure time of: 
	${TIME} s. 


	The observation has:
	${COUNTS} in the background region.
	The Background has:
	${BCOUNTS} in the background region.
	
	

EOF
 else
  cat <<EOF
  
  
	The Source appears to have a 
	deadtime corrected exposure time of: 
	${TIME} s. 


	The observation has:
	${COUNTS} in the background region.
	The Closed Background has:
	${BCOUNTS} in the background region.
	
	

EOF
   
   
 endif
 
 set FACTOR = `echo "scale=10;${RATE}/${BRATE}" | bc -l`
 if ( $N == 1) then

  cat <<EOF

	For the background Region:
	
	Your source appears to have a rate of:     ${RATE} Counts s^-1 9.5-12 keV
	Your background appears to have a rate of: ${BRATE} Counts s^-1 9.5-12 keV

	This gives a background fudge factor of: ${FACTOR}

EOF
 
  echo "For the background Region"							>  BKG_NORM.txt
  echo " "										>> BKG_NORM.txt
  echo "Your source appears to have a rate of:     ${RATE} Counts s^-1 9.5-12 keV"	>> BKG_NORM.txt
  echo "Your background appears to have a rate of: ${BRATE} Counts s^-1 9.5-12 keV"	>> BKG_NORM.txt
  echo " "										>> BKG_NORM.txt
  echo "This gives a background fudge factor of: ${FACTOR}"				>> BKG_NORM.txt 

 else

  cat <<EOF

	For the background Region:
	
	Your source appears to have a rate of:     ${RATE} Counts s^-1 9.5-12 keV
	Your closed bkg appears to have a rate of: ${BRATE} Counts s^-1 9.5-12 keV

	This gives a background fudge factor of: ${FACTOR}

EOF
 
  echo "For the background Region"							>  PARBKG_NORM.txt
  echo " "										>> PARBKG_NORM.txt
  echo "Your source appears to have a rate of:     ${RATE} Counts s^-1 9.5-12 keV"	>> PARBKG_NORM.txt
  echo "Your closed bkg appears to have a rate of: ${BRATE} Counts s^-1 9.5-12 keV"	>> PARBKG_NORM.txt
  echo " "										>> PARBKG_NORM.txt
  echo "This gives a background fudge factor of: ${FACTOR}"				>> PARBKG_NORM.txt 
 endif  
 @ N++
end
cd ${top}/Images/  
cp ${local_tools}ADD_CHANDRA_IMAGES.csh .

cat <<EOF

	Creating Local Background for dmfilth.
	
EOF

set PNTSRCREG = ( `less ../data/event_2/srcs/Pntsrcs.reg` )
set i = 1
while ( $i <= $#PNTSRCREG )
 set rx = `echo ${PNTSRCREG[$i]} | awk -F "," '{print$3}'`
 set ry = `echo ${PNTSRCREG[$i]} | awk -F "," '{print$4}'`
 set rx2 = `echo "scale=10;${rx}*2" | bc -l`
 set ry2 = `echo "scale=10;${ry}*2" | bc -l`
 set prefix1 = `echo ${PNTSRCREG[$i]} | awk -F "," '{print$1}'`
 set prefix2 = `echo ${PNTSRCREG[$i]} | awk -F "," '{print$2}'`
 set suffix = `echo ${PNTSRCREG[$i]} | awk -F "," '{print$5}'`
 if ( $i == 1 ) then
  echo "${prefix1},${prefix2},${rx2},${ry2},${suffix}" > pntsrc_bkg.reg
 else
  echo "${prefix1},${prefix2},${rx2},${ry2},${suffix}" >> pntsrc_bkg.reg
 endif
 echo "-${prefix1},${prefix2},${rx},${ry},${suffix}" >> pntsrc_bkg.reg
 @ i++
end

cat <<EOF

 	Done!  


#############################################################
		CREATE OOTs v 1.0 (Readout Artifacts)




	Now creating the Readout Artifact events...


EOF

cd ${top}/data/event_2/
punlearn dmcopy
dmcopy "../event_1/acis_reset_evt1.fit[@${obj}_${CCD}_evt2_goodgti.fits]" ../event_1/tmp/${obj}_evt1_goodgti.fits clobber=yes
set TiMe  = `fdump ../event_1/tmp/${obj}_evt1_goodgti.fits+1 prdata=no page=no STDOUT - - | grep "ONTIME " | awk '{printf"%13.13f",$3}'`
set TiMe2 = `fdump ../event_1/tmp/${obj}_evt1_goodgti.fits+1 prdata=no page=no STDOUT - - | grep "EXPOSURE=" | awk '{printf"%13.13f",$2}'`


cd ../event_1/
if ( $destreak == "Y" ) then 
 if ( $vf == "yes" ) then
  cat <<EOF

	Since you want the events de-streaked, and you have a 
	very faint observation, I will process the events without 
	the vf-flag.  De-streak and then apply the vf-flag.

EOF
 endif

 if ( -f tmp/${obj}_ra_streaked_evt1.fits ) rm -f tmp/${obj}_ra_streaked_evt1.fits 
 make_readout_bg_Danny \
  tmp/${obj}_evt1_goodgti.fits \
  tmp/${obj}_ra_streaked_evt1.fits \
  ${asol} \
  ${badpix} \
  0 \
  CALDB \
  CALDB \
  "no" >& tmp/make_readout_bg_Danny.err



 punlearn dmcopy
 dmcopy \
  infile  = "tmp/${obj}_ra_streaked_evt1.fits[EVENTS][grade=0,2,3,4,6][status=${filterbits}]" \
  outfile = "tmp/${obj}_ra_streaked_filtered_evt1.fits" \
  clobber = yes

   cat <<EOF
 
	Destreaking the semi-filtered image....

EOF

 punlearn destreak
 destreak verbose=0 infile="tmp/${obj}_ra_streaked_filtered_evt1.fits" outfile="tmp/${obj}_ra_dstrk_evt1.fits" ccd_id="" filter=no clobber=yes
 punlearn dmcopy
 dmcopy \
  "tmp/${obj}_ra_dstrk_evt1.fits[EVENTS][status=xxxxxxxxxxxxxxxx0xxxxxxxxxxxxxxx]" "tmp/${obj}_ra_dstrk_clean_evt1.fits" clobber=yes
 punlearn dmcopy
 dmcopy "tmp/${obj}_ra_dstrk_evt1.fits[EVENTS][status=00000000000000001000000000000000]" "tmp/${obj}_ra_streaked_evt1.fits" clobber=yes


   cat <<EOF
 
	The image has been destreaked.  Check 
	data/event_1/tmp/${obj}_ra_streaked_evt1.fits
	to see if any significant emission has been
	removed.

EOF
 if ( $vf == "yes" ) then
 
  cat <<EOF
 
	Reprocessing the data for Cosmic
	Ray afterglow...

EOF

  punlearn acis_process_events
  acis_process_events \
   infile       = "tmp/${obj}_ra_dstrk_clean_evt1.fits" \
   outfile      = "tmp/${obj}_ra_new_evt1.fits" \
   acaofffile   = ${asol} \
   check_vf_pha = ${vf} \
   clobber      = yes \
   doevtgrade   = no \
   calculate_pi = no \
   apply_tgain  = no \
   apply_cti    = no \
   badpixfile   = NONE \
   stop         = none >& /tmp/error.log

  # Get rid of the silly warning
  set errorcheck = `less /tmp/error.log | grep "ERROR"`
  if ($#errorcheck > 0 ) echo $errorcheck
 else
  cp tmp/${obj}_ra_dstrk_clean_evt1.fits tmp/${obj}_ra_new_evt1.fits
 endif
else
 make_readout_bg_Danny \
  tmp/${obj}_evt1_goodgti.fits \
  tmp/${obj}_ra_new_evt1.fits \
  ${asol} \
  ${badpix} \
  0 \
  CALDB \
  CALDB \
  ${vf} >& tmp/make_readout_bg_Danny.err
endif
punlearn dmcopy

dmcopy \
 infile  = "tmp/${obj}_ra_new_evt1.fits[EVENTS][grade=0,2,3,4,6][status=${filterbits}]" \
 outfile = "tmp/${obj}_ra_flt_evt1.fits" \
 clobber = yes
cat <<EOF

	Creating New Event-2 files
EOF

punlearn dmcopy
dmcopy \
 infile  = "tmp/${obj}_ra_flt_evt1.fits[EVENTS][@${flt1}][cols -phas]" \
 outfile = "../event_2/${obj}_OOT_evt2.fits" \
 clobber = yes

cat <<EOF

 	Appending Fake GTI...
 
EOF
cd ../event_2/
set tstart = `fkeyprint ${obj}_OOT_evt2.fits+0 TSTART | grep "TSTART" | grep "Obs" | awk '{printf"%10.10f\n",$3}'`
set GTI = ( `fstruct "${obj}_OOT_evt2.fits" | grep BINTABLE | grep GTI | awk '{printf"%g\n",$1}'` )
set j = 1
while ( $j <= $#GTI )
 fdelhdu ${obj}_OOT_evt2.fits+${GTI[1]} no yes
 @ j++
end

set ONTIME = `echo "scale=13;${TiMe}*3.2/0.041" | bc -l`
set LIVETIME = `echo "scale=13;${TiMe2}*3.2/0.041" | bc -l`
# BC does not take exponential notation, so let's convert
# the exponent = fix( ontime/ln(10))
set LOG = `echo "scale=10;l(${ONTIME})/l(10)" | bc -l`
set EXP = `echo "scale=0;${LOG}/1" | bc -l`
set mantissa = `echo "scale=10;${ONTIME}/10^(${EXP})" | bc -l`
if ( ${EXP} < 10 ) set EXP = "0${EXP}"
set ontime = "${mantissa}E+${EXP}"
set LOG2 = `echo "scale=13;l(${LIVETIME})/l(10)" | bc -l`
set EXP2 = `echo "scale=0;${LOG}/1" | bc -l`
set mantissa2 = `echo "scale=13;${LIVETIME}/10^(${EXP})" | bc -l`
if ( ${EXP2} < 10 ) set EXP2 = "0${EXP2}"
set livetime = "${mantissa2}E+${EXP2}"
set tstop = `echo "scale=13;$tstart+$ONTIME" | bc -l`
echo "start 1d" > TMPJNKtempl
echo "stop 1d" >> TMPJNKtempl
echo "$tstart $tstop" > TMPJNKdata
fcreate TMPJNKtempl TMPJNKdata TMPJNKfits2 extname="GTI" clobber=yes
fappend TMPJNKfits2+1 ${obj}_OOT_evt2.fits
fparkey $ontime ${obj}_OOT_evt2.fits+0 ONTIME add=yes
fparkey $ontime ${obj}_OOT_evt2.fits+1 ONTIME add=yes
fparkey $livetime ${obj}_OOT_evt2.fits+0 EXPOSURE add=yes
fparkey $livetime ${obj}_OOT_evt2.fits+1 EXPOSURE add=yes
fparkey $livetime ${obj}_OOT_evt2.fits+0 LIVETIME add=yes
fparkey $livetime ${obj}_OOT_evt2.fits+1 LIVETIME add=yes
fparkey dummy ${obj}_OOT_evt2.fits+1 -TSTOP
# Make the OOTs like the Background with just a single exposure,
# livetime, and Ontime keyword
set LIVTIMES = ( `fdump ${obj}_OOT_evt2.fits prdata=no page=no STDOUT - - | grep LIVTIME | awk -F "LIVTIME" '{print$2}' | awk -F "=" '{print$1}'` )
set j = 1
while ($j <= $#LIVTIMES)
 set ccd = ${LIVTIMES[$j]}
 fparkey dummy ${obj}_OOT_evt2.fits+1 -ONTIME${ccd}
 fparkey dummy ${obj}_OOT_evt2.fits+1 -LIVTIME${ccd}
 fparkey dummy ${obj}_OOT_evt2.fits+1 -EXPOSUR${ccd}
 @ j++
end
rm TMPJNKtempl TMPJNKdata TMPJNKfits2

cat <<EOF

 Done!


	Using Vikhlinin's badpixfilter to filter bad pixels for both background and observation


EOF
cd ../event_1/
if ( -f "../event_2/${obj}_OOT_evt2_clean.fits" ) rm -f "../event_2/${obj}_OOT_evt2_clean.fits"
./badpixfilter "../event_2/${obj}_OOT_evt2.fits" "../event_2/${obj}_OOT_evt2_clean.fits" ${obj}_badpix

cat <<EOF

Done!

EOF

cd ../event_2/
punlearn dmcopy
dmcopy \
 infile="${obj}_OOT_evt2_clean.fits[ccd_id=${ccdid}][sky=region(${ccdreg})]" \
 outfile="${obj}_${CCD}_OOT_evt2.fits" clobber=yes option=all

# The keywords seem to get screwed up here so better to reset them.  (ONLY IN THE EVENTS HDU)
fparkey $ontime ${obj}_${CCD}_OOT_evt2.fits+1 ONTIME add=yes
fparkey $livetime ${obj}_${CCD}_OOT_evt2.fits+1 EXPOSURE add=yes
fparkey $livetime ${obj}_${CCD}_OOT_evt2.fits+1 LIVETIME add=yes



# Event_2 file in the 0.3 - 10.0 keV only I0123 chips for making Image
punlearn dmcopy
 dmcopy \
 infile="${obj}_${CCD}_OOT_evt2.fits[energy=300:10000]" \
 outfile="${obj}_${CCD}_OOT_evt2_0.3-10.fits" clobber=yes option=all

cat <<EOF

Done!


	Cleaning point sources from the event file

EOF

punlearn dmcopy
dmcopy \
 infile="${obj}_${CCD}_OOT_evt2.fits[sky=region(${REGION})]" \
 outfile="${obj}_${CCD}_OOT_evt2_clean.fits" clobber=yes option=all

if ( -f ${obj}_${CCD}_OOT_evt2_clean_goodgti.fits ) rm -f ${obj}_${CCD}_OOT_evt2_clean_goodgti.fits
ln -s ${obj}_${CCD}_OOT_evt2_clean.fits ${obj}_${CCD}_OOT_evt2_clean_goodgti.fits

punlearn dmcopy
dmcopy \
 infile="${obj}_${CCD}_OOT_evt2_clean.fits[energy=300:10000]" \
 outfile="${obj}_${CCD}_OOT_evt2_clean_0.3-10.fits" clobber=yes option=all

if ( -f ${obj}_${CCD}_OOT_evt2_clean_goodgti_0.3-10.fits) rm -f ${obj}_${CCD}_OOT_evt2_clean_goodgti_0.3-10.fits
ln -s ${obj}_${CCD}_OOT_evt2_clean_0.3-10.fits ${obj}_${CCD}_OOT_evt2_clean_goodgti_0.3-10.fits

set ootfile = "${obj}_${CCD}_OOT_evt2_clean.fits"
punlearn dmextract
dmextract "${ootfile}[EVENTS][energy>9500&&energy<12000][bin pi][sky=region(${bkgreg})]" ../BKG/src_OOT.pi clobber=yes


cat <<EOF

 	Done!  Check that your point sources aren't clumps of extended emission,
	and that your dmfilth background regions don't overlap with point sources.
	In fact, you should check all your files and error logs to make sure
	everything is in order.
	

EOF
