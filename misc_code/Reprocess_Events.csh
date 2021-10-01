#!/bin/tcsh -f


if ( $#argv != 2 ) then
 cat <<EOF 

usage 
 Reprocess_Events.csh  \
	 path_to_CXC.log \
	 bkg_pntsrcs.reg \


path_to_CXC.log			is the path to the CXC.log (.e.g.
				PASS_01/CXCRI)
			
bkg_pntsrcs.reg			The region file with the point sources and
				background regions for use with dmfilth.  This
				script extracts the point sources from this file,
				as well as using it for dmfilth.  Make sure you
				have all the point sources begin with -.


e.g.
Reprocess_Events.csh \
 "PASS_01/CXCRI" \
 "PASS_01/Images/pntsrc_bkg.reg"

EOF
 exit
endif

cat <<EOF

		  Reprocess Events v1.2
		  05-07-24


#############################################################
# The author assumes no responsibility for this tool or its #
# misuse.						    #
# Comments, suggestions, and improvements are welcome.	    #
#############################################################

Danny Hudson
dhudson@astro.uni-bonn.de
2005-July-24

EOF

set cxc      =   $1
set pntsrc   =   $2
# SET LOCAL DIRECTORY
set local_dir = `pwd`

# Determine if relative or absolute paths were given
# If they are absolute they should begin with a backslash
set firstchar = `echo $cxc | awk -F "/" '{print$1}'`
if ( $#firstchar != 0 ) set cxc = ${local_dir}/${cxc}
set firstchar = `echo $pntsrc | awk -F "/" '{print$1}'`
if ( $#firstchar != 0 ) set pntsrc = ${local_dir}/${pntsrc}

cat <<EOF

 Checking the Directories and Files....
 
EOF

if (! -f $cxc/CXC.log ) then
 
 cat <<EOF
 
 The CXCRI.log file
 does not seem to exist in ${cxc}.
 Exiting...

EOF
 exit
endif

cat <<EOF

 Using the CXC.log file to set variables...
 
EOF
set obj = `less ${cxc}/CXC.log | grep OBJECT | awk '{print$3}'`
set output = `less ${cxc}/CXC.log | grep MAIN_DIR | awk '{print$3}'`
set CCD = `less ${cxc}/CXC.log | grep CCDLIST | awk '{print$3}'`
set Chips = ( `less ${cxc}/CXC.log | grep CHIPS | awk -F = '{print$2}'` )
set asol = ( `less ${cxc}/CXC.log | grep ASOL | awk -F = '{print$2}'` )

cat <<EOF

 Using:
 OBJECT 		: ${obj}
 OUTPUT DIRECTORY 	: ${output}
 CCD LIST		: ${CCD}
 Chips			: ${Chips}
 ATTITUDE FILE		: ${asol}
 
 
 Determining the Background Normalization Factor...
 
EOF
set FIRST_CHIP = ( "I0" "I1" "I2" "I3" "S0" "S1" "S2" "S3" "S4" "S5" )

if ( ! -f ${output}/data/BKG/BKG_NORM.txt ) then

 cat <<EOF
 
 The background normalization file
 ${output}/data/BKG/BKG_NORM.txt
 does not appear to exist.  It has
 either been erased, moved, or you are
 using the wrong directory.
 Exiting....

EOF
 exit
endif

# NOTE ON THE BACKGROUND NORMALIZATION
# The background normalization is calculated
# using the original point-sources found by
# wavdetect and not the updated sources by the
# user.  My logic is this:  the bkg normalization
# should be a source free region (and even if the
# user feels that the whole chip(s) are source free
# in the 9.5-10 keV range ( cool cluster, etc...)), so
# removing additional emission (esp clumps of gas) is
# probably better, since we are only interested in
# background emission levels.

# Check the Background Normalization
# This should be in ${output}/data/BKG/BKG_NORM.txt
set FACTOR = `less ${output}/data/BKG/BKG_NORM.txt | \
              grep "This gives a background fudge factor of:" | \
	      awk -F "This gives a background fudge factor of:" '{print$2}'`
	      
# There is a small bug in Reduce_Chandra that causes it to print this
# line several times (I'll fix this later).  In the mean-time...the work-around
# is to take one of these (I'll choose the last one since it should be right
# (occasionally the earlier ones are blank) I think I just fixed it.
set FACTOR = ${FACTOR[$#FACTOR]}

cat <<EOF

	Using $FACTOR as the background
	normalization factor.

EOF

# Make a file of point sources
less ${pntsrc} | grep "-" | awk -F "-" '{print$2}' > ${output}/data/event_2/Point_Sources.reg
set region = "${output}/data/event_2/Point_Sources.reg"


cat <<EOF

	Setting up Region Files...
	${output}/Regions/${CCD}.reg -> CCD Region only
	${output}/Regions/${CCD}_nopntsrcs.reg -> CCD Region no point sources updated
	${output}/Regions/background_pntsrcs.reg -> Background for filled pntsrcs (dmfilth)

EOF

set ccdreg = $output/Regions/${CCD}.reg
if ( -f ${CCD}_nopntsrcs.reg ) mv ${CCD}_nopntsrcs_old.reg
cp $output/Regions/${CCD}.reg $output/Regions/${CCD}_nopntsrcs.reg
less ${pntsrc} | grep "-" | awk '{print$1}' >> ${output}/Regions/${CCD}_nopntsrcs.reg
set REGION = "${output}/Regions/${CCD}_nopntsrcs.reg"
set ALLREG = ( `less ${pntsrc}` )
set i = 1
set chkfirst = "y"
while ( $i <= $#ALLREG )
 set minuschk = `echo ${ALLREG[$i]} | grep "-"`
 if ( $#minuschk == 0 ) then
  if ( $chkfirst == "y" ) then
   set chkfirst = "n"
   set j = `echo "$i + 1" | bc -l`
   echo "${ALLREG[$i]}${ALLREG[$j]}" >  ${output}/Regions/background_pntsrcs.reg
  else
   set j = `echo "$i + 1" | bc -l`
   echo "${ALLREG[$i]}${ALLREG[$j]}"  >>  ${output}/Regions/background_pntsrcs.reg
  endif
 endif
 @ i++
 @ i++
end

set bkgpntsrcs = "${output}/Regions/background_pntsrcs.reg"

# Determine the BADPIX file
set badpix = ${output}/data/event_2/${obj}_badpix_used.fits
# SET THE ARDLIB TO THE BADPIXFILE
punlearn ardlib
foreach j ( $Chips )
 pset \
  ardlib AXAF_ACIS${j}_BADPIX_FILE = \
      "${badpix}[BADPIX${j}]"
end


cat <<EOF

	Reprocessing the data with the new point-source file
	
EOF
# What data files do we have? and need?
# have				-> 		status
# ${obj}_evt2.fits				raw (no processing after evt2 status)
# ${obj}_${CCD}_evt2.fits			gc chip screened (cs) 
# ${obj}_${CCD}_evt2_0.3-10.fits		gc-cs 0.3-10 keV
# ${obj}_${CCD}_evt2_clean.fits			gc-cs-psr (must be reprocessed)
# ${obj}_${CCD}_evt2_clean_goodgti.fits		gc-cs-psr GTI (must be reprocessed)
# ${obj}_${CCD}_evt2_clean_goodgti_0.3-10..fits	gc-cs-psr GTI (must be reprocessed) 0.3-10 keV

cd ${output}/data/event_2/
if (! -d old ) mkdir old
if ( -f ${obj}_${CCD}_evt2_clean.fits) mv ${obj}_${CCD}_evt2_clean.fits old/
if ( -f ${obj}_${CCD}_evt2_clean_goodgti.fits ) mv ${obj}_${CCD}_evt2_clean_goodgti.fits old/
if ( -f ${obj}_${CCD}_evt2_clean_goodgti_0.3-10.fits ) mv ${obj}_${CCD}_evt2_clean_goodgti_0.3-10.fits old/
if ( -f ${obj}_${CCD}_OOT_evt2_clean.fits) mv ${obj}_${CCD}_OOT_evt2_clean.fits old/
if ( -f ${obj}_${CCD}_OOT_evt2_clean_0.3-10.fits ) mv ${obj}_${CCD}_OOT_evt2_clean_0.3-10.fits old/
if ( -f ${obj}_${CCD}_BKG_evt2_clean.fits ) mv ${obj}_${CCD}_BKG_evt2_clean.fits old/
if ( -f ${obj}_${CCD}_BKG_evt2_clean_0.3-10.fits ) mv ${obj}_${CCD}_BKG_evt2_clean_0.3-10.fits old/
if ( -f ${obj}_${CCD}_PARBKG_evt2_clean.fits )  mv ${obj}_${CCD}_PARBKG_evt2_clean.fits old/
if ( -f ${obj}_${CCD}_PARBKG_evt2_clean_0.3-10.fits )  mv ${obj}_${CCD}_PARBKG_evt2_clean_0.3-10.fits old/

punlearn dmcopy
dmcopy "${obj}_${CCD}_evt2.fits[sky=region(${REGION})]" ${obj}_${CCD}_evt2_clean.fits clobber=yes opt=all
punlearn dmcopy
dmcopy "${obj}_${CCD}_evt2_goodgti.fits[sky=region(${REGION})]" ${obj}_${CCD}_evt2_clean_goodgti.fits opt=all

punlearn dmcopy
dmcopy "${obj}_${CCD}_OOT_evt2.fits[sky=region(${REGION})]" ${obj}_${CCD}_OOT_evt2_clean.fits clobber=yes option=all
punlearn dmcopy
dmcopy "${obj}_${CCD}_OOT_evt2_clean.fits[energy=300:10000]" ${obj}_${CCD}_OOT_evt2_clean_0.3-10.fits clobber=yes option=all


punlearn dmcopy
dmcopy "${obj}_${CCD}_evt2_clean_goodgti.fits[energy=300:10000]" ${obj}_${CCD}_evt2_clean_goodgti_0.3-10.fits clobber=yes
punlearn dmcopy
dmcopy "${obj}_${CCD}_OOT_evt2_clean.fits[energy=300:10000]" ${obj}_${CCD}_OOT_evt2_clean_0.3-10.fits clobber=yes option=all
punlearn dmcopy
dmcopy infile="${obj}_${CCD}_BKG_evt2.fits[sky=region(${REGION})]" outfile="${obj}_${CCD}_BKG_evt2_clean.fits" clobber=yes
punlearn dmcopy
dmcopy infile="${obj}_${CCD}_BKG_evt2_clean.fits[energy=300:10000]" outfile="${obj}_${CCD}_BKG_evt2_clean_0.3-10.fits" clobber=yes
punlearn dmcopy
dmcopy infile="${obj}_${CCD}_BKG_evt2.fits[energy=300:10000][sky=region(${ccdreg})]" outfile="${obj}_${CCD}_BKG_evt2_0.3-10.fits" clobber=yes
if ( -f  ${obj}_${CCD}_PARBKG_evt2.fits ) then
 punlearn dmcopy
 dmcopy infile="${obj}_${CCD}_PARBKG_evt2.fits[sky=region(${REGION})]" outfile="${obj}_${CCD}_PARBKG_evt2_clean.fits" clobber=yes
 punlearn dmcopy
 dmcopy infile="${obj}_${CCD}_PARBKG_evt2_clean.fits[energy=300:10000]" outfile="${obj}_${CCD}_PARBKG_evt2_clean_0.3-10.fits" clobber=yes
 punlearn dmcopy
 dmcopy infile="${obj}_${CCD}_PARBKG_evt2.fits[energy=300:10000][sky=region(${ccdreg})]" outfile="${obj}_${CCD}_PARBKG_evt2_0.3-10.fits" clobber=yes
endif



cat <<EOF

Done!
###########################################################


Now you can use create Images.



EOF
