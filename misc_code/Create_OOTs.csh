#!/bin/tcsh -f

if ( $#argv != 1 ) then

 cat <<EOF
 
 usage Create_OOTs.csh path_to_CXC.log
 
 e.g.
 ./Create_OOTs.csh PASS_01/CXCRI
 
EOF
 exit
endif

cat <<EOF

		  Create_OOTs v1.0
		  05-07-21


#############################################################
# The author assumes no responsibility for this tool or its #
# misuse.						    #
# Comments, suggestions, and improvements are welcome.	    #
#############################################################

Danny Hudson
dhudson@astro.uni-bonn.de
2005-July-21

EOF

set cxc      =   $1
set local_dir = `pwd`
# Determine if relative or absolute paths were given
# If they are absolute they should begin with a backslash
set firstchar = `echo $cxc | awk -F "/" '{print$1}'`
if ( $#firstchar != 0 ) set cxc = ${local_dir}/${cxc}
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
set destreak = ( `less ${cxc}/CXC.log | grep DESTREAK | awk -F = '{print$2}'` )
set DATAMODE = ( `less ${cxc}/CXC.log | grep MODE | awk -F = '{print$2}'` )
set badpix = ( `less ${cxc}/CXC.log | grep BADPIXFILE | awk -F = '{print$2}'` )
set pathe = ( `less ${cxc}/CXC.log | grep RAW_DIR | awk -F = '{print$2}'` )
set flt1 = `ls ${pathe}/secondary/*flt1*`
if (! -f ${flt1} ) then
 cat <<EOF
 
The filter file does not appear to be in the secondary folder!
Exiting...
EOF
 exit
endif
set ccdreg = ( `less ${cxc}/CXC.log | grep CCD_REG | awk -F = '{print$2}'` )
set REGION = "${output}/Regions/${CCD}_nopntsrcs.reg"

if ( $DATAMODE == "VFAINT" ) then 
 set vf = yes
else
 set vf = no
endif

set filterbits = "00000x000000000000000xx0xxxxxxxx"
cat <<EOF

 Using:
 OBJECT 		: ${obj}
 OUTPUT DIRECTORY 	: ${output}
 CCD LIST		: ${CCD}
 Chips			: ${Chips}
 ATTITUDE FILE		: ${asol}
 DESTREAK		: ${destreak}
 MODE			: ${DATAMODE}
 BADPIXFILE		: ${badpix}
 FILTER			: ${flt1}
 CCD REGION		: ${ccdreg}
 CLEAN REGION		: ${REGION}

	Now creating the Readout Artifact events...

 
EOF

set FIRST_CHIP = ( "I0" "I1" "I2" "I3" "S0" "S1" "S2" "S3" "S4" "S5" )
cd ${output}/data/event_2/
set j = 1
while ($j <= $#Chips)
 set ccd = ${Chips[$j]}
 set k = `echo "scale=0;$ccd+1" | bc -l`
 if ( $j == 1 ) then
  dmcopy "../event_1/acis_reset_evt1.fit[@${obj}_${FIRST_CHIP[$k]}_clean_GTI.fits]" ../event_1/tmp/${obj}_evt1_goodgti.fits clobber=yes
  set mrbDccd = "${ccd}"
  set TiMe = `fdump ../event_1/tmp/${obj}_evt1_goodgti.fits+1 prdata=no page=no STDOUT - - | grep "ONTIME${ccd}" | awk '{print$3}'`
 else
  dmcopy "../event_1/tmp/${obj}_evt1_goodgti.fits[@${obj}_${FIRST_CHIP[$k]}_clean_GTI.fits]" temp.fits clobber=yes
  mv temp.fits ../event_1/tmp/${obj}_evt1_goodgti.fits
  set mrbDccd = "${mrbDccd},${ccd}"
  set TiMe = ${TiMe},`fdump ../event_1/tmp/${obj}_evt1_goodgti.fits+1 prdata=no page=no STDOUT - - | grep "ONTIME${ccd}" | awk '{print$3}'`
 endif
 @ j++
end

if ( $destreak == "Y" ) then 
 cat <<EOF

	Since you want the events de-streaked, I will process the
	events without the vf-flag and badpixfile.  De-streak 
	and then apply the vf-flag (if needed) and the badfixfile.

EOF

cd ../event_1/
 make_readout_bg_Danny \
  tmp/${obj}_evt1_goodgti.fits \
  tmp/${obj}_ra_streaked_evt1.fits \
  ${asol} \
  NONE \
  ${TiMe} \
  CALDB \
  CALDB \
  ${mrbDccd} \
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
	
	Reprocessing the data for badpix and Cosmic
	Ray afterglow (for VFAINT only)

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
  badpixfile   = ${badpix} \
  stop         = none >& /tmp/error.log
  
  # Get rid of the silly warning
  set errorcheck = `less /tmp/error.log | grep "ERROR"`
  if ($#errorcheck > 0 ) echo $errorcheck

else
 make_readout_bg_Danny \
  tmp/${obj}_evt1_goodgti.fits \
  tmp/${obj}_ra_new_evt1.fits \
  ${asol} \
  ${badpix} \
  ${TiMe} \
  CALDB \
  CALDB \
  ${mrbDccd} \
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

 Done!


	Using Vikhlinin's badpixfilter to filter bad pixels for both background and observation


EOF
if ( -f "../event_2/${obj}_OOT_evt2_clean.fits" ) rm -f "../event_2/${obj}_OOT_evt2_clean.fits"
./badpixfilter "../event_2/${obj}_OOT_evt2.fits" "../event_2/${obj}_OOT_evt2_clean.fits" ${obj}_badpix
cat <<EOF

Done!

EOF

punlearn dmcopy
dmcopy \
 infile="../event_2/${obj}_OOT_evt2_clean.fits[ccd_id=${mrbDccd}][sky=region(${ccdreg})]" \
 outfile="../event_2/${obj}_${CCD}_OOT_evt2.fits" clobber=yes

# Event_2 file in the 0.3 - 10.0 keV only I0123 chips for making Image
punlearn dmcopy
 dmcopy \
 infile="../event_2/${obj}_${CCD}_OOT_evt2.fits[energy=300:10000]" \
 outfile="../event_2/${obj}_${CCD}_OOT_evt2_0.3-10.fits" clobber=yes

cat <<EOF

Done!


	Cleaning point sources from the event file

EOF
cd ../event_2/
punlearn dmcopy
dmcopy \
 infile="${obj}_${CCD}_OOT_evt2.fits[sky=region(${REGION})]" \
 outfile="${obj}_${CCD}_OOT_evt2_clean.fits" clobber=yes

cp ${obj}_${CCD}_OOT_evt2_clean.fits ${obj}_${CCD}_OOT_evt2_clean_goodgti.fits

punlearn dmcopy
dmcopy \
 infile="${obj}_${CCD}_OOT_evt2_clean_goodgti.fits[energy=300:10000]" \
 outfile="${obj}_${CCD}_OOT_evt2_clean_goodgti_0.3-10.fits" clobber=yes

cat <<EOF

 	Done!  
		

EOF
