#!/bin/tcsh -f


# History
# 05-07-27 changed energy filtering to energy<=${loe} && energy<${hie}
# 	   so we don't skip over energies between bins.
#
# 05-07-24-25 Added OOTs
#
# 04-11-30  Added binning as user input

if ( $#argv != 7 ) then
 cat <<EOF 

usage 
 SIC.csh  \
	 path_to_CXC.log \
	 output-directory \
	 loenergy \
	 hienergy \
	 bin \
	 sigma1 \
	 sigma2

path_to_CXC.log			is the path to the CXC.log (.e.g.
				PASS_01/CXCRI)

output-directory		Where the files should be stored.

loenergy 			The low energy range(s) for the image in eV.

hienergy			The high energy range(s) for the image in eV.

bin				The amount the image should be binned.

sigma1				The level sigma to smooth to...

sigma2				The level sigma to consider p.s.

e.g.
SIC.csh \
 "PASS_01/CXCRI" \
 "PASS_01/Images/TEMPERATURE_SMOOTHED_IMAGES \
 "300 501 701  901 1101 1301 1501 1701 1901 2101 2301 2501 2701 2901 3101 3301 3501 3701 3901 4101 4301 4501 4701 4901 5101 5301 5501 5701 5901 6101 6301 6501 6701" \
 "500 700 900 1100 1300 1500 1700 1900 2100 2300 2500 2700 2900 3100 3300 3500 3700 3900 4100 4300 4500 4700 4900 5100 5300 5500 5700 5900 6100 6300 6500 6700 7000" \
  8
  "5" \
  "6"

EOF
 exit
endif

cat <<EOF

		  SIC (Simple Image Creator) v1.4
		  05-07-27


#############################################################
# The author assumes no responsibility for this tool or its #
# misuse.						    #
# Comments, suggestions, and improvements are welcome.	    #
#############################################################

Danny Hudson
dhudson@jca.umbc.edu
2005-July-27 

EOF

set cxc      =   $1
set outdir   =   $2
set loenergy = ( $3 )
set hienergy = ( $4 )
set BIN	     =   $5
set sigma    = ( $6 $7 )
# SET LOCAL DIRECTORY
set local_dir = `pwd`

# Determine if relative or absolute paths were given
# If they are absolute they should begin with a backslash
set firstchar = `echo $cxc | awk -F "/" '{print$1}'`
if ( $#firstchar != 0 ) set cxc = ${local_dir}/${cxc}
set firstchar = `echo $outdir | awk -F "/" '{print$1}'`
if ( $#firstchar != 0 ) set outdir = ${local_dir}/${outdir}

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

 Determining the Background Normalization Factor...
 
EOF

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
#echo "field()" > ${output}/event_2/Point_Sources.reg
set region = "${output}/data/event_2/Point_Sources.reg"


# Resetting the nomenclature for the Chips
set FIRST_CHIP = ( "I0" "I1" "I2" "I3" "S0" "S1" "S2" "S3" "S4" "S5" )

cat <<EOF

	Using Region Files...
	${output}/Regions/${CCD}.reg -> CCD Region only
	${output}/Regions/${CCD}_nopntsrcs.reg -> CCD Region no point sources updated
	${output}/Regions/background_pntsrcs.reg -> Background for filled pntsrcs (dmfilth)

EOF
set ccdreg = $output/Regions/${CCD}.reg
set REGION = "${output}/Regions/${CCD}_nopntsrcs.reg"
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

# Determining the exposure time of each CCD.
cd ${output}/data/BKG/
set TIME = `fdump src.pi STDOUT - - prdata=no page=no | grep "EXPOSURE" | awk '{printf"%10.10f\n",$2}'` # I checked this with matlab and it is correct
set BTIME = `fdump bkg.pi STDOUT - - prdata=no page=no | grep EXPOSURE | awk '{printf"%10.10f\n",$2}'`
set OTIME = `fdump src_OOT.pi STDOUT - - prdata=no page=no | grep EXPOSURE | awk '{printf"%10.10f\n",$2}'`

  cat <<EOF

  
	The source appears to have a 
	deadtime corrected exposure time of: 
	${TIME} s.



EOF




cd ${outdir}
# Set up the directories
if (! -d FINAL_IMAGE) mkdir FINAL_IMAGE
if (! -d EXP_MAPS) mkdir EXP_MAPS
if (! -d INST_MAPS) mkdir INST_MAPS
if (! -d images) mkdir images
if (! -d tmp ) mkdir tmp
#if (! -f ADD_CHANDRA_IMAGES.csh ) cp /data/ginseng/dhudson/Chandra_tools/ADD_CHANDRA_IMAGES.csh .
#if (! -f ADD_CHANDRA_IMAGES.csh ) cp /upc76_2/dhudson/Chandra_Tools/ADD_CHANDRA_IMAGES.csh .
if (! -f ADD_CHANDRA_IMAGES.csh ) cp /aibn156_a/dhudson/Chandra_Tools/ADD_CHANDRA_IMAGES.csh .

########################################	Make_Image version 3.0		##################################
cat <<EOF



#############################################################
		Make_Image v 3.0



EOF
#set evt_holes = "${output}/data/event_2/${obj}_${CCD}_evt2_clean_goodgti_0.3-10.fits"
#set evt =  ${obj}_evt2_filled.fits
set evtraw = "${output}/data/event_2/${obj}_${CCD}_evt2_clean_goodgti.fits"
set evt =    "${output}/data/event_2/${obj}_${CCD}_evt2_clean_goodgti_0.3-10.fits"

set bkg =    "${output}/data/event_2/${obj}_${CCD}_BKG_evt2_0.3-10.fits"
set oot =    "${output}/data/event_2/${obj}_${CCD}_OOT_evt2.fits"

set SIZE = `echo "scale=0;8192/${BIN}" | bc -l`
set SRCEXP = ${TIME}
set BKGEXP = ${BTIME}
set OOTEXP = ${OTIME}
# Reduce the background normalization by 1.3% in order to account for OOTs
set BKGFACTOR = `echo "scale=10;${FACTOR}-0.041/3.2" | bc -l`
set DTCOR=0.98733739787229


cat <<EOF

	Determining the aspect history

EOF
punlearn asphist
foreach c ($Chips)
 asphist \
  infile  ="${asol}[@${evtraw}[GTI${c}]]" \
  outfile ="tmp/asphist_${c}.fits" \
  evtfile ="${evt}" \
  dtffile = "" \
  clobber = yes \
  verbose = 0
end


cat << EOF

	Making Statistical Images (raw)
	to use with CSMOOTH...
	
EOF
set loe_id = `echo "scale=2;${loenergy[1]} / 1000" | bc -l`
set hie_id = `echo "scale=2;${hienergy[$#hienergy]}/ 1000" | bc -l`
dmcopy \
 "${evt}[bin x=${BIN},y=${BIN}][sky = region(${REGION})]" \
 tmp/${obj}_raw_counts_image_swiss_cheese.fits \
 clobber=yes

dmcopy \
 "${bkg}[bin x=${BIN},y=${BIN}][sky = region(${ccdreg})]" \
 FINAL_IMAGE/${obj}_raw_counts_bkg.fits \
 clobber=yes

dmcopy \
 "${oot}[bin x=${BIN},y=${BIN}][sky = region(${ccdreg})]" \
 FINAL_IMAGE/${obj}_raw_counts_oot.fits \
 clobber=yes

cat <<EOF

	Filling the holes in the source image....
	
EOF

dmfilth infile="tmp/${obj}_raw_counts_image_swiss_cheese.fits" outfile="FINAL_IMAGE/${obj}_raw_counts.fits" method=POISSON \
        srclist="@${region}" bkglist="@${bkgpntsrcs}" randseed=123 clobber=yes verbose=0

cat <<EOF

 Done!
	
EOF

cat <<EOF

	Smoothing with csmooth....
	This make take awhile....
	
EOF

csmooth \
 infile="FINAL_IMAGE/${obj}_raw_counts.fits" \
 sclmap=none \
 outfile="FINAL_IMAGE/${obj}_raw_${sigma[1]}-${sigma[2]}_smooth.fits" \
 outsigfile="FINAL_IMAGE/${obj}_${sigma[1]}-${sigma[2]}_sig.fits" \
 outsclfile="FINAL_IMAGE/${obj}_${sigma[1]}-${sigma[2]}_kernel.fits" \
 conmeth=fft conkerneltype=gauss sigmin=${sigma[1]} sigmax=${sigma[2]} \
 sclmin=INDEF sclmax=INDEF sclmode=compute clobber=yes verbose=5

cat << EOF

	Done!
	
	
EOF



cat <<EOF

	Making exposure corrected images....
	
EOF

set COUNTER = 1
set ERATE = 0
set ERMAX = 0
while ($COUNTER <= $#loenergy )
 set loe_id = `echo "scale=2;${loenergy[$COUNTER]} / 1000" | bc -l`
 set hie_id = `echo "scale=2;${hienergy[$COUNTER]}/ 1000" | bc -l`

 cat <<EOF

		$loe_id to $hie_id keV

EOF
 cat <<EOF

		Extracting Energy Filtered event list

EOF
 if ( ${COUNTER} == $#loenergy ) then
  dmcopy \
   infile ="${evt}[energy=${loenergy[$COUNTER]}:${hienergy[$COUNTER]}]" outfile="tmp/evt2_${loe_id}-${hie_id}.fits" clobber=yes
 else
  dmcopy \
   infile ="${evt}[energy>=${loenergy[$COUNTER]}&& energy < ${hienergy[$COUNTER]}]" outfile="tmp/evt2_${loe_id}-${hie_id}.fits" clobber=yes
 
 endif

 cat <<EOF

		Creating Histogram to determine highest count rate energy

EOF
 dmextract \
  infile  ="tmp/evt2_${loe_id}-${hie_id}.fits[bin energy=${loenergy[$COUNTER]}:${hienergy[$COUNTER]}:20]" outfile ="tmp/obj_histogram_energy_${loe_id}-${hie_id}.fits" \
  opt = generic clobber = yes
 set max_rate = `dmstat "tmp/obj_histogram_energy_${loe_id}-${hie_id}.fits[cols count_rate]" | grep max | awk '{print $2;}'`
 set max_rate = `echo "$max_rate * 0.99" | bc -l`
 set max_energy = \
  `dmlist "tmp/obj_histogram_energy_${loe_id}-${hie_id}.fits[count_rate > ${max_rate}][cols energy,count_rate]" data,clean | head -2 | tail -1 | awk '{print $1;}'`
 set max_energy = `echo "$max_energy / 1000" | bc -l`
 set energy_id = `echo "scale=2;${max_energy}/1" | bc -l`
 set MAX_CHECK = `echo "scale=0;(${max_rate}*10000)/1" | bc -l`
 if ( ${MAX_CHECK} > $ERATE ) then
  set ERATE = ${MAX_CHECK}
  set ERMAX = ${energy_id}
 endif
 
 cat <<EOF

		Making an instrument and exposure map for each chip at $energy_id keV 

EOF
 set n = 0;
 foreach c ($Chips)
  set i = `echo "scale=0;${c}+1" | bc -l`
  @ n++
  punlearn mkinstmap
  mkinstmap \
   obsfile = "tmp/asphist_${c}.fits[asphist]" \
   outfile = "INST_MAPS/instmap_${energy_id}keV_${FIRST_CHIP[$i]}.fits" \
   det = ACIS-${c} \
   mirror = HRMA \
   pixelgrid = "1:1024:#1024,1:1024:#1024" \
   ardlibparfile = ardlib.par \
   spectrumfile = NONE \
   monoenergy = ${max_energy} \
   verbose = 0 \
   grating = NONE \
   maskfile = NONE \
   clobber = yes 

  mkexpmap \
   instmapfile = "INST_MAPS/instmap_${energy_id}keV_${FIRST_CHIP[$i]}.fits" \
   outfile = "tmp/expmap_${energy_id}keV_${FIRST_CHIP[$i]}.fits" \
   xygrid = "0.5:8192.5:#${SIZE},0.5:8192.5:#${SIZE}" \
   asphistfile = tmp/asphist_${c}.fits \
   useavgaspect = no \
   normalize = no \
   verbose = 0 \
   clobber = yes

  
  if ($COUNTER == 1) then
   cat <<EOF

		Determining a "time-only" exposure map for background time normalization

EOF
    # MAKE A TIME NORMALIZED EXPOSURE FOR THE BACKGROUND
    set EXPOSURE = `echo "scale=10;${SRCEXP}/${BKGEXP}" | bc -l`
    fimgtrim "tmp/expmap_${energy_id}keV_${FIRST_CHIP[$i]}.fits" threshlo=0.0 \
     threshup=1 const_lo =0.0 const_up=${EXPOSURE} outfile="tmp/BKG_EXPTIME_NORM_${FIRST_CHIP[$i]}.fits" clobber=yes
    set OOTEXPOSURE = `echo "scale=10;${SRCEXP}/${OOTEXP}" | bc -l`
    fimgtrim "tmp/expmap_${energy_id}keV_${FIRST_CHIP[$i]}.fits" threshlo=0.0 \
     threshup=1 const_lo =0.0 const_up=${OOTEXPOSURE} outfile="tmp/OOT_EXPTIME_NORM_${FIRST_CHIP[$i]}.fits" clobber=yes
  endif
 end
 if ($COUNTER == 1) then
  if ($#Chips > 1) then
  
   ls tmp/BKG_EXPTIME_NORM_I*.fits > BKG_EXPTIME.lis
   cat BKG_EXPTIME.lis

   dmregrid \
    infile="@BKG_EXPTIME.lis" \
    outfile= "EXP_MAPS/BKG_EXPTIME_NORM_${CCD}.fits" \
    bin="1:${SIZE}:1,1:${SIZE}:1" \
    rotangle=0 \
    npts=1 \
    xoffset=0 \
    yoffset=0 \
    rotxcenter=0 \
    rotycenter=0 \
    clobber=yes

   punlearn dmcopy
   dmcopy \
    infile  ="EXP_MAPS/BKG_EXPTIME_NORM_${CCD}.fits[sky=region(${ccdreg})]" \
    outfile ="EXP_MAPS/BKG_EXPTIME_NORM_${CCD}.fits" \
    clobber = yes

   ls tmp/OOT_EXPTIME_NORM_I*.fits > OOT_EXPTIME.lis
   cat OOT_EXPTIME.lis

   dmregrid \
    infile="@OOT_EXPTIME.lis" \
    outfile= "EXP_MAPS/OOT_EXPTIME_NORM_${CCD}.fits" \
    bin="1:${SIZE}:1,1:${SIZE}:1" \
    rotangle=0 \
    npts=1 \
    xoffset=0 \
    yoffset=0 \
    rotxcenter=0 \
    rotycenter=0 \
    clobber=yes

   punlearn dmcopy
   dmcopy \
    infile  ="EXP_MAPS/OOT_EXPTIME_NORM_${CCD}.fits[sky=region(${ccdreg})]" \
    outfile ="EXP_MAPS/OOT_EXPTIME_NORM_${CCD}.fits" \
    clobber = yes
   
  else

   punlearn dmcopy
   dmcopy \
    infile  ="tmp/BKG_EXPTIME_NORM_${CCD}.fits[sky=region(${ccdreg})]" \
    outfile = "EXP_MAPS/BKG_EXPTIME_NORM_${CCD}.fits" clobber=yes

   punlearn dmcopy
   dmcopy \
    infile  ="tmp/OOT_EXPTIME_NORM_${CCD}.fits[sky=region(${ccdreg})]" \
    outfile = "EXP_MAPS/OOT_EXPTIME_NORM_${CCD}.fits" clobber=yes

  endif
 endif
 if ($#Chips > 1) then
  ls tmp/expmap_${energy_id}keV_I*.fits > expmap_${energy_id}keV.lis
  cat expmap_${energy_id}keV.lis


  dmregrid \
   infile="@expmap_${energy_id}keV.lis" \
   outfile= "tmp/expmap_${energy_id}keV_${CCD}.fits" \
   bin="1:${SIZE}:1,1:${SIZE}:1" \
   rotangle=0 \
   npts=1 \
   xoffset=0 \
   yoffset=0 \
   rotxcenter=0 \
   rotycenter=0 \
   clobber=yes
 endif
 dmcopy \
  infile  ="tmp/expmap_${energy_id}keV_${CCD}.fits[sky=region(${ccdreg})]" \
  outfile ="EXP_MAPS/expmap_${energy_id}keV_${CCD}_small.fits" \
  clobber = yes
  
 dmcopy \
  infile  ="tmp/evt2_${loe_id}-${hie_id}.fits[bin x=${BIN},y=${BIN}]" \
  outfile ="tmp/${obj}_${loe_id}-${hie_id}_swiss_cheese_img.fits" \
  clobber =yes

 cat <<EOF
 
 	Filling the point source regions with POISSON noise...
EOF

 dmfilth \
  infile="tmp/${obj}_${loe_id}-${hie_id}_swiss_cheese_img.fits" \
  outfile="tmp/${obj}_${loe_id}-${hie_id}_img.fits" \
  method=POISSON \
  srclist="@${region}" bkglist="@${bkgpntsrcs}" randseed=123 clobber=yes verbose=0


 dmcopy \
  infile  = "tmp/${obj}_${loe_id}-${hie_id}_img.fits[sky=region(${ccdreg})]" \
  outfile = "tmp/${obj}_${loe_id}-${hie_id}_img.fits" \
  clobber = yes
 
 # Now A trick to make the exposure map and image the same size, in case they are slightly off
 farith "FINAL_IMAGE/${obj}_raw_counts.fits" "FINAL_IMAGE/${obj}_raw_counts.fits" temp SUB clobber=yes
 ./ADD_CHANDRA_IMAGES.csh "temp" "tmp/${obj}_${loe_id}-${hie_id}_img.fits" "temp2"
 mv temp2 tmp/${obj}_${loe_id}-${hie_id}_img.fits
 ./ADD_CHANDRA_IMAGES.csh "temp" "EXP_MAPS/expmap_${energy_id}keV_${CCD}_small.fits" "temp2"
 mv temp2 "EXP_MAPS/expmap_${energy_id}keV_${CCD}_small.fits"

 rm -f temp

 mv tmp/${obj}_${loe_id}-${hie_id}_img.fits images/${obj}_${loe_id}-${hie_id}_rawimg.fits

 cat <<EOF
 
 	Smoothing the Image and the Exposure Map
	With the same kernel....
	
EOF

 csmooth \
  infile="images/${obj}_${loe_id}-${hie_id}_rawimg.fits" \
  sclmap="FINAL_IMAGE/${obj}_${sigma[1]}-${sigma[2]}_kernel.fits" \
  outfile="images/${obj}_${loe_id}-${hie_id}_smoothimg.fits" \
  outsigfile="tmp/${obj}_${loe_id}-${hie_id}_sig.fits" \
  outsclfile="tmp/${obj}_${loe_id}-${hie_id}_kernel.fits" \
  conmeth=fft conkerneltype=gauss sigmin=${sigma[1]} sigmax=${sigma[2]} \
  sclmin=INDEF sclmax=INDEF sclmode=user clobber=yes verbose=5
 
 csmooth \
  infile="EXP_MAPS/expmap_${energy_id}keV_${CCD}_small.fits" \
  sclmap="FINAL_IMAGE/${obj}_${sigma[1]}-${sigma[2]}_kernel.fits" \
  outfile="EXP_MAPS/expmap_${energy_id}keV_${CCD}_smooth.fits" \
  outsigfile="tmp/${obj}_${loe_id}-${hie_id}_sig.fits" \
  outsclfile="tmp/${obj}_${loe_id}-${hie_id}_kernel.fits" \
  conmeth=fft conkerneltype=gauss sigmin=${sigma[1]} sigmax=${sigma[2]} \
  sclmin=INDEF sclmax=INDEF sclmode=user clobber=yes verbose=5

 # AND NOW FOR THE BACKGROUND AND OOTs
 ## Bin them
 if ( ${COUNTER} == $#loenergy ) then
  punlearn dmcopy
  dmcopy \
   infile  ="${bkg}[energy=${loenergy[$COUNTER]}:${hienergy[$COUNTER]}][bin x=${BIN},y=${BIN}]" \
   outfile ="tmp/UNWEIGHTED_${CCD}_${loe_id}-${hie_id}_bg.img" \
   clobber = yes
 
  punlearn dmcopy
  dmcopy \
   infile  ="${oot}[energy=${loenergy[$COUNTER]}:${hienergy[$COUNTER]}][bin x=${BIN},y=${BIN}]" \
   outfile ="tmp/${obj}_${CCD}_${loe_id}-${hie_id}_oot.img" \
   clobber = yes

 else
  punlearn dmcopy
  dmcopy \
   infile  ="${oot}[energy>=${loenergy[$COUNTER]} && energy < ${hienergy[$COUNTER]}][bin x=${BIN},y=${BIN}]" \
   outfile ="tmp/${obj}_${CCD}_${loe_id}-${hie_id}_oot.img" \
   clobber = yes

  punlearn dmcopy
  dmcopy \
   infile  ="${bkg}[energy>=${loenergy[$COUNTER]} && energy < ${hienergy[$COUNTER]}][bin x=${BIN},y=${BIN}]" \
   outfile ="tmp/UNWEIGHTED_${CCD}_${loe_id}-${hie_id}_bg.img" \
   clobber = yes
 endif
 # I took out the deadtime correction as this should have been factored into the
 # "Fudge Factor"  Also I could find no reference to including this step in
 # the Threads.
 #set weight=`echo "scale=10; 1/${DTCOR} * ${BKGFACTOR}" | bc -l`
 set weight=`echo "scale=10; ${BKGFACTOR}" | bc -l`		

 dmimgcalc \
  infile="tmp/UNWEIGHTED_${CCD}_${loe_id}-${hie_id}_bg.img" \
  weight=$weight \
  infile2="tmp/UNWEIGHTED_${CCD}_${loe_id}-${hie_id}_bg.img" \
  weight2=0.0 \
  outfile="tmp/${obj}_${CCD}_${loe_id}-${hie_id}_bg.img" \
  operation=add \
  verbose=0 \
  mode=h \
  clobber=yes
  
 # Now A trick to make the exposure map and image the same size, in case they are slightly off
 farith "FINAL_IMAGE/${obj}_raw_counts.fits" "FINAL_IMAGE/${obj}_raw_counts.fits" temp SUB clobber=yes
 ./ADD_CHANDRA_IMAGES.csh "temp" "EXP_MAPS/BKG_EXPTIME_NORM_${CCD}.fits" temp2
 mv temp2 "EXP_MAPS/BKG_EXPTIME_NORM_${CCD}.fits"
 ./ADD_CHANDRA_IMAGES.csh "temp" "tmp/${obj}_${CCD}_${loe_id}-${hie_id}_bg.img" temp2
 mv temp2 "tmp/${obj}_${CCD}_${loe_id}-${hie_id}_bg.img"

 ./ADD_CHANDRA_IMAGES.csh "temp" "EXP_MAPS/OOT_EXPTIME_NORM_${CCD}.fits" temp2
 mv temp2 "EXP_MAPS/OOT_EXPTIME_NORM_${CCD}.fits"
 ./ADD_CHANDRA_IMAGES.csh "temp" "tmp/${obj}_${CCD}_${loe_id}-${hie_id}_oot.img" temp2
 mv temp2 "tmp/${obj}_${CCD}_${loe_id}-${hie_id}_oot.img"
 rm -f temp

 farith "tmp/${obj}_${CCD}_${loe_id}-${hie_id}_bg.img" "EXP_MAPS/BKG_EXPTIME_NORM_${CCD}.fits" "tmp/${obj}_${CCD}_${loe_id}-${hie_id}_bg.img" MUL clobber=yes
 farith "tmp/${obj}_${CCD}_${loe_id}-${hie_id}_oot.img" "EXP_MAPS/OOT_EXPTIME_NORM_${CCD}.fits" "tmp/${obj}_${CCD}_${loe_id}-${hie_id}_oot.img" MUL clobber=yes
 
 cat <<EOF
 
 	Smoothing the Background
	with the same kernel....
	
EOF

 csmooth \
  infile="tmp/${obj}_${CCD}_${loe_id}-${hie_id}_bg.img" \
  sclmap="FINAL_IMAGE/${obj}_${sigma[1]}-${sigma[2]}_kernel.fits" \
  outfile="tmp/${obj}_${CCD}_${loe_id}-${hie_id}_smoothbg.img" \
  outsigfile="tmp/${obj}_${loe_id}-${hie_id}_sig.fits" \
  outsclfile="tmp/${obj}_${loe_id}-${hie_id}_kernel.fits" \
  conmeth=fft conkerneltype=gauss sigmin=${sigma[1]} sigmax=${sigma[2]} \
  sclmin=INDEF sclmax=INDEF sclmode=user clobber=yes verbose=5

 cat <<EOF
 
 	Smoothing the OOTs
	with the same kernel....
	
EOF

 csmooth \
  infile="tmp/${obj}_${CCD}_${loe_id}-${hie_id}_oot.img" \
  sclmap="FINAL_IMAGE/${obj}_${sigma[1]}-${sigma[2]}_kernel.fits" \
  outfile="tmp/${obj}_${CCD}_${loe_id}-${hie_id}_smoothoot.img" \
  outsigfile="tmp/${obj}_${loe_id}-${hie_id}_sig.fits" \
  outsclfile="tmp/${obj}_${loe_id}-${hie_id}_kernel.fits" \
  conmeth=fft conkerneltype=gauss sigmin=${sigma[1]} sigmax=${sigma[2]} \
  sclmin=INDEF sclmax=INDEF sclmode=user clobber=yes verbose=5

 dmimgcalc \
  infile  ="images/${obj}_${loe_id}-${hie_id}_smoothimg.fits" \
  infile2 ="tmp/${obj}_${CCD}_${loe_id}-${hie_id}_smoothbg.img" \
  outfile ="tmp/${obj}_${CCD}_${loe_id}-${hie_id}_smoothimg-bkg.fits" \
  operation = sub \
  clobber = yes

 dmimgcalc \
  infile  ="tmp/${obj}_${CCD}_${loe_id}-${hie_id}_smoothimg-bkg.fits" \
  infile2 ="tmp/${obj}_${CCD}_${loe_id}-${hie_id}_smoothoot.img" \
  outfile ="tmp/${obj}_${CCD}_${loe_id}-${hie_id}_smoothimg-bkg-oot.fits" \
  operation = sub \
  clobber = yes

 dmimgcalc \
  infile  ="tmp/${obj}_${CCD}_${loe_id}-${hie_id}_smoothimg-bkg-oot.fits" \
  infile2 ="EXP_MAPS/expmap_${energy_id}keV_${CCD}_smooth.fits" \
  outfile ="tmp/${obj}_${CCD}_${loe_id}-${hie_id}_norm.fits" \
  operation = div \
  clobber = yes

 dmimgthresh \
  infile  ="tmp/${obj}_${CCD}_${loe_id}-${hie_id}_norm.fits" \
  outfile ="images/${obj}_${CCD}_${loe_id}-${hie_id}_clean.fits" \
  expfile ="EXP_MAPS/expmap_${energy_id}keV_${CCD}_small.fits" \
  cut = "1.5%" \
  value = "0.0" \
  clobber = yes

 if ( ${COUNTER} == 1 ) then
  cp "images/${obj}_${CCD}_${loe_id}-${hie_id}_clean.fits" "FINAL_IMAGE/${obj}_SBmap.fits"
 else
  farith "FINAL_IMAGE/${obj}_SBmap.fits" "images/${obj}_${CCD}_${loe_id}-${hie_id}_clean.fits" TEMP ADD clobber=yes
  mv TEMP "FINAL_IMAGE/${obj}_SBmap.fits"
 endif
 @ COUNTER++
end



cat <<EOF

Done!
###########################################################


Now you can use HARDNESS.csh, MAKE_HARDNESS.m 
and Make_kT.m to make a surface brightness and 
temperature map.



EOF
