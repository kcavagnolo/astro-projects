#!/bin/tcsh -f

if ($#argv != 5 ) then

 cat <<EOF
 
  usage equal_energy_range.csh PATH/TO/CXC low_energy high_energy num_of_bins output
  
  energies in eV
  
  e.g.
  ./equal_energy_range.csh ../PASS_01/CXCRI 700 10000 20 Energy_Ranges.txt
  
EOF
 exit
endif

set cxc = $1
set loe = $2
set hie = $3
set bin = $4
set out = $5

set local_dir = `pwd`
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
cat <<EOF

 Using:
 OBJECT 		: ${obj}
 OUTPUT DIRECTORY 	: ${output}
 CCD LIST		: ${CCD}
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
set FACTOR = `less ${output}/data/BKG/BKG_NORM.txt | \
              grep "This gives a background fudge factor of:" | \
	      awk -F "This gives a background fudge factor of:" '{print$2}'`
set FACTOR = ${FACTOR[$#FACTOR]}
cat <<EOF

	Using $FACTOR as the background
	normalization factor.  Note this
	factor will be reduced by ~1.3%
	to account for OOTs.

EOF

dmcopy "$output/data/event_2/${obj}_${CCD}_evt2_clean_goodgti.fits[energy=${loe}:${hie}]" temp_evt.fits clobber=yes
dmcopy "$output/data/event_2/${obj}_${CCD}_BKG_evt2_clean.fits[energy=${loe}:${hie}]" temp_bkg.fits clobber=yes
dmcopy "$output/data/event_2/${obj}_${CCD}_OOT_evt2_clean.fits[energy=${loe}:${hie}]" temp_oot.fits clobber=yes

cat <<EOF

	Determining time normalization
	for Background and OOTs

EOF

set EXP = `fkeyprint temp_evt.fits+1 EXPOSURE | grep "Exposure time" | awk '{printf"%13.13f",$2}'`
set BEX = `fkeyprint temp_bkg.fits+1 EXPOSURE | grep "Total exposure time" | awk '{printf"%13.13f",$2}'`
set OEX = `fkeyprint temp_oot.fits+1 EXPOSURE | grep "Exposure time" | awk '{printf"%13.13f",$2}'`

set BNM = `echo "scale=13;${EXP}/${BEX}*(${FACTOR}-0.041/3.2)" | bc -l`
set ONM = `echo "scale=13;${EXP}/${OEX}" | bc -l`

set SEV = `fstatistic temp_evt.fits+1 TIME - | grep "The number of points used in calculation is" | awk '{print$9}'`
set BEV = `fstatistic temp_bkg.fits+1 X - | grep "The number of points used in calculation is" | awk '{print$9}'`
set OEV = `fstatistic temp_oot.fits+1 TIME - | grep "The number of points used in calculation is" | awk '{print$9}'`
set NEV = `echo "scale=13;${SEV} - ${BEV}*${BNM} - ${OEV}*${ONM}" | bc -l`
set EV  = `echo "scale=0;${NEV}/${bin}" | bc -l`

cat <<EOF

	Your net source counts from ${loe} to ${hie} eV is:
	${NEV}
	This gives ~${EV} events per bin.
	
EOF

set j = 1
set th = `echo "${loe}+100" | bc -l`
set tl = ${loe}
while ( $j < ${bin} )
 dmcopy "temp_evt.fits[energy>=${tl} && energy < ${th}]" temp1 clobber=yes
 dmcopy "temp_bkg.fits[energy>=${tl} && energy < ${th}]" temp2 clobber=yes
 dmcopy "temp_oot.fits[energy>=${tl} && energy < ${th}]" temp3 clobber=yes
 
 set sev = `fstatistic temp1+1 TIME - | grep "The number of points used in calculation is" | awk '{print$9}'`
 set bev = `fstatistic temp2+1 X - | grep "The number of points used in calculation is" | awk '{print$9}'`
 set oev = `fstatistic temp3+1 TIME - | grep "The number of points used in calculation is" | awk '{print$9}'`
 set nrw = `echo "scale=13;${sev} - ${bev}*${BNM} - ${oev}*${ONM}" | bc -l`
 set chk = `echo "scale=0;(${sev} - ${bev}*${BNM} - ${oev}*${ONM})/1" | bc -l`
 if ( $chk < $EV ) then
  set th = `echo "${th}+100" | bc -l`
 else
  set th = `echo "${th}-90" | bc -l`
  dmcopy "temp_evt.fits[energy>=${tl} && energy < ${th}]" temp1 clobber=yes
  dmcopy "temp_bkg.fits[energy>=${tl} && energy < ${th}]" temp2 clobber=yes
  dmcopy "temp_oot.fits[energy>=${tl} && energy < ${th}]" temp3 clobber=yes
  set sev = `fstatistic temp1+1 TIME - | grep "The number of points used in calculation is" | awk '{print$9}'`
  set bev = `fstatistic temp2+1 X - | grep "The number of points used in calculation is" | awk '{print$9}'`
  set oev = `fstatistic temp3+1 TIME - | grep "The number of points used in calculation is" | awk '{print$9}'`
  set nrw = `echo "scale=13;${sev} - ${bev}*${BNM} - ${oev}*${ONM}" | bc -l`
  set chk = `echo "scale=0;(${sev} - ${bev}*${BNM} - ${oev}*${ONM})/1" | bc -l`
  while ( ${chk} < ${EV} )
   set th = `echo "${th}+10" | bc -l`
   dmcopy "temp_evt.fits[energy>=${tl} && energy < ${th}]" temp1 clobber=yes
   dmcopy "temp_bkg.fits[energy>=${tl} && energy < ${th}]" temp2 clobber=yes
   dmcopy "temp_oot.fits[energy>=${tl} && energy < ${th}]" temp3 clobber=yes
   set sev = `fstatistic temp1+1 TIME - | grep "The number of points used in calculation is" | awk '{print$9}'`
   set bev = `fstatistic temp2+1 X - | grep "The number of points used in calculation is" | awk '{print$9}'`
   set oev = `fstatistic temp3+1 TIME - | grep "The number of points used in calculation is" | awk '{print$9}'`
   set nrw = `echo "scale=13;${sev} - ${bev}*${BNM} - ${oev}*${ONM}" | bc -l`
   set chk = `echo "scale=0;(${sev} - ${bev}*${BNM} - ${oev}*${ONM})/1" | bc -l`
  end
  set th = `echo "${th}-10" | bc -l`
  dmcopy "temp_evt.fits[energy>=${tl} && energy < ${th}]" temp1 clobber=yes
  dmcopy "temp_bkg.fits[energy>=${tl} && energy < ${th}]" temp2 clobber=yes
  dmcopy "temp_oot.fits[energy>=${tl} && energy < ${th}]" temp3 clobber=yes
  set sev = `fstatistic temp1+1 TIME - | grep "The number of points used in calculation is" | awk '{print$9}'`
  set bev = `fstatistic temp2+1 X - | grep "The number of points used in calculation is" | awk '{print$9}'`
  set oev = `fstatistic temp3+1 TIME - | grep "The number of points used in calculation is" | awk '{print$9}'`
  set nrw = `echo "scale=13;${sev} - ${bev}*${BNM} - ${oev}*${ONM}" | bc -l`
  set chk = `echo "scale=0;(${sev} - ${bev}*${BNM} - ${oev}*${ONM})/1" | bc -l`
  while ( ${chk} < ${EV} )
   set th = `echo "${th}+1" | bc -l`
   dmcopy "temp_evt.fits[energy>=${tl} && energy < ${th}]" temp1 clobber=yes
   dmcopy "temp_bkg.fits[energy>=${tl} && energy < ${th}]" temp2 clobber=yes
   dmcopy "temp_oot.fits[energy>=${tl} && energy < ${th}]" temp3 clobber=yes
   set sev = `fstatistic temp1+1 TIME - | grep "The number of points used in calculation is" | awk '{print$9}'`
   set bev = `fstatistic temp2+1 X - | grep "The number of points used in calculation is" | awk '{print$9}'`
   set oev = `fstatistic temp3+1 TIME - | grep "The number of points used in calculation is" | awk '{print$9}'`
   set nrw = `echo "scale=13;${sev} - ${bev}*${BNM} - ${oev}*${ONM}" | bc -l`
   set chk = `echo "scale=0;(${sev} - ${bev}*${BNM} - ${oev}*${ONM})/1" | bc -l`
  end
  # Since all bins must have > ${EV} counts the last bin, which is all that is left
  # over is much smaller (for 20 bins and 1433 counts/bin, my last bin was 901 counts)
  # Therefore subtract 1 from energy and see if the energy below ${EV} counts is closer
  # to ${EV} than the one above it.  Then take the closest one.  
  set th2 = `echo "${th}-1" | bc -l`
  dmcopy "temp_evt.fits[energy=${tl}:${th2}]" temp1 clobber=yes
  dmcopy "temp_bkg.fits[energy=${tl}:${th2}]" temp2 clobber=yes
  dmcopy "temp_oot.fits[energy=${tl}:${th2}]" temp3 clobber=yes
  set sev = `fstatistic temp1+1 TIME - | grep "The number of points used in calculation is" | awk '{print$9}'`
  set bev = `fstatistic temp2+1 X - | grep "The number of points used in calculation is" | awk '{print$9}'`
  set oev = `fstatistic temp3+1 TIME - | grep "The number of points used in calculation is" | awk '{print$9}'`
  set nrw2 = `echo "scale=13;${sev} - ${bev}*${BNM} - ${oev}*${ONM}" | bc -l`
  set chk2 = `echo "scale=0;(${sev} - ${bev}*${BNM} - ${oev}*${ONM})/1" | bc -l`
  set dch1 = `echo "${chk}-${EV}" | bc -l`
  set dch2 = `echo "${EV}-${chk2}" | bc -l`
  if ( ${dch1} > ${dch2} ) then
   set nrw = ${nrw2}
   set th = ${th2}
  endif
  cat <<EOF
  
  Energy Range ${tl} - ${th} eV has: ${nrw} Counts
  
EOF
  if ( $j == 1 ) then
   set OLE = ${tl}
   set OHE = ${th}
  else
   set OLE = ( ${OLE} ${tl} )
   set OHE = ( ${OHE} ${th} )
  endif
  set tl = `echo "${th}" | bc -l`
  set th = `echo "${tl}+100" | bc -l`
  @ j++
 endif
end
set th = ${hie}
dmcopy "temp_evt.fits[energy=${tl}:${th}]" temp1 clobber=yes
dmcopy "temp_bkg.fits[energy=${tl}:${th}]" temp2 clobber=yes
dmcopy "temp_oot.fits[energy=${tl}:${th}]" temp3 clobber=yes
set sev = `fstatistic temp1+1 TIME - | grep "The number of points used in calculation is" | awk '{print$9}'`
set bev = `fstatistic temp2+1 X - | grep "The number of points used in calculation is" | awk '{print$9}'`
set oev = `fstatistic temp3+1 TIME - | grep "The number of points used in calculation is" | awk '{print$9}'`
set nrw = `echo "scale=13;${sev} - ${bev}*${BNM} - ${oev}*${ONM}" | bc -l`
cat <<EOF
  
  Energy Range ${tl} - ${th} eV has: ${nrw} Counts
  
EOF
set OLE = ( ${OLE} ${tl} )
set OHE = ( ${OHE} ${th} )
echo ${OLE} > ${out}
echo ${OHE} >> ${out}
rm -f temp_evt.fits temp_bkg.fits temp_oot.fits temp?
