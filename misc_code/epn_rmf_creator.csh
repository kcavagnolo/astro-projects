#! /bin/tcsh -f

if ( $#argv != 5 ) then
 cat <<EOF 

Usage 
./epn_rmf_creator.csh  EPN.evt canned_rmf_dir canned_rmf_prefix region outrmf

This routine used pre-made rmfs (see MAKE_EPN_RMFS.csh) to make a weighted RMF
for a given region file.  NOTE: As of 2004-June-08 this method is more correct
than just using rmfgen.

At this point CCD lines 0-19 are not supported as they are poor anyway and
rmfgen (which this program relies) craps out on them.

EOF
 exit
endif

set ev = $1
set dir = $2
set prefix = $3
set region = $4
set out = $5

 cat <<EOF
  
   			epn_rmf_creator v1.0
   			2004-June-08
   			Danny Hudson
   			dhudson@astro.uni-bonn.de
   
   
   	This routine used pre-made rmfs (see MAKE_EPN_RMFS.csh) 
	to make a weighted RMF for a given region file.  
	NOTE: As of 2004-June-08 this method is more correct
	than just using rmfgen.

  
EOF



cat <<EOF
  
   Extracting spectrum for the region in order to determine total counts... 
  
EOF

evselect \
 table=${ev} \
 expression="(REGION(${region},X,Y)) && (PATTERN<=4) && (FLAG==0)" \
 withzcolumn=no spectrumset="${dir}/temp.pi" energycolumn='PI' withspecranges=yes spectralbinsize=5 \
 specchannelmin=0 specchannelmax=20479

set COUNTS = `fstatistic "${dir}/temp.pi" COUNTS - | grep "The sum of the selected column is" | awk '{printf"%d\n",$8}'`


cat <<EOF
  
   There appear to be $COUNTS in your region.
   Now determining the distribution in RAWY space... 
  
EOF

rm -f "${dir}/temp.pi"

set Y = ( 1 2 3 4 5 6 7 8 9 )
set i = 1
while ( $i <= $#Y )
 set rawymin = `echo "scale=0;${Y[$i]}*20" | bc -l`
 set rawymax = `echo "scale=0;$rawymin + 19" | bc -l`
 cat <<EOF
  
   Extracting spectrum for your region and RAWY CCD Lines ${rawymin} - ${rawymax} 
  
EOF

 evselect \
  table=${ev} \
  expression="(REGION(${region},X,Y)) && (RAWY>=${rawymin} && RAWY<=${rawymax})  && (PATTERN<=4) && (FLAG==0)" \
  withzcolumn=no spectrumset="${dir}/temp_Y${Y[$i]}.pi" energycolumn='PI' withspecranges=yes spectralbinsize=5 \
  specchannelmin=0 specchannelmax=20479 >& /dev/null

 set cnts = `fstatistic "${dir}/temp_Y${Y[$i]}.pi" COUNTS - | grep "The sum of the selected column is" | awk '{printf"%d\n",$8}'`
 set weight = `echo "scale=10;${cnts}/${COUNTS}" | bc -l`

 cat <<EOF
  
   There are ${cnts} in your region on this section of the chip,
   Giving it a weighting factor of ${weight} 
  
EOF
 
 
 if ( $i == 1 ) then
  echo "${dir}/${prefix}_Y${Y[$i]}.rmf ${weight}" > RMF_WEIGHTING.txt
  set sum = ${weight}
 else
  echo "${dir}/${prefix}_Y${Y[$i]}.rmf ${weight}" >> RMF_WEIGHTING.txt
  set sum = `echo "${weight}+${sum}" | bc -l`
 endif
 rm -f "${dir}/temp_Y${Y[$i]}.pi"
 @ i++
end
cat <<EOF
  
   The sum of your weights is ${sum}.
   If this is significantly less than 1, then
   there is probably a problem.
  
EOF

addrmf @RMF_WEIGHTING.txt rmffile=$out clobber=yes
