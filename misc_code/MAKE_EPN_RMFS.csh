#! /bin/tcsh -f

# This script should create canned rmfs for an EPN observation
# which can then be weight added to create an rmf for any region
#
# From the XMM help-desk
# 
# ...the EPIC-pn response function depends on the position on the chip. 
# The correct operation would be for rmfgen to average the RAW-Y
# position of each (region) and produce the corresponding RMF. This 
# is planned for the future but currently the task simply finds the geometrical 
# centre of the source areas, which is somewhere near to the centre of the 
# array and trys to produce the RMF for this position. In this case, 
# even if it worked ok this would produce the wrong response for the 
# spectra. As it turns out there is a rounding error which is causing 
# the routine to crash anyway.

# From the SAS task pages rmfgen - 1.52..2
# The following list summarises the current and planned features of rmfgen:

# Item 		Description 								Status
#1 	Grouping of response data above a threshold value 				implemented
#2 	Channel rebinning 								implemented
#3 	Spectrum-response channel range matching 					implemented
#4 	PN instrument support 								implemented
#5 	User-defined energy grid 							implemented
#6 	Modelling for spectral distortion due to pile-up 				initial implementation, not available
#7 	DSS support (including eg pattern selection) 					implemented
#9 	Modelling of spectral distortion due to Charge Transfer Inefficiency (CTI) 	to be coded

# There was no variation between CHIPS for the same Y, so I decided to remove this part of the code
# in the interest of time to run...
if ( $#argv != 4 ) then
 cat <<EOF 

Usage 
./MAKE_EPN_RMFS.csh  EPN.evt  outdir  out_prefix   pntsrcs.fits

This routine makes "canned" rmfs for a EPN observation.  These may be weight averaged in order
to determine an rmf for any given region with the task EPNRMFGEN.csh

EOF
 exit
endif

set ev = $1
set dir = $2
set prefix = $3
set pntsrcs = $4

set Y = ( 0 1 2 3 4 5 6 7 8 9 )
set i = 1
while ( $i <= $#Y )
 set rawymin = `echo "scale=0;${Y[$i]}*20" | bc -l`
 set rawymax = `echo "scale=0;$rawymin + 19" | bc -l`
 cat <<EOF
  
   Extracting events in RAWY CCD Lines ${rawymin} - ${rawymax} 
  
EOF

 evselect \
  table=${ev} expression="(RAWY>=${rawymin} && RAWY<=${rawymax}) && (PATTERN<=4) && (FLAG==0) && (REGION(${pntsrcs},X,Y))" \
  filteredset="${dir}/temp_Y${Y[$i]}.evt" 

 set mindetx = `fstatistic ${dir}/temp_Y${Y[$i]}.evt+1 DETX - | grep "The minimum of selected column is" | awk '{printf"%g\n",$7}'`
 set maxdetx = `fstatistic ${dir}/temp_Y${Y[$i]}.evt+1 DETX - | grep "The maximum of selected column is" | awk '{printf"%g\n",$7}'`
 set mindety = `fstatistic ${dir}/temp_Y${Y[$i]}.evt+1 DETY - | grep "The minimum of selected column is" | awk '{printf"%g\n",$7}'`
 set maxdety = `fstatistic ${dir}/temp_Y${Y[$i]}.evt+1 DETY - | grep "The maximum of selected column is" | awk '{printf"%g\n",$7}'`
 cat <<EOF
  
   Extracting spectrum for RAWY CCD Lines ${rawymin} - ${rawymax} 
  
EOF

 evselect \
  table=${ev} \
  expression="(RAWY>=${rawymin} && RAWY<=${rawymax})  && (PATTERN<=4) && (FLAG==0) && (DETX>=${mindetx} && DETX<=${maxdetx}) && (DETY>=${mindety} && DETY<=${maxdety})" \
  withzcolumn=no spectrumset="${dir}/temp_Y${Y[$i]}.pi" energycolumn='PI' withspecranges=yes spectralbinsize=5 \
  specchannelmin=0 specchannelmax=20479
 cat <<EOF
  
   Creating RMF for RAWY CCD Lines ${rawymin} - ${rawymax}
  
EOF
 rmfgen \
  spectrumset="${dir}/temp_Y${Y[$i]}.pi" rmfset="${dir}/${prefix}_Y${Y[$i]}.rmf"
 
 rm -f "${dir}/temp_Y${Y[$i]}.pi" "${dir}/temp_Y${Y[$i]}.evt" 
 @ i++
end
