#!/bin/tcsh -f
#
# A script to automate the process of fitting a deprojected temperature and 
# abundance profile, using the XSPEC projct model. The process is quite
# time consuming. Start with a mekal model I think, as this makes numbering 
# easier.
#
# 11/8/05 - v1.1 - Adding ability to use xmmpsf model if desired.
#

set script_version=1.1

if($HOST != feynman) then
 nice +19
endif

cat <<EOF

----------------- DEPROJ_FIT.CSH version $script_version ----------------

EOF

if (($#argv != 10)&&($#argv != 1)&&($#argv != 15)) then

cat <<EOF

Usage: deproj_fit.csh root no_bins thawZ errors Emin Emax T Z nH redshift
    [image Beta Core RA Dec]

root     - file root, not including instrument or bin number
no_bins  - number of spectral bins in use
thawZ    - thaw and fit abundance? (Y/N)
errors   - do not fit errors & exit to prompt (2), fit errors for individual 
            parameters (1) or all together (0)
Emin     - Minium energy for fit (keV)
Emax     - Maximum energy for fit (keV)
T        - starting temperature
Z        - starting abundance
nH       - hydrogen column (fixed)
redshift - redshift of the target
[image]  - image for XMMPSF model (or N to use SB model)[optional]
[Beta]   - Beta of SB distribution for XMMPSF (or N to use image) [optional]
[Core]   - Core radius IN ARCSEC for XMMPSF (or N to use image)[optional]
[RA]     - RA of target in decimal degrees for XMMPSF [optional]
[Dec]    - Declination in decimal degrees for XMMPSF [optional]

Note that the script saves files called \$root_xmmpsf_mix_\$n.fits which
    contain the mixing factors for each bin. If these are already present,
    it will assume you wish to read them in (so as to save time).

e.g.: deproj_fit.csh trial 6 Y 1 0.4 7.0 2.6 0.6 0.05 0.03| & tee deproj_fit.log
or: deproj_fit.csh trial 6 Y 1 0.4 7.0 2.6 0.6 0.05 0.03 trial_xmmpsf_image.fits N N 110.55643 -18.33335 | & tee deproj_fit.log

type deproj_fit.csh help for more information.

EOF
exit
endif

if ($1 == help) then

cat <<EOF

deproj_fit is designed to fit a deprojected temperature (and abundance)
    profile to a set of XMM data. The following assumptions are made:

    1)All three cameras are in use.
    2)The spectra have the bg, arf and rmf files associated with them, and
    the header keywords necessary for deprojection are in place. The PN has
    also been corrected to match the MOS (keywords.csh)
    3)The number of bins is small enough to prevent XSPEC crashing.
    4)heasoft has been started.
    5)The files are all in the local directory.
    6)Assumes you have numbered the bins in order from the inmost outwards

the script uses XSPEC, with a MEKAL model combined with the PROJCT model to
    allow 2D<->3D deprojection. The XMMPSF model can also be used,
    correcting for PSF blurring, in which case you should have previously
    run xmmpsf_prep.csh on the spectra. If using an image with the XMMPSF
    model, this can be any image, binned as you like (8.8arcs pixels are OK)

EOF

exit
endif

echo $0:t $*

set root=$1
set bins=$2
set thawZ=$3
set errors=$4
set Emin=$5
set Emax=$6
set T=$7
set Z=$8
set nH=$9
set red=$10
if($#argv == 15) then
 set image=$11
 set Beta=$12
 set Core=$13
 set RA=$14
 set Dec=$15
 set usexmmpsf=1
else
 set usexmmpsf=0
 set Beta=N
 set image=N
 set Core=N
 set RA=N
 set Dec=N
endif

if ($usexmmpsf == 1) then
 if ($image != N) then
  set test=`ls | grep -c $image`
  if ($test != 1) then
   echo "XMMPSF image $image not found, quitting."
   exit
  endif
 endif
endif

#Hidden parameter - which plasma model to use
set plasmamodel=apec  # or =mekal
if ($plasmamodel == mekal) then
 set kTplus=2
 set Zplus=4
 set zplus=5
 set Nplus=7
else
 set kTplus=2
 set Zplus=3
 set zplus=4
 set Nplus=5
endif

#check there's no save file to be overwritten
set savtest=0
if (-e ${root}_projct_model.xcm) set savtest=1
if ($savtest == 1) then
 echo  ${root}_projct_model.xcm" already exists. Move it and retry."
 exit
endif

# Figure out which version of xspec is being used
set xspectest=`which xspec | grep -c headas`
if ($xspectest > 0) then
 set xspec=xspec11
else
 set xspec=xspec
endif

# First need to grab some of the header parameters so as to be able to set the
# projct radii and angle correctly on each step. Use m1 files.
set major=`ls ${root}_m1_*_src.ds`
set minor=`ls ${root}_m1_*_src.ds`
set orient=`ls ${root}_m1_*_src.ds`
set major[1]=0
set minor[1]=0
set orient[1]=0
set n=2
while ($n <= $bins)
 set x=`expr $n - 1`
 set major[$n]=`fkeyprint ${root}_m1_${x}_src.ds+1 XFLT0001 exact=yes outfile=STDOUT | grep XFLT0001 | tail -1 | awk '{print $2}'`
 set minor[$n]=`fkeyprint ${root}_m1_${x}_src.ds+1 XFLT0002 exact=yes outfile=STDOUT | grep XFLT0002 | tail -1 | awk '{print $2}'`
 set orient[$n]=`fkeyprint ${root}_m1_${x}_src.ds+1 XFLT0003 exact=yes outfile=STDOUT | grep XFLT0003 | tail -1 | awk '{print $2}'`
 @ n ++
end

#Begin construction of xspec script
cat >! ${root}_projct_xspecscript.tcl <<EOF

setplot energy
abund grsa

query yes

renorm PREFIT

EOF

# Sort out input lines and get them into the script
set spec_in=(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20)
set n=1
set s=0
while ($n <= $bins)
set p=`expr $s + 1`
set q=`expr $s + 2`
set r=`expr $s + 3`
 set spec_in[$n]=${n}":"${p}" ${root}_m1_${n}_src.ds "${n}":"${q}" ${root}_m2_${n}_src.ds "${n}":"${r}" ${root}_pn_${n}_src.ds" 
 cat >> ${root}_projct_xspecscript.tcl <<EOF
 data $spec_in[$n]
EOF
@ n ++
set s=$r
end

#Ignore dodgy channels etc.
cat >> ${root}_projct_xspecscript.tcl <<EOF
 ignore bad
 ignore **-${Emin} ${Emax}-**
EOF

#set up for using XMMPSF or not - if using SB model, those params set later
if($usexmmpsf == 1) then
 set mixmodel="xmmpsf*"
 set parambase=7
 if ($Beta != N) then
  cat >> ${root}_projct_xspecscript.tcl <<EOF
  xset XMMPSF-RA $RA
  xset XMMPSF-DEC $Dec
EOF
 else
  cat >> ${root}_projct_xspecscript.tcl <<EOF
  xset XMMPSF-IMAGE $image
  xset XMMPSF-RA $RA
  xset XMMPSF-DEC $Dec
EOF
 endif
 set n=1
 while ($n < = 3)
  set test=0
  if (-e ${root}_xmmpsf_mix_${n}.fits) set test=1
  if ($test == 1) then
   echo "XMMPSF Mix file for camera $n found, reading it in."
   cat >> ${root}_projct_xspecscript.tcl <<EOF
   xset XMMPSF-MIXFACT-IFILE${n} ${root}_xmmpsf_mix_${n}.fits
EOF
  else
   echo "No XMMPSF Mix file found for camera $n, will write one for you."
   cat >> ${root}_projct_xspecscript.tcl <<EOF
   xset XMMPSF-MIXFACT-OFILE${n} ${root}_xmmpsf_mix_${n}.fits
EOF
  endif
  @ n ++
 end
else
 set mixmodel=""
 set parambase=3
endif

# Set energy ranges and model
set nHparam=`expr $parambase + 1`
set kTparam=`expr $parambase + $kTplus`
set Zparam=`expr $parambase + $Zplus`
set zparam=`expr $parambase + $zplus`
set minaxparam=`expr $parambase - 2`
set maxaxparam=`expr $parambase - 1`
set orientparam=$parambase

#if xmmpsf sb model is used, set params as model defined, as each newpar
#will force a recalculation of the mixing matrices. 
if(($usexmmpsf == 1)&&($Beta != N)) then
cat >> ${root}_projct_xspecscript.tcl <<EOF
model ${mixmodel}projct*wabs*${plasmamodel} 
2.0
$Beta
$Core
0
/*
EOF
else
cat >> ${root}_projct_xspecscript.tcl <<EOF
model ${mixmodel}projct*wabs*${plasmamodel} & /*
EOF
endif

cat >> ${root}_projct_xspecscript.tcl <<EOF
newpar $nHparam $nH
newpar $kTparam $T
newpar $Zparam $Z
newpar $zparam $red

freeze $nHparam $kTparam
newpar $minaxparam ,,0,0,1000,1000
newpar $maxaxparam ,,0,0,1000,1000

EOF


# do the fitting cycle, freezing and thawing each bin in turn, from the 
# outside inwards.
set number=$bins

while ($number > 0)
 set norm=`expr $number \* \( $parambase + $Nplus \)`
 set Tnum=`echo $number $parambase $Nplus $kTplus | awk '{print ($1-1)*($2+$3)+$2+$4}'`
 set Znum=`echo $number $parambase $Nplus $Zplus | awk '{print ($1-1)*($2+$3)+$2+$4}'`

 cat >> ${root}_projct_xspecscript.tcl <<EOF
 newpar $maxaxparam $major[$number]
 newpar $minaxparam $minor[$number]
 newpar $orientparam $orient[$number]

 show parameters

 untie $norm $Tnum
 thaw $norm $Tnum 
EOF

 if ($thawZ == Y)then
  cat >> ${root}_projct_xspecscript.tcl <<EOF
  untie $Znum
  thaw $Znum
EOF
 endif

 cat >> ${root}_projct_xspecscript.tcl <<EOF
 fit 10000
# freeze $norm $Tnum $Znum
 freeze $Tnum $Znum
EOF
 set number=`expr $number - 1`
end

#Should now have all the bins fitted individually, time to do a final fit of 
# all of them together.
set n=1
set Ttxt=""
set Ztxt=""
set ntxt=""
while ($n <= $bins)
 set Tnum=`echo $parambase $n $Nplus $kTplus| awk '{print (($1+$3)*($2-1))+$1+$4}'`
 set norm=`echo $parambase $n $Nplus | awk '{print ($1+$3)*$2}'`
 set Ttxt=(${Ttxt}" "${Tnum})
 set ntxt=(${ntxt}" "${norm})
 if ($thawZ == Y)then
  set Znum=`echo $parambase $n $Nplus $Zplus| awk '{print (($1+$3)*($2-1))+$1+$4}'`
  set Ztxt=(${Ztxt}" "${Znum})
 endif
 @ n ++
end
cat >> ${root}_projct_xspecscript.tcl <<EOF
thaw $Ttxt
thaw $ntxt
EOF
if ($thawZ == Y) then
cat >> ${root}_projct_xspecscript.tcl <<EOF
thaw $Ztxt
EOF
endif
cat >> ${root}_projct_xspecscript.tcl <<EOF
fit 10000

# grab a plot of this, start a log and dump the best fit to it, save the 
# current state of the fit in case the error analysis goes mental.
cpd ${root}_projct.eps/cps
plot ldata residuals
puts "Plot written to ${root}_projct.eps"

log ${root}_projct_model.txt
show parameters
show fit
log none

save all ${root}_projct_model.xcm


EOF



#Now figure out the errors 
if ($errors == 1) then
 echo "Calculating errors individually for all free parameters"
 if ($thawZ == N) then
  set errnums=($Ttxt $ntxt)
 else
  set errnums=($Ttxt $ntxt $Ztxt)
 endif

 cat >> ${root}_projct_xspecscript.tcl <<EOF
 log ${root}_projct_errors.txt
EOF

 foreach num ($errnums)
  cat >> ${root}_projct_xspecscript.tcl <<EOF
  error $num
EOF
 end

 #Finish and close script
 cat >> ${root}_projct_xspecscript.tcl <<EOF
 log none
 log ${root}_projct_model.txt
 show parameters
 show fit
 log none
 save all ${root}_projct_model.xcm
 exit
EOF
 
else if ($errors == 0) then
 echo "Calculating errors for all free parameters simultaneously"
 if ($thawZ == N) then
  set errnums=($Ttxt $ntxt)
 else
  set errnums=($Ttxt $ntxt $Ztxt)
 endif

 cat >> ${root}_projct_xspecscript.tcl <<EOF
 log ${root}_projct_errors.txt
 error $errnums
 log none
 log ${root}_projct_model.txt
 show parameters
 show fit
 log none
 save all ${root}_projct_model.xcm
 exit
EOF

else  #this will hopefully be the "do no errors but let the user try" option.
 cat >> ${root}_projct_xspecscript.tcl <<EOF
 #exit
 puts "OK, now it's up to you to do the errors, guv."
 puts "Free kT params: $Ttxt"
 puts "Free nH params: $ntxt"
 puts "Free Z params: $Ztxt"
EOF
 

endif

#and run the script.
echo "Running script in XSPEC"
${xspec} - ${root}_projct_xspecscript.tcl | & tee ${root}_projct_xspeclog.txt
echo "script complete"

exit
