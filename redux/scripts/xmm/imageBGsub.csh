#! /star/local/bin/tcsh 
#
# AMR 17/03/03
# allowance for no-DBG

set version=2.2

nice +19

cat <<EOF 


--------------- IMAGEBGSUB VERSION $version -----------------

EOF

if ( $#argv != 16 ) then
 cat <<EOF 

A script to perform rigourous background subtraction of an image. An
image is created from an event file, plus a corresponding background
(particles+photons) image and a particles image from respectively, a
blank-sky background file, and a CLOSED event file, both having been
SKYCASTed previously (see task skys=cast). The Event file and
blank-sky file should have been identically filtered
previously. Scalings between the three files are estimated from event
counts (in optionally large energy ranges, to improve the statistics)
outside of the detector field of view (FOV), though these can be
overridden. Control is given over the image energy and pattern
bounds. An exposure map and mask are created. Once the particles
(correctly scaled) have been removed from the image and the background
image, a double-subtraction (of the photons) can be performed, whereby
soft photon excesses in thin energy bands are calculated within a thin
large-radius source-free annulus. Vignetting-corrected soft-excess
photon images are created and added to the original scaled (photon)
background to produce a double-subtracted (photon) background. The
scaled particles are then added back into the background to give a
total background image. Background-subtracted and exposure-corrected
(and masked) images are finally produced.

Files produced (all images) ...

  im_*.fits      - Raw image (from event file)
  imp_*.fits     - Particle (P) image (from CLOSED file)
  imscp_*.fits   - Scaled-P image (imp*clscale)
  imbg_*.fits    - Background (BG) image (from blank-sky file)
  imscbg_*.fits  - Scaled-BG image (imbg*bgscale)
  imexp_*.fits   - Exposure image
  imps_*.fits    - Image minus scaled-P (im-imscp)
  imbgps_*·fits  - Scaled-BG minus scaled-P (imscbg-imscp)
  imsebg_*.fits  - Double-subtraction soft excess (SE) BG
  imdbgph_*.fits - Double BG (photons) (imbgps+imsebg)
  imtotbg_*.fits - Total background (DBG photons + scaled-P) (imdbgph+imscp)
  imbgs_*.fits   - (Total) BG-subtracted image (im-imtotbg)
  immsk_*.fits   - Mask image
  imbgsc_*.fits  - Exposure-corrected (Total)BG-sub image (imbgs/imexp)
  imbgscm_*.fits - Masked exp-corr (Total)BG-sub image (imbgsc*immsk)

Use: imagebgsub evfile bgfile clfile attfile bgscale clscale scaleElo scaleEhi Elow Ehigh pattern DBG_iter DBG_r DBG_width DBG_secut root

  evfile    - Event file (with sky coords)
  bgfile    - "Skycasted" Background (blank sky) file (with sky coords) 
  clfile    - "Skycasted" "CLOSED" (particle) file (with sky coords)
  attfile   - Attitude file
  bgscale   - Scaling of BG file (N = calculated from out-of-FOV counts)
  clscale   - Scaling of "CLOSED" file (N = calculated from out-of-FOV counts)
  scaleElo  - Lower energy band for out-of-FOV calculations (eV) (N = 300) 
  scaleEhi  - Higher energy band for out-of-FOV calculations (eV) (N = 10000) 
  Elow 	    - Image lower energy band (eV)
  Ehigh     - Image higher energy band (eV)
  pattern   - Largest pattern (0, 4, 12 etc)
  DBG_iter  - BG doublesubtraction iterations (1,2,3... 0=no DBG analysis performed)
  DBG_r     - BG doublesubtraction off-source radius (arcsec, ~600) 
  DBG_width - BG doublesubtraction annular width (arcsec, ~60) 
  DBG_secut - BG doublesubtraction soft excess cut-off energy (eV, ~2200 eV)
  root      - Rootname for output files

e.g. imagebgsub evfile bgfile clfile attfile N N N N 500 7000 4 3 600 60 2200 500_7000

EOF
 exit
endif

set ev=$1
set bg=$2
set closed=$3
set att=$4
set bgscale=$5
set clscale=$6
set scaleelo=$7
set scaleehi=$8
set elo=$9
set ehi=$10
set pat=$11
set dbgi=$12
set dbgr=$13
set dbgw=$14
set dbgsecut=$15
set root=$16

#XMM sas started?
if (! -e $SAS_DIR) then 
  echo "It looks like XMM SAS has not been started... exiting..."
  exit
endif
#CCF OK?
if (! -e $SAS_CCF) then 
  echo "Environment variable SAS_CCF does not exist... exiting..." 
  exit
endif
if (! -e $SAS_CCFPATH) then 
  echo "Environment variable SAS_CCFPATH does not exist... exiting..." 
  exit
endif

#-----------------------

set parimages='xcolumn=X ycolumn=Y ximagebinsize=400 yimagebinsize=400 updateexposure=false imagedatatype=Real32 squarepixels=true imagebinning=binSize withimageset=Y writedss=false withimagedatatype=true keepfilteroutput=false'

if ($bgscale == N || $clscale == N) then 

  echo 'Evaluating out-of-FOV counts...'
  if ($scaleelo == N) then 
    set scaleelo = 300
  endif
  if ($scaleehi == N) then
    set scaleehi = 10000
  endif
  echo 'Using energy range: '$scaleelo' to '$scaleehi
  evselect -w 0 -V 0 table=$ev imageset=tempfov1 expression='PI>='$scaleelo'&&PI<='$scaleehi' && PATTERN<='$pat' && #XMMEA_16' $parimages
  set oFOVev = `fimgstat tempfov1 0 99999999 | grep sum | awk '{print $8}'`

  evselect -w 0 -V 0 table=$closed imageset=tempclo1 expression='PI>='$scaleelo'&&PI<='$scaleehi' && PATTERN<='$pat' && #XMMEA_16' $parimages
  set oFOVclo = `fimgstat tempclo1 0 99999999 | grep sum | awk '{print $8}'`

  evselect -w 0 -V 0 table=$bg imageset=tempfov2 expression='PI>='$scaleelo'&&PI<='$scaleehi' && PATTERN<='$pat' && #XMMEA_16' $parimages
  set oFOVbg = `fimgstat tempfov2 0 99999999 | grep sum | awk '{print $8}'`

  set EPscale = `echo $oFOVev $oFOVclo | awk '{print $1/$2}'` 
  set EBscale = `echo $oFOVev $oFOVbg | awk '{print $1/$2}'` 
  set PBscale = `echo $oFOVclo $oFOVbg | awk '{print $1/$2}'` 
  #echo $oFOVev,$oFOVclo,$oFOVbg

  rm tempfov1 tempfov2 tempclo1
endif

if ($bgscale != N) then 
  set EBscale = $bgscale
endif
if ($clscale != N) then 
  set EPscale = $clscale
endif
echo 'Events-blank-sky scaling: '$EBscale' Events-particle scaling: '$EPscale

#--------- forming images -------------

set parimages='xcolumn=X ycolumn=Y ximagebinsize=80 yimagebinsize=80 updateexposure=true imagedatatype=Real32 squarepixels=true imagebinning=binSize withimageset=Y writedss=true withimagedatatype=true keepfilteroutput=false'

echo 'Creating source image...'
evselect -w 0 -V 0 table=$ev imageset=im_$root'.fits' expression='PI>='$elo'&&PI<='$ehi' && PATTERN<='$pat'' $parimages

echo 'Creating particle background image...'
evselect -w 0 -V 0 table=$closed imageset=imp_$root'.fits' expression='PI>='$elo'&&PI<='$ehi' && PATTERN<='$pat'' $parimages

echo 'Scaling particle background...'
if (-e imscp_$root'.fits') then 
  rm -f imscp_$root'.fits'
endif
fcarith imp_$root'.fits' $EPscale imscp_$root'.fits' MUL

echo 'Creating full background image...'
evselect -w 0 -V 0 table=$bg imageset=imbg_$root'.fits' expression='PI>='$elo'&&PI<='$ehi' && PATTERN<='$pat'' $parimages

echo 'Scaling full background...'
if (-e imscbg_$root'.fits') then 
  rm -f imscbg_$root'.fits'
endif
fcarith imbg_$root'.fits' $EBscale imscbg_$root'.fits' MUL

echo 'Creating exposure map...'
eexpmap -w 0 -V 0 attitudeset=$att pimin=$elo pimax=$ehi imageset=im_$root'.fits' expimageset=imexp_$root'.fits' eventset=$ev usedss=no
echo 'Exposure map created...'

echo 'Subtracting particles from image...'
if (-e imps_$root'.fits') then 
  rm -f imps_$root'.fits'
endif
farith im_$root'.fits' imscp_$root'.fits' imps_$root'.fits' SUB

echo 'Subtracting particles from full background...'
if (-e imbgps_$root'.fits') then 
  rm -f imbgps_$root'.fits'
endif
farith imscbg_$root'.fits' imscp_$root'.fits' imbgps_$root'.fits' SUB

# double subtraction

set i=0
set j=0
if ($dbgi == 0) then 
  echo 'DGB_iter chosen == 0... No DBG analysis will be performed...'
  set dbgi=0
endif

if ($dbgi > 0) then 

set rout = `echo $dbgr $dbgw | awk '{print $1+$2}'`
set rout = `echo $rout 20 | awk '{print $1*$2}'`
set dbgr = `echo $dbgr 20 | awk '{print $1*$2}'`

echo "Analysing annulus..."
evselect -w 0 -V 0 table=$bg imageset=tempimB2 expression='ANNULUS(0,0,'$dbgr','$rout',DETX,DETY)' $parimages
evselect -w 0 -V 0 table=$ev imageset=tempimE2 expression='ANNULUS(0,0,'$dbgr','$rout',DETX,DETY)' $parimages
farith tempimB2 tempimE2 tempimE3 ADD
fcarith tempimE3 1E-20 tempimE4 ADD
farith tempimE3 tempimE4 tempEXP1 DIV

set totegap = `echo $dbgsecut $elo | awk '{print $1-$2}'`
set egap = `echo $totegap $dbgi | awk '{print $1/$2}'`
echo "Double subtraction: "$dbgi" iterations of E-width: "$egap

while ($i != $dbgi)
set pi1=`echo $i $egap | awk '{print $1*$2}'`
set pi1=`echo $elo $pi1 | awk '{print $1+$2}'`
set pi1=`echo $pi1 | awk '{print int($1+0.5)}'`
set pi2=`echo $pi1 $egap | awk '{print $1+$2}'`
set pi2=`echo $pi2 | awk '{print int($1+0.5)}'`
echo "Double subtraction: "$pi1" eV to "$pi2" eV"

@ i = $i + 1

echo 'Creating DBG source annulus...iteration '$i
evselect -w 0 -V 0 table=$ev imageset=tempimE1 expression='ANNULUS(0,0,'$dbgr','$rout',DETX,DETY) && PI>='$pi1'&&PI<='$pi2' && PATTERN<='$pat'' $parimages
echo 'Creating DBG particle annulus...iteration '$i
evselect -w 0 -V 0 table=$closed imageset=tempimP1 expression='ANNULUS(0,0,'$dbgr','$rout',DETX,DETY) && PI>='$pi1'&&PI<='$pi2' && PATTERN<='$pat'' $parimages
echo 'Creating DBG full background annulus...iteration '$i
evselect -w 0 -V 0 table=$bg imageset=tempimB1 expression='ANNULUS(0,0,'$dbgr','$rout',DETX,DETY) && PI>='$pi1'&&PI<='$pi2' && PATTERN<='$pat'' $parimages

set Ects = `fimgstat tempimE1 0 99999999 | grep sum | awk '{print $8}'`
set Pcts = `fimgstat tempimP1 0 99999999 | grep sum | awk '{print $8}'`
set Bcts = `fimgstat tempimB1 0 99999999 | grep sum | awk '{print $8}'`
set scPcts = `echo $Pcts $EPscale | awk '{print $1*$2}'`
set scBcts = `echo $Bcts $EBscale | awk '{print $1*$2}'`
#echo $Ects $Pcts $Bcts $scPcts $scBcts

set Ects = `echo $Ects $scPcts | awk '{print $1-$2}'`
set scBcts = `echo $scBcts $scPcts | awk '{print $1-$2}'`
set diff = `echo $Ects $scBcts | awk '{print $1-$2}'`
set difffrac = `echo $diff $scBcts | awk '{print $1/$2}'`
#echo $Ects $scBcts $diff 
echo 'Background correction factor... '$difffrac

echo 'Creating DBG exposure map...iteration '$i
eexpmap -w 0 -V 0 attitudeset=$att pimin=$pi1 pimax=$pi2 imageset=im_$root'.fits' expimageset=tempexp1 eventset=$ev usedss=no
echo 'DBG exposure map created...'
farith tempexp1 tempEXP1 tempexp2 MUL
set Expcts = `fimgstat tempexp2 0 99999999 | grep sum | awk '{print $8}'`
#echo $Expcts

set expscale = `echo $diff $Expcts | awk '{print $1/$2}'`
fcarith tempexp1 $expscale tempimE6 MUL

if ($i == 1) then 
  cp tempimE6 tempDBG$i
else
  @ j = $i - 1
farith tempDBG$j tempimE6 tempDBG$i ADD
endif

rm tempim?? 
rm tempexp? 

end

cp tempDBG$i imsebg_$root'.fits'
cphead im_$root'.fits' imsebg_$root'.fits' scale=Y

else

cp im_$root'.fits' tempDBG0
fcarith tempDBG0 0 imsebg_$root'.fits' MUL clobber=Y
cphead im_$root'.fits' imsebg_$root'.fits' scale=Y

endif

echo 'Creating double background (photons)...'
if (-e imdbgph_$root'.fits') then 
  rm -f imdbgph_$root'.fits'
endif
farith imbgps_$root'.fits' imsebg_$root'.fits' imdbgph_$root'.fits' ADD

echo 'Creating total background (double background photons + particles)...'
if (-e imtotbg_$root'.fits') then 
  rm -f imtotbg_$root'.fits'
endif
farith imdbgph_$root'.fits' imscp_$root'.fits' imtotbg_$root'.fits' ADD

echo 'Subtracting total background...'
if (-e imbgs_$root'.fits') then 
  rm -f imbgs_$root'.fits'
endif
farith im_$root'.fits' imtotbg_$root'.fits' imbgs_$root'.fits' SUB

echo 'Creating detector mask...'
  emask -w 0 -V 0 expimageset=imexp_$root'.fits' detmaskset=immsk_$root'.fits'  threshold1=0.25 threshold2=0.5
cphead imexp_$root'.fits' immsk_$root'.fits' scale=Y

echo 'Exposure correcting...'
if (-e imbgsc_$root'.fits') then 
  rm -f imbgsc_$root'.fits'
endif
farith imbgs_$root'.fits' imexp_$root'.fits' imbgsc_$root'.fits' DIV

echo 'Masking...'
if (-e imbgscm_$root'.fits') then 
  rm -f imbgscm_$root'.fits'
endif
farith imbgsc_$root'.fits' immsk_$root'.fits' imbgscm_$root'.fits' MUL

rm tempEXP1
rm tempDBG?

echo 'ImageBGsub Completed.'

exit


