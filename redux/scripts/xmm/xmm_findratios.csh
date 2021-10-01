#!/bin/tcsh

# A short script to determine normalisation ratios between 
# blank-sky background files and a source events file. 
#
# The code has been taken from xmm_clean.csh but I want
# to bypass all the clipping/skycasting arguments. 
#
# The inputs will just be an events file and a skycasted blank
# background file. There will be no output.  


# Version 1.0 - RFT - 09/11/2005
# Version 1.1 - RFT - 16/11/2005
#                     I am not happy with the comparison with   
#                     the scaling by HE counts/s. Going to modify
#                     the script to have a look at the HE c/s
#                     in a particular region over a broad band
#                     2-10 keV energy range. 

set version=1.1

nice +19

set tmp_sys=`uname -a`
if ( `echo $tmp_sys|grep -c Generic` == "1" ) then
 set tmp_machine=solaris
 alias echo '/bin/echo'
endif
if ( "$tmp_sys" =~ Linux* ) then
 set tmp_machine=linux
endif

cat <<EOF

--------------------`echo $0:t | tr '[a-z]' '[A-Z]'` version $version `date +%x`--------------------

EOF


if ( $#argv != 3 ) then
cat <<EOF

Use xmm_findratios.csh events bgevents region


  events      - input events table
  bgevents    - cleaned and skycasted background events table
  region      - input CIAO region file

EOF
exit
endif


set evt=$1
set bgevt=$2
set region=$3

#check that the input files exist

if ( ! -e $evt ) then
 echo " Error: $evt does no exist. Exiting...\n" 
 exit
endif

if ( ! -e $bgevt ) then
 echo " Error: $bgevt does no exist. Exiting...\n" 
 exit
endif

if ( ! -e $region ) then
 echo "Error: region file not found. Exiting...\n"
 exit
endif

set sourcefilter=`/data1/rft/rftscripts/xmmstring.csh $region | grep expression | awk '{print $4 }'`

#get instrument
if ( -e tmpdmp1 ) then
 rm -f tmpdmp1
endif
fdump $evt tmpdmp1 TIME - prdata=no showcol=no showunit=no showrow=no clobber=yes
set instr=`grep INSTRUME < tmpdmp1 | head -1 | awk '{print $2}'`
set instr=`echo $instr | tr -d "'"`
rm -f tmpdmp1
echo 'Instrument is '$instr "\n"
endif

#Getting livetimes
#Comparing livetime of cleaned events table
set livetime_bg=`fkeyprint "${bgevt}[EVENTS]" LIVETIME | grep "Live time" | awk '{print $2}'`
set livetime_src=`fkeyprint "${evt}[EVENTS]" LIVETIME | grep "Live time" | awk '{print $2}'`
#Getting ratio of livetimes
set livetime_ratio=`echo $livetime_src $livetime_bg | awk '{print $1/$2}'`
#echo "Livetime ratio (Events/Background)= $livetime_ratio \n"

#Getting OOFOV counts
if ( $instr == EPN ) then
   echo "Comparing out of FOV counts for singles and doubles...\n"
   set patexp="(PATTERN<=4)"
   set parcirc="!(CIRCLE(-2120,-1080,18000,DETX,DETY))"
   else
   echo "Comparing out of FOV counts for singles, doubles, triples and quads ...\n"
   set patexp="(PATTERN<=12)"
   set parcirc="!(CIRCLE(0,0,18000,DETX,DETY))&&((CIRCLE(0,15000,34500,DETX,DETY)||DETX<-10000||DETX>10000))"
endif

set parimages='xcolumn=X ycolumn=Y ximagebinsize=400 yimagebinsize=400 updateexposure=false imagedatatype=Real32 squarepixels=true imagebinning=binSize withimageset=Y writedss=false withimagedatatype=true keepfilteroutput=false'  
set badratio = 0
#set enlo=300
#set enhi=10000
set enlo=2000
set enhi=10000

echo "Creating images ${enlo}-${enhi}eV; Pattern expression: ${patexp})\n"
evselect -w 0 -V 0 table=$evt imageset=tempfov1 expression='PI>='$enlo'&&PI<='$enhi'&& '$patexp'&& '$parcirc'&& #XMMEA_16' $parimages 
set oFOVev = `fimgstat tempfov1 0 99999999 | grep sum | awk '{print $8}'`
set oFOVev = `echo $oFOVev | awk '{print int($1+0.5)}'`
set oFOVevexp=`fkeyprint tempfov1 EXPOSURE | grep ONTIME | awk '{print $2}'`

if ($oFOVev <= 0) then 
   echo 'No out-of-FOV events found in Events - perhaps you have previously rejected them'
   set FOVratio = 0.0
   set badratio = 1
endif

evselect -w 0 -V 0 table=$bgevt imageset=tempfov2 expression='PI>='$enlo'&&PI<='$enhi'&& '$patexp'&& '$parcirc'&& #XMMEA_16' $parimages 
set oFOVbg = `fimgstat tempfov2 0 99999999 | grep sum | awk '{print $8}'`
set oFOVbg = `echo $oFOVbg | awk '{print int($1+0.5)}'`
set oFOVbgexp=`fkeyprint tempfov2 EXPOSURE | grep ONTIME | awk '{print $2}'`

if ( $oFOVbg <= 0 ) then 
   echo 'No out-of-FOV events found in Background - perhaps you have previously rejected them'
   set badratio = 1
endif

if ( $badratio == 0 ) then 
   set FOVratio = `echo $oFOVev $oFOVbg | awk '{print $1/$2}'`
#   echo "Out-of-FOV count ratio (Events/Background): ${FOVratio}"
#   set FOVrateratio=`echo $oFOVev $oFOVbg $oFOVevexp $oFOVbgexp | awk '{print ($1*$4)/($2*$3)}'`
   set FOVrateratio=`echo $oFOVev $oFOVbg $livetime_src $livetime_bg | awk '{print ($1*$4)/($2*$3)}'`
#   echo "Out of FOV count rate ratio (Events/Background): $FOVrateratio"  
 else
   echo 'no out-of-FOV count ratio given'
endif
rm -f tempfov?

set flag=1 # flag=1 means High energy band


# Getting high energy counts (modified) 
# Using 2-10keV for all cameras. 
set parimages='xcolumn=X ycolumn=Y ximagebinsize=80 yimagebinsize=80 updateexposure=false imagedatatype=Real32 squarepixels=true imagebinning=binSize withimageset=Y writedss=false withimagedatatype=true keepfilteroutput=false' 
# 1) Using high energy full image counts
if ( $instr == EPN ) then
  if ( $flag == 1 ) then  
   set elo=12000
   set ehi=14000
  else  
   set elo=2000
   set ehi=10000
  endif
 echo "Creating high energy count rates using region image counts...(${elo}-${ehi})\n"
 evselect -w 0 -V 0 table=$evt $parimages imageset=temp_src.fits expression="(PI in [${elo}:${ehi}])&&(FLAG==0)&&(PATTERN<=4)&&${sourcefilter}"  
 evselect -w 0 -V 0 table=$bgevt $parimages imageset=temp_bg_src.fits expression="(PI in [${elo}:${ehi}])&&(FLAG==0)&&(PATTERN<=4)&&${sourcefilter}"   
else 
  if ( $flag == 1 ) then 
   set elo=10000
   set ehi=12000
  else
   set elo=2000
   set ehi=10000
  endif 
echo "Creating high energy count rates using region image counts...(${elo}-${ehi})\n"
 evselect -w 0 -V 0 table=$evt $parimages imageset=temp_src.fits expression="(PI in [${elo}:${ehi}])&&(FLAG==0)&&(PATTERN<=12)&&${sourcefilter}" 
 evselect -w 0 -V 0 table=$bgevt $parimages imageset=temp_bg_src.fits expression="(PI in [${elo}:${ehi}])&&(FLAG==0)&&(PATTERN<=12)&&${sourcefilter}" 
endif
set srcexpo=`fkeyprint temp_src.fits EXPOSURE | grep ONTIME | awk '{print $2}'`
set bkgexpo=`fkeyprint temp_bg_src.fits EXPOSURE | grep ONTIME | awk '{print $2}'`
set srccts=`fimgstat temp_src.fits INDEF INDEF | grep sum | awk '{print $8}'`
set bkgcts=`fimgstat temp_bg_src.fits INDEF INDEF | grep sum | awk '{print $8}'`

echo $srccts $bkgcts
#set HEratio=`echo $srccts $bkgcts $srcexpo $bkgexpo | awk '{print ($1*$4)/($2*$3)}'`
#set HEctratio=`echo $srccts $bkgcts | awk '{print $1/$2}'`
set HEratio=`echo $srccts $bkgcts $livetime_src $livetime_bg | awk '{print ($1*$4)/($2*$3)}'`
set HEctratio=`echo $srccts $bkgcts | awk '{print $1/$2}'`

rm temp_src.fits
rm temp_bg_src.fits

echo "All ratios are as follows(Evt/Bkg):\n"
echo "Exposure               : $livetime_ratio " 
echo "High E counts          : $HEctratio      "
echo "Out of FOV counts      : $FOVratio       "
echo "Count rate ratios are:"
echo "(evt counts* bg exposure)/(evt exposure *bg counts)"
echo "Using EXPOSURE not ONTIME keyword"
echo "High E count rate      : $HEratio        "
echo "Out of FOV count rate  : $FOVrateratio \n"

echo "High energy count rates were determined using the full "
echo "image in the FOV extracted between $elo - $ehi keV "
echo "OOFOV count rates were extracted between 300 and 10000 keV \n "







