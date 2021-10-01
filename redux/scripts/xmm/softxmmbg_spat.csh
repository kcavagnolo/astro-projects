#!/bin/tcsh
# Script to take an XMM dataset and identify the spatial
# variation of soft protons to the XMM background. The input
# files will be a cleaned and flared events dataset. Comparing
# the spectra between flared and clean times should allow us 
# to determine the extent of the soft protons on the bg.


set version=1.0


cat <<EOF

--------------------`echo $0:t | tr '[a-z]' '[A-Z]'` version $version `date +%x`--------------------

EOF


if ( $#argv < 4 ) then
cat <<EOF

Use softxmmbg_spat.csh cl_evt fl_evt reg1 root1 root2

 cl_evt        - the cleaned events file for your first observation
 fl_evt        - the flare events file for your first observation
 reg1          - a region file to extract a spectrum for your
                 first observation. 
 root          - root filename for output files. 


EOF
exit
endif

set cl_evt=$1
set fl_evt=$2
set reg1=$3
set root=$4


#Check input files exist...

#check input files exist
if ( ! -e $cl_evt ) then
  echo "Error: file $cl_evt not found. Exiting..." 
  exit
endif

if ( ! -e $fl_evt ) then
  echo "Error: file $fl_evt not found. Exiting..." 
  exit
endif

if ( ! -e $reg1 ) then
  echo "Error: file $reg1 not found. Exiting..." 
  exit
endif

#check that region file is in CIAO format

set rtype=`head -1 $reg1 | awk '{print $5}'`
if ( $rtype != CIAO ) then
 echo ERROR - REGION FILE "$reg1" DOES NOT APPEAR TO BE IN CIAO FORMAT"\n"
 exit
endif

#Getting INST:
if ( -e tmpdmp1 ) then
 rm -f tmpdmp1
endif

fdump $cl_evt tmpdmp1 TIME - prdata=no showcol=no showunit=no showrow=no clobber=yes
set instr=`grep INSTRUME < tmpdmp1 | head -1 | awk '{print $2}'`
set instr=`echo $instr | tr -d "'"`
rm -f tmpdmp1

#Set up some energy bands in which to generate images. 

set elo1=300
set ehi1=2000
set elo2=2000
set ehi2=5000
set elo3=5000
set ehi3=7500
set elo4=7500
set ehi4=10000

#Create images in different bands:

set parimages='xcolumn=X ycolumn=Y ximagebinsize=80 yimagebinsize=80 updateexposure=false imagedatatype=Real32 squarepixels=true imagebinning=binSize withimageset=Y writedss=false withimagedatatype=true keepfilteroutput=false' 

echo "Creating images in band $elo1 - $ehi1 eV ...\n "

if ( $instr == EPN ) then
 evselect -w 0 -V 0 table=$cl_evt $parimages imageset=${root}_cl1.fits expression="(PI in [${elo1}:${ehi1}])&&(#XMMEA_EP)&&(PATTERN<=4)"
 evselect -w 0 -V 0 table=$fl_evt $parimages imageset=${root}_fl1.fits expression="(PI in [${elo1}:${ehi1}])&&(#XMMEA_EP)&&(PATTERN<=4)" 
else 
 evselect -w 0 -V 0 table=$cl_evt $parimages imageset=${root}_cl1.fits expression="(PI in [${elo1}:${ehi1}])&&(#XMMEA_EM)&&(PATTERN<=12)"
 evselect -w 0 -V 0 table=$fl_evt $parimages imageset=${root}_fl1.fits expression="(PI in [${elo1}:${ehi1}])&&(#XMMEA_EM)&&(PATTERN<=12)"
endif

echo "Creating images in band $elo2 - $ehi2 eV ...\n "

if ( $instr == EPN ) then
 evselect -w 0 -V 0 table=$cl_evt $parimages imageset=${root}_cl2.fits expression="(PI in [${elo2}:${ehi2}])&&(#XMMEA_EP)&&(PATTERN<=4)"
 evselect -w 0 -V 0 table=$fl_evt $parimages imageset=${root}_fl2.fits expression="(PI in [${elo2}:${ehi2}])&&(#XMMEA_EP)&&(PATTERN<=4)" 
else 
 evselect -w 0 -V 0 table=$cl_evt $parimages imageset=${root}_cl2.fits expression="(PI in [${elo2}:${ehi2}])&&(#XMMEA_EM)&&(PATTERN<=12)"
 evselect -w 0 -V 0 table=$fl_evt $parimages imageset=${root}_fl2.fits expression="(PI in [${elo2}:${ehi2}])&&(#XMMEA_EM)&&(PATTERN<=12)"
endif

echo "Creating images in band $elo3 - $ehi3 eV ...\n "

if ( $instr == EPN ) then
 evselect -w 0 -V 0 table=$cl_evt $parimages imageset=${root}_cl3.fits expression="(PI in [${elo3}:${ehi3}])&&(#XMMEA_EP)&&(PATTERN<=4)"
 evselect -w 0 -V 0 table=$fl_evt $parimages imageset=${root}_fl3.fits expression="(PI in [${elo3}:${ehi3}])&&(#XMMEA_EP)&&(PATTERN<=4)" 
else 
 evselect -w 0 -V 0 table=$cl_evt $parimages imageset=${root}_cl3.fits expression="(PI in [${elo3}:${ehi3}])&&(#XMMEA_EM)&&(PATTERN<=12)"
 evselect -w 0 -V 0 table=$fl_evt $parimages imageset=${root}_fl3.fits expression="(PI in [${elo3}:${ehi3}])&&(#XMMEA_EM)&&(PATTERN<=12)"
endif

echo "Creating images in band $elo4 - $ehi4 eV ...\n "

if ( $instr == EPN ) then
 evselect -w 0 -V 0 table=$cl_evt $parimages imageset=${root}_cl4.fits expression="(PI in [${elo4}:${ehi4}])&&(#XMMEA_EP)&&(PATTERN<=4)"
 evselect -w 0 -V 0 table=$fl_evt $parimages imageset=${root}_fl4.fits expression="(PI in [${elo4}:${ehi4}])&&(#XMMEA_EP)&&(PATTERN<=4)" 
else 
 evselect -w 0 -V 0 table=$cl_evt $parimages imageset=${root}_cl4.fits expression="(PI in [${elo4}:${ehi4}])&&(#XMMEA_EM)&&(PATTERN<=12)"
 evselect -w 0 -V 0 table=$fl_evt $parimages imageset=${root}_fl4.fits expression="(PI in [${elo4}:${ehi4}])&&(#XMMEA_EM)&&(PATTERN<=12)"
endif

#Get stats from image. i.e. c/s and exptime. 

set cl_cts1=`fimgstat ${root}_cl1.fits 0 99999999 | grep sum | awk '{print $8}'`
set cl_cts1=`echo $cl_cts1 | awk '{print int($1+0.5)}'`
set cl_exp1=`fkeyprint ${root}_cl1.fits EXPOSURE | grep ONTIME | awk '{print $2}'`
set fl_cts1=`fimgstat ${root}_fl1.fits 0 99999999 | grep sum | awk '{print $8}'`
set fl_cts1=`echo $fl_cts1 | awk '{print int($1+0.5)}'`
set fl_exp1=`fkeyprint ${root}_fl1.fits EXPOSURE | grep ONTIME | awk '{print $2}'`
set cl_rat1=`echo $cl_cts1 $cl_exp1 | awk '{print $1/$2}'`
set fl_rat1=`echo $fl_cts1 $fl_exp1 | awk '{print $1/$2}'`
set fl_inc1=`echo $cl_rat1 $fl_rat1 | awk '{print $2*100/$1}'`

set cl_cts2=`fimgstat ${root}_cl2.fits 0 99999999 | grep sum | awk '{print $8}'`
set cl_cts2=`echo $cl_cts2 | awk '{print int($1+0.5)}'`
set cl_exp2=`fkeyprint ${root}_cl2.fits EXPOSURE | grep ONTIME | awk '{print $2}'`
set fl_cts2=`fimgstat ${root}_fl2.fits 0 99999999 | grep sum | awk '{print $8}'`
set fl_cts2=`echo $fl_cts2 | awk '{print int($1+0.5)}'`
set fl_exp2=`fkeyprint ${root}_fl2.fits EXPOSURE | grep ONTIME | awk '{print $2}'`
set cl_rat2=`echo $cl_cts2 $cl_exp2 | awk '{print $1/$2}'`
set fl_rat2=`echo $fl_cts2 $fl_exp2 | awk '{print $1/$2}'`
set fl_inc2=`echo $cl_rat2 $fl_rat2 | awk '{print $2*100/$1}'`

set cl_cts3=`fimgstat ${root}_cl3.fits 0 99999999 | grep sum | awk '{print $8}'`
set cl_cts3=`echo $cl_cts3 | awk '{print int($1+0.5)}'`
set cl_exp3=`fkeyprint ${root}_cl3.fits EXPOSURE | grep ONTIME | awk '{print $2}'`
set fl_cts3=`fimgstat ${root}_fl3.fits 0 99999999 | grep sum | awk '{print $8}'`
set fl_cts3=`echo $fl_cts3 | awk '{print int($1+0.5)}'`
set fl_exp3=`fkeyprint ${root}_fl3.fits EXPOSURE | grep ONTIME | awk '{print $2}'`
set cl_rat3=`echo $cl_cts3 $cl_exp3 | awk '{print $1/$2}'`
set fl_rat3=`echo $fl_cts3 $fl_exp3 | awk '{print $1/$2}'`
set fl_inc3=`echo $cl_rat3 $fl_rat3 | awk '{print $2*100/$1}'`

set cl_cts4=`fimgstat ${root}_cl4.fits 0 99999999 | grep sum | awk '{print $8}'`
set cl_cts4=`echo $cl_cts4 | awk '{print int($1+0.5)}'`
set cl_exp4=`fkeyprint ${root}_cl4.fits EXPOSURE | grep ONTIME | awk '{print $2}'`
set fl_cts4=`fimgstat ${root}_fl4.fits 0 99999999 | grep sum | awk '{print $8}'`
set fl_cts4=`echo $fl_cts4 | awk '{print int($1+0.5)}'`
set fl_exp4=`fkeyprint ${root}_fl4.fits EXPOSURE | grep ONTIME | awk '{print $2}'`
set cl_rat4=`echo $cl_cts4 $cl_exp4 | awk '{print $1/$2}'`
set fl_rat4=`echo $fl_cts4 $fl_exp4 | awk '{print $1/$2}'`
set fl_inc4=`echo $cl_rat4 $fl_rat4 | awk '{print $2*100/$1}'`

echo "Count rates for different energy bands are as follows: \n"
echo " $elo1 - $ehi1 "
echo " Clean $cl_rat1 " 
echo " Flare $fl_rat1 "
echo " % Incr: $fl_inc1 \n"
echo " $elo2 - $ehi2 "
echo " Clean $cl_rat2 " 
echo " Flare $fl_rat2 "
echo " % Incr: $fl_inc2 \n"
echo " $elo3 - $ehi3 "
echo " Clean $cl_rat3 " 
echo " Flare $fl_rat3 "
echo " % Incr: $fl_inc3 \n"
echo " $elo4 - $ehi4 "
echo " Clean $cl_rat4 " 
echo " Flare $fl_rat4 "
echo " % Incr: $fl_inc4 \n"



