#!/bin/tcsh

#script to find the mean and sigma of a source dataset and 
#apply that cut to the background. an optional input is
# a scaling factor for the background which will be
#multiplied by the mean and sigma such that the correct parts of the
#background spectrum are selected. 


set version=1.0

# version 1.0 RFT 16/11/2005

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


if ( $#argv < 3 ) then
cat <<EOF

Use xmm_sclbg.csh events bgevents root -n[norm]

    events	- the events data for your observation

    bgevents	- a blank sky background data file

    region      - region to extract count rates

    root        - root filename   

-----optional argument---------
  
   -n[norm]     - a normalisation constant scaling
                  the background to the source dataset


EOF
exit
endif

set evt=$1
set bg=$2
set reg=$3
set root=$4

#check that input files exist

if ( ! -e $evt ) then
 echo "Error: $evt does not exist. Exiting...\n"
 exit
endif

if ( ! -e $bg ) then
 echo "Error: $bg does not exist. Exiting...\n"
 exit
endif

#check if normalisation is present. 
set i=5
set norm=1.0
while ( $i <= $#argv ) 
 set c=`echo $argv[${i}] | cut -c1`
 if ( $c != - ) then
  echo "ERROR: argument $i doesn't begin with a -\n"
  exit
 endif 
set type=`echo $argv[${i}] | cut -c2`
 if ( $type != n ) then
  echo "ERROR: argument $i is not of type n\n"
  echo $type $argv[${i}]
  exit
 endif

 set norm=`echo $argv[${i}] | tail +3c`

@ i++
end

#get inst
#Getting INST:
if ( -e tmpdmp1 ) then
 rm -f tmpdmp1
endif
fdump $evt tmpdmp1 TIME - prdata=no showcol=no showunit=no showrow=no clobber=yes
set instr=`grep INSTRUME < tmpdmp1 | head -1 | awk '{print $2}'`
set instr=`echo $instr | tr -d "'"`
rm -f tmpdmp1
echo 'Instrument is '$instr "\n"
if ( $instr == EPN) then
  set elo=12000
  set ehi=14000
 else
  set elo=10000
  set ehi=12000
endif

#not filtering on any flag or pattern. 

 set bin=100
 evselect -w 0 -V 0 table=$evt rateset=${root}_rate.fits expression="(PI in [${elo}:${ehi}])" withrateset=Y timebinsize=${bin} maketimecolumn=Y makeratecolumn=Y

fstatistic "${root}_rate.fits[RATE]" rate rows="-" outfile=${root}_stats.txt clobber=yes

set mean=`grep mean < ${root}_stats.txt | awk '{print $8}'`
set sigma=`grep standard < ${root}_stats.txt | awk '{print $9}'`
echo "mean, sigma" $mean $sigma

# gets 3 sigma clip. do need to divide....

#set meanrate=`echo $mean $bin $norm | awk '{print $1/($3*$2)}'`
#set sigrate=`echo $sigma $bin $norm | awk '{print $1/($3*$2)}'`
#echo $meanrate $sigrate

#doing same for background

#applying 3sig level to background.  

set sigmax=`echo $mean $sigma | awk '{print $1+3*$2}'`
set sigmin=`echo $mean $sigma | awk '{print $1-3*$2}'`
echo "max, min" $sigmax $sigmin

  evselect -w 0 -V 0 table=$bg rateset=${root}_bg_rate.fits expression="(PI in [${elo}:${ehi}])" withrateset=Y timebinsize=${bin} maketimecolumn=Y makeratecolumn=Y

#generate GTI


tabgtigen table=${root}_bg_rate.fits gtiset=${root}_bg_rate_gti.fits expression="( RATE.lt.$sigmax )&&( RATE.gt.$sigmin ) "

evselect table=$bg withfilteredset=yes filteredset=${root}_bg_clean_evt.fits filtertype=expression expression="GTI(${root}_bg_rate_gti.fits,TIME)" destruct=yes keepfilteroutput=yes

#calculate time loss due to flaring
set livetime=`fkeyprint "${bg}[EVENTS]" LIVETIME | grep "Live time" | awk '{print $2}'`
set livetime_clean=`fkeyprint "${root}_bg_clean_evt.fits[EVENTS]" LIVETIME | grep "Live time" | awk '{print $2}'`
set time_diff=`echo $livetime $livetime_clean | awk '{print $1-$2}'`
echo "Old LIVETIME was ${livetime}s; removed ${time_diff}s, leaving a LIVETIME of ${livetime_clean}s.\n"


exit
rm ${root}_rate.fits
rm ${root}_stats.txt
rm ${root}_bg_rate.fits
rm ${root}_bg_rate_gti.fits











