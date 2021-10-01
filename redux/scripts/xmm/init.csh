#!/bin/csh
#	script to perform initial directory structure and get useful information
#	about each obervation. Please source this file to run it. You should start
#	in you directory with the odf and pps directories.

#	Version 1.0 RFT 14/09/2004


set version=1.0

set tmp_sys=`uname -a`
if ( `echo $tmp_sys|grep -c Generic` == "1" ) then
 set tmp_machine=solaris
 alias echo '/bin/echo'
endif
if ( "$tmp_sys" =~ Linux* ) then
 set tmp_machine=linux
endif

cat <<EOF

--------------------INIT.CSH version $version `date +%x`--------------------

EOF


set ls=`ls`
#mv *.ps ../lc/
set odfdir=`echo $ls | awk '{print $1}' -`
set ppsdir=`echo $ls | awk '{print $2}' -`

cd $odfdir
source /data1/rft/rftscripts/xmmsetup.csh
cd ../
set topdir=`pwd`

echo "Unzipping all FTZ files"
cd $ppsdir
source /data1/rft/rftscripts/FTZ2fits.csh
cd $topdir

mkdir work/
cd work
set raw=_raw.fits
ln -s ../$ppsdir/*M1*MIEVLI* mos1${raw}
ln -s ../$ppsdir/*M2*MIEVLI* mos2${raw}
ln -s ../$ppsdir/*PN*PIEVLI* pn${raw}
atthkgen atthkset=atthk.dat timestep=1  -V 0 |& tee atthkgen_log.txt

#getting filter parameters
set temp=T
dmkeypar mos1${raw} FILTER
set f1 =  `pget dmkeypar value`
set f1mod = `echo $f1 | cut -c1`
if ( $f1mod == $temp ) then
set  f1m=t
else
set  f1m=m
endif
dmkeypar mos2${raw} FILTER
set f2 =  `pget dmkeypar value`
set f2mod = `echo $f2 | cut -c1`
if ( $f2mod == $temp ) then
set  f2m=t
else
set  f2m=m
endif
dmkeypar pn${raw} FILTER
set f3 =  `pget dmkeypar value`
set f3mod = `echo $f3 | cut -c1`
if ( $f3mod == $temp ) then
set  f3m=t
else
set  f3m=m
endif

#getting window parameters
set temp=E
dmkeypar pn${raw} SUBMODE
set s3 =  `pget dmkeypar value`
set s3mod = `echo $s3 | cut -c16`
if ( $s3mod == $temp ) then
set  s3m=e
else
set  s3m=f
endif

set bg=_raw_bg.fits
#creating a link to all the background files
rm *${bg}
ln -s ../../bgevents/E2_f${f1m}0000_M1.fits mos1${bg}
ln -s ../../bgevents/E2_00f${f2m}00_M2.fits mos2${bg}
ln -s ../../bgevents/E2_0000${s3m}${f3m}_PN.fits pn${bg}

cd $topdir












