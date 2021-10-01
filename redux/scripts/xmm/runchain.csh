#!/bin/tcsh

#script to run emchain and emchain.

#you must have your envormonment variables already set up


mkdir emchain
mkdir epchain

cd emchain

emchain |& tee emchain_log.txt
cd ../
if ( -e mos1_raw.fits) then
rm mos1_raw.fits
endif
if ( -e mos2_raw.fits) then
rm mos2_raw.fits
endif
cp emchain/*1MIEVL* ./mos1_raw.fits
cp emchain/*2MIEVL* ./mos2_raw.fits

cd epchain
epchain |& tee epchain_log.txt 
cd ../
if ( -e pn_raw.fits) then
rm pn_raw.fits
endif
cp epchain/*PIEVL* ./pn_raw.fits

#obtain correct background files if they don't exist
if ( -e mos1_raw_bg.fits) then
exit
endif

set raw=_raw.fits
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

exit







