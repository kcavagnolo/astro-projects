#!/bin/tcsh

#a quick program to create an image.

#input file is mos1_clean.fits & mos2_clean.fits

set events=mos1_raw.fits
set output=mos1_img.fits
set region=src.reg
set sourcefilter=`/data1/rft/rftscripts/xmmstring.csh $region | grep expression | awk '{print $4 }'`
    echo $sourcefilter

set parimages='xcolumn=X ycolumn=Y ximagebinsize=100 yimagebinsize=100 updateexposure=false imagedatatype=Real32 squarepixels=true imagebinning=binSize withimageset=Y writedss=false withimagedatatype=true keepfilteroutput=false'

set enlo=300
set enhi=10000

evselect -w 0 -V 0 table=$events imageset=$output expression='PI>='$enlo'&&PI<='$enhi'&&'$sourcefilter'' $parimages


