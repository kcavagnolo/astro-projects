#!/bin/tcsh -f
#
# Script to derive scaling factor between two data sets,
# e.g. source & bkg data.
 
nice +19

set dir     = /exgal7/jesper/WJ1325
set m1_raw  = {$dir}/m1_raw_evt.fits
set m1_src  = {$dir}/m1_clean.fits
set m1_bkg  = {$dir}/Blanksky/m1_skycast.fits
set m1_gti  = {$dir}/Cleaning/m1gti.fits
set m2_raw  = {$dir}/m2_raw_evt.fits
set m2_src  = {$dir}/m2_clean.fits
set m2_bkg  = {$dir}/Blanksky/m2_skycast.fits
set m2_gti  = {$dir}/Cleaning/m2gti.fits
set pn_raw  = {$dir}/pn_raw_evt.fits
set pn_src  = {$dir}/pn_clean.fits
set pn_bkg  = {$dir}/Blanksky/pn_skycast.fits
set pn_gti  = {$dir}/Cleaning/pngti.fits
set binsize = 4
set elo     = 10000
set ehi     = 12000
set pn_elo  = 12000
set pn_ehi  = 14000

set bin=`echo $binsize 20 | awk '{print $1*$2}'`
set parimages="xcolumn=X ycolumn=Y ximagebinsize=$bin yimagebinsize=$bin updateexposure=true imagedatatype=Real32 squarepixels=true imagebinning=binSize withimageset=Y writedss=true withimagedatatype=true keepfilteroutput=false"

#goto skip

# SCALING FROM HIGH-ENERGY FULL-IMAGE COUNTS
 
echo "Creating high-energy images..."
evselect -w 0 -V 0 table=$m1_src $parimages imageset=m1_src.fits expression='PI>='$elo'&&PI<='$ehi' &&PATTERN<=12 && #XMMEA_EM'
evselect -w 0 -V 0 table=$m1_bkg $parimages imageset=m1_bkg.fits expression='PI>='$elo'&&PI<='$ehi' &&PATTERN<=12 && #XMMEA_EM'
evselect -w 0 -V 0 table=$m2_src $parimages imageset=m2_src.fits expression='PI>='$elo'&&PI<='$ehi' &&PATTERN<=12 && #XMMEA_EM'
evselect -w 0 -V 0 table=$m2_bkg $parimages imageset=m2_bkg.fits expression='PI>='$elo'&&PI<='$ehi' &&PATTERN<=12 && #XMMEA_EM'
evselect -w 0 -V 0 table=$pn_src $parimages imageset=pn_src.fits expression='PI>='$pn_elo'&&PI<='$pn_ehi' &&PATTERN<=4 && #XMMEA_EP'
evselect -w 0 -V 0 table=$pn_bkg $parimages imageset=pn_bkg.fits expression='PI>='$pn_elo'&&PI<='$pn_ehi' &&PATTERN<=4 && #XMMEA_EP'

echo "Comparing count rates for each instrument..."
set inst = m1
set srcexpo = `fkeyprint {$inst}_src.fits EXPOSURE | grep ONTIME | awk '{print $2}'`
set bkgexpo = `fkeyprint {$inst}_bkg.fits EXPOSURE | grep ONTIME | awk '{print $2}'`
set srccts = `fimgstat {$inst}_src.fits INDEF INDEF | grep sum | awk '{print $8}'`
set bkgcts = `fimgstat {$inst}_bkg.fits INDEF INDEF | grep sum | awk '{print $8}'`
set srcctrate = `echo $srccts $srcexpo | awk '{print $1/$2}'`
set bkgctrate = `echo $bkgcts $bkgexpo | awk '{print $1/$2}'`
set ratio = `echo $srcctrate $bkgctrate | awk '{print $1/$2}'`
echo "High-energy count rate ratio (src/closed) is $ratio for $inst"
\rm {$inst}_src.fits 
\rm {$inst}_bkg.fits 

set inst = m2
set srcexpo = `fkeyprint {$inst}_src.fits EXPOSURE | grep ONTIME | awk '{print $2}'`
set bkgexpo = `fkeyprint {$inst}_bkg.fits EXPOSURE | grep ONTIME | awk '{print $2}'`
set srccts = `fimgstat {$inst}_src.fits INDEF INDEF | grep sum | awk '{print $8}'`
set bkgcts = `fimgstat {$inst}_bkg.fits INDEF INDEF | grep sum | awk '{print $8}'`
set srcctrate = `echo $srccts $srcexpo | awk '{print $1/$2}'`
set bkgctrate = `echo $bkgcts $bkgexpo | awk '{print $1/$2}'`
set ratio = `echo $srcctrate $bkgctrate | awk '{print $1/$2}'`
echo "High-energy count rate ratio (src/closed) is $ratio for $inst"
\rm {$inst}_src.fits 
\rm {$inst}_bkg.fits 

set inst = pn
set srcexpo = `fkeyprint {$inst}_src.fits EXPOSURE | grep ONTIME | awk '{print $2}'`
set bkgexpo = `fkeyprint {$inst}_bkg.fits EXPOSURE | grep ONTIME | awk '{print $2}'`
set srccts = `fimgstat {$inst}_src.fits INDEF INDEF | grep sum | awk '{print $8}'`
set bkgcts = `fimgstat {$inst}_bkg.fits INDEF INDEF | grep sum | awk '{print $8}'`
set srcctrate = `echo $srccts $srcexpo | awk '{print $1/$2}'`
set bkgctrate = `echo $bkgcts $bkgexpo | awk '{print $1/$2}'`
set ratio = `echo $srcctrate $bkgctrate | awk '{print $1/$2}'`
echo "High-energy count rate ratio (src/closed) is $ratio for $inst"
\rm {$inst}_src.fits 
\rm {$inst}_bkg.fits 





# SCALING FROM BROAD-BAND OUT-OF-FOV IMAGES
 
set elo = 300
set ehi = 10000

set inst = m1
set pat = 12

echo "Extracting source out-of-FOV events, creating image..."
evselect -w 0 -V 0 table=$m1_raw withfilteredset=Y filteredset={$inst}_oofov_src.fits filtertype=expression expression="GTI($m1_gti,TIME) && ( PI > 150 || PI < -150 ) && #XMMEA_16 && PATTERN<=$pat" destruct=Y keepfilteroutput=True
evselect -w 0 -V 0 table={$inst}_oofov_src.fits $parimages imageset=temp_src.fits expression='PI>='$elo'&&PI<='$ehi' &&PATTERN<='$pat''

echo "Extracting background out-of-FOV events, creating image..."
evselect -w 0 -V 0 table=$m1_bkg withfilteredset=Y filteredset={$inst}_oofov_bkg.fits filtertype=expression expression="( PI > 150 || PI < -150 ) && #XMMEA_16 && PATTERN<=12" destruct=Y keepfilteroutput=True
evselect -w 0 -V 0 table={$inst}_oofov_bkg.fits $parimages imageset=temp_bkg.fits expression='PI>='$elo'&&PI<='$ehi' &&PATTERN<='$pat''

set srcexpo = `fkeyprint temp_src.fits EXPOSURE | grep ONTIME | awk '{print $2}'`
set bkgexpo = `fkeyprint temp_bkg.fits EXPOSURE | grep ONTIME | awk '{print $2}'`
set srccts = `fimgstat temp_src.fits INDEF INDEF | grep sum | awk '{print $8}'`
set bkgcts = `fimgstat temp_bkg.fits INDEF INDEF | grep sum | awk '{print $8}'`
set srcctrate = `echo $srccts $srcexpo | awk '{print $1/$2}'`
set bkgctrate = `echo $bkgcts $bkgexpo | awk '{print $1/$2}'`
set ratio = `echo $srcctrate $bkgctrate | awk '{print $1/$2}'`
echo "Out-of-FOV count rate ratio (src/closed) is $ratio for $inst"
\rm temp_src.fits 
\rm temp_bkg.fits 



set inst = m2
set pat = 12

echo "Extracting source out-of-FOV events, creating image..."
evselect -w 0 -V 0 table=$m2_raw withfilteredset=Y filteredset={$inst}_oofov_src.fits filtertype=expression expression="GTI($m2_gti,TIME) && ( PI > 150 || PI < -150 ) && #XMMEA_16 && PATTERN<=$pat" destruct=Y keepfilteroutput=True
evselect -w 0 -V 0 table={$inst}_oofov_src.fits $parimages imageset=temp_src.fits expression='PI>='$elo'&&PI<='$ehi' &&PATTERN<='$pat''

echo "Extracting background out-of-FOV events, creating image..."
evselect -w 0 -V 0 table=$m2_bkg withfilteredset=Y filteredset={$inst}_oofov_bkg.fits filtertype=expression expression="( PI > 150 || PI < -150 ) && #XMMEA_16 && PATTERN<=12" destruct=Y keepfilteroutput=True
evselect -w 0 -V 0 table={$inst}_oofov_bkg.fits $parimages imageset=temp_bkg.fits expression='PI>='$elo'&&PI<='$ehi' &&PATTERN<='$pat''

set srcexpo = `fkeyprint temp_src.fits EXPOSURE | grep ONTIME | awk '{print $2}'`
set bkgexpo = `fkeyprint temp_bkg.fits EXPOSURE | grep ONTIME | awk '{print $2}'`
set srccts = `fimgstat temp_src.fits INDEF INDEF | grep sum | awk '{print $8}'`
set bkgcts = `fimgstat temp_bkg.fits INDEF INDEF | grep sum | awk '{print $8}'`
set srcctrate = `echo $srccts $srcexpo | awk '{print $1/$2}'`
set bkgctrate = `echo $bkgcts $bkgexpo | awk '{print $1/$2}'`
set ratio = `echo $srcctrate $bkgctrate | awk '{print $1/$2}'`
echo "Out-of-FOV count rate ratio (src/closed) is $ratio for $inst"
\rm temp_src.fits 
\rm temp_bkg.fits 




set inst = pn
set pat = 4

echo "Extracting source out-of-FOV events, creating image..."
evselect -w 0 -V 0 table=$pn_raw withfilteredset=Y filteredset={$inst}_oofov_src.fits filtertype=expression expression="GTI($pn_gti,TIME) && ( PI > 150 || PI < -150 ) && #XMMEA_16 && PATTERN<=$pat" destruct=Y keepfilteroutput=True
evselect -w 0 -V 0 table={$inst}_oofov_src.fits $parimages imageset=temp_src.fits expression='PI>='$elo'&&PI<='$ehi' &&PATTERN<='$pat''

echo "Extracting background out-of-FOV events, creating image..."
evselect -w 0 -V 0 table=$pn_bkg withfilteredset=Y filteredset={$inst}_oofov_bkg.fits filtertype=expression expression="( PI > 150 || PI < -150 ) && #XMMEA_16 && PATTERN<=$pat" destruct=Y keepfilteroutput=True
evselect -w 0 -V 0 table={$inst}_oofov_bkg.fits $parimages imageset=temp_bkg.fits expression='PI>='$elo'&&PI<='$ehi' &&PATTERN<='$pat''

set srcexpo = `fkeyprint temp_src.fits EXPOSURE | grep ONTIME | awk '{print $2}'`
set bkgexpo = `fkeyprint temp_bkg.fits EXPOSURE | grep ONTIME | awk '{print $2}'`
set srccts = `fimgstat temp_src.fits INDEF INDEF | grep sum | awk '{print $8}'`
set bkgcts = `fimgstat temp_bkg.fits INDEF INDEF | grep sum | awk '{print $8}'`
set srcctrate = `echo $srccts $srcexpo | awk '{print $1/$2}'`
set bkgctrate = `echo $bkgcts $bkgexpo | awk '{print $1/$2}'`
set ratio = `echo $srcctrate $bkgctrate | awk '{print $1/$2}'`
echo "Out-of-FOV count rate ratio (src/closed) is $ratio for $inst"
\rm temp_src.fits 
\rm temp_bkg.fits 


skip:


# SCALING FROM MEDIUM-BAND LARGE-RADIUS ANNULI

set elo = 2000
set ehi = 7000
set region = largerad.reg

evselect -w 0 -V 0 table=$m1_src $parimages imageset=m1_src.fits expression='PI>='$elo'&&PI<='$ehi' &&PATTERN<=12 && #XMMEA_EM'
evselect -w 0 -V 0 table=$m1_bkg $parimages imageset=m1_bkg.fits expression='PI>='$elo'&&PI<='$ehi' &&PATTERN<=12 && #XMMEA_EM'
evselect -w 0 -V 0 table=$m2_src $parimages imageset=m2_src.fits expression='PI>='$elo'&&PI<='$ehi' &&PATTERN<=12 && #XMMEA_EM'
evselect -w 0 -V 0 table=$m2_bkg $parimages imageset=m2_bkg.fits expression='PI>='$elo'&&PI<='$ehi' &&PATTERN<=12 && #XMMEA_EM'
evselect -w 0 -V 0 table=$pn_src $parimages imageset=pn_src.fits expression='PI>='$pn_elo'&&PI<='$pn_ehi' &&PATTERN<=4 && #XMMEA_EP'
evselect -w 0 -V 0 table=$pn_bkg $parimages imageset=pn_bkg.fits expression='PI>='$pn_elo'&&PI<='$pn_ehi' &&PATTERN<=4 && #XMMEA_EP'


#Filter annuli:
dmcopy "m1_src.fits[(x,y)=region($region)]" m1_src2.fits




exit

