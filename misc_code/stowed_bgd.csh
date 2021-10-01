#!/bin/csh -f

############################################################
##  Made by M. Sun 
##   to process stowed background
############################################################

cp $CALDB/stowed/acis_E_0123567_stowed_evt_280807.fits ./bg.fits
mv bg.fits mmmb.fits
fcopy mmmb.fits"[events][status==bxxxxxxxxxxxx0000xxxxxxxxxxxxxxxx]" mmmbb.fits
mv mmmbb.fits mmmb.fits
punlearn dmcopy ; dmcopy "mmmb.fits[events][grade=0,2,3,4,6]" mmmb1.fits opt=all 
mv mmmb1.fits mmmb.fits
badpixfilter evtfile=mmmb.fits badpixfilter=/home/sunm/chandra_cal_dat/badpix_D o=mmmb1.fits
mv mmmb1.fits mmmb.fits
set j = `grep "DATAMODE" header | sed s/\'//g`
set mode = $j[2]
echo "mode" $mode
if ($mode == "VFAINT") then
    fcopy mmmb.fits"[events][status==bxxxxxxxx0xxxxxxxxxxxxxxxxxxxxxxx]" mmmb1.fits
    mv mmmb1.fits mmmb.fits
endif
mv mmmb.fits bg.fits
set fname1 = `ls *evt2_fi*fits*`
cp $fname1 evt2.fits
set aoff1 = `ls *aoff1*fits`
cp $aoff1 aoff.fits
punlearn reproject_events; reproject_events infile="tempbgd.fits" outfile="$bgevtfile" aspect="$asol" match=$evtfile random=0 mode=h verbose=$verb clobber=yes
mv bg.fits bg_stowed_E.fits
fextract bg_stowed_E.fits"[gti]" bg_stowed_E_gti.fits
rm evt2.fits aoff.fits
