#!/bin/tcsh -f

if ( $#argv != 3 ) then
 cat <<EOF 

Usage 
./ADD_CHANDRA_IMAGES.csh  image1.fits  image2.fits  image_net.fits

Image2.fits is merged on to image1.fits producing image_net.fits
If image1.fits is small image2.fits may be cropped.

EOF
 exit
endif

set img1 = $1
set img2 = $2
set img3 = $3

set RA1 = `fdump ${img1} prdata=no page=no columns=- rows=- outfile=STDOUT | grep "CRVAL1 " | awk '{printf"%10.10f\n",$3}'`
set DEC1 = `fdump ${img1} prdata=no page=no columns=- rows=- outfile=STDOUT | grep "CRVAL2 " | awk '{printf"%10.10f\n",$3}'`
set X1 = `fdump ${img1} prdata=no page=no columns=- rows=- outfile=STDOUT | grep "CRPIX1 " | awk '{printf"%10.10f\n",$3}'`
set Y1 = `fdump ${img1} prdata=no page=no columns=- rows=- outfile=STDOUT | grep "CRPIX2 " | awk '{printf"%10.10f\n",$3}'`
set DX1 = `fdump ${img1} prdata=no page=no columns=- rows=- outfile=STDOUT | grep "CDELT1 " | awk '{printf"%10.10f\n",$3}'`
set DY1 = `fdump ${img1} prdata=no page=no columns=- rows=- outfile=STDOUT | grep "CDELT2 " | awk '{printf"%10.10f\n",$3}'`

set RA2 = `fdump ${img2} prdata=no page=no columns=- rows=- outfile=STDOUT | grep "CRVAL1 " | awk '{printf"%10.10f\n",$3}'`
set DEC2 = `fdump ${img2} prdata=no page=no columns=- rows=- outfile=STDOUT | grep "CRVAL2 " | awk '{printf"%10.10f\n",$3}'`
set X2 = `fdump ${img2} prdata=no page=no columns=- rows=- outfile=STDOUT | grep "CRPIX1 " | awk '{printf"%10.10f\n",$3}'`
set Y2 = `fdump ${img2} prdata=no page=no columns=- rows=- outfile=STDOUT | grep "CRPIX2 " | awk '{printf"%10.10f\n",$3}'`
set DX2 = `fdump ${img2} prdata=no page=no columns=- rows=- outfile=STDOUT | grep "CDELT1 " | awk '{printf"%10.10f\n",$3}'`
set DY2 = `fdump ${img2} prdata=no page=no columns=- rows=- outfile=STDOUT | grep "CDELT2 " | awk '{printf"%10.10f\n",$3}'`

set pi = 3.14159265358979
#set cos = `echo "scale=10;c(((${DEC1})+(${DEC2}))/2*${pi}/180)" | bc -l`
set cos = `echo "scale=10;c(${DEC2}*${pi}/180.0)" | bc -l`
#set DELTX = `echo "scale=10;(($RA1)-($RA2))*($DX1)/(${cos}) + ($X1)-($X2)"  | bc -l`
set DELTX = `echo "scale=10;(($RA2)-($RA1))/($DX2)*(${cos}) + ($X1)-($X2)"  | bc -l`
#set DELTY = `echo "scale=10;(($DEC1)-($DEC2))*($DY1) + ($Y1)-($Y2)"  | bc -l`
set DELTY = `echo "scale=10;(($DEC2)-($DEC1))/($DY2) + ($Y1)-($Y2)"  | bc -l`

echo " X-Shift = ${DELTX}  "
echo " Y-Shift = ${DELTY}  "

fimgmerge \
 infile=${img1} \
 list=${img2} \
 outfile=${img3} \
 xoffset=${DELTX} \
 yoffset=${DELTY} \
 clobber=yes
