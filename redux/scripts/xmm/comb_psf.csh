#! /bin/tcsh 
#
# script to combine the PSF's of the three telescopes, with PN weighted 2x MOS
# as PN has twice as many counts, then normalise the psf.
#
# version 1 - 11/01/02 by bjm

set version=1

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

--------------------`echo $0:t | tr '[a-z]' '[A-Z]'` version $version---------------------

EOF

if ( $#argv != 4 ) then
cat <<EOF

Use $0:t pnpsf mos1psf mos2psf out

    pnpsf             - input pn psf fits image
    mos1psf           - input mos1 psf fits image
    mos2psf           - input mos2 psf fits image
    out               - output fits image

EOF
exit
endif

set pnpsf=$1
set mos1psf=$2
set mos2psf=$3
set out=$4

#check input files exist
if ( ! -e $pnpsf ) then
 echo "Error: $pnpsf does not exist"
 exit
endif
if ( ! -e $mos1psf ) then
 echo "Error: $mos1psf does not exist"
 exit
endif
if ( ! -e $mos2psf ) then
 echo "Error: $mos2psf does not exist"
 exit
endif

echo "Input was $0 $*\n\n"

#weight pnpsf by factor 2
fcarith "${pnpsf}[0]" 2.0 \!$out mul

#add up psfs
farith "${pnpsf}[0]" "${mos1psf}[0]" \!tmppsf add
farith "tmppsf[0]" "${mos2psf}[0]" \!out add
rm tmppsf

#normalise
set sum=`fimgstat $out INDEF INDEF | grep sum | awk '{print $8}'`
fcarith "${out}[0]" $sum \!$out div
set sum=`fimgstat $out INDEF INDEF | grep sum | awk '{print $8}'`

echo "Output combined psf image $out created with a sum of $sum counts.\n"

exit
