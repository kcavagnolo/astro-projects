#! /bin/tcsh
# Script to convert FTZ data files into fits files
#
# version 1.0 - RFT 21/07/04/
#
set version=1.0
nice +19

cat <<EOF
--------------------FTZ2FITS.CSH version $version `date +%x`--------------------
EOF

set verb=1
set number=`ls -1 |grep FTZ| wc -l`
set i=1
while ( $i <= $number)
set number=`ls -1 |grep .FTZ| wc -l`
#echo $number
set filename=`ls -1 | grep .FTZ | head -$i`
#echo $filename
set temp1=`echo $filename | cut -d"." -f1`.fits.gz
if ($filename !="") then
 mv $filename $temp1
 gunzip `echo $temp1`
 set output2=`echo $temp1 | cut -d"." -f1`.fits
  if ($verb == 1 ) then
   echo $number "output file:" $output2
  endif
endif
end
exit
