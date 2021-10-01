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
set number=`ls -1 |grep fits| wc -l`
set i=1
while ( $i < $number)
 set number=`ls -1 |grep .fits| wc -l`
 set filename=`ls -1 | grep .fits | head -$i`
 gzip `echo $filename`
 set temp1=`echo $filename | cut -d"." -f1`.FTZ
 if ($filename !="") then
  mv ${filename}.gz $temp1
  set output2=`echo $temp1 | cut -d"." -f1`.FTZ
   if ($verb == 1 ) then
    echo $number "output file:" $output2
   endif
 endif
 end
exit
