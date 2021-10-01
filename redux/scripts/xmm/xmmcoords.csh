#! /bin/tcsh 
#
# script to convert between coordinate systems in xmm. Give it an events
# list, and an image (optional), and a coordinate in WCS, sky or image coords
# and it will work out as many other coordinates as it can
#
# version 1 - 22/8/01 by BJM
# version 1.1 - 21/05/02 by BJM, extended to use esky2det to convert to det and raw coords

set version=1.1

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

--------------------XMMCOORDS.CSH version $version `date +%x`--------------------

EOF

if ( $#argv != 5 ) then
cat <<EOF

Use xmmcoords.csh events image coord x y

    evt         - Events list
    image       - Image created from events list (enter "none" if reqiured)
    coord       - input coordinate format (WCS sky or image)
    x           - x (or RA in form h:m:s) coord
    y           - y (or dec in form d:m:s) coord

EOF
exit
endif

set evt=$1
set image=$2
set coord=$3
set in_x=$4
set in_y=$5

#check input files exist
if ( ! -e $evt ) then
 echo "Error: $evt does not exist"
 exit
endif
if ( $image != none ) then
 if ( ! -e $image ) then
  echo "Error: $image does not exist"
  exit
 endif
endif
if (( $coord != WCS )&&( $coord != sky )&&( $coord != image )) then
 echo "ERROR: coord=$coord - must be WCS, sky, or image\n"
 exit
endif

echo "Input was $0 $*\n\n"

#convert sky to other coords
if ( $coord == sky ) then
 set x=$in_x
 set y=$in_y

 set temp=`xy2sky "${evt}[1]" xcol=x ycol=y $x $y | grep SKY`
 set ra=`echo $temp | awk '{print $4}' | cut -d"," -f1`
 set dec=`echo $temp | awk '{print $5}'`
 
 if ( $image != none ) then
  set temp=`sky2xy "${image}[0]" xcol=x ycol=y $ra $dec | grep pixel`
  set imgx=`echo $temp | awk '{print $4}' | cut -d"," -f1`
  set imgy=`echo $temp | awk '{print $5}'`
 endif

endif

#convert WCS to other coordinates
if ( $coord == WCS ) then
 set h=`echo $in_x | cut -d":" -f1`
 set m=`echo $in_x | cut -d":" -f2`
 set s=`echo $in_x | cut -d":" -f3`
 set d=`echo $in_y | cut -d":" -f1`
 set am=`echo $in_y | cut -d":" -f2`
 set as=`echo $in_y | cut -d":" -f3`

 set ra=`echo $h $m $s | awk '{print ($1+$2/60+$3/3600)*15}'`
 set dec=`echo $d $am $as | awk '{print $1+$2/60+$3/3600}'`

 set temp=`sky2xy "${evt}[1]" xcol=x ycol=y $ra $dec | grep pixel`
 set x=`echo $temp | awk '{print $4}' | cut -d"," -f1`
 set y=`echo $temp | awk '{print $5}'`
 
 if ( $image != none ) then
  set temp=`sky2xy "${image}[0]" xcol=x ycol=y $ra $dec | grep pixel`
  set imgx=`echo $temp | awk '{print $4}' | cut -d"," -f1`
  set imgy=`echo $temp | awk '{print $5}'`
 endif

endif

#convert image coords into others
if ( $coord == image ) then
 if ( $image == none ) then
  echo "ERROR: Need an image to convert from image coords, and image is set = none\n"
  exit
 endif

 set imgx=$in_x
 set imgy=$in_y

 set temp=`xy2sky "${image}[0]" xcol=x ycol=y $imgx $imgy | grep SKY`
 set ra=`echo $temp | awk '{print $4}' | cut -d"," -f1`
 set dec=`echo $temp | awk '{print $5}'`

 set temp=`sky2xy "${evt}[1]" xcol=x ycol=y $ra $dec | grep pixel`
 set x=`echo $temp | awk '{print $4}' | cut -d"," -f1`
 set y=`echo $temp | awk '{print $5}'`
endif

set h=`echo $ra | awk '{print int($1/15)}'`
set m=`echo $ra | awk '{print int($1/15%1*60)}'` 
set s=`echo $ra | awk '{print $1/15%1*60%1*60}'`

set d=`echo $dec | awk '{print int($1)}'`
set am=`echo $dec | awk '{print int($1%1*60)}'`
set as=`echo $dec | awk '{print $1%1*60%1*60}'`

#use esky2det to convert ra & dec into detector & raw coords
#This is a bit convoluted at the moment, because esky2det doesn't seem to work properly
#make fits input file
cat >! tmpcdfile <<EOF
RA E degrees
DEC E degrees
EOF
cat >! tmpdatafile <<EOF
$ra $dec
EOF
fcreate tmpcdfile tmpdatafile \!tmpoutfile.fits extname=sourcepos
cphead "${evt}[0]" "tmpoutfile.fits[0]"
#note the datetime parameter doesn't matter, as it is read from the correct one in the header, but you still need to give one on the command line - bug in esky2det!
esky2det datastyle=set intable="tmpoutfile.fits:sourcepos" outunit=det calinfostyle=user datetime='2006-01-10T09:50:00' withouttable=yes outtable=tmpesky2det.fits calinfostyle=srcset >&! tmpesky2detout
set detx=`fdump "tmpesky2det.fits[1]" STDOUT '*' '-' page=no | tail -1 | awk '{printf("%g\n",$2)}'`
set dety=`fdump "tmpesky2det.fits[1]" STDOUT '*' '-' page=no | tail -1 | awk '{printf("%g\n",$3)}'`
esky2det datastyle=set intable="tmpoutfile.fits:sourcepos" outunit=raw calinfostyle=user datetime='2006-01-10T09:50:00' withouttable=yes outtable=tmpesky2det.fits calinfostyle=srcset >&! tmpesky2detout
set rawx=`fdump "tmpesky2det.fits[1]" STDOUT '*' '-' page=no | tail -1 | awk '{printf("%g\n",$2)}'`
set rawy=`fdump "tmpesky2det.fits[1]" STDOUT '*' '-' page=no | tail -1 | awk '{printf("%g\n",$3)}'`
set ccdnr=`fdump "tmpesky2det.fits[1]" STDOUT '*' '-' page=no | tail -1 | awk '{printf("%g\n",$4)}'`
#rm tmpcdfile tmpdatafile tmpoutfile.fits tmpesky2detout tmpesky2det.fits

printf "WCS \t\tRA = $h $m $s \t\tDec = $d $am $as\n"
printf "WCSdeg \t\tRA = %g \t\t\tDec = %g\n" $ra $dec
printf "sky \t\tx = %g \t\t\ty = %g\n" $x $y
if ( $image != none ) then
 printf "image \t\tx = %g \t\t\ty = %g\n" $imgx $imgy
endif
printf "DET \t\tDETX = $detx \t\t\tDETY = $dety\n"
printf "RAW \t\tRAWX = $rawx \t\t\tRAWY = $rawy\n"
echo "CCD number = $ccdnr"
echo ""

exit 
