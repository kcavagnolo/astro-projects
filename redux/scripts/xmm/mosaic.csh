#! /bin/tcsh 
#
# script to make a mosaiced image from the events lists of the 3 detectors
# will optionally weight by their exposure maps.
#
# version 1 - 5/9/01 by BJM
# version 1.1 - 21/01/02 BJM corrected variable $root after calls to xmmexpmap.csh
# version 1.2 - 15/03/02 BJM keep normalised exposure maps, and make a mosaic using them too
# version 1.3 - 5/4/02 BJM renormalise expmaps so their max value when added together is 1

set version=1.3

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

if ( $#argv != 8 ) then
cat <<EOF

Use xmmmosaic.csh pn mos1 mos2 attfile bin root

    pn         - pn events list - enter "none" if required
    mos1       - mos1 events list - enter "none" if required
    mos2       - mos2 events list - enter "none" if required (obviously, need at
                 least two images to mosaic!)
    attfile    - the attitude history file, enter none if you dont want to 
                 compute exposure maps
    bin        - binning factor of the images
    E_min      - lower energy bound (eV)
    E_max      - upper energy bound (eV)
    root       - root name for all output files

EOF
exit
endif

set pn=$1
set mos1=$2
set mos2=$3
set att=$4
set bin=$5
set en_low=$6
set en_high=$7
set root=$8

#check input files exist
if ( $pn != none ) then
 if ( ! -e $pn ) then
  echo "Error: $pn does not exist"
  exit
 endif
endif
if ( $mos1 != none ) then
 if ( ! -e $mos1 ) then
  echo "Error: $mos1 does not exist"
  exit
 endif
endif
if ( $mos2 != none ) then
 if ( ! -e $mos2 ) then
  echo "Error: $mos2 does not exist"
  exit
 endif
endif
if ( $att != none ) then
 if ( ! -e $att ) then
  echo "Error: $att does not exist"
  exit
 endif
endif

echo "Input was $0 $*\n\n"

if ( $pn != none ) then
 set p=yes
 #determine if expmaps required
 if ( $att == none ) then
  #make image
  echo "Making PN image ..."
  evselect table="${pn}:EVENTS" withimageset=yes imageset=${root}pn_img.fits xcolumn=X ycolumn=Y imagebinning=binSize ximagebinsize=$bin yimagebinsize=$bin expression="PI in [${en_low}:${en_high}]" writedss=yes
  echo "  image written to ${root}pn_img.fits\n"
  set pn_img=${root}pn_img.fits
  set pn_exp=""
 else
  echo "Making PN image, and generating exposure map..."
  /home/cavagnolo/research/redux/scripts/xmm/expmap.csh $pn $att $bin ${en_low} ${en_high} ${root}pn_ >! ${root}pn_expmaplog.txt
  set ver=`grep "XMMEXPMAP.CSH" < ${root}pn_expmaplog.txt | cut -d"H" -f2 | cut -d"-" -f1`
  echo "  used XMMEXPMAP.CSH $ver - log written to ${root}pn_expmaplog.txt\n"
  set root=$8
  rm ${root}pn_img_flat.fits
  set pn_img=${root}pn_img.fits
  set pn_exp=${root}pn_expmap.fits
  set pn_exp_norm=${root}pn_expmap_norm.fits
 endif
else set pn_img=""
 set pn_exp=""
 set pn_exp_norm=""
 set p=no
endif

if ( $mos1 != none ) then
 set m1=yes
 #determine if expmaps required
 if ( $att == none ) then
  #make image
  echo "Making MOS1 image ..."
  evselect table="${mos1}:EVENTS" withimageset=yes imageset=${root}mos1_img.fits xcolumn=X ycolumn=Y imagebinning=binSize ximagebinsize=$bin yimagebinsize=$bin expression="PI in [${en_low}:${en_high}]" writedss=yes
  echo "  image written to ${root}mos1_img.fits\n"
  set mos1_img=${root}mos1_img.fits
  set mos1_exp=""
 else
  echo "Making MOS1 image, and generating exposure map..."
  /home/cavagnolo/research/redux/scripts/xmm/expmap.csh $mos1 $att $bin ${en_low} ${en_high} ${root}mos1_ >! ${root}m1_expmaplog.txt
  set ver=`grep "XMMEXPMAP.CSH" < ${root}m1_expmaplog.txt | cut -d"H" -f2 | cut -d"-" -f1`
  echo "  used XMMEXPMAP.CSH $ver - log written to ${root}m1_expmaplog.txt\n"
  set root=$8
  rm ${root}mos1_img_flat.fits
  set mos1_img=${root}mos1_img.fits
  set mos1_exp=${root}mos1_expmap.fits
  set mos1_exp_norm=${root}mos1_expmap_norm.fits
 endif
else set mos1_img=""
 set mos1_exp=""
 set mos1_exp_norm=""
 set m1=no
endif

if ( $mos2 != none ) then
 set m2=yes
 #determine if expmaps required
 if ( $att == none ) then
  #make image
  echo "Making MOS2 image ..."
  evselect table="${mos2}:EVENTS" withimageset=yes imageset=${root}mos2_img.fits xcolumn=X ycolumn=Y imagebinning=binSize ximagebinsize=$bin yimagebinsize=$bin expression="PI in [${en_low}:${en_high}]" writedss=yes
  echo "  image written to ${root}mos2_img.fits\n"
  set mos2_img=${root}mos2_img.fits
  set mos2_exp=""
 else
  echo "Making MOS2 image, and generating exposure map..."
  /home/cavagnolo/research/redux/scripts/xmm/expmap.csh $mos2 $att $bin ${en_low} ${en_high} ${root}mos2_ >! ${root}m2_expmaplog.txt
  set ver=`grep "XMMEXPMAP.CSH" < ${root}m2_expmaplog.txt | cut -d"H" -f2 | cut -d"-" -f1`
  echo "  used XMMEXPMAP.CSH $ver - log written to ${root}m2_expmaplog.txt\n"
  set root=$8
  rm ${root}mos2_img_flat.fits
  set mos2_img=${root}mos2_img.fits
  set mos2_exp=${root}mos2_expmap.fits
  set mos2_exp_norm=${root}mos2_expmap_norm.fits
 endif
else set mos2_img=""
 set mos2_exp=""
 set mos2_exp_norm=""
 set m2=no
endif


if ( $att != none ) then
#because emosaic adds the exposure maps together before dividing the data by them,
#they must be renormalised so their max value when added together is 1
#find number of datasets used
if (( $p == yes) && ( $m1 == yes ) && ( $m2 == yes )) then
 set numsets=3
else if ((( $p == yes) && ( $m1 == yes ) && ( $m2 == no )) || (( $p == yes) && ( $m1 == no ) && ( $m2 == yes )) || (( $p == no ) && ( $m1 == yes ) && ( $m2 == yes ))) then 
 set numsets=2
else if ((( $p == yes) && ( $m1 == no ) && ( $m2 == no )) || (( $p == no) && ( $m1 == yes ) && ( $m2 == no )) || (( $p == no ) && ( $m1 == no ) && ( $m2 == yes ))) then 
 set numsets=1
else echo "\nERROR - not sure how many datasets are in use.\n\n"
 exit
endif
echo "$numsets data set(s) in use - renormalising exposure maps so their max value when added together is 1"
if ( $p == yes ) then
 fcarith $pn_exp_norm $numsets \!$pn_exp_norm DIV
endif
if ( $m1 == yes ) then
 fcarith $mos1_exp_norm $numsets \!$mos1_exp_norm DIV
endif
if ( $m2 == yes ) then
 fcarith $mos2_exp_norm $numsets \!$mos2_exp_norm DIV
endif

#mosaic images
echo "Mosaicing images..."
if ( $att == none ) then
 emosaic imagesets="$pn_img $mos1_img $mos2_img" mosaicedset=${root}mosaic.fits withexposure=no sampling=point
echo "  mosaic written to ${root}mosaic.fits \n"
else
 emosaic imagesets="$pn_img $mos1_img $mos2_img" mosaicedset=${root}mosaic.fits withexposure=yes exposuresets="$pn_exp $mos1_exp $mos2_exp" sampling=point
 emosaic imagesets="$pn_img $mos1_img $mos2_img" mosaicedset=${root}mosaic_norm.fits withexposure=yes exposuresets="$pn_exp_norm $mos1_exp_norm $mos2_exp_norm" sampling=point
echo "  mosaic written to ${root}mosaic.fits and normalised mosaic (created with exposure maps normalised so their maximum is 1) written to ${root}mosaic_norm.fits\n"
endif

#remove unwanted images
if ( $pn != none ) then
 rm $pn_img 
 if ( $att != none ) then
  rm $pn_exp ${root}pn_expmap_norm.fits
 endif
endif
if ( $mos1 != none ) then
 rm $mos1_img 
 if ( $att != none ) then
  rm $mos1_exp ${root}mos1_expmap_norm.fits
 endif
endif
if ( $mos2 != none ) then
 rm $mos2_img 
 if ( $att != none ) then
  rm $mos2_exp ${root}mos2_expmap_norm.fits
 endif
endif

exit
