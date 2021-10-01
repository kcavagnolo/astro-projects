#! /bin/tcsh 
#
# script to make a mosaiced image from the events lists of the 3 detectors
# will optionally create exposure maps, and weight by these.
#
# version 1 - 5/9/01 by BJM
# version 1.1 - 21/01/02 BJM corrected variable $root after calls to xmmexpmap.csh
# version 1.2 - 15/03/02 BJM keep normalised exposure maps, and make a mosaic using them too
# version 1.3 - 5/4/02 BJM renormalise expmaps so their max value when added together is 1
# version 1.4 - 11/11/02 BJM corrected, so expmaps normalised by ratio of exposure times 
# version 2.0 - 21/11/02 BJM use new version (2.0) of xmmexpmap.csh
# version 2.1 - 17/02/02 BJM optionally weight expmaps by effective area and add header keywords
# version 2.2 - 28/07/03 BJM use new version (2.1) of xmmexpmap.csh

set version=2.2

set host=`echo $HOST|tr '.' ' '|awk '{print $1}'`
if ( $host == lnxa ) then
 set BJMSCRIPTS=/exgal6/bjm/scripts #location of my other scripts
else
 set BJMSCRIPTS=/exgal1/rft/scripts #location of my other scripts
endif

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

--------------------`echo $0:t | tr '[a-z]' '[A-Z]'` version $version `date +%x`--------------------

EOF

if (( $#argv <= 7 )&&( $1 != help )) then
cat <<EOF

Script to make a mosaiced image from the events lists of the 3 detectors.
Will optionally create exposure maps, and weight by these.

Use $0:t pn m1 m2 attfile bin E_min E_max root -e[exptype] -r[reg] -b[bg] -w[weight]

arguments in [] are optional, and can be in any order

Use $0:t help for detailed help

EOF
exit

else if ( $1 == help ) then

cat <<EOF

Script to make a mosaiced image from the events lists of the 3 detectors.
Will optionally create exposure maps, and weight by these.

Use $0:t pn m1 m2 attfile bin E_min E_max root -e[exptype] -r[reg] -b[bg] -w[weight]

arguments in [] are optional, and can be in any order

    pn         - pn events list - enter "none" if not required
    mos1       - mos1 events list - enter "none" if not required
    mos2       - mos2 events list - enter "none" if not required (obviously, need at
                 least two images to mosaic!)
    attfile    - the attitude history file, enter none if you dont want to 
                 compute exposure maps
    bin        - binning factor of the images
    E_min      - lower energy bound (eV)
    E_max      - upper energy bound (eV)
    root       - root name for all output files

             ------- Optional Arguments ------

    exptype     - type of energy weighting to use. Must be one of
		  "mono" - make expmap for mean E of range
		  "spec" - make spectrally weighted expmap
		  default is mono
		  any other entry is taken to be the name of a spectral 
		  weights file (like those produced by sherpas spectrum.sl 
		  (use my specweights script to make one). This is the "correct" 
		  way to weight exposure maps.
    reg         - reg is a ciao format ds9 region file of a source
		  region within which the counts used to weight the
		  spectrally weighted expmap are calculated.
		  If no reg is specified, whole field is used
    bg          - bg is a ciao format ds9 region file of a bg
		  region within which the bg counts used to weight the
		  spectrally weighted expmap are calculated.
		  If no bg is entered, no bg subtraction is done when 
		  calculating the weights.
		  You can only use -bbg if you also use -rreg
    weight      - weight exposure maps by approximate effective area 
		  "y" divide the mos expmaps by a factor of 2.2 so 
 		  their effective area is approximately the same as pn
		  "n" do not (default)

EOF
exit
endif

#sort out required arguments
set pn=$1
set mos1=$2
set mos2=$3
set att=$4
set bin=$5
set en_low=$6
set en_high=$7
set root=$8
#set default optional arguments
set reg=none
set bg=none
set exptype=mono
set w=n

#sort out optional arguments
set i=9
while ( $i <= $#argv )
 set c=`echo $argv[${i}] | cut -c1`
 if ( $c != - ) then
  echo "ERROR: argument $i doesn't begin with -\n"
  exit
 endif
 set type=`echo $argv[${i}] | cut -c2`
 if (( $type != e )&&( $type != r )&&( $type != b )&&( $type != w )) then
  echo "ERROR: argument $i is not of type e r b or w\n"
  echo $type $argv[${i}]
  exit
 endif
 
 set arg=`echo $argv[${i}] | tail +3c`
 
 if ( $type == r ) then
  set reg=$arg
 else if ( $type == b ) then
  set bg=$arg
 else if ( $type == e ) then
  set exptype=$arg
 else if ( $type == w ) then
  set w=$arg
 endif
@ i++
end

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
if (( $reg == none )&&( $bg != none )) then
 echo "Error: may not use -bbg if -rreg is not used"
 exit
endif
if (( $exptype != mono )&&( $exptype != spec )) then
 if ( ! -e $exptype ) then
  echo "Error: exptype $exptype is not mono or spec, and the file $exptype does not exist"
  exit
 endif
endif
if ( $reg != none ) then
 if ( ! -e $reg ) then
  echo "ERROR: $reg does not exist"
  exit
 endif
endif
if ( $bg != none ) then
 if ( ! -e $bg ) then
  echo "ERROR: $bg does not exist"
  exit
 endif
endif
if (( $w != y )&&( $w != n)) then
 echo "Error: weight $w is not y or n"
 exit
endif

#check software dependencies
set exit=n
#check if SAS is installed
if ( ! $?SAS_DIR ) then
 echo "ERROR: SAS not installed - required by script."
 set exit=y
endif
#check if CIAO is installed
if ( ! $?ASCDS_INSTALL ) then
 echo "ERROR: CIAO not installed - required by script."
 set exit=y
endif
#check if FTOOLS is installed
if ( ! $?FTOOLS ) then
 echo "ERROR: FTOOLS not installed - required by script."
 set exit=y
endif
if ( $exit == y ) then
 exit
endif

echo "Input was $0 $*\n\n"

#set defaults
set pt=0
set m1t=0
set m2t=0

if ( $pn != none ) then
 set p=yes
 #determine if expmaps required
 if ( $att == none ) then
  #make image
  echo "Making PN image ..."
  evselect table="${pn}:EVENTS" withimageset=yes imageset=${root}pn_img.fits xcolumn=X ycolumn=Y imagebinning=binSize ximagebinsize=$bin yimagebinsize=$bin expression="PI in [${en_low}:${en_high}]" writedss=yes withimagedatatype=yes imagedatatype='Real32'
  echo "  image written to ${root}pn_img.fits\n"
  set pn_img=${root}pn_img.fits
  set pn_exp=""
 else
  echo "Making PN image, and generating exposure map..."
  ${BJMSCRIPTS}/xmmexpmap.csh $pn $att $bin ${en_low} ${en_high} $exptype ${root}pn_ -r$reg -b$bg |& tee ${root}pn_expmaplog.txt
  set ver=`grep "XMMEXPMAP.CSH" < ${root}pn_expmaplog.txt | cut -d"H" -f2 | cut -d"-" -f1`
  echo "  used XMMEXPMAP.CSH $ver - log written to ${root}pn_expmaplog.txt\n"
  rm ${root}pn_expmaplog.txt >& /dev/null
  set root=$8
  set pn_img=${root}pn_img.fits
  set pn_exp=${root}pn_expmap.fits
  set pt=`fimgstat ${root}pn_expmap.fits INDEF INDEF | grep "maximum of" | awk '{print $7}'`
  #add header keywords
  ${BJMSCRIPTS}/grpimg.csh $pn_img -e$pn_exp -n$en_low -N$en_high -B$bin 
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
  evselect table="${mos1}:EVENTS" withimageset=yes imageset=${root}m1_img.fits xcolumn=X ycolumn=Y imagebinning=binSize ximagebinsize=$bin yimagebinsize=$bin expression="PI in [${en_low}:${en_high}]" writedss=yes withimagedatatype=yes imagedatatype='Real32'
  echo "  image written to ${root}m1_img.fits\n"
  set m1_img=${root}m1_img.fits
  set m1_exp=""
 else
  echo "Making MOS1 image, and generating exposure map..."
  ${BJMSCRIPTS}/xmmexpmap.csh $mos1 $att $bin ${en_low} ${en_high} $exptype ${root}m1_ -r$reg -b$bg |& tee ${root}m1_expmaplog.txt
  set ver=`grep "XMMEXPMAP.CSH" < ${root}m1_expmaplog.txt | cut -d"H" -f2 | cut -d"-" -f1`
  echo "  used XMMEXPMAP.CSH $ver - log written to ${root}m1_expmaplog.txt\n"
  rm ${root}m1_expmaplog.txt >& /dev/null
  set root=$8
  set m1_img=${root}m1_img.fits
  set m1_exp=${root}m1_expmap.fits
  if ( $w == y ) then
    fcarith $m1_exp 2.2 \!$m1_exp DIV
    echo "  Divided MOS1 expmap by factor 2.2 for effective area."
  endif
  set m1t=`fimgstat ${root}m1_expmap.fits INDEF INDEF | grep "maximum of" | awk '{print $7}'`
  #add header keywords
  ${BJMSCRIPTS}/grpimg.csh $m1_img -e$m1_exp -n$en_low -N$en_high -B$bin 
 endif
else set m1_img=""
 set m1_exp=""
 set m1_exp_norm=""
 set m1=no
endif

if ( $mos2 != none ) then
 set m2=yes
 #determine if expmaps required
 if ( $att == none ) then
  #make image
  echo "Making MOS2 image ..."
  evselect table="${mos2}:EVENTS" withimageset=yes imageset=${root}m2_img.fits xcolumn=X ycolumn=Y imagebinning=binSize ximagebinsize=$bin yimagebinsize=$bin expression="PI in [${en_low}:${en_high}]" writedss=yes withimagedatatype=yes imagedatatype='Real32'
  echo "  image written to ${root}m2_img.fits\n"
  set m2_img=${root}m2_img.fits
  set m2_exp=""
 else
  echo "Making MOS2 image, and generating exposure map..."
  ${BJMSCRIPTS}/xmmexpmap.csh $mos2 $att $bin ${en_low} ${en_high} $exptype ${root}m2_ -r$reg -b$bg |& tee ${root}m2_expmaplog.txt
  set ver=`grep "XMMEXPMAP.CSH" < ${root}m2_expmaplog.txt | cut -d"H" -f2 | cut -d"-" -f1`
  echo "  used XMMEXPMAP.CSH $ver - log written to ${root}m2_expmaplog.txt\n"
  rm ${root}m2_expmaplog.txt >& /dev/null
  set root=$8
  set m2_img=${root}m2_img.fits
  set m2_exp=${root}m2_expmap.fits
  if ( $w == y ) then
    fcarith $m2_exp 2.2 \!$m2_exp DIV
    echo "  Divided MOS2 expmap by factor 2.2 for effective area."
  endif
  set m2t=`fimgstat ${root}m2_expmap.fits INDEF INDEF | grep "maximum of" | awk '{print $7}'`
  #add header keywords
  ${BJMSCRIPTS}/grpimg.csh $m2_img -e$m2_exp -n$en_low -N$en_high -B$bin 
 endif
else set m2_img=""
 set m2_exp=""
 set m2_exp_norm=""
 set m2=no
endif

if ( $att != none ) then
#because emosaic adds the exposure maps together before dividing the data by them,
#they must be renormalised so their max value when added together is 1
#to do this, add maximums together, then divide each by this total
 set ttot=`echo $pt $m1t $m2t | awk '{print $1+$2+$3}'`
 echo "Total exposure time is $ttot s - dividing each exposure map by this value to make normalised mosaic"
 if ( $p == yes ) then
  fcarith $pn_exp $ttot \!${root}pn_expmap_n.fits DIV
  set pn_exp_norm=${root}pn_expmap_n.fits
 endif
 if ( $m1 == yes ) then
  fcarith $m1_exp $ttot \!${root}m1_expmap_n.fits DIV
  set m1_exp_norm=${root}m1_expmap_n.fits
 endif
 if ( $m2 == yes ) then
  fcarith $m2_exp $ttot \!${root}m2_expmap_n.fits DIV
  set m2_exp_norm=${root}m2_expmap_n.fits
 endif
endif

#mosaic images
echo "Mosaicing images..."
if ( $att == none ) then
 emosaic imagesets="$pn_img $m1_img $m2_img" mosaicedset=${root}mosaic.fits withexposure=no sampling=point
 #add header keywords
 ${BJMSCRIPTS}/grpimg.csh ${root}mosaic.fits -n$en_low -N$en_high -B$bin 
echo "  mosaic written to ${root}mosaic.fits \n"
else
 emosaic imagesets="$pn_img $m1_img $m2_img" mosaicedset=${root}mosaic_n.fits withexposure=yes exposuresets="$pn_exp_norm $m1_exp_norm $m2_exp_norm" sampling=point
 #add header keywords
 ${BJMSCRIPTS}/grpimg.csh ${root}mosaic_n.fits -n$en_low -N$en_high -B$bin 
 echo "  normalised mosaic (created with exposure maps normalised so their maximum is 1) written to ${root}mosaic_n.fits\n"
endif

#remove unwanted images
if ( $pn != none ) then
 if ( $att != none ) then
#  rm ${root}pn_expmap_n.fits
 endif
endif
if ( $m1 != none ) then
 if ( $att != none ) then
#  rm ${root}m1_expmap_n.fits
 endif
endif
if ( $m2 != none ) then 
 if ( $att != none ) then
#  rm ${root}m2_expmap_n.fits
 endif
endif

#mosaicing the exposure maps

 emosaic imagesets="${root}pn_expmap_n.fits ${root}m1_expmap_n.fits ${root}m2_expmap_n.fits" mosaicedset=total_expmap.fits withexposure=no sampling=point

dmimgcalc ${root}mosaic_n.fits none test.fits op="imgout=sqrt(fabs(img1))"
farith test.fits total_expmap.fits err_image2.fits DIV
fimgtrim infile=err_image2.fits outfile=err_image.fits threshlo=0 threshup=100 const_lo=0.00001 const_up=0.00001
#rm test.fits err_image2.fits

exit
