#! /bin/tcsh
#	Script xmmradial.csh
#	This is a script to generate radial profiles, using different
#	binning methods. This is basically a hack of BJM's script
#	radial.csh I recommend you use that script for any circular
#	source region which you will want to use annuli to generate your
#	profile. This script hopefully will deal with box-type bins.
#	e.g. I want to generate a minor axis diffuse emission profile.
#	My input image is generated fom BJM's xmmmosaic.csh script.
#	See the threads at http://www.sr.bham.ac.uk/xmm2/internal
#	for more guidance.
#
#version 1.0 RFT 29/10/04	Just starting with the basics. This script
#			will be a toned down version of radial.csh. Fancy features
#			etc will be added later.

set version=1.0

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

if ( $#argv <= 9 ) then

cat <<EOF

Script to make a radial profile of a FITS image, with different binning
specifications. You can optionally provide a mask to exclude regions,
and/or a region file of exluded regions. You will need to put in 4
co-ordinate points in order to define a line perpendicular to which your regions
will be extracted.


Use $0:t img x1 y1 x2 y2 rmax binreq bg root [-mmask] [-rreg] [-aang1,ang2]

arguments in [] are optional, and can be in any order
all coordinates/radii should be given in PHYSICAL pixels

Use $0:t help for detailed help
    img            - input fits image
    x1             - x1 point in PHYSICAL coords
    y1             - y1 point in PHYSICAL coords
    x2		   - x2 point in PHYSICAL coords
    y2		   - y2 point in PHYSICAL coords
    theta	   - rotation angle from vertical. Use the rotating box in ds9 to help you
    		     get this right.
    rmax           - maximum radius (pixels)
    binreq         - the binning requirement. Either use "b" "n" "s" or "m" followed
                     by a number, e.g.
		     "b2"    - will make bins of width 2 pixels
		     "n40"   - will make 40 equal width bins
		     "s3"    - will make adaptive sized bins with min signal to
			       noise of 3 (DISABLED)
		     "m1000" - will make adaptive sized bins with min 1000 net counts
		               (DISABLED)
		     "ffile" - use bins given in "file", should be a CIAO format
			       region file of circles, as output by $0:t (DISABLED)
    bg             - either bg in counts/pix, or a CIAO format
                     DS9 region file from which the bg rate will
                     be calculated.
    root           - root name for output files

             ------- Optional Arguments ------

    -mmask         - Apply mask to image before making profile
		     mask must be a fits image file of the same size as img
		     any pixels in mask with value 0 will be masked out of img
		     e.g. use exposure map to mask out chip gaps and bad pixels
		     Note that computed areas will take this into account
    -rreg          - Exclude regions in file "reg", where reg is a
		     CIAO format ds9 region of parts of the image
                     to exclude (e.g. point sources).
 		     Save these as INCLUDED regions!
		     Note that computed areas will take this into account
    -aang1,ang2    - Make profile between ang1 and ang2
                     angles are in degrees anti-cwise from y axis

EOF
exit
endif

#sort out required arguments
set img=$1
set x1=$2
set y1=$3
set x2=$4
set y2=$5
set angle=$6
set rmax=$7
set binreq=$8
set bg=$9
set root=$10
#set default optional arguments to none
set mask=none
set exc=none
set ang1=none

#sort out optional arguments
set i=11
while ( $i <= $#argv )
 set c=`echo $argv[${i}] | cut -c1`
 if ( $c != - ) then
  echo "ERROR: argument $i doesn't begin with a -\n"
  exit
 endif
 set type=`echo $argv[${i}] | cut -c2`
 if (( $type != m )&&( $type != r )&&( $type != a )) then
  echo "ERROR: argument $i is not of type m r or a\n"
  echo $type $argv[${i}]
  exit
 endif

 set arg=`echo $argv[${i}] | tail +3c`

 if ( $type == m ) then
  set mask=$arg
 else if ( $type == r ) then
  set exc=$arg
 else if ( $type == a ) then
  set ang1=`echo $arg | cut -d, -f1`
  set ang2=`echo $arg | cut -d, -f2`
 endif
@ i++
end

#check software dependencies
set exit=n
#check if CIAO is installed
if ( ! $?ASCDS_INSTALL ) then
 set ciao=no
 echo "WARNING: CIAO not installed - profile will be created, and a sherpascript will be written, but won't attempt to fit the profile within the script.\n"
else set ciao=yes
endif
#check if FTOOLS is installed
if ( ! $?FTOOLS ) then
 echo "ERROR: FTOOLS not installed - required by script."
 set exit=y
endif
if (( $ang1 != none )||( $mask != none )) then
 #check if FUNTOOLS is installed
 printenv PATH | grep funtools >/dev/null
 if ( $status == 1 ) then
  echo "ERROR: FUNTOOLS not installed - required by script."
  set exit=y
 endif
 #check if ZHTOOLS is installed
 if ( ! $?ZHTOOLS ) then
  echo "ERROR: ZHTOOLS not installed - required by script."
  set exit=y
 endif
endif
if ( $exit == y ) then
 exit
endif

#Find out what background is given
printf "%f\n" $bg >& /dev/null
if ( $status == 1 ) then
 set bgtype=reg
else set bgtype=num
endif

#check input files exist
if ( ! -e $img ) then
 echo "Error: $img does not exist"
 exit
endif
if ( $bgtype == reg ) then
 if ( ! -e $bg ) then
  echo "Error: $bg does not exist."
 endif
endif
if ( $exc != none ) then
 if ( ! -e $exc ) then
  echo "Error: $exc does not exist."
 endif
endif
if ( $mask != none ) then
 if ( ! -e $mask ) then
  echo "Error: $mask does not exist."
 endif
endif

#sort out binning requirement
set req=`echo $binreq | cut -c1`
if ( $req == b ) then
 set bwidth=`echo $binreq | tail +2c`
else if ( $req == n ) then
 set nbins=`echo $binreq | tail +2c`
else if ( $req == s ) then
 set min=`echo $binreq | tail +2c`
else if ( $req == m ) then
 set min=`echo $binreq | tail +2c`
else if ( $req == f ) then
 set bfile=`echo $binreq | tail +2c`
 if ( ! -e $bfile ) then
  echo "Error: $bfile does not exist"
  exit
 endif
else
 echo "ERROR - binreq $binreq must begin with b n s m or f\n\n"
 exit
endif

echo "Input was $0 $*\n\n"

set pi=3.141592654
set area=2220

cp $img ${root}img.fits

#set echo verbose

#add offset $off so no zero pixels, the after performing filtering,
#any pixel with value zero can be considered excluded from the image,
#and any pixel with value $off was actually zero in the original image
set off=1e-20
fcarith ${root}img.fits $off \!${root}img.fits ADD datatype=real

#apply mask to image if required
if ( $mask != none ) then
 echo "Applying mask $mask to image...\n"
 #make mask into 1s and 0s
 mkcond ${root}mask.fits $mask '!=' 0
 #multiply image by mask (note order important to keep header info)
 farith ${root}img.fits ${root}mask.fits \!${root}img.fits MUL
 rm ${root}mask.fits
endif

#exclude unwanted areas
if ( $exc != none ) then
 echo "Excluding unwanted regions..."
 set reg=$exc
 #convert region to fselect expression
 #reformat region file
 grep -v CIAO $reg | grep -v '^\s*$' >! ${root}tmpreg.txt
 set numreg=`wc -l ${root}tmpreg.txt | awk '{print $1}'`
 set i=1

 #filter out each region from image
 set exreg
 while ( $i <= $numreg )
  set tmpexreg=`head -$i ${root}tmpreg.txt | tail -1`
  set exreg="${exreg}-${tmpexreg}"
  @ i++
 end
 set exreg=`echo $exreg | tail +2c`
# echo 'dmcopy "'${root}'img.fits[(x,y)=field()-'${exreg}']" \\!'${root}'img.fits'
 dmcopy "${root}img.fits[(x,y)=field()-${exreg}]" \!${root}img.fits
 echo "  regions excluded, image showing excluded regions written to ${root}img.fits\n"
endif

#find bg rate if region
if ( $bgtype == reg ) then
 set reg=$bg
 echo "Computing bg count rate in $reg..."
 #use null=0 to exclude pixels with value zero from area calculation
 dmstat "${root}img.fits[(x,y)=region(${reg})][opt null=0]" centroid=no >! tmptmp
 set bgcounts=`grep sum tmptmp | awk '{print $2}'`
 set bgnpix=`grep good tmptmp | awk '{print $2}'`
 #remove offset from counts
 set bgcounts=`echo $bgcounts $off $bgnpix | awk '{print $1-($2*$3)}'`
 echo "  bgcounts=$bgcounts   bgarea=$bgnpix"
 set bgrate=`echo $bgcounts $bgnpix | awk '{print $1/$2}'`
 set mybgcounts=$bgcounts
 set mybgnpix=$bgnpix
else
 set bgrate=$bg
 set mybgcounts=$bg
 set mybgnpix=1
endif

echo "  bg rate = $bgrate counts/pixel\n"

#remove old output file and set initial params
if ( -e ${root}rprof.txt ) then
 rm ${root}rprof.txt
endif
touch ${root}rprof.txt
if ( -e ${root}bins.reg ) then
 rm ${root}bins.reg
endif
echo "# Region file format: CIAO version 1.0" >! ${root}bins.reg
if ( -e ${root}rdata.txt ) then
 rm ${root}rdata.txt
endif
touch ${root}rdata.txt
set test=1
set rad=1
set netcounts=0
set binwidth=0
set blength=`echo $x1 $x2 $y1 $y2 | awk '{print sqrt ( ($2-$1)*($2-$1) + ($4-$3)*($4-$3)) }'`
set x3=`echo $x1 $x2 | awk '{print ($1+($2-$1)/2)}'`
set y3=`echo $y1 $y2 | awk '{print ($1+($2-$1)/2)}'`
set x0=`echo $x3 $y1 $y2 $rmax $blength | awk '{print $1-$4*sqrt(($3-$2)^2)/$5}'`
set y0=`echo $y3 $x1 $x2 $rmax $blength | awk '{print $1-$4*sqrt(($3-$2)^2)/$5}'`
set x1in=$x0
set y1in=$y0

#set dummy exposure values - replace if I get round to including expmap in profile
set sexp=1
set bexp=1

#do regular binning
if (( $req == b ) || ( $req == n )) then
  if ( $req == n ) then
   set bwidth=`echo $rmax $nbins 2 | awk '{print $1/($2*$3)}'`
  endif
  set r0=`echo $rmax $bwidth | awk '{print -$1-$2}'`
 echo "Binning radial profile with bin width $bwidth pixels...\n"
 #set up output file
 printf "#r_mid\tS_counts\tS_area\tS_exp\tB_counts\tB_area\tB_exp\n" >! ${root}rdata.txt
 #step out through radius, and record results
 set radtest=1
 while ( $radtest == 1 )
 #set echo verbose
 echo --------
 set xin=$x1in
 set yin=$y1in
 dmstat "${root}img.fits[(x,y)=rotbox(${xin},${yin},${bwidth},${blength},${angle})][opt null=0]" centroid=no >! tmptmp
# echo x0=$x0 y0=$y0
 set x1in=`echo $xin $y1 $y2 $bwidth $blength | awk '{print $1+ $4*sqrt(($3-$2)^2)/$5}'`
 set y1in=`echo $yin $x1 $x2 $bwidth $blength | awk '{print $1+ $4*sqrt(($3-$2)^2)/$5}'`
# echo $x1in $y1in
 set counts=`grep sum tmptmp | awk '{print $2}'`
 set npix=`grep good tmptmp | awk '{print $2}'`
 #remove offset from counts
 set counts=`echo $counts $off $npix | awk '{print $1-($2*$3)}'`
 set bgcounts=`echo $npix $bgrate | awk '{print $1*$2}'`
 set netcounts=`echo $counts $bgcounts | awk '{print $1-$2}'`
 set sn=`echo $netcounts $counts | awk '{print $1/sqrt($2)}'`
 set rmid=`echo $r0 $bwidth | awk '{print $2+$1 }'`
 set r0=$rmid
# echo rmid=$rmid r0=$r0
 set err=`echo $counts | awk '{print sqrt($1)}'`
 set err_rate=`echo $err $npix | awk '{print $1/$2}'`
 set net_rate=`echo $netcounts $npix | awk '{print $1/$2}'`
 set tot_rate=`echo $counts $npix | awk '{print $1/$2}'`
 echo "distance $rmid -- net counts = $netcounts   area = $npix pixels   S/N = $sn"
 echo "xin= $xin -- yin = $yin"
 printf "%g\t%g\t%g\t%g\t%g\t%g\t%g\t%g\n" $rmid $counts $npix $sexp $mybgcounts $mybgnpix $bexp >> ${root}rdata.txt
#cat >> ${root}rdata.txt <<EOF
#$rin - $rout -- total_counts= $counts  bgcounts= $bgcounts  net= $netcounts  sn= $sn  area= $npix  net_count_rate= $net_rate error= $err_rate
#EOF
 #echo "radius $rin - $rout -- total_counts= $counts bgcounts= $bgcounts net= $netcounts sn= $sn"
cat >> ${root}rprof.txt <<EOF
$rmid $net_rate $err_rate
EOF
#changing the output to net_rate instead of tot_rate. I want my data background subtracted
#echo $rmid $tot_rate $net_rate
#echo radtest=$radtest
 set radtest=`echo $r0 $rmax | awk '{if ($1>=$2) print 0; if ($1<$2) print 1}'`
 end
 echo "  radial profile written to ${root}rprof.txt and detailed radial information written to ${root}rdata.txt\n\n"
endif

#remove unneeded files to save space...
rm ${root}img.fits tmptmp ${root}tmpreg.txt >& /dev/null

#get pixel size from image header, and convert radius to arcsec
echo "Attempting to convert radii to arcsec..."
set pixsz=`fkeyprint $img cdelt1 | grep "CDELT1 " | awk '{print $3}'`
set cdelttest=`fkeyprint $img cdelt1 | grep "CDELT1 " |  wc -l`
if ( $cdelttest != 0 ) then
 set negtest=`echo $pixsz | cut -c1`
 if ( $negtest == - ) then
  set pixsz=`echo $pixsz | tail +2c`
 endif
 set pixszarcs=`echo $pixsz | awk '{print $1*3600}'`
 echo "  Image pixel size is $pixszarcs arcsec"

 cat ${root}rprof.txt | awk '{ printf("%g\t%g\t%g\n",$1*'$pixszarcs',$2,$3 )}' >! ${root}rprof2.txt
 mv ${root}rprof2.txt ${root}rprof.txt
# echo "  Radial profile ${root}rprof.txt columns are    Radius(arcs)   counts/pix   error\n"
else
 echo "  Image header doesn't appear to have a CDELT1 keyword - unable to convert radii, so keeping them in pixels.\n"
endif

echo "Attempting to convert counts/pixel into photons/s/arcsec^2/cm^2..."

set exp1=`fkeyprint $img emsce001 | grep "EMSCE001" | awk '{print $2}' | tail +11c`
set exp2=`fkeyprint $img emsce002 | grep "EMSCE002" | awk '{print $2}' | tail +11c`
set exp3=`fkeyprint $img emsce003 | grep "EMSCE003" | awk '{print $2}' | tail +11c`
set exp=`echo $exp1 $exp2 $exp3 | awk '{print $1+$2+$3}'`

set conv=`echo $exp $area $pixszarcs | awk '{print $1*$2* $3^2 }'`

 cat ${root}rprof.txt | awk '{ printf("%g\t%g\t%g\n",$1,$2/'$conv',$3/'$conv' )}' >! ${root}rprof2.txt
 mv ${root}rprof2.txt ${root}rprof.txt
 echo "  Radial profile ${root}rprof.txt columns are    Radius(arcs)  photons/s/arcsec^2/cm^2    error\n"

 cp ${root}rprof.txt radprofiledata.txt



