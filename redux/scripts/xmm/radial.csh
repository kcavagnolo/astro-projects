#! /bin/tcsh
#
# Script to make a radial profile, with adaptively sized annular bins, 
# so each bin has a signal to noise ratio above a specified limit, 
# out to a specified radius. 
#
# version 1.0 - 04/10/02 - adapted from arprof, and added optional arguments to apply masks, and work in angular bins
# version 1.1 - 16/9/03 - made small improvement to region exclusion with dmcopy
# version 1.2 - 5/11/03 - output detailed radial info file to allow rebinning later
# version 1.3 - 07/09/03 RFT - not convinced about the conversion to arcsec. 


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

--------------------`echo $0:t | tr '[a-z]' '[A-Z]'` version $version `date +%x`--------------------

EOF

if (( $#argv <= 6 )&&( $1 != help )) then
cat <<EOF

Script to make a radial profile of a FITS image, with different binning 
specifications. You can optionally provide a mask to exclude regions,
and/or a region file of exluded regions.
You may also specify a range of angles to perform the profile in

Use $0:t img xcent ycent rmax binreq bg root [-mmask] [-rreg] [-aang1,ang2]

arguments in [] are optional, and can be in any order
all coordinates/radii should be given in PHYSICAL pixels

Use $0:t help for detailed help

EOF
exit

else if ( $1 == help ) then

cat <<EOF

Script to make a radial profile of a FITS image, with different binning 
specifications. You can optionally provide a mask to exclude regions,
and/or a region file of exluded regions.
You may also specify a range of angles to perform the profile in

Use $0:t img xcent ycent rmax binreq bg root [-mmask] [-rreg] [-aang1,ang2]

arguments in [] are optional, and can be in any order
all coordinates/radii should be given in PHYSICAL pixels

Use $0:t help for detailed help
    img            - input fits image
    xcent          - x centre of source in PHYSICAL coords
    ycent          - y centre of source in PHYSICAL coords
    rmax           - maximum radius (pixels)
    binreq         - the binning requirement. Either use "b" "n" "s" or "m" followed 
                     by a number, e.g.
		     "b2"    - will make bins of width 2 pixels   
		     "n40"   - will make 40 equal width bins 
		     "s3"    - will make adaptive sized bins with min signal to
			       noise of 3
		     "m1000" - will make adaptive sized bins with min 1000 net counts
		     "ffile" - use bins given in "file", should be a CIAO format
			       region file of circles, as output by $0:t
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
set x=$2
set y=$3
set rmax=$4
set binreq=$5
set bg=$6
set root=$7
#set default optional arguments to none
set mask=none
set exc=none
set ang1=none
#sort out optional arguments
set i=8
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
  if (( $i == 24 )||( $i == 48 )||( $i == 72 )) then
  set exreg=`echo $exreg | tail +2c` #removes the '-' on the first expression
  dmcopy "${root}img.fits[(x,y)=field()-${exreg}]" ${root}img.fits clobber=yes
  set exreg
  endif
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

#set echo verbose
  
#apply angular mask to image if required
if ( $ang1 != none ) then
 echo "Selecting wedge between $ang1 and $ang2 from image...\n"
 #make initial mask use != to some arbitrary number, so mask is all 1s
 mkcond tmpmask $img '!=' 9.912333487
 #copy original image header over
 cphead $img+0 tmpmask+0 scale=yes
 #select wedge region with funtools
 funimage "tmpmask[PIE(${x},${y},${ang1},${ang2})]" ${root}wedge.fits
 #multiply image by mask
 farith ${root}wedge.fits ${root}img.fits \!${root}img.fits MUL
 rm ${root}wedge.fits tmpmask
 #copy original image header over
 cphead $img+0 ${root}img.fits+0 scale=yes
endif

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
set rmin=0
set rout=0
set binwidth=0

#set echo verbose

#set dummy exposure values - replace if I get round to including expmap in profile
set sexp=1
set bexp=1

#do regular binning
if (( $req == b ) || ( $req == n )) then
 if ( $req == n ) then
  set bwidth=`echo $rmax $nbins | awk '{print $1/$2}'`
 endif
 echo "Binning radial profile with bin width $bwidth pixels...\n"
 #set up output file
 printf "#r_in\tr_out\tS_counts\tS_area\tS_exp\tB_counts\tB_area\tB_exp\n" >! ${root}rdata.txt
 #step out through radius, and record results
 set radtest=1
 while ( $radtest == 1 )
 #set echo verbose
 set rin=$rout
 set rout=`echo $rin $bwidth | awk '{print $1+$2}'`
 dmstat "${root}img.fits[(x,y)=annulus(${x},${y},${rin},${rout})][opt null=0]" centroid=no >! tmptmp
 set counts=`grep sum tmptmp | awk '{print $2}'`
 set npix=`grep good tmptmp | awk '{print $2}'`
 #remove offset from counts
 set counts=`echo $counts $off $npix | awk '{print $1-($2*$3)}'` 
 set bgcounts=`echo $npix $bgrate | awk '{print $1*$2}'`
 set netcounts=`echo $counts $bgcounts | awk '{print $1-$2}'`
 set sn=`echo $netcounts $counts | awk '{print $1/sqrt($2)}'`
 set rmid=`echo $rin $rout | awk '{print ($1+$2)/2}'`
 set err=`echo $counts | awk '{print sqrt($1)}'`
 set err_rate=`echo $err $npix | awk '{print $1/$2}'`
 set net_rate=`echo $netcounts $npix | awk '{print $1/$2}'`
 set tot_rate=`echo $counts $npix | awk '{print $1/$2}'`
 echo "radius $rin - $rout -- net counts = $netcounts   area = $npix pixels   S/N = $sn"
 printf "%g\t%g\t%g\t%g\t%g\t%g\t%g\t%g\n" $rin $rout $counts $npix $sexp $mybgcounts $mybgnpix $bexp >> ${root}rdata.txt
#cat >> ${root}rdata.txt <<EOF
#$rin - $rout -- total_counts= $counts  bgcounts= $bgcounts  net= $netcounts  sn= $sn  area= $npix  net_count_rate= $net_rate error= $err_rate 
#EOF
 #echo "radius $rin - $rout -- total_counts= $counts bgcounts= $bgcounts net= $netcounts sn= $sn"
cat >> ${root}rprof.txt <<EOF
$rmid $tot_rate $err_rate
EOF
 set radtest=`echo $rout $rmax | awk '{if ($1>=$2) print 0; if ($1<$2) print 1}'`
 end
 echo "  radial profile written to ${root}rprof.txt and detailed radial information written to ${root}rdata.txt\n\n"
endif

#do adaptive binning
set snmax=0
set netmax=0
set snmaxtest=0
set netmaxtest=0
if (( $req == s ) || ( $req == m )) then
 #step out through radius, checking net counts and record results
 if ( $req == s ) then
  echo "Adaptively binning radial profile so all bins have S/N > $min..."
 else if ( $req == m ) then
  echo "Adaptively binning radial profile so all bins have net counts > $min..."
 endif
 #set echo verbose
 while ( $rad <= $rmax )
  while (( $test == 1 ) && ( $rad <= $rmax ))
   dmstat "${root}img.fits[(x,y)=annulus(${x},${y},${rmin},${rad})][opt null=0]" centroid=no >! tmptmp
   set counts=`grep sum tmptmp | awk '{print $2}'`
   set npix=`grep good tmptmp | awk '{print $2}'`
   #remove offset from counts
   set counts=`echo $counts $off $npix | awk '{print $1-($2*$3)}'` 
   set bgcounts=`echo $npix $bgrate | awk '{print $1*$2}'`
   set netcounts=`echo $counts $bgcounts | awk '{print $1-$2}'`
   set postest=`echo $netcounts | awk '{if ($1<0.0) print 0;if ($1>=0.0) print 1}'`
   if ( $postest == 1 ) then
    set sn=`echo $netcounts $counts | awk '{print $1/sqrt($2)}'`
    set err=`echo $counts | awk '{print sqrt($1)}'`
    set err_rate=`echo $err $npix | awk '{print $1/$2}'`
    set net_rate=`echo $netcounts $npix | awk '{print $1/$2}'`
    set tot_rate=`echo $counts $npix | awk '{print $1/$2}'`
    set snmaxtest=`echo $sn $snmax | awk '{if ($1<=$2) print 0;if ($1>$2) print 1}'`
    set netmaxtest=`echo $netcounts $netmax | awk '{if ($1<=$2) print 0;if ($1>$2) print 1}'`
   else
    set sn=0
    set err=0
    set err_rate=0
    set net_rate=0
    set tot_rate=0
    set snmaxtest=0
    set netmaxtest=0
   endif    
   if ( $snmaxtest == 1 ) then
    set snmax=$sn
    set snmaxstring="radius $rmin - $rad -- counts = $counts  net counts = $netcounts   area = $npix pixels   S/N = $sn   total count rate = $tot_rate  net_count_rate=$net_rate   error=$err_rate"
   endif
   if ( $netmaxtest == 1 ) then
    set netmax=$netcounts
    set netmaxstring="radius $rmin - $rad -- counts = $counts  net counts = $netcounts   area = $npix pixels   S/N = $sn   total count rate = $tot_rate  net_count_rate=$net_rate   error=$err_rate"
   endif
@ rad++
   if ( $req == s ) then
    set test=`echo $sn $min | awk '{if ($1<$2) print 1;if ($1>=$2) print 0}'`
   else if ( $req == m ) then
    set test=`echo $netcounts $min | awk '{if ($1<$2) print 1;if ($1>=$2) print 0}'`
   endif
   end
  if ( $test == 0 ) then
  set rad=`echo $rad | awk '{print $1-1}'`
  set rout=$rad
  set rmid=`echo $rmin $rad | awk '{print ($1+$2)/2}'`
cat >> ${root}rdata.txt <<EOF
radius $rmin - $rad -- total_counts= $counts  bgcounts= $bgcounts  net= $netcounts  sn= $sn  area= $npix  net_count_rate= $net_rate error= $err_rate 
EOF
  echo "radius $rmin - $rad -- net counts = $netcounts   area = $npix pixels   S/N = $sn"
  #echo "radius $rmin - $rad -- total_counts= $counts bgcounts= $bgcounts net= $netcounts sn= $sn area= $npix"
cat >> ${root}rprof.txt <<EOF
$rmid $tot_rate $err_rate
EOF
cat >> ${root}bins.reg <<EOF
circle(${x},${y},${rad})
EOF
  endif
  set test=1
  #endif
  set rminold=$rmin
  set rmin=$rad
  set snmax=0
  set netmax=0
  set snmaxtest=0
  set netmaxtest=0
@ rad++
  end
@ rad--
@ rad--
 echo "\nLast bin, not included in profile..."
 echo "radius $rminold-$rad -- net counts = $netcounts   area = $npix pixels   S/N = $sn   net_count_rate=$net_rate   error=$err_rate\n"
 echo "Max S/N in last bin was at"
 echo "$snmaxstring\n"
 echo "Max net counts in last bin was at"
 echo "$netmaxstring\n"
 echo "  radial profile written to ${root}rprof.txt and DS9 region file showing bins used written to ${root}bins.reg"
 echo "  detailed radial information written to ${root}rdata.txt"
 if ( $req == s ) then
  echo "  emission detected out to $rout pixels with S/N > $min\n"
 else if ( $req == m ) then
  echo "  emission detected out to $rout pixels with net counts > $min\n"
 endif
endif

#do region file binning
if ( $req == f ) then
 tail +2 $bfile >! tmpreg
 set nbins=`cat tmpreg | wc -l`
 echo "Binning radial profile with $nbins bins given in $bfile...\n"
 #step out through radius, and record results
 set rout=0
 set i=1
 while ( $i <= $nbins )
  #set echo verbose
  set rin=$rout
  set rout=`head -$i tmpreg | tail -1 | cut -d"," -f3 | cut -d")" -f1`
  dmstat "${root}img.fits[(x,y)=annulus(${x},${y},${rin},${rout})][opt null=0]" centroid=no >! tmptmp
  set counts=`grep sum tmptmp | awk '{print $2}'`
  set npix=`grep good tmptmp | awk '{print $2}'`
  #remove offset from counts
  set counts=`echo $counts $off $npix | awk '{print $1-($2*$3)}'` 
  set bgcounts=`echo $npix $bgrate | awk '{print $1*$2}'`
  set netcounts=`echo $counts $bgcounts | awk '{print $1-$2}'`
  set sn=`echo $netcounts $counts | awk '{print $1/sqrt($2)}'`
  set rmid=`echo $rin $rout | awk '{print ($1+$2)/2}'`
  set err=`echo $counts | awk '{print sqrt($1)}'`
  set err_rate=`echo $err $npix | awk '{print $1/$2}'`
  set net_rate=`echo $netcounts $npix | awk '{print $1/$2}'`
  set tot_rate=`echo $counts $npix | awk '{print $1/$2}'`
  echo "radius $rin - $rout -- net counts = $netcounts   area = $npix pixels   S/N = $sn"
cat >> ${root}rdata.txt <<EOF
radius $rin - $rout -- total_counts= $counts  bgcounts= $bgcounts  net= $netcounts  sn= $sn  area= $npix  net_count_rate= $net_rate error= $err_rate 
EOF
  #echo "radius $rin - $rout -- total_counts= $counts bgcounts= $bgcounts net= $netcounts sn= $sn"
cat >> ${root}rprof.txt <<EOF
$rmid $tot_rate $err_rate
EOF
@ i++
  end
 echo "  radial profile written to ${root}rprof.txt and detailed radial information written to ${root}rdata.txt\n\n"
endif

#remove unneeded files to save space...
rm  tmptmp ${root}tmpreg.txt >& /dev/null

#get pixel size from image header, and convert radius to arcsec
echo "Attempting to convert radii to arcsec..."
set pixsz=`fkeyprint $img cdelt1 | grep "CDELT1 " | awk '{print $3}'`
set cdelttest=`fkeyprint $img cdelt1 | grep "CDELT1 " |  wc -l`
if ( $cdelttest != 0 ) then
 set negtest=`echo $pixsz | cut -c1`
 if ( $negtest == - ) then
  set pixsz=`echo $pixsz | tail +2c`
 endif
 set flg=0 
 if ( $flg == 1 ) then
   set pixszarcs=`echo $pixsz | awk '{print $1*0.6}'`
  else
   set pixszarcs=`echo $pixsz | awk '{print $1*60}'`
 endif
 echo "  Image pixel size is $pixszarcs arcsec"
 set pixszarcs2=`echo $pixszarcs 40 | awk '{print $1/$2}'`
 cat ${root}rprof.txt | awk '{ printf("%g\t%g\t%g\n",$1*'$pixszarcs2',$2,$3 )}' >! ${root}rprof2.txt
 mv ${root}rprof2.txt ${root}rprof.txt
 echo "  Radial profile ${root}rprof.txt columns are    Radius(arcs)   counts/pix   error\n"
else 
 echo "  Image header doesn't appear to have a CDELT1 keyword - unable to convert radii, so keeping them in pixels.\n"
endif

#create sherpascript
cat >! ${root}sherpascript.txt <<EOF
#read data
read data ${root}rprof.txt ascii 1 2
read errors ${root}rprof.txt ascii 1 3

statistic chi dvar

paramprompt off                                                                         

#set up model
source=beta1d[king]+const1d[bck]
king.beta=0.666
king.xpos=0.0
king integrate on
#fix bg level to that measured in data
bck.c0=$bgrate
fre bck.c0

fit                                           
lp fit

log x
log y
xlabel "Radius (arcmin)"
ylabel "Counts/pix"
title ""
redraw       
                            
print postfile ${root}rprof_fit.eps

projection
goodness

exit

EOF

if ( $ciao != no ) then
 echo "Attempting to fit radial profile in sherpa...\n"
 sherpa ${root}sherpascript.txt >! ${root}sherpalog.txt
 tail -17 ${root}sherpalog.txt

 echo "  Sherpascript written to ${root}sherpascript.txt log written to ${root}sherpalog.txt and best fit plotted to ${root}rprof_fit.eps\n"
else echo "  Sherpascript written to ${root}sherpascript.txt\n"
endif

exit
