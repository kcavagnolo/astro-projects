#!/bin/tcsh
# program to adaptively determine circular annuli for an XMM dataset. 
#
# The best way to tackle it would seem to be as follows:
#
# 1) determine what the total number of counts is in the image 
#    minus point sources
#
# 2) determine an appropriate number of counts for each bin. 
#    Perhaps such that there are N bins
#
# 3) Iteratively generate annuli until they contain n counts. 
#
# 4) Output several annuli files.
#
# AAARRRRRRRRGGGGGGGGHHHHHHH. Everything needs to be in image coordinates!
# Going to have to convert back and forth to get the correct results.
#
# For the moment I am not going to have a menu for this. Just going 
# to have manual input. 
#
# Caveat: only works for 10 bins...... need to change at some point.
#
# This will work better of I have number of counts/bin as input instead of 
# number of bins.
#
#
#
#

set version=1.1

#           version 1.1  RFT  14/04/06   making a few adjustments
#                                        1. got number of counts/bin as input
#                                        2. got energy range as input
#                                        3. I should probably point out that is is not necessary 
#                                        to convert to image coords, hence my setting of grad to 
#                                        zero. The number of bins is still input manually in the 
#                                        script. There is a lot of redundant code, but best 
#                                        to leave as it is.                                         


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

if ( $#argv < 8 ) then
cat <<EOF

Use find_annuli.csh events ncounts xcent ycent src elo ehi root

 events -      the events file for your observation (sources pre-removed)
 ncounts-      number of counts that you want to have in each bin.
 xcent  -      central x-value for your cluster 
 ycent  -      central y-value for your cluster
               (both in PHYSICAL coordinates)
 src    -      a CIAO region file with your point sources. 
 elo    -      lower energy range (ev)
 ehi    -      upper energy range (ev)
 root   -      root filename for your dataset

--------------Optional arguments---------------

 -ssrc2 -      A region that you want to exclude 
	       e.g. some detector noise that was not 
               removed from the cleaning.
               Use -ssrc2.reg. e.g. -sds9.reg

EOF
exit
endif

set events=$1
set ncounts=$2
set xcent=$3
set ycent=$4
set src=$5
set elo=$6
set ehi=$7
set root=$8

if ( ! -e $events ) then
echo "Error: file $events not found. Exiting...."
exit
endif

 if ( ! -e $src ) then
   echo "Error: $src does not exist"
   exit
 endif

set rtype=`head -1 $src | awk '{print $5}'`
if ( $rtype != CIAO ) then
 echo ERROR - REGION FILE "$src" DOES NOT APPEAR TO BE IN CIAO FORMAT"\n"
 exit
endif

#sort out optional args
set src2=0

set i=9
while ( $i <= $#argv )
 set c=`echo $argv[${i}] | cut -c1`
  if ( $c != - ) then
   echo "ERROR: argument $i must begin with a -\n"
   exit
  endif
 set type=`echo $argv[${i}] | cut -c2`
  if ( $type != s ) then
   echo "ERROR: argument $i is not of type s\n"
   exit
  endif
 
 set arg=`echo $argv[${i}] | tail +3c`
  if ( $type == s ) then 
    set src2=$arg
    set rtype=`head -1 $src2 | awk '{print $5}'`
    if ( $rtype != CIAO ) then
      echo ERROR - REGION FILE "$src" DOES NOT APPEAR TO BE IN CIAO FORMAT"\n"
      exit
    endif
  endif

@ i++
end

#get instrument

if ( -e tmpdmp1 ) then
 rm -f tmpdmp1
endif
fdump $events tmpdmp1 TIME - prdata=no showcol=no showunit=no showrow=no clobber=yes
set instr=`grep INSTRUME < tmpdmp1 | head -1 | awk '{print $2}'`
set instr=`echo $instr | tr -d "'"`
rm -f tmpdmp1
echo 'Instrument is '$instr "\n"

if ( $instr == EPN ) then
  set patexp="PATTERN<=4"
  set expr="#XMMEA_EP"
 else
  set patexp="PATTERN<=12"
  set expr="#XMMEA_EM"
endif

#make sure that you can see script sourcefilter.csh

if ( ! -e /data1/rft/rftscripts/xmmstring.csh ) then
echo "Can't find script xmmstring.csh. Exiting..."
exit
endif

#Creating image:

evselect table=${events}:EVENTS imagebinning='binSize' imageset='tmpimg1.fits' withimageset=yes xcolumn='X' ycolumn='Y' ximagebinsize=40 yimagebinsize=40 expression="${expr}&&(PI in [${elo}:${ehi}])&&(${patexp})&&(FLAG==0)" 

set ct_bin=$ncounts

#conversion factors between image and physical.
#dont have to adjust these
set ix1=1.0
set ix2=20
set px1=20.5
set px2=780.5

set grad=`echo $ix1 $ix2 $px1 $px2 | awk '{print ($2-$1)/($4-$3)}'`
set grad=1
set int=`echo $ix1 $grad $px1 | awk '{print $1-($2*$3)}'`

set t1=`echo $grad $int $px1 | awk '{print $3*$1 + $2}'`

set xcen=`echo $xcent $grad $int | awk '{print $1*$2+$3}'`
set ycen=`echo $ycent $grad $int | awk '{print $1*$2+$3}'`


set it=100
set rad=300
set flag=0
set r0=0
set i=0
set nbins=5


if ( -e ${root}_annuli.reg) then
 rm ${root}_annuli.reg
endif
touch ${root}_annuli.reg
echo "# Region file format: CIAO version 1.0" >! ${root}_annuli.reg

while ( $i != $nbins )

set flag=0
  while ( $flag != 1) # 1st while loop
  dmstat "tmpimg1.fits[(x,y)=annulus(${xcent},${ycent},${r0},${rad})][opt null=0]" centroid=no >! tmptmp

  set cts=`grep sum tmptmp | awk '{print $2}'`
  set cts2=$ct_bin
  echo $cts
  if ( $cts <= $cts2 ) then
    set rad=`echo $rad $it | awk '{print $1+$2}'`
   else
    set flag=1
  endif
  end # end of first while loop

@ i++

#need to convert these back to PHYS coords...
set t1=`echo $r0 $grad | awk '{print $1/$2}'`
set t2=`echo $rad $grad | awk '{print $1/$2}'`
printf "annulus(${xcent},${ycent},${t1},${t2})\n" >> ${root}_annuli.reg
if ( -e ${root}_an${i}.reg ) then
 rm ${root}_an${i}.reg
endif
touch ${root}_an${i}.reg
echo "# Region file format: CIAO version 1.0" >> ${root}_an${i}.reg
echo "annulus(${xcent},${ycent},${t1},${t2})" >> ${root}_an${i}.reg

#Need to loop through the sources to see if any of the points overlap with
# and of the annuli.....


grep -v CIAO $src | grep -v '^\s*$' >! ${root}tmpreg.txt
set numreg=`wc -l ${root}tmpreg.txt | awk '{print $1}'`
set j=1
  while ( $j <= $numreg ) # start of first while loop
  set tmpexreg=`head -$j ${root}tmpreg.txt | tail -1`
  set x=`echo $tmpexreg | cut -c8-14  `
  set y=`echo $tmpexreg | cut -c14-25 | cut -d"," -f2`
  set rprime=`echo $tmpexreg | cut -c23- | cut -d"," -f2 | cut -d ")" -f1`
  set big_rprime=`echo $x $y $xcent $ycent | awk '{print sqrt(($4-$2)^2 + ($3-$1)^2)}' | cut -d"." -f1  `
  set rtot=`echo $rprime $big_rprime | awk '{print $2+$1}'`
  set rmin=`echo $rprime $big_rprime | awk '{print $2-$1}'`
  set flag=0

  if ( $rmin > $t2 ) then
   set flag=1
  endif
  if ( $rtot < $t1 ) then
   set flag=1
  endif
  if ( $flag == 0 ) then 
   echo "-${tmpexreg}" >> ${root}_an${i}.reg
  endif

  @ j++
  end # end of second while loop

#This is where I add any extra regions to exclude...

if ( $src2 != 0 ) then
 grep -v CIAO $src2| grep -v '^\s*$' >! ${root}tmpreg2.txt
 set numreg2=`wc -l ${root}tmpreg2.txt | awk '{print $1}'`
 set k=1
  while ( $k <= $numreg2 ) 
   set tmpexreg=`head -$j ${root}tmpreg2.txt | tail -1`
   echo "-${tmpexreg}" >> ${root}_an${i}.reg
   @ k++
  end 
endif # for adding extra exclusion regions

echo $i

set r0=$rad
set rad=`echo $rad $it | awk '{print $1+$2}'`

end





rm tmptmp
rm ${root}tmpreg.txt
if ( -e ${root}tmpreg2.txt ) then
rm ${root}tmpreg2.txt
endif
echo "All Done!"
