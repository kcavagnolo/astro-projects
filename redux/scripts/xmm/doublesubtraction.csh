#! /star/local/bin/tcsh 
#
# AMR 10/05/02

set version=1.2

nice +19

cat <<EOF 

--------------- DOUBLESUBTRACTION VERSION $version -----------------

EOF

if ( $#argv != 8 ) then
 cat <<EOF 

The script accepts a source spectrum and a scaled background spectrum
extracted from the same area (created using createspectra), together
with a separate large-radius source-minus-scaled-background
(i.e. "soft") spectrum (also from createspectra). A double-background
spectrum comprising the original background plus the area-scaled
"soft" background is created. This is then associated with the source
spectrum, along with arf and rmf files (from e.g. createspectra) and
grouped together as input to xspec.

Use: doublesubtraction srcspec scbgspec softspec dbspec arf rmf outgrp

  srcspec  - source spectrum (from createspectra) 
  scbgspec - scaled background spectrum, over same area (createspectra)
  softspec - "soft" (source-scaled bg) large-R spectrum (createspectra) 
  dbspec   - output double-background spectrum 
  arf      - arf file associated with srcspec (createspectra)
  rmf      - rmf file associated with srcspec (createspectra)
  outgrp   - output grouped pha file (input to xspec)
  grpno    - amount that data is going to be grouped (using grppha)


e.g doublesubtraction SPs SPscb SPsoft2 SPdb SParf SPrmf SPgrp.pha

EOF
 exit
endif

set src=$1
set bg=$2
set soft=$3
set dbg=$4
set arf=$5
set rmf=$6
set grp=$7
set group=$8
######################################################

#XMM sas started?
if (! -e $SAS_DIR) then 
  echo "It looks like XMM SAS has not been started...exiting..."
  exit
endif

punlearn fkeyprint

fkeyprint $src+1 BACKSCAL exact=yes outfile=tempba01 clobber=yes
set backscal_src=`grep BACKSCAL= < tempba01 | head -1 | awk '{print $2}'`
echo "Backscale for source spectrum: "$backscal_src
fkeyprint $bg+1 BACKSCAL exact=yes outfile=tempba02 clobber=yes
set backscal_bg=`grep BACKSCAL= < tempba02 | head -1 | awk '{print $2}'`
echo "Backscale for scaled background spectrum: "$backscal_bg
fkeyprint $soft+1 BACKSCAL exact=yes outfile=tempba03 clobber=yes
set backscal_soft=`grep BACKSCAL= < tempba03 | head -1 | awk '{print $2}'`
echo "Backscale for soft spectrum: "$backscal_soft

set ratio=`echo $backscal_bg $backscal_soft | awk '{print $1/$2}'`
echo "Soft spectrum area scaling: "$ratio

cp $bg tempsp01
cp $soft tempsp02
punlearn fcalc
punlearn fdelcol
fcalc tempsp01 tempsp03 COUNTSBG "COUNTS" clobber=yes
fdelcol tempsp03+1 COUNTS N Y
faddcol tempsp03 tempsp02 COUNTS
fcalc tempsp03 tempsp04 COUNTSSOFT "COUNTS" clobber=yes
fdelcol tempsp04+1 COUNTS N Y
fcalc tempsp04 tempsp05 COUNTSSOFTR "COUNTSSOFT * $ratio" clobber=yes
fcalc tempsp05 tempsp06 COUNTS "COUNTSBG + COUNTSSOFTR" clobber=yes
fdelcol tempsp06+1 COUNTSBG N Y
fdelcol tempsp06+1 COUNTSSOFT N Y
fdelcol tempsp06+1 COUNTSSOFTR N Y

if (-e $dbg ) then 
  echo 'output file exists... overwriting'
  rm -f $dbg
endif
cp tempsp06 $dbg

punlearn fkeyprint
punlearn fparkey
fkeyprint $src+1 EXPOSURE exact=yes outfile=tempba04 clobber=yes
set exposure=`grep EXPOSURE= < tempba04 | head -1 | awk '{print $2}'`
echo "Exposure for source spectrum: "$exposure
fparkey $exposure $dbg+1 EXPOSURE comm='Exposure' add=yes
fparkey $exposure $bg+1 EXPOSURE comm='Exposure' add=yes

#grppha

echo "Associating double-BG spectrum and grouping counts into >= ${group} bins"
if (-e $grp ) then 
  echo 'output grp file exists... overwriting'
  rm -f $grp
endif
punlearn grppha
grppha $src $grp comm="chkey backfile $dbg & chkey respfile $rmf & chkey ancrfile $arf & group min $group & exit" > tempba05

rm -f tempsp??
rm -f tempba??

echo 'Doublesubtraction completed.'
exit


