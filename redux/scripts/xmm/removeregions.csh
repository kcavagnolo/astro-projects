#! /star/local/bin/tcsh 
#
# version 1 - AMR 21/08/01

set version=1.0

nice +19

cat <<EOF 


--------------- REMOVEREGIONS VERSION $version -----------------

EOF

if ( $#argv != 5 ) then
 cat <<EOF 

Script to 

Use: removeregions evfile regfile column1 column2 root

    evfile  - events file 
    regfile - DS9 region file (circular regions at the moment)
    column1 - 1st column for selection expression (e.g. X)
    column2 - 2nd column for selection expression (e.g. Y)
    root    - root for output filenames

e.g. removeregions ev.fits ds9.reg DETX DETY r1

EOF
 exit
endif

set ev=$1
set reg=$2
set col1=$3
set col2=$4
set root=$5

if (! -e $ev ) then 
  echo 'error: input events fits file does not exist'
  exit
endif
if (! -e $reg ) then 
  echo 'error: input region file does not exist'
  exit
endif
if (-e $root'_rmreg.fits' ) then 
  echo 'Output file exists... overwriting'
  rm -f $root'_rmreg.fits'
endif

echo 'Removing circular regions from '$reg' ('$col1','$col2') from file '$ev'' 
echo 'Output file: '$root'_rmreg.fits'

cp $reg tempreg1
cp $ev tempev01

set n = 0
set count = 0
set nmax = 0
set nmaxtot = `wc -l < tempreg1`
@ nmax = $nmaxtot - 1
set expold=`echo ""`

while ($n != $nmax)
@ n = $n + 1
@ count = $count + 1
set exp1=`tail -$n tempreg1 | awk '{print $1}' | awk -F'(' '{print $2}'`
set exp2=`echo $exp1 | awk -F')' '{print $1}'`

if ($n == $nmax) then 
  set exp=""$expold"!(circle("$exp2,$col1,$col2"))"
  fselect tempev01 tempev02 $exp
  echo -n "${n} sources (of ${nmax}) removed\r"
  mv -f tempev02 tempev01
else
  if ($count != 20) then 
    set exp=""$expold"!(circle("$exp2,$col1,$col2"))&&"
    set expold=$exp
  endif
  if ($count == 20) then 
    set exp=""$expold"!(circle("$exp2,$col1,$col2"))"
    set expold=`echo ""`
    set count = 0  
    fselect tempev01 tempev02 $exp
    echo -n "${n} sources (of ${nmax}) removed\r"
    mv -f tempev02 tempev01
  endif
endif

end

rm -f tempreg1 
mv -f tempev01 $root'_rmreg.fits'

exit

#set pcdone=`echo $n $nmax | awk '{printf "%2d",(100*$1)/$2}'`
#echo -n "   Progress: ${pcdone}% complete\r"


