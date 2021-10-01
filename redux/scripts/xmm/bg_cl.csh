#! /bin/tcsh
#       Script to clean background data sets
#
#
#

set version=1.1

#       Version 1.1 RFT  16/02/06   modified to allow a minimum value
#                                   and a normalisation

#  Erm, just found the script that I want. no major modifications made...



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

if ( $#argv != 4 ) then
cat <<EOF

Use bg_cl.csh bg_sky.fits 100 mos1_

    bgevents      - the events data for your observation

    max           - cut off value used to clean events datasets
    min           - minimum cut-off value
    norm          - normalisation of the background e.g. 0.9
    root          - root name for output files


EOF
exit
endif

set bg=$1
set max=$2
set min=$3
set norm=$4
set root=$5

if ( ! -e $bg ) then
 echo "Error: $bg does not exist"
 exit
endif

#get inst

fdump $bg tmpdmp1 TIME - prdata=no showcol=no showunit=no showrow=no clobber=yes
set instr=`grep INSTRUME < tmpdmp1 | head -1 | awk '{print $2}'`
set instr=`echo $instr | tr -d "'"`
echo 'Instrument is '$instr "\n"
if ($instr == EPN ) then
  set emin=12000
  set emax=14000
else
  set emin=10000
  set emax=12000
endif
rm tmpdmp1

set hist=${root}bg_histo.fits
set gti=${root}bg_gti.fits
set filt=${root}bg_clevt.fits

 evselect table=$bg withhistogramset=true histogramcolumn=TIME expression="(PI in [${emin}:${emax}])" histogramset=${hist} histogrambinsize=100

tabgtigen table=$hist gtiset=$gti expression="(COUNTS.lt.${max})"

evselect table=$bg withfilteredset=yes filteredset=$filt filtertype=expression expression="GTI(${gti},TIME)" destruct=yes keepfilteroutput=yes

set livetime=`fkeyprint "${bg}[EVENTS]" LIVETIME | grep "Live time" | awk '{print $2}'`

set livetime_clean=`fkeyprint "${filt}[EVENTS]" LIVETIME | grep "Live time" | awk '{print $2}'`

set time_diff=`echo $livetime $livetime_clean | awk '{print $1-$2}'`

echo "Old livetime was ${livetime}s, cleaning removed ${time_diff}s, leaving a livetime of ${livetime_clean}s"
