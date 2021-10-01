#!/bin/tcsh
# Script to do 3sigma cleaning on lightcurve and produce gti file
# and a cleaned events table. Cleans the lightcurve recursively
# until the mean count rate per bin is constant. Semi optional
# argument -mmax required to run the script completely.
#
# by default uses energy 10-15keV to produce lightcurve for cleaning.
#
# NOTE: Requires CIAO to be started to plot the lightcurve.
# NOTE: lightcurve produced using chips is the unfiltered data.
#
#
#
# This is going to be very similar to BJM's script 'xmmlight_clean.csh
# However, I have had a few problems running it, so I am starting again
# from first principles.
# version 1.0 -	RFT 16/09/2004
# version 1.1 - RFT 28/09/2004 Am altering the format of the input files a little.
#		The argument mmax must be input to run the recursion algorithm.
#		Otherwise all files are deleted apart from the time histogram.
#		The user will be told to look at this table and select a max value.

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

if ( $#argv < 4 ) then
cat <<EOF

Use xmmlight_clean.csh table expression binsize root [-eelow] [-Eehi] [-mmax]

arguments in [] are optional, and can be in any order

    table       - your input table (e.g. pn_raw_evt.fits)
    expression  - the filter expression you wish to apply to the cleaned events,
		  e.g '#XMMEA_EP&&(PATTERN <= 4)'. Enter "none" if required.
		  Note that because of a bug in evselect you should put the FLAG
                  filter before the PATTERN filter as above, otherwise you may
                  have problems later with arfgen.
    binsize     - binsize to use in seconds.
    root        - The root name for all output files.

             ------- Semi Optional Arguments -----

    -mmax	- "max" is an upper limit on the number of counts per bin considered
		  by the cleaning algorithm. Useful with noisy lightcurves, to
                  exclude big flares, with e.g. -m500 if 50s bins are used. This is
		  required to run the script completely.

             ------- Optional Arguments ------

    -eelo       - "elo" is a lower energy bound (eV) default=10000 e.g -e10000
    -Eehi       - "ehi" is an upper energy bound (eV) default=15000 e.g. -E15000


EOF
exit
endif

set table=$1
set expr="$2"
set bin=$3
set root=$4

#rm -fr temp*
#Currently bypassing the menu system and leaving optional args alone.
#set table=../pn_raw.fits
#set expr='#XMMEA_EP&&(PATTERN <= 4)'
#set bin=50
#set root=temp


#check input files exist
set tabroot=`echo $table | cut -d ":" -f1`
if ( ! -e $tabroot ) then
 echo "Error: $tabroot does not exist"
 exit
endif
#set default optional arguments to none
set elo=10000
set ehi=15000
set max="none"

#sort out optional arguments
set i=5
while ( $i <= $#argv )
 set c=`echo $argv[${i}] | cut -c1`
 if ( $c != - ) then
  echo "ERROR: argument $i doesn't begin with a -\n"
  exit
 endif
 set type=`echo $argv[${i}] | cut -c2`
 if (( $type != e )&&( $type != E )&&( $type != m )) then
  echo "ERROR: argument $i is not of type e, E or m\n"
  echo $type $argv[${i}]
  exit
 endif

 set arg=`echo $argv[${i}] | tail +3c`

 if ( $type == e ) then
  set elo=$arg
 else if ( $type == E ) then
  set ehi=$arg
 else if ( $type == m ) then
  set max=$arg
 endif
@ i++
end

echo "Input was $0 $*\n\n"

#check if CIAO is installed
if ( ! $?ASCDS_INSTALL ) then
 set ciao=no
 echo "WARNING: CIAO not installed - lightcurve cleaning will work, but the results will not be plotted.\n"
else set ciao=yes
endif

#Creating a rate curve
#echo "Creating rate curve in range ${elo}-${ehi} eV..."
#evselect -V 0 table=${table} withrateset=Y timebinsize=${bin} rateset=${root}_rate.fits maketimecolumn=Y expression="(PI in [${elo}:${ehi}])"
#echo Rate Set output to ${root}_rate.fits

#Creating a time histogram
set enmin=$elo
set enmax=$ehi
echo "Creating lightcurve histogram in range ${enmin}-${enmax} eV..."
if ( "$expr" != none ) then
 echo evselect table=$table withhistogramset=true histogramcolumn=TIME expression="(PI in [${enmin}:${enmax}])&&(${expr})" histogramset=${root}lc_hist.fits histogrambinsize=$bin
 evselect table=$table withhistogramset=true histogramcolumn=TIME expression="(PI in [${enmin}:${enmax}])&&(${expr})" histogramset=${root}lc_hist.fits histogrambinsize=$bin
else
 echo evselect table=$table withhistogramset=true histogramcolumn=TIME expression="(PI in [${enmin}:${enmax}])" histogramset=${root}lc_hist.fits histogrambinsize=$bin
 evselect table=$table withhistogramset=true histogramcolumn=TIME expression="(PI in [${enmin}:${enmax}])" histogramset=${root}lc_hist.fits histogrambinsize=$bin
 endif
echo "  histogram written to ${root}lc_hist.fits\n"


if ( $max == "none" ) then
echo Please look at ${root}lc_hist.fits and select a maximum cutoff value from the table. Put this into the max argument as -m500 for example.
exit
endif

cp ${root}lc_hist.fits ${root}lc_hist_clean.fits

#finding mean of the data.

fselect "${root}lc_hist_clean.fits[HISTO]" "${root}lc_hist_clean.fits" "((COUNTS <= $max)&&(COUNTS >= 0))" clobber=yes

fstatistic "${root}lc_hist_clean.fits[HISTO]" counts rows="-" outfile=${root}stats.txt clobber=yes
set mean=`grep mean < ${root}stats.txt | awk '{print $8}'`
set sigma=`grep standard < ${root}stats.txt | awk '{print $9}'`
#echo "  mean = $mean counts, sigma = $sigma counts\n"
set maxcounts=`echo $sigma $mean | awk '{print $2+3*$1}'`
set mincounts=`echo $sigma $mean | awk '{print $2-3*$1}'`
set meandiff=1
set i=1

echo Running recursive cleaning algorithm
while ( $meandiff != 0 )
#filter histogram removing bins outside +/- 3sigma
fselect "${root}lc_hist_clean.fits[HISTO]" "${root}lc_hist_clean.fits" "((COUNTS <= $maxcounts)&&(COUNTS >= $mincounts))" clobber=yes

#find mean and 3sigma
fstatistic "${root}lc_hist_clean.fits[HISTO]" counts rows="-" outfile=${root}stats.txt clobber=yes
#echo "Histogram statistics written to ${root}stats.txt"
set mean2=`grep mean < ${root}stats.txt | awk '{print $8}'`
set sigma2=`grep standard < ${root}stats.txt | awk '{print $9}'`
#echo "  mean = $mean2 counts, sigma = $sigma2 counts\n"
set maxcounts=`echo $sigma2 $mean2 | awk '{print $2+3*$1}'`
#echo "  maxcounts = $maxcounts"
set mincounts=`echo $sigma2 $mean2 | awk '{print $2-3*$1}'`
set meandiff=`echo $mean $mean2 | awk '{print $1-$2}'`
echo "  step $i   reduction in mean rate = ${meandiff} counts per bin"
set mean=$mean2
@ i++
end
echo "  Cleaned histogram written to ${root}lc_hist_clean.fits\n"

#create GTI file
set maxrate=`echo $maxcounts $bin | awk '{print $1/$2}'`
echo "Creating GTI file for time bins with ($mincounts < counts < $maxcounts) (<$maxrate counts/s)..."
echo tabgtigen table=${root}lc_hist.fits gtiset=${root}gti.fits expression="(COUNTS.lt.$maxcounts)&&(COUNTS.gt.$mincounts)"
tabgtigen table=${root}lc_hist.fits gtiset=${root}gti.fits expression="(COUNTS.lt.$maxcounts)&&(COUNTS.gt.$mincounts)"
echo "  GTI written to ${root}gti.fits\n"

#apply GTI
echo "Applying GTI to events..."
if ( "$expr" != none ) then
echo evselect table=$table withfilteredset=yes filteredset=${root}clean_evt.fits filtertype=expression expression="(GTI(${root}gti.fits,TIME))&&(${expr})" destruct=yes keepfilteroutput=yes
evselect table=$table withfilteredset=yes filteredset=${root}clean_evt.fits filtertype=expression expression="(GTI(${root}gti.fits,TIME))&&(${expr})" destruct=yes keepfilteroutput=yes
else
echo evselect table=$table withfilteredset=yes filteredset=${root}clean_evt.fits filtertype=expression expression="GTI(${root}gti.fits,TIME)" destruct=yes keepfilteroutput=yes
evselect table=$table withfilteredset=yes filteredset=${root}clean_evt.fits filtertype=expression expression="GTI(${root}gti.fits,TIME)" destruct=yes keepfilteroutput=yes
endif

set countrate=`echo $mean2 $bin | awk '{print $1/$2}'`
set sigmarate=`echo $sigma2 $bin | awk '{print $1/$2}'`

echo "Cleaned events written to ${root}clean_evt.fits with mean rate $countrate counts/s and a standard deviation of $sigmarate counts/s in range ${enmin}-${enmax} eV\n"

#compare LIVETIME of original and cleaned events tables
set livetime=`fkeyprint "${table}[EVENTS]" LIVETIME | grep "Live time" | awk '{print $2}'`
set livetime_clean=`fkeyprint "${root}clean_evt.fits[EVENTS]" LIVETIME | grep "Live time" | awk '{print $2}'`
set time_diff=`echo $livetime $livetime_clean | awk '{print $1-$2}'`
echo "Old LIVETIME was ${livetime}s, xmmlight_clean removed ${time_diff}s, leaving a LIVETIME of ${livetime_clean}s\n"

#write chips script and plot
if ( -e ${root}chipsscript.txt ) then
 rm ${root}chipsscript.txt
endif
cat > ${root}chipsscript.txt <<EOF
plot ${root}lc_hist.fits x 2 y 1
symbol none
curve red
curve histo
plot ${root}lc_hist_clean.fits x 2 y 1
symbol none
curve simpleline
xlabel "Time (s)"
ylabel "Counts per bin"
title "Lightcurve of $root with bins of ${bin}s."
print postfile ${root}lc.eps
exit

EOF

if ( $ciao != no ) then
 chips ${root}chipsscript.txt > ${root}temp
 rm ${root}temp
endif

echo "Lightcurve plotted to ${root}lc.eps and chipsscript written to ${root}chipsscript.txt\n"

exit








