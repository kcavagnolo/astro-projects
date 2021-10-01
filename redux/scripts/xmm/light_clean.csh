#! /bin/tcsh 
#
# Script to do 3sigma cleaning on lightcurve and produce gti file
# and a cleaned events table. Cleans the lightcurve recursively
# until the mean count rate per bin is constant. 
#
# by default uses energy 10-15keV to produce lightcurve for cleaning.
#
# NOTE: Requires CIAO to be started to plot the lightcurve.
#
# version 1 - BJM 17/8/01
# version 2 - BJM 27/8/01 corrected tabgtigen use to generate correct GTI
# version 3 - BJM 20/12/01 now uses 10-15keV events for cleaning.
# version 3.1 - BJM 29/01/02 apply filter expression befor creating lightcurve
#               should only have minor affect.
# version 3.2 - BJM 12/02/02 added note about filter expression order.
# version 3.3 - BJM 9/03/02 fixed bug when no expression is used.

set version=3.3

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

if ( $#argv != 4 ) then
cat <<EOF

Use xmmlight_clean.csh table expression binsize root

    table       - your input table (e.g. pn_raw_evt.fits)
    expression  - the filter expression you wish to apply to the cleaned events, 
		  e.g '#XMMEA_EP&&(PATTERN <= 4)'. Enter "none" if required.
		  Note that because of a bug in evselect you should put the FLAG
                  filter before the PATTERN filter as above, otherwise you may
                  have problems later with arfgen.
    binsize     - binsize to use in seconds.
    root        - The root name for all output files.

EOF
exit
endif

set table=$1
set expr="$2"
set bin=$3
set root=$4

#check input files exist
set tabroot=`echo $table | cut -d ":" -f1`
if ( ! -e $tabroot ) then
 echo "Error: $tabroot does not exist"
 exit
endif

echo "Input was $0 $*\n\n"

#check if CIAO is installed
if ( ! $?ASCDS_INSTALL ) then
 set ciao=no
 echo "WARNING: CIAO not installed - lightcurve cleaning will work, but the results will not be plotted.\n"
else set ciao=yes
endif

#create time histogram
set enmin=10000
set enmax=15000
echo "Creating lightcurve histogram in range ${enmin}-${enmax} eV..."
if ( "$expr" != none ) then
 evselect table=$table withhistogramset=true histogramcolumn=TIME expression="(PI in [${enmin}:${enmax}])&&(${expr})" histogramset=${root}lc_hist.fits histogrambinsize=$bin
else 
 evselect table=$table withhistogramset=true histogramcolumn=TIME expression="(PI in [${enmin}:${enmax}])" histogramset=${root}lc_hist.fits histogrambinsize=$bin
endif
echo "  histogram written to ${root}lc_hist.fits\n"
cp ${root}lc_hist.fits ${root}lc_hist_clean.fits

#find mean and 3sigma
fstatistic "${root}lc_hist_clean.fits[HISTO]" counts rows="-" outfile=${root}stats.txt clobber=yes 
#echo "Histogram statistics written to ${root}stats.txt"
set mean=`grep mean < ${root}stats.txt | awk '{print $8}'`
set sigma=`grep standard < ${root}stats.txt | awk '{print $9}'`
#echo "  mean = $mean counts, sigma = $sigma counts\n"
set maxcounts=`echo $sigma $mean | awk '{print $2+3*$1}'`
set mincounts=`echo $sigma $mean | awk '{print $2-3*$1}'`
set meandiff=1

set i=1

echo "Recursively cleaning the data until the mean counts per bin is constant\n"
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


