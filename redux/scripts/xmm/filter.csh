#! /bin/tcsh 
#
# Script to act as front end for evselect.
#
# version 1 - BJM 17/8/01

set version=1

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

--------------------xmmfilter.csh version $version---------------------

EOF

if ( $#argv != 5 ) then
cat <<EOF

Use xmmfilter.csh table type outfile expression binsize

    table       - your input table with extension (e.g. j0046PN1evt.FIT:EVENTS)
    type        - the type of output to produce, must be 
                  events, image, spectrum, lightcurve
    outfile      - name of output file
    expression  - the filter expression you wish to apply, 
		  (e.g '(FLAG == 0)&&(PATTERN == 0)'
		  Enter "none" if required
    binsize     - binsize to use in lightcurve (s), image (pixels), 
                  spectrum (eV), histogram().
                  Enter "none" if required.

EOF
exit
endif

set table=$1
set type=$2
set out=$3
set expr="$4"
set bin=$5

#check input files exist
set tabroot=`echo $table | cut -d ":" -f1`
if ( ! -e $tabroot ) then
 echo "Error: $tabroot does not exist"
 exit
endif
if (( $type != events )&&( $type != image )&&( $type != spectrum )&&( $type != lightcurve )) then
 echo "$type must be events, image, spectrum, or lightcurve"
endif

echo "Input was $0 $*\n\n"

#sort out expression
if ( "$expr" == none ) then
 set expr=
endif
if ( $bin == none ) then
 set bin=1
endif

#run evselect
if ( $type == events ) then
echo "Creating new events file...\n" 
echo evselect table=$table withfilteredset=yes filteredset=$out filtertype=expression expression="$expr" destruct=yes keepfilteroutput=yes writedss=yes
 evselect table=$table withfilteredset=yes filteredset=$out filtertype=expression expression="$expr" destruct=yes keepfilteroutput=yes writedss=yes
endif

if ( $type == image ) then
 echo "Creating image, binning=$bin pix..."
 echo evselect table=$table withimageset=yes imageset=$out xcolumn=X ycolumn=Y imagebinning=binSize ximagebinsize=$bin yimagebinsize=$bin expression="$expr" writedss=yes withimagedatatype=yes imagedatatype=Real32
 evselect table=$table withimageset=yes imageset=$out xcolumn=X ycolumn=Y imagebinning=binSize ximagebinsize=$bin yimagebinsize=$bin expression="$expr" writedss=yes withimagedatatype=yes imagedatatype=Real32
endif

if ( $type == lightcurve ) then
 echo "Creating lightcurve, binning=$bin s..."
 echo evselect table=$table withrateset=yes rateset=$out maketimecolumn=yes timecolumn=TIME timebinsize=$bin
 evselect table=$table withrateset=yes rateset=$out maketimecolumn=yes timecolumn=TIME timebinsize=$bin
endif

if ( $type == spectrum ) then
 echo "Extracting PI spectrum, binning=$bin eV..."
 echo evselect table=$table withspectrumset=yes spectrumset=$out energycolumn=PI spectralbinsize=$bin specchannelmin=0 specchannelmax=20479 expression="$expr" writedss=yes
 evselect table=$table withspectrumset=yes spectrumset=$out energycolumn=PI spectralbinsize=$bin specchannelmin=0 specchannelmax=20479 expression="$expr" writedss=yes
endif

#find number of events kept in filter for image and events filtering
set num=arse
if ( $type == image ) then
 set num=`fimgstat $out INDEF INDEF | grep sum | awk '{print $8}'`
endif
if ( $type == events ) then
 set num=`fstatistic "${out}[EVENTS]" PI - | grep number | awk '{print $9}'`
endif

echo "\nOutput $type written to $out\n"

if ( $num != arse ) then
 echo "There are $num events in the filter.\n"
endif

exit

