#! /bin/tcsh
#
#	Script to read in data files. Use as a template for future scripts.
#
#
#

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

if (( $#argv <= 7 )||( $#argv >=9 )) then
cat <<EOF

Use readdata.csh events bgevents asol sources bkg_sources circ rad

    events	- the events data for your observation

    bgevents	- a blank sky background data file

    asol	- aspect solution file (can be a stack)

    sources	- CIAO region file of all the regions in your
    		  events data set. Each region should begin on a
		  new line.

    bkg_sources	- Region file of annuli of data around the
    		  each source. This is generated from mkBkSub.pl
		  script and should have the format:
		  ellipse(3880.3,3819.5,4.591836,2.841662,52.210387)
		  -ellipse(3880.3,3819.5,2.295918,1.420831,52.210387)

    circ	- possible redundant varible. This asks for a CIAO
    		  region file around your source. This is used to calculate
		  the peak energy of the energy histogram.

    rad		- This is the itnum parameter used in the smoothing algorithm
    		  See "msmooth -help" for more details.

EOF
exit
endif
echo "end"
exit


set events=$1
set back=$2
set asol=$3
set sources=$4
set bk_sub=$5
set circ=$6
set rad=$7

#check input files exist
if ( ! -e $events ) then
 echo "Error: $events does not exist"
 exit
endif

if ( ! -e $back ) then
 echo "Error: $back does not exist"
 exit
endif

if ( ! -e $asol ) then
 echo "Error: $asol does not exist"
 exit
endif

if ( ! -e $sources ) then
 echo "Error: $sources does not exist"
 exit
endif

if ( ! -e $bk_sub ) then
 echo "Error: $bk_sub does not exist"
 exit
endif

if ( ! -e $circ ) then
 echo "Error: $circ does not exist"
 exit
endif

#Note that i shouldnt do the same for $7 as it is a number and the file
#'2' will not be found.


#check regions are in ciao format
set rtype=`head -1 $sources | awk '{print $5}'`
if ( $rtype != CIAO ) then
 echo "  ERROR - REGION FILE DOES NOT APPEAR TO BE IN CIAO FORMAT\n\n"
 exit
endif

















