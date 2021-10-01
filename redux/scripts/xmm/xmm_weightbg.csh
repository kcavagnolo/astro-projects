#!/bin/tcsh

#script to normalise the blank-sky background by a factor
#corresponding to a ratio of HE count rates/ooFOV count rates.
#These are determined in the script xmm_clean.csh



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

if ( $#argv < 2 ) then
cat <<EOF

Use xmm_weightbg.csh bgevents scale output

   bgevents     - the background file that you want scaling
		  this should already have a WEIGHT column 
		  generated from evigweight.   
   factor       - scaling factor. This is of the order 1. 
   
----Optional Argument-----------

   output       - name for output file. If left empty then
		  input file will be overwritten.


EOF
exit
endif

set evt=$1
set fact=$2

#check if third input file exists.

if ( $3 == "" ) then
 set out=$evt
 else
 set out=$3
endif

#check input files exist.

if ( ! -e $evt ) then
 echo "Error: $evt does not exist. Exiting\n"
 exit
endif


punlearn fcalc

cp $evt tempEv01

fcalc tempEv01 tempEv02 WEIGHT "WEIGHT * $fact " clobber=yes

cp tempEv02 $out

rm tempEv*







