#! /bin/tcsh
#
#	Script to setup ardlib.par as I am sick of running it all the time
#	Still needs to be tested

nice +19

set version=1.0
#	1.0	RFT	10/12/04	Simple script to setup my ardlib.par file.

cat <<EOF

--------------------`echo $0:t | tr '[a-z]' '[A-Z]'` version $version `date +%x`--------------------

EOF

if ( $#argv != 2 ) then
cat <<EOF

Use ardlib.csh events bpix

    events	- the events data for your observation
    bpix	- the bad pixel file for your observation. This should either
    		  be the full extension
		  e.g. /data1/rft/ngcXXX/chandra/765/primary/acisxxxxbpix1.fits
		  or you should make sure that the badpix file is in your current
		  directory when you run scripts that need it e.g. psextract

EOF
exit
endif

set events=$1
set bpix=$2

#check input files exist
if ( ! -e $events ) then
 echo "Error: $events does not exist"
 exit
endif

if ( ! -e $bpix ) then
 echo "Error: $bpix does not exist"
 exit
endif

set tmptmp=temp.txt
dmkeypar $events DETNAM echo+ >! $tmptmp
set numchar=`wc -L $tmptmp | cut -c6,7`
set str=`head -1 $tmptmp | cut -d"-" -f1 | wc -L `
set num=`echo $str | awk '{print $1+2}' `

set i=$num
while ( $i <= $numchar )
set j=`head -1 $tmptmp | cut -c$i `
#echo $i $j
pset ardlib AXAF_ACIS${j}_BADPIX_FILE = "${bpix}[BADPIX${j}]"
@ i++
end



#foreach d (0 1 2 3 6)
#pset ardlib AXAF_ACIS${d}_BADPIX_FILE = "/data1/rft/legacy/0958_1103/chandra/3205/primary/acisf03205_001N002_bpix1.fits[BADPIX${d}]"
#end





















