#!/bin/tcsh

#       This is a complete rewrite of xmm_clean.csh.
#       As the previous script is useful, I am not going to 
#       overwrite it. 

#       The main difference is that the old script used a 
#       3 sigma clip on the data, calculated iteratively from ftools.
#       I felt that this script was not cleaning my data sufficiently
#       and this script addresses this problem.  

#       This will call clean.f so make sure that you have that file
#       handy. This will use the various FORTRAN libraries
#       so make sure that you have access to those. 

#       This script essentially creates a count rate histogram (which 
#       will be visible as one of the outputs) and fits a Gaussian
#       to the data. This Gaussian is used to evaluated upper and lower 
#       limits on the count rate range of the input dataset. 

#       These limits will be reinput back into ftools to extract a 
#       cleaned lightcurve. Before and after EXPOSURE values
#       will be output to the screen, and a cleaned/noncleaned
#       lightcurve will be plotted using Chips. 

#       Also uses xmmstring.csh

#       Data must be skycasted (if it is a background dataset)

#       Adding in an extra variable: bg? If the file is a blank-sky
#       background file, the data will be cropped prior to 


set version=1.0   #RFT 02/11/2007 


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

if ( $#argv < 5 ) then
cat <<EOF

Use xmm_lcclean.csh events expression binsize [-eelow] [-Eehi] [-mmax]

arguments in [] are optional, and can be in any order

    events      - your input table (e.g. pn_raw_evt.fits)
    expression  - the filter expression you wish to apply to the cleaned events,
		  e.g '#XMMEA_EP&&(PATTERN <= 4)'. Enter "none" if required.
		  Note that because of a bug in evselect you should put the FLAG
                  filter before the PATTERN filter as above, otherwise you may
                  have problems later with arfgen. Not had any issues recently. 
    binsize     - binsize to use in seconds.
    bg          - Is the input file a background dataset? (Y/y) 
    root        - root filename

             ------- Optional Arguments ------

    -eelo       - "elo" is a lower energy bound (eV) default=10 keV (mos),
                   12 keV (pn) e.g. -e10000 (note energies in eV)
    -Eehi       - "ehi" is an upper energy bound (eV) default=12 keV (mos) 
                   14 keV (pn) e.g. -E15000 (again energies in eV)            
 
    -rreg       - region file in which to extract the lightcurve. 
                  Must be in CIAO format. Uses xmmstring.csh. e.g. -rds9.reg

    -mN         - manually apply an upper limit to the data cut. This is
	          count rate, not counts or counts per bin. 

EOF
exit
endif

echo "Input was xmm_clean.csh" $1 $2 $3 $4 $5"\n"

set events=$1
set expr="$2"
set bin=$3
set bg=$4
set root=$5

#root name not comes directly from the inst value


#Getting INST:
if ( -e tmpdmp1 ) then
 rm -f tmpdmp1
endif
fdump $events tmpdmp1 TIME - prdata=no showcol=no showunit=no showrow=no clobber=yes
set instr=`grep INSTRUME < tmpdmp1 | head -1 | awk '{print $2}'`
set instr=`echo $instr | tr -d "'"`
rm -f tmpdmp1
echo 'Instrument is '$instr "\n"
if ( $instr == EPN) then
  set elo=12000
  set ehi=14000
 else
  set elo=10000
  set ehi=12000
endif

set regflag=false
set maxflag=false

set i=6
while ( $i <= $#argv )
 set c=`echo $argv[${i}] | cut -c1`
 if ( $c != - ) then
  echo "ERROR: argument $i doesn't begin with a -\n"
  exit
 endif
 set type=`echo $argv[${i}] | cut -c2`
 if (( $type != e )&&( $type != E )&&( $type != m )&&( $type != s )&&( $type != r )) then
  echo "ERROR: argument $i is not of type e, E, m, s, b or r.\n"
  echo $type $argv[${i}]
  exit
 endif

 set arg=`echo $argv[${i}] | tail -c +3`
 if ( $type == e ) then
  set elo=$arg
 else if ( $type == E ) then
  set ehi=$arg
 else if ( $type == r ) then
  set reg=$arg
  set regflag=true
 else if ( $type == m ) then
  set max=$arg
  set maxflag=true
 endif
@ i++
end



#check if CIAO is installed
if ( ! $?ASCDS_INSTALL ) then
 set ciao=no
 echo "WARNING: CIAO not installed - lightcurve cleaning will work, but the results will not be plotted.\n"
else set ciao=yes
endif

#Check to see if region file exists + in CIAO format.
if ( $regflag == true ) then
  if ( ! -e $reg ) then
   echo "Region file $reg does not exist. Exiting...\n"  
   exit
  endif
  set ciaotest=`grep CIAO < $reg | wc -l`
  if ( $ciaotest != 1 ) then
   echo "  ERROR - BG REGION FILE DOES NOT APPEAR TO BE IN CIAO FORMAT\n\n"
   exit
  endif 
  set sourcefilter=`/data1/rft/rftscripts/xmmstring.csh $reg | grep expression | awk '{print $4}'`
endif

#Identify the background parameter.
 set bgflag=0
if (( $bg == "y" )||( $bg == "Y" )) then 
 set bgflag=1
endif


echo "Creating a rate curve in range ${elo}-${ehi} keV.\n "

#Create based on expression parameter
set bin2=$bin

if ( $regflag == true ) then

 if ( "$expr" != none ) then
  evselect -w 0 -V 0 table=$events filteredset=tempEv01 expression="(PI in [${elo}:${ehi}])&&(${expr})" updateexposure=N writedss=Y withfilteredset=T destruct=Y keepfilteroutput=T
 evselect -V 0 table=tempEv01 withrateset=Y timebinsize=${bin2} rateset=rate.fits maketimecolumn=Y makeratecolumn=Y expression=$sourcefilter
  else 
  evselect -w 0 -V 0 table=$events filteredset=tempEv01 expression="(PI in [${elo}:${ehi}])&&(${expr})" updateexposure=N writedss=Y withfilteredset=T destruct=Y keepfilteroutput=T
 evselect -V 0 table=tempEv01 withrateset=Y timebinsize=${bin2} rateset=rate.fits maketimecolumn=Y makeratecolumn=Y expression=$sourcefilter
 endif 
 
else
 if ( "$expr" != none ) then
 evselect -V 0 table=$events withrateset=Y timebinsize=${bin2} rateset=rate.fits maketimecolumn=Y makeratecolumn=Y expression="(PI in [${elo}:${ehi}])&&(${expr})"
  else 
 evselect -V 0 table=$events withrateset=Y timebinsize=${bin2} rateset=rate.fits maketimecolumn=Y makeratecolumn=Y expression="(PI in [${elo}:${ehi}])"
 endif

endif

# Create a histogram based on the same expression parameter

if ( $regflag == true ) then
 if ( "$expr" != none ) then
  evselect -w 0 -V 0 table=$events filteredset=tempEv01 expression="(PI in [${elo}:${ehi}])&&(${expr})" updateexposure=N writedss=Y withfilteredset=T destruct=Y keepfilteroutput=T
  evselect table=tempEv01 withhistogramset=true histogramcolumn=TIME expression=$sourcefilter histogramset=hist.fits histogrambinsize=$bin
 else
  evselect -w 0 -V 0 table=$events filteredset=tempEv01 expression="(PI in [${elo}:${ehi}])" updateexposure=N writedss=Y withfilteredset=T destruct=Y keepfilteroutput=T
 evselect table=tempEv01 withhistogramset=true histogramcolumn=TIME expression=$sourcefilter histogramset=hist.fits histogrambinsize=$bin 
 endif
else
 if ( "$expr" != none ) then
 evselect table=$events withhistogramset=true histogramcolumn=TIME expression="(PI in [${elo}:${ehi}])&&(${expr})" histogramset=hist.fits histogrambinsize=$bin
 else
  evselect table=$events withhistogramset=true histogramcolumn=TIME expression="(PI in [${elo}:${ehi}])" histogramset=hist.fits histogrambinsize=$bin 
 endif

endif

if ( -e bin.txt ) then
 rm bin.txt
endif
touch bin.txt

printf $bin >> bin.txt 

if ( -e minmax.txt ) then
 rm minmax.txt
endif # This is the output from the cleaning script. Should always be deleted before running.


# bin.txt and rate.fits are hardwired inputs into clean.f
#Everything else should run OK. 

#For blank-sky background files, the data will be clipped first. 
if ( $bgflag == 1 ) then
 if ( $instr == EMOS1 ) then
    if ( $bin == 100 ) then
 fselect "rate.fits[RATE]" "rate.fits" "((RATE <= 0.2)&&(RATE >= 0.01))" clobber=yes
    else
 fselect "rate.fits[RATE]" "rate.fits" "(RATE >= 0.01)" clobber=yes    
    endif
 endif
 if ( $instr == EMOS2 ) then
    if ( $bin == 100 ) then
 fselect "rate.fits[RATE]" "rate.fits" "((RATE <= 0.3)&&(RATE >= 0.01))" clobber=yes
    else
 fselect "rate.fits[RATE]" "rate.fits" "(RATE >= 0.01)" clobber=yes    
    endif
 endif
 if ( $instr == EPN ) then
    if ( $bin == 100 ) then
 fselect "rate.fits[RATE]" "rate.fits" "((RATE <= 0.5)&&(RATE >= 0.01))" clobber=yes
    else
 fselect "rate.fits[RATE]" "rate.fits" "(RATE >= 0.01)" clobber=yes    
    endif
 endif
endif

# if maxflag==true, then clip the data accordong to that parameter
if ( $maxflag == "true" ) then
 fselect "rate.fits[RATE]" "rate.fits" "(RATE <= $max)" clobber=yes
endif


f77 -o clean -O clean.f -lcfitsio -lm -lnsl -L /usr/local/pgplot -lpgplot -L /usr/X11R6/lib -lX11

./clean

#Now I need to do the following: 
#3) use the ranges to define a GTI file (from xmm_clean.csh)
#4) plot an overlay of the new data over the old one. 
#5) copy the filenames to represent the instrument. 

#1) 

# Sort out the plotting. 
#Only useful output is the count rate histogram. All other plots should be set to NULL.
# Output is called histo.ps

rm hist.qdp # dont actually need the qdp file output from this script. 
rm bin.txt 

#read in the output file (minmax.txt) 
set minrate=`cat minmax.txt | awk '{print $1}'`
set maxrate=`cat minmax.txt | awk '{print $2}'`
set centre=`cat minmax.txt | awk '{print $3}'`
echo "centre for $root is $centre c/s "
set minrate=-0.1 # only going to filter on the upper count rate. 

#if ( $bin == 100 ) then
# if ( $instr == EMOS1 ) then
#  set maxrate=0.24
# endif
# if ( $instr == EMOS2 ) then
#  set maxrate=0.20
# endif
# if ( $instr == EPN ) then
#  set maxrate=0.60
# endif
#endif

if ( $maxflag == "true" ) then
 set tmp1=`echo $max | awk '{print $1*10000}' | cut -d "." -f1`
 set tmp2=`echo $maxrate | awk '{print $1*10000}' | cut -d "." -f1`
 if ( $tmp1 <= $tmp2 ) then
    echo "here1"
  set maxrate=$max
 endif
endif

echo "Creating GTI file for time bins with count rate ($minrate < count rate <$maxrate)"
tabgtigen table=rate.fits gtiset=rate_gti.fits expression="(RATE.lt.$maxrate)&&(RATE.gt.$minrate)"

if ( "$expr" != none ) then
 evselect table=$events withfilteredset=yes filteredset=${root}_clean_evt.fits filtertype=expression expression="(GTI(rate_gti.fits,TIME))&&(${expr})" destruct=yes keepfilteroutput=yes
else
 evselect table=$events withfilteredset=yes filteredset=${root}_clean_evt.fits filtertype=expression expression="GTI(rate_gti.fits,TIME)" destruct=yes keepfilteroutput=yes
endif

#Comparing livetime of cleaned events table
set livetime=`fkeyprint "${events}[EVENTS]" LIVETIME | grep "Live time" | awk '{print $2}'`
set livetime_clean=`fkeyprint "${root}_clean_evt.fits[EVENTS]" LIVETIME | grep "Live time" | awk '{print $2}'`
set time_diff=`echo $livetime $livetime_clean | awk '{print $1-$2}'`
echo "Old LIVETIME was ${livetime}s; removed ${time_diff}s, leaving a LIVETIME of ${livetime_clean}s.\n"

#Need to do this in histograms. Got the original histogram (hist.fits)
cp hist.fits hist_clean.fits
set max=`echo $maxrate $bin | awk '{print $1*$2}'`
set min=`echo $minrate $bin | awk '{print $1*$2}'`
fselect "hist_clean.fits[HISTO]" "hist_clean.fits" "((COUNTS <= $max)&&(COUNTS >=$min ))" clobber=yes

#writing chips script and plot
if ( -e ${root}_chipsscript.txt ) then
  rm ${root}_chipsscript.txt
endif

cat > ${root}_chipsscript.txt <<EOF
plot hist.fits x 2 y 1
symbol none
curve red
curve histo
plot hist_clean.fits x 2 y 1
symbol none
curve simpleline
xlabel "Time (s)"
ylabel "Counts per bin"
title "Lightcurve of ${instr} with bins of ${bin}"
print postfile ${root}_lc.eps 
exit

EOF

if ( $ciao != no ) then
 chips ${root}_chipsscript.txt > ${root}temp
 rm ${root}temp
endif

echo "Lightcurve plotted to ${root}_lc.eps" 
#rm ${root}_chipsscript.txt hist.fits hist_clean.fits rate.fits rate_gti.fits  
rm minmax.txt
mv histo.eps ${root}_crhist.eps


