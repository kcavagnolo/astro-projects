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
# from first principles. This is a modified version of xmm_lc.csh - 
# the clip is going to be performed using count rate not counts.
#
# The out of FOV ratio will be determined using singles and doubles for 
# PN and singles,doubles, triples and quads for MOS1 and MOS2. The energy  
# range will be set to be 0.3 to 12 keV. If you want to alter any of these 
# parameters then use AMR's compareoutofFOV script.
#
#
#
# version 1.0 -	RFT 15/02/04
# version 1.1 - RFT 22/02/04 adding background to cleaning. Option added
#                            to skycast background. No need to have the 
#                            skycast program. Compares out of FOV counts
#                            using in built script. Uses fixed values to 
#                            compare. 
# version 1.2 - RFT 07/03/05 changing the gti file for the background to 
#                            only account for the upper count rates. If 
#                            you have an energetic source, for example,
#                            the count rates are going to be higher than
#                            the background by a significant amount. 
#                            Therefore, using a clip on the count rate 
#                            values on the lower half of the background 
#                            ends up cutting all the background out. 
#                            (Or at least most of it).
# version 1.3 - RFT 10/03/05 Not using ONTIME keyword for images anymore. 
#                            The ONTIME keyword should be used when 
#                            determining values for CLOSED data sets.
# version 1.4 - RFT 29/07/05 Going to have a specific region in which the
#                            lightcurves are going to be extracted. The
#                            plan is to have a second cleaning run which 
#                            generates a second broad band lightcurve from
#                            a FOV region at high radius to see if any 
#                            further cleaning is needed from the lightcurve
#                            in lower energy bands. The background will 
#                            then be clipped using absolute rates as we have
#                            discovered that the shape of the bg spectrum is 
#                            different at different count rates. The input 
#                            region needs to be point source extracted so a 
#                            preliminary clean needs to be done at high   
#                            energies first such that edetect_chain can be 
#                            run. Once the point sourced have been identified,
#                            only then can the region file be used in this 
#                            script
# version 1.5 - RFT 02/11/05 Set up an option to allow no background to 
#                            be input. Also fixed a bug so blank background
#                            files can now be three sigma clipped


# need option to allow previously skycasted data to calc oofov counts


set version=1.5


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

Use xmmlight_clean.csh table expression binsize root [-eelow] [-Eehi] [-mmax]

arguments in [] are optional, and can be in any order

    events      - your input table (e.g. pn_raw_evt.fits)
    expression  - the filter expression you wish to apply to the cleaned events,
		  e.g '#XMMEA_EP&&(PATTERN <= 4)'. Enter "none" if required.
		  Note that because of a bug in evselect you should put the FLAG
                  filter before the PATTERN filter as above, otherwise you may
                  have problems later with arfgen.
    binsize     - binsize to use in seconds.
    bgevents    - background events file. set to 'none' if not required. 
    root        - The root name for all output files.

             ------- Semi Optional Arguments ----(can be input in any order)

    -mmax	- "max" is an upper limit on the number of counts per bin considered
		  by the cleaning algorithm. Useful with noisy lightcurves, to
                  exclude big flares, with e.g. -m500 if 50s bins are used. This is
		  required to run the script completely.

             ------- Optional Arguments ------

    -eelo       - "elo" is a lower energy bound (eV) default=10 keV (mos),
                   12 keV (pn) e.g. -e10000 (note energies in eV)
    -Eehi       - "ehi" is an upper energy bound (eV) default=12 keV (mos) 
                   14 keV (pn) e.g. -E15000 (again energies in eV)               
    -sY         - skycast background events data? default=no e.g. -sY or -sy
    -rreg       - region file in which to extract the lightcurve. 
                  Must be in CIAO format. Uses xmmstring.csh. e.g. -rds9.reg

EOF
exit
endif

echo "Input was xmm_clean.csh" $1 $2 $3 $4 $5 "\n"

set events=$1
set expr="$2"
set bin=$3
set bgevents=$4
set root=$5

#check input files exist
set tabroot=`echo $events | cut -d ":" -f1`
if ( ! -e $tabroot ) then
 echo "Error: $tabroot does not exist"
 exit
endif
if ( $bgevents != "none" ) then
 if ( ! -e $bgevents ) then
   echo "Error: $bgevents does not exist"
   exit
 endif
endif

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
set max="none"
set regflag=false
set sky=N

#sort out optional arguments
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

 set arg=`echo $argv[${i}] | tail +3c`


 if ( $type == e ) then
  set elo=$arg
 else if ( $type == E ) then
  set ehi=$arg
 else if ( $type == m ) then
  set max=$arg
 else if ( $type == s ) then
  set sky=$arg
 else if ( $type == r ) then
  set reg=$arg
  set regflag=true
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

#Creating a rate curve

#Creating a time histogram

if ( $regflag == true ) then
 echo "Creating lightcurve histogram in range ${elo}-${ehi} eV..."
 if ( "$expr" != none ) then
  evselect -w 0 -V 0 table=$events filteredset=tempEv01 expression="(PI in [${elo}:${ehi}])&&(${expr})" updateexposure=N writedss=Y withfilteredset=T destruct=Y keepfilteroutput=T
  evselect table=tempEv01 withhistogramset=true histogramcolumn=TIME expression=$sourcefilter histogramset=${root}_hist.fits histogrambinsize=$bin
 else
  evselect -w 0 -V 0 table=$events filteredset=tempEv01 expression="(PI in [${elo}:${ehi}])" updateexposure=N writedss=Y withfilteredset=T destruct=Y keepfilteroutput=T
 evselect table=tempEv01 withhistogramset=true histogramcolumn=TIME expression=$sourcefilter histogramset=${root}_hist.fits histogrambinsize=$bin 

 endif

else

 echo "Creating lightcurve histogram in range ${elo}-${ehi} eV..."
 cp $events tempEv01
 if ( "$expr" != none ) then
    #echo "evselect table=$events withhistogramset=true histogramcolumn=TIME expression="(PI in [${elo}:${ehi}])&&(${expr})" histogramset=${root}_hist.fits histogrambinsize=$bin "
    echo here1
 evselect table=$events withhistogramset=true histogramcolumn=TIME expression="(PI in [${elo}:${ehi}])&&(${expr})" histogramset=${root}_hist.fits histogrambinsize=$bin
 else
  evselect table=$events withhistogramset=true histogramcolumn=TIME expression="(PI in [${elo}:${ehi}])" histogramset=${root}_hist.fits histogrambinsize=$bin 
 endif

endif

echo "  histogram written to ${root}_hist.fits\n"

if ( $max == "none" ) then
echo Please look at ${root}_hist.fits and select a maximum cutoff value from the table. Put this into the max argument as -m500 for example.
exit
endif

cp ${root}_hist.fits ${root}_hist_clean.fits


echo "Creating a rate curve in range ${elo}-${ehi} keV.\n "

#Create based on expression parameter

if ( $regflag == true ) then

 if ( "$expr" != none ) then
 evselect -V 0 table=tempEv01 withrateset=Y timebinsize=${bin} rateset=${root}_rate.fits maketimecolumn=Y makeratecolumn=Y expression=$sourcefilter
  else 
 evselect -V 0 table=tempEv01 withrateset=Y timebinsize=${bin} rateset=${root}_rate.fits maketimecolumn=Y makeratecolumn=Y expression=$sourcefilter
 endif 
 
else
 if ( "$expr" != none ) then
 evselect -V 0 table=$events withrateset=Y timebinsize=${bin} rateset=${root}_rate.fits maketimecolumn=Y makeratecolumn=Y expression="(PI in [${elo}:${ehi}])&&(${expr})"
  else 
 evselect -V 0 table=$events withrateset=Y timebinsize=${bin} rateset=${root}_rate.fits maketimecolumn=Y makeratecolumn=Y expression="(PI in [${elo}:${ehi}])"
 endif

endif

#finding mean of the data.

fselect "${root}_hist_clean.fits[HISTO]" "${root}_hist_clean.fits" "((COUNTS <= $max)&&(COUNTS >= 0.01))" clobber=yes

fstatistic "${root}_hist_clean.fits[HISTO]" counts rows="-" outfile=${root}_stats.txt clobber=yes
set mean=`grep mean < ${root}_stats.txt | awk '{print $8}'`
set sigma=`grep standard < ${root}_stats.txt | awk '{print $9}'`
#echo "  mean = $mean counts, sigma = $sigma counts\n"
set maxcounts=`echo $sigma $mean | awk '{print $2+3*$1}'`
set mincounts=`echo $sigma $mean | awk '{print $2-3*$1}'`
set meandiff=1
set i=1

echo Running recursive cleaning algorithm
while ( $meandiff != 0 )
#filter histogram removing bins outside +/- 3sigma
fselect "${root}_hist_clean.fits[HISTO]" "${root}_hist_clean.fits" "((COUNTS <= $maxcounts)&&(COUNTS >= $mincounts))" clobber=yes

#find mean and 3sigma
fstatistic "${root}_hist_clean.fits[HISTO]" counts rows="-" outfile=${root}_stats.txt clobber=yes
set mean2=`grep mean < ${root}_stats.txt | awk '{print $8}'`
set sigma2=`grep standard < ${root}_stats.txt | awk '{print $9}'`
set maxcounts=`echo $sigma2 $mean2 | awk '{print $2+3*$1}'`
set mincounts=`echo $sigma2 $mean2 | awk '{print $2-3*$1}'`
set meandiff=`echo $mean $mean2 | awk '{print $1-$2}'`
echo "  step $i   reduction in mean rate = ${meandiff} counts per bin"
set mean=$mean2
#echo "mean2 maxcounts, mincounts= $mean2 $maxcounts $mincounts" 
@ i++
end
echo "  Cleaned histogram written to ${root}lc_hist_clean.fits\n"
set maxrate=`echo $maxcounts $bin | awk '{print $1/$2}'`
set meanrate=`echo $mean $bin | awk '{print $1/$2}'`
set minrate=`echo $mincounts $bin | awk '{print $1/$2}'`
#echo "mean rate, max rate, min rate = $meanrate $maxrate $minrate" 
#echo "mean counts, max counts, min counts = $mean $maxcounts $mincounts" 

#Creating GTI file

#echo "Creating GTI file for time bins with counts ($mincounts < counts < $maxcounts )"
#tabgtigen table=${root}_hist.fits gtiset=${root}_hist_gti.fits expression="(COUNTS.lt.$maxcounts)&&(COUNTS.gt.$mincounts)"

echo "Creating GTI file for time bins with count rate ($minrate < count rate <$maxrate)"
tabgtigen table=${root}_rate.fits gtiset=${root}_rate_gti.fits expression="(RATE.lt.$maxrate)&&(RATE.gt.$minrate)"

#Applying GTI to events data.
echo "Applying GTI to events..."

if ( "$expr" != none ) then
 evselect table=$events withfilteredset=yes filteredset=${root}_clean_evt.fits filtertype=expression expression="(GTI(${root}_rate_gti.fits,TIME))&&(${expr})" destruct=yes keepfilteroutput=yes
else
 evselect table=$events withfilteredset=yes filteredset=${root}_clean_evt.fits filtertype=expression expression="GTI(${root}_rate_gti.fits,TIME)" destruct=yes keepfilteroutput=yes
endif

#Comparing livetime of cleaned events table
set livetime=`fkeyprint "${events}[EVENTS]" LIVETIME | grep "Live time" | awk '{print $2}'`
set livetime_clean=`fkeyprint "${root}_clean_evt.fits[EVENTS]" LIVETIME | grep "Live time" | awk '{print $2}'`
set time_diff=`echo $livetime $livetime_clean | awk '{print $1-$2}'`
echo "Old LIVETIME was ${livetime}s; removed ${time_diff}s, leaving a LIVETIME of ${livetime_clean}s.\n"

#writing chips script and plot
if ( -e ${root}_chipsscript.txt ) then
  rm ${root}_chipsscript.txt
endif

cat > ${root}_chipsscript.txt <<EOF
plot ${root}_hist.fits x 2 y 1
symbol none
curve red
curve histo
plot ${root}_hist_clean.fits x 2 y 1
symbol none
curve simpleline
xlabel "Time (s)"
ylabel "Counts per bin"
title "Lightcurve of ${root} with bins of ${bin}"
print postfile ${root}_lc.eps 
exit

EOF

if ( $ciao != no ) then
 chips ${root}_chipsscript.txt > ${root}temp
 rm ${root}temp
endif

echo "Lightcurve plotted to ${root}_lc.eps and chipsscript written to ${root}_chipsscript.txt.\n"


#Now to deal with the background
#Skycasting background

if ( $bgevents == "none") then
 echo  "No background file selected. Exiting...\n"
 exit
endif

set skyevents=${root}_sky.fits
if ( -e $skyevents ) then
 rm -f $skyevents
endif
if (( $sky == Y)||( $sky == y )) then
echo "skycasting background data..."
 echo "Getting attitude information from template file $events..."
 rm -f temp0001
 fdump $events+0 temp0001 1 1-1
 set ra_pnt=`grep RA_PNT < temp0001 | head -1 | awk '{print $3}'`
 set dec_pnt=`grep DEC_PNT < temp0001 | head -1 | awk '{print $3}'`
 set pa_pnt=`grep PA_PNT < temp0001 | head -1 | awk '{print $3}'`
 set ranom=`grep REFXCRVL < temp0001 | head -1 | awk '{print $2}'`
 set decnom=`grep REFYCRVL < temp0001 | head -1 | awk '{print $2}'`
 rm -f temp0001

#Using PNT attitude - do attcalc
 echo "Calculating sky coordinates..."
 cp $bgevents $skyevents
 attcalc -w 0 -V 0 eventset=$skyevents attitudelabel=fixed fixedra=$ra_pnt fixeddec=$dec_pnt fixedposangle=$pa_pnt withatthkset=N refpointlabel=user nominalra=$ranom nominaldec=$decnom
 echo "Skycast Completed."

echo "Skycasted file written to $skyevents \n"
endif

#applying the same cleaning limits to the background that
#were applied to the events file. 
# If this is the second run through the cleaning i.e. 
# $regflag==true, then we are going to use the mean c/s
# +/- 1 sigma to generate our bs bg file. 


echo "Creating a time and rate histogram in range ${elo}-${ehi} keV for background data...\n "

if ( -e $skyevents ) then
 set bginput=$skyevents
 else
 set bginput=$bgevents
endif

#Create based on expression parameter
 
if ( $regflag == true ) then 

 if ( "$expr" != none ) then
  evselect -w 0 -V 0 table=${bginput} filteredset=tempEv02 expression="(PI in [${elo}:${ehi}])&&(${expr})" updateexposure=N writedss=Y withfilteredset=T destruct=Y keepfilteroutput=T
   evselect -V 0 table=tempEv02 withhistogramset=T histogramcolumn=TIME expression=$sourcefilter histogramset=${root}_bg_hist.fits histogrambinsize=${bin}
   evselect table=tempEv02 withrateset=Y timebinsize=$bin rateset=${root}_bg_rate.fits maketimecolumn=Y makeratecolumn=Y expression=$sourcefilter
 else
  evselect -w 0 -V 0 table=${bginput} filteredset=tempEv02 expression="(PI in [${elo}:${ehi}])" updateexposure=N writedss=Y withfilteredset=T destruct=Y keepfilteroutput=T
   evselect -V 0 table=tempEv02 withhistogramset=T histogramcolumn=TIME expression=$sourcefilter histogramset=${root}_bg_hist.fits histogrambinsize=${bin}
   evselect table=tempEv02 withrateset=Y timebinsize=$bin rateset=${root}_bg_rate.fits maketimecolumn=Y makeratecolumn=Y expression=$sourcefilter

 endif #for expr==none

else # for regflag == true

 if ( "$expr" != none ) then 
  evselect table=${bginput} withhistogramset=T histogramcolumn=TIME expression="(PI in [${elo}:${ehi}])&&(${expr})" histogramset=${root}_bg_hist.fits histogrambinsize=$bin
   evselect table=$bginput withrateset=Y timebinsize=$bin rateset=${root}_bg_rate.fits maketimecolumn=Y makeratecolumn=Y expression="(PI in [${elo}:${ehi}])&&(${expr})"
 else 
  evselect table=${bginput} withhistogramset=T histogramcolumn=TIME expression="(PI in [${elo}:${ehi}])" histogramset=${root}_bg_hist.fits histogrambinsize=$bin
  evselect table=$bginput withrateset=Y timebinsize=$bin rateset=${root}_bg_rate.fits maketimecolumn=Y makeratecolumn=Y expression="(PI in [${elo}:${ehi}])"
 endif # for expr==none

endif #(for regflag==true)

#Generate GTI file
 echo "Creating bg GTI file for time bins with count rate ($minrate < count rate < $maxrate ) \n"

 tabgtigen table=${root}_bg_rate.fits gtiset=${root}_bg_rate_gti.fits expression="( RATE.lt.$maxrate )&&( RATE.gt.$minrate ) "


#Applying GTI to bgevents
echo "Applying GTI to bgevents...\n"

if ( "$expr" != none ) then
 evselect table=$bginput withfilteredset=yes filteredset=${root}_bg_clean_evt.fits filtertype=expression expression="(GTI(${root}_bg_rate_gti.fits,TIME))&&(${expr})" destruct=yes keepfilteroutput=yes
else
 evselect table=$bginput withfilteredset=yes filteredset=${root}_bg_clean_evt.fits filtertype=expression expression="GTI(${root}_bg_rate_gti.fits,TIME)" destruct=yes keepfilteroutput=yes
endif

#Getting livetimes
#Comparing livetime of cleaned events table
set livetime_bg=`fkeyprint "${bgevents}[EVENTS]" LIVETIME | grep "Live time" | awk '{print $2}'`
set livetime_clean_bg=`fkeyprint "${root}_bg_clean_evt.fits[EVENTS]" LIVETIME | grep "Live time" | awk '{print $2}'`
set time_diff_bg=`echo $livetime_bg $livetime_clean_bg | awk '{print $1-$2}'`
echo "Old bg LIVETIME was ${livetime_bg}s; removed ${time_diff_bg}s, leaving a LIVETIME of ${livetime_clean_bg}s.\n"

goto scriptend

#Getting ratio of livetimes
set livetime_ratio=`echo $livetime_clean $livetime_clean_bg | awk '{print $1/$2}'`
echo "Livetime ratio (Events/Background)= $livetime_ratio \n"

#Getting out of FOV counts: (skycasting needs to be done) 
#Check that data has been skycasted
if (($sky == Y )||( $sky == y)) then
  if ( $instr == EPN ) then
   echo "Comparing out of FOV counts for singles and doubles...\\n"
   set patexp="PATTERN<=4"
   set parcirc="!(CIRCLE(-2120,-1080,18000,DETX,DETY))"
   else
   echo "Comparing out of FOV counts for singles, doubles, triples and quads ...\n"
   set patexp="PATTERN<=12"
   set parcirc="!(CIRCLE(0,0,18000,DETX,DETY))&&((CIRCLE(0,15000,34500,DETX,DETY)||DETX<-10000||DETX>10000))"
  endif
  set parimages='xcolumn=X ycolumn=Y ximagebinsize=400 yimagebinsize=400 updateexposure=false imagedatatype=Real32 squarepixels=true imagebinning=binSize withimageset=Y writedss=false withimagedatatype=true keepfilteroutput=false'  
  set badratio = 0
  set enlo=300
  set enhi=10000
  echo "Creating images ${enlo}-${enhi}eV; Pattern expression: ${patexp})\n"
  evselect -w 0 -V 0 table=${root}_clean_evt.fits imageset=tempfov1 expression='PI>='$enlo'&&PI<='$enhi'&& '$patexp'&& '$parcirc'&& #XMMEA_16' $parimages
  set oFOVev = `fimgstat tempfov1 0 99999999 | grep sum | awk '{print $8}'`
  set oFOVev = `echo $oFOVev | awk '{print int($1+0.5)}'`
  set oFOVevexp=`fkeyprint tempfov1 EXPOSURE | grep ONTIME | awk '{print $2}'`
  if ($oFOVev <= 0) then 
   echo 'No out-of-FOV events found in Events - perhaps you have previously rejected them'
   set FOVratio = 0.0
   set badratio = 1
  endif
   
  evselect -w 0 -V 0 table=${root}_bg_clean_evt.fits imageset=tempfov2 expression='PI>='$enlo'&&PI<='$enhi'&& '$patexp'&& '$parcirc'&& #XMMEA_16' $parimages
  set oFOVbg = `fimgstat tempfov2 0 99999999 | grep sum | awk '{print $8}'`
  set oFOVbg = `echo $oFOVbg | awk '{print int($1+0.5)}'`
  set oFOVbgexp=`fkeyprint tempfov2 EXPOSURE | grep ONTIME | awk '{print $2}'`
  if ($oFOVbg <= 0) then 
   echo 'No out-of-FOV events found in Background - perhaps you have previously rejected them'
   set badratio = 1
  endif
  if ($badratio == 0) then 
   set FOVratio = `echo $oFOVev $oFOVbg | awk '{print $1/$2}'`
   echo "Out-of-FOV count ratio (Events/Background): ${FOVratio}"
#   set FOVrateratio=`echo $oFOVev $oFOVbg $oFOVevexp $oFOVbgexp | awk '{print ($1*$4)/($2*$3)}'`
    set FOVrateratio=`echo $oFOVev $oFOVbg $livetime_clean $livetime_clean_bg | awk '{print ($1*$4)/($2*$3)}'`
   echo "Out of FOV count rate ratio (Events/Background): $FOVrateratio"  
  else
   echo 'no out-of-FOV count ratio given'
  endif
  rm -f tempfov?
  else
  echo "Not calculating out of FOV - data has not been skycasted" 
endif #skycasting check

#Calculating high energy count rates: 
# Using 10-12keV for MOS and 12-14keV for PN
set parimages='xcolumn=X ycolumn=Y ximagebinsize=80 yimagebinsize=80 updateexposure=false imagedatatype=Real32 squarepixels=true imagebinning=binSize withimageset=Y writedss=false withimagedatatype=true keepfilteroutput=false' 
# 1) Using high energy full image counts
echo "Creating high energy count rates using full image counts...(${elo}-${ehi})\n"
if ( $instr == EPN ) then
 evselect -w 0 -V 0 table=${root}_clean_evt.fits $parimages imageset=${root}_src.fits expression="(PI in [${elo}:${ehi}])&&(#XMMEA_EP)&&(PATTERN<=4)"
 evselect -w 0 -V 0 table=${root}_bg_clean_evt.fits $parimages imageset=${root}_bg_src.fits expression="(PI in [${elo}:${ehi}])&&(#XMMEA_EP)&&(PATTERN<=4)" 
else 
 evselect -w 0 -V 0 table=${root}_clean_evt.fits $parimages imageset=${root}_src.fits expression="(PI in [${elo}:${ehi}])&&(#XMMEA_EM)&&(PATTERN<=12)"
 evselect -w 0 -V 0 table=${root}_bg_clean_evt.fits $parimages imageset=${root}_bg_src.fits expression="(PI in [${elo}:${ehi}])&&(#XMMEA_EM)&&(PATTERN<=12)"
endif
set srcexpo=`fkeyprint ${root}_src.fits EXPOSURE | grep ONTIME | awk '{print $2}'`
set bkgexpo=`fkeyprint ${root}_bg_src.fits EXPOSURE | grep ONTIME | awk '{print $2}'`
set srccts=`fimgstat ${root}_src.fits INDEF INDEF | grep sum | awk '{print $8}'`
set bkgcts=`fimgstat ${root}_bg_src.fits INDEF INDEF | grep sum | awk '{print $8}'`
#set HEratio=`echo $srccts $bkgcts $srcexpo $bkgexpo | awk '{print ($1*$4)/($2*$3)}'`
#set HEctratio=`echo $srccts $bkgcts | awk '{print $1/$2}'`
set HEratio=`echo $srccts $bkgcts $livetime_clean $livetime_clean_bg | awk '{print ($1*$4)/($2*$3)}'`
set HEctratio=`echo $srccts $bkgcts | awk '{print $1/$2}'`

echo "High Energy full image count rate ratio is $HEratio \n"
echo "High Energy full image count ratio is $HEctratio \n"
#dont think that this is particularly useful)

echo "All ratios are as follows(Evt/Bkg):\n"
echo "Exposure               : $livetime_ratio " 
echo "High E counts          : $HEctratio      "
echo "Out of FOV counts      : $FOVratio       "
echo "Count rate ratios are:"
echo "(evt counts* bg exposure)/(evt exposure *bg counts)"
echo "Using EXPOSURE not ONTIME keyword"
echo "High E count rate      : $HEratio        "
echo "Out of FOV count rate  : $FOVrateratio \n"

echo "Old,new, and difference in exposure times are as follows (s):\n"
echo "Source     : $livetime $livetime_clean "
echo "Background : $livetime_bg $livetime_clean_bg \n"

echo "Cleaned events file is ${root}_clean_evt.fits; cleaned background events file is ${root}_bg_clean_evt.fits.\n"
echo "All done!\n"

scriptend:

if ( -e tempEv01 ) then
 rm tempEv01
endif

if ( -e tempEv02 ) then
 rm tempEv02
endif

rm *filter*

rm ${root}_stats.txt
rm ${root}_hist.fits
rm ${root}_chipsscript.txt
rm ${root}_rate.fits
rm ${root}_rate_gti.fits
rm ${root}_hist_clean.fits
rm ${root}_bg_rate.fits
rm ${root}_bg_hist.fits
rm ${root}_bg_rate_gti.fits

exit
