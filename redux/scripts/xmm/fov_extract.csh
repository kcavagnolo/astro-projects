#!/bin/tcsh
# Script to perform an analysis on the out of fov 
# count rates during flaring. If the count rates
# increase significantly in the oofov during flaring,
# then we know that there is some leakage from the fov
# to the oofov. 
# inputs will be a non-cleaned events file for a 
# source dataset. An initial run will be done to 
# determine the GTI's for the cleaning. No background
# file is necessary. 
#

set version=1.1

# version 1.0 RFT 16/03/05
# version 1.1 RFT 13/10/06 - adding a region file to generate FOV count rates


cat <<EOF

--------------------`echo $0:t | tr '[a-z]' '[A-Z]'` version $version `date +%x`--------------------

EOF

if ( $#argv < 3 ) then
cat <<EOF

Use fov_extract.csh events bin root [-c0.1]

data in brackets are optional

 events       - the events file for your observation
 bin          - time binning for your rate curve
 root         - root name for your output files
 -----Optional Argument------
 -c           - level at which to cut your data before
                the data is 3 sigma clipped

EOF
exit
endif

set events=$1
set bin=$2
set root=$3
set max="none"


#check input files exist
if ( ! -e $events ) then
  echo "Error: file $events not found. Exiting..." 
  exit
endif

#check if CIAO is installed
if ( ! $?ASCDS_INSTALL ) then
 set ciao=no
 echo "WARNING: CIAO not installed - lightcurve cleaning will work, but the results will not be plotted.\n"
else set ciao=yes
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
#set elo=300
#set ehi=10000
set i=4
while ( $i <= $#argv )
 set c=`echo $argv[${i}] | cut -c1`
 if ( $c != - ) then
  echo "ERROR: argument $i doesn't begin with a -\n"
  exit
 endif
 set type=`echo $argv[${i}] | cut -c2`
 if ( $type != c ) then
  echo "ERROR: argument $i is not of type c.\n"
  echo $type $argv[${i}]
  exit
 endif

 set arg=`echo $argv[${i}] | tail +3c`

 if ( $type == c ) then
  set max=$arg
 endif
@ i++
end

#Creating a time histogram
echo "Creating lightcurve histogram in range ${elo}-${ehi} eV..."

  evselect table=$events withhistogramset=true histogramcolumn=TIME expression="(PI in [${elo}:${ehi}])" histogramset=${root}_hist.fits histogrambinsize=$bin
endif

echo "Histogram written to ${root}_hist.fits\n"

if ( $max == "none" ) then
echo Please look at ${root}_hist.fits and select a maximum cutoff value from the table. Put this into the max argument as -m500 for example.
exit
endif

cp ${root}_hist.fits ${root}_hist_clean.fits

#Create rate curve
evselect -V 0 table=${events} withrateset=Y timebinsize=${bin} rateset=${root}_rate.fits maketimecolumn=Y makeratecolumn=Y expression="(PI in [${elo}:${ehi}])"

#getting statistics:

fselect "${root}_hist_clean.fits[HISTO]" "${root}_hist_clean.fits" "((COUNTS <= $max)&&(COUNTS >= 0))" clobber=yes

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
@ i++
end
echo "  Cleaned histogram written to ${root}lc_hist_clean.fits\n"
set maxrate=`echo $maxcounts $bin | awk '{print $1/$2}'`
set meanrate=`echo $mean $bin | awk '{print $1/$2}'`
set minrate=`echo $mincounts $bin | awk '{print $1/$2}'`

echo "Creating GTI file for time bins with count rate ($minrate < count rate <$maxrate)"
tabgtigen table=${root}_rate.fits gtiset=${root}_rate_gti.fits expression="(RATE.lt.$maxrate)&&(RATE.gt.$minrate)"

echo "Applying GTI to events..."

 evselect table=$events withfilteredset=yes filteredset=${root}_clean_evt.fits filtertype=expression expression="GTI(${root}_rate_gti.fits,TIME)" destruct=yes keepfilteroutput=yes

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

#echo "Creating GTI file for time bins with count rate ($minrate > count rate > $maxrate)"
#tabgtigen table=${root}_rate.fits gtiset=${root}_flare_rate_gti.fits expression="(RATE.gt.$maxrate)&&(RATE.lt.$minrate)"

echo "Creating GTI file for time bins with count rate (count rate > $maxrate)"
tabgtigen table=${root}_rate.fits gtiset=${root}_flare_rate_gti.fits expression="(RATE.gt.$maxrate)"

echo "Applying GTI to events..."

 evselect table=$events withfilteredset=yes filteredset=${root}_flare_evt.fits filtertype=expression expression="GTI(${root}_flare_rate_gti.fits,TIME)" destruct=yes keepfilteroutput=yes

#Comparing livetime of cleaned events table
set livetime_flare=`fkeyprint "${root}_flare_evt.fits[EVENTS]" LIVETIME | grep "Live time" | awk '{print $2}'`
set time_diff_flare=`echo $livetime $livetime_flare | awk '{print $1-$2}'`
echo "Old LIVETIME was ${livetime}s; removed ${time_diff_flare}s, leaving a LIVETIME of ${livetime_flare}s.\n"

#So we have:
# a cleaned events data file
# a gti file
# a flared events data file (i.e. all regions outside the gtis)
# 
#We need:
# counts rates for non-flared data in 
# the fov and oofov. 


#Getting out of FOV counts:
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
  evselect -w 0 -V 0 table=${root}_flare_evt.fits imageset=tempfov2 expression='PI>='$enlo'&&PI<='$enhi'&& '$patexp'&& '$parcirc'&& #XMMEA_16' $parimages
#  evselect -w 0 -V 0 table=$events imageset=tempfov3 expression='PI>='$enlo'&&PI<='$enhi'&& '$patexp'&& '$parcirc'&& #XMMEA_16' $parimages
  

  set oFOVev = `fimgstat tempfov1 0 99999999 | grep sum | awk '{print $8}'`
  set oFOVev = `echo $oFOVev | awk '{print int($1)}'`
  set oFOVexp=`fkeyprint tempfov1 EXPOSURE | grep ONTIME | awk '{print $2}'`
 
  set oFOVflev = `fimgstat tempfov2 0 99999999 | grep sum | awk '{print $8}'`
  set oFOVflev = `echo $oFOVflev | awk '{print int($1)}'`
  set oFOVflexp=`fkeyprint tempfov2 EXPOSURE | grep ONTIME | awk '{print $2}'`

  echo "oFOV ev,exp,cl_livetime:" $oFOVev $oFOVexp $livetime_clean
  echo "oFOV fl ev,exp,fl_livetime:" $oFOVflev $oFOVflexp $livetime_flare

  if ($oFOVev <= 0) then 
   echo 'No out-of-FOV events found in Events - perhaps you have previously rejected them'
   set FOVratio = 0.0
   set badratio = 1
  endif
   set oFOVrate_exp=`echo $oFOVexp $oFOVev | awk '{print $2/$1}'`
   set oFOVflrate_exp=`echo $oFOVflexp $oFOVflev | awk '{print $2/$1}'`
   set oFOVrate_liv=`echo $oFOVev $livetime_clean | awk '{print $1/$2}'`
   set oFOVflrate_liv=`echo $oFOVflev $livetime_flare | awk '{print $1/$2}'`
   set per_inc_exp=`echo $oFOVrate_exp $oFOVflrate_exp | awk '{print ($2-$1)*100/$1}'` 
   set per_inc_liv=`echo $oFOVrate_liv $oFOVflrate_liv | awk '{print ($2-$1)*100/$1}'`
#   set FOVrate=`echo $FOVexp $FOVev | awk '{print $2/$1}'`
   echo "\n"
   echo "Using the EXPOSURE keyword, \n"
   echo "Out of FOV c/s for cleaned data is $oFOVrate_exp "
   echo "Ouf of FOV c/s for flared data is $oFOVflrate_exp"
   echo "Percentage increase is ${per_inc_exp}% \n"
   echo "Using the LIVETIME keyword, \n"
   echo "Out of FOV c/s for cleaned data is $oFOVrate_liv "   
   echo "Out of FOV c/s for flared data is $oFOVflrate_liv "
   echo "Percentage increase is ${per_inc_liv}% \n"

#   echo " FOV count rate is: $FOVrate"

  else
   echo 'no out-of-FOV count ratio given'
endif


# Okay. Well the numbers seem a little high, but I see no reason to disbelieve them
# So what we want to do now is to get a similar set of values for the FOV and compare the increase in count rate there. 

#Creating tempfov3 (cleaned fov data) tempfov4 (flared fov data)

#extracting FOV data
set reg=ds9.reg
set sourcefilter=`/data1/rft/rftscripts/xmmstring.csh $reg | grep expression | awk '{print $4}'`
if ( $instr == EPN ) then
   echo "Comparing FOV counts for singles and doubles...\\n"
   set parcirc="!(CIRCLE(-2120,-1080,18000,DETX,DETY))"
   set flag="#XMMEA_EP"
 else
   echo "Comparing FOV counts for singles, doubles, triples and quads ...\n"
   set parcirc="!(CIRCLE(0,0,18000,DETX,DETY))&&((CIRCLE(0,15000,34500,DETX,DETY)||DETX<-10000||DETX>10000))"
   set flag="#XMMEA_EM"
endif

  set badratio = 0
  set enlo=300
  set enhi=10000
  echo "Creating images ${enlo}-${enhi}eV; Pattern expression: ${patexp})\n"
  evselect -w 0 -V 0 table=${root}_clean_evt.fits imageset=tempfov3 expression='PI>='$enlo'&&PI<='$enhi'&&'$patexp'&&'$flag'' $parimages
  evselect -w 0 -V 0 table=${root}_clean_evt.fits imageset=tempfov3b expression='PI>='$enlo'&&PI<='$enhi'&&'$patexp'&&'$flag'&&'$sourcefilter'' $parimages
  evselect -w 0 -V 0 table=${root}_flare_evt.fits imageset=tempfov4 expression='PI>='$enlo'&&PI<='$enhi'&&'$patexp'&&'$flag'' $parimages
  evselect -w 0 -V 0 table=${root}_flare_evt.fits imageset=tempfov4b expression='PI>='$enlo'&&PI<='$enhi'&&'$patexp'&&'$flag'&&'$sourcefilter'' $parimages
  set FOVev = `fimgstat tempfov3 0 99999999 | grep sum | awk '{print $8}'`
  set FOVev = `echo $FOVev | awk '{print int($1)}'`
 
  set FOVflev = `fimgstat tempfov4 0 99999999 | grep sum | awk '{print $8}'`
  set FOVflev = `echo $FOVflev | awk '{print int($1)}'`

  echo "FOV ev:" $FOVev
  echo "FOV fl ev:" $FOVflev

  set FOVrate_exp=`echo $oFOVexp $FOVev | awk '{print $2/$1}'`
  set FOVflrate_exp=`echo $oFOVflexp $FOVflev | awk '{print $2/$1}'`
  set FOVrate_liv=`echo $FOVev $livetime_clean | awk '{print $1/$2}'`
  set FOVflrate_liv=`echo $FOVflev $livetime_flare | awk '{print $1/$2}'`
  set Fper_inc_exp=`echo $FOVrate_exp $FOVflrate_exp | awk '{print ($2-$1)*100/$1}'` 
  set Fper_inc_liv=`echo $FOVrate_liv $FOVflrate_liv | awk '{print ($2-$1)*100/$1}'`
  echo "\n"
  echo "Using the EXPOSURE keyword, \n"
  echo "FOV c/s for cleaned data is $FOVrate_exp "
  echo "FOV c/s for flared data is $FOVflrate_exp"
  echo "Percentage increase is ${Fper_inc_exp}% \n"
  echo "Using the LIVETIME keyword, \n"
  echo "FOV c/s for cleaned data is $FOVrate_liv "   
  echo "FOV c/s for flared data is $FOVflrate_liv "
  echo "Percentage increase is ${Fper_inc_liv}% \n"

# Finally we want to detect whether there are any soft protons in the fov at high energies. We are going to do the same thing again, but with a 10-12 keV range. 

if ( $instr == EPN ) then
   echo "Comparing FOV counts for singles and doubles...\\n"
   set parcirc="!(CIRCLE(-2120,-1080,18000,DETX,DETY))"
   set flag="#XMMEA_EP"
 else
   echo "Comparing FOV counts for singles, doubles, triples and quads ...\n"
   set parcirc="!(CIRCLE(0,0,18000,DETX,DETY))&&((CIRCLE(0,15000,34500,DETX,DETY)||DETX<-10000||DETX>10000))"
   set flag="#XMMEA_EM"
endif

  set badratio = 0
  set enlo=10000
  set enhi=12000
  echo "Creating images ${enlo}-${enhi}eV; Pattern expression: ${patexp})\n"
  evselect -w 0 -V 0 table=${root}_clean_evt.fits imageset=tempfov5 expression='PI>='$enlo'&&PI<='$enhi'&&'$patexp'&&'$flag'' $parimages
  evselect -w 0 -V 0 table=${root}_flare_evt.fits imageset=tempfov6 expression='PI>='$enlo'&&PI<='$enhi'&&'$patexp'&&'$flag'' $parimages

  set HEFOVev = `fimgstat tempfov5 0 99999999 | grep sum | awk '{print $8}'`
  set HEFOVev = `echo $HEFOVev | awk '{print int($1)}'`
 
  set HEFOVflev = `fimgstat tempfov6 0 99999999 | grep sum | awk '{print $8}'`
  set HEFOVflev = `echo $HEFOVflev | awk '{print int($1)}'`

  echo "HEFOV ev:" $HEFOVev
  echo "HEFOV fl ev:" $HEFOVflev

  set HEFOVrate_exp=`echo $oFOVexp $HEFOVev | awk '{print $2/$1}'`
  set HEFOVflrate_exp=`echo $oFOVflexp $HEFOVflev | awk '{print $2/$1}'`
  set HEFOVrate_liv=`echo $HEFOVev $livetime_clean | awk '{print $1/$2}'`
  set HEFOVflrate_liv=`echo $HEFOVflev $livetime_flare | awk '{print $1/$2}'`
  set HEFper_inc_exp=`echo $HEFOVrate_exp $HEFOVflrate_exp | awk '{print ($2-$1)*100/$1}'` 
  set HEFper_inc_liv=`echo $HEFOVrate_liv $HEFOVflrate_liv | awk '{print ($2-$1)*100/$1}'`
  echo "\n"

  echo "Using the EXPOSURE keyword, \n"
  echo "FOV c/s for cleaned data is $HEFOVrate_exp "
  echo "FOV c/s for flared data is $HEFOVflrate_exp"
  echo "Percentage increase is ${HEFper_inc_exp}% \n"
  echo "Using the LIVETIME keyword, \n"
  echo "FOV c/s for cleaned data is $HEFOVrate_liv "   
  echo "FOV c/s for flared data is $HEFOVflrate_liv "
  echo "Percentage increase is ${HEFper_inc_liv}% \n"

# Summarising the results. Excluding the EXPOSURE keyword

  echo "Summary...\n"
  echo "Cleaned data corresponds to data which has been 3 sigma clipped"
  echo "Flared data corresponds to everything else.\n"
  echo "Using the LIVETIME keyword.\n"
  
  echo "Broad band out-of-FOV count rates. (0.3-10keV)\n"
#  echo "Using the EXPOSURE keyword, \n"
#  echo "Out of FOV c/s for cleaned data is $oFOVrate_exp "
#  echo "Ouf of FOV c/s for flared data is $oFOVflrate_exp"
#  echo "Percentage increase is ${per_inc_exp}% \n"
#  echo "Using the LIVETIME keyword, \n"
  echo "Out of FOV c/s for cleaned data is $oFOVrate_liv "   
  echo "Out of FOV c/s for flared data is $oFOVflrate_liv "
  echo "Percentage increase is ${per_inc_liv}% \n"

  echo "Broad band FOV count rates. (0.3-10keV)\n"
#  echo "Using the EXPOSURE keyword, \n"
#  echo "FOV c/s for cleaned data is $FOVrate_exp "
#  echo "FOV c/s for flared data is $FOVflrate_exp"
#  echo "Percentage increase is ${Fper_inc_exp}% \n"
#  echo "Using the LIVETIME keyword, \n"
  echo "FOV c/s for cleaned data is $FOVrate_liv "   
  echo "FOV c/s for flared data is $FOVflrate_liv "
  echo "Percentage increase is ${Fper_inc_liv}% \n"

  echo "High band FOV count rates. (10-12keV)\n"
#  echo "Using the EXPOSURE keyword, \n"
#  echo "FOV c/s for cleaned data is $HEFOVrate_exp "
#  echo "FOV c/s for flared data is $HEFOVflrate_exp"
#  echo "Percentage increase is ${HEFper_inc_exp}% \n"
#  echo "Using the LIVETIME keyword, \n"
  echo "FOV c/s for cleaned data is $HEFOVrate_liv "   
  echo "FOV c/s for flared data is $HEFOVflrate_liv "
  echo "Percentage increase is ${HEFper_inc_liv}% \n"


# rm -f tempfov?
