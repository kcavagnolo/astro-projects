#!/bin/tcsh
# script to clean data using bad-time-intervals. This is only useful
# for determining the soft proton contribution to the data. I will 
# be running this on several datasets in order to determine the spectral
# variation of soft protons. 
#
# version 1.0 RFT 29/04/05 Determining the GTI's, calculating BTI's from 
#                          them and extracting spectra.  
# 
#
# assuming no filtering on PATTERN/FLAG etc. (unless generating spectra)
#

set version=1.0


cat <<EOF

--------------------`echo $0:t | tr '[a-z]' '[A-Z]'` version $version `date +%x`--------------------
EOF

set args="$#argv"
if ( $args <= 2 ) then

cat <<EOF

Use bgrate_clean.csh bgfile [-ccut] [-eelo] [-Eehi] [-rY] [-aY] [-ssrc]
argument in [] is optional

  bgfile     - events file
  bin        - time bin size
  root       - root name for output files
  ------------------------------------------
  -c         - maximum cut off limit for your
               rate curve
  -eelo      - lower energy range def=10kev(MOS)
               12kev(PN)
  -Eehi      - upper energy range def=12kev(MOS)
               14kev(PN)
  -rY        - generate an rmf? def=N
  -aY        - generate an arf? def=N
  -ssrc      - source region file. Necessary if 
               generating a spectrum

EOF
exit
endif

set events=$1
set bin=$2
set root=$3

#Check file exists
if ( ! -e $events ) then
 echo "Error: $events does not exist"
 exit
endif
#sort out optional args

set max="none"
set srcreg="none"
set rmfgen="N"
set arfgen="N"
set srcreg="none"

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

set i=4
while ( $i <= $args ) 
 set c=`echo $argv[${i}] | cut -c1`
 if ( $c != - ) then
  echo "ERROR: argument $i doesn't begin with a -\n"
  exit
 endif
 set type=`echo $argv[${i}] | cut -c2`
 if (( $type != c )&&( $type != e )&&( $type != E )&&( $type != r )&&( $type != a )&&( $type != s)) then
  echo "Error: argument $i must begin with a c,a,r,s,e or E.\n"
  exit
 endif
 if ( $type == c ) then
  set max=`echo $argv[${i}] | tail +3c`
 endif
 if ( $type == e ) then
  set elo=`echo $argv[${i}] | tail +3c`
 endif
 if ( $type == E ) then
  set ehi=`echo $argv[${i}] | tail +3c`
 endif
 if ( $type == r) then
  set rmfgen="Y"
 endif
 if ( $type == a ) then
  set arfgen="Y"
 endif
 if ( $type == s ) then
  set srcreg=`echo $argv[${i}] | tail +3c`
 endif
@ i++
end

#check source region file is in CIAO format
if ( $srcreg != "none" ) then
 set rtype=`head -1 $srcreg | awk '{print $5}'`
 if ( $rtype != CIAO ) then
  echo ERROR - REGION FILE "$srcreg" DOES NOT APPEAR TO BE IN CIAO FORMAT"\n"
  exit
 endif
endif

echo "Creating lightcurve histogram in range ${elo}-${ehi} eV..."

  evselect table=$events withhistogramset=true histogramcolumn=TIME expression="(PI in [${elo}:${ehi}])" histogramset=${root}_hist.fits histogrambinsize=$bin

echo "  histogram written to ${root}_hist.fits\n"

if ( $max == "none" ) then
echo "look at ${root}_hist.fits and select a cut off value from the rate curve"
exit
endif

#Creating rate curve
echo "Creating rate histogram...\n"
evselect -V 0 table=${events} withrateset=Y timebinsize=${bin} rateset=${root}_rate.fits maketimecolumn=Y makeratecolumn=Y expression="(PI in [${elo}:${ehi}])"

cp ${root}_hist.fits ${root}_hist_clean.fits


echo "Creating a rate curve in range ${elo}-${ehi} keV.\n "

#finding mean of the data.

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
echo "mean rate, max rate, min rate = $meanrate $maxrate $minrate" 
echo "mean counts, max counts, min counts = $mean $maxcounts $mincounts" 

#Generate GTI file
echo "Creating BTI file for time bins with count rate ( count rate > $maxrate) \n"
tabgtigen table=${root}_rate.fits gtiset=bti.fits expression="(RATE.gt.$maxrate)"
tabgtigen table=${root}_rate.fits gtiset=gti.fits expression="(RATE.lt.$maxrate)&&(RATE.gt.$minrate)"

echo "Applying BTI to events...\n"
 evselect table=$events withfilteredset=yes filteredset=${root}_bticl.fits filtertype=expression expression="GTI(bti.fits,TIME)" destruct=yes keepfilteroutput=yes
echo "Applying GTI to events...\n"
 evselect table=$events withfilteredset=yes filteredset=${root}_gticl.fits filtertype=expression expression="GTI(gti.fits,TIME)" destruct=yes keepfilteroutput=yes
rm bti.fits
rm gti.fits 
rm ${root}_hist*
rm ${root}_rate*
rm ${root}_stats*

# unnecessary files removed. The only ones that important are the 
# raw events file, the gti events and the bti events


#need to extract a spectrum for each of these

if ( $specgen == N ) then
 echo "Not creating spectrum...exiting\n"
 echo "Output file written to ${root}_bgcl.fits"
 exit
endif

if ( ! -e $srcreg ) then
 echo "No source region found. Spectum will not be generated. Exiting...\n"
 exit
endif

#set up parameters
if ( $instr == "EPN" ) then
 set patexp="PATTERN<=4"
 set flag="#XMMEA_EP" 
 set spchmax = 20475
 set spbinsize = 5
else
 set patexp="PATTERN<=12"
 set flag="#XMMEA_EM"
 set spchmax = 11999
 set spbinsize  = 15
endif
set expr="$flag&&$patexp"
set parspec="withspectrumset=Y spectralbinsize=$spbinsize energycolumn=PI specchannelmax=$spchmax specchannelmin=0 withspecranges=Y" 
set sourcefilter=`/data1/rft/rftscripts/xmmstring.csh $srcreg | grep expression | awk '{print $4}'`


echo "Creating spectrum ...\n"

evselect -w 0 -V 0 table=${root}_bgcl.fits filteredset=tempEv01 expression="$expr" updateexposure=N writedss=Y withfilteredset=T destruct=Y keepfilteroutput=T 
evselect table=tempEv01 spectrumset=tempsp01 expression=$sourcefilter $parspec

if ( $norm_cts == "none" ) then
 cp tempsp01 ${root}_sp.fits
else
punlearn fcalc
punlearn fdelcol
fcalc tempsp01 tempsp02 COUNTS2 "COUNTS * $norm" clobber=yes
fcalc tempsp02 tempsp03 COUNTS "COUNTS2" clobber=yes
fdelcol tempsp03+1 COUNTS2 N Y
cp tempsp01 ${root}_sp_un.fits
cp tempsp03 ${root}_sp.fits
endif
#rm tempsp0* tempEv01

if ( $rmfgen == Y ) then
 echo "Calculating rmf....\n"
 rmfgen -w 0 -V 0 spectrumset=${root}_sp.fits rmfset=${root}_sp.rmf 
endif


#Create rmf/arf? Only need to do this once.

rm *_filter*

exit
