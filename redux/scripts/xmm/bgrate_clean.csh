#!/bin/tcsh
# script to clean bg files only at different levels
# will create a rate curve for the data. Input will allow 
# selection of different cutoff values from the top. 
#
# version 1.0 RFT 14/03/05 Just extracting the necessary info from
#                          xmm_clean.csh. No need to skycast.
# version 1.1 RFT 27/04/05 Need to scale all the data to a mean count rate
#
#
# assuming no filtering on PATTERN/FLAG etc. (unless generating spectra)
#

set version=1.1


cat <<EOF

--------------------`echo $0:t | tr '[a-z]' '[A-Z]'` version $version `date +%x`--------------------
EOF

set args="$#argv"
if ( $args < 3 ) then

cat <<EOF

Use bgrate_clean.csh bgfile [-ccut] [-eelo] [-Eehi] [-rY] [-aY] [-ssrc]
argument in [] is optional

  bgfile     - your blank sky background file
  bin        - time bin size
  spec?      - create spectrum? Y/N
  root       - root name for output files
  ------------------------------------------
  -c         - maximum cut off limit for your
               rate curve
  -d         - minimum cut off limit for your 
               rate curve
  -eelo      - lower energy range def=10kev(MOS)
               12kev(PN)
  -Eehi      - upper energy range def=12kev(MOS)
               14kev(PN)
  -rY        - generate an rmf?
  -aY        - generate an arf?
  -ssrc      - source region file. Necessary if 
               generating a spectrum
  -nscl      - normalising the spectrum with 
               another of a different count rate.
               insert the mean count rate of the 
	       spectrum that you want to normalise 
	       the data to here.

EOF
exit
endif

set bgevents=$1
set bin=$2
set specgen=$3
set root=$4

#Check file exists
if ( ! -e $bgevents ) then
 echo "Error: $bgevents does not exist"
 exit
endif
#sort out optional args

set max="none"
set srcreg="none"
set rmfgen="N"
set arfgen="N"
set srcreg="none"
set min=0.0
set norm_cts="none"
#echo $min

#Getting INST:
if ( -e tmpdmp1 ) then
 rm -f tmpdmp1
endif
fdump $bgevents tmpdmp1 TIME - prdata=no showcol=no showunit=no showrow=no clobber=yes
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

set i=5
while ( $i <= $args ) 
 set c=`echo $argv[${i}] | cut -c1`
 if ( $c != - ) then
  echo "ERROR: argument $i doesn't begin with a -\n"
  exit
 endif
 set type=`echo $argv[${i}] | cut -c2`
 if (( $type != c )&&( $type != e )&&( $type != E )&&( $type != r )&&( $type != a )&&( $type != s)&&( $type != d )&&( $type != n )) then
  echo "Error: argument $i must begin with a c,a,r,s,e,n or E.\n"
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
 if ( $type == d) then
  set min=`echo $argv[${i}] | tail +3c`
 endif
 if ( $type == n) then
  set norm_cts=`echo $argv[${i}] | tail +3c`
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


#Creating rate curve
echo "Creating rate histogram...\n"
evselect -V 0 table=${bgevents} withrateset=Y timebinsize=${bin} rateset=${root}_rate.fits maketimecolumn=Y makeratecolumn=Y expression="(PI in [${elo}:${ehi}])"

if ( $max == "none" ) then
echo "look at ${root}_rate.fits and select a cut off value from the rate curve"
exit
endif

cp ${root}_rate.fits tmptmp1.fits
fselect "tmptmp1.fits[RATE]" "tmptmp2.fits" "((RATE <= $max)&&(RATE >= $min))" clobber=yes

fstatistic "tmptmp2.fits[RATE]" RATE rows="-" outfile=${root}_stats.txt clobber=yes
set mean=`grep mean < ${root}_stats.txt |awk '{print $8}'`
echo " mean is $mean "

if ( $norm_cts != "none" ) then
 set norm=`echo $norm_cts $mean | awk '{print $1/$2}'`
 echo "multiplying spectrum by $norm \n"
endif

rm tmptmp*

#Generate GTI file
echo "Creating bg GTI file for time bins with count rate ( $min < count rate < $max) \n"
tabgtigen table=${root}_rate.fits gtiset=gti.fits expression="(RATE.lt.$max)&&(RATE.gt.$min)"

echo "Applying GTI to bgevents...\n"
 evselect table=$bgevents withfilteredset=yes filteredset=${root}_bgcl.fits filtertype=expression expression="GTI(gti.fits,TIME)" destruct=yes keepfilteroutput=yes
rm gti.fits

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
