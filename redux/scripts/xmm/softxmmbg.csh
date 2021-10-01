#!/bin/tcsh
#Script to take an XMM events dataset and identify the spectral 
#variation of soft protons to the background. The input arguments 
#will be a flare cleaned events dataset and a flared set containing
#only the BTI (bad time intervals) i.e. those times that are 
#outside of the 3 sigma boundary. 

set version=1.1

# version 1.1          RFT 14/06/05 - Including a normalisation to the 
#                                     same mean count rate for better   
#                                     means of comparison. Also have 
#                                     changed the spectral generation 
#                                     part of it. Not calling 
#                                     xmm_specgen.csh anymore.                                      

cat <<EOF

--------------------`echo $0:t | tr '[a-z]' '[A-Z]'` version $version `date +%x`--------------------

EOF

if ( $#argv < 5 ) then
cat <<EOF

Use softxmmbg.csh cl_evt1 fl_evt1 reg1 root1 root2

 cl_evt1       - the cleaned events file for your first observation
 fl_evt1       - the flare events file for your first observation
 reg1          - a region file to extract a spectrum for your
                 first observation. 
 root1         - root name for cleaned data
 root2         - root name for flared data

EOF
exit
endif

set cl_ev1=$1
set fl_ev1=$2
set reg1=$3
set root1a=$4
set root1b=$5

#check input files exist
if ( ! -e $cl_ev1 ) then
  echo "Error: file $cl_ev1 not found. Exiting..." 
  exit
endif

if ( ! -e $fl_ev1 ) then
  echo "Error: file $fl_ev1 not found. Exiting..." 
  exit
endif

if ( ! -e $reg1 ) then
  echo "Error: file $reg1 not found. Exiting..." 
  exit
endif

#check that region file is in CIAO format

set rtype=`head -1 $reg1 | awk '{print $5}'`
if ( $rtype != CIAO ) then
 echo ERROR - REGION FILE "$reg1" DOES NOT APPEAR TO BE IN CIAO FORMAT"\n"
 exit
endif

#Getting INST:
if ( -e tmpdmp1 ) then
 rm -f tmpdmp1
endif

fdump $cl_ev1 tmpdmp1 TIME - prdata=no showcol=no showunit=no showrow=no clobber=yes
set instr=`grep INSTRUME < tmpdmp1 | head -1 | awk '{print $2}'`
set instr=`echo $instr | tr -d "'"`
rm -f tmpdmp1

#using a lower energy range
set elo=300
set ehi=10000

if ( instr == EPN ) then
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
set sourcefilter=`/data1/rft/rftscripts/xmmstring.csh $reg1 | grep expression | awk '{print $4}'`
set parimages='xcolumn=X ycolumn=Y ximagebinsize=400 yimagebinsize=400 updateexposure=false imagedatatype=Real32 squarepixels=true imagebinning=binSize withimageset=Y writedss=false withimagedatatype=true keepfilteroutput=false' 

echo "Creating spectrum ...\n"

evselect -w 0 -V 0 table=$cl_ev1 filteredset=tempEv01 expression="$expr" updateexposure=N writedss=Y withfilteredset=T destruct=Y keepfilteroutput=T 
evselect table=tempEv01 spectrumset=tempsp01 expression=$sourcefilter $parspec

evselect -w 0 -V 0 table=$fl_ev1 filteredset=tempEv02 expression="$expr" updateexposure=N writedss=Y withfilteredset=T destruct=Y keepfilteroutput=T 
evselect table=tempEv02 spectrumset=tempsp02 expression=$sourcefilter $parspec

#need to create images to determine the count rates.
echo "Creating images ...\n"

set enlo=300
set enhi=10000

evselect -w 0 -V 0 table=$cl_ev1 imageset=tempimg01 expression='PI>='$enlo'&&PI<='$enhi'&& '$expr' && '$sourcefilter''  $parimages

evselect -w 0 -V 0 table=$fl_ev1 imageset=tempimg02 expression='PI>='$enlo'&&PI<='$enhi'&& '$expr' && '$sourcefilter''  $parimages

# getting counts for data. 

set cl_cts=`fimgstat tempimg01 0 99999999 | grep sum | awk '{print $8}'`
set cl_cts=`echo $cl_cts | awk '{print int($1+0.5)}'`
set cl_exp=`fkeyprint tempimg01 EXPOSURE | grep ONTIME | awk '{print $2}'`
set fl_cts=`fimgstat tempimg02 0 99999999 | grep sum | awk '{print $8}'`
set fl_cts=`echo $fl_cts | awk '{print int($1+0.5)}'`
set fl_exp=`fkeyprint tempimg02 EXPOSURE | grep ONTIME | awk '{print $2}'`

#set norm=`echo $cl_cts $cl_exp $fl_cts $fl_exp | awk '{print $1*$4/($2*$3)}'`
rm tempimg*
set norm=`echo $cl_cts $fl_cts | awk '{print $1/$2}'`
echo "norm = $norm "
set norm=1
echo "normalising spectra ...\n"
punlearn fcalc
punlearn fdelcol

fcalc tempsp02 tempsp03 COUNTS2 "COUNTS / $norm" clobber=yes
fcalc tempsp03 tempsp04 COUNTS "COUNTS2" clobber=yes
fdelcol tempsp04+1 COUNTS2 N Y

cp tempsp01 ${root1a}_sp_cl.fits
cp tempsp04 ${root1b}_sp_fl.fits
cp tempsp02 ${root1b}_sp_flnn.fits

rm tempEv*
rm tempsp*

#/data1/rft/rftscripts/xmm_specgen.csh $cl_ev1 $reg1 $reg1 N N atthk.dat Y Y 0 $pat $root1a

#/data1/rft/rftscripts/xmm_specgen.csh $fl_ev1 $reg1 $reg1 N N atthk.dat N N 0 $pat $root1b

rm sp_b_*
rm sp_scb_*
rm sp_bs_*
