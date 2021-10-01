#!/bin/tcsh
# Script to look at spatial variation of hard particles using 
# closed filter data. a region file will be read in 
# containing annuli, which will extract spectra from each annulus.

set version=1.0

nice +19

cat <<EOF

--------------------`echo $0:t | tr '[a-z]' '[A-Z]'` version $version `date +%x`--------------------

EOF

if ( $#argv < 3 ) then
cat <<EOF

Use hardspatvar.csh events region root

    events       - closed events file
    region      - A CIAO region file containing annuli 
    root        - root name for all output files.
    
EOF
exit
endif

set events=$1
set region=$2
set root=$3

#check input files exist

if ( ! -e $1 ) then
  echo "Input events file $1 not found, exiting....\n"
  exit
endif

if ( ! -e $1 ) then
  echo "Region file $2 not found, exiting....\n"
  exit
endif

#check region file is in CIAO format. 

set ciaotest=`grep CIAO < $region | wc -l`
if ( $ciaotest != 1 ) then
  echo "  ERROR - SOURCE REGION FILE DOES NOT APPEAR TO BE IN CIAO FORMAT\n\n"
  exit
endif

#need to identify how many annuli there are

cp $region tempreg1
set maxtot=`wc -l < tempreg1`
set n=0
set count=0
@ nmax=$maxtot - 1
while ($n != $nmax )
@ n = $n + 1 
@ count = $count + 1
set exp0=`head -1 tempreg1 `
set exp1=`tail -$n tempreg1 | awk '{print $1}'`
set exp2=`echo $exp1 | awk -F' a' '{print $1}'`

printf "$exp0\n" >! temp_${n}.reg
printf "$exp2" >> temp_${n}.reg

end
rm tempreg1

#setting energy range:
set elo=300
set ehi=10000
#getting INST
fdump $events tmpdmp1 TIME - prdata=no showcol=no showunit=no showrow=no clobber=yes
set instr=`grep INSTRUME < tmpdmp1 | head -1 | awk '{print $2}'`
set instr=`echo $instr | tr -d "'"`
rm -f tmpdmp1

if ( $instr == EPN ) then
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


#generating spectra, responses, arfs for each annulus...

set n=$nmax
while ( $n != 0 )
echo "Creating spectra for annulus ${n}...\n"

set sourcefilter=`/data1/rft/rftscripts/xmmstring.csh temp_${n}.reg | grep expression | awk '{print $4}'`

evselect -w 0 -V 0 table=$events filteredset=tempEv01 expression="$expr" updateexposure=N writedss=Y withfilteredset=T destruct=Y keepfilteroutput=T 
evselect table=tempEv01 spectrumset=tempsp01 expression=$sourcefilter $parspec
cp tempsp01 sp_s_${n}_${root}.fits

echo "Generating rmf...\n"

set rmffile=sp_s_${n}_${root}.rmf
rmfgen -w 0 -V 0 spectrumset=sp_s_${n}_${root}.fits rmfset=$rmffile 

if ( -e tempout1 ) then
 rm tempout1
endif
if ( -e grp_${n}_${root}.fits ) then
 rm grp_${n}_${root}.fits
endif

grppha sp_s_${n}_${root}.fits grp_${n}_${root}.fits comm="chkey respfile $rmffile & group min 20 & exit" > tempout1
rm tempout1

@ n = $n - 1
end

rm temp*
rm *filter*




