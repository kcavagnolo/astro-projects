#!/bin/tcsh

#script to extract and fit the residual spectrum for a cluster.
#Starting from the raw events lists. 
#

set version=1.1

# Version 1.1 - RFT 30/10/07. Including adding a seperate region
#                             input for each camera. 
#                             This is because rmfgen throws a wobbly for some
#                             PN regions and they may need to be smaller to 
#                             so as to remove the error. 
#                             May also want to exclude regions in MOS1 where
#                             the CCD has been switched off. 

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


if ( $#argv != 15 ) then
cat <<EOF

Use residspec.csh src1 src2 src3 bg1 bg2 bg3 reg1 reg2 reg3

    src1   -  events file for dataset 1.   
    src2   -  events file for dataset 2.   
    src3   -  events file for dataset 3.   
    bg1    -  bg events file for dataset 1.  | These should 
    bg2    -  bg events file for dataset 2.  | be skycasted
    bg3    -  bg events file for dataset 3.  | 
    reg1   -  region file for spectral extraction. 
	      Point sources should be excluded regions.
    reg2   -  ditto for your second events file  
    reg3   -  ditto for your thid events file. 
	      (^^ You might want to have different region files if you 
	      need to exclude certain regions for cameras, e.g. bad pixels 
	      not removed from the cleaning or missing CCD chips. 
	      See script text for more discussion. )
   max1    -  cut src1 spectrum prior to fitting? | Cuts on count rate
   max2    -  cut src2 spectrum prior to fitting? | "N"=no, a number  
   max3    -  cut src3 spectrum prior to fitting? | is count rate cut.
   max1b   -  cut src1 spectrum prior to fitting? |
   max2b   -  cut src2 spectrum prior to fitting? | This is the second
   max3b   -  cut src3 spectrum prior to fitting? | cut using 10s bins. 
EOF
exit
endif

set src1=$1
set src2=$2
set src3=$3
set bg1=$4
set bg2=$5
set bg3=$6
set reg1=$7
set reg2=$8
set reg3=$9
set max1=$10
set max2=$11
set max3=$12
set max1b=$13
set max2b=$14
set max3b=$15

#check input files exist
if ( ! -e $src1 ) then
 echo "Error: $src1 does not exist"
 exit
endif
if ( ! -e $src2 ) then
 echo "Error: $src2 does not exist"
 exit
endif
if ( ! -e $src3 ) then
 echo "Error: $src3 does not exist"
 exit
endif
if ( ! -e $bg1 ) then
 echo "Error: $bg1 does not exist"
 exit
endif
if ( ! -e $bg2 ) then
 echo "Error: $bg2 does not exist"
 exit
endif
if ( ! -e $bg3 ) then
 echo "Error: $bg3 does not exist"
 exit
endif
if ( ! -e $reg1 ) then
 echo "Error: $reg1 does not exist"
 exit
endif
if ( ! -e $reg2 ) then
 echo "Error: $reg1 does not exist"
 exit
endif
if ( ! -e $reg3 ) then
 echo "Error: $reg1 does not exist"
 exit
endif
set rtype=`head -1 $reg1 | awk '{print $5}'`
if ( $rtype != CIAO ) then
 echo ERROR - REGION FILE "$reg1" DOES NOT APPEAR TO BE IN CIAO FORMAT"\n"
 exit
endif
set rtype=`head -1 $reg2 | awk '{print $5}'`
if ( $rtype != CIAO ) then
 echo ERROR - REGION FILE "$reg2" DOES NOT APPEAR TO BE IN CIAO FORMAT"\n"
 exit
endif
set rtype=`head -1 $reg3 | awk '{print $5}'`
if ( $rtype != CIAO ) then
 echo ERROR - REGION FILE "$reg3" DOES NOT APPEAR TO BE IN CIAO FORMAT"\n"
 exit
endif

#identify rate cut? 
set maxflag1=F
set maxflag2=F
set maxflag3=F
if ( $max1 != N ) then
 set maxflag1=T
endif
if ( $max2 != N ) then
 set maxflag2=T
endif
if ( $max3 != N ) then
 set maxflag3=T
endif

set maxflag1b=F
set maxflag2b=F
set maxflag3b=F
if ( $max1b != N ) then
 set maxflag1b=T
endif
if ( $max2b != N ) then
 set maxflag2b=T
endif
if ( $max3b != N ) then
 set maxflag3b=T
endif

#------------------------------------------------------------------------------

#Step 1. Determine instrument parameters

#find instrument and set parameters
fdump $src1 tmpdmp1 TIME - prdata=no showcol=no showunit=no showrow=no clobber=yes
set inst1=`grep INSTRUME < tmpdmp1 | head -1 | awk '{print $2}'`
set inst1=`echo $inst1 | tr -d "'"`
if ($inst1 == EPN ) then
  set spchmax1 = 20475
  set spbinsize1 = 5
  set pnline1 = 190
  set flag1="FLAG==0"
  set patexp1="PATTERN<=4"
  set binsize1=256
else
  set spchmax1 = 11999
  set spbinsize1  = 15
  set pnline1 = 0
  set flag1="FLAG==0"
  set patexp1="PATTERN<=12"
  set binsize1=200
endif
set expr1="$flag1&&$patexp1"
rm -fr tmpdmp1
fdump $src2 tmpdmp1 TIME - prdata=no showcol=no showunit=no showrow=no clobber=yes
set inst2=`grep INSTRUME < tmpdmp1 | head -1 | awk '{print $2}'`
set inst2=`echo $inst2 | tr -d "'"`
if ($inst2 == EPN ) then
  set spchmax2 = 20475
  set spbinsize2 = 5
  set pnline2 = 190
  set flag2="FLAG==0"
  set patexp2="PATTERN<=4"
  set binsize2=256
else
  set spchmax2 = 11999
  set spbinsize2  = 15
  set pnline2 = 0
  set flag2="FLAG==0"
  set patexp2="PATTERN<=12"
  set binsize2=200
endif
set expr2="$flag2&&$patexp2"
rm -fr tmpdmp1
fdump $src3 tmpdmp1 TIME - prdata=no showcol=no showunit=no showrow=no clobber=yes
set inst3=`grep INSTRUME < tmpdmp1 | head -1 | awk '{print $2}'`
set inst3=`echo $inst3 | tr -d "'"`
if ($inst3 == EPN ) then
  set spchmax3 = 20475
  set spbinsize3 = 5
  set pnline3 = 190
  set flag3="FLAG==0"
  set patexp3="PATTERN<=4"
  set binsize3=256
else
  set spchmax3 = 11999
  set spbinsize3  = 15
  set pnline3 = 0
  set flag3="FLAG==0"
  set patexp3="PATTERN<=12"
  set binsize3=200
endif
set expr3="$flag3&&$patexp3"
rm -fr tmpdmp1




#--------------------------------------------------------------------------------

#goto step2

# Step 2. Reclean the data using a 2-pass 3 sigma clip
# Also correct for vignetting. Important that I
# clean on FLAG==0. Output results to a log file and save lightcurves

if ( -e ${inst1}_log.txt ) then
 rm ${inst1}_log.txt
endif
touch ${inst1}_log.txt
if ( -e ${inst2}_log.txt ) then
 rm ${inst2}_log.txt
endif
touch ${inst2}_log.txt
if ( -e ${inst3}_log.txt ) then
 rm ${inst3}_log.txt
endif
touch ${inst3}_log.txt

if ( $maxflag1 == "T" ) then
 ./xmm_lcclean.csh $src1 "${expr1}" 100 n tmpevt1 -m${max1} >> ${inst1}_log.txt
else
 ./xmm_lcclean.csh $src1 "${expr1}" 100 n tmpevt1 >> ${inst1}_log.txt
endif
if ( $maxflag1b == "T" ) then
./xmm_lcclean.csh tmpevt1_clean_evt.fits "${expr1}" 10 n tmpevt2 -e3000 -E10000 -r${reg1} -m${max1b} >> ${inst1}_log.txt
else
./xmm_lcclean.csh tmpevt1_clean_evt.fits "${expr1}" 10 n tmpevt2 -e3000 -E10000 -r${reg1} >> ${inst1}_log.txt
endif
evigweight ineventset=tmpevt2_clean_evt.fits outeventset=${inst1}_cleanw.fits 
cp tmpevt1_lc.eps ${inst1}_lc1.eps
cp tmpevt2_lc.eps ${inst1}_lc2.eps
cp tmpevt1_crhist.eps ${inst1}_crhist.eps
cp tmpevt2_crhist.eps ${inst1}_crhist2.eps
rm -fr tmp* tempEv*

if ( $maxflag2 == "T" ) then
./xmm_lcclean.csh $src2 "${expr2}" 100 n tmpevt1 -m${max2} >> ${inst2}_log.txt
else
./xmm_lcclean.csh $src2 "${expr2}" 100 n tmpevt1 >> ${inst2}_log.txt
endif
if ( $maxflag2b == "T" ) then
./xmm_lcclean.csh tmpevt1_clean_evt.fits "${expr2}" 10 n tmpevt2 -e3000 -E10000 -r${reg2} -m${max2b} >> ${inst2}_log.txt
else
./xmm_lcclean.csh tmpevt1_clean_evt.fits "${expr2}" 10 n tmpevt2 -e3000 -E10000 -r${reg2} >> ${inst2}_log.txt
endif
evigweight ineventset=tmpevt2_clean_evt.fits outeventset=${inst2}_cleanw.fits 
cp tmpevt1_lc.eps ${inst2}_lc1.eps
cp tmpevt2_lc.eps ${inst2}_lc2.eps
cp tmpevt1_crhist.eps ${inst2}_crhist.eps
cp tmpevt2_crhist.eps ${inst2}_crhist2.eps
rm -fr tmp* tempEv*

if ( $maxflag3 == "T" ) then
./xmm_lcclean.csh $src3 "${expr3}" 100 n tmpevt1 -m${max3}>> ${inst3}_log.txt
else
./xmm_lcclean.csh $src3 "${expr3}" 100 n tmpevt1 >> ${inst3}_log.txt
endif
if ( $maxflag3b == "T" ) then
./xmm_lcclean.csh tmpevt1_clean_evt.fits "${expr3}" 10 n tmpevt2 -e3000 -E10000 -r${reg3} -m${max3b} >> ${inst3}_log.txt
else
./xmm_lcclean.csh tmpevt1_clean_evt.fits "${expr3}" 10 n tmpevt2 -e3000 -E10000 -r${reg3} >> ${inst3}_log.txt
endif
evigweight ineventset=tmpevt2_clean_evt.fits outeventset=${inst3}_cleanw.fits 
cp tmpevt1_lc.eps ${inst3}_lc1.eps
cp tmpevt2_lc.eps ${inst3}_lc2.eps
cp tmpevt1_crhist.eps ${inst3}_crhist.eps
cp tmpevt2_crhist.eps ${inst3}_crhist2.eps
rm -fr tmp* tempEv*

rm -fr tmp* tempEv*
rm *filter* *chips*

goto step3

#and for the background events lists. 

if ( -e ${inst1}_bglog.txt ) then
 rm ${inst1}_bglog.txt
endif
touch ${inst1}_bglog.txt
if ( -e ${inst2}_bglog.txt ) then
 rm ${inst2}_bglog.txt
endif
touch ${inst2}_bglog.txt
if ( -e ${inst3}_bglog.txt ) then
 rm ${inst3}_bglog.txt
endif
touch ${inst3}_bglog.txt

./xmm_lcclean.csh $bg1 "${expr1}" 100 y tmpevt1 >> ${inst1}_bglog.txt
./xmm_lcclean.csh tmpevt1_clean_evt.fits "${expr1}" 10 y tmpevt2  -e300 -E10000 -r${reg1}  >> ${inst1}_bglog.txt
evigweight ineventset=tmpevt2_clean_evt.fits outeventset=${inst1}_bgcleanw.fits 
cp tmpevt1_lc.eps ${inst1}_bglc1.eps
cp tmpevt2_lc.eps ${inst1}_bglc2.eps
cp tmpevt1_crhist.eps ${inst1}_bg_crhist.eps
cp tmpevt2_crhist.eps ${inst1}_bg_crhist2.eps
rm -fr tmp* tempEv*

./xmm_lcclean.csh $bg2 "${expr2}" 100 y tmpevt1 >> ${inst2}_bglog.txt
./xmm_lcclean.csh tmpevt1_clean_evt.fits "${expr2}" 10 y tmpevt2  -e300 -E10000 -r${reg2}  >> ${inst2}_bglog.txt
evigweight ineventset=tmpevt2_clean_evt.fits outeventset=${inst2}_bgcleanw.fits 
cp tmpevt1_lc.eps ${inst2}_bglc1.eps
cp tmpevt2_lc.eps ${inst2}_bglc2.eps
cp tmpevt1_crhist.eps ${inst2}_bg_crhist.eps
cp tmpevt2_crhist.eps ${inst2}_bg_crhist2.eps
rm -fr tmp* tempEv*

./xmm_lcclean.csh $bg3 "${expr3}" 100 y tmpevt1 >> ${inst3}_bglog.txt
./xmm_lcclean.csh tmpevt1_clean_evt.fits "${expr3}" 10 y tmpevt2  -e300 -E10000 -r${reg3}  >> ${inst3}_bglog.txt
evigweight ineventset=tmpevt2_clean_evt.fits outeventset=${inst3}_bgcleanw.fits 
cp tmpevt1_lc.eps ${inst3}_bglc1.eps
cp tmpevt2_lc.eps ${inst3}_bglc2.eps
cp tmpevt1_crhist.eps ${inst3}_bg_crhist.eps
cp tmpevt2_crhist.eps ${inst3}_bg_crhist2.eps
rm -fr tmp* tempEv*

mkdir lc
cd lc
rm -fr *.eps
mv ../*.eps .
cd ../

step3:

# 2b. Write out the total losses to a file. 
if ( -e losses.txt ) then
rm losses.txt
endif
touch losses.txt

set livetime=`fkeyprint "${src1}[EVENTS]" LIVETIME | grep "Live time" | awk '{print $2}'`
set livetime_clean=`fkeyprint "${inst1}_cleanw.fits[EVENTS]" LIVETIME | grep "Live time" | awk '{print $2}'`
set time_diff=`echo $livetime $livetime_clean | awk '{print $1-$2}'`
echo "Old LIVETIME was ${livetime}s; removed ${time_diff}s, leaving a LIVETIME of ${livetime_clean}s for ${inst1}.\n" >> losses.txt
set livetime=`fkeyprint "${src2}[EVENTS]" LIVETIME | grep "Live time" | awk '{print $2}'`
set livetime_clean=`fkeyprint "${inst2}_cleanw.fits[EVENTS]" LIVETIME | grep "Live time" | awk '{print $2}'`
set time_diff=`echo $livetime $livetime_clean | awk '{print $1-$2}'`
echo "Old LIVETIME was ${livetime}s; removed ${time_diff}s, leaving a LIVETIME of ${livetime_clean}s for ${inst2}.\n"  >> losses.txt
set livetime=`fkeyprint "${src3}[EVENTS]" LIVETIME | grep "Live time" | awk '{print $2}'`
set livetime_clean=`fkeyprint "${inst3}_cleanw.fits[EVENTS]" LIVETIME | grep "Live time" | awk '{print $2}'`
set time_diff=`echo $livetime $livetime_clean | awk '{print $1-$2}'`
echo "Old LIVETIME was ${livetime}s; removed ${time_diff}s, leaving a LIVETIME of ${livetime_clean}s for ${inst3}.\n" >> losses.txt
set livetime=`fkeyprint "${bg1}[EVENTS]" LIVETIME | grep "Live time" | awk '{print $2}'`
set livetime_clean=`fkeyprint "${inst1}_bgcleanw.fits[EVENTS]" LIVETIME | grep "Live time" | awk '{print $2}'`
set time_diff=`echo $livetime $livetime_clean | awk '{print $1-$2}'`
echo "Old LIVETIME was ${livetime}s; removed ${time_diff}s, leaving a LIVETIME of ${livetime_clean}s for ${inst1}bg.\n" >> losses.txt
set livetime=`fkeyprint "${bg2}[EVENTS]" LIVETIME | grep "Live time" | awk '{print $2}'`
set livetime_clean=`fkeyprint "${inst2}_bgcleanw.fits[EVENTS]" LIVETIME | grep "Live time" | awk '{print $2}'`
set time_diff=`echo $livetime $livetime_clean | awk '{print $1-$2}'`
echo "Old LIVETIME was ${livetime}s; removed ${time_diff}s, leaving a LIVETIME of ${livetime_clean}s for ${inst2}bg.\n" >> losses.txt
set livetime=`fkeyprint "${bg3}[EVENTS]" LIVETIME | grep "Live time" | awk '{print $2}'`
set livetime_clean=`fkeyprint "${inst3}_bgcleanw.fits[EVENTS]" LIVETIME | grep "Live time" | awk '{print $2}'`
set time_diff=`echo $livetime $livetime_clean | awk '{print $1-$2}'`
echo "Old LIVETIME was ${livetime}s; removed ${time_diff}s, leaving a LIVETIME of ${livetime_clean}s for ${inst3}bg.\n" >> losses.txt

#---------------------------------------------------------------------------

# 3. Identify scale ratios for the cluster scaling by high energy and compare
# to GWP's and ensure that the spectra are the same at high energies. 
# Using evselect and ensuring that the WEIGHT column is included.

set sourcefilter1=`/data1/rft/rftscripts/xmmstring.csh $reg1 | grep expression | awk '{print $4 }'`
set sourcefilter2=`/data1/rft/rftscripts/xmmstring.csh $reg2 | grep expression | awk '{print $4 }'`
set sourcefilter3=`/data1/rft/rftscripts/xmmstring.csh $reg3 | grep expression | awk '{print $4 }'`
set parimages='xcolumn=X ycolumn=Y ximagebinsize=80 yimagebinsize=80 updateexposure=false imagedatatype=Real32 squarepixels=true imagebinning=binSize withimageset=Y writedss=false withimagedatatype=true keepfilteroutput=false' 

#inst1
if ( $inst1 == EPN ) then
   set elo=12000
   set ehi=14000
 echo "Creating high energy count rates using region image counts...(${elo}-${ehi})\n"
 evselect -w 0 -V 0 table=${inst1}_cleanw.fits $parimages imageset=temp_src.fits expression="(PI in [${elo}:${ehi}])&&(FLAG==0)&&(PATTERN<=4)&&${sourcefilter1}" withzcolumn=Y withzerrorcolumn=N 
 evselect -w 0 -V 0 table=${inst1}_bgcleanw.fits $parimages imageset=temp_bg_src.fits expression="(PI in [${elo}:${ehi}])&&(FLAG==0)&&(PATTERN<=4)&&${sourcefilter1}" withzcolumn=Y withzerrorcolumn=N  
else 
   set elo=10000
   set ehi=12000
echo "Creating high energy count rates using region image counts...(${elo}-${ehi})\n"
 evselect -w 0 -V 0 table=${inst1}_cleanw.fits $parimages imageset=temp_src.fits expression="(PI in [${elo}:${ehi}])&&(FLAG==0)&&(PATTERN<=12)&&${sourcefilter1}" withzcolumn=Y withzerrorcolumn=N
 evselect -w 0 -V 0 table=${inst1}_bgcleanw.fits $parimages imageset=temp_bg_src.fits expression="(PI in [${elo}:${ehi}])&&(FLAG==0)&&(PATTERN<=12)&&${sourcefilter1}" withzcolumn=Y withzerrorcolumn=N 
endif

set srccts=`fimgstat temp_src.fits INDEF INDEF | grep sum | awk '{print $8}'`
set bkgcts=`fimgstat temp_bg_src.fits INDEF INDEF | grep sum | awk '{print $8}'`
set livetime_bg=`fkeyprint "${inst1}_bgcleanw.fits[EVENTS]" LIVETIME | grep "Live time" | awk '{print $2}'`
set livetime_src=`fkeyprint "${inst1}_cleanw.fits[EVENTS]" LIVETIME | grep "Live time" | awk '{print $2}'`


set HEratio1=`echo $srccts $bkgcts $livetime_src $livetime_bg | awk '{print ($1*$4)/($2*$3)}'`

#inst2
if ( $inst2 == EPN ) then
   set elo=12000
   set ehi=14000
 echo "Creating high energy count rates using region image counts...(${elo}-${ehi})\n"
 evselect -w 0 -V 0 table=${inst2}_cleanw.fits $parimages imageset=temp_src.fits expression="(PI in [${elo}:${ehi}])&&(FLAG==0)&&(PATTERN<=4)&&${sourcefilter2}" withzcolumn=Y withzerrorcolumn=N
 evselect -w 0 -V 0 table=${inst2}_bgcleanw.fits $parimages imageset=temp_bg_src.fits expression="(PI in [${elo}:${ehi}])&&(FLAG==0)&&(PATTERN<=4)&&${sourcefilter2}" withzcolumn=Y withzerrorcolumn=N 
else 
   set elo=10000
   set ehi=12000
echo "Creating high energy count rates using region image counts...(${elo}-${ehi})\n"
 evselect -w 0 -V 0 table=${inst2}_cleanw.fits $parimages imageset=temp_src.fits expression="(PI in [${elo}:${ehi}])&&(FLAG==0)&&(PATTERN<=12)&&${sourcefilter2}" withzcolumn=Y withzerrorcolumn=N
 evselect -w 0 -V 0 table=${inst2}_bgcleanw.fits $parimages imageset=temp_bg_src.fits expression="(PI in [${elo}:${ehi}])&&(FLAG==0)&&(PATTERN<=12)&&${sourcefilter2}" withzcolumn=Y withzerrorcolumn=N 
endif

set srccts=`fimgstat temp_src.fits INDEF INDEF | grep sum | awk '{print $8}'`
set bkgcts=`fimgstat temp_bg_src.fits INDEF INDEF | grep sum | awk '{print $8}'`
set livetime_bg=`fkeyprint "${inst2}_bgcleanw.fits[EVENTS]" LIVETIME | grep "Live time" | awk '{print $2}'`
set livetime_src=`fkeyprint "${inst2}_cleanw.fits[EVENTS]" LIVETIME | grep "Live time" | awk '{print $2}'`


set HEratio2=`echo $srccts $bkgcts $livetime_src $livetime_bg | awk '{print ($1*$4)/($2*$3)}'`

#inst3
if ( $inst3 == EPN ) then
    set elo=12000
    set ehi=14000
 echo "Creating high energy count rates using region image counts...(${elo}-${ehi})\n"
 evselect -w 0 -V 0 table=${inst3}_cleanw.fits $parimages imageset=temp_src.fits expression="(PI in [${elo}:${ehi}])&&(FLAG==0)&&(PATTERN<=4)&&${sourcefilter3}" withzcolumn=Y withzerrorcolumn=N
 evselect -w 0 -V 0 table=${inst3}_bgcleanw.fits $parimages imageset=temp_bg_src.fits expression="(PI in [${elo}:${ehi}])&&(FLAG==0)&&(PATTERN<=4)&&${sourcefilter3}" withzcolumn=Y withzerrorcolumn=N 
else 
   set elo=10000
   set ehi=12000
echo "Creating high energy count rates using region image counts...(${elo}-${ehi})\n"
 evselect -w 0 -V 0 table=${inst3}_cleanw.fits $parimages imageset=temp_src.fits expression="(PI in [${elo}:${ehi}])&&(FLAG==0)&&(PATTERN<=12)&&${sourcefilter3}" withzcolumn=Y withzerrorcolumn=N
 evselect -w 0 -V 0 table=${inst3}_bgcleanw.fits $parimages imageset=temp_bg_src.fits expression="(PI in [${elo}:${ehi}])&&(FLAG==0)&&(PATTERN<=12)&&${sourcefilter3}" withzcolumn=Y withzerrorcolumn=N 
endif

set srccts=`fimgstat temp_src.fits INDEF INDEF | grep sum | awk '{print $8}'`
set bkgcts=`fimgstat temp_bg_src.fits INDEF INDEF | grep sum | awk '{print $8}'`
set livetime_bg=`fkeyprint "${inst3}_bgcleanw.fits[EVENTS]" LIVETIME | grep "Live time" | awk '{print $2}'`
set livetime_src=`fkeyprint "${inst3}_cleanw.fits[EVENTS]" LIVETIME | grep "Live time" | awk '{print $2}'`


set HEratio3=`echo $srccts $bkgcts $livetime_src $livetime_bg | awk '{print ($1*$4)/($2*$3)}'`

echo "HE count rate ratio for ${inst1} is ${HEratio1} \n"
echo "HE count rate ratio for ${inst2} is ${HEratio2} \n"
echo "HE count rate ratio for ${inst3} is ${HEratio3} \n"

exit

#--------------------------------------------------------------------------------

# 4. Extract spectra for each camera. Plot these at high energies, and compare
# count rates in this regime. Use weighted spectra scaling by c/s

#inst1
if ( ${inst1} == EPN ) then
 set pat="sd"
else
 set pat="sdtq"
endif

/data1/rft/rftscripts/xmm_specgen.csh ${inst1}_cleanw.fits ${reg1} ${inst1}_bgcleanw.fits N N atthk.dat Y Y 0 ${pat} ${inst1} -s${HEratio1} -vY


#inst2
if ( ${inst2} == EPN ) then
 set pat="sd"
else
 set pat="sdtq"
endif
/data1/rft/rftscripts/xmm_specgen.csh ${inst2}_cleanw.fits ${reg2} ${inst2}_bgcleanw.fits N N atthk.dat Y Y 0 ${pat} ${inst2} -s${HEratio2} -vY


#inst3
if ( ${inst3} == EPN ) then
 set pat="sd"
else
 set pat="sdtq"
endif
/data1/rft/rftscripts/xmm_specgen.csh ${inst3}_cleanw.fits ${reg3} ${inst3}_bgcleanw.fits N N atthk.dat Y Y 0 ${pat} ${inst3} -s${HEratio3} -vY


#Generate rmf and arf files. Ignore this section but leave in incase I need it
goto ignorermf

if ( $inst1} == EPN ) then
  rmfgen spectrumset=sp_s_${inst1}.fits rmfset=sp_s_${inst1}.rmf 
  arfgen arfset=sp_s_${inst1}.arf spectrumset=sp_s_${inst1}.fits withbadpixcorr=N modelee=N withdetbounds=Y filterdss=N detmaptype=flat detxbins=1 detybins=1 withsourcepos=Y sourcecoords=tel sourcex=0 sourcey=0
else 
  rmfgen spectrumset=sp_s_${inst1}.fits rmfset=sp_s_${inst1}.rmf detmaptype=flat
  arfgen arfset=sp_s_${inst1}.arf spectrumset=sp_s_${inst1}.fits withbadpixcorr=N modelee=N withdetbounds=Y filterdss=N detmaptype=flat detxbins=1 detybins=1 withsourcepos=Y sourcecoords=tel sourcex=0 sourcey=0
endif

if ( $inst2} == EPN ) then
  rmfgen spectrumset=sp_s_${inst2}.fits rmfset=sp_s_${inst2}.rmf 
  arfgen arfset=sp_s_${inst2}.arf spectrumset=sp_s_${inst2}.fits withbadpixcorr=N modelee=N withdetbounds=Y filterdss=N detmaptype=flat detxbins=1 detybins=1 withsourcepos=Y sourcecoords=tel sourcex=0 sourcey=0
else 
  rmfgen spectrumset=sp_s_${inst2}.fits rmfset=sp_s_${inst2}.rmf detmaptype=flat
  arfgen arfset=sp_s_${inst2}.arf spectrumset=sp_s_${inst2}.fits withbadpixcorr=N modelee=N withdetbounds=Y filterdss=N detmaptype=flat detxbins=1 detybins=1 withsourcepos=Y sourcecoords=tel sourcex=0 sourcey=0
endif

if ( $inst3} == EPN ) then
  rmfgen spectrumset=sp_s_${inst3}.fits rmfset=sp_s_${inst3}.rmf 
  arfgen arfset=sp_s_${inst3}.arf spectrumset=sp_s_${inst3}.fits withbadpixcorr=N modelee=N withdetbounds=Y filterdss=N detmaptype=flat detxbins=1 detybins=1 withsourcepos=Y sourcecoords=tel sourcex=0 sourcey=0
else
  rmfgen spectrumset=sp_s_${inst3}.fits rmfset=sp_s_${inst3}.rmf detmaptype=flat
  arfgen arfset=sp_s_${inst3}.arf spectrumset=sp_s_${inst3}.fits withbadpixcorr=N modelee=N withdetbounds=Y filterdss=N detmaptype=flat detxbins=1 detybins=1 withsourcepos=Y sourcecoords=tel sourcex=0 sourcey=0 
endif

ignorermf:

#step3:

#5. Subtract one from the other. 
#Need to use mathpha to do this successfully. 
#subtract bg from source. Errors are propagated using Gehrels algorithm
#

#inst1
if ( -e ${inst1}_res.fits ) then
rm ${inst1}_res.fits
endif
mathpha expr="sp_s_${inst1}.fits-sp_scb_${inst1}.fits" units='RATE' outfil="${inst1}_res.fits" exposure='NULL' areascal='NULL' ncomments=0
#inst2
if ( -e ${inst2}_res.fits ) then
rm ${inst2}_res.fits
endif
mathpha expr="sp_s_${inst2}.fits-sp_scb_${inst2}.fits" units='RATE' outfil="${inst2}_res.fits" exposure='NULL' areascal='NULL' ncomments=0
#inst3
if ( -e ${inst3}_res.fits ) then
rm ${inst3}_res.fits
endif
mathpha expr="sp_s_${inst3}.fits-sp_scb_${inst3}.fits" units='RATE' outfil="${inst3}_res.fits" exposure='NULL' areascal='NULL' ncomments=0

#and rebin spectrum, rmf and associate rmf/arf with input file. 

if ( -e ${inst1}_res${binsize1}.fits ) then
rm ${inst1}_res${binsize1}.fits ${inst1}_res${binsize1}.rmf
endif
if ( -e ${inst2}_res${binsize2}.fits ) then
rm ${inst2}_res${binsize2}.fits ${inst2}_res${binsize2}.rmf
endif
if ( -e ${inst3}_res${binsize3}.fits ) then
rm ${inst3}_res${binsize3}.fits ${inst3}_res${binsize3}.rmf
endif
#inst1
rbnpha infile=${inst1}_res.fits outfile=${inst1}_res${binsize1}.fits fchan=0 finchan=${binsize1} cmpmode=linear
rbnrmf infile=sp_s_${inst1}.rmf fchan=0 nchan=${binsize1} cmpmode=linear outfile=${inst1}_res${binsize1}.rmf
fparkey ${inst1}_res${binsize1}.rmf ${inst1}_res${binsize1}.fits RESPFILE
fparkey sp_s_${inst1}.arf ${inst1}_res${binsize1}.fits ANCRFILE
#inst2
rbnpha infile=${inst2}_res.fits outfile=${inst2}_res${binsize2}.fits fchan=0 finchan=${binsize2} cmpmode=linear
rbnrmf infile=sp_s_${inst2}.rmf fchan=0 nchan=${binsize2} cmpmode=linear outfile=${inst2}_res${binsize2}.rmf
fparkey ${inst2}_res${binsize2}.rmf ${inst2}_res${binsize2}.fits RESPFILE
fparkey sp_s_${inst2}.arf ${inst2}_res${binsize2}.fits ANCRFILE
#inst3
rbnpha infile=${inst3}_res.fits outfile=${inst3}_res${binsize3}.fits fchan=0 finchan=${binsize3} cmpmode=linear
rbnrmf infile=sp_s_${inst3}.rmf fchan=0 nchan=${binsize3} cmpmode=linear outfile=${inst3}_res${binsize3}.rmf
fparkey ${inst3}_res${binsize3}.rmf ${inst3}_res${binsize3}.fits RESPFILE
fparkey sp_s_${inst3}.arf ${inst3}_res${binsize3}.fits ANCRFILE

#rebin progenitor spectra and associate approproate files. 

if ( -e sp_s_${inst1}_${binsize1}.fits ) then
rm sp_s_${inst1}_${binsize1}.fits 
endif
if ( -e sp_s_${inst2}_${binsize2}.fits ) then
rm sp_s_${inst2}_${binsize2}.fits 
endif
if ( -e sp_s_${inst3}_${binsize3}.fits ) then
rm sp_s_${inst3}_${binsize3}.fits 
endif
#inst1
rbnpha infile=sp_s_${inst1}.fits outfile=sp_s_${inst1}_${binsize1}.fits fchan=0 finchan=${binsize1} cmpmode=linear
fparkey ${inst1}_res${binsize1}.rmf sp_s_${inst1}_${binsize1}.fits RESPFILE
fparkey sp_s_${inst1}.arf sp_s_${inst1}_${binsize1}.fits ANCRFILE
#inst2
rbnpha infile=sp_s_${inst2}.fits outfile=sp_s_${inst2}_${binsize2}.fits fchan=0 finchan=${binsize2} cmpmode=linear
fparkey ${inst2}_res${binsize2}.rmf sp_s_${inst2}_${binsize2}.fits RESPFILE
fparkey sp_s_${inst2}.arf sp_s_${inst2}_${binsize2}.fits ANCRFILE
#inst3
rbnpha infile=sp_s_${inst3}.fits outfile=sp_s_${inst3}_${binsize3}.fits fchan=0 finchan=${binsize3} cmpmode=linear
fparkey ${inst3}_res${binsize3}.rmf sp_s_${inst3}_${binsize3}.fits RESPFILE
fparkey sp_s_${inst3}.arf sp_s_${inst3}_${binsize3}.fits ANCRFILE

#repeat for the background
if ( -e sp_scb_${inst1}_${binsize1}.fits ) then
rm sp_scb_${inst1}_${binsize1}.fits 
endif
if ( -e sp_scb_${inst2}_${binsize2}.fits ) then
rm sp_scb_${inst2}_${binsize2}.fits 
endif
if ( -e sp_scb_${inst3}_${binsize3}.fits ) then
rm sp_scb_${inst3}_${binsize3}.fits 
endif
#inst1
rbnpha infile=sp_scb_${inst1}.fits outfile=sp_scb_${inst1}_${binsize1}.fits fchan=0 finchan=${binsize1} cmpmode=linear
fparkey ${inst1}_res${binsize1}.rmf sp_scb_${inst1}_${binsize1}.fits RESPFILE
fparkey sp_s_${inst1}.arf sp_scb_${inst1}_${binsize1}.fits ANCRFILE
#inst2
rbnpha infile=sp_scb_${inst2}.fits outfile=sp_scb_${inst2}_${binsize2}.fits fchan=0 finchan=${binsize2} cmpmode=linear
fparkey ${inst2}_res${binsize2}.rmf sp_scb_${inst2}_${binsize2}.fits RESPFILE
fparkey sp_s_${inst2}.arf sp_scb_${inst2}_${binsize2}.fits ANCRFILE
#inst3
rbnpha infile=sp_scb_${inst3}.fits outfile=sp_scb_${inst3}_${binsize3}.fits fchan=0 finchan=${binsize3} cmpmode=linear
fparkey ${inst3}_res${binsize3}.rmf sp_scb_${inst3}_${binsize3}.fits RESPFILE
fparkey sp_s_${inst3}.arf sp_scb_${inst3}_${binsize3}.fits ANCRFILE

echo "Residual spectrum for ${src1} is ${inst1}_res_${binsize1}.fits,"
echo "with linked rmf and arf files (source minus background).\n" 

echo "Progenitor spectrum for source dataset is sp_s_${inst1}_${binsize1}.fits"
echo "rmf and arf files are associated with spectrum.\n"

echo "Progenitor spectrum for bg dataset is sp_scb_${inst1}_${binsize1}.fits"
echo "rmf and arf files are associated with spectrum.\n"

beep
beep
beep

