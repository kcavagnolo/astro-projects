#! /bin/tcsh
#
#xmm_specgen.csh
#
#    This script is going to read in an events file and either a 
#    blank sky background or a local region file. Filtering
#    on PATTERN and FLAG will occur in the script. At the moment,
#    all cleaned data sets should be flare cleaned only. This script 
#    is a combination of BJM's script xmmspec.csh and AMR's scripts 
#    createspectra and double subtraction. 
#
#    Please ensure that your background data has been skycasted!
#
#    Version 1.0   RFT 01/02/2005 
#    Version 1.1   RFT 25/07/2005   Just realised that the flags for
#                                   vignetting dont actually do anything. 
#                                   Going to use evigweight (crudely)
#                                   on the events and bg data so that 
#                                   they will have the same response. 
#    Version 1.2   RFT 10/11/05     Enabling no background to be input. 
#    Version 1.3   RFT 14/11/05     added detmaptype=flat into rmfgen. 
#                                   at the moment this is a permanent option
#                                   will have to change in script for a 
#                                   point source. 
#    Version 1.4   RFT 18/01/06     revising the double subtraction part.
#                                   I am unconvinced by the calculation of
#                                   the 'soft excess' as done in AMR's
#                                   double subtraction script. 
#    Version 1.5   RFT 06/02/06     adding another parameter to input args.    #         
#                                   If the data is already vignetting 
#                                   corrected then need to set up a flag
#                                   so that arfgen is calculated differently.
#
#    Version 1.6   RFT 20/05/06     Made a correction to the vignetting 
#                                   calculation. This means that spectra are
#                                   calculated using RATE instead of COUNTS. 
#                                   This is not a major problem, but
#                                   requires some fine tuning of the
#                                   background scaling part of the program. 
#
#    Version 1.7   RFT 07/09/06     Corrected backscale calc, soft spec calc.
#                                   Also added a list of what all the files do
#                                   at the bottom for clarity.
#
#    Version 1.8   RFT 26/10/06     Filtering on FLAG==0 not XMMEA_EM etc
#
#
#    Uses xmmstring.csh
#
#    Need to sort out argument options. It is crashing if too few args. 
#    need to get it to run with no background i.e. have option "none"



set version=1.4

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
#need to sort out end triggers
if ( $#argv < 11 ) then
cat <<EOF

Use xmm_specgen.csh events srcreg.reg blank.fits dbs vig atthk.dat rmfgen arfgen 0 sdtq mos1 -s0.01 -rann.reg -g20 -vY

    events	- The events data for your observation. This should
                  have already been cleaned for flaring. 

    srcreg	- This is the source region file in which you want 
                  to extract your source spectrum. Must be in CIAO 
                  format (*.reg)

    bgfile      - This is either a CIAO region file(*.reg) such that a
                  local background is to be used for your spectra. 
                  This option can also be a blank sky background file. 
                  If the latter is the case then several options need
                  to be input (see below). Can also be "none". 

    dbs         - Perform double subtraction (Y/N). Cannot be Y if 
                  local background is used.

    vig         - Use evigweight to correct your data from vignetting. (Y/N)

    atthk       - The attitude housekeeping file for your observation

    rmfgen      - calculate rmf? (Y/N) Use a precanned rmf? (C)

    arfgen      - calculate arf? (Y/N) (also calculates BACKSCALE)

    events2     - Events file pre source removal. (0 if non-existent) 
                  (not working yet)
    pattern     - PATTERN code for your observation. The options are 
                  as follows: 
		  (s,d,t,q,sd,sdtq - singles, doubles, triples, quads)

    root        - Root output file name for your data

------------------Optional Arguments-(in any order)-----------------

    scale       - Scaling ratio for your observation. This can be
                  determined from the output for xmm_clean.csh.
                  Must begin with -s e.g. -s1.02

    highRann    - High radius annulus in which the double subtraction
                  will be performed. 
                  Must begin with -r e.g. -rann.reg

    group       - If you want your spectra to be grouped for XSPEC fitting
                  Must begin with -g e.g. -g20
    vigflag     - Is data already corrected for vignetting using evigweight? 
                  Cannot be y/Y if vig=y/Y. arfgen is calculated differently
 		  (see threads online at 
		  http://xmm.vilspa.esa.es/external/xmm_sw_cal/sas.shtml )
                  Must begin with -v e.g. -vY
 


Output files are as follows: (*=root)(**=group number)(^=only if blank sky background used)

sp_s_*.fits     : source spectrum
sp_s_*.rmf      : rmf corresponding to source region
sp_s_*.arf      : arf corresponding to source region
sp_b_*.fits     : background spectrum
sp_scb_*.fits   : scaled background spectrum ^   
sp_bs_*.fits    : background subtracted spectrum ^ 
                  (source - minus scaled background)
sp_dbs_*.fits   : double subtracted background spectrum ^ 
                  (source - background - soft excess)
sp_grp**_*.fits : grouped spectral data for fitting with XSPEC
                  (if option selected)

EOF
exit
endif

set events=$1
set srcreg=$2
set back=$3
set dbs=$4
set vig=$5
set att=$6
set rmf=$7
set arf=$8
set events2=$9
set pattern=$10
set root=$11

#SAS started?
if (! $?SAS_DIR) then
  echo "It looks like XMMSAS has not been started\n"
  echo "Exiting ...."
endif
#FTOOLS started?
if ( ! $?FTOOLS) then
  echo "It looks like FTOOLS has not been started\n"
  echo "Exiting ..."
endif

#check input files exist
if ( ! -e $events ) then
 echo "Error: $events does not exist"
 exit
endif

if ( ! -e $srcreg ) then
 echo "Error: $srcreg does not exist"
 exit
endif

if ( $back != none ) then
 if ( ! -e $back ) then
  echo "Error: $back does not exist"
  exit 
 endif
endif

if ( $events2 != 0 ) then
 if ( ! -e $events2 ) then
  echo "Error: $events2 does not exist"
  exit
 endif
endif

if ( ! -e $att ) then
 echo "Error: $att does not exist"
 exit
endif

#sort out optional arguments

set scale=0
set HRann=0
set group=0
set vigflag=0

set i=12
while ( $i <= $#argv )
 set c=`echo $argv[${i}] | cut -c1`
 if ( $c != - ) then
  echo "ERROR: argument $i doesn't begin with a -\n"
  exit
 endif
 set type=`echo $argv[${i}] | cut -c2`
 if (( $type != s )&&( $type != r )&&( $type != g )&&( $type != v )) then
  echo "ERROR: argument $i is not of type s r g or v\n"
  echo $type $argv[${i}]
  exit
 endif

 set arg=`echo $argv[${i}] | tail +3c`

  if ( $type == s ) then
   set scale=$arg
  else if ( $type == r ) then
   set HRann=$arg
  else if ( $type == g ) then
   set group=$arg
  else if ( $type == v ) then
   set vigflag=$arg
  endif #arg type
 endif #if $c!= '-'
@ i++
end

if ( $HRann != 0 ) then
 if ( ! -e $HRann ) then
   echo "Error: $HRann does not exist"
   exit
 endif
endif

#determine background type (region or blank backgroud or none.)

if ( $back != none ) then
 set exp=`echo $back | tail -4c`
 if ( $exp == reg) then
 set bgfilt=local
 else
 set bgfilt=blank
 endif
else 
 set bgfilt=none
endif

#Check scaling ratio for blank sky background
if ( $bgfilt == blank) then
  if ( $scale == 0 ) then
  echo "No scaling ratio (Ev/Bg) supplied.....exiting"
  exit
  endif
endif

#Check region files exist for double subtraction
if ( $dbs == Y || $dbs == y ) then
   if ( $HRann == 0 ) then
   echo  "No high radius region file found for double subtraction.....exiting"
   exit
   endif 
endif

#Check vigflag settings
if ( $vigflag == Y || $vigflag == y ) then
   set vigflag=1
endif

#check regions are in ciao format

set rtype=`head -1 $srcreg | awk '{print $5}'`
if ( $rtype != CIAO ) then
 echo ERROR - REGION FILE "$srcreg" DOES NOT APPEAR TO BE IN CIAO FORMAT"\n"
 exit
endif

if ($bgfilt == local ) then
  set rtype=`head -1 $back | awk '{print $5}'`
  if ( $rtype != CIAO ) then
   echo ERROR - REGION FILE "$back" DOES NOT APPEAR TO BE IN CIAO FORMAT"\n"
   exit
  endif
endif

if ($HRann != 0 ) then
 set rtype=`head -1 $HRann | awk '{print $5}'`
  if ( $rtype != CIAO ) then
   echo ERROR - REGION FILE "$HRann" DOES NOT APPEAR TO BE IN CIAO FORMAT"\n"
  exit
 endif
endif


#identify calculation procedure

set rmfgen=false
if ($rmf == y || $rmf == Y ) then 
set rmfgen=true
endif
set rmfcal=false
if ($rmf == c || $rmf == C ) then
set rmfcal=true
endif

set vigcor=false
if ($vig == y || $vig == Y ) then
set vigcor=true
endif

set arfgen=false
if ($arf == y || $arf == Y ) then
set arfgen=true
endif
set arfcal=false
if ($arf == c || $arf == C ) then
set arfcal=true
endif

set dbscal=false
if ($dbs == y || $dbs == Y ) then
set dbscal=true
endif

#set pattern codes
set patexp="PATTERN==0"

if ($pattern == s) then 
   set patexp="PATTERN==0"
endif
if ($pattern == d) then
   set patexp="PATTERN>=1&&PATTERN<=4"
endif
if ($pattern == t) then
   set patexp="PATTERN>=5&&PATTERN<=8"
endif
if ($pattern == q) then
   set patexp="PATTERN>=9&&PATTERN<=12"
endif
if ($pattern == sd) then
   set patexp="PATTERN<=4"
endif
if ($pattern == sdtq) then
   set patexp="PATTERN<=12"
endif

#find instrument and set parameters
fdump $events tmpdmp1 TIME - prdata=no showcol=no showunit=no showrow=no clobber=yes
set instr=`grep INSTRUME < tmpdmp1 | head -1 | awk '{print $2}'`
set instr=`echo $instr | tr -d "'"`
echo 'Instrument is '$instr "\n"
if ($instr == EPN ) then
  set spchmax = 20475
  set spbinsize = 5
  set pnline = 190
else
  set spchmax = 11999
  set spbinsize  = 15
  set pnline = 0
endif

rm -fr tmpdmp1

#Alter pattern expression to account for flag and pattern
if ( $instr == EPN ) then
set flag="#XMMEA_EP" 
set flag="FLAG==0"
else
set flag="#XMMEA_EM"
set flag="FLAG==0"
endif

set expr="$flag&&$patexp"

#Does xmmstring exist?
set scriptpath=/data1/rft/rftscripts/
if ( ! -e ${scriptpath}xmmstring.csh ) then
set scriptpath=/exgal6/bjm/scripts/
endif

#Generate string expression for source region (and bg + HRann if needed)
set sourcefilter=`${scriptpath}xmmstring.csh $srcreg | grep expression | awk '{print $4}'`

if ( $bgfilt == local ) then
set bgfilter=`${scriptpath}xmmstring.csh $back | grep expression | awk '{print $4}'`
endif

if ( $HRann != 0 ) then 
set HRfilter=`${scriptpath}xmmstring.csh $HRann | grep expression | awk '{print $4}'`
endif
rm *_filter.txt*


#Set up parspec and parimage
set parspec="withspectrumset=Y spectralbinsize=$spbinsize energycolumn=PI specchannelmax=$spchmax specchannelmin=0 withspecranges=Y" 
set parimage="withimageset=Y xcolumn=DETX ycolumn=DETY ximagebinsize=40 yimagebinsize=40 imagedatatype=Real32 squarepixels=Y imagebinning=binSize withimagedatatype=Y ximagesize=1020 yimagesize=1020 keepfilteroutput=N withxranges=Y withyranges=Y ximagemin=-20399 yimagemin=-20399 ximagemax=20400 yimagemax=20400"

#set verbosity and warning surpression options

set V=0
set w=0

#Set vignetting flags for local background only. 
cp $events tempEv00

if ( $vigcor == true ) then
echo "Correcting events data for vignetting...\n" 
evigweight ineventset=$events outeventset=tempEv00
set vigflag=1
endif

#Set vignetting weights for bs bg. 
if ( $bgfilt == blank ) then
   cp $back temBEv00
   if ( $vigcor == true ) then
      echo "Correcting bg events data for vignetting...\n"
      evigweight ineventset=$back outeventset=tempBEv00
   endif
endif

#Filtering source data on pattern and flag....
echo "Filtering source data on pattern and flag....\n"
echo evselect table=tempEv00 filteredset=tempEv01 expression="$expr" updateexposure=N writedss=Y withfilteredset=T destruct=Y keepfilteroutput=T "\n"
evselect -w $w -V $V table=tempEv00 filteredset=tempEv01 expression="$expr" updateexposure=N writedss=Y withfilteredset=T destruct=Y keepfilteroutput=T 

#Extract source spectrum
 echo "Extracting source spectrum.....\n"
if ( $vigflag == 1 ) then
 echo evselect table=tempEv01 spectrumset=tempsp01 expression=$sourcefilter $parspec  zcolumn=Y zerrorcolumn=N "\n"
 evselect table=tempEv01 spectrumset=tempsp01 expression=$sourcefilter $parspec withzcolumn=Y withzerrorcolumn=N 
else
 echo evselect table=tempEv01 spectrumset=tempsp01 expression=$sourcefilter $parspec "\n"
 evselect table=tempEv01 spectrumset=tempsp01 expression=$sourcefilter $parspec
endif


#Filtering background data on pattern and flag....(if necessary)

if ( $bgfilt != none) then
 if ( $bgfilt == blank ) then
  echo "Filtering background data on pattern and flag...\n"
  echo evselect table=temBEv00 filteredset=tempEv02 expression="$expr" updateexposure=N writedss=Y withfilteredset=T destruct=Y keepfilteroutput=T "\n"
  evselect -w $w -V $V table=temBEv00 filteredset=tempEv02 expression="$expr" updateexposure=N writedss=Y withfilteredset=T destruct=Y keepfilteroutput=T 
  if ( $vigflag == 1 ) then
   echo "Extracting background spectrum....\n"
   echo evselect table=tempEv02 spectrumset=tempsp02 expression=$sourcefilter $parspec withzcolumn=Y withzerrorcolumn=N"\n"
   evselect table=tempEv02 spectrumset=tempsp02 expression=$sourcefilter $parspec withzcolumn=Y withzerrorcolumn=N 
  else
   echo "Extracting background spectrum....\n"
   echo evselect table=tempEv02 spectrumset=tempsp02 expression=$sourcefilter $parspec "\n"
   evselect table=tempEv02 spectrumset=tempsp02 expression=$sourcefilter $parspec 
  endif #vigflag==1
 else
  if ( $vigflag == 1) then
   echo "Extracting background spectrum....\n"
   echo evselect table=tempEv00 spectrumset=tempsp02 expression=$bgfilter $parspec "\n"
   evselect table=tempEv00 spectrumset=tempsp02 expression=$bgfilter $parspec withzcolumn=Y withzerrorcolumn=N 
  else
   echo "Extracting background spectrum....\n"
   echo evselect table=tempEv00 spectrumset=tempsp02 expression=$bgfilter $parspec "\n"
   evselect table=tempEv00 spectrumset=tempsp02 expression=$bgfilter $parspec 
  endif
 endif # bgfile == blank
endif


#Accounting for pre-source subtracted events file
if ( $events2 != 0 ) then
echo "Creating image from pre and post-source subtracted Events files....\n"
#not bothering with changing DET values (yet)

evselect -w $w -V $V table=$events2 filteredset=tempEv03 expression="$expr" updateexposure=N writedss=Y withfilteredset=T destruct=Y keepfilteroutput=T 

#Extract source image
echo "Extracting source image....(pre-source removal)\n"
evselect -w $w -V $V table=tempEv03 imageset=tempim02 expression=$sourcefilter $parimage
echo "Extracting source image....(post-source removal)\n"
evselect -w $w -V $V table=tempEv01 imageset=tempim01 expression=$sourcefilter $parimage

farith tempim02 tempim01 tempim03 SUB clobber=yes
fcarith tempim03 1.0E-20 tempim04 ADD clobber=yes
farith tempim03 tempim04 tempim05 DIV clobber=yes
set srcpixno = `fimgstat tempim05 0 1 | grep sum | awk '{print $8}'`
echo $srcpixno
set srcpixno = `echo $srcpixno 1600 | awk '{print $1*$2}'`

endif

cp tempsp01 sp_s_${root}.fits
if ( $bgfilt != none ) then
 cp tempsp02 sp_b_${root}.fits
endif
#Scaling background (if blank sky used )
#No need to subtract bg here. output file is pretty meaningless. 
#not going to erase as it is used later on. can delete at the end

if ($bgfilt == blank ) then

 if ( $vigflag == 1 ) then
  echo "Scaling and subtracting background....\n"
  punlearn fcalc
  punlearn fdelcol
  fcalc tempsp02 tempsp03 RATE2 "RATE * $scale" clobber=yes
  fcalc tempsp03 tempsp06 RATE "RATE2" clobber=yes
  fdelcol tempsp06+1 RATE2 N Y
  fdelcol tempsp03+1 RATE N Y
  faddcol tempsp03 tempsp01 RATE
  fcalc tempsp03 tempsp04 RATE1 "RATE" clobber=yes
  fdelcol tempsp04+1 RATE N Y
  fcalc tempsp04 tempsp05 RATE "RATE1 - RATE2" clobber=yes
  fdelcol tempsp05+1 RATE1 N Y 
  fdelcol tempsp05+1 RATE2 N Y
  cp tempsp05 sp_bs_${root}.fits
  cp tempsp06 sp_scb_${root}.fits
 else
  echo "Scaling and subtracting background....\n"
  punlearn fcalc
  punlearn fdelcol
  fcalc tempsp02 tempsp03 COUNTS2 "COUNTS * $scale" clobber=yes
  fcalc tempsp03 tempsp06 COUNTS "COUNTS2" clobber=yes
  fdelcol tempsp06+1 COUNTS2 N Y
  fdelcol tempsp03+1 COUNTS N Y
  faddcol tempsp03 tempsp01 COUNTS
  fcalc tempsp03 tempsp04 COUNTS1 "COUNTS" clobber=yes
  fdelcol tempsp04+1 COUNTS N Y
  fcalc tempsp04 tempsp05 COUNTS "COUNTS1 - COUNTS2" clobber=yes
  fdelcol tempsp05+1 COUNTS1 N Y 
  fdelcol tempsp05+1 COUNTS2 N Y
  cp tempsp05 sp_bs_${root}.fits
  cp tempsp06 sp_scb_${root}.fits
 endif # vigflag == 1

endif

#Generating rmf.....

set extended=Y

if ( $rmfgen == true ) then
 echo "Calculating rmf....\n"
 if ( $extended == Y ) then
  echo "rmfgen -w $w -V $V spectrumset=sp_s_${root}.fits rmfset=sp_s_${root}.rmf detmaptype=flat"
  rmfgen -w $w -V $V spectrumset=sp_s_${root}.fits rmfset=sp_s_${root}.rmf detmaptype=flat
 else
  rmfgen -w $w -V $V spectrumset=sp_s_${root}.fits rmfset=sp_s_${root}.rmf
 endif
endif

if ( $rmfcal == true ) then
 echo "Calculating rmf from canned response files....\n"
 #Finding raw y coords
 #Finding centre of source 

 set x=`head -2 $srcreg | tail -1 | cut -d"(" -f2 | cut -d"," -f1`
 set y=`head -2 $srcreg | tail -1 | cut -d"(" -f2 | cut -d"," -f2`
 #echo "Centre of region is (x,y) = $x,$y - If the region is not simple i.e. a circle or an annulus, this might be incorrect.\n"

 #set rawy=`${scriptpath}xmmcoords.csh tempEv00 none sky $x $y | grep RAW | awk '{print $7}'`
 #echo $rawy
 #this bit is a bit screwed. xmmcoords.csh is horrifically out of date. Need to
 #have a look at this script and get it sorted. RT (03/02/05)

 #at the moment am going to use raw coordinates of 200. 
 echo "Using on-axis rmf. Is this what you want?" 
 set Y=9

#Getting files for each instrument...

 if ( $instr == EPN ) then
   echo "Using rmf ${SAS_CCFPATH}extras/responses/PN/epn_ff20_${pattern}Y${Y}_v6.7.rmf.gz "

   echo "creating link\n"

   set rmffile=sp_s_${root}.rmf
     if ( -e $rmffile ) then
      rm $rmffile
     endif

   cp ${SAS_CCFPATH}extras/responses/PN/epn_ff20_${pattern}Y${Y}_v6.7.rmf.gz sp_s_${root}.rmf.gz && gunzip sp_s_${root}.rmf.gz

 endif #for EPN

 if (( $instr == EMOS1 )||( $instr == EMOS2 )) then
#get revolution number
 set revol=`fkeyprint "tempEv00[EVENTS]" REVOLUT | grep Revolution | cut -d"=" -f2`
 set revol=`echo $revol | cut -c1-3 `
#decide which rmf to use?
#if revol.gt.534 then use most recent files, else use 
#the one closest to the revolution number.......
#calibrated revolution numbers are:
 set rev1=000
 set rev2=110
 set rev3=170 #actually 169 # just rounded up for calcs, 
 set rev4=256 #actually 255 # but won't affect results
 set rev5=310
 set rev6=330
 set rev7=360
 set rev8=440 #actually 439
 set rev9=534 

   if ( $revol >= 534 ) then
   set rev=$rev9
    else if (( $revol >= $rev1 )&&( $revol <= $rev2 )) then
    set temp1=`echo $rev2 $rev1 2 | awk '{print ($1-$2)/$3}'`
    set temp2=`echo $rev2 $revol | awk '{print $1-$2}'`
      if ( $temp2 <= $temp1 ) then
        set rev=$rev2
      else
        set rev=$rev1
      endif
    else if (( $revol > $rev2 )&&( $revol <= $rev3 )) then
    set temp1=`echo $rev3 $rev2 2 | awk '{print ($1-$2)/$3}'`
    set temp2=`echo $rev3 $revol | awk '{print $1-$2}'`
       if ( $temp2 <= $temp1 ) then
         set rev=`echo $rev3 | awk '{print $1-1}'`
       else
         set rev=$rev2
       endif
    else if (( $revol >= $rev3 )&&( $revol <= $rev4 )) then
    set temp1=`echo $rev4 $rev3 2 | awk '{print ($1-$2)/$3}'`
    set temp2=`echo $rev4 $revol | awk '{print $1-$2}'`
       if ( $temp2 <= $temp1 ) then
         set rev=`echo $rev4 | awk '{print $1-1}'`
       else
         set rev=`echo $rev3 | awk '{print $1-1}'`
       endif
    else if (( $revol >= $rev4 )&&( $revol <= $rev5 )) then
    set temp1=`echo $rev5 $rev4 2 | awk '{print ($1-$2)/$3}'`
    set temp2=`echo $rev5 $revol | awk '{print $1-$2}'`
       if ( $temp2 <= $temp1 ) then
         set rev=$rev5
       else
         set rev=`$rev4 | awk '{print $1-1}'`
       endif
    else if (( $revol >= $rev5 )&&( $revol <= $rev6 )) then
    set temp1=`echo $rev6 $rev5 2 | awk '{print ($1-$2)/$3}'`
    set temp2=`echo $rev6 $revol | awk '{print $1-$2}'`
       if ( $temp2 <= $temp1 ) then
         set rev=$rev6
       else
         set rev=$rev5
       endif
    else if (( $revol >= $rev6 )&&( $revol <= $rev7 )) then
    set temp1=`echo $rev7 $rev6 2 | awk '{print ($1-$2)/$3}'`
    set temp2=`echo $rev7 $revol | awk '{print $1-$2}'`
       if ( $temp2 <= $temp1 ) then
         set rev=$rev7
       else
         set rev=$rev6
       endif
    else if (( $revol >= $rev7 )&&( $revol <= $rev8 )) then
    set temp1=`echo $rev8 $rev7 2 | awk '{print ($1-$2)/$3}'`
    set temp2=`echo $rev8 $revol | awk '{print $1-$2}'`
       if ( $temp2 <= $temp1 ) then
         set rev=`echo $rev8 | awk '{print $1-1}'`
       else
         set rev=$rev7
       endif
    else if (( $revol >= $rev8 )&&( $revol <= $rev9 )) then
    set temp1=`echo $rev9 $rev8 2 | awk '{print ($1-$2)/$3}'`
    set temp2=`echo $rev9 $revol | awk '{print $1-$2}'`
       if ( $temp2 <= $temp1 ) then
         set rev=$rev9
       else
         set rev=`echo $rev8 | awk '{print $1-1}'`
       endif

   endif #for getting revolution number

#setting pattern... Only have option of 0 or all. 
 if ( $pattern == s ) then
  set patno=p0
  else
  set patno=pall
 endif

#getting correct instrument
 set instmos=`echo $instr | cut -c5 `

 echo "Using rmf ${SAS_CCFPATH}extras/responses/MOS/m${instmos}_${rev}_im_${patno}_v1.2.rmf.gz\n"

 set rmffile=sp_s_${root}.rmf
 if ( -e $rmffile ) then
  rm $rmffile
 endif

 cp ${SAS_CCFPATH}extras/responses/MOS/m${instmos}_${rev}_im_${patno}_v1.2.rmf.gz sp_s_${root}.rmf.gz && gunzip sp_s_${root}.rmf.gz

 endif # for MOS


endif #(for canned rmf)

if (( ${rmfcal} == false )&&( ${rmfgen} == false )) then
 echo "Not generating an rmf...\n"
endif

#Generating arf files

if ( $vigflag == 1 ) then
 arfgen arfset=sp_s_${root}.arf spectrumset=sp_s_${root}.fits withbadpixcorr=N modelee=N withdetbounds=Y filterdss=N detmaptype=flat detxbins=1 detybins=1 withsourcepos=Y sourcecoords=tel sourcex=0 sourcey=0
else

 if ( $arfgen == true ) then
  echo "Creating detector map for arfgen....\n"
  evselect -w $w -V $V table=tempEv00 destruct=false withimageset=true imageset=detmap_${root}.fits xcolumn=DETX ycolumn=DETY imagebinning=binSize ximagebinsize=120 yimagebinsize=120 expression=$sourcefilter writedss=true updateexposure=true
  echo "Generating arf...\n"
  arfgen -w $w -V $V spectrumset=sp_s_${root}.fits arfset=sp_s_${root}.arf setbackscale=N withbadpixcorr=Y badpixlocation=tempEv00 extendedsource=Y detmaptype=dataset detmaparray=detmap_${root}.fits withrmfset=Y rmfset=sp_s_${root}.rmf withfilteredset=Y 
  echo "removing unwanted files.....\n"
  rm detmap_${root}.fits filteredpixellist.ds rawpixellist.ds BADPIX*
 endif #for creating arf
endif #vigcor arfgen


if ( $arfcal == true ) then
echo "Using canned rmf files...\n"
#although not sure how to do this
endif

if ( $arfgen == true ) then
echo "calculating BACKSCALE....\n"
punlearn fkeyprint
punlearn fparkey
 

backscale -V $V -w $w spectrumset=sp_s_${root}.fits withbadpixcorr=yes badpixlocation=tempEv00

fkeyprint sp_s_${root}.fits+1 BACKSCAL exact=yes outfile=tempba01 clobber=yes
set backscale=`grep BACKSCAL= < tempba01 | head -1 | awk '{print $2}'`
echo "Backscale for source spectrum:" $backscale 

 if ( $bgfilt == blank ) then

  if ( $events2 != 0 ) then
   set backsc_c = `echo $backscale $srcpixno | awk '{print $1-$2}'`
   echo "...minus sources/bad pixels newly subtracted..." $backsc_c
   fparkey $backsc_c sp_s_${root}.fits+1 BACKSCAL comm='Scaling factor for background' add=yes
   fparkey $backsc_c sp_b_${root}.fits+1 BACKSCAL comm='Scaling factor for background' add=yes
   fparkey $backsc_c sp_scb_${root}.fits+1 BACKSCAL comm='Scaling factor for background' add=yes
   fparkey $backsc_c sp_bs_${root}.fits+1 BACKSCAL comm='Scaling factor for background' add=yes 
  else
   fparkey $backscale sp_b_${root}.fits+1 BACKSCAL comm='Scaling factor for background' add=yes
   fparkey $backscale sp_scb_${root}.fits+1 BACKSCAL comm='Scaling factor for background' add=yes
   fparkey $backscale sp_bs_${root}.fits+1 BACKSCAL comm='Scaling factor for background' add=yes
  endif
  echo "$backscale added to spectra\n"
 #running backscale for local background spectrum
 else
  echo "Calculating BACKSCALE for local background spectrum...\n"
 backscale spectrumset=sp_b_${root}.fits withbadpixcorr=yes badpixlocation=$events

 endif
endif #(arfgen==true)

if ( $arfgen == false ) then
echo "Calculating BACKSCALE....\n"
punlearn fparkey
punlearn fkeyprint

backscale -V $V -w $w spectrumset=sp_s_${root}.fits withbadpixcorr=yes badpixlocation=tempEv00
fkeyprint sp_s_${root}.fits+1 BACKSCAL exact=yes outfile=tempba02 clobber=yes
set backscale=`grep BACKSCAL= < tempba02 | head -1 | awk '{print $2}'`
echo "Backscale for source spectrum:" $backscale 

 if ( $bgfilt == blank ) then

  if ( $events2 != 0 ) then
    set backsc_c = `echo $backscale $srcpixno | awk '{print $1-$2}'`
    echo "...minus sources/bad pixels newly subtracted..." $backsc_c
    fparkey $backsc_c sp_s_${root}.fits+1 BACKSCAL comm='Scaling factor for background' add=yes
    fparkey $backsc_c sp_b_${root}.fits+1 BACKSCAL comm='Scaling factor for background' add=yes
    fparkey $backsc_c sp_scb_${root}.fits+1 BACKSCAL comm='Scaling factor for background' add=yes
    fparkey $backsc_c sp_bs_${root}.fits+1 BACKSCAL comm='Scaling factor for background' add=yes 
  else
    fparkey $backscale sp_b_${root}.fits+1 BACKSCAL comm='Scaling factor for background' add=yes
    fparkey $backscale sp_scb_${root}.fits+1 BACKSCAL comm='Scaling factor for background' add=yes
    fparkey $backscale sp_bs_${root}.fits+1 BACKSCAL comm='Scaling factor for background' add=yes
  endif
  echo "Backscale added to spectra\n"

  else
  echo "Calculating BACKSCALE for local background spectrum...\n"
 backscale spectrumset=sp_b_${root}.fits withbadpixcorr=yes badpixlocation=$events

 endif
endif #arfgen=false

if ( $bgfilt == none ) then
 echo "No further processing as no background selected." 
 echo "Ignore error above. Exiting...\n"
 exit
endif


#Double subtraction
#Effectively have to run createspectra again, but no rmfgen this time
#arfgen wont be run either. Backscale will be calculated using the SAS task

if ( $dbscal == true ) then
 echo "Double subtracting data...\n"
 echo "Creating high radius spectra...."
#Filtering source data on pattern and flag....
 echo "Filtering source data on pattern and flag....\n"
#Extract source spectrum
 echo "Extracting high radius source spectrum.....\n"
   if ( $vigflag == 1 ) then
     echo evselect table=tempEv01 spectrumset=tempsp11 expression=$HRfilter $parspec withzcolumn=Y withzerrorcolumn=N  "\n"
    evselect table=tempEv01 spectrumset=tempsp11 expression=$HRfilter $parspec  withzcolumn=Y withzerrorcolumn=N 
    echo "Extracting high radius background spectrum....\n"
    echo evselect table=tempEv02 spectrumset=tempsp12 expression=$HRfilter $parspec  withzcolumn=Y withzerrorcolumn=N "\n"
    evselect table=tempEv02 spectrumset=tempsp12 expression=$HRfilter $parspec withzcolumn=Y withzerrorcolumn=N 
   else
    echo evselect table=tempEv01 spectrumset=tempsp11 expression=$HRfilter $parspec "\n"
    evselect table=tempEv01 spectrumset=tempsp11 expression=$HRfilter $parspec 
    echo "Extracting high radius background spectrum....\n"
    echo evselect table=tempEv02 spectrumset=tempsp12 expression=$HRfilter $parspec "\n"
    evselect table=tempEv02 spectrumset=tempsp12 expression=$HRfilter $parspec
   endif #vigflag ==1

#Accounting for pre-source subtracted events file
  if ( $events2 != 0 ) then
   echo "Creating image from pre and post-source subtracted Events files....\n"
#not bothering with changing DET values (yet)

#Extract source image
   echo "Extracting source image....(pre-source removal)\n"
   evselect -w $w -V $V table=tempEv03 imageset=tempim12 expression=$HRfilter $parimage
   echo "Extracting source image....(post-source removal)\n"
   evselect -w $w -V $V table=tempEv01 imageset=tempim11 expression=$HRfilter $parimage

   farith tempim12 tempim11 tempim13 SUB clobber=yes
   fcarith tempim13 1.0E-20 tempim14 ADD clobber=yes
   farith tempim13 tempim14 tempim15 DIV clobber=yes
   set srcpixno_dbs = `fimgstat tempim15 0 1 | grep sum | awk '{print $8}'`
   echo $srcpixno_dbs
   set srcpixno_dbs = `echo $srcpixno 1600 | awk '{print $1*$2}'`
  endif #events2 /=/0

 cp tempsp11 sp_s_dbs_${root}.fits
 cp tempsp12 sp_b_dbs_${root}.fits

#before scaling the bg, need another scaling ratio (one that is 
#of the order 0.01). 
#for this, I need to know the exposure times for each observation. 

set srcexpo=`fkeyprint tempsp11 EXPOSURE | awk '{print $2}' | cut -d ":" -f2`
set bgexpo=`fkeyprint tempsp12 EXPOSURE | awk '{print $2}' | cut -d ":" -f2`
set scl2=`echo $srcexpo $bgexpo | awk '{print $1/$2}'`
#Scaling background

 echo "Scaling and subtracting high radius background....\n"

   if ( $vigflag == 1 ) then
    set scl2=1
    punlearn fcalc
    punlearn fdelcol
    fcalc tempsp12 tempsp13 RATE2 "$scale * RATE" clobber=yes
    fcalc tempsp13 tempsp16 RATE "RATE2" clobber=yes
    fdelcol tempsp16+1 RATE2 N Y
    fdelcol tempsp13+1 RATE N Y
    faddcol tempsp13 tempsp11 RATE
    fcalc tempsp13 tempsp14 RATE1 "RATE / $scl2" clobber=yes
    fdelcol tempsp14+1 RATE N Y
    fcalc tempsp14 tempsp15 RATE "RATE1 - RATE2" clobber=yes
    fdelcol tempsp15+1 RATE1 N Y 
    fdelcol tempsp15+1 RATE2 N Y
    cp tempsp15 sp_bs_dbs_${root}.fits
    cp tempsp16 sp_scb_dbs_${root}.fits
   else
    punlearn fcalc
    punlearn fdelcol
    fcalc tempsp12 tempsp13 COUNTS2 "$scale * COUNTS" clobber=yes
    fcalc tempsp13 tempsp16 COUNTS "COUNTS2" clobber=yes
    fdelcol tempsp16+1 COUNTS2 N Y
    fdelcol tempsp13+1 COUNTS N Y
    faddcol tempsp13 tempsp11 COUNTS
    fcalc tempsp13 tempsp14 COUNTS1 "COUNTS / $scl2" clobber=yes
    fdelcol tempsp14+1 COUNTS N Y
    fcalc tempsp14 tempsp15 COUNTS "COUNTS1 - COUNTS2" clobber=yes
    fdelcol tempsp15+1 COUNTS1 N Y 
    fdelcol tempsp15+1 COUNTS2 N Y
    cp tempsp15 sp_bs_dbs_${root}.fits
    cp tempsp16 sp_scb_dbs_${root}.fits
   endif # vigflag ==1
#Calculating backscale for dbs data

 echo "Calculating BACKSCALE for high annulus data....\n"
 punlearn fparkey
 punlearn fkeyprint

 backscale -V $V -w $w spectrumset=sp_s_dbs_${root}.fits withbadpixcorr=yes badpixlocation=tempEv00
 fkeyprint sp_s_dbs_${root}.fits+1 BACKSCAL exact=yes outfile=tempba03 clobber=yes
 set backscal_dbs=`grep BACKSCAL= < tempba03 | head -1 | awk '{print $2}'`
 echo "Backscale for source spectrum:" $backscal_dbs 

  if ( $events2 != 0 ) then 
   set backsc_c_dbs = `echo $backscal_dbs $srcpixno_dbs | awk '{print $1-$2}'`
   echo "...minus sources/bad pixels newly subtracted..." $backsc_c
   fparkey $backsc_c_dbs sp_s_dbs_${root}.fits+1 BACKSCAL comm='Scaling factor for background' add=yes
   fparkey $backsc_c_dbs sp_b_dbs_${root}.fits+1 BACKSCAL comm='Scaling factor for background' add=yes
   fparkey $backsc_c_dbs sp_scb_dbs_${root}.fits+1 BACKSCAL comm='Scaling factor for background' add=yes
   fparkey $backsc_c_dbs sp_bs_dbs_${root}.fits+1 BACKSCAL comm='Scaling factor for background' add=yes 
  else
   fparkey $backscal_dbs sp_b_dbs_${root}.fits+1 BACKSCAL comm='Scaling factor for background' add=yes
   fparkey $backscal_dbs sp_scb_dbs_${root}.fits+1 BACKSCAL comm='Scaling factor for background' add=yes
   fparkey $backscal_dbs sp_bs_dbs_${root}.fits+1 BACKSCAL comm='Scaling factor for background' add=yes
  endif
 echo "Backscale added to high radius spectra\n"

 punlearn fkeyprint
 fkeyprint sp_s_${root}.fits+1 BACKSCAL exact=yes outfile=tempba04 clobber=yes
 set backscal_src=`grep BACKSCAL= < tempba04 | head -1 | awk '{print $2}'`
 echo "Backscale for source spectrum:" $backscal_src "\n"
 fkeyprint sp_scb_${root}.fits+1 BACKSCAL exact=yes outfile=tempba05 clobber=yes
 set backscal_bg=`grep BACKSCAL= < tempba05 | head -1 | awk '{print $2}'`
 echo "Backscale for scaled background spectrum:" $backscal_bg "\n"
 fkeyprint sp_bs_dbs_${root}.fits+1 BACKSCAL exact=yes outfile=tempba06 clobber=yes 
 set backscal_soft=`grep BACKSCAL= < tempba06 | head -1 | awk '{print $2}'`
 echo "Backscale for soft spectrum:" $backscal_soft

 set ratio=`echo $backscal_bg $backscal_soft | awk '{print $1/$2}'`
 echo "Soft spectrum area scaling:" $ratio "\n"
# echo $backscal_bg $backscal_soft
    

    cp sp_scb_${root}.fits tempsp21
    cp sp_bs_dbs_${root}.fits tempsp22 

#   going to alter tempsp22 to remove events from 2.0keV or higher. 
#   This will leave me with a true 'soft' spectrum. 
#   For PN: 2.0keV starts at channel: 937
#   Need to find the right ftools for editing the spectrum. 
#   the command is fmodtab tempsp22+1 RATE modfile
#   Now I just need to create a modfile.....
#

    if ( $instr == EPN ) then
      set fin=1600
      set i=400
      set mod=pn_modfile.txt
    else
      set fin=534
      set i=134
      set mod=mos_modfile.txt
    endif  


    if ( -e $mod ) then
        goto dbsstart
       else 
        touch $mod
          while ( $i < $fin )
          echo "$i 0.0 " >> $mod
          @ i++
         end
    endif

dbsstart:
goto ignoremod
   if ( $vigflag == 1 ) then  
     fmodtab tempsp22+1 RATE $mod
   else
     fmodtab tempsp22+1 COUNTS $mod    
   endif
ignoremod:
   if ( $vigflag == 1 ) then
    punlearn fcalc
    punlearn fdelcol
    fcalc tempsp21 tempsp23 RATEBG "RATE" clobber=yes
    fdelcol tempsp23+1 RATE N Y
    faddcol tempsp23 tempsp22 RATE
    fcalc tempsp23 tempsp24 RATESOFT "RATE" clobber=yes
    fdelcol tempsp24+1 RATE N Y
    fcalc tempsp24 tempsp25 RATESOFTR "RATESOFT * $ratio" clobber=yes
    fcalc tempsp25 tempsp26 RATE "RATEBG + RATESOFTR" clobber=yes 
    fdelcol tempsp26+1 RATEBG N Y
    fdelcol tempsp26+1 RATESOFT N Y
    fdelcol tempsp26+1 RATESOFTR N Y
    cp tempsp26 sp_dbs_${root}.fits 
   else
    punlearn fcalc
    punlearn fdelcol
    fcalc tempsp21 tempsp23 COUNTSBG "COUNTS" clobber=yes
    fdelcol tempsp23+1 COUNTS N Y
    faddcol tempsp23 tempsp22 COUNTS
    fcalc tempsp23 tempsp24 COUNTSSOFT "COUNTS" clobber=yes
    fdelcol tempsp24+1 COUNTS N Y
    fcalc tempsp24 tempsp25 COUNTSSOFTR "COUNTSSOFT * $ratio" clobber=yes
    fcalc tempsp25 tempsp26 COUNTS "COUNTSBG + COUNTSSOFTR" clobber=yes 
    fdelcol tempsp26+1 COUNTSBG N Y
    fdelcol tempsp26+1 COUNTSSOFT N Y
    fdelcol tempsp26+1 COUNTSSOFTR N Y
    cp tempsp26 sp_dbs_${root}.fits 
   endif #vigflag==1
#removing unwanted files....

echo "Double subtraction completed.\n"

endif #dbscal

rm OFFSET*

#Grouping data
if ( $group != 0 ) then
echo "Associating double background spectrum, and grouping into >=" $group "bins....\n"
set grpname=sp_grp${group}_${root}.fits
if ( -e $grpname ) then
rm -f $grpname
endif
if ( -e tempba07 ) then
rm -f tempba07
endif

set arffile=sp_s_${root}.arf
set rspfile=sp_s_${root}.rmf

punlearn grppha
grppha sp_s_${root}.fits $grpname comm="chkey backfile sp_dbs_${root}.fits & chkey respfile $rspfile & chkey ancrfile $arffile & group min $group & exit" > tempba07

echo "Data grouped to sp_grp${group}_${root}.fits"
endif

# Also vignetting (evigweight)
#why was arfgen crashing when using canned rmf files?

#cleanup


rm temp*

rm sp_s_dbs* sp_b_dbs* sp_scb_dbs* sp_bs_${root}.fits

if ( -e detmap_${root}.fits ) then
    rm detmap_${root}.fits
endif

if ( -e temBEv00 ) then
    rm temBEv00 
endif

if ( -e sp_bs_${root}.fits ) then 
    rm sp_bs_${root}.fits
endif

echo "All done"

# I'm going to try and make sense of this once and for all.
# Gonna save me time in the future. 
#filename   meaning
#temBEv00 - original BG events file. Corrected for weighting if necessary. 
#tempEv00 - original events file. Ditto.
#tempEv01 - events filtered on PATTERN and FLAG
#tempeV02 - BG events filtered on PATTERN and FLAG 
#tempba01 - backscale for source spectrum
#tempba02 - backscale calcs
#tempba03 - backscale calcs
#tempba04 - backscale calcs
#tempba05 - backscale calcs
#tempba06 - backscale calcs
#tempba07 - backscale calcs
#tempsp01 - source spectrum
#tempsp02 - background spectrum. Either local or blank-sky
#tempsp03 - RATE=source spec, RATE2=scaled bg spec
#tempsp04 - RATE1=source spec, RATE2=scaled bg spec
#tempsp05 - RATE=RATE1-RATE2. Source minus background spectrum. Meaningless
#           result as src spectum is contaminated with emission. 
#tempsp06 - scaled background spectrum
#tempsp11 - HR spectrum (src dataset)
#tempsp12 - HR spectrum (bg dataset)
#tempsp13 - RATE=HR src spectrum, RATE2=scaled HR bg spectrum 
#tempsp14 - RATE1=HR src spectrum, RATE2=scaled HR bg spectrum. Note that 
#           RATE1 is scaled if no weighting is applied by the ratio of 
#           exposure times such that the normalisation of the two spectra 
#           is the same.
#tempsp15 - RATE=RATE1-RATE2. This is the source minus background spectrum. 
#           This is also known as a soft spectrum as the spectra at the
#           higher ends should match up nicely. This is what I will be adding
#           to the scaled background spectrum taken at the source region.   
#tempsp16 - HR spectrum (bg dataset) scaled. 
#tempsp21 - scaled bg spectrum = tempsp06
#tempsp22 - soft spectrum = tempsp15
#tempsp23 - RATEBG=scaled bg, RATE=soft spectrum
#tempsp24 - RATEBG=scaled bg, RATESOFT=soft spectrum
#tempsp25 - RATEBG=scaled bg, RATESOFTR=scaled soft spectrum. Note that 
#           the soft spectrum scaling ratio is a ratio of the areas that 
#           contains each source region. Calculated from backscale. 
#           This might well be where I am going wrong....
#tempsp26 - RATE=RATEBG+RATESOFTR. This is a double subtracted spectrum. 
#sp_b_dbs_${root}.fits = tempsp12
#sp_b_mos1_${root} =tempsp02
#sp_bs_dbs_${root}.fits = tempsp15
#sp_bs_mos1_${root} = tempsp05
#sp_dbs_${root}.fits = tempsp26
#sp_grp20_${root}.fits - grouped spectra
#sp_s_dbs_${root}.fits = tempsp11
#sp_s_${root}.arf - generated arf based on generated rmf
#sp_s_${root}.fits =tempsp01
#sp_s_${root}.rmf - generated rmf using source spectrum
#sp_scb_dbs_${root}.fits =tempsp16
#sp_scb_${root}.fits = tempsp06







