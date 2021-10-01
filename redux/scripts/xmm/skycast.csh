#! /star/local/bin/tcsh 
#
# 06/06/02
# Andy Read

set version=1.2

nice +19

cat <<EOF 

--------------- SKYCAST VERSION $version -----------------

EOF

if ( $#argv != 3 ) then 
  cat <<EOF 

Script to cast an XMM EPIC background dataset (or indeed any EPIC
event dataset) onto the sky, at the position given by an input
template event dataset (e.g. the event file you are interested in
producing a background for). It is required that the XMM-SAS has been
initiated, and that SAS variables, e.g. SAS_CCF, SAS_ODF, are
correctly set.

Use: skycast background template outfile

  background - Blank-sky background (or in fact any) event list
  template   - Template file (e.g. event file/image) with attitude info  
  outfile    - Background file with sky co-ords relevant to template file

e.g. skycast /BGfiles/mos1BG.ds /mydata/myEvfile.fits mos1BGcorrectsky.fits

EOF
 exit
endif

set backgr=$1
set template=$2
set outfile=$3

#XMM sas started?
if (! -e $SAS_DIR) then 
 echo "XMM SAS has not been started... exiting..."
 exit
endif

#check input
if ( ! -e $backgr ) then
 echo "Error: User input file $backgr does not exist... exiting..."
 exit
else if ( -e $backgr ) then 
 echo "Input file... $backgr"
 cp $backgr $outfile
endif

#CCF OK?
if (! -e $SAS_CCF) then 
 echo "Environment variable SAS_CCF does not exist...needs to be set... " 
 exit
endif
if (! -e $SAS_CCFPATH) then 
 echo "Environment variable SAS_CCFPATH does not exist...needs to be set... " 
 exit
endif

#Get pnt values from template dataset
if (! -e $template ) then 
 echo "Error: Template file $template does not exist... exiting..."
 exit
else if (-e $template ) then 
 echo "Getting attitude information from template file $template..."
 rm -f temp0001
 fdump $template+0 temp0001 1 1-1
 set ra_pnt=`grep RA_PNT < temp0001 | head -1 | awk '{print $3}'`
 set dec_pnt=`grep DEC_PNT < temp0001 | head -1 | awk '{print $3}'`
 set pa_pnt=`grep PA_PNT < temp0001 | head -1 | awk '{print $3}'`
 set ranom=`grep REFXCRVL < temp0001 | head -1 | awk '{print $2}'`
 set decnom=`grep REFYCRVL < temp0001 | head -1 | awk '{print $2}'`
 rm -f temp0001
endif

#Using PNT attitude - do attcalc
 echo "Calculating sky coordinates..."
 attcalc -w 0 -V 0 eventset=$outfile attitudelabel=fixed fixedra=$ra_pnt fixeddec=$dec_pnt fixedposangle=$pa_pnt withatthkset=N refpointlabel=user nominalra=$ranom nominaldec=$decnom
 echo "Skycast Completed."
endif

exit









