#! /star/local/bin/tcsh 
##### NOTE: USER MAY HAVE TO EDIT THIS LINE TO FIND LOCAL tcsh
#
# AMR 06/06/02
# 1.2 AMR 12/05/03 - hole excluded from MOS (JBB)

set version=1.2

nice +19

cat <<EOF 


--------------- COMPAREOUTOFFOV VERSION $version -----------------

EOF

if ( $#argv != 7 ) then
 cat <<EOF 

A script to, given two event files (e.g. a source file and an
equivalent blank sky background file or a CLOSED filter event file),
compare the counts (within a certain energy/pattern range) in the
out-of-field-of-view (FOV) regions of the datasets. The output ratio
is a useful scaling value in subsequent tasks (e.g. createspectra).

Use: compareoutofFOV evfile bgfile Elow Ehigh patcode keepimages root

	evfile 	   - Event file
	bgfile 	   - Background (blank sky) file
	Elow 	   - Lower energy band (eV)
	Ehigh 	   - Higher energy band (eV)
        patcode    - Pattern code (s,d,t,q,sd,sdtq - singles, doubles, triples, quads)
        keepimages - keep images? (Y/N)
	root       - root for output filenames

e.g. compareoutofFOV evfile bgfile 300 12000 sd N run1

EOF
 exit
endif

set ev=$1
set bg=$2
set elo=$3
set ehi=$4
set patcode=$5
set keep=$6
set root=$7

#XMM sas started?
if (! -e $SAS_DIR) then 
  echo "It looks like XMM SAS has not been started... exiting..."
  exit
endif

set patexp="PATTERN==0"
if ($patcode == s) then
  set patexp="PATTERN==0"
endif
if ($patcode == d) then
  set patexp="PATTERN>=1&&PATTERN<=4"
endif
if ($patcode == t) then
  set patexp="PATTERN>=5&&PATTERN<=8"
endif
if ($patcode == q) then
  set patexp="PATTERN>=9&&PATTERN<=12"
endif
if ($patcode == sd) then
  set patexp="PATTERN<=4"
endif
if ($patcode == sdtq) then
  set patexp="PATTERN<=12"
endif

#get instrument
fdump $ev tempdmp1 TIME - prdata=no showcol=no showunit=no showrow=no clobber=yes
set instr=`grep INSTRUME < tempdmp1 | head -1 | awk '{print $2}'`
set instr=`echo $instr | tr -d "'"`
if ($instr == EPN) then 
  set parcirc="!(CIRCLE(-2120,-1080,18000,DETX,DETY))"
else
  set parcirc="!(CIRCLE(0,0,18000,DETX,DETY))&&((CIRCLE(0,15000,34500,DETX,DETY)||DETX<-10000||DETX>10000))"
endif

set parimages='xcolumn=X ycolumn=Y ximagebinsize=400 yimagebinsize=400 updateexposure=false imagedatatype=Real32 squarepixels=true imagebinning=binSize withimageset=Y writedss=false withimagedatatype=true keepfilteroutput=false'

set badratio = 0 
echo 'Creating images - ('$elo'-'$ehi' eV; Pattern expression: '$patexp')'
evselect -w 0 -V 0 table=$ev imageset=tempfov1 expression='PI>='$elo'&&PI<='$ehi'&& '$patexp'&& '$parcirc'&& #XMMEA_16' $parimages
set oFOVev = `fimgstat tempfov1 0 99999999 | grep sum | awk '{print $8}'`
set oFOVev = `echo $oFOVev | awk '{print int($1+0.5)}'`
if ($oFOVev <= 0) then 
  echo 'No out-of-FOV events found in Events - perhaps you have previously rejected them'
  set ratio = 0.0
  set badratio = 1
endif

evselect -w 0 -V 0 table=$bg imageset=tempfov2 expression='PI>='$elo'&&PI<='$ehi'&& '$patexp'&& '$parcirc'&& #XMMEA_16' $parimages
set oFOVbg = `fimgstat tempfov2 0 99999999 | grep sum | awk '{print $8}'`
set oFOVbg = `echo $oFOVbg | awk '{print int($1+0.5)}'`
if ($oFOVbg <= 0) then 
  echo 'No out-of-FOV events found in Background - perhaps you have previously rejected them'
  set badratio = 1
endif

if ($badratio == 0) then 
  set ratio = `echo $oFOVev $oFOVbg | awk '{print $1/$2}'`
  echo 'out-of-FOV count ratio (Ev/BG): '$ratio  
else
  echo 'no out-of-FOV count ratio given'
endif

if ($keep == Y) then
  mv -f tempfov1 imoFOVev_$root'.fits'
  mv -f tempfov2 imoFOVbg_$root'.fits'
else
  rm -f tempfov?
endif

exit

