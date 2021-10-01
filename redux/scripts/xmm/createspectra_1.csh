#! /star/local/bin/tcsh
#
# AMR 06/06/02

set version=1.5

nice +19

cat <<EOF

--------------- CREATESPECTRA VERSION $version -----------------

EOF

if ( $#argv != 11 ) then
 cat <<EOF

Script to, given two event files (usually a source file and a blank
sky background file), create spectra extracted from a chosen
annulus. A scaling factor (from e.g. compareoutofFOV) can be applied
to the background spectrum, and a scaled-background spectrum and a
source-minus-scaled-background spectrum are produced (this second
spectrum being effectively [in non-source regions] the extra cosmic
soft component due to the particular position on the sky). This output
can be used in the task doublesubtraction. RMF files and ARF files can
be created, and the BACKSCAL keywords are corectly calculated (within
the ARF generation). If sources/features have been removed from the
source file, a further BACKSCAL correction for the area of the removed
sources is performed, if the event file pre-source-removal is given.

Note, as a word of warning, that scaling down a spectrum by a largish
factor can lead to unrealistically large numbers of channels with zero
counts. In the case of a typical background X-ray spectrum, this can
manifest itself as a softening of the spectrum.

Files produced (*=root):
 sp_s_*.fits   : source spectrum
 sp_b_*.fits   : background spectrum
 sp_scb_*.fits : scaled background spectrum
 sp_bs_*.fits  : background subtracted (source minus scaled background) spectrum

Use: createspectra evfile bgfile bgscale Rinner Router withrmfgen witharfgen evfile_old patcode root

 evfile     - Event file
 bgfile     - Background (blank sky) file
 bgscale    - Background scaling factor (from e.g. compareoutofFOV)
 Rinner     - Inner spectral extraction radius (integer arcsec)
 Router     - Outer spectral extraction radius (integer arcsec)
 withrmfgen - Create rmf? (Y/N)
 witharfgen - Create arfs? (Y/N) (also calculates BACKSCALE)
 evfile_old - Event file pre-source-removal (0 if non-existent)
 patcode    - Pattern code (s,d,t,q,sd,sdtq - singles, doubles, triples, quads)
 root       - Rootname for spectra, arfs, rmfs etc
 source     - CIAO region file (.reg)


e.g. comparespectra evfile bgfile 0.15 500 800 N Y evfile_old sd run1

EOF
 exit
endif

########################################################################

set ev=$1
set bg=$2
set bgscale=$3
set rin=$4
set rout=$5
set rmfgen=$6
set arfgen=$7
set ev2=$8
set patcode=$9
set root=$10
set source=$11

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

#  GET INSTRUMENT

fdump $ev tempdmp1 TIME - prdata=no showcol=no showunit=no showrow=no clobber=yes
set instr=`grep INSTRUME < tempdmp1 | head -1 | awk '{print $2}'`
set instr=`echo $instr | tr -d "'"`
echo 'Instrument is '$instr
if ($instr == EPN) then
  set spchmax = 20475
  set spbnsize = 5
  set pnline=190
else
  @ spchmax = 11999
  @ spbnsize = 15
  @ pnline = 0
endif
rm -f tempdmp1

#  SET UP PARSPEC AND PARIMAGE

set parspec="withspectrumset=Y spectralbinsize=$spbnsize energycolumn=PI specchannelmax=$spchmax specchannelmin=0 withspecranges=Y"
set parimage="withimageset=Y xcolumn=DETX ycolumn=DETY ximagebinsize=40 yimagebinsize=40 imagedatatype=Real32 squarepixels=Y imagebinning=binSize withimagedatatype=Y ximagesize=1020 yimagesize=1020 keepfilteroutput=N withxranges=Y withyranges=Y ximagemin=-20399 yimagemin=-20399 ximagemax=20400 yimagemax=20400"
@ r0 = $rin * 20
@ r1 = $rout * 20

#  CREATE EV SPEC AND IMAGE

echo 'Creating source spectrum (Pattern expression - '$patexp' )...'
evselect -w 0 -V 0 table=$ev filteredset=tempEv01 expression="$patexp" updateexposure=N writedss=Y withfilteredset=Y destruct=Y keepfilteroutput=T
# alter legal DET values
fdump tempEv01 tempfd01 1 1-1 clobber=yes
if (-e tempfm01) rm -f tempfm01
set lodetx = `grep "Legal minimum for DETX" < tempfd01 | head -1 | awk '{print $1}'`
echo $lodetx' -20400 / Minimum for DETX (altered)' > tempfm01
set hidetx = `grep "Legal maximum for DETX" < tempfd01 | head -1 | awk '{print $1}'`
echo $hidetx' 20400 / Maximum for DETX (altered)' >> tempfm01
set lodety = `grep "Legal minimum for DETY" < tempfd01 | head -1 | awk '{print $1}'`
echo $lodety' -20400 / Minimum for DETY (altered)' >> tempfm01
set hidety = `grep "Legal maximum for DETY" < tempfd01 | head -1 | awk '{print $1}'`
echo $hidety' 20400 / Maximum for DETY (altered)' >> tempfm01
fmodhead tempEv01 tempfm01
rm -f tempfm01

set sourcefilter=`/exgal1/rft/scripts/xmmstring.csh $source | grep expression | awk  '{print $4}'`
echo $sourcefilter
evselect -w 0 -V 0 table=tempEv01 spectrumset=tempsp01 expression=${sourcefilter} $parspec
evselect -w 0 -V 0 table=tempEv01 imageset=tempim01 expression=${sourcefilter} $parimage
rm -f tempEv01
#  CREATE BG SPEC
echo 'Creating BG spectrum (Pattern expression - '$patexp' )...'

evselect -w 0 -V 0 table=$bg filteredset=tempEv02 expression="$patexp" updateexposure=N writedss=Y withfilteredset=Y destruct=Y keepfilteroutput=T

evselect -w 0 -V 0 table=tempEv02 spectrumset=tempsp02 expression=${sourcefilter} $parspec
rm -f tempEv02


#  IF, MAKE PRE-SRC IMAGE

set srcpixno=0
if ($ev2 != 0) then
  echo 'Creating image from pre-source-subtracted Ev file ...'
  cp $ev2 tempEv03
  # alter legal DET values
  fdump tempEv03 tempfd03 1 1-1 clobber=yes
  if (-e tempfm01) rm -f tempfm01
  set lodetx = `grep "Legal minimum for DETX" < tempfd03 | head -1 | awk '{print $1}'`
  echo $lodetx' -20400 / Minimum for DETX (altered)' > tempfm01
  set hidetx = `grep "Legal maximum for DETX" < tempfd03 | head -1 | awk '{print $1}'`
  echo $hidetx' 20400 / Maximum for DETX (altered)' >> tempfm01
  set lodety = `grep "Legal minimum for DETY" < tempfd03 | head -1 | awk '{print $1}'`
  echo $lodety' -20400 / Minimum for DETY (altered)' >> tempfm01
  set hidety = `grep "Legal maximum for DETY" < tempfd03 | head -1 | awk '{print $1}'`
  echo $hidety' 20400 / Maximum for DETY (altered)' >> tempfm01
  fmodhead tempEv03 tempfm01
  rm -f tempfm01
  evselect -w 0 -V 4 table=tempEv03 imageset=tempim02 expression=${sourcefilter} $parimage
  rm -f tempEv03

  echo 'Estimating pixels lost due to source removal...'
  farith tempim02 tempim01 tempim03 SUB clobber=yes
  fcarith tempim03 1.0E-20 tempim04 ADD clobber=yes
  farith tempim03 tempim04 tempim05 DIV clobber=yes
  set srcpixno = `fimgstat tempim05 0 1 | grep sum | awk '{print $8}'`
  set srcpixno = `echo $srcpixno 1600 | awk '{print $1*$2}'`
endif

echo 'Scaling and subtracting background...'
cp tempsp01 sp_s_$root'.fits'
cp tempsp02 sp_b_$root'.fits'
punlearn fcalc
punlearn fdelcol
fcalc tempsp02 tempsp03 COUNTS2 "$bgscale * COUNTS" clobber=yes
fcalc tempsp03 tempsp06 COUNTS "COUNTS2" clobber=yes
fdelcol tempsp06+1 COUNTS2 N Y
fdelcol tempsp03+1 COUNTS N Y
faddcol tempsp03 tempsp01 COUNTS
fcalc tempsp03 tempsp04 COUNTS1 "COUNTS"  clobber=yes
fdelcol tempsp04+1 COUNTS N Y
fcalc tempsp04 tempsp05 COUNTS "COUNTS1 - COUNTS2"  clobber=yes
fdelcol tempsp05+1 COUNTS1 N Y
fdelcol tempsp05+1 COUNTS2 N Y

cp tempsp05 sp_bs_$root'.fits'
cp tempsp06 sp_scb_$root'.fits'
#dsplot -w 0 -V 0 table=sp_bs_$root'.fits' x='CHANNEL' y='COUNTS'

echo 'ok?....'
if ($rmfgen == Y) then
  echo "Creating rmf for source spectrum..."
  rmfgen -w 0 -V 4 spectrumset=sp_s_$root'.fits' rmfset=sp_s_$root'.rmf'
endif

if ($arfgen == Y) then
  echo "Creating detector map for arfgen..."
  evselect -w 0 -V 4 table=$ev withimageset=Y imageset=detmap_$root'.ds' xcolumn=DETX ycolumn=DETY withxranges=Y ximagemin=-20000 ximagemax=20000 withyranges=Y yimagemin=-20000 yimagemax=20000 imagebinning=imageSize ximagesize=80 yimagesize=80 expression=$sourcefilter writedss=Y updateexposure=Y
  echo "Creating arf for source spectrum..."
  arfgen -w 0 -V 0 spectrumset=sp_s_$root'.fits' arfset=sp_s_$root'.arf' setbackscale=Y withbadpixcorr=Y badpixlocation=$ev extendedsource=Y detmaptype=dataset detmaparray=detmap_$root'.ds:'

if ($arfgen == Y) then
  echo "Calculating BACKSCAL (witharfgen=Y)..."
  punlearn fkeyprint
  punlearn fparkey
  fkeyprint sp_s_$root'.fits'+1 BACKSCAL exact=yes outfile=tempba01 clobber=yes
  set backscal=`grep BACKSCAL= < tempba01 | head -1 | awk '{print $2}'`
  echo "Backscale for source spectrum: "$backscal
  if ($ev2 != 0) then
    set backsc_c = `echo $backscal $srcpixno | awk '{print $1-$2}'`
    echo "...minus sources/bad pixels newly subtracted... "$backsc_c
    fparkey $backsc_c sp_s_$root'.fits'+1 BACKSCAL comm='Scaling factor for background ' add=yes
    fparkey $backsc_c sp_b_$root'.fits'+1 BACKSCAL comm='Scaling factor for background ' add=yes
    fparkey $backsc_c sp_scb_$root'.fits'+1 BACKSCAL comm='Scaling factor for background ' add=yes
    fparkey $backsc_c sp_bs_$root'.fits'+1 BACKSCAL comm='Scaling factor for background ' add=yes
  else
    fparkey $backscal sp_s_$root'.fits'+1 BACKSCAL comm='Scaling factor for background ' add=yes
   echo "BACKSCAL added to spectra..."
   fparkey $backscal sp_b_$root'.fits'+1 BACKSCAL comm='Scaling factor for background ' add=yes
    fparkey $backscal sp_scb_$root'.fits'+1 BACKSCAL comm='Scaling factor for background ' add=yes
    fparkey $backscal sp_bs_$root'.fits'+1 BACKSCAL comm='Scaling factor for background ' add=yes
  endif
  echo "BACKSCAL added to spectra..."
endif

rm -f tempsp??
rm -f tempba??
rm -f tempim??
rm -f tempfd??
echo "Createspectra completed."

exit

