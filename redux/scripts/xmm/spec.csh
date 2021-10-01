#! /bin/tcsh 
#
# Script to extract spectra for XMM observation.
# Extracts source and background spectra, and generates arf and rmf files.
#
# Version 1 - 17/8/01 by BJM
# version 1.1 - 22/2/02 BJM, added option to provide own arf and rmf

set version=1

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

--------------------`echo $0:t | tr '[a-z]' '[A-Z]'` version $version---------------------

EOF

if ( $#argv != 7 ) then
cat <<EOF

Use xmmspec.csh table source back rawY rmftype arftype root

    table       - your input table - should have initial data prep done allready
                  (i.e. patterns selected, flares removed...)
    source      - source region - use a CIAO format region file of the regions 
                  you wish to extract from.
                  Each region in the file should be on a new line, and excluded
                  regions should begin with '-'. e.g.
                  # Region file format: CIAO version 1.0
                  circle(14422,24933,1000)   
                  -circle(14422,24933,500) 
    back        - background region, same format as above  
    rawY        - the RAWY coordinate of the source, used to determine the correct
                  response (for PN only). If you enter "none" then the script will 
                  find it for you by making an image of the source in RAW coords
                  and taking the mean value. Should be in range 0-200.
                  If you are providing your own rmf, this is ignored, so enter
                  "none"
    rmftype     - enter "cal" to use a precalculated rmf from the XMM cal, or
                  enter "rmfgen" to generate one with rmfgen, or
                  enter filename to use an existing rmf.
    arftype     - enter "arfgen" to generate an arf, or
                  enter filename to use an existing arf
    root        - root name for all output files.

EOF
exit
endif

set table=$1
set source=$2
set back=$3
set rawy=$4
set rmftype=$5
set arftype=$6
set root=$7

#check input files exist
set tabroot=`echo $table | cut -d ":" -f1`
if ( ! -e $tabroot ) then
 echo "Error: $tabroot does not exist"
 exit
endif
if ( ! -e $source ) then
 echo "Error: $source does not exist"
 exit
endif
if ( ! -e $back ) then
 echo "Error: $back does not exist"
 exit
endif
if (( $rmftype != cal )&&( $rmftype != rmfgen )) then
 if ( ! -e $rmftype ) then
  echo "Error: $rmftype does not exist"
  exit
 endif
set rmffile=$rmftype
set rmftype=file
endif
if ( $arftype != arfgen ) then
 if ( ! -e $arftype ) then
  echo "Error: $arftype does not exist"
  exit
 endif
set arffile=$arftype
set arftype=file
endif

echo "Input was $0 $*\n\n"

#Get pattern filter from table statistics
echo "Attempting to detect previous filtering from table statistics..."
set pattmin=`fstatistic "${table}" pattern - | grep minimum | awk '{printf("%d\n",$7)}'`
set pattmax=`fstatistic "${table}" pattern - | grep maximum | awk '{printf("%d\n",$7)}'`
echo "  events were filtered between pattern(s) ${pattmin} - ${pattmax}\n"
if ( $pattmin != 0 ) then
 echo "WARNING - you don't appear to have selected single (pattern = 0) events.\n"
endif

#check instrument type
set instrument=`fkeyprint "${table}[EVENTS]" INSTRUME | grep "INSTRUME=" | cut -d"'" -f2`
if ( $instrument == EPN ) then
 echo "Instrument is $instrument - using PI = 0-20479 channels in 5 channel bins\n"
 set bin=5
 set pi_max=20479
 set pi_min=0
 if ( $pattmax == 0 ) then
  echo "single events (pattern = 0) only are in use."
  set pat=s
 else if ( $pattmax == 4 ) then
  echo "single and double events (pattern <= 4) are in use."
  set pat=sd
 else echo "WARNING - unsure what events are in use, assuming single and double"
  set pat=sd
 endif
else if ( $instrument == EMOS1 ) then
 echo "Instrument is $instrument - using PI = 0-11999 channels in 15 channels bins\n"
 set bin=15
 set pi_max=11999
 set pi_min=0
 set rmfstring=m1_
 if ( $pattmax == 0 ) then
  echo "single events (pattern = 0) only are in use."
  set pat=p0
 else if ( $pattmax == 12 ) then
  echo "single and double events (pattern <= 12) are in use."
  set pat=all
 else echo "WARNING - unsure what events are in use, assuming single and double"
  set pat=all
 endif
else if ( $instrument == EMOS2 ) then
 echo "Instrument is $instrument - using PI = 0-11999 channels in 15 channels bins\n"
 set bin=15
 set pi_max=11999
 set pi_min=0
 set rmfstring=m2_
 if ( $pattmax == 0 ) then
  echo "single events (pattern = 0) only are in use."
  set pat=p0
 else if ( $pattmax == 12 ) then
  echo "single and double events (pattern <= 12) are in use."
  set pat=all
 else echo "WARNING - unsure what events are in use, assuming single and double"
  set pat=all
 endif
else echo "Instrument is $instrument - type unknown.\n"
 exit
endif

#find centre of source
set x=`head -2 $source | tail -1 | cut -d"(" -f2 | cut -d"," -f1`
set y=`head -2 $source | tail -1 | cut -d"(" -f2 | cut -d"," -f2`
echo "Centre of region is (x,y) = $x,$y - If the region is not simple this may be incorrect.\n"

#convert region files to filter expressions
set sourcefilter=`csh /home/cavagnolo/research/redux/scripts/xmm/string.csh $source | grep expression | awk  '{print $4}'`
set bgfilter=`csh /home/cavagnolo/research/redux/scripts/xmm/string.csh $back | grep expression | awk '{print $4}'`

if ( $rmftype != file ) then
#find rawy coord of source for rmf (uses mean value of RAWY in source region)
if ( $instrument == EPN ) then
 if ( $rawy == none ) then
  echo "Finding region centre in RAW coords..."
  evselect table="${table}:EVENTS" withfilteredset=yes filteredset=${root}source_evt.fits filtertype=expression expression="$sourcefilter" destruct=yes keepfilteroutput=yes
  fstatistic "${root}source_evt.fits[EVENTS]" rawy rows="-" outfile=${root}source_evt_stats.txt clobber=yes
  set rawy=`fstatistic "${root}source_evt.fits[EVENTS]" rawy "-" | grep mean | awk '{print int($8+0.5)}'`
 endif
 echo "RAWY = $rawy\n"
 set Y=`echo $rawy | awk '{print int($1/20)}'`
 if ( $rawy == 200 ) then
  set Y=9
 endif
endif

#Choose appropriate rmf and link to it
if ( $rmftype == cal ) then
echo "Deciding on appropriate RMF..."

#find filter
set filter=`fkeyprint "${table}[EVENTS]" FILTER | grep Filter | cut -d"'" -f2`
if ( $filter == Thick ) then
 set filter=thick
 set filter2=thick
else if ( $filter == Medium ) then
 set filter=medium
 set filter2=med
else if (( $filter == Thin1 )||( $filter == THIN )) then
 set filter=thin
 set filter2=thin1
else echo "ERROR: filter not recognised - filter=$filter\n"
 exit
endif

#check instrument
if ( $instrument == EPN ) then
echo "  using rmf ${SAS_CCFPATH}/extras/responses/pn/${filter}/epn_ff20_${pat}Y${Y}_${filter}.rmf"
echo "  creating link\n"
set rmffile=epn_ff20_${pat}Y${Y}_${filter}.rmf
if ( -e $rmffile ) then
 rm -f $rmffile
endif
ln -s ${SAS_CCFPATH}/extras/responses/pn/${filter}/$rmffile
endif
if (( $instrument == EMOS1 )||( $instrument == EMOS2 )) then
echo "  using rmf ${SAS_CCFPATH}/extras/responses/mos/${filter}/${rmfstring}${filter2}v9q19t5r5_${pat}_15.rsp"
echo "  creating link\n"
set rmffile=${rmfstring}${filter2}v9q19t5r5_${pat}_15.rsp
if ( -e $rmffile ) then
 rm -f $rmffile
endif
ln -s ${SAS_CCFPATH}/extras/responses/mos/${filter}/$rmffile
endif
endif

endif

#extract source spectrum
echo "Extracting source spectrum..."
#echo evselect table="${table}:EVENTS" withspectrumset=yes spectrumset=${root}spec.fits energycolumn=PI spectralbinsize=$bin withspecranges=true specchannelmin=0 specchannelmax=$pi_max writedss=yes expression="${sourcefilter}"
evselect table="${table}:EVENTS" withspectrumset=yes spectrumset=${root}spec.fits energycolumn=PI spectralbinsize=$bin withspecranges=true specchannelmin=0 specchannelmax=$pi_max writedss=yes expression="${sourcefilter}"
echo "  source spectrum written to ${root}spec.fits\n"

#extract background spectrum
echo "Extracting background spectrum..."
#echo evselect table="${table}:EVENTS" withspectrumset=yes spectrumset=${root}bgspec.fits energycolumn=PI spectralbinsize=$bin withspecranges=true specchannelmin=0 specchannelmax=$pi_max writedss=yes expression="${bgfilter}"
evselect table="${table}:EVENTS" withspectrumset=yes spectrumset=${root}bgspec.fits energycolumn=PI spectralbinsize=$bin withspecranges=true specchannelmin=0 specchannelmax=$pi_max writedss=yes expression="${bgfilter}"
echo "  background spectrum written to ${root}bgspec.fits\n"

if ( $rmftype != file ) then
# generate rmf with rmfgen
if ( $rmftype == rmfgen ) then
 echo "Generating RMF..."
 if ( $rawy == none ) then
  set rawy=0
 endif
 set nbins=`echo $pi_min $pi_max $bin | awk '{print ($2+1-$1)/$3}'`
# echo rmfgen spectrumset=${root}spec.fits rmfset=${root}rmf.fits threshold=1e-5 withenergybins=false pnline=$rawy
 rmfgen spectrumset=${root}spec.fits rmfset=${root}rmf.fits threshold=1e-5 withenergybins=false
 set rmffile=${root}rmf.fits
 echo "  RMF written to ${root}rmf.fits\n"
endif
endif

if ( $arftype != file ) then
#generate detector map
echo "Making detector map to weight ARF..."
#echo evselect table="${table}:EVENTS" destruct=false withfilteredset=true withimageset=true imageset=${root}detmap.fits xcolumn=DETX ycolumn=DETY imagebinning=binSize ximagebinsize=120 yimagebinsize=120 expression="${sourcefilter}" writedss=true updateexposure=true
evselect table="${table}:EVENTS" destruct=false withfilteredset=true withimageset=true imageset=${root}detmap.fits xcolumn=DETX ycolumn=DETY imagebinning=binSize ximagebinsize=120 yimagebinsize=120 expression="${sourcefilter}" writedss=true updateexposure=true
echo "  detector map written to ${root}detmap.fits\n"

echo "Generating ARF..."
#echo arfgen spectrumset=${root}spec.fits arfset=${root}arf.fits detmaptype=dataset detmaparray="${root}detmap.fits:" extendedsource=yes withrmfset=true rmfset=$rmffile setbackscale=yes withbadpixcorr=true badpixlocation=$table withfilteredset=true filteredset=${root}detmap_filter.fits withsourcepos=yes sourcecoords=pos sourcex=$x sourcey=$y
arfgen spectrumset=${root}spec.fits arfset=${root}arf.fits detmaptype=dataset detmaparray="${root}detmap.fits:" extendedsource=yes withrmfset=true rmfset=$rmffile setbackscale=yes withbadpixcorr=true badpixlocation=$table withfilteredset=true filteredset=${root}detmap_filter.fits withsourcepos=yes sourcecoords=pos sourcex=$x sourcey=$y
echo "  ARF written to ${root}arf.fits\n"
set arffile=${root}arf.fits

#remove unwanted detmaps
echo "Removing unwanted detmaps..."
rm -f ${root}detmap_filter.fits ${root}detmap.fits

#run backscale on source spec if arftype is a file
else echo "Computing BACKSCAL for source spectrum..."
backscale spectrumset=${root}spec.fits withbadpixcorr=yes badpixlocation=$table
endif

#run backscale on BG spectrum
echo "Computing BACKSCAL for background spectrum..."
backscale spectrumset=${root}bgspec.fits withbadpixcorr=yes badpixlocation=$table

#Associate background, and group counts into bins of size $group counts
set group=20
echo ""
echo "Associating background spectrum and grouping counts into bins >= ${group} using rmf $rmffile and arf $arffile"
if ( -e ${root}spec_grppha.fits ) then
 rm -f ${root}spec_grppha.fits
endif
punlearn grppha
grppha ${root}spec.fits ${root}spec_grppha.fits comm="chkey backfile ${root}bgspec.fits & chkey respfile $rmffile & chkey ancrfile $arffile & group min ${group} & exit" > tempfile
rm -f tempfile
echo "    Output written to ${root}spec_grppha.fits"
echo ""

exit
