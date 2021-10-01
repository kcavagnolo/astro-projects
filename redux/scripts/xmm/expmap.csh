#! /bin/tcsh 
#
# script to generate an exposure map, and exposure map weighted image
#
# version 1 - 21/8/01 by BJM
# version 1.1 - 15/3/02 BJM - use maximum of expmap to normalise it

set version=1.1

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

if ( $#argv != 6 ) then
cat <<EOF

Use xmmexpmap.csh table attfile bin root

    table       - your input table - should have initial data prep done allready
                  (i.e. patterns selected, flares removed...)
    attfile     - your attitude file
    bin         - binning factor for images.
    E_min       - lower energy bound (eV)
    E_max       - upper energy bound (eV)
    root        - root name for all output files.

EOF
exit
endif

set table=$1
set attitude=$2
set bin=$3
set en_low=$4
set en_high=$5
set root=$6

#check input files exist
set tabroot=`echo $table | cut -d ":" -f1`
if ( ! -e $tabroot ) then
 echo "Error: $tabroot does not exist"
 exit
endif
if ( ! -e $attitude ) then
 echo "Error: $attitude does not exist"
 exit
endif

echo "Input was $0 $*\n\n"

#Get energy and pattern filters from table header
echo "Attempting to detect previous filtering from statistics..."
set pattmin=`fstatistic "${table}" pattern - | grep minimum | awk '{printf("%d\n",$7)}'`
set pattmax=`fstatistic "${table}" pattern - | grep maximum | awk '{printf("%d\n",$7)}'`
set pattern
set n=$pattmin
while ( $n <= $pattmax )
 set pattern=($pattern $n)
 @ n++
end
if ( $pattmin != 0 ) then
 echo "WARNING - you don't appear to have selected single (pattern = 0) events.\n"
endif
echo "  events were filtered between ${en_low}-${en_high}eV and pattern(s)  ${pattmin} - ${pattmax}  (${pattern})\n"

#check instument because for MOS, eexpmap needs a list of the patterns used (e.g. pattern='0 1 2 3 4...') while PN uses 0 for singles, and 1 for all non-single events
set instrument=`fkeyprint "${table}[EVENTS]" INSTRUME | grep "INSTRUME=" | cut -d"'" -f2`
echo "Checking instrument type..."
if ( $instrument == EPN ) then
 echo "Instrument is $instrument\n"
 if ( $pattmax == 0 ) then
  set pattern=0
 else if ( $pattmax == 4 ) then
  set pattern="0 1"
 else echo "WARNING - unsure what events are in use, assuming single and double"
  set pattern="0 1"
 endif
else if ( $instrument == EMOS1 ) then
 echo "Instrument is $instrument\n"
else if ( $instrument == EMOS2 ) then
 echo "Instrument is $instrument\n"
else echo "Instrument is $instrument - type unknown.\n"
 exit
endif

#make image
echo "Making image..."
evselect table=$table withimageset=yes imageset=${root}img.fits xcolumn=X ycolumn=Y imagebinning=binSize ximagebinsize=$bin yimagebinsize=$bin expression="PI in [${en_low}:${en_high}]" writedss=yes
echo "  image written to ${root}img.fits\n"

#make exposure map
echo "Making exposure map..."
echo eexpmap imageset=${root}img.fits attitudeset=$attitude eventset=$table expimageset=${root}expmap.fits withdetcoords=no withvignetting=yes usefastpixelization=no attrebin=4.0 pimin=$en_low pimax=$en_high
eexpmap imageset=${root}img.fits attitudeset=$attitude eventset=$table expimageset=${root}expmap.fits withdetcoords=no withvignetting=yes usefastpixelization=no attrebin=4.0 pimin=$en_low pimax=$en_high
echo "  exposure map written to ${root}expmap.fits\n"

#normalise expmap by dividing by max
echo "Normalising exposure map by dividing by its max..."
set max=`fimgstat ${root}expmap.fits INDEF INDEF | grep "maximum of" | awk '{print $7}'`
fcarith ${root}expmap.fits $max ${root}expmap_norm.fits DIV clobber=yes
echo "  normalised exposure map written to ${root}expmap_norm.fits\n"

#flatten image
echo "Flattening image with exposure map..."
farith ${root}img.fits ${root}expmap_norm.fits ${root}img_flat.fits DIV blank=0
echo "  flattened image written to ${root}img_flat.fits\n"

exit
