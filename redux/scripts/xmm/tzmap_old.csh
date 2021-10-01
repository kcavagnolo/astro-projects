#! /bin/csh
#	Script to generate teperature and abundance maps for a Chandra(?)
#	dataset. Script originally written by D. Akerman, but no explanations
#	present as to what to do to the data.
#
#
set version=1.0

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

if ($argv !=13) then
cat <<EOF
Use tzmap.csh events bgevents aoff bgreg xmax xmin ymax ymin nxpix nypix mincounts model metals

	events		- the events data for your observation
	aoff		- aoff file for your observation
	bgreg		- background region
	bgevents	- background events file
	xmax		- ?
	xmin		- ?
	ymax		- ?
	ymin		- ?
	nxpix		- ?
	nypix		- ?
	mincounts	- ?
	model		- ?
	metals		- ?
EOF
exit
endif

set events = $1
set aoff   = $2
set bgreg  = $3
set bgevents = $4
set xhi=$5
set xlo=$6
set yhi=$7
set ylo=$8
set nxpix=$9
set nypix=$10
set mincounts = $11
set model_type = $12
set metals_fit = $13


#check input files exist
#check input files exist
if ( ! -e $events ) then
 echo "Error: $events does not exist"
 exit
endif
if ( ! -e $aoff ) then
 echo "Error: $aoff does not exist"
 exit
endif
if ( ! -e $bgevents) then
 echo "Error: $bgevents does not exist"
 exit
endif

#check regions are in ciao format
set rtype=`head -1 $bgreg | awk '{print $5}'`
if ( $rtype != CIAO ) then
 echo "  ERROR - REGION FILE DOES NOT APPEAR TO BE IN CIAO FORMAT\n\n"
 exit
endif



set binsfitted=0
set xwidth = `echo $xhi $xlo | awk '{print $1-$2}'`
set ywidth = `echo $yhi $ylo | awk '{print $1-$2}'`
# The output format should include more than the 6 digit default to 
# avoid rounding errors when the number of bins is large
set xbin   = `echo $xwidth $nxpix | awk 'OFMT = "%.10g" {print $1/$2}'`
set ybin   = `echo $ywidth $nypix | awk 'OFMT = "%.10g" {print $1/$2}'`

set ktfile = kt${nxpix}_${nypix}
set red_chifile = red_chi${nxpix}_${nypix}
set metallicityfile = metallicity${nxpix}_${nypix}
set photon_indexfile = photon_index${nxpix}_${nypix}

echo 'Spatial bins are ' ${xbin} 'x' ${ybin}

foreach file (${ktfile}.txt ${red_chifile}.txt)
  rm -f $file
  touch $file
end

if ( $metals_fit == thaw ) then 
  rm -f ${metallicityfile}.txt
  touch ${metallicityfile}.txt
endif

if ( $model_type == 2 ) then
  rm -f ${photon_indexfile}.txt
  touch ${photon_indexfile}.txt
endif

punlearn psextract
pset psextract events=$events"[sky=region(mapscript.reg)]"
pset psextract bgevents=none
pset psextract root=region
pset psextract aoff=$aoff
pset psextract bgaoff=""
pset psextract ptype = pi
pset psextract mode = h  
pset psextract gtype = NUM_CTS
pset psextract gspec = 20

# Extract a background spectrun
if ( ${bgReg} != 'none' ) then
  dmextract ${bkg_events}"[sky=region("${bgReg}")][bin pi]" bg_pi.fits clobber=yes
endif

set i=1
while ($i <= $nxpix)

set j=1
while ($j <= $nypix)

   set flag = 1
   set x = `echo $i $xbin $xlo | awk '{print ($1*$2-0.5*$2)+$3}'`
   set y = `echo $j $ybin $ylo | awk '{print ($1*$2-0.5*$2)+$3}'`
   echo 'x= ' $x  ', y= ' $y ', i= ' $i  ', j= ' $j

# Check whether the bin at this location was fitted in the previous step if not then don't bother fitting this time
# Must be integer arithmetic otherwise the next @ command may fail
@ previousNxpix =  $nxpix / 2
@ previousNypix =  $nypix / 2
set previousFile = kt${previousNxpix}_${previousNypix}.txt
# This needs to be integer arithmetic i.e. 3/2 = 1
echo 'i='$i ' j=' $j 
@ line = ( ( $j + 1 ) / 2 ) + ( ( $i - 1 ) / 2 ) * ${previousNypix}

if ( -e $previousFile ) then
# This will be -1 if there were insufficient counts
set lastTime = `head -n$line $previousFile | tail -1 | awk '{print $3}'`
else 
set lastTime = 1
endif

if ( $lastTime == -1 ) then
    set kT = -1
    set abund = -1
    set abs = -1
    set red_chi = -1
    set photon_index = -99
    set flag = 0
    echo 'Bin not fitted last time so not fitting this time.'
endif

# write a region file. x and y are the co-ordinates of the bin centre.
rm -f mapscript.reg
echo '+box('$x','$y','$xbin','$ybin',0)' > mapscript.reg

# Make sure there are enough counts in the specified region
set ncounts=`dmstat $events"[cols time][(x,y)=region(mapscript.reg)]" sig=no | grep 'good' | awk '{print $2}'`
echo 'ncounts=' $ncounts

if ( $ncounts < $mincounts ) then 
    set kT = -1
    set abund = -1
    set abs = -1
    set red_chi = -1
    set photon_index = -99
    set flag = 0
    echo 'Fewer than ' $mincounts ' counts in bin. Not fitting ...'
endif

if ( $flag == 1 ) then 
   @ binsfitted++
   psextract verbose=0
   if ( ${bgReg} == 'none' ) then
      dmextract ${bkg_events}"[sky=region(mapscript.reg)][bin pi]" bg_pi.fits clobber=yes
   endif
# Make sure the arf file was sucessfully created
set arfmax = `dmstat "region.arf[cols SPECRESP]" | grep max | awk '{print $2}'`

if ( $arfmax == 0 ) then 
    set kT = 0
    set abund = -1
    set abs = -1
    set red_chi = -1
    set photon_index = -99
    echo 'ARF was computed to be zero at all energies. Not fitting.'
else

# Fit the spectrum in sherpa
rm -f fit_results.txt
sherpa ./sherpa_fit.txt > fit_results.txt

set kT = `grep 'kT' fit_results.txt | grep 'thawed' | awk '{print $4}'`
set abund = `grep 'Abund' fit_results.txt | grep 'thawed' | awk '{print $4}'`
set abs = `grep 'nH' fit_results.txt | grep 'cm\^2' | grep 'thawed' | awk '{print $4}'`
set red_chi = `grep 'Reduced statistic' fit_results.txt | awk '{print $4}'`
if ( $red_chi == 'nan' ) then
   set red_chi = -1
   echo 'No reduced chi squared value obtained from fit.'
endif 

if ( $model_type == 2) then
# Really is '{print $3}' because the 1 and the PhoIndx run into one another.
set photon_index = `grep 'PhoIndx' fit_results.txt | grep 'thawed' | awk '{print $3}'`
endif

endif # from the 'Is the ARF zero ?' if block
rm -f region*
endif

echo $x '  ' $y '  ' $kT >> ${ktfile}.txt
echo $x '  ' $y '  ' $red_chi >> ${red_chifile}.txt

if ( $metals_fit == thaw ) then
    echo $x '  ' $y '  ' $abund >> ${metallicityfile}.txt
endif

if ( $model_type == 2 ) then
    echo $x '  ' $y '  ' $photon_index >> ${photon_indexfile}.txt
endif

# Tidy up 
   rm -f fit_results.txt
   rm -f mapscript.reg

   @ j++
end
   @ i++
end

echo 'Number_of_bins_fitted= ' $binsfitted

# Make an image from the dmcopied events list and get wcs info
rm -f wcs_keywords.lis
echo "#add" > wcs_keywords.lis
dmcopy acis_events_restricted.fits"[events][bin x="${xlo}":"${xhi}":#"${nxpix}",y="${ylo}":"${yhi}":#"${nypix}"]" image.fits clobber=yes
foreach keyword ( CTYPE1 CRVAL1 CRPIX1 CDELT1 CTYPE2 CRVAL2 CRPIX2 CDELT2 CTYPE1P CRVAL1P CRPIX1P CDELT1P  CTYPE2P CRVAL2P CRPIX2P CDELT2P LTV1 LTM1_1 LTV2 LTM2_2 )
    dmlist image.fits header,raw,clean | grep "${keyword} " >> wcs_keywords.lis
end

cat ~da/AUX/astheader ${ktfile}.txt > temp.txt
txt2ast inp='temp.txt' out=${ktfile}
rm temp.txt
flst2im infile=${ktfile}.txt outfile=${ktfile}.fits xrange="${xlo},${xhi}" yrange="${ylo},${yhi}" rows="-" nxbin=$nxpix nybin=$nxpix clobber=yes
#flst2im infile=${ktfile}.txt outfile=${ktfile}.fits xrange="${xlo},${xhi}" yrange="${ylo},${yhi}" rows="-" nxbin=35 nybin=35 clobber=yes

dmhedit infile=${ktfile}.fits filelist=wcs_keywords.lis

sed s/Temperature/'Reduced Chi squared'/ < ~da/AUX/astheader > temp_astheader
cat temp_astheader ${red_chifile}.txt > temp.txt
txt2ast inp='temp.txt' out=${red_chifile}
rm temp.txt
rm temp_astheader
flst2im infile=${red_chifile}.txt outfile=${red_chifile}.fits xrange="${xlo},${xhi}" yrange="${ylo},${yhi}" rows="-" nxbin=$nxpix nybin=$nxpix clobber=yes
dmhedit infile=${red_chifile}.fits filelist=wcs_keywords.lis

if ( $metals_fit == thaw ) then 
sed s/Temperature/Metallicity/ < ~da/AUX/astheader > temp_astheader 
cat temp_astheader ${metallicityfile}.txt > temp.txt
txt2ast inp='temp.txt' out=${metallicityfile}
rm temp.txt
rm temp_astheader
#flst2im infile=${metallicityfile}.txt outfile=${metallicityfile}.fits xrange="${xlo},${xhi}" yrange="${ylo},${yhi}" rows="-" nxbin=$nxpix nybin=$nxpix clobber=yes
flst2im infile=${metallicityfile}.txt outfile=${metallicityfile}.fits xrange="${xlo},${xhi}" yrange="${ylo},${yhi}" rows="-" nxbin=35 nybin=35 clobber=yes
dmhedit infile=${metallicityfile}.fits filelist=wcs_keywords.lis
endif

if ( $model_type == 2 ) then 
sed s/Temperature/'Photon Index'/ < ~da/AUX/astheader > temp_astheader 
cat temp_astheader ${photon_indexfile}.txt > temp.txt
txt2ast inp='temp.txt' out=${photon_indexfile}
rm temp.txt
rm temp_astheader
flst2im infile=${photon_indexfile}.txt outfile=${photon_indexfile}.fits xrange="${xlo},${xhi}" yrange="${ylo},${yhi}" rows="-" nxbin=$nxpix nybin=$nxpix clobber=yes
dmhedit infile=${photon_indexfile}.fits filelist=wcs_keywords.lis
endif

pset psextract mode =ql  # return to default 
exit
