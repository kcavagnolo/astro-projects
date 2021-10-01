#! /star/local/bin/tcsh -f
nice +19

set events = $1
set asol   = $2
set bgReg  = $3
set xhi=$4 
set xlo=$5
set yhi=$6 
set ylo=$7 
set nxpix=$8 
set nypix=$9
set mincounts = $10
set model_type = $11
set metals_fit = $12
set bkg_events = $13
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
pset psextract asol=$asol
pset psextract bgasol=""
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
#   cp fit_results.txt ./test${i}${j}.txt
   rm -f fit_results.txt
   rm -f mapscript.reg

   @ j++
end
   @ i++
end

echo 'Number_of_bins_fitted= ' $binsfitted

cat ~da/AUX/astheader ${ktfile}.txt > temp.txt
txt2ast inp='temp.txt' out=${ktfile}
rm temp.txt

sed s/Temperature/'Reduced Chi squared'/ < ~da/AUX/astheader > temp_astheader
cat temp_astheader ${red_chifile}.txt > temp.txt
txt2ast inp='temp.txt' out=${red_chifile}
rm temp.txt
rm temp_astheader

if ( $metals_fit == thaw ) then
sed s/Temperature/Metallicity/ < ~da/AUX/astheader > temp_astheader
cat temp_astheader ${metallicityfile}.txt > temp.txt
txt2ast inp='temp.txt' out=${metallicityfile}
rm temp.txt
rm temp_astheader
endif

if ( $model_type == 2 ) then 
sed s/Temperature/'Photon Index'/ < ~da/AUX/astheader > temp_astheader 
cat temp_astheader ${photon_indexfile}.txt > temp.txt
txt2ast inp='temp.txt' out=${photon_indexfile}
rm temp.txt
rm temp_astheader
endif

pset psextract mode =ql  # return to default 
exit
