#! /star/local/bin/tcsh -f
#
# Script to adaptively bin data and reture temperature & metallicity maps
#
#
#
#needs tzmap.csh
#needs combine.exe



set version = 1.0
#	RFT	10/12/04 V1.0	Modifying the script to have a better input sequence.
#				Struggled to get end triggers to work properly when
#				initially run. This is because I sourced the file
#				instead of running tcsh on it. This had the consequence
#				of not sending the data to the logfile but to the screen

# Things to do:
# get it running with a blank sky background
# allow option arguments for point sources. (or use removeregions.reg)
#

nice +19

# Test whether ciao and Asterix are already started 
if ( $?ASCDS_INSTALL ) then 
#    echo 'ciao already started'
else
    echo 'Please start ciao before running adapt.csh'
    exit
endif

if ( ${?AST_BIN} ) then
#    echo 'Asterix already started'
else
    source /soft/asterix/asterix/ix86_Linux/startup
endif

cat <<EOF

--------------------`echo $0:t | tr '[a-z]' '[A-Z]'` version $version `date +%x`--------------------

EOF

if ($#argv != 19 ) then
cat <<EOF
Use adapt.csh events bgevents bgreg xlo xhi ylo yhi nxpix nypix modeltype
    elo ehi mincounts temp met nH redshift metfit showfit

An aspect solution file needs to be in the current directory with 'asol'
somewhere in the filename.

    events	- Input events data set
    bgevents	- Background data set. Can be a blank sky background. (Not sure that it can). 
    		  Make sure that the data is already skycasted
    bgreg	- Background region file. This should be a CIAO
    		  region file. If using a blank sky background set
		  this to 'none'.
    xlo		- minimum x-value In PHYSICAL coordinates
    xmax	- maximum x-value
    ymin	- minimum y-value
    ymax	- maximum y-value
    nxpix	- minimum number of x-pixels. e.g. If nxpix=nypix=10 then
    		  the first iteration will extract a 10*10 grid. The second
		  iteration will be 20*20 etc
    nypix	- as for nxpix
    modeltype	- 1 = mekal, 2 = mekal+powerlaw. Both models have a single
    		  absorption component. A zwabs component needs to be added
		  at some point.
    elo		- minimum energy that spectrum will be fitted in. e.g. 0.3
    		  Please ensure that the energies are input in keV.
    ehi		- maximum energy e.g. 8.0 see above.
    mincounts	- minimum counts per bin required for fitting a spectra. Data
    		  will be ignored that has less counts than this. Values of -1
		  will be used instead for all parameters in the case of
		  non-fitting.
    temp	- initial estimate for temperature in keV
    met		- initial estimate for metallicity.
    nH		- initial estimate for absorbing column. I use the local galactic
		  value for this.
    redshift	- value for redshift of object.
    metfit	- Fit metallicities? If 'y' 'Y' 'yes' 'YES', then metallicity will
    		  be thawed. Another input will freeze it at the value set by 'met'.
		  Note that the '' should not be inserted around the input. DO NOT
		  leave this field blank, it is a mandatory parameter and will result
		  in crashing if ignored.
    showfit	- Show spectra after each fit? This has the same parameters as metfit.
    		  I would strongly recommend setting this to 'no', especially if you are
		  likely to be running this script for several hours.

EOF
exit
endif

set events_all=$1
set bkg_events=$2
set bgReg=$3
set xlo=$4
set xhi=$5
set ylo=$6
set yhi=$7
set nxpix=$8
set nypix=$9
set model_type=$10
set en_lo=$11
set en_hi=$12
set mincounts=$13
set init_temp=$14
set init_metals=$15
set abs_nh=$16
set redshift=$17
set quest=$18
set answer=$19
  # Are metals being fitted ?
  if ( $quest == 'y' || $quest == 'Y' || $quest == 'yes' || $quest == 'YES' ) then
    set metals_fit = thaw
  else
    set metals_fit = freeze
  endif
  # Are spectra being shown after each fit?
  if ( $answer == 'y' || $answer == 'Y' || $answer == 'yes' || $answer == 'YES' ) then
    set showplot = lplot
    set showexit = %exit
  else
    set showplot = %lplot
    set showexit = exit
  endif

#Check Input files exist

if ( ! -e $events_all )then
 echo "Error: $events_all does not exist. Exiting..."
 exit
endif

if ( ! -e $bkg_events ) then
 echo "Error: $bkg_events does not exist. Exiting...."
 exit
endif

if ( ! -e $bgReg ) then
 echo "Error: $bgReg does not exist. Exiting...."
 exit
endif

#Check region files are in ciao form
set rtype=`head -1 $bgReg | awk '{print $5}'`
if ( $rtype != CIAO ) then
 echo "  Error: region file does not appear to be in CIAO format. Exiting...\n\n"
 exit
endif




# Prepare the sherpa file #####################################################
# Which model is being used ?
rm -f sherpa_fit.txt
if ( $model_type == 1 ) then

cat <<EOF > sherpa_fit.txt
% Sherpa Commands to fit the pi spectra in the temperature map script MEKAL with absorption
data region.pi
back bg_pi.fits
% subtract the background spectrum
subtract
% Ensure this energy range matches the dmcopy in tzmap.csh
ignore energy :${en_lo}, ${en_hi}:
% Set up model: an absorbed mekal plasma
xswabs[abs]
xsmekal[mk]
source = abs * mk
% Absorption component: absorbing nh column density can be obtained from the heasoft nh program
% The units are 10^22/cm^2
abs.nH = ${abs_nh}
freeze abs.nh
% MEKAL component 
mk.kT = ${init_temp}
mk.kT.min = 0.01
mk.kT.max = 20.0
freeze mk.kT
mk.nh = 0.01
freeze mk.nH
mk.Abund = ${init_metals}
mk.Abund.min = 0.1
mk.Abund.max = 10
freeze mk.Abund
mk.Redshift = ${redshift}
freeze mk.Redshift
freeze mk.switch
mk.norm.min = 0
mk.norm.max = 1000.0
thaw mk.norm 
% Set the fitting method
statistic CHI GEHRELS
fit
thaw mk.kT
fit
${metals_fit} mk.Abund
fit
show source
goodness
${showplot} fit data
% These are used to flag the different nh values and should not be changed
freeze mk.nH
thaw abs.nh
${showexit}
EOF

else

cat <<EOF > sherpa_fit.txt
% Sherpa Commands to fit the pi spectra in the temperature map script MEKAL + power law with absorption
data region.pi
back bg_pi.fits
% subtract the background spectrum
subtract
% Ensure this energy range matches the dmcopy in tzmap.csh
ignore energy :${en_lo}, ${en_hi}:
% Set up model: an absorbed mekal plasma + power law
xswabs[abs]
xsmekal[mk]
xspowerlaw[pwr]
source = abs * (mk + pwr)
% Initialise fit parameters 
% absorbing nh column density can be obtained from the  heasoft nh program
% The units are 10^22/cm^2
abs.nH = ${abs_nh}
freeze abs.nh
% MEKAL component 
mk.kT = ${init_temp}
mk.kT.min = 0.01
mk.kT.max = 20.0
freeze mk.kT
mk.nh = 0.01
freeze mk.nH
mk.Abund = ${init_metals}
mk.Abund.min = 0.1
mk.Abund.max = 10
freeze mk.Abund
mk.Redshift = ${redshift}
freeze mk.Redshift
freeze mk.switch
mk.norm.min = 0
mk.norm.max = 1000.0
thaw mk.norm 
% power law component 
pwr.norm = 1.0
pwr.norm.min = 0
pwr.norm.max = 1e+24
pwr.PhoIndx.min = -3
pwr.PhoIndx.max = 10
pwr.PhoIndx = 1
freeze pwr.PhoIndx
% Set the fitting method
statistic CHI GEHRELS
fit
thaw mk.kT
fit
thaw pwr.PhoIndx
fit
${metals_fit} mk.Abund
fit
show source
goodness
${showplot} fit data
% These are used to flag the different nh values and should not be changed
freeze mk.nH
thaw abs.nh
$showexit
EOF
endif

# Find aspect offset file
set asol_files = `ls *asol*.fits | wc -w`
if ( $asol_files == 1 ) then
   set asol = `ls *asol*.fits`
   echo 'Using aspect offset file ' $asol
else if ( $asol_files == 0 ) then
   echo 'No asol file found'
   exit
else
   echo -n 'Enter name of asol file >'
   set asol = $<
endif

#Restrict energies included in events list to the those used in the sherpa fit
# NB ENERGY MUST BE IN eV NOT keV
set energy_lo = `echo $en_lo 1000 | awk '{print $1*$2}'`
set energy_hi = `echo $en_hi 1000 | awk '{print $1*$2}'`
dmcopy $events_all"[events][energy="${energy_lo}":"${energy_hi}"]" acis_events_restricted.fits clobber=yes
set events = acis_events_restricted.fits

set coarseX=$nxpix
set coarseY=$nypix
set fineX=0
set fineY=0
set layers=0
set binsfitted=1

while ( $binsfitted != 0 )
  echo 'Forming ' ${nxpix} ' x ' ${nypix} 'layer'
  if ( $showexit == exit ) then
  echo 'Check the progress of the script in' log${nxpix}_${nypix}'...'
    tcsh /data1/rft/rftscripts/tzmap.csh $events $asol $bgReg $xhi $xlo $yhi $ylo $nxpix $nypix $mincounts $model_type $metals_fit $bkg_events >& log${nxpix}_${nypix}
#     /data1/rft/rftscripts/tzmap.csh $events $asol $bgReg $xhi $xlo $yhi $ylo $nxpix $nypix $mincounts $model_type $metals_fit $bkg_events |& tee log${nxpix}_${nypix}
#     uncheck the above bit if you want piles of stuff output to the screen.
  else
    tcsh /data1/rft/rftscripts/tzmap.csh $events $asol $bgReg $xhi $xlo $yhi $ylo $nxpix $nypix $mincounts $model_type $metals_fit $bkg_events > log${nxpix}_${nypix}
  endif

  set binsfitted = `grep 'Number_of_bins_fitted=' log${nxpix}_${nypix} | awk '{print $2}'`

  if ( $binsfitted != 0 ) then
     @ layers++
  endif
  @ nxpix = ${nxpix} * 2
  @ nypix = ${nypix} * 2
end

echo 'Fitted ' $layers ' layers'
if ( $layers < 2 ) then
   exit
endif

# return binsizes to the last value which gave some fitted bins
@ nxpix = ${nxpix} / 4
@ nypix = ${nypix} / 4

# overlay the different images
@ fineX = ${coarseX} * 2
@ fineY = ${coarseY} * 2
set file1 = kt${coarseX}_${coarseY}.txt
set file2 = kt${fineX}_${fineY}.txt
set file3 = red_chi${coarseX}_${coarseY}.txt
set file4 = red_chi${fineX}_${fineY}.txt
set file5 = metallicity${coarseX}_${coarseY}.txt
set file6 = metallicity${fineX}_${fineY}.txt
set file7 = photon_index${coarseX}_${coarseY}.txt
set file8 = photon_index${fineX}_${fineY}.txt

while ( $fineX <= $nxpix )
# temperature map
    echo 'Overlaying images '$file1 ' ' $file2
    ~da/bin/combine.exe $coarseX $fineX $coarseY $fineY $file1 $file2
    mv combinedImage.txt combinedImage.txt~
# chi-squared map
    echo 'Overlaying images '$file3 ' ' $file4
    ~da/bin/combine.exe $coarseX $fineX $coarseY $fineY $file3 $file4
    mv combinedImage.txt combined_red_chi.txt
# metallicity map
      if ( $metals_fit == thaw ) then
        echo 'Overlaying images '$file5 ' ' $file6
        ~da/bin/combine.exe $coarseX $fineX $coarseY $fineY $file5 $file6
        mv combinedImage.txt combined_metallicity.txt
      endif
# photon index
      if ( $model_type == 2 ) then
	echo echo 'Overlaying images '$file7 ' ' $file8
	~da/bin/combine.exe $coarseX $fineX $coarseY $fineY $file7 $file8
	mv combinedImage.txt combined_photon_index.txt
      endif

    @ fineX = ${fineX} * 2
    @ fineY = ${fineY} * 2
    @ coarseX = ${coarseX} * 2
    @ coarseY = ${coarseY} * 2
    set file1 = combinedImage.txt~
    set file2 = kt${fineX}_${fineY}.txt
    set file3 = combined_red_chi.txt
    set file4 = red_chi${fineX}_${fineY}.txt
    set file5 = combined_metallicity.txt
    set file6 = metallicity${fineX}_${fineY}.txt
    set file7 = combined_photon_index.txt
    set file8 = photon_index${fineX}_${fineY}.txt
end

# convert tempearture map to an sdf
cat ~da/AUX/astheader combinedImage.txt~ > temp.txt
txt2ast inp='temp.txt' out='kt_combined'
rm temp.txt
# convert chi-squared map to an sdf 
sed s/Temperature/'Reduced Chi squared'/ < ~da/AUX/astheader > temp_astheader
cat temp_astheader combined_red_chi.txt > temp.txt
txt2ast inp='temp.txt' out='red_chi_combined'
rm temp.txt
rm temp_astheader
# convert metallicity map to an sdf
if ( $metals_fit == thaw ) then
  sed s/Temperature/Metallicity/ < ~da/AUX/astheader > temp_astheader 
  cat temp_astheader combined_metallicity.txt > temp.txt
  txt2ast inp='temp.txt' out='metallicity_combined'
  rm temp.txt
  rm temp_astheader
endif
# convert photon index map to an sdf
if ( $model_type == 2 ) then
  sed s/Temperature/'Photon Index'/ < ~da/AUX/astheader > temp_astheader 
  cat temp_astheader combined_photon_index.txt > temp.txt
  txt2ast inp='temp.txt' out='photon_index_combined'
  rm temp.txt
  rm temp_astheader
endif

exit
