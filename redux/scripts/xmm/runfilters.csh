#/bin/csh
#	source runfilters.csh evt2_c7_clean.fits bkg_evt2_c7.fits asol.fits 
#       sources_mod.reg bkg_sub.reg circ2_75arcsec.reg 2 |& tee log.txt
#	This script is used to generate a set of exposure corrected images
#       around four energy bands. This script needs an input data file (EVENTS)
#       and a corresponding blank sky background (BGEVENTS) you will need to
#       have created a source list and a background subtracted source list already. 
#	My data is located in sources/ so you may need to edit the line 
#       appropriately below. This also based on an observation around the 
#       ACIS-S3 chip so be careful if you have an ACIS_I observation...
#	Also, due to the length of time that it takes to smooth, I suggest 
#       that images should not be more than about 250 by 250 pixels across.
#       I find that this only runs if you start a new terminal, run source 
#       ~/.start and then type heasoft and ciao in , in that order. 
#
#	Version 1.0 RFT 23/08/2004
#	Version 1.1 RFT 02/09/2004 - 4 exposure maps, one set of smoothing scales 
#                                    for all data, different order of performing 
#                                    calculations. Smoothing then dividing by
#				     exposure map, finally subtracting background. 
#                                    Any negative values are going to be set to zero 
#                                    and hopefully should be out of the main cluster. 
#                                    Spectral weighting for the exposure maps are 
#                                    generated from my FORTRAN script using an fdump 
#                                    of the energy histogram. Script is called
#				     createdata.f, and the energy inputs will need to 
#                                    be inserted manually.
#	Version 1.2 RFT 10/09/2004 - going to normalise the images using the values 
#                                    generated in the filters00.txt file. This might 
#                                    help to reduce the amount of bad data values 
#                                    present in the data.
#	Version 1.3 RFT 13/09/2004 - The old smoothing scales oversmoothed the images 
#                                    considerably once they once they were normalised.
#                                    I have dropped the smoothing scale by a factor
#				     10 to compensate. The output script fimg2tab was 
#                                    returning carriage returns in the data. Using 
#                                    fimgdmp instead. Initial results look good. Also, 
#                                    need to scale the background with the same 
#                                    normalisation. A new menu system has been installed.
#       Version 1.4 RFT 04/05/2005 - Need to have another option for having only 3 
#                                    colour filters (as opposed to having four quotient 
#                                    filters). 
#
# Things to do:
# Need to determine whether circ is needed. (parameter 6). 
# Need to generate a data file to read in to get the spectral bands
# - dont want to have to keep on putting them in manually each time. 
# Put in option for normalisations. 
# Put in option to renormlise background data. 
# Option for not skycasting background data


set version=1.3
nice +19
cat <<EOF
--------------------`echo $0:t | tr '[a-z]' '[A-Z]'` version $version `date +%x`--------------------
EOF
if ( $#argv < 7 ) then
cat <<EOF

Use runfilters.csh events bgevents asol sources bkg_sources circ rad

    events	- the events data for your observation

    bgevents	- a blank sky background data file (not reprojected)

    asol	- aspect solution file (can be a stack)

    sources	- CIAO region file of all the regions in your
    		  events data set. Each region should begin on a
		  new line.

    bkg_sources	- Region file of annuli of data around the
    		  each source. This is generated from mkBkSub.pl
		  script and should have the format:
		  ellipse(3880.3,3819.5,4.591836,2.841662,52.210387)
		  -ellipse(3880.3,3819.5,2.295918,1.420831,52.210387)

    circ	- possibly a redundant varible. This asks for a CIAO
    		  region file around your source. This is used to 
                  calculate the peak energy of the energy histogram.

    rad		- This is the itnum parameter used in the smoothing 
                  algorithm. See "msmooth -help" for more details.


----Optional args----

    -nN          - Number of filters that you have in your data. This
                  most likely be 4 (for quotient filters) or 3 (for 
                  colour filters). The default is 4. Write in the 
		  form e.g -n4. The program is unable to cope with 
                  anything other than 3 or 4 at the moment. 
EOF
exit
endif

set events=$1
set back=$2
set asol=$3
set sources=$4
set bk_sub=$5
set circ=$6
set sm=$7

#check input files exist
if ( ! -e $events ) then
 echo "Error: $events does not exist"
 exit
endif

if ( ! -e $back ) then
 echo "Error: $back does not exist"
 exit
endif

if ( ! -e $asol ) then
 echo "Error: $asol does not exist"
 exit
endif

if ( ! -e $sources ) then
 echo "Error: $sources does not exist"
 exit
endif

if ( ! -e $bk_sub ) then
 echo "Error: $bk_sub does not exist"
 exit
endif

if ( ! -e $circ ) then
 echo "Error: $circ does not exist"
 exit
endif

#sort out optional args
set nfilt=4
set i=8
while ( $i <= $#argv )
 set c=`echo $argv[${i}] | cut -c1`
 if ( $c != - ) then
  echo "ERROR: argument $i doesn't begin with a -\n"
  exit
 endif
 set type=`echo $argv[${i}] | cut -c2`
 if ( $type != n ) then
  echo "ERROR: argument $i is not of type n.\n"
  echo $type $argv[${i}]
  exit
 endif
 set arg=`echo $argv[${i}] | tail +3c`
 if ( $type == n ) then
  set nfilt=$arg
  if (( $nfilt != 3 )&&( $nfilt != 4)) then
   echo " $nfilt must be 3 or 4 to work properly"
   echo "Number of filters is $nfilt"
   exit
 endif
@ i++
end
set topdir=`pwd`
set all = "1 2 3 4"
if ( $nfilt == 3) then
set all = " 1 2 3"
endif
# setting filter. 
# semi manual input here. If the file ebandranges.txt is found, then
# it will be used to generate the filters. This file is created from 
# the program findbands.f
# createdata.f will still have to be modified. 
# Manual inputs are used if file not found.
set filter = "x=4055:4405:1,y=3655:4005:1"
set energ1 = "energy=292:686"
set energ2 = "energy=686:876"
set energ3 = "energy=1241:3504"
set energ4 = "energy=1080:3504"
if ( -e ebandranges.txt ) then
 set temp1=` head -4 ebandranges.txt | awk '{print $1}'`
 set temp2=` head -4 ebandranges.txt | awk '{print $2}'`
 set e1l=`echo $temp1 | awk '{print $1}'`
 set e2l=`echo $temp1 | awk '{print $2}'`
 set e3l=`echo $temp1 | awk '{print $3}'`
 set e4l=`echo $temp1 | awk '{print $4}'`
 set e1u=`echo $temp2 | awk '{print $1}'`
 set e2u=`echo $temp2 | awk '{print $2}'`
 set e3u=`echo $temp2 | awk '{print $3}'`
 set e4u=`echo $temp2 | awk '{print $4}'`
 set energ1 = "energy=${e1l}:${e1u}"
 set energ2 = "energy=${e2l}:${e2u}"
 set energ3 = "energy=${e3l}:${e3u}"
 set energ4 = "energy=${e4l}:${e4u}"
endif

#setting normalisation scales:
#note that I have multiplied all the original numbers by
#10 as they are going to be over smoothed with the current
#smoothing scales. Basically there are no normalisations. 
#not any more RFT 04/05/05
set n1 = 1.00
set n2 = 1.00
set n3 = 1.00
set n4 = 1.00

#removing old directories

foreach i ($all)
rm -fr f{$i}/
mkdir f{$i}
end
rm -fr background/
mkdir background/
cd background
foreach i ($all)
mkdir f{$i}
end
cd ../


#extracting events data in energy bands
echo "extracting EVENTS and BGEVENTS data in $nfilt energy bands"
dmcopy "${events}[bin $filter][$energ1]" im_f1.fits clobber=yes
dmcopy "${events}[bin $filter][$energ2]" im_f2.fits clobber=yes
dmcopy "${events}[bin $filter][$energ3]" im_f3.fits clobber=yes
if ( $nfilt == 4 ) then
dmcopy "${events}[bin $filter][$energ4]" im_f4.fits clobber=yes
endif
set binevents = evt2_c7_clean_binned.fits
dmcopy "${events}[bin $filter]" $binevents clobber=yes

#generating reprojected background file
echo "reprojecting background data "
punlearn reproject_events
reproject_events infile="${back}[cols -time]" outfile=bkg_reproj.fits aspect=$asol match=$events random=0 clobber=yes
set bgevents = bkg_reproj.fits

dmcopy "${bgevents}[bin $filter][$energ1]" im_f1_bg.fits clobber=yes
dmcopy "${bgevents}[bin $filter][$energ2]" im_f2_bg.fits clobber=yes
dmcopy "${bgevents}[bin $filter][$energ3]" im_f3_bg.fits clobber=yes
if ( $nfilt == 4 ) then
dmcopy "${bgevents}[bin $filter][$energ4]" im_f4_bg.fits clobber=yes
endif
echo "removing point sources and interpolating the gaps with poissonian data from the surrounding background"
#rm $sources
#rm $bk_sub
#ln -s sources/$sources
#ln -s sources/$bk_sub
#is this really necessary? I mean really? I guess not. 

foreach i ($all)
mv im_f${i}.fits f${i}/
mv im_f${i}_bg.fits background/f${i}/
end

echo "normalising events and background data"
dmimgcalc f1/im_f1.fits none f1/im_f1_norm.fits mul weight=$n1
dmimgcalc f2/im_f2.fits none f2/im_f2_norm.fits mul weight=$n2
dmimgcalc f3/im_f3.fits none f3/im_f3_norm.fits mul weight=$n3
if ( $nfilt == 4 ) then
dmimgcalc f4/im_f4.fits none f4/im_f4_norm.fits mul weight=$n4
endif
dmimgcalc background/f1/im_f1_bg.fits none background/f1/im_f1_bg_norm.fits mul weight=$n1
dmimgcalc background/f2/im_f2_bg.fits none background/f2/im_f2_bg_norm.fits mul weight=$n2
dmimgcalc background/f3/im_f3_bg.fits none background/f3/im_f3_bg_norm.fits mul weight=$n3
if ( $nfilt == 4 ) then
dmimgcalc background/f4/im_f4_bg.fits none background/f4/im_f4_bg_norm.fits mul weight=$n4
endif
foreach i ($all)
cd f${i}
#ln -s ../sources/$sources
#ln -s ../sources/$bk_sub
# what crap coding
ln -s ../$sources
ln -s ../$bk_sub
dmfilth infile=im_f${i}_norm.fits outfile=im_f${i}_filled.fits method=POISSON srclist=@$sources bkglist=@$bk_sub randseed=123
cd ../
end

echo "just ignore these warnings (if you get any)- just me being lazy"

echo "rescaling background"
#generating the ratio of exposures
dmkeypar $bgevents EXPOSURE
set bgexp =  `pget dmkeypar value`
dmkeypar $events EXPOSURE
set exp =  `pget dmkeypar value`
set conv = `echo $exp $bgexp | awk '{print $1/$2}'`
echo $conv $bgexp $exp

dmkeypar $bgevents EXPOSURE echo+ >tmpdmp1
set bgexp = `head -1 tmpdmp1 | awk '{print $1}'`
rm tmpdmp1
dmkeypar $events EXPOSURE echo+ >tmpdmp1
set exp = `head -1 tmpdmp1 | awk '{print $1}'`
rm tmpdmp1
set conv = `echo $exp $bgexp | awk '{print $1/$2}'`
echo $conv $bgexp $exp

cd background
foreach i ($all)
cd f${i}/
dmimgcalc im_f${i}_bg_norm.fits none im_f${i}_bg_scl.fits mul weight=$conv clobber=yes
cd ../
end
cd ../

echo "identifying the pixel limits for the image"
#get_sky_limits $binevents 1
#set filter2 = `pget get_sky_limits xygrid`
#echo $filter2

#copying some of the code from get_sky_limits, as I am unable to run the script correctly
set t_string=`dmlist $binevents blocks | awk '$4 == "Image" { print $5; exit; }' - | sed -e "s/[(x)]/ /g" | cut -d " " -f 2,3`
set nx=`echo $t_string | awk '{ print $1; }' -`
set ny=`echo $t_string | awk '{ print $2; }' -`
#echo "  Image has $nx x $ny pixels"
dmcoords $binevents option=logical logicalx=0.5 logicaly=0.5
set x0=`pget dmcoords x | awk '{ printf "%.1f", $1; }' -`
set y0=`pget dmcoords y | awk '{ printf "%.1f", $1; }' -`
#echo "  Lower left (0.5,0.5) corner is x,y= $x0, $y0"
dmcoords $binevents option=logical logicalx=${nx}.5 logicaly=${ny}.5
set x1=`pget dmcoords x | awk '{ printf "%.1f", $1; }' -`
set y1=`pget dmcoords y | awk '{ printf "%.1f", $1; }' -`
#echo "  Upper right ($nx.5,$ny.5) corner is x,y= $x1, $y1"

set spec_x = "${x0}:${x1}:#${nx}"
set spec_y = "${y0}:${y1}:#${ny}"
set xygrid = "${spec_x},${spec_y}"
#echo $xygrid


set histo = histo_energy.fits
#identifying peak energy
#this area need to be looked at....
dmextract infile="${events}[sky=region($circ)][bin energy=300:10000]" outfile=$histo opt=generic clobber=yes
#Use Chips to determine the maximum value on the data.Or...
#dmstat "histo_energy.fits[cols count_rate]"
set test_string=`dmstat "${histo}[cols count_rate]" | awk '$1 == "max:" { print $2; exit; }' `
set max = `echo $test_string | awk '{ print $1; }' -`
set perc = `echo $max 500  | awk '{print $1/$2}'`
#echo $perc
set max_mod = `echo $max $perc  | awk '{print $1-$2}'`
if ( -e temp.txt ) then
rm temp.txt
endif
set t1 = temp.txt
dmlist "${histo}[count_rate > ${max_mod}][cols energy,count_rate]" data,clean >$t1
set energ_max = `tail -1 $t1 | awk '{print $1/1000}'`
echo "energy histogram peaks at "${energ_max} "keV"
#creating a dump of the output data file.
#Executing my FORTRAN program to get the spectral weights
echo "Executing createdata.f to generate spectral weighting of each energy band"
#rm out.txt
#fdump "histo_energy.fits+1" out.txt prhead=no "ENERGY COUNT_RATE" -
foreach i ($all)
if ( -e data${i}.dat ) then
rm data${i}.dat
endif
end
f77g createdata.f
a.out

set asphist = asphist_c7.fits
if ( -e $asphist ) then
rm $asphist
endif
echo "creating aspect histogram"
punlearn asphist
asphist infile="${asol}[@${events}[ccd_id=7]]" outfile=$asphist evtfile=$events clobber=yes dtffile=""

#creating $nfilt instrument maps for the different filters.
echo "creating the instrument maps"
foreach i ($all)
if ( -e instmap${i}.fits ) then
rm instmap${i}.fits
endif
punlearn mkinstmap
mkinstmap obsfile="${asphist}[asphist]" outfile=instmap${i}.fits det=ACIS-S3 pixelgrid="1:1024:#1024,1:1024:#1024" spectrumfile=data${i}.dat clobber=yes grating=NONE maskfile=NONE monoenergy=1.0
end

echo "creating the exposure maps"
foreach i ($all)
if ( -e expmap_c7_${i}.fits ) then
rm expmap_c7_${i}.fits
endif
punlearn mkexpmap
mkexpmap instmapfile=instmap${i}.fits outfile=expmap_c7_${i}.fits xygrid=$xygrid asphistfile=$asphist useavgaspect=no normalize=no clobber=yes
end
echo "here1"

#The new order is to smooth then divide then subtract. All images will be smoothed to one map.
#The suggestion was the band 3 have the data smoothed, and that band was used for processing the data.
cd f3/
msmooth im_f3_filled.fits -o temp.fits -i ${sm} -cg -S map${sm}.fits
rm temp.fits
rm temp.fits
cd ../
ln -s f3/map${sm}.fits

echo "smoothing all events images"
foreach i ($all)
cd f$i/
ln -s ../f3/map${sm}.fits
msmooth im_f${i}_filled.fits -o im_f${i}_gc${sm}.fits -i ${sm} -cgu -S map${sm}.fits
cd ../
end

echo "smoothing background images"
foreach i ($all)
cd background/f${i}
ln -s $topdir/f3/map${sm}.fits
msmooth im_f${i}_bg_scl.fits -o im_f${i}_bg_gc${sm}.fits -i ${sm} -cgu -S map${sm}.fits

cd ../../
end

echo "smoothing the exposure map"
cd $topdir
foreach i ($all)
msmooth expmap_c7_${i}.fits -o expmap_c7_${i}_gc${sm}.fits -i ${sm} -cgu -S map${sm}.fits
end

echo "dividing through by the exposure map"

foreach i ($all)
cd f$i/
ln -s ../expmap_c7_${i}_gc${sm}.fits
dmimgcalc im_f${i}_gc${sm}.fits expmap_c7_${i}_gc${sm}.fits im_f${i}_gc${sm}_exp.fits div
cd ../background/f$i
ln -s ../../expmap_c7_${i}_gc${sm}.fits
dmimgcalc im_f${i}_bg_gc${sm}.fits expmap_c7_${i}_gc${sm}.fits im_f${i}_bg_gc${sm}_exp.fits div
cd ../../

end


echo "subtracting smoothed background from events data:"
foreach i ($all)
cd f$i/
ln -s ../background/f$i/im_f${i}_bg_gc${sm}_exp.fits
dmimgcalc im_f${i}_gc${sm}_exp.fits im_f${i}_bg_gc${sm}_exp.fits f${i}_bksub_gc${sm}.fits sub
cd ../
end

rm -fr final/
mkdir final
cd final/
foreach i ($all)
ln -s ../f$i/f${i}_bksub_gc${sm}.fits
end


echo "generating the ascii files."
#Want to generate a 200 by 200 grid
set pixX = `echo $nx | awk '{print($1-200)/2}' `
set pixX2 = `echo $pixX | awk '{print$1+199}' `
echo "extracting regions around central 200 pixels:" $pixX "-" $pixX2

foreach i ($all)
fimgdmp f${i}_bksub_gc${sm}.fits f${i}.txt ${pixX} ${pixX2} ${pixX} ${pixX2}
end

tar -czf data.tar.gz *.txt
cd ../
#removing unwanted files
rm temp.txt
rm data*.dat
rm expmap*
rm instmap*
rm asphist_c7.fits
echo Analysis finished at `date`



