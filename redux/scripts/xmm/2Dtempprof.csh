#! /bin/tcsh
#
# Script to make a 2D temperature profile, with a specific number of bins out to
# a max radius rmax. This is a hack of BJM'sd radial.csh script.
# version 1.0 - 09/11/04 RFT	hacking the radial profile script. There will be
#				no adaptive binning yet in this version.
# version 1.1 - 11/01/05 RFT    argument structure changed. Blank sky backgrounds 
#                               and different scaling factors for each background 
#                               are now input. If blank-sky backgrounds are used, 
#                               then the script will use my modified version of 
#                               AMR's script - createspectra_1. In the event that 
#                               a region file is inserted in arg 14, then the 
#                               script will use BJM's xmmspec.csh script to generate 
#                               the spectra using xmmspec.csh 
#
# Uses:	xmmstring.csh
#	xmmspec.csh
#       createspectra_1
#
#
# NOTES:			I spent a long time rewriting this script to remove
#				source regions before generating spectra. However,
#				doing that still results in an events file, but the
#				GTI headers dont seem to work well with xmmspec.csh
#				The command used to generate events data with ciao
#				and heasoft:
#		dmcopy "${evt1t}[(x,y)=field()-${exreg}]" ${evt1t} clobber=yes
#		evselect table='evt.fits:EVENTS' \
#		expression='circle(22318.5,25094.5,520.63841,X,Y)' filteredset=test.fits
#				In the latter, this simply selects the region, as opposed
#				to excluding it. Im sure that there is a way to do it in
#				evselect, but it doesnt like the field() command. The way
#				that I deal with point source removal is to select the
#				annuli and put the excluded regions into one region file
#				specralregion.reg. This might look like:
#		Region file format: CIAO version 1.0
#		annulus(24337,24689,0,400)
#		-circle(22318.5,25094.5,520.63841)
#				It is simpler to do it this way as you dont have to use
#				xmmstring.csh to convert the region files into a correct
#				expression form. (although this is necessary for xmmspec.csh)



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

if ( $#argv <= 14 ) then
cat <<EOF

Script to make a 2D temperature profile of a FITS image. This is only a rough
script, and does not take into account deprojection. The input files needed are
mos1,mos2 and pn event files.

Uses the script createspectra_1 (xmmspec.csh) (which both call xmmstring.csh)

Use $0:t img xcent ycent rmax binreq bg root [-rreg]
e.g. 2Dtempprof.csh pn_clean_evt.fits mos1_clean_evt.fits mos2_clean_evt.fits 1750 2100 150 b2 back.reg tempprof_

arguments in [] are optional, and can be in any order
all coordinates/radii should be given in PHYSICAL pixels

Use $0:t help for detailed help
    evt1           - input events data 1. Please make sure these start 
    evt2	   - input events data 2  with mos1_ , mos2_ , or pn_, as 
    evt3	   - input events data 3  the root names are taken from these.
    xcent          - x centre of source in PHYSICAL coords
    ycent          - y centre of source in PHYSICAL coords
    rmax           - maximum radius (pixels)
    binreq         - the binning requirement. Either use "b" "n" "s" or "m" 
                     followed by a number, e.g.
		     "b2"    - will make bins of width 2 pixels
		     "n40"   - will make 40 equal width bins
    back1          - blank sky background file for evt1
    back2          - blank sky background file for evt2
    back3          - blank sky background file fot evt3
    backscale1     - background scaling factor for evt1
    backscale2     - background scaling factor for evt2
    backscale3     - background scaling factor for evt3
    bgreg             - a CIAO format DS9 region file from which the bg spectra will
                     be calculated. Enter "none" if using blank sky background.
    root           - root name for output files. Actually this is redundant,
                     as the root names are calculated from the events lists.
                     Please put something in here though, as root is
		     used in temporary files. 

             ------- Optional Arguments ------

    -rreg          - Exclude regions in file "reg", where reg is a
		     CIAO format ds9 region of parts of the image
                     to exclude (e.g. point sources).
 		     Save these as INCLUDED regions!
		     Note that computed areas will take this into account


EOF
exit
endif

#sort out required arguments
set evt1=$1
set evt2=$2
set evt3=$3
set x=$4
set y=$5
set rmax=$6
set binreq=$7
set back1=$8
set back2=$9
set back3=$10
set backscale1=$11
set backscale2=$12
set backscale3=$13
set bgreg=$14
set root=$15

#set default optional arguments to none
set exc=none
#sort out optional arguments
set i=16
while ( $i <= $#argv )
 set c=`echo $argv[${i}] | cut -c1`
 if ( $c != - ) then
  echo "ERROR: argument $i doesn't begin with a -\n"
  exit
 endif
 set type=`echo $argv[${i}] | cut -c2`
 if ( $type != r ) then
  echo "ERROR: argument $i  r "
  echo $type $argv[${i}]
  exit
 endif
 set arg=`echo $argv[${i}] | tail +3c`
 if ( $type == r ) then
  set exc=$arg
 endif
@ i++
end

#check software dependencies
set exit=n
#check if CIAO is installed
if ( ! $?ASCDS_INSTALL ) then
 set ciao=no
 echo "WARNING: CIAO not installed - profile will be created, and a sherpascript will be written, but won't attempt to fit the profile within the script.\n"
else set ciao=yes
endif
#check if FTOOLS is installed
if ( ! $?FTOOLS ) then
 echo "ERROR: FTOOLS not installed - required by script."
 set exit=y
endif
if ( $exit == y ) then
 exit
endif


set bgtype=none
#Find out what background is given
if ( $bgreg != none ) then
printf "%f\n" $bg >& /dev/null
  if ( $status == 1 ) then
   set bgtype=reg
   else set bgtype=num
  endif
endif

#check input files exist
if ( ! -e $evt1 ) then
 echo "Error: $evt1 does not exist"
 exit
endif
set temp1=`echo $evt1 | cut -c1-5`
set c=`echo $evt1 | cut -c5`
if ( $c != _ ) then
 set  root1=`echo $evt1 | cut -c1-3`
 else set root1=$temp1
endif

if ( ! -e $evt2 ) then
 echo "Error: $evt2 does not exist"
 exit
endif
set temp1=`echo $evt2 | cut -c1-5`
set c=`echo $evt2 | cut -c5`
if ( $c != _ ) then
 set  root2=`echo $evt2 | cut -c1-3`
 else set root2=$temp1
endif

if ( ! -e $evt3 ) then
 echo "Error: $evt3 does not exist"
 exit
endif
set temp1=`echo $evt3 | cut -c1-5`
set c=`echo $evt3 | cut -c5`
if ( $c != _ ) then
 set  root3=`echo $evt3 | cut -c1-3`
 else set root3=$temp1
endif

if ( $bgtype == reg ) then
 if ( ! -e $bg ) then
  echo "Error: $bg does not exist."
 endif
endif
if ( $exc != none ) then
 if ( ! -e $exc ) then
  echo "Error: $exc does not exist."
 endif
endif

#sort out binning requirement
set req=`echo $binreq | cut -c1`
if ( $req == b ) then
 set bwidth=`echo $binreq | tail +2c`
else if ( $req == n ) then
 set nbins=`echo $binreq | tail +2c`
else
 echo "ERROR - binreq $binreq must begin with b or n "
 exit
endif

echo "Input was $0 $*\n\n"

set pi=3.141592654
#set echo verbose


#add offset $off so no zero pixels, the after performing filtering,
#any pixel with value zero can be considered excluded from the image,
#and any pixel with value $off was actually zero in the original image
set off=1e-20
#fcarith ${root1}_evt.fits $off \!${root1}_evt.fits ADD datatype=real
#fcarith ${root2}_evt.fits $off \!${root2}_evt.fits ADD datatype=real
#fcarith ${root3}_evt.fits $off \!${root3}_evt.fits ADD datatype=real

#cant add offset while the images are still as events files.



#exclude unwanted areas
if ( $exc != none ) then
 echo "Excluding unwanted regions..."
 set reg=$exc
 #convert region to fselect expression
 #reformat region file
 grep -v CIAO $reg | grep -v '^\s*$' >! ${root}tmpreg.txt
 set numreg=`wc -l ${root}tmpreg.txt | awk '{print $1}'`
 set i=1
 #filter out each region from image
# set exreg
 while ( $i <= $numreg )
  set tmpexreg=`head -$i ${root}tmpreg.txt | tail -1`
#  echo $tmpexreg
#  set exreg${i}=$tmpexreg
#  set exreg="${exreg}-${tmpexreg}"
#  echo $exreg${i}
  @ i++
 end
# set exreg=`echo $exreg | tail +2c`

endif

#set echo verbose

set radmin=0
set radout=0
set radin=0
rm -fr spec/
mkdir spec/
cp xmmstring.csh spec/
cp atthk.dat spec/
#cp fluxcorr spec/
#do regular binning
if (( $req == b ) || ( $req == n )) then
 if ( $req == n ) then
  set bwidth=`echo $rmax $nbins | awk '{print $1/$2}'`
 endif
 echo "Binning radial profile with bin width $bwidth pixels...\n"
 #step out through radius, and record results
 set radtest=1
 while ( $radtest == 1 )
 #set echo verbose
 set radin=$radout
 set radout=`echo $radin $bwidth | awk '{print $1+$2}'`
 printf "# Region file format: CIAO version 1.0\n" >! spectralregion.reg
 printf "annulus(%g,%g,%g,%g)\n" $x $y $radin $radout  >> spectralregion.reg
 set reg=$exc
 #convert region to fselect expression
 #reformat region file
 grep -v CIAO $reg | grep -v '^\s*$' >! ${root}tmpreg.txt
 set numreg=`wc -l ${root}tmpreg.txt | awk '{print $1}'`
  set i=1
   #filter out each region from image
   set exreg
   while ( $i <= $numreg )
   set tmpexreg=`head -$i ${root}tmpreg.txt | tail -1`
#   echo $tmpexreg
   printf "-$tmpexreg\n" >> spectralregion.reg
#  set exreg${i}=$tmpexreg
#  set exreg="${exreg}-${tmpexreg}"
#  echo $exreg${i}
  @ i++
 end
  set rmid=`echo $radin $radout | awk '{print ($1+$2)/2}'`
 echo "Spectra being created using annulus("${x},${y},${radin},${radout}")"...
 cd spec/
 mkdir $rmid
 #creating spectra for all cameras
# cp ../$evt1 ${root1}_events.fits
#cp ../$evt2 ${root2}_events.fits
# cp ../$evt3 ${root3}_events.fits
# ln -s /data1/rft/rftscripts/xmmstring.csh 

if ( $root1 == pn_ ) then

/data1/rft/rftscripts/xmm_specgen.csh ../$evt1 ../spectralregion.reg ../$back1 N N atthk.dat Y Y 0 sd ${root1}${rmid} -s${backscale1} 

else 

/data1/rft/rftscripts/xmm_specgen.csh ../$evt1 ../spectralregion.reg ../$back1 N N atthk.dat Y Y 0 sdtq ${root1}${rmid} -s${backscale1}

endif

if ( $root2 == pn_ ) then

/data1/rft/rftscripts/xmm_specgen.csh ../$evt2 ../spectralregion.reg ../$back2 N N atthk.dat Y Y 0 sd ${root1}${rmid} -s${backscale2} 

else 

/data1/rft/rftscripts/xmm_specgen.csh ../$evt2 ../spectralregion.reg ../$back2 N N atthk.dat Y Y 0 sdtq ${root2}${rmid} -s${backscale2}

endif

if ( $root3 == pn_ ) then

/data1/rft/rftscripts/xmm_specgen.csh ../$evt3 ../spectralregion.reg ../$back3 N N atthk.dat Y Y 0 sd ${root3}${rmid} -s${backscale3} 

else 

/data1/rft/rftscripts/xmm_specgen.csh ../$evt3 ../spectralregion.reg ../$back3 N N atthk.dat Y Y 0 sdtq ${root3}${rmid} -s${backscale3}

endif


 mv *${root1}* *${root2}* *${root3}* $rmid
cd $rmid

#cp ../../mos1.rmf .
#cp ../../mos2.rmf .
#cp ../../pn.rmf .

#/data1/rft/rftscripts/doublesubtraction sp_s_mos1_${rmid}.fits sp_scb_mos1_${rmid}.fits sp_bs_mos1_${rmid}_dbs.fits mos1_dbs.fits sp_s_mos1_${rmid}.arf mos1.rmf mos1_grp_dbs.fits 20

#/data1/rft/rftscripts/doublesubtraction sp_s_mos2_${rmid}.fits sp_scb_mos2_${rmid}.fits sp_bs_mos2_${rmid}_dbs.fits mos2_dbs.fits sp_s_mos2_${rmid}.arf mos2.rmf mos2_grp_dbs.fits 20

#/data1/rft/rftscripts/doublesubtraction sp_s_pn_${rmid}.fits sp_scb_pn_${rmid}.fits sp_bs_pn_${rmid}_dbs.fits pn_dbs.fits sp_s_pn_${rmid}.arf pn.rmf pn_grp_dbs.fits 40


#if ( $bgreg == none ) then
# grppha sp_s_${root1}${rmid}.fits ${root1}grp.fits comm="chkey backfile sp_scb_${root1}${rmid}.fits & chkey respfile sp_s_${root1}${rmid}.rmf & chkey ancrfile sp_s_${root1}${rmid}.arf &  exit"
# grppha sp_s_${root2}${rmid}.fits ${root2}grp.fits comm="chkey backfile sp_scb_${root2}${rmid}.fits & chkey respfile sp_s_${root2}${rmid}.rmf & chkey ancrfile sp_s_${root2}${rmid}.arf &  exit"
# grppha sp_s_${root3}${rmid}.fits ${root3}grp.fits comm="chkey backfile sp_scb_${root3}${rmid}.fits & chkey respfile sp_s_${root3}${rmid}.rmf & chkey ancrfile sp_s_${root3}${rmid}.arf &  exit"
#endif

 cd ../../
 rm *tmpreg*

 set radtest=`echo $radout $rmax | awk '{if ($1>=$2) print 0; if ($1<$2) print 1}'`
 end
endif

