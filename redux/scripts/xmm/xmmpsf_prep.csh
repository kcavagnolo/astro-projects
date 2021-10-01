#!/bin/tcsh -f
#
# script to prepare spectra for fitting using the XMM PSF model in 
# xspec. Spectra require an image with specific binning to be in their
# zeroth extension. This script should do that. Note below that dmcopy 
# is used rather than evselect to generate the WMAP images. I don't know
# why, but evselect just doesn't work - probably some unspecified keyword
# issue...
#
# v1.0 EJOS 24/4/06
set script_ver=1.1
# v1.1 RFT  20/03/2007
# The new version ensures a list of region files to be input in CIAO format
# This way, you can account for point sources when extracting the images. 

cat <<EOF

==================== XMMPSF_PREP.CSH version $script_ver ======================

EOF

if ($#argv != 5) then
cat <<EOF

Usage: xmmpsf_prep.csh m1evt m2evt pnevt root regionfile

where: ??evt      - the events files from which spectra were extracted
       root       - the rootname of the spectra in use
       list       - the a list of the region files used to create the spectra
       
Example: xmmpsf_prep.csh m1_evt.fits m2_evt.fits pn_evt.fits ann8000 regions.lis

EOF
exit
endif

echo "Your input: "$0:t $*

set m1evt=$1
set m2evt=$2
set pnevt=$3
set root=$4
set regfile=$5

#params of images
set wbin=64
set skybin=32
set pixsize=4.444444444444448e-04; #degrees

#check that input files exist

set tmp1=`echo $m1evt | cut -d ":" -f1`
if ( ! -e $tmp1 ) then
 echo "Error: $tmp1 does not exist"
 exit
endif

set tmp1=`echo $m2evt | cut -d ":" -f1`
if ( ! -e $tmp1 ) then
 echo "Error: $tmp1 does not exist"
 exit
endif

set tmp1=`echo $pnevt | cut -d ":" -f1`
if ( ! -e $tmp1 ) then
 echo "Error: $tmp1 does not exist"
 exit
endif

set tmp1=`echo $regfile | cut -d ":" -f1`
if ( ! -e $tmp1 ) then
 echo "Error: $tmp1 does not exist"
 exit
endif

set nreg=`wc -l $regfile | awk '{print $1}'`

set n=1

while ($n <= $nreg)
#Need to determine whether the region file has pont sources in. 
# I am assuming that the region files have a CIAO header present.
 set region=`head -$n $regfile | tail -1` 
 set nlines=`wc -l  $region | awk '{print $1}'` 
 if ( $nlines == 2 ) then
  set reg=`tail -1 $region`
 else
     if ( -e temp.reg ) then
      rm temp.reg
     endif
  touch temp.reg
  set m=2
     while ( $m <= $nlines )
      set tmp2=`head -$m $region | tail -1`
      printf $tmp2 >> temp.reg
      @ m ++
     end
  set reg=`tail -1 temp.reg`
  rm temp.reg
 endif
  echo "\n Region used is: $reg \n"


 #m1 first
 dmcopy "${m1evt}[bin detx=::${wbin},dety=::${wbin}][(x,y)=${reg}][opt null=-1,type=r4]" m1_tmp_im.fits clobber=yes
 fimgtrim infile=m1_tmp_im.fits threshlo=1e-6 threshup=INDEF const_lo=-1 const_up=INDEF outfile=m1_tmp_im.fits clobber=yes
 fparkey WMAP m1_tmp_im.fits+0 HDUNAME add=yes
 fparkey $wbin m1_tmp_im.fits+0 WMREBIN add=yes comm="Weighted Map rebinning"
 fparkey $pixsize m1_tmp_im.fits+0 PIXSIZE add=yes comm="Image binsize (degrees)"
 fparkey $skybin m1_tmp_im.fits+0 SKYBIN add=yes comm="Image binning factor"
 fextract m1_tmp_im.fits+0 m1_tmp_im.fits clobber=yes
 fappend sp_scb20_mos1_an${n}.fits"[spectrum]" m1_tmp_im.fits
 set regext=`fstruct sp_scb20_mos1_an${n}.fits | grep REG0 | awk '{print $3}'`
 set regext_test=`echo $regext | wc -w `
 if ($regext_test != 1) then
  echo "problem copying region extension in mos 1 spectrum "$n
  exit
 else
  fappend sp_scb20_mos1_an${n}.fits"[$regext]" m1_tmp_im.fits
 endif
 mv m1_tmp_im.fits ${root}_psf_m1_${n}_src.fits

 #repeat for m2
 dmcopy "${m2evt}[bin detx=::${wbin},dety=::${wbin}][(x,y)=${reg}][opt null=-1,type=r4]" m2_tmp_im.fits clobber=yes
 fimgtrim infile=m2_tmp_im.fits threshlo=1e-6 threshup=INDEF const_lo=-1 const_up=INDEF outfile=m2_tmp_im.fits clobber=yes
 fparkey WMAP m2_tmp_im.fits+0 HDUNAME add=yes
 fparkey $wbin m2_tmp_im.fits+0 WMREBIN add=yes comm="Weighted Map rebinning"
 fparkey $pixsize m2_tmp_im.fits+0 PIXSIZE add=yes comm="Image binsize (degrees)"
 fparkey $skybin m2_tmp_im.fits+0 SKYBIN add=yes comm="Image binning factor"
 fextract m2_tmp_im.fits+0 m2_tmp_im.fits clobber=yes
 fappend sp_scb20_mos2_an${n}.fits"[spectrum]" m2_tmp_im.fits
 set regext=`fstruct sp_scb20_mos2_an${n}.fits | grep REG0 | awk '{print $3}'`
 set regext_test=`echo $regext | wc -w `
 if ($regext_test != 1) then
  echo "problem copying region extension in mos 2 spectrum "$n
  exit
 else
  fappend sp_scb20_mos2_an${n}.fits"[$regext]" m2_tmp_im.fits
 endif
 mv m2_tmp_im.fits ${root}_psf_m2_${n}_src.fits


 #and then for pn

  dmcopy "${pnevt}[bin detx=::${wbin},dety=::${wbin}][(x,y)=${reg}][opt null=-1,type=r4]" pn_tmp_im.fits clobber=yes
 fimgtrim infile=pn_tmp_im.fits threshlo=1e-6 threshup=INDEF const_lo=-1 const_up=INDEF outfile=pn_tmp_im.fits clobber=yes
 fparkey WMAP pn_tmp_im.fits+0 HDUNAME add=yes
 fparkey $wbin pn_tmp_im.fits+0 WMREBIN add=yes comm="Weighted Map rebinning"
 fparkey $pixsize pn_tmp_im.fits+0 PIXSIZE add=yes comm="Image binsize (degrees)"
 fparkey $skybin pn_tmp_im.fits+0 SKYBIN add=yes comm="Image binning factor"
 fextract pn_tmp_im.fits+0 pn_tmp_im.fits clobber=yes
 fappend sp_scb40_pn_an${n}.fits"[spectrum]" pn_tmp_im.fits
 set regext=`fstruct sp_scb40_pn_an${n}.fits | grep REG0 | awk '{print $3}'`
 set regext_test=`echo $regext | wc -w `
 if ($regext_test != 1) then
  echo "problem copying region extension in mos 1 spectrum "$n
  exit
 else
  fappend sp_scb40_pn_an${n}.fits"[$regext]" pn_tmp_im.fits
 endif
 mv pn_tmp_im.fits ${root}_psf_pn_${n}_src.fits
 #
 @ n ++
end


exit
