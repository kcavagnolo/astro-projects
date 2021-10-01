#!/bin/tcsh

# Script to determine source regions over 5 different energy bands
# Inputs are 3 events files. 
# Script is based on the thread online. Uses edetect_chain for 
# point source identification

# Input data will have already been cleaned and corrected for pattern/flag
# Energy bands are as follows: 

# 0.2-0.5 keV, 0.5-2 keV, 2-4.5 keV, 4.5-7.5 keV, 7.5-12 keV







set version=1.0 # RFT 10/03/06

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

if ( $#argv != 4 ) then
cat <<EOF

Use src_detect.csh evt1 evt2 evt3 atthk 


    evt1	- input events file 1 - mos1

    evt2	- input events file 2 - mos2

    evt3	- input events file 3 - pn

    atthk	- attitude housekeeping file

EOF
exit
endif

set evt1=$1
set evt2=$2
set evt3=$3
set atthk=$4

#check input files exist
if ( ! -e $evt1 ) then
 echo "Error: $evt1 does not exist"
 exit
endif

if ( ! -e $evt2 ) then
 echo "Error: $evt2 does not exist"
 exit
endif

if ( ! -e $evt3 ) then
 echo "Error: $evt3 does not exist"
 exit
endif

if ( ! -e $atthk ) then
 echo "Error: $atthk does not exist"
 exit
endif



#1. Generate images in different energy bands

#1a. Mos1

echo "Creating images in MOS1...\n"

evselect table=${evt1}:EVENTS imagebinning='binSize' imageset='m1_image_full.fits' \
withimageset=yes xcolumn='X' ycolumn='Y' ximagebinsize=40 yimagebinsize=40 \
expression='#XMMEA_EM&&(PI in [200:12000])&&(PATTERN in [0:12])&&(FLAG==0)'

evselect table=${evt1}:EVENTS  imagebinning='binSize' imageset='m1_image_b1.fits' \
withimageset=yes xcolumn='X' ycolumn='Y' ximagebinsize=40 yimagebinsize=40 \
expression='#XMMEA_EM&&(PI in [200:500])&&(PATTERN in [0:12])&&(FLAG==0)'

evselect table=${evt1}:EVENTS  imagebinning='binSize' imageset='m1_image_b2.fits' \
withimageset=yes xcolumn='X' ycolumn='Y' ximagebinsize=40 yimagebinsize=40 \
expression='#XMMEA_EM&&(PI in [500:2000])&&(PATTERN in [0:12])&&(FLAG==0)'

evselect table=${evt1}:EVENTS  imagebinning='binSize' imageset='m1_image_b3.fits' \
withimageset=yes xcolumn='X' ycolumn='Y' ximagebinsize=40 yimagebinsize=40 \
expression='#XMMEA_EM&&(PI in [2000:4500])&&(PATTERN in [0:12])&&(FLAG==0)'

evselect table=${evt1}:EVENTS  imagebinning='binSize' imageset='m1_image_b4.fits' \
withimageset=yes xcolumn='X' ycolumn='Y' ximagebinsize=40 yimagebinsize=40 \
expression='#XMMEA_EM&&(PI in [4500:7500])&&(PATTERN in [0:12])&&(FLAG==0)'

evselect table=${evt1}:EVENTS  imagebinning='binSize' imageset='m1_image_b5.fits' \
withimageset=yes xcolumn='X' ycolumn='Y' ximagebinsize=40 yimagebinsize=40 \
expression='#XMMEA_EM&&(PI in [7500:12000])&&(PATTERN in [0:12])&&(FLAG==0)' 
 
#1b. Mos2
echo "Creating images in MOS2...\n"

evselect table=${evt2}:EVENTS imagebinning='binSize' imageset='m2_image_full.fits' \
withimageset=yes xcolumn='X' ycolumn='Y' ximagebinsize=40 yimagebinsize=40 \
expression='#XMMEA_EM&&(PI in [200:12000])&&(PATTERN in [0:12])&&(FLAG==0)'

evselect table=${evt2}:EVENTS  imagebinning='binSize' imageset='m2_image_b1.fits' \
withimageset=yes xcolumn='X' ycolumn='Y' ximagebinsize=40 yimagebinsize=40 \
expression='#XMMEA_EM&&(PI in [200:500])&&(PATTERN in [0:12])&&(FLAG==0)'

evselect table=${evt2}:EVENTS  imagebinning='binSize' imageset='m2_image_b2.fits' \
withimageset=yes xcolumn='X' ycolumn='Y' ximagebinsize=40 yimagebinsize=40 \
expression='#XMMEA_EM&&(PI in [500:2000])&&(PATTERN in [0:12])&&(FLAG==0)'

evselect table=${evt2}:EVENTS  imagebinning='binSize' imageset='m2_image_b3.fits' \
withimageset=yes xcolumn='X' ycolumn='Y' ximagebinsize=40 yimagebinsize=40 \
expression='#XMMEA_EM&&(PI in [2000:4500])&&(PATTERN in [0:12])&&(FLAG==0)'

evselect table=${evt2}:EVENTS  imagebinning='binSize' imageset='m2_image_b4.fits' \
withimageset=yes xcolumn='X' ycolumn='Y' ximagebinsize=40 yimagebinsize=40 \
expression='#XMMEA_EM&&(PI in [4500:7500])&&(PATTERN in [0:12])&&(FLAG==0)'

evselect table=${evt2}:EVENTS  imagebinning='binSize' imageset='m2_image_b5.fits' \
withimageset=yes xcolumn='X' ycolumn='Y' ximagebinsize=40 yimagebinsize=40 \
expression='#XMMEA_EM&&(PI in [7500:12000])&&(PATTERN in [0:12])&&(FLAG==0)' 

#1c. EPN
echo "Creating Images in EPN...\n"

evselect table=${evt3}:EVENTS imagebinning='binSize' imageset='pn_image_full.fits' \
withimageset=yes xcolumn='X' ycolumn='Y' ximagebinsize=80 yimagebinsize=80 \
expression='#XMMEA_EP&&(PI in [200:12000])&&(PATTERN in [0:4])&&(FLAG==0)'

evselect table=${evt3}:EVENTS  imagebinning='binSize' imageset='pn_image_b1.fits' \
withimageset=yes xcolumn='X' ycolumn='Y' ximagebinsize=80 yimagebinsize=80 \
expression='#XMMEA_EP&&(PI in [200:500])&&(PATTERN in [0:4])&&(FLAG==0)'

evselect table=${evt3}:EVENTS  imagebinning='binSize' imageset='pn_image_b2.fits' \
withimageset=yes xcolumn='X' ycolumn='Y' ximagebinsize=80 yimagebinsize=80 \
expression='#XMMEA_EP&&(PI in [500:2000])&&(PATTERN in [0:4])&&(FLAG==0)'

evselect table=${evt3}:EVENTS  imagebinning='binSize' imageset='pn_image_b3.fits' \
withimageset=yes xcolumn='X' ycolumn='Y' ximagebinsize=80 yimagebinsize=80 \
expression='#XMMEA_EP&&(PI in [2000:4500])&&(PATTERN in [0:4])&&(FLAG==0)'

evselect table=${evt3}:EVENTS  imagebinning='binSize' imageset='pn_image_b4.fits' \
withimageset=yes xcolumn='X' ycolumn='Y' ximagebinsize=80 yimagebinsize=80 \
expression='#XMMEA_EP&&(PI in [4500:7500])&&(PATTERN in [0:4])&&(FLAG==0)'

evselect table=${evt3}:EVENTS  imagebinning='binSize' imageset='pn_image_b5.fits' \
withimageset=yes xcolumn='X' ycolumn='Y' ximagebinsize=80 yimagebinsize=80 \
expression='#XMMEA_EP&&(PI in [7500:12000])&&(PATTERN in [0:4])&&(FLAG==0)' 

#2. Need to determine energy correction factors (ecf) for each camera.
# http://xmmssc-www.star.le.ac.uk/pubdocs/ssc-lux-tn-0059.shtml
#2a. Mos1
# need filter type 
echo "Determining ECF (energy correction factors) values...\n"

dmkeypar $evt1 FILTER
set f1 =  `pget dmkeypar value`
set f1mod = `echo $f1 | cut -c1`
if ( $f1mod == T ) then
 set f1mod = `echo $f1 | cut -c4`
  if ( $f1mod == n ) then
   set f1m=t
  else
   set f1m=T
  endif
endif
if ( $f1mod == M ) then
 set  f1m=m
endif
if ( $f1mod == O ) then
 set  f1m=o
endif

#2b. Mos2

dmkeypar $evt2 FILTER
set f2 =  `pget dmkeypar value`
set f2mod = `echo $f1 | cut -c1`
if ( $f2mod == T ) then
 set f2mod = `echo $f1 | cut -c4`
  if ( $f2mod == n ) then
   set f2m=t
  else
   set f2m=T
  endif
endif
if ( $f2mod == M ) then
 set  f2m=m
endif
if ( $f1mod == O ) then
 set  f1m=o
endif

#2c EPN

dmkeypar $evt2 FILTER
set f3 =  `pget dmkeypar value`
set f3mod = `echo $f1 | cut -c1`
if ( $f3mod == T ) then
 set f3mod = `echo $f1 | cut -c4`
  if ( $f3mod == n ) then
   set f3m=t
  else
   set f3m=T
  endif
endif
if ( $f3mod == M ) then
 set  f3m=m
endif
if ( $f3mod == O ) then
 set  f3m=o
endif

#3. Run edetect_chain for each camera
#3a. Mos1
echo "Running edetect_chain for MOS1...\n"

if ( $f1m == t ) then
set ecf_string='1.8035 1.9872 0.76318 0.26770 0.029085'
endif
if ( $f1m == T ) then
set ecf_string='1.0207 1.6274 0.72354 0.26339 0.028949'
endif
if ( $f1m == o ) then
set ecf_string='3.0305 2.1646 0.76051 0.26241 0.028435'
endif
if ( $f1m == m ) then
set ecf_string='1.6050 1.9451 0.75455 0.26727 0.029071'
endif
#echo $ecf_string



edetect_chain imagesets='"m1_image_b1.fits" "m1_image_b2.fits" "m1_image_b3.fits" "m1_image_b4.fits" "m1_image_b5.fits"' \
eventsets=${evt1} attitudeset=${atthk} \
pimin='200 500 2000 4500 7500' pimax='500 2000 4500 7500 12000' \
ecf="${ecf_string}" \
eboxl_list='m1_eboxlist_l.fits' eboxm_list='m1_eboxlist_m.fits' \
esp_nsplinenodes=16 eml_list='m1_emllist.fits' esen_mlmin=15



#3b. Mos2
echo "Running edetect_chain for MOS2...\n"

if ( $f2m == t ) then
set ecf_string='1.8035 1.9872 0.76318 0.26770 0.029085'
endif
if ( $f2m == T ) then
set ecf_string='1.0207 1.6274 0.72354 0.26339 0.028949'
endif
if ( $f2m == o ) then
set ecf_string='3.0305 2.1646 0.76051 0.26241 0.028435'
endif
if ( $f2m == m ) then
set ecf_string='1.6050 1.9451 0.75455 0.26727 0.029071'
endif
#echo $ecf_string

edetect_chain imagesets='"m2_image_b1.fits" "m2_image_b2.fits" "m2_image_b3.fits" "m2_image_b4.fits" "m2_image_b5.fits"' \
eventsets=${evt2} attitudeset=${atthk} \
pimin='200 500 2000 4500 7500' pimax='500 2000 4500 7500 12000' \
ecf="${ecf_string}" \
eboxl_list='m2_eboxlist_l.fits' eboxm_list='m2_eboxlist_m.fits' \
esp_nsplinenodes=16 eml_list='m2_emllist.fits' esen_mlmin=15


#3c. Epn
echo "Running edetect_chain for EPN...\n"
if ( $f1m == t ) then
set ecf_string='10.596 6.8157 2.0542 0.99483 0.25933'
endif
if ( $f1m == T ) then
set ecf_string='5.6883 5.4768 1.9428 0.97991 0.25819'
endif
if ( $f1m == o ) then
set ecf_string='18.350 7.6882 2.0683 0.98474 0.25609'
endif
if ( $f2m == m ) then
set ecf_string='9.2851 6.6525 2.0296 0.99334 0.25926'
endif
#echo $ecf_string

edetect_chain imagesets='"pn_image_b1.fits" "pn_image_b2.fits" "pn_image_b3.fits" "pn_image_b4.fits" "pn_image_b5.fits"' \
eventsets=${evt3} attitudeset=${atthk} \
pimin='200 500 2000 4500 7500' pimax='500 2000 4500 7500 12000' \
ecf="${ecf_string}" \
eboxl_list='pn_eboxlist_l.fits' eboxm_list='pn_eboxlist_m.fits' \
esp_nsplinenodes=16 eml_list='pn_emllist.fits' esen_mlmin=15 

