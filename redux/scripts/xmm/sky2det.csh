#!/bin/tcsh

# Script to convert CIAO region files into detector coordinates. 
# Ok this is going to be pretty tricky. 
  
# This script is being generated as part of Steve Snowden's method 
# of background subtraction. There will be no CIAO statement at
# the start of the new file. 
 
# As esky2det requires an input of ra and dec, there will have to 
# be a conversion between physical and ra/dec before getting to 
# DETX/ DETY coordinates. Think its best just to start with region files
# saved in WCS coordinates. Note that the radii are in arcmin.

# Input files will be a region file and a template file for the 
# conversion. 

# esky2det ra=05h47m38.145s dec=-31d52m16.95s outunit=det instrument=EMOS2 calinfostyle=set calinfoset=mos2_raw.fits:+1
#
# Region files should be in the format

# CIAO blah blah
# circle(x,y,rad)
# -circle(x,y,rad) etc

#or

# CIAO blah
# annulus(x,y,rin,rout) 
# -circle(x,y,rad) etc

# i.e. all excluded regions are circles. 
# 
# e.g
# Region file format: CIAO version 1.0
#annulus(05:47:38.145,-31:52:16.95,0',0.50352')



set version=1.0

# Version 1.0 RFT 02/09/05

cat <<EOF

--------------------`echo $0:t | tr '[a-z]' '[A-Z]'` version $version `date +%x`------------------

EOF


if ( $#argv != 3 ) then
cat <<EOF

Use sky2det.csh region template output

    region      - Your region file to be converted. 
                  should be in ra/dec coordinates.
                  
    template    - A template file for the camera. 
                  NOTE: must be MOS only.
                  This should be an events file.

    output      - the name of your output file



EOF
exit
endif

set region=$1
set events=$2
set output=$3

#check input files exist.

if ( ! -e $region ) then
 echo "Error: $region does not exist. Exiting...\n"
 exit
endif

if ( ! -e $events ) then
 echo "Error: $events does not exist. Exiting...\n"
 exit
endif

#check regions are in ciao format
set rtype=`head -1 $region | awk '{print $5}'`
if ( $rtype != CIAO ) then
 echo "  ERROR - REGION FILE DOES NOT APPEAR TO BE IN CIAO FORMAT\n\n"
 exit
endif

#get instrument

if ( -e tmpdmp1 ) then
 rm -f tmpdmp1
endif
fdump $events tmpdmp1 TIME - prdata=no showcol=no showunit=no showrow=no clobber=yes
set instr=`grep INSTRUME < tmpdmp1 | head -1 | awk '{print $2}'`
set instr=`echo $instr | tr -d "'"`
rm -f tmpdmp1
echo 'Instrument is '$instr "\n"

#determine how many data points have to be adjusted. 

grep -v CIAO $region | grep -v '^\s*$' >! tmpsrc.txt
set numsrc=`wc -l tmpsrc.txt | awk '{print $1}'`
rm tmpsrc.txt

# determine whether first region is a circle(c) or annulus(a).
set str1=`head -2 $region | tail -1 | cut -c1`
if (( $str1 != c )&&( $str1 != a )) then
 echo "initial region must be a circle or an annulus. Exiting...\n"
 exit
endif

if ( -e $output ) then
 rm $output 
endif
touch $output

set i=1
while ( $i <= $numsrc ) 
echo $i
set j=`echo $i | awk '{print $1+1}'`
set x1=`head -${j} $region | tail -1 | cut -d ',' -f1 | cut -d '(' -f2`
set x1_h=`echo $x1 | cut -d ':' -f1`
set x1_m=`echo $x1 | cut -d ':' -f2`  
set x1_s=`echo $x1 | cut -d ':' -f3`
set x1_fin=`echo $x1_h $x1_m $x1_s | awk '{print 15*($1+($2/60)+($3/3600))}'`

set y1=`head -${j} $region | tail -1 | cut -d ',' -f2`
set y1_d=`echo $y1 | cut -d ':' -f1`
set y1_m=`echo $y1 | cut -d ':' -f2`  
set y1_s=`echo $y1 | cut -d ':' -f3`
#check sign on y1_d
set sign=`echo $y1_d | cut -c1`
if ( $sign == - ) then
 set y1_fin=`echo $y1_d $y1_m $y1_s | awk '{print ($1-($2/60)-($3/3600))}'`
else
 set y1_fin=`echo $y1_d $y1_m $y1_s | awk '{print ($1+($2/60)+($3/3600))}'`
endif

if ( $i == 1 ) then
 #If initial region is a circle
 if ( $str1 == c ) then
  set r=`head -${j} $region | tail -1 | cut -d ',' -f3 | cut -d "'" -f1`
  set r=`echo $r | awk '{print $1/60}'` #now in degrees
  #need to setup a coordinate conversion system. 
  # add this radius to the declination to get xprime, yprime
  set y_prime=`echo $y1_fin $r | awk '{print $1+$2}'`
  #need to generate string expressions for input into esky2det
  set x_str=`echo ${x1_h}"h"${x1_m}"m"${x1_s}"s"`
  set y_str=`echo ${y1_d}"d"${y1_m}"m"${y1_s}"s"`
  set x_str_prime=`echo ${x1_h}"h"${x1_m}"m"${x1_s}"s"`
 
  set y1_d_prime=`echo $y_prime | cut -d '.' -f1`  
  set y1_m_prime=`echo $y_prime | cut -d '.' -f2` 
  set y1_tmp=`echo "0."${y1_m_prime} `
  set y1_m_prime=`echo $y1_tmp | awk '{print $1*60}' | cut -d '.' -f1`
  set y1_tmp2=`echo $y1_tmp | awk '{print $1*60}' | cut -d '.' -f2`
  set y1_s_prime=`echo "0."${y1_tmp2} | awk '{print $1*60}'`
  set y_str_prime=`echo ${y1_d_prime}"d"${y1_m_prime}"m"${y1_s_prime}"s"`
  #running esky2det for central coordinates
  esky2det ra=${x_str} dec=${y_str} outunit=det instrument=${instr} calinfostyle=set calinfoset=${events}:+1 >! tmpfile
  set detx=`grep "    " < tmpfile | awk '{print $1}' | cut -d '#' -f2`
  set dety=`grep "    " < tmpfile | awk '{print $2}' | cut -d 'X' -f2`
  rm tmpfile
  #running esky2det for primed coordinates. 
  #This is to get a distance conversion for radii
  esky2det ra=${x_str_prime} dec=${y_str_prime} outunit=det instrument=${instr} calinfostyle=set calinfoset=${events}:+1 >! tmpfile 
  set detx_p=`grep "    " < tmpfile | awk '{print $1}' | cut -d '#' -f2`
  set dety_p=`grep "    " < tmpfile | awk '{print $2}' | cut -d 'X' -f2`
  rm tmpfile
  #using pythag to get radius
  set detr=`echo $detx $dety $detx_p $dety_p | awk '{print sqrt( ($1-$3)^2 + ($2-$4)^2 )}'` 
  echo "A radius of $r degrees corresponds to a radius of $detr in DETECTOR coordinates. Please Check.\n"
  #writing data to file
  echo "((DETX,DETY) IN circle(${detx},${dety},${detr}))" >> $output
 endif 

#If initial region is an annulus
 if ( $str1 == a ) then
  set r1=`head -${j} $region | tail -1 | cut -d ',' -f3 | cut -d "'" -f1`
  set r1=`echo $r1 | awk '{print $1/60}'` #now in degrees
  set r2=`head -${j} $region | tail -1 | cut -d ',' -f4 | cut -d "'" -f1`
  set r2=`echo $r2 | awk '{print $1/60}'` #again in degrees
 if ( $r1 == 0 ) then #i.e. its a circle
  echo "Initial annulus is a circle"  
  set y_prime=`echo $y1_fin $r2 | awk '{print $1+$2}'`
  #need to generate string expressions for input into esky2det
  set x_str=`echo ${x1_h}"h"${x1_m}"m"${x1_s}"s"`
  set y_str=`echo ${y1_d}"d"${y1_m}"m"${y1_s}"s"`
  set x_str_prime=`echo ${x1_h}"h"${x1_m}"m"${x1_s}"s"`

  set y1_d_prime=`echo $y_prime | cut -d '.' -f1`  
  set y1_m_prime=`echo $y_prime | cut -d '.' -f2` 
  set y1_tmp=`echo "0."${y1_m_prime} `
  set y1_m_prime=`echo $y1_tmp | awk '{print $1*60}' | cut -d '.' -f1`
  set y1_tmp2=`echo $y1_tmp | awk '{print $1*60}' | cut -d '.' -f2`
  set y1_s_prime=`echo "0."${y1_tmp2} | awk '{print $1*60}'`
  set y_str_prime=`echo ${y1_d_prime}"d"${y1_m_prime}"m"${y1_s_prime}"s"`

  #running esky2det for central coordinates
  esky2det ra=${x_str} dec=${y_str} outunit=det instrument=${instr} calinfostyle=set calinfoset=${events}:+1 >! tmpfile
  set detx=`grep "    " < tmpfile | awk '{print $1}' | cut -d '#' -f2`
  set dety=`grep "    " < tmpfile | awk '{print $2}' | cut -d 'X' -f2`
  rm tmpfile
 
  #running esky2det for primed coordinates. 
  #This is to get a distance conversion for radii
  esky2det ra=${x_str_prime} dec=${y_str_prime} outunit=det instrument=${instr} calinfostyle=set calinfoset=${events}:+1 >! tmpfile 
  set detx_p=`grep "    " < tmpfile | awk '{print $1}' | cut -d '#' -f2`
  set dety_p=`grep "    " < tmpfile | awk '{print $2}' | cut -d 'X' -f2`
  rm tmpfile
  #using pythag to get radius
  set detr=`echo $detx $dety $detx_p $dety_p | awk '{print sqrt( ($1-$3)^2 + ($2-$4)^2 )}'` 
  echo "A radius of $r2 degrees corresponds to a radius of $detr in DETECTOR coordinates. Please Check.\n"
  #writing data to file
  echo "((DETX,DETY) IN circle(${detx},${dety},${detr}))" >> $output
 else #i.e. is is actually an annulus
  #going to need r1 and r2
  set y_prime_r1=`echo $y1_fin $r1 | awk '{print $1+$2}'`
  set y_prime_r2=`echo $y1_fin $r2 | awk '{print $1+$2}'`
  
  set x_str=`echo ${x1_h}"h"${x1_m}"m"${x1_s}"s"`
  set y_str=`echo ${y1_d}"d"${y1_m}"m"${y1_s}"s"`
  set x_str_prime=`echo ${x1_h}"h"${x1_m}"m"${x1_s}"s"`

  set y1_d_pr_r1=`echo $y_prime_r1 | cut -d '.' -f1`  
  set y1_m_pr_r1=`echo $y_prime_r1 | cut -d '.' -f2` 
  set y1_tmp=`echo "0."${y1_m_pr_r1} `
  set y1_m_pr_r1=`echo $y1_tmp | awk '{print $1*60}' | cut -d '.' -f1`
  set y1_tmp2=`echo $y1_tmp | awk '{print $1*60}' | cut -d '.' -f2`
  set y1_s_pr_r1=`echo "0."${y1_tmp2} | awk '{print $1*60}'`
  set y_str_pr_r1=`echo ${y1_d_pr_r1}"d"${y1_m_pr_r1}"m"${y1_s_pr_r1}"s"`

  set y1_d_pr_r2=`echo $y_prime_r2 | cut -d '.' -f1`  
  set y1_m_pr_r2=`echo $y_prime_r2 | cut -d '.' -f2` 
  set y1_tmp=`echo "0."${y1_m_pr_r2} `
  set y1_m_pr_r2=`echo $y1_tmp | awk '{print $1*60}' | cut -d '.' -f1`
  set y1_tmp2=`echo $y1_tmp | awk '{print $1*60}' | cut -d '.' -f2`
  set y1_s_pr_r2=`echo "0."${y1_tmp2} | awk '{print $1*60}'`
  set y_str_pr_r2=`echo ${y1_d_pr_r2}"d"${y1_m_pr_r2}"m"${y1_s_pr_r2}"s"`

  #running esky2det for central coordinates
  esky2det ra=${x_str} dec=${y_str} outunit=det instrument=${instr} calinfostyle=set calinfoset=${events}:+1 >! tmpfile
  set detx=`grep "    " < tmpfile | awk '{print $1}' | cut -d '#' -f2`
  set dety=`grep "    " < tmpfile | awk '{print $2}' | cut -d 'X' -f2`
  rm tmpfile
 
  #running esky2det for primed coordinates. (r1 and r2)
  esky2det ra=${x_str_prime} dec=${y_str_pr_r1} outunit=det instrument=${instr} calinfostyle=set calinfoset=${events}:+1 >! tmpfile 
  set detx_p1=`grep "    " < tmpfile | awk '{print $1}' | cut -d '#' -f2`
  set dety_p1=`grep "    " < tmpfile | awk '{print $2}' | cut -d 'X' -f2`
  rm tmpfile
  esky2det ra=${x_str_prime} dec=${y_str_pr_r2} outunit=det instrument=${instr} calinfostyle=set calinfoset=${events}:+1 >! tmpfile 
  set detx_p2=`grep "    " < tmpfile | awk '{print $1}' | cut -d '#' -f2`
  set dety_p2=`grep "    " < tmpfile | awk '{print $2}' | cut -d 'X' -f2`
  rm tmpfile
  
  set detr1=`echo $detx $dety $detx_p1 $dety_p1 | awk '{print sqrt( ($1-$3)^2 + ($2-$4)^2 )}'`
  set detr2=`echo $detx $dety $detx_p2 $dety_p2 | awk '{print sqrt( ($1-$3)^2 + ($2-$4)^2 )}'`  

  echo " A radius of $r1 degrees corresponds to $detr1 in DET coords. " 

  echo "((DETX,DETY) IN circle(${detx},${dety},${detr2}))&&!((DETX,DETY) IN circle(${detx},${dety},${detr1}))" >> $output

 endif # circle/annulus
 endif # for str=a
endif #for i=1
  #now onto all the other point sources to exclude. 

if ( $i != 1 ) then

 echo "Excluding remaining data points"
 #All remaining sources will be circles so apply above code. 
 set r=`head -${j} $region | tail -1 | cut -d ',' -f3 | cut -d "'" -f1`
 set r=`echo $r | awk '{print $1/60}'` #now in degrees
 #need to setup a coordinate conversion system. 
 # add this radius to the declination to get xprime, yprime
 set y_prime=`echo $y1_fin $r | awk '{print $1+$2}'`
 #need to generate string expressions for input into esky2det
 set x_str=`echo ${x1_h}"h"${x1_m}"m"${x1_s}"s"`
 set y_str=`echo ${y1_d}"d"${y1_m}"m"${y1_s}"s"`
 set x_str_prime=`echo ${x1_h}"h"${x1_m}"m"${x1_s}"s"`
 
 set y1_d_prime=`echo $y_prime | cut -d '.' -f1`  
 set y1_m_prime=`echo $y_prime | cut -d '.' -f2` 
 set y1_tmp=`echo "0."${y1_m_prime} `
 set y1_m_prime=`echo $y1_tmp | awk '{print $1*60}' | cut -d '.' -f1`
 set y1_tmp2=`echo $y1_tmp | awk '{print $1*60}' | cut -d '.' -f2`
 set y1_s_prime=`echo "0."${y1_tmp2} | awk '{print $1*60}'`
 set y_str_prime=`echo ${y1_d_prime}"d"${y1_m_prime}"m"${y1_s_prime}"s"`
 #running esky2det for central coordinates
 esky2det ra=${x_str} dec=${y_str} outunit=det instrument=${instr} calinfostyle=set calinfoset=${events}:+1 >! tmpfile
 set detx=`grep "    " < tmpfile | awk '{print $1}' | cut -d '#' -f2`
 set dety=`grep "    " < tmpfile | awk '{print $2}' | cut -d 'X' -f2`
 rm tmpfile
 #running esky2det for primed coordinates. 
 #This is to get a distance conversion for radii
 esky2det ra=${x_str_prime} dec=${y_str_prime} outunit=det instrument=${instr} calinfostyle=set calinfoset=${events}:+1 >! tmpfile 
 set detx_p=`grep "    " < tmpfile | awk '{print $1}' | cut -d '#' -f2`
 set dety_p=`grep "    " < tmpfile | awk '{print $2}' | cut -d 'X' -f2`
 rm tmpfile
 #using pythag to get radius
 set detr=`echo $detx $dety $detx_p $dety_p | awk '{print sqrt( ($1-$3)^2 + ($2-$4)^2 )}'` 
 #writing data to file
 echo "&&!((DETX,DETY) IN circle(${detx},${dety},${detr}))" >> $output
endif

endif



@ i++
end

