#!/bin/tcsh

#script to follow on from proj1.csh. It is assumed that proj1.csh has been
#successfully run and that there are many text files named
# root_i_vals/err/res.txt for source values/errors/and residuals.

#This script will do a couple of things.
#1) it will sort out those output files into something more readable
#2) it will generate fake spectra for the residual spectrum 
#(use fakeit in xspec)

#script outputs are: root_proj2.xcm - xspec file to create faked spectra
#, root_*.txt for different parameters, + root_proj.txt- projected
#results with errors and annular regions (if err=yes).

#also needs residual spectra (mos1_res.txt pn_res.txt etc)


set version=1.0

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

#need to sort out end triggers
if ( $#argv != 7 ) then
cat <<EOF

Use mkxcm.csh m1_root m2_root pn_root Nann err metfix root

    m1_root  - same as for proj1 
    m2_root  - ditto above	      
    pn_root  - ditto above
    Nann     - number of annuli
    err      - same as for proj1
    metfix   - ditto
    root     - ditto

EOF
exit
endif

set m1_root=$1
set m2_root=$2
set pn_root=$3
set N=$4
set err=$5
set met=$6
set root=$7

#1. Create a single output file for each annulus

if ( -e ${root}_vals.txt ) then
rm ${root}_vals.txt
endif
touch ${root}_vals.txt

if ( -e ${root}_res.txt ) then
rm ${root}_res.txt
endif
touch ${root}_res.txt

if ( $err == 0 ) then #0=yes
if ( -e ${root}_err.txt ) then
rm ${root}_err.txt
endif
touch ${root}_err.txt
endif

#2. Temperature, Metallicity and Normalisations first. 
set i=1
while ( $i <= $N )

set p=`head -6 ${root}_${i}_vals1.txt | tail -1 | awk '{print $1}'`
set q=`head -12 ${root}_${i}_vals1.txt | tail -1 | awk '{print $1}'`
set r=`head -18 ${root}_${i}_vals1.txt | tail -1 | awk '{print $1}'`
 printf "%g\t%g\t%g\t%g\n" $i $p $q $r >> ${root}_vals.txt
endif
@ i++
end

#delete unnecessary files at the end....

#3. Residual spectra values. 

set i=1
while ( $i <= $N )

set a=`head -6 ${root}_${i}_res1.txt | tail -1 | awk '{print $1}'`
set b=`head -12 ${root}_${i}_res1.txt | tail -1 | awk '{print $1}'`
set c=`head -18 ${root}_${i}_res1.txt | tail -1 | awk '{print $1}'`
set d=`head -24 ${root}_${i}_res1.txt | tail -1 | awk '{print $1}'`
set e=`head -30 ${root}_${i}_res1.txt | tail -1 | awk '{print $1}'`
set f=`head -36 ${root}_${i}_res1.txt | tail -1 | awk '{print $1}'`
set g=`head -42 ${root}_${i}_res1.txt | tail -1 | awk '{print $1}'`

 printf "%g\t%g\t%g\t%g\t%g\t%g\t%g\t%g\t%g\t%g\t%g\n" $i $a $b $c $d $e $f $g   >> ${root}_res.txt
endif
@ i++
end



#4. Error values.....
# Going to be tricky for a few reasons. 
# - may not all be on the same line
# - abundance may/may not be fixed. 

if ( $err == 0 ) then

set i=1
while ( $i <= $N )
 set nlines=`cat ${root}_${i}_err1.txt | wc -l`
 set string=`head -$nlines ${root}_${i}_err1.txt | tail -7`
 set string2=`head -$nlines ${root}_${i}_err1.txt | tail -10`
 if ( $met == 0 ) then
  set Tlo=`echo $string | cut -d " " -f24`
  set Thi=`echo $string | cut -d " " -f25`
  set Zlo=0.0
  set Zhi=0.0
 endif
 if ( $met == N ) then
  set Tlo=`echo $string | cut -d " " -f11` 
  set Thi=`echo $string | cut -d " " -f12`
  set Zlo=`echo $string | cut -d " " -f19`
  set Zhi=`echo $string | cut -d " " -f20`
#need to trap errors in the same way as below. 
   set tmp1=`echo $string | cut -d " " -f5`
   set tmp1b=`echo $string | cut -d " " -f1`  
   set tmp1=`echo $tmp1 | cut -c1`
   set tmp1b=`echo $tmp1b | cut -c1`
   set tmp1c=`echo $string2 | cut -d " " -f1`
   set tmp1c=`echo $tmp1c | cut -c1`
   
    if ( $tmp1c == P ) then #this is if non-mon probs for met.
     set Tlo=`echo $string2 | cut -d " " -f7` 
     set Thi=`echo $string2 | cut -d " " -f8`
     set Zlo=`echo $string2 | cut -d " " -f51`
     set Zhi=`echo $string2 | cut -d " " -f52`
    endif
    if ( $tmp1 == t ) then #this is for 2 params having same statistic
     set Tlo=`echo $string | cut -d " " -f26` 
     set Thi=`echo $string | cut -d " " -f27`
     set Zlo=`echo $string | cut -d " " -f34`
     set Zhi=`echo $string | cut -d " " -f35`
    endif
 endif
 if ( $met != N ) then
  if ( $met > $i ) then
#trapping errors...
#Need a proper way of dealing with this. 
#1) Parameter 5 should be "Parameter"
   set tmp1=`echo $string | cut -d " " -f5`
   set tmp1b=`echo $string | cut -d " " -f1`  
   set tmp1=`echo $tmp1 | cut -c1`
   set tmp1b=`echo $tmp1b | cut -c1`
   set tmp1c=`echo $string2 | cut -d " " -f1`
   set tmp1c=`echo $tmp1c | cut -c1`
   set tmp1d=`echo $string | cut -d " " -f9`
   set tmp1d=`echo $tmp1d | cut -c1`
   set tmp1e=`echo $string | cut -d " " -f15`
   set tmp1e=`echo $tmp1e | cut -c1`
    if ( $tmp1 == P ) then
     set Tlo=`echo $string | cut -d " " -f11` 
     set Thi=`echo $string | cut -d " " -f12`
     set Zlo=`echo $string | cut -d " " -f19`
     set Zhi=`echo $string | cut -d " " -f20`
#2) may be problems with commas in the wrong place.
#Temperatures wont be affected by this if the above condition is true. 
#2a) determine if first error characters match up with commas     

     set tmp2=`echo $string | cut -d " " -f14`
     set tmp2=`echo $tmp2 | cut -c10`
     if ( $tmp2 == E ) then # i.e. first comma is affected
#      set j=`expr $j + 1`
#      echo $j
#Check to see if second comma affected       
       set tmp3=`echo $string | cut -d " " -f15`
       set tmp3=`echo $tmp3 | cut -c9`
        if ( $tmp3 == E ) then
         set Zlo=`echo $string | cut -d " " -f17`
         set Zhi=`echo $string | cut -d " " -f18`
        else
         set Zlo=`echo $string | cut -d " " -f18`
         set Zhi=`echo $string | cut -d " " -f19`
        endif
     else
#Check to see if second affected but not first
      set tmp4=`echo $string | cut -d " " -f16`
      set tmp4=`echo $tmp4 | cut -c9`
       if ( $tmp4 == E ) then
        set Zlo=`echo $string | cut -d " " -f18`
        set Zhi=`echo $string | cut -d " " -f19`      
       endif
     endif
    else if ( $tmp1 == t ) then #this is for 2 params having same statistic
     set Tlo=`echo $string | cut -d " " -f26` 
     set Thi=`echo $string | cut -d " " -f27`
     set Zlo=`echo $string | cut -d " " -f34`
     set Zhi=`echo $string | cut -d " " -f35`
    else if ( $tmp1b == P ) then #this is for par4 pegging at lower limit
     set Tlo=`echo $string | cut -d " " -f7` 
     set Thi=`echo $string | cut -d " " -f8`
     set Zlo=`echo $string | cut -d " " -f25`
     set Zhi=`echo $string | cut -d " " -f26`
    else if ( $tmp1c == P ) then #this is if non-mon probs for met.
     set Tlo=`echo $string2 | cut -d " " -f7` 
     set Thi=`echo $string2 | cut -d " " -f8`
     set Zlo=`echo $string2 | cut -d " " -f51`
     set Zhi=`echo $string2 | cut -d " " -f52`
      if ( $tmp1e == S ) then #more non-mon problems (see 0006_3 and 2217_7)
       set Tlo=`echo $string | cut -d " " -f26` 
       set Thi=`echo $string | cut -d " " -f27`
       set Zlo=`echo $string | cut -d " " -f34`
       set Zhi=`echo $string | cut -d " " -f35`
        set tmp1f=`echo $string | cut -d " " -f31`
	set tmp1f=`echo $tmp1f | cut -c9`
#        echo $tmp1f
        if ( $tmp1f == E ) then #comma issues
         set Zlo=`echo $string | cut -d " " -f33`
         set Zhi=`echo $string | cut -d " " -f34`         
        endif
      endif
    else if ( $tmp1b == C ) then #Chi2< prev min probs
     set Tlo=`echo $string | cut -d " " -f20` 
     set Thi=`echo $string | cut -d " " -f21`
     set Zlo=`echo $string | cut -d " " -f28`
     set Zhi=`echo $string | cut -d " " -f29`
    else if ( $tmp1d == W ) then #same statistic again , but for T not met
     set Tlo=`echo $string | cut -d " " -f2` 
     set Thi=`echo $string | cut -d " " -f3`
     set Zlo=`echo $string | cut -d " " -f34`
     set Zhi=`echo $string | cut -d " " -f35`
    else if ( $tmp1c == S ) then
     set Tlo=`echo $string | cut -d " " -f26` 
     set Thi=`echo $string | cut -d " " -f27`
     set Zlo=`echo $string | cut -d " " -f34`
     set Zhi=`echo $string | cut -d " " -f35`    
    endif
    else
     echo $tmp1 $tmp1b $tmp1c $tmp1d $tmp1e
     echo "problems in bin ${i}, need to revise"
     if ( $i == 8 ) then
      set Tlo=`echo $string | cut -d " " -f26` 
      set Thi=`echo $string | cut -d " " -f27` 
      set Zlo=0.0
      set Zhi=0.0
     endif
#    exit
    endif

  else #i.e. no metallicity component being calculated
   set Tlo=`echo $string | cut -d " " -f24`
   set Thi=`echo $string | cut -d " " -f25`
   set Zlo=0.0
   set Zhi=0.0
    if ( $Tlo == "check" ) then#steppar issues with T
     set Tlo=`echo $string | cut -d " " -f32`
     set Thi=`echo $string | cut -d " " -f33`    
    endif
  endif
 endif
 echo $i $Tlo $Thi $Zlo $Zhi
 printf "%g\t%g\t%g\t%g\t%g\n" $i $Tlo $Thi $Zlo $Zhi  >> ${root}_err.txt
endif
@ i++
end


#job's done. For values where steppar is required, am going to have to modify. 
#Would like to combine errors, datasets and anular regions in arcsec. 

if ( -e ${root}_proj.txt ) then 
rm ${root}_proj.txt
endif
touch ${root}_proj.txt
#need to grab XFLT0001 keywords for each annulus. 
set i=1
set r_in=0.0
while ( $i <= $N ) 
set r_out=`dmkeypar ${m1_root}_an${i}.fits XFLT0001 echo+`
set str=`head -$i ${root}_vals.txt | tail -1 | cut -d " " -f2`
set T=`echo $str | cut -d " " -f2`
set Z=`echo $str | cut -d " " -f3`
set no=`echo $str | cut -d " " -f4`
set str=`head -$i ${root}_err.txt | tail -1 | cut -d " " -f2`
set Tlo=`echo $str | cut -d " " -f2`
set Thi=`echo $str | cut -d " " -f3`
set Zlo=`echo $str | cut -d " " -f4`
set Zhi=`echo $str | cut -d " " -f5`
set r=`echo $r_in $r_out | awk '{print 0.5 * ($1 + $2) }'`

 printf "%g\t%g\t%g\t%g\t%g\t%g\t%g\t%g\t%g\n" $r $r_in $r_out $T $Tlo $Thi $Z $Zlo $Zhi  >> ${root}_proj.txt

set r_in=`echo ${r_out}`
@ i++
end

endif #err==0


#delete unwanted files.
#Basically rm run1_*_*1.txt . Not going to delete at the moment.


exit
