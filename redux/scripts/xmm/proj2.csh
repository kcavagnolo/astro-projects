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

 printf "%g\t%g\t%g\t%g\t%g\t%g\t%g\t%g\n" $i $a $b $c $d $e $f $g  >> ${root}_res.txt
endif
@ i++
end



#4. Error values.....
# Going to be tricky for a few reasons. 
# - may not all be on the same line
# - abundance may/may not be fixed. 
#found a much better way of dealing with the problem.

if ( $err == 0 ) then

set i=1
while ( $i <= $N )
 set nlines=`cat ${root}_${i}_err1.txt | wc -l`
 set Tparam=2
 set Zparam=4
   if ( $met == 0 ) then # all metallicities fixed
    set j=1
     while ( $j <= $nlines )
      set tmp=`head -$j ${root}_${i}_err1.txt | tail -1`
      set tmp2=`echo $tmp | cut -d " " -f1`
       if ( $tmp2 == $Tparam ) then
        set Tlo=`echo $tmp | cut -d " " -f2`
        set Thi=`echo $tmp | cut -d " " -f3`
        echo $Tlo $Thi
       endif 
      set Z=`head -$i ${root}_vals.txt | tail -1 | cut -d " " -f2`
      set Z=`echo $Z | cut -d  " " -f3`
      set Zlo=$Z
      set Zhi=$Z
      @ j++
     end
   endif # all metallicities fixed

   if ( $met == N ) then # all thawed
    set j=1
     while ( $j <= $nlines )
      set tmp=`head -$j ${root}_${i}_err1.txt | tail -1`
      set tmp2=`echo $tmp | cut -d " " -f1`
       if ( $tmp2 == $Tparam ) then
        set Tlo=`echo $tmp | cut -d " " -f2`
        set Thi=`echo $tmp | cut -d " " -f3`
        echo $Tlo $Thi
       endif 
       if ( $tmp2 == $Zparam ) then
        set Zlo=`echo $tmp | cut -d " " -f2`
        set Zhi=`echo $tmp | cut -d " " -f3`
        echo $Zlo $Zhi
       endif 
      @ j++
     end
   endif # all thawed
 echo $met
 if ( $met != N ) then
  if ( $met > $i ) then # some thawed
    set j=1
     while ( $j <= $nlines ) 
      set tmp=`head -$j ${root}_${i}_err1.txt | tail -1`
      set tmp2=`echo $tmp | cut -d " " -f1`
       if ( $tmp2 == $Tparam ) then
        set Tlo=`echo $tmp | cut -d " " -f2`
        set Thi=`echo $tmp | cut -d " " -f3`
        echo $Tlo $Thi
       endif
       if ( $tmp2 == $Zparam ) then
        set Zlo=`echo $tmp | cut -d " " -f2`
        set Zhi=`echo $tmp | cut -d " " -f3`
        echo $Zlo $Zhi
       endif
     @ j++
     end  
  else
    set j=1
     while ( $j <= $nlines )
      set tmp=`head -$j ${root}_${i}_err1.txt | tail -1`
      set tmp2=`echo $tmp | cut -d " " -f1`
       if ( $tmp2 == $Tparam ) then
        set Tlo=`echo $tmp | cut -d " " -f2`
        set Thi=`echo $tmp | cut -d " " -f3`
        echo $Tlo $Thi
       endif 
      set Z=`head -$i ${root}_vals.txt | tail -1 | cut -d " " -f2`
      set Z=`echo $Z | cut -d  " " -f3`
      set Zlo=$Z
      set Zhi=$Z
      @ j++
     end

  endif
 endif # some metallicities varying

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
