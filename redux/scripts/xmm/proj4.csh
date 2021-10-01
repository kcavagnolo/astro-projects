#!/bin/tcsh
#script to create one error file for deprojected data
#need to have run root_proj3.xcm first.


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
if ( $#argv != 9 ) then
cat <<EOF

Use mkxcm.csh m1_root m2_root pn_root Nann err metfix root

    m1_root  - same as for proj3
    m2_root  - ditto above	      
    pn_root  - ditto above
    Nann     - number of annuli
    nH       - same as proj
    Z        - redshift
    err      - same as for proj3
    metfix   - ditto
    root     - ditto

EOF
exit
endif

set m1_root=$1
set m2_root=$2
set pn_root=$3
set N=$4
set nH=$5
set Red=$6
set err=$7
set met=$8
set root=$9

#1. Create a single output file for each annulus

if ( -e ${root}_vals2.txt ) then
rm ${root}_vals2.txt
endif
touch ${root}_vals2.txt

if ( $err == 0 ) then #0=yes
if ( -e ${root}_err2.txt ) then
rm ${root}_err2.txt
endif
touch ${root}_err2.txt
endif

#2. Temperature, Metallicity and Normalisations. 
set i=1
while ( $i <= $N )
set p=`head -6 ${root}_${i}_vals2.txt | tail -1 | awk '{print $1}'`
set q=`head -12 ${root}_${i}_vals2.txt | tail -1 | awk '{print $1}'`
set r=`head -18 ${root}_${i}_vals2.txt | tail -1 | awk '{print $1}'`
 printf "%g\t%g\t%g\t%g\n" $i $p $q $r >> ${root}_vals2.txt
endif
@ i++
end

#4. Error values.....
# Going to be tricky for a few reasons. 
# - may not all be on the same line
# - abundance may/may not be fixed. 
#
# Gonna assume that they are all fixed at the moment. Saves a 
# lot of coding trouble. 
#Need to include the normalisations here too. 
#Going to try a different method. 

set npars=18
set i=1
while ( $i <= $N )
 set nlines=`cat ${root}_${i}_err2.txt | wc -l`
 set string=`head -$nlines ${root}_${i}_err2.txt `
  set Tparam=`echo $i $npars | awk '{print $1*$2*2-31}'`
  set Nparam=`echo $i $npars | awk '{print $1*$2*2-26}'`
  echo $Tparam $Nparam
  set j=1
   while ( $j <= $nlines ) 
    set tmp=`head -$j ${root}_${i}_err2.txt | tail -1`
    set tmp2=`echo $tmp | cut -d " " -f1`
     if ( $tmp2 == $Tparam ) then
      set Tlo=`echo $tmp | cut -d " " -f2`
      set Thi=`echo $tmp | cut -d " " -f3`
      echo $Tlo $Thi
     endif
     if ( $tmp2 == $Nparam ) then
      set Nlo=`echo $tmp | cut -d " " -f2`
      set Nhi=`echo $tmp | cut -d " " -f3`
      echo $Nlo $Nhi
     endif
     set Z=`head -$i ${root}_vals2.txt | tail -1 | cut -d " " -f2`
     set Z=`echo $Z | cut -d  " " -f3`
     set Zlo=$Z
     set Zhi=$Z
   @ j++
   end
  printf "%g\t%g\t%g\t%g\t%g\t%g\t%g\n" $i $Tlo $Thi $Zlo $Zhi $Nlo $Nhi >> ${root}_err2.txt 

@ i++
end




#Now going to put it all together

#Would like to combine errors, datasets and annular regions in arcsec. 

if ( -e ${root}_deproj.txt ) then 
rm ${root}_deproj.txt
endif
touch ${root}_deproj.txt
#need to grab XFLT0001 keywords for each annulus. 
set i=1
set r_in=0.0
while ( $i <= $N ) 
set r_out=`dmkeypar ${m1_root}_an${i}.fits XFLT0001 echo+`
set str=`head -$i ${root}_vals2.txt | tail -1 | cut -d " " -f2`
set T=`echo $str | cut -d " " -f2`
set Z=`echo $str | cut -d " " -f3`
set no=`echo $str | cut -d " " -f4`
set str=`head -$i ${root}_err2.txt | tail -1 | cut -d " " -f2`
set Tlo=`echo $str | cut -d " " -f2`
set Thi=`echo $str | cut -d " " -f3`
set Zlo=`echo $str | cut -d " " -f4`
set Zhi=`echo $str | cut -d " " -f5`
set Nlo=`echo $str | cut -d " " -f6`
set Nhi=`echo $str | cut -d " " -f7`

set r=`echo $r_in $r_out | awk '{print 0.5 * ($1 + $2) }'`

 printf "%g\t%g\t%g\t%g\t%g\t%g\t%g\t%g\t%g\t%g\t%g\n" $r $r_in $r_out $T $Tlo $Thi $Z $Zlo $Zhi $Nlo $Nhi  >> ${root}_deproj.txt

set r_in=`echo ${r_out}`
@ i++
end


#And now to combine everything into a single text file for use in Al's scripts
#Output filename is {root}_all.txt 
if ( -e ${root}_all.txt ) then 
rm ${root}_all.txt
endif
touch ${root}_all.txt
cat >> ${root}_all.txt <<EOF
cname r r.lo r.up kT.2d kT.2d.lo kT.2d.up kT.3d kT.3d.lo kT.3d.up A.2d A.2d.lo A.2d.up A.3d A.3d.lo A.3d.up norm.3d norm.3d.lo norm.3d.up nH Z
EOF

set i=1
set r_in=0.0
while ( $i <= $N ) 
set r_out=`dmkeypar ${m1_root}_an${i}.fits XFLT0001 echo+`
set r_out=`echo $r_out | awk '{print $1*1}'`
set r=`echo $r_in $r_out | awk '{print 0.5 * ($1 + $2) }'`
set str=`head -$i ${root}_vals2.txt | tail -1 | cut -d " " -f2`
set T3=`echo $str | cut -d " " -f2`
set Z3=`echo $str | cut -d " " -f3`
set no3=`echo $str | cut -d " " -f4`
set str=`head -$i ${root}_err2.txt | tail -1 | cut -d " " -f2`
set Tlo3=`echo $str | cut -d " " -f2`
set Thi3=`echo $str | cut -d " " -f3`
set Zlo3=`echo $str | cut -d " " -f4`
set Zhi3=`echo $str | cut -d " " -f5`
set Nlo3=`echo $str | cut -d " " -f6`
set Nhi3=`echo $str | cut -d " " -f7`
set str=`head -$i ${root}_vals.txt | tail -1 | cut -d " " -f2`
set T2=`echo $str | cut -d " " -f2`
set Z2=`echo $str | cut -d " " -f3`
set no2=`echo $str | cut -d " " -f4`
set str=`head -$i ${root}_err.txt | tail -1 | cut -d " " -f2`
set Tlo2=`echo $str | cut -d " " -f2`
set Thi2=`echo $str | cut -d " " -f3`
set Zlo2=`echo $str | cut -d " " -f4`
set Zhi2=`echo $str | cut -d " " -f5`
set Nlo2=`echo $str | cut -d " " -f6`
set Nhi2=`echo $str | cut -d " " -f7`

cat >> ${root}_all.txt <<EOF
$root $r $r_in $r_out $T2 $Tlo2 $Thi2 $T3 $Tlo3 $Thi3 $Z2 $Zlo2 $Zhi2 $Z3 $Zlo3 $Zhi3 $no3 $Nlo3 $Nhi3 $nH $Red
EOF

# printf "%g\t%g\t%g\t%g\t%g\t%g\t%g\t%g\t%g\t%g\t%g\t%g\t%g\t%g\t%g\t%g\t%g\t%g\t%g\t%g\n" "$root" $r $r_in $r_out $T2 $Tlo2 $Thi2 $T3 $Tlo3 $Thi3 $Z2 $Zlo2 $Zhi2 $Z3 $Zlo3 $Zhi3 $no3 $Nlo $Nhi $nH $Red   >> ${root}_all.txt

set r_in=`echo ${r_out}`
@ i++
end



