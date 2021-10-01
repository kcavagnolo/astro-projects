#!/bin/tcsh

#Third script in my deprojection routine. Slight change of plan with
#regards to the initial run. Not going to use the faked spectra anymore.
#Am going to use the result from the projected fits and fix them to reduce
#the number of free parameters.

# need to add the extra deproj components aswell. Can't believe that I forgot 
#them!! Lunacy, I tell you! 


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

Use mkxcm.csh m1_root m2_root pn_root Nann nH Redshift err metfix root

    m1_root  - same as for proj1 
    m2_root  - ditto above	      
    pn_root  - ditto above
    Nann     - number of annuli
    nH       - nH
    Redshift - Redshift
    err      - same as for proj1
    metfix   - F=fix at projected value. Other values are as proj1. 
    root     - ditto

EOF
exit
endif

set m1_root=$1
set m2_root=$2
set pn_root=$3
set N=$4
set nH=$5
set Z=$6
set err=$7
set met=$8
set root=$9


set outfile=${root}_proj3.xcm
#does outfile exist?
if ( -e $outfile ) then
rm $outfile
endif
touch $outfile

#most steps will be close to proj1.csh
#1. Add data to the outfile. 

set i=1
set j=1
set spec_in=(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20)
set M=`echo $N | awk '{print $1*3}'`
while ( $i <= $M ) 
set p=`expr $i `
set q=`expr $i + 1`
set r=`expr $i + 2`
set spec_in[$j]=${p}":"${p}" ${m1_root}_an${j}.fits "${q}":"${q}" ${m2_root}_an${j}.fits "${r}":"${r}" ${pn_root}_an${j}.fits" 

cat >> $outfile <<EOF
data $spec_in[$j]
EOF
@ i++
@ i++
@ i++
@ j++
end

#2. Ignore energy regions
set i=1
set j=1
while ( $i <= $M ) 
set p=`expr $i `
set q=`expr $i + 1`
set r=`expr $i + 2`
cat >> $outfile <<EOF
ignore ${p}:${p} **-0.5 1.4-1.8 8.0-**
ignore ${q}:${q} **-0.5 1.4-1.8 8.0-**
ignore ${r}:${r} **-0.5 1.4-1.8 7.5-**
EOF
@ i++
@ i++
@ i++
@ j++
end

#3. Usual dross for XSPEC

cat >> $outfile <<EOF
setplot energy
cpd /xw
pl ld

 statistic chi
 abund grsa
 xsect bcmc
 xset forcecalc off
 cosmo   70.000   0.000   0.730
 query yes
EOF

#4. Determine model. As am now deprojecting data, 
#number of parameters=18. projct* (wabs*mekal)+mekal+powerlaw

set npars=18
cat >> $outfile <<EOF
model projct*(wabs*mekal)+mekal+pow  & /*
EOF

#5. Start fixing and thawing parameters
#5a. nH, T, mekal_nH, redshift, kT2, Z2, A2, mekal_nh2, switch, switch2

set i=1
while ( $i <= $M )
set p=`echo $i $npars | awk '{print ($1*$2)-15+1 }'` #nH
set r=`echo $i $npars | awk '{print ($1*$2)-15+3 }'` #mek_nH
set q=`echo $i $npars | awk '{print ($1*$2)-15+5 }'` #redshift
set s=`echo $i $npars | awk '{print ($1*$2)-15+6 }'` #switch
set t=`echo $i $npars | awk '{print ($1*$2)-15+8 }'` #kT2
set u=`echo $i $npars | awk '{print ($1*$2)-15+9 }'` #mek_nH2
set v=`echo $i $npars | awk '{print ($1*$2)-15+10 }'` #A2
set w=`echo $i $npars | awk '{print ($1*$2)-15+11 }'` #Z2
set x=`echo $i $npars | awk '{print ($1*$2)-15+13 }'` #norm2
set y=`echo $i $npars | awk '{print ($1*$2)-15+12 }'` #switch2
set z=`echo $i $npars | awk '{print ($1*$2)-15+7 }'`  #norm
@ i++
cat >> $outfile <<EOF
untie ${p} ${r} ${q} ${s} ${z} ${t} ${u} ${v} ${w} ${y} ${x} 
newpar ${p} $nH -1 
newpar ${r} 0.001 -1
newpar ${q} $Z -1
newpar ${z} 1E-4 1
newpar ${u} 0.001 -1
newpar ${v} 1.0 -1
newpar ${w} 0.0 -1
newpar ${x} 1E-4 0.01 -1E24 -1E24 1E24 1E24
EOF
end

#5b. Tie the mekal normalisations and temperatures together
set i=1
while ( $i <= $N )

set p=`echo $i $npars | awk '{print ($1*3*$2)-51+7 }'` 
set s=`echo $i $npars | awk '{print ($1*3*$2)-51+2 }'`
set v=`echo $i $npars | awk '{print ($1*3*$2)-51+4 }'`

set q=`echo $p $npars | awk '{print $1+$2 }'`
set r=`echo $p $npars | awk '{print $1+(2*$2) }'`
set t=`echo $s $npars | awk '{print $1+$2 }'`
set u=`echo $s $npars | awk '{print $1+(2*$2) }'`
set w=`echo $v $npars | awk '{print $1+$2 }'`
set x=`echo $v $npars | awk '{print $1+(2*$2) }'`

cat >> $outfile <<EOF
untie ${p} ${q} ${r} ${s} ${t} ${u} ${v} ${w} ${x}
newpar ${q} = ${p}
newpar ${r} = ${p}
newpar ${t} = ${s}
newpar ${u} = ${s}
newpar ${w} = ${v}
newpar ${x} = ${v}

EOF
@ i++
end

#5c. And finally abundance. Remember 0=fix, N=thaw all, F=freeze at proj val. 
#    a number means freeze in bin (and subsequent bins).

if (( $met == 0 ) || ( $met == F ) || ( $met == N )) then 
  if ( $met == N ) then
  set bool=1
  else
  set bool=-1
  endif
 set i=1
 set j=0
 set k=1
   while ( $i <= $M ) 
   set p=`echo $i $npars | awk '{print ($1*$2)-15+4 }'` 
   #set up a counter to determine the right starting value
   set j=`expr $j + 1`
    if ( $j > 3 ) then
     set j=1
    endif
   set string=`cat ${root}_proj.txt | head -$k | tail -1`
   set A=`echo $string | cut -d " " -f7`
    if ( $j == 3 ) then
     set k=`expr $k + 1`
    endif
cat >> $outfile <<EOF
untie ${p} 
newpar ${p} ${A} ${bool} 
EOF
@ i++
   end
else # i.e. metallicity is fixed after a certain radius
 set i=1
 set j=0
 set k=1
 while ( $i <= $M ) 
   set p=`echo $i $npars | awk '{print ($1*$2)-15+4 }'` 
   #set up a counter to determine the right starting value
   set j=`expr $j + 1`
    if ( $j > 3 ) then
     set j=1
    endif
    if ( $met <= $k ) then
     set bool=-1
    else
     set bool=1
    endif
   set string=`cat ${root}_proj.txt | head -$k | tail -1`
   set A=`echo $string | cut -d " " -f7`
    if ( $j == 3 ) then
     set k=`expr $k + 1`
    endif

cat >> $outfile <<EOF
untie ${p} 
newpar ${p} ${A} ${bool} 
EOF
@ i++
   end
endif #abundance loop

#5d. retie the abundances together (same code as 5b)

set i=1
while ( $i <= $N )

set v=`echo $i $npars | awk '{print ($1*3*$2)-51+4 }'`
set w=`echo $v $npars | awk '{print $1+$2 }'`
set x=`echo $v $npars | awk '{print $1+(2*$2) }'`

cat >> $outfile <<EOF
newpar ${w} = ${v}
newpar ${x} = ${v}

EOF
@ i++
end

#OK, this is better, now I at least have some control of the abundances. 

#6. Fix the soft excess in each band to projected values. 
#File is {root}_res.txt
#Gonna need the same loop as I had for abundances


set i=1
set j=0
set k=1

while ( $i <= $M ) 
 set p=`echo $i $npars | awk '{print ($1*$2)-15+8 }'` #T2
 set q=`echo $i $npars | awk '{print ($1*$2)-15+13 }'` #N2
 set r=`echo $i $npars | awk '{print ($1*$2)-15+14 }'` #G
 set s=`echo $i $npars | awk '{print ($1*$2)-15+15 }'` #GN
 #set up a counter to determine the right starting value
 set j=`expr $j + 1`
  if ( $j > 3 ) then
   set j=1
  endif
 set string=`cat ${root}_res.txt | head -$k | tail -1`
 set T2=`echo $string | cut -d " " -f2`
    if ( $j == 1 ) then
 set N2=`echo $string | cut -d " " -f3`
 set G=`echo $string | cut -d " " -f4`
 set GN=`echo $string | cut -d " " -f5`
    endif
    if ( $j == 2 ) then
 set N2=`echo $string | cut -d " " -f6`
 set G=`echo $string | cut -d " " -f7`
 set GN=`echo $string | cut -d " " -f8`
    endif
    if ( $j == 3 ) then
 set N2=`echo $string | cut -d " " -f9`
 set G=`echo $string | cut -d " " -f10`
 set GN=`echo $string | cut -d " " -f11`
    endif
  if ( $j == 3 ) then
   set k=`expr $k + 1`
  endif
cat >> $outfile <<EOF
untie ${p} ${q} ${r} ${s}
newpar ${p} ${T2} -1
newpar ${q} ${N2} -1
newpar ${r} ${G} -1
newpar ${s} ${GN} -1
EOF
@ i++
end


#6.5 get the correct parameters for major, minor and orient. 
set i=1

while ( $i <= $N ) 
#set major=`fkeyprint ${m1_root}_an${i}.fits+1 XFLT0001 exact=yes outfile=STDOUT | grep XFLT0001 | tail -1 | awk '{print $2}'`
#set minor=`fkeyprint ${m1_root}_an${i}.fits+1 XFLT0002 exact=yes outfile=STDOUT | grep XFLT0002 | tail -1 | awk '{print $2}'`
#set orient=`fkeyprint ${m1_root}_an${i}.fits+1 XFLT0003 exact=yes outfile=STDOUT | grep XFLT0003 | tail -1 | awk '{print $2}'`
set major=`dmkeypar ${m1_root}_an${i}.fits+1 XFLT0001 echo+`
set minor=`dmkeypar ${m1_root}_an${i}.fits+1 XFLT0002 echo+`
set orient=`dmkeypar ${m1_root}_an${i}.fits+1 XFLT0003 echo+`


set param=`echo $npars $i | awk '{print $1*$2*3}'`
set maxax1=`echo $param | awk '{print $1-53}'`
set minax1=`echo $param | awk '{print $1-52}'`
set orient1=`echo $param | awk '{print $1-51}'`
set maxax2=`echo $param | awk '{print $1-35}'`
set minax2=`echo $param | awk '{print $1-34}'`
set orient2=`echo $param | awk '{print $1-33}'`
set maxax3=`echo $param | awk '{print $1-17}'`
set minax3=`echo $param | awk '{print $1-16}'`
set orient3=`echo $param | awk '{print $1-15}'`

cat >> $outfile <<EOF
newpar $minax1 ,,0,0,1000,1000
newpar $maxax1 ,,0,0,1000,1000
newpar $maxax1 $major
newpar $minax1 $minor
newpar $orient1 $orient
newpar $minax2 ,,0,0,1000,1000
newpar $maxax2 ,,0,0,1000,1000
newpar $maxax2 $major
newpar $minax2 $minor
newpar $orient2 $orient
newpar $minax3 ,,0,0,1000,1000
newpar $maxax3 ,,0,0,1000,1000
newpar $maxax3 $major
newpar $minax3 $minor
newpar $orient3 $orient
EOF

@ i++
end


#7. Fit and plot the data

cat >> $outfile <<EOF
show parameters
fit 10000
pl ld re
fit
fit
fit
pl
EOF

#8. Output values to a logfile. (T, Abund, norm). 
#File is ${root}_an_vals2.txt
#Errors go to {root}_an_err2.txt

set i=1
while ( $i <= $N )
 set p=`echo $i $npars | awk '{print ($1*3*$2)-51+2 }'` #T
 set q=`echo $i $npars | awk '{print ($1*3*$2)-51+4 }'` #A
 set r=`echo $i $npars | awk '{print ($1*3*$2)-51+7 }'` #N
cat >> ${outfile} <<EOF
log ${root}_${i}_vals2.txt
show par ${p}
show par ${q}
show par ${r}
log none
EOF

  if ( $err == 0 ) then #0=calc errors
cat >> ${outfile} <<EOF
log ${root}_${i}_err2.txt
err ${p} ${q} ${r}
log none
EOF
  endif
@ i++
end

