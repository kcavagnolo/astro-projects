#!/bin/tcsh
#First of four scripts to do projection and deprojection for clusters
#This script will create a .xcm file that will be used to do the projected 
#fits (+errors optional). 

#This script is based on the mkxcm.csh scripts

#The input files should be already grouped with assosciated arf, rmf and bg
#Note that they will be regrouped later to take into account the 
#new background files that I will create. These 'should' also have the 
#necessary project keywords included. 

#projected results will be stored in root_i_vals1.txt
#residual values will be stored in root_i_res1.txt
#errors will be stored in root_i_err1.txt
#script 2 will delete all these files and make them more sensible...


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
if ( $#argv != 15 ) then
cat <<EOF

Use mkxcm.csh m1_root m2_root pn_root Nann nH Z T Abund kTsoft m1_gamma
    m2_gamma pn_gamma err metfix root

    m1_root  - list of mos1 spectra (key words added for deprojection 
	      (if required) e.g. {root}_an1.fits
    m2_root  - list of mos2 spectra, ditto above	      
    pn_root  - list of pn spectra, ditto above
    Nann     - number of annuli
    nH       - galactic column density
    Z        - source redshift
    T        - temperature (initial)
    Abund    - metallicity (initial)
    kTsoft   - temperature of soft galactic background
    m1_gamma - powerlaw value (0 if no component)
    m2_gamma - ditto
    pn_gamma - ditto
    err      - boolean. 0=yes, 1=no (or otherwise) Calculate errors?
	       Will calculate these for kT and Abund.
    metfix   - fix metallicities? 0=fix at value set in par 9. N=thaw all.
	       A number between 1 and N fixes the metallicity from that
	       bin (and all subsequent bins) to par 9.
    root     - rootname for output file

EOF
exit
endif

set m1_root=$1
set m2_root=$2
set pn_root=$3
set N=$4
set nH=$5
set Z=$6
set T=$7
set A=$8
set kT2=$9
set m1_gam=$10
set m2_gam=$11
set pn_gam=$12
set err=$13
set met=$14
set root=$15

while ( $i <= $N )
if ( ! -e ${m1_root}_an${i}.fits ) then
 echo "Error: ${m1_root}_an${i}.fits does not exist"
 exit
endif
@ i++
end
#(mos2)
set i=1
while ( $i <= $N )
if ( ! -e ${m2_root}_an${i}.fits ) then
 echo "Error: ${m2_root}_an${i}.fits does not exist"
 exit
endif
@ i++
end
#(and pn)
set i=1
while ( $i <= $N )
if ( ! -e ${pn_root}_an${i}.fits ) then
 echo "Error: ${pn_root}_an${i}.fits does not exist"
 exit
endif
@ i++
end

set outfile=${root}_proj1.xcm
#does outfile exist?
if ( -e $outfile ) then
rm $outfile
endif
touch $outfile


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
EOF

#4. Determine model. As am projecting data initially, 
#number of parameters=15. wabs*mekal+mekal+powerlaw

set npars=15
cat >> $outfile <<EOF
model (wabs*mekal)+mekal+pow  & /*
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
newpar ${t} $kT2 -1
newpar ${u} 0.001 -1
newpar ${v} 1.0 -1
newpar ${w} 0.0 -1
newpar ${x} 1E-4 0.01 -1E24 -1E24 1E24 1E24
EOF
end


#5b. Tie the mekal normalisations and temperatures together
set i=1
while ( $i <= $N )

set p=`echo $i $npars | awk '{print ($1*3*$2)-45+7 }'` 
set s=`echo $i $npars | awk '{print ($1*3*$2)-45+2 }'`
set v=`echo $i $npars | awk '{print ($1*3*$2)-45+4 }'`

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

#5c. And finally abundance. Remember 0=fix, N=thaw all,
#    a number means freeze in bin (and subsequent bins).

if ( $met == N ) then
set i=1
while ( $i <= $M ) 
set p=`echo $i $npars | awk '{print ($1*$2)-15+4 }'` 
cat >> $outfile <<EOF
untie ${p} 
newpar ${p} $A 1 
EOF
@ i++
end
endif

if ( $met == 0 ) then
set i=1
while ( $i <= $M ) 
set p=`echo $i $npars | awk '{print ($1*$2)-15+4 }'` 
cat >> $outfile <<EOF
untie ${p} 
newpar ${p} $A -1 
EOF
@ i++
end
endif

if ( $met != N ) then
if ( $met > 0 ) then
 set barrier=`echo $met | awk '{print $1*3 -3 }'` # -3 makes it inclusive
 set i=1
while ( $i <= $barrier ) 
set p=`echo $i $npars | awk '{print ($1*$2)-15+4 }'` 
cat >> $outfile <<EOF
untie ${p} 
newpar ${p} $A 1 
EOF
@ i++
end
while ( $i <= $M )
set p=`echo $i $npars | awk '{print ($1*$2)-15+4 }'`
cat >> $outfile <<EOF
untie ${p} 
newpar ${p} $A -1 
EOF
@ i++
  end
endif
endif

#tying abundances.
set i=1
while ( $i <= $N )
set v=`echo $i $npars | awk '{print ($1*3*$2)-45+4 }'`
set w=`echo $v $npars | awk '{print $1+$2 }'`
set x=`echo $v $npars | awk '{print $1+(2*$2) }'`
cat >> $outfile <<EOF
untie ${w} ${x}
newpar ${w} = ${v}
newpar ${x} = ${v}
EOF
@ i++
end

#5d. work out powerlaws. If gam=0, then norm=0

set i=1
while ( $i <= $N )
set p=`echo $i $npars | awk '{print ($1*3*$2)-45+14 }'` 
set q=`echo $p $npars | awk '{print $1+$2 }'` #q=mos2
set r=`echo $p $npars | awk '{print $1+(2*$2) }'` #r=pn
set s=`expr $p + 1 ` #s,t,r are normalisations
set t=`expr $q + 1 `
set u=`expr $r + 1 `

if ( $m1_gam == 0 ) then
cat >> $outfile <<EOF
untie ${p} ${s}
newpar ${p} ${m1_gam} -1
newpar ${s} 0.0 -1
EOF
else
cat >> $outfile <<EOF
untie ${p} ${s}
newpar ${p} ${m1_gam} -1
newpar ${s} 1E-6 1
EOF
endif
if ( $m2_gam == 0 ) then
cat >> $outfile <<EOF
untie ${q} ${t}
newpar ${q} ${m2_gam} -1
newpar ${t} 0.0 -1
EOF
else
cat >> $outfile <<EOF
untie ${q} ${t}
newpar ${q} ${m2_gam} -1
newpar ${t} 1E-6 1
EOF
endif
if ( $pn_gam == 0 ) then
cat >> $outfile <<EOF
untie ${r} ${u}
newpar ${r} ${pn_gam} -1
newpar ${u} 0.0 -1
EOF
else
cat >> $outfile <<EOF
untie ${r} ${u}
newpar ${r} ${pn_gam} -1
newpar ${u} 1E-6 1
EOF
endif

@ i++
end

#6. Fit and plot the data

cat >> $outfile <<EOF
show parameters
fit 10000
pl ld re
fit
fit
fit
pl
EOF

#7. output all the useful parameters to a logfile, also ktnorm, gamma, pownorm
# These are T, Abund, norm. Only need one per annulus as they are tied pars.
#calculate errors at the same time (if required).

#Am going to to error calculations here too (if required). Freezing every
#non relevant bin as no parameters are linked. 

#For each annulus, need to cycle through and freeze all other bins.
#Then fit again. Then send source values to one logfile, residuals 
#to another, and errors to another. 

#Will need to have 2 loops running i (for each annulus) 
#j (for freezing and thawing parameters)

#need to sort out metallicities too.......

set totpars=`echo $npars $N | awk '{print $1*$2*3}'`  

set i=1
set j=1
while ( $i <= $N )

  while ( $j <= $N ) # when i=j, then parameters need to be thawed...
    if ( $i == $j ) then
cat >> ${outfile} <<EOF
freeze 1-${totpars}
EOF
     #Need to thaw T, Z, Norm, Norm2, Gamma, Gamma_norm, 
     set p=`echo $j $npars | awk '{print ($1*3*$2)-45+2 }'` #T
     set q=`echo $j $npars | awk '{print ($1*3*$2)-45+4 }'` #A
     set r=`echo $j $npars | awk '{print ($1*3*$2)-45+7 }'` #N
     set s=`echo $j $npars | awk '{print ($1*3*$2)-45+13 }'` #N2
     set t=`echo $j $npars | awk '{print ($1*3*$2)-45+14 }'` #N2
     set u=`echo $j $npars | awk '{print ($1*3*$2)-45+15 }'` #NG
     set v=`echo $j $npars | awk '{print ($1*3*$2)-45+8 }'` #kT2
     set s1=`echo $j $npars | awk '{print ($1*3*$2)-30+13 }'` #N2
     set t1=`echo $j $npars | awk '{print ($1*3*$2)-30+14 }'` #N2
     set u1=`echo $j $npars | awk '{print ($1*3*$2)-30+15 }'` #NG
     set s2=`echo $j $npars | awk '{print ($1*3*$2)-15+13 }'` #N2
     set t2=`echo $j $npars | awk '{print ($1*3*$2)-15+14 }'` #N2
     set u2=`echo $j $npars | awk '{print ($1*3*$2)-15+15 }'` #NG
     if ( $met == 0 ) then # met=0 i.e. fixed
      if ( ${m1_gam} == 0 ) then
cat >> ${outfile} <<EOF
thaw ${p} ${r} ${s} 
EOF
      goto m2
      else 
cat >> ${outfile} <<EOF
thaw ${p} ${r} ${s} ${u} 
EOF
      goto m2
      endif
     endif #met==0
     if ( $met == N ) then #met=N
      if ( ${m1_gam} == 0 ) then
cat >> ${outfile} <<EOF
thaw ${p} ${q} ${r} ${s} 
EOF
      else 
cat >> ${outfile} <<EOF
thaw ${p} ${q} ${r} ${s} ${u} 
EOF
      endif
     endif #met=N i.e. all thawed
     set k=`echo $j | awk '{print $1 - 1}'`
     if ( $met != N ) then
     if ( $met > $j ) then #met => j
      if ( ${m1_gam} == 0 ) then
cat >> ${outfile} <<EOF
thaw ${p} ${q} ${r} ${s} 
EOF
      else 
cat >> ${outfile} <<EOF
thaw ${p} ${q} ${r} ${s} ${u} 
EOF
     endif
     else
      if ( ${m1_gam} == 0 ) then
cat >> ${outfile} <<EOF
thaw ${p} ${r} ${s} 
EOF
      else 
cat >> ${outfile} <<EOF
thaw ${p} ${r} ${s} ${u} 
EOF
      endif
     endif #met =>j
     endif
m2:
      if ( $m2_gam == 0 ) then
cat >> ${outfile} <<EOF
thaw ${s1} 
EOF
      else
cat >> ${outfile} <<EOF
thaw ${s1} ${u1}
EOF
      endif
      if ( $pn_gam == 0 ) then
cat >> ${outfile} <<EOF
thaw ${s2} 
EOF
       else
cat >> ${outfile} <<EOF
thaw ${s2} ${u2}
EOF
      endif
      #Now correct data is thawed, need to refit again. Output parameters
      #to various logfiles.....
cat >> ${outfile} <<EOF
fit
fit
log ${root}_${i}_vals1.txt
show par ${p}
show par ${q}
show par ${r}
log none
log ${root}_${i}_res1.txt
show par ${v}
show par ${s}
show par ${t}
show par ${u}
show par ${s1}
show par ${t1}
show par ${u1}
show par ${s2}
show par ${t2}
show par ${u2}
log none
EOF
      #Are there errors?
      if ( $err == 0 ) then #0=yes calc errors for pqr
cat >> ${outfile} <<EOF
log ${root}_${i}_err1.txt
err ${p} ${q}
log none
EOF
      endif

    endif #if i=j
    

  @ j++
  end


set j=1
@ i++
end


