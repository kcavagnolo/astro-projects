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

#This modification is to try and sort out error calculations in the central bin
#Going to try and do each annulus seperately. 

set version=1.1


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


#0. Need a HUGE while loops to run through each series of annuli....

set ii=1
while ( $ii <= $N ) 

#1. Add data to the outfile. Simpler than it was before


 set M=`echo $N | awk '{print $1*3}'`
 set spec_in="1:1 ${m1_root}_an${ii}.fits 2:2 ${m2_root}_an${ii}.fits 3:3 ${pn_root}_an${ii}.fits"

cat >> $outfile <<EOF
data $spec_in
EOF

#2. Ignore energy regions
 if ( $ii == 1 )  then
cat >> $outfile <<EOF
ignore 1:1 **-0.5 1.4-1.8 7.0-**
ignore 2:2 **-0.5 1.4-1.8 7.0-**
ignore 3:3 **-0.5 1.4-1.8 7.5-**
EOF
 else
cat >> $outfile <<EOF
ignore 1:1 **-0.5 1.4-1.8 8.0-**
ignore 2:2 **-0.5 1.4-1.8 8.0-**
ignore 3:3 **-0.5 1.4-1.8 7.5-**
EOF
 endif
#3. Usual dross for XSPEC

cat >> $outfile <<EOF
setplot energy
cpd /xw
pl ld
 query yes
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

 set i=1
 while ( $i <= 3 ) 
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
  @ i++
 end

#4b. Tie the mekal normalisations and temperatures together

cat >> $outfile <<EOF
untie 2 4 7 17 19 22 32 34 37
newpar 17 = 2
newpar 32 = 2
newpar 19 = 4
newpar 34 = 4
newpar 22 = 7
newpar 37 = 7
EOF

#4c. And finally abundance. Remember 0=fix, N=thaw all,
#    a number means freeze in bin (and subsequent bins).
 set metflag=1 # 1=thawed
 if ( $met == N ) then
cat >> $outfile <<EOF
newpar 4 $A 1 
EOF
 endif
 if ( $met == 0 ) then
  set metflag=0
cat >> $outfile <<EOF
newpar 4 $A -1 
EOF
 endif
 if ( $met != N ) then
  if ( $met > 0 ) then
   set barrier=`echo $met | awk '{print $1 -1 }'` # -1 makes it inclusive
   if ( $ii <= $barrier ) then
cat >> $outfile <<EOF 
newpar 4 $A 1 
EOF
   else 
    set metflag=0
cat >> $outfile <<EOF
newpar 4 $A -1 
EOF
@ i++
   endif
  endif
 endif

#4d. work out powerlaws. If gam=0, then norm=0

 if ( $m1_gam == 0 ) then
cat >> $outfile <<EOF
untie 14 15
newpar 14 0.0 -1
newpar 15 0.0 -1
EOF
 else
cat >> $outfile <<EOF
untie 14 15
newpar 14 ${m1_gam} -1
newpar 15 1E-6 0.01 -1E24 -1E24 1E24 1E24
EOF
 endif
 if ( $m2_gam == 0 ) then
cat >> $outfile <<EOF
untie 29 30
newpar 29 0.0 -1
newpar 30 0.0 -1
EOF
 else
cat >> $outfile <<EOF
untie 29 30
newpar 29 ${m2_gam} -1
newpar 30 1E-6 0.01 -1E24 -1E24 1E24 1E24
EOF
 endif
 if ( $pn_gam == 0 ) then
cat >> $outfile <<EOF
untie 44 45
newpar 44 0.0 -1
newpar 45 0.0 -1
EOF
 else
cat >> $outfile <<EOF
untie 44 45
newpar 44 ${pn_gam} -1
newpar 45 1E-6 0.01 -1E24 -1E24 1E24 1E24
EOF
 endif

#5. Fit and plot the data

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

#need to sort out metallicities too.......

 set p=2  #T
 set q=4  #A
 set r=7 #N
 set s=13 #N2
 set t=14 #G1
 set u=15 #NG
 set v=8  #kT2
 set s1=28 #N2
 set t1=29 #G2
 set u1=30 #NG
 set s2=43 #N2
 set t2=44 #G3
 set u2=45 #NG

#print values first, then residuals 
cat >> ${outfile} <<EOF
log ${root}_${ii}_vals1.txt
show par ${p}
show par ${q}
show par ${r}
log none

log ${root}_${ii}_res1.txt
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

#errors? 

 if ( $err == 0 ) then #0=yes calc errors for pqr
cat >> ${outfile} <<EOF
log ${root}_${ii}_err1.txt
err 2 4 
log none
EOF
 endif


@ ii++
end










