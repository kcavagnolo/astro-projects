#!/bin/tcsh

#script to convert a CIAO region file into an image data table
#for easy reading into FORTRAN. 


#input file is a CIAO region file in PHYS coordinates.

set version=1.0 

# Version 1.0    RFT 13/09/05

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

--------------------`echo $0:t | tr '[a-z]' '[A-Z]'` version $version `date +%x`--------------

EOF

if ( $#argv != 1 ) then
cat <<EOF

Use phys2im.csh region

 region   -   a region file saved in PHYS coordinates
    

EOF
exit
endif

set reg=$1

if ( ! -e $reg ) then
 echo "Error: $reg does not exist"
 exit
endif

#check regions are in ciao format
set rtype=`head -1 $reg | awk '{print $5}'`
if ( $rtype != CIAO ) then
 echo "  ERROR - REGION FILE DOES NOT APPEAR TO BE IN CIAO FORMAT\n\n"
 exit
endif

# conversion factors:
# all input manually
# assuming x,y are scaled the same IM=(PHYS+C1)*C2
# unfortunately they are not!
#
#
set c1=62.906
set c2=`echo 1 64 | awk '{print $1/$2}'`
set c3=62.941
set c4=`echo 1 64 | awk '{print $1/$2}'`

#how many regions?

grep -v CIAO $reg | grep -v '^\s*$' >! tmpsrc.txt
set numsrc=`wc -l tmpsrc.txt | awk '{print $1}'`
rm tmpsrc.txt
echo "$numsrc" sources
#cycle through all regions

set out=sources.txt
if ( -e $out ) then
 rm $out
endif 
touch $out

set i=1
while ( $i <= $numsrc)
echo $i
set j=`echo $i | awk '{print $1+1}'`
set x=`head -${j} $reg | tail -1 | cut -d ',' -f1 | cut -d '(' -f2`
set y=`head -${j} $reg | tail -1 | cut -d ',' -f2`
set r=`head -${j} $reg | tail -1 | cut -d ',' -f3 | cut -d ')' -f1`

#convert to image coords

set x=`echo $x $c1 $c2 | awk '{print ($1+$2)*$3}'`
set y=`echo $y $c3 $c4 | awk '{print ($1+$2)*$3}'`
set r=`echo $r $c2 | awk '{print $1*$2}'`
#write data to output file.

printf  "%g\t%g\t%g\n" $x $y $r >> $out


@ i++
end


