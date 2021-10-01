#!/bin/tcsh

#script to identify source regions from the .fits files determined
#using the script src_detect.csh. Script might end up being a bolt-on
# to aforementioned script.

#input files are the .fits files at the moment. 

#problems to overcome
#1) reading datafiles
#2) multiple overcounting of regions. seems that it is regions *6
#3) image coords are different in each camera (mos1, mos2 look the same)
#4) want to have one source region file for all cameras. 
#   Needs to be eventually in PHYS coords. 
#5) Maybe convert from IM-PHYS in this script. Makes the script bulky though. 
#6) Going to need to convert between PHYS and IM in script. 
#   Will need 2 input x and y coords for each camera. Manual input I think 

# version 1.0 RFT 14/03/06
set version=1.0

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

if ( $#argv != 3 ) then
cat <<EOF

Use src_detect.csh mos1_src mos2_src pn_src

    mos1_src	- mos1 source region file
                  e.g. m1_emllist.fits    

    mos1_src    - mos2_source region file

    pn_src      - pn_source region file

EOF
exit
endif

set s1=$1
set s2=$2
set s3=$3

#check input files exist
if ( ! -e $s1 ) then
 echo "Error: $evt1 does not exist"
 exit
endif

if ( ! -e $s2 ) then
 echo "Error: $evt2 does not exist"
 exit
endif

if ( ! -e $s3 ) then
 echo "Error: $evt3 does not exist"
 exit
endif

dmlist "m1_emllist.fits[cols X_IMA,Y_IMA]" opt=data outfile=m1.txt
dmlist "m2_emllist.fits[cols X_IMA,Y_IMA]" opt=data outfile=m2.txt
dmlist "pn_emllist.fits[cols X_IMA,Y_IMA]" opt=data outfile=pn.txt



# Starting just with mos1

echo "Analysing mos1...\n"

#create output file (for all three)
if ( -e src.reg ) then
 rm -fr src.reg
endif
touch src.reg
echo "# Region file format: CIAO version 1.0" >> src.reg
#create buffer file for comparing other cameras. 
if ( -e temp.txt ) then
 rm -fr temp.txt
endif
touch temp.txt

#determine coordinate conversion factors:
set x1im=800
set x2im=400
set x1ph=31980.5
set x2ph=15980.5
set y1im=800
set y2im=400
set y1ph=31980.5
set y2ph=15980.5
# conv takes the form xI=axP+b where a= (x1I-x2I)/(x1P-x2P)
# and b=x1I-a*x1P

set ax=`echo $x1im $x2im $x1ph $x2ph | awk '{print ($1-$2)/($3-$4)}'`
set ay=`echo $y1im $y2im $y1ph $y2ph | awk '{print ($1-$2)/($3-$4)}'`
set bx=`echo $x1im $x1ph $ax | awk '{print $1-($2*$3)}'`
set by=`echo $y1im $y1ph $ay | awk '{print $1-($2*$3)}'`

#get number of lines in the file
set N=`wc -l < m1.txt`
cp m1.txt tmpreg

#cycle through til the first row is determined

set n=1
set N1=0
set j=1

while ( $n <= $N )
#echo $n
 set exp1=`head -$n m1.txt | tail -1`

#determine the row number which corresponds to the first data point
set exp2=`echo $exp1 | awk '{print $1}'`
 if ( $exp2 == 1 ) then
  set N1=$n
 endif
#determine how many data point there are then
set N2=`echo $N $N1 | awk '{print $1-$2+1}'`

 if ( $N1 != 0 ) then
  set exp3=`echo $exp1 | awk '{print $2}'`
  set exp4=`echo $exp1 | awk '{print $3}'`
# Data is stored as $exp2 $exp3 $exp4
# $exp2 is just the row number but a useful counter
# As there are 6 rows each time, just store the values 
#  corresponding to $exp2/6    
  set exp5=`echo $j | awk '{print $1 * 6}'`
   if ( $exp2 == $exp5 ) then
     set j2=`echo $N2 | awk '{print $1 / 6}'`
     set exp3=`echo $exp3 | cut -c1-6`
     set exp4=`echo $exp4 | cut -c1-6`
     set exp3=`echo $exp3 $ax $bx | awk '{print ($1-$3)/$2}'`
     set exp4=`echo $exp4 $ay $by | awk '{print ($1-$3)/$2}'`
      echo "circle(${exp3},${exp4},400)" >> src.reg  
# Write to buffer
      echo "${exp3} ${exp4}" >> temp.txt
      echo $j of $j2
     @ j = $j + 1
   endif
 endif

@ n++
end

# need a threshold radius to compare.
# arbitrarily choosing a radius of 100 PHYS pixels = 5 arcsec


# Continuing with mos2
echo "Analysing mos2...\n"

#determine coordinate conversion factors:
set x1im=800
set x2im=400
set x1ph=31980.5
set x2ph=15980.5
set y1im=800
set y2im=400
set y1ph=31980.5
set y2ph=15980.5
# conv takes the form xI=axP+b where a= (x1I-x2I)/(x1P-x2P)
# and b=x1I-a*x1P

set ax=`echo $x1im $x2im $x1ph $x2ph | awk '{print ($1-$2)/($3-$4)}'`
set ay=`echo $y1im $y2im $y1ph $y2ph | awk '{print ($1-$2)/($3-$4)}'`
set bx=`echo $x1im $x1ph $ax | awk '{print $1-($2*$3)}'`
set by=`echo $y1im $y1ph $ay | awk '{print $1-($2*$3)}'`

#get number of lines in the file
set N=`wc -l < m2.txt`

#cycle through til the first row is determined

set n=1
set N1=0
set j=1
set count=0

while ( $n <= $N )
 set exp1=`head -$n m2.txt | tail -1`
#determine the row number which corresponds to the first data point
set exp2=`echo $exp1 | awk '{print $1}'`
 if ( $exp2 == 1 ) then
  set N1=$n
 endif
#determine how many data point there are then
set N2=`echo $N $N1 | awk '{print $1-$2+1}'`
 if ( $N1 != 0 ) then
  set exp3=`echo $exp1 | awk '{print $2}'`
  set exp4=`echo $exp1 | awk '{print $3}'`
# Data is stored as $exp2 $exp3 $exp4
# $exp2 is just the row number but a useful counter
# As there are 6 rows each time, just store the values 
#  corresponding to $exp2/6    
  set exp5=`echo $j | awk '{print $1 * 6}'`
   if ( $exp2 == $exp5 ) then
     set j2=`echo $N2 | awk '{print $1 / 6}'`
     set exp3=`echo $exp3 | cut -c1-6`
     set exp4=`echo $exp4 | cut -c1-6`
     set exp3=`echo $exp3 $ax $bx | awk '{print ($1-$3)/$2}'`
     set exp4=`echo $exp4 $ay $by | awk '{print ($1-$3)/$2}'`

# Determine if the source region corresponds to one already in src.reg
# Need to cycle through temp.txt for each source.
      set NUM=`wc -l < temp.txt`   
      set k=1
      set flg=1
       while ( $k <= $NUM )
        set exp6=`head -$k temp.txt | tail -1`          
        set exp7=`echo $exp6 | awk '{print $1}'`
        set exp8=`echo $exp6 | awk '{print $2}'`
        set D=`echo $exp3 $exp4 $exp7 $exp8 | awk '{print sqrt( (($1-$3)^2)+(($2-$4)^2) ) }' | cut -d"." -f1`
#        echo D is $D
         if ( $D <= 100 ) then #a positive.  
          set k=$NUM # i.e. no need to carry on scanning. Region already there.
          set flg=0
         endif      
       @ k++
       end
      if ( $flg == 1 ) then 
       echo "circle(${exp3},${exp4},400)" >> src.reg
       echo "Region added $exp3 $exp4"
       echo "${exp3} ${exp4}" >> temp.txt  # new region added to tmp.txt
       set count=`echo $count | awk '{print $1 +1 }'`
      endif
     echo $j of $j2
     @ j = $j + 1
   endif
 endif
@ n++
end
echo "$count regions added to src.reg from mos2"


# Finally for pn
echo "Analysing pn...\n"

#determine coordinate conversion factors:
set x1im=800
set x2im=400
set x1ph=63960.5
set x2ph=31960.5
set y1im=800
set y2im=400
set y1ph=63960.5
set y2ph=31960.5
# conv takes the form xI=axP+b where a= (x1I-x2I)/(x1P-x2P)
# and b=x1I-a*x1P

set ax=`echo $x1im $x2im $x1ph $x2ph | awk '{print ($1-$2)/($3-$4)}'`
set ay=`echo $y1im $y2im $y1ph $y2ph | awk '{print ($1-$2)/($3-$4)}'`
set bx=`echo $x1im $x1ph $ax | awk '{print $1-($2*$3)}'`
set by=`echo $y1im $y1ph $ay | awk '{print $1-($2*$3)}'`

#get number of lines in the file
set N=`wc -l < pn.txt`

#cycle through til the first row is determined

set n=1
set N1=0
set j=1
set count1=0

while ( $n <= $N )
#echo $n
 set exp1=`head -$n pn.txt | tail -1`

#determine the row number which corresponds to the first data point
set exp2=`echo $exp1 | awk '{print $1}'`
 if ( $exp2 == 1 ) then
  set N1=$n
 endif
#determine how many data point there are then
set N2=`echo $N $N1 | awk '{print $1-$2+1}'`

 if ( $N1 != 0 ) then
  set exp3=`echo $exp1 | awk '{print $2}'`
  set exp4=`echo $exp1 | awk '{print $3}'`
# Data is stored as $exp2 $exp3 $exp4
# $exp2 is just the row number but a useful counter
# As there are 6 rows each time, just store the values 
#  corresponding to $exp2/6    
  set exp5=`echo $j | awk '{print $1 * 6}'`
   if ( $exp2 == $exp5 ) then
     set j2=`echo $N2 | awk '{print $1 / 6}'`
     set exp3=`echo $exp3 | cut -c1-6`
     set exp4=`echo $exp4 | cut -c1-6`
     set exp3=`echo $exp3 $ax $bx | awk '{print ($1-$3)/$2}'`
     set exp4=`echo $exp4 $ay $by | awk '{print ($1-$3)/$2}'`

# Determine if the source region corresponds to one already in src.reg
# Need to cycle through temp.txt for each source.
      set NUM=`wc -l < temp.txt`   
      set k=1
      set flg=1
       while ( $k <= $NUM )
        set exp6=`head -$k temp.txt | tail -1`          
        set exp7=`echo $exp6 | awk '{print $1}'`
        set exp8=`echo $exp6 | awk '{print $2}'`
        set D=`echo $exp3 $exp4 $exp7 $exp8 | awk '{print sqrt( (($1-$3)^2)+(($2-$4)^2) ) }' | cut -d"." -f1`
#        echo D is $D
         if ( $D <= 100 ) then #a positive. 
          set k=$NUM # i.e. no need to carry on scanning. Region already there.
          set flg=0
         endif      
       @ k++
       end
      if ( $flg == 1 ) then 
       echo "circle(${exp3},${exp4},400)" >> src.reg
       echo "Region added $exp3 $exp4"
       echo "${exp3} ${exp4}" >> temp.txt  # new region added to tmp.txt
       set count1=`echo $count1 | awk '{print $1 +1 }'`
      endif
     echo $j of $j2
     @ j = $j + 1
   endif
 endif

@ n++
end
echo $count1 regions added to src.reg from pn

echo "Final region file written to src.reg\n"
# Note that the final region file is likely to contain a lot of cluster emission.
# You will probably have to delete many of the central regions. 

echo "All done!"


#All looks pretty reasonable so far. 

# Next step is to create one source region file. 

# Plan: get mos1 data to write to an output file and source region file. 
# e.g. test.txt and src.reg (the latter is the final output file)
# when cycling through m2.txt, I should compare every value with test.txt
# There should be a closeness parameter in x and y. If the region is not found
# then it appends to src.reg. Either way, test.txt is appended with
# each iteration. Repeat finally for pn. Might be best doing this by altering 
# the code above as opposed to having to create a whole set of loops. 


#delete unwanted files
# rm -fr temp.txt 
