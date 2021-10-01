#! /bin/tcsh 
#
# Script to convert ds9 region file into a filter expression
#
# Use ciao format region files
# Currently only works for circles, boxes, annuli, and ellipses
#
# version 1 - by BJM 5/8/01

set version=1

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

--------------------xmmstring.csh version $version---------------------

EOF

if ( $#argv != 1 ) then
cat <<EOF

Use xmmstring.csh region

    region      - a CIAO format region file of the regions you wish to filter.
                  Each region in the file should be on a new line, and excluded
                  regions should begin with '-'. e.g.
                  # Region file format: CIAO version 1.0
                  circle(14422,24933,1000)   
                  -circle(14422,24933,500)   

EOF
endif

set reg=$1

#check input files exist
if ( ! -e $reg ) then
 echo "Error: $reg does not exist"
 exit
endif

echo "Input was xmmfilter.csh $1"
echo ""
echo ""

#make filter expression, using reg file
if ( -e tempexc ) then
rm tempexc
endif
if ( -e tempinc ) then
rm tempinc
endif
echo "Creating new filter from region file..."
echo ""
#set expr=
set regfilt=
set num=`wc -l $reg | awk '{print $1}'`
set i=1
#Seperate included and excluded regions first
grep -v "#" < $reg | egrep -v "^-" | sed s%rotbox%box% > tempinc
set numinc=`wc -l tempinc | awk '{print $1}'`
egrep "^-" < $reg | sed s%rotbox%box% > tempexc
set numexc=`wc -l tempexc | awk '{print $1}'`

#test if all excluded regions
#if all excluded regions use &&!(reg1 || reg2 || reg3...)
if ( $numinc == 0 ) then
while ( $i <= $numexc )
 set inc=`head -$i tempexc | tail -1 | egrep -v "^-" | wc -l`
 set temp=`head -$i tempexc | tail -1`
 set length=`echo $temp | wc -c | awk '{print $1-1}'`
 if ( $inc == 0 ) then
  set temp=`echo $temp | cut -c2-$length`
 endif
 set shape=`echo $temp | cut -d"(" -f1`
#convert to halfwidths if box
 if ( $shape == box ) then
  set xcent=`echo $temp | cut -d "(" -f2 | cut -d "," -f1`
  set ycent=`echo $temp | cut -d "," -f2`
  set xhalfwidth=`echo $temp | awk -F "," '{print $3/2}'`
  set yhalfwidth=`echo $temp | awk -F "," '{print $4/2}'`
  set angle=`echo $temp | cut -d ")" -f1 | cut -d "," -f5`
  set temp="box(${xcent},${ycent},${xhalfwidth},${yhalfwidth},${angle})"
  #else set temp=`head -$i tempexc | tail -1 | sed s%rotbox%box% | cut -d"-" -f2`
 endif
#add X,Y to end of region
 set temp=`echo $temp | cut -d")" -f1`,X,Y\) 
 if ( $i == 1 ) then
  set regfilt="${temp}"
  else  set regfilt="${regfilt}||${temp}"
 endif
@ i++
end
#set expr="${expr}"\&\&\!\("${regfilt}"\)
set expr=\("${regfilt}"\)
endif

#if some included regions use &&(reg1 || reg2 &&! reg3...)
if ( $numinc != 0 ) then
#do included regions
set i=1
while ( $i <= $numinc )
 set inc=`head -$i tempinc | tail -1 | egrep -v "^-" | wc -l`
 set temp=`head -$i tempinc | tail -1`
 set length=`echo $temp | wc -c | awk '{print $1-1}'`
 if ( $inc == 0 ) then
  set temp=`echo $temp | cut -c2-$length`
 endif
 #echo "inc = $inc"
 #echo "temp = $temp"
 set shape=`echo $temp | cut -d"(" -f1`
#convert to halfwidths if box
 if ( $shape == box ) then
  set xcent=`echo $temp | cut -d "(" -f2 | cut -d "," -f1`
  set ycent=`echo $temp | cut -d "," -f2`
  set xhalfwidth=`echo $temp | awk -F "," '{print $3/2}'`
  set yhalfwidth=`echo $temp | awk -F "," '{print $4/2}'`
  set angle=`echo $temp | cut -d ")" -f1 | cut -d "," -f5`
  set temp="box(${xcent},${ycent},${xhalfwidth},${yhalfwidth},${angle})"
 endif
#add X,Y to end of region
 set temp=`echo $temp | cut -d")" -f1`,X,Y\) 
 if ( $i == 1 ) then
  set regfilt="${temp}"
  else set regfilt="${regfilt}||${temp}"
 endif
@ i++
end
#do excluded regions
set i=1
while ( $i <= $numexc )
 set inc=`head -$i tempexc | tail -1 | egrep -v "^-" | wc -l`
 set temp=`head -$i tempexc | tail -1`
 set length=`echo $temp | wc -c | awk '{print $1-1}'`
 if ( $inc == 0 ) then
  set temp=`echo $temp | cut -c2-$length`
 endif
 #echo "inc = $inc"
 #echo "temp = $temp"
 set shape=`echo $temp | cut -d"(" -f1`
#convert to halfwidths if box
 if ( $shape == box ) then
  set xcent=`echo $temp | cut -d "(" -f2 | cut -d "," -f1`
  set ycent=`echo $temp | cut -d "," -f2`
  set xhalfwidth=`echo $temp | awk -F "," '{print $3/2}'`
  set yhalfwidth=`echo $temp | awk -F "," '{print $4/2}'`
  set angle=`echo $temp | cut -d ")" -f1 | cut -d "," -f5`
  set temp="box(${xcent},${ycent},${xhalfwidth},${yhalfwidth},${angle})"
 endif
#add X,Y to end of region
 set temp=`echo $temp | cut -d")" -f1`,X,Y\) 
# echo $temp
 set regfilt="${regfilt}&&\!${temp}"
@ i++
end

#set expr="$expr"\&\&\("$regfilt"\)
set expr=\("$regfilt"\)
endif

if ( -e tempexc ) then
rm tempexc
endif
if ( -e tempinc ) then
rm tempinc
endif

echo "Filter expression is $expr"
set stringout=`echo $reg | cut -d"." -f1`_filter.txt
if ( -e $stringout ) then
 rm $stringout
endif
echo "$expr" > $stringout
echo "Copy written to $stringout"
echo ""

exit
