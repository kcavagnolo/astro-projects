#! /bin/tcsh 
#
# Script to compare certain keywords in the headers of two fits files, report 
# any differences, and copy the keyword from the second to the first if
# the first is missing any of the keywords

#
# Version 1 - 6/9/01 by BJM

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

--------------------XMMFKEYCHECK.CSH version $version---------------------

EOF

if ( $#argv != 3 ) then
cat <<EOF

Use xmmfkeycheck.csh "fits1[x]" "fits2[y]" root

    fits1[x]    - the name of the first fits file, where x is the
                  extension number
    fits2[y]    - the name of the second fits file, where y is the
                  extension number
    root        - root name for all output files.

The script will compare keywords in the headers of fits1[x] and fits2[y],
report any differences, and if any of the keywords is missing from the
header of fits1[x] they will be copied from the header of fits2[y].
Note the input file fits1 is changed, but fits2 won't be.
Output are the text file used to modify fits1, and another that can be 
used to undo the changes.

EOF
exit
endif

set fits1="$1"
set fits2="$2"
set root=$3

#check input files exist
set f1root=`echo "$fits1" | cut -d "[" -f1`
if ( ! -e $f1root ) then
 echo "Error: $f1root does not exist"
 exit
endif
set f2root=`echo "$fits2" | cut -d "[" -f1`
if ( ! -e $f2root ) then
 echo "Error: $f2root does not exist"
 exit
endif

echo "Input was $0 $*\n\n"

#prepare fmodhead files
if ( -e ${root}modify.txt ) then
 rm ${root}modify.txt
endif
touch ${root}modify.txt
if  ( -e ${root}revert.txt ) then
 rm ${root}revert.txt
endif
touch ${root}revert.txt

set modified=0

#loop over keywords
foreach key ( DATE-OBS INSTRUME TELESCOP DATAMODE FILTER RA_PNT DEC_PNT PA_PNT REFXCTYP REFXCRPX REFXCRVL REFXCDLT REFYCTYP REFYCRPX REFYCRVL REFYCDLT )
echo "Keyword $key"
set keyval1=`fkeyprint "$fits1" $key | tail -1`
set test1=`echo "$keyval1" | grep EXTENSION | wc -l`
if ( $test1 == 1 ) then
 echo "$fits1 - no keyword $key found."
endif
#echo "keyval1 = $keyval1"
set keyval2=`fkeyprint "$fits2" $key | tail -1`
set test2=`echo "$keyval2" | grep EXTENSION | wc -l`
if ( $test2 == 1 ) then
 echo "$fits2 - no keyword $key found.\n"
endif
#echo "keyval2 = $keyval2"
if (( $test1 != 1 )&&( $test2 != 1 )) then
 if ( "$keyval1" == "$keyval2" ) then
  echo "$key same in both files.\n"
 else echo "Keyword difference..."
  echo "  $fits1 - $keyval1"
  echo "  $fits2 - $keyval2\n"
 endif
endif
if (( $test1 == 1 )&&( $test2 != 1 )) then
set modified=1
echo "$key - will copy value $keyval2 from $fits2 to $fits1\n"
cat >> ${root}modify.txt <<EOF 
$keyval2
EOF
cat >> ${root}revert.txt <<EOF 
-$key
EOF
endif
end

if ( $modified == 1 ) then
 echo "Modifying $fits1 header..."
 fmodhead "$fits1" ${root}modify.txt
 echo "  done\n"
 echo "To revert to original $fits1 use the command fmodhead '${fits1}' ${root}revert.txt\n"
else echo "No changes were made\n"
endif

exit
