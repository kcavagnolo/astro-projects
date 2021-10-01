#! /bin/tcsh -f
#
# script to rename all files in a directory to upper case,
# uses upcase.pl perl script from xmm website.
#
# version 1 - BJM 27/08/01

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

--------------------UPCASE.CSH version $version---------------------

EOF

if ( $#argv != 1 ) then
cat <<EOF

Use upcase.csh dir

    dir   - The toplevel directory to uppercase within

EOF
exit
endif

set dir=$1

#check input files exist
if ( ! -e $dir ) then
 echo "Error: $dir does not exist"
 exit
endif

echo "Input was $0 $*\n\n"

if ( -e temp ) then
 rm temp
endif
cat > temp <<EOF
$dir

EOF
if ( -e rename.csh ) then
 rm rename.csh
endif
upcase.pl < temp > rename.csh
chmod u=rwx rename.csh

rename.csh

echo "Filenames in $dir have been uppercased, uppercaseing script written to rename.csh\n"
if ( -e temp ) then
 rm temp
endif

exit
