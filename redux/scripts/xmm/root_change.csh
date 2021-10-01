#! /bin/tcsh 
#
# Script to change the root of files - input the root and its 
# replacement and the script will rename all files beginning
# with root in the current directory.
#
# version 1 - 5/7/01 by BJM

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


---------------------ROOT_CHANGE.CSH VERSION $version-----------------


EOF

if ( $#argv != 2 ) then
cat <<EOF
Use root_change.csh root replacement

    root     -  The root you want to replace,
    replacement - Its replacement.

NOTE: All files in this directory, beginning "root" will be renamed.

EOF
exit
endif

set root=$1
set new=$2

foreach file ( ${root}* )
 mv $file `echo $file | sed s%${root}%${new}%` 
end

exit
