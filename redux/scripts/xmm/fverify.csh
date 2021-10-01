#! /bin/tcsh 
# Script to go through all the files in a directory, and run fverify on them, 
# and report which ones fail.

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

--------------------`/bin/echo $0:t | tr '[a-z]' '[A-Z]'` version $version---------------------

EOF

if ( $#argv != 1 ) then
cat <<EOF

Use $0:t log

    log            - log file 

EOF
exit
endif

set log=$1

/bin/echo "Input was $0 $*\n\n"

#remove old log file
if ( -e $log ) then
 rm $log
endif
touch $log

foreach file ( `ls` )
 fverify $file \!tmp.txt >&! tmp.txt
 set error=`grep Error: < tmp.txt | wc -l`
 if ( $error == 0 ) then
  /bin/echo "  $file passes fverify"
 else /bin/echo "***  $file fails fverify  ***"
  more tmp.txt >> $log
 endif
end

/bin/echo "  log of failures written to $log\n\n"

exit

