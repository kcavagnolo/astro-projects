#! /bin/tcsh 
#
# script to set up xmm sas for a set of data.
# run in the directory with your data (ODF) files

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

----------------------XMMSETUP.CSH version $version---------------------

EOF

# check that sas is running
#set check=`echo $SAS_DIR`
#if ( check =~ *undefined*) then
#    echo "ERROR: SAS not running.\n"
#endif

#find host machine
set host=`echo $HOST|tr '.' ' '|awk '{print $1}'`

#Set environment variables
set odfdir=`pwd`
setenv SAS_ODF ${odfdir}
setenv SAS_CCFPATH /usr/local/ccf
setenv SAS_CCFFILES /usr/local/ccf
setenv SAS_CCF ${odfdir}/ccf.cif
setenv SAS_VERBOSITY 1
setenv SAS_SUPRESS_WARNING 3
setenv SAS_IMAGEVIEWER ds9

#if ccf.cif doesn't exist, then run cifbuild
if ( ! -e ccf.cif ) then
    echo "No ccf.cif file found, so running cifbuild...\n"
    cifbuild
    setenv SAS_CCF ${odfdir}/ccf.cif
else echo "ccf.cif file found, so cifbuild will not be run.\nIf you want to run it then delete or rename ccf.cif, and rerun this script.\n"
endif

#if SUM.SAS doesn't exist, then run odfingest
#set test=`ls *SUM* | grep "SUM.SAS" | wc -l`
#if ( $test == 0 ) then
#    echo "No SUM.SAS file found, so running odfingest...\n"
#    odfingest
#else echo "SUM.SAS file found, so odfingest will not be run.\nIf you want to run it then delete or rename SUM.SAS, and rerun this script.\n"
#endif

# Re-define SAS_ODF
#set sumfile=`ls -1 *SUM.SAS`
#setenv SAS_ODF ${odfdir}/${sumfile}

exit
