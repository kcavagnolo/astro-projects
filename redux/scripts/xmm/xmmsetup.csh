#! /bin/tcsh
#
# script to set up xmm sas for a set of data.
# run in the directory with your data (ODF) files
#
# version 1.1 - 12/11/01 by BJM
# version 1.2 - 4/3/02 BJM changed for lnxa local stuff
# version 1.3 - 11/3/02 BJM changed SAS_ODF variable to point to odf directory not SUM.SAS
#               file - required for epchain.
# version 1.4 - 05/05/04 RFT changed path for CCF files
# version 1.5 - 21/07/04 RFT removed the SAS_CCFFILES variable. Latest version of SAS
#		doesnt like the variable as odfingest keep crashing even thought the
#		pointing is to the correct directory.

set version=1.5

set tmp_sys=`uname -a`
if ( `echo $tmp_sys|grep -c Generic` == "1" ) then
 set tmp_machine=solaris
 alias echo '/bin/echo'
endif
if ( "$tmp_sys" =~ Linux* ) then
 set tmp_machine=linux
endif

cat <<EOF

----------------------XMMSETUP.CSH version $version `date +%x`--------------------

NOTE: You should SOURCE this script to set the environment variables in your shell.

EOF

#find host machine
set host=`echo $HOST|tr '.' ' '|awk '{print $1}'`

#Set environment variables
set odfdir=`pwd`
setenv SAS_ODF ${odfdir}
if ( $host != lnxa ) then
 setenv SAS_CCFPATH /caldb/data/xmm/ccf/
#setenv SAS_CCFFILES /xmm2/caldb/xmm/ccf/
#setenv SAS_CCFPATH /data/rft/ccf
#setenv SAS_CCFFILES /data/rft/ccf

else setenv SAS_CCFPATH /exgal6/bjm/caldb/xmm/ccf
 setenv SAS_CCFFILES /exgal6/bjm/caldb/xmm/ccf
endif
setenv SAS_VERBOSITY 0
setenv SAS_SUPRESS_WARNING 3
setenv SAS_IMAGEVIEWER ds9

#if ccf.cif doesn't exist, then run cifbuild
if ( ! -e ccf.cif ) then
    echo "No ccf.cif file found, so running cifbuild...\n"
    cifbuild withccfpath=no analysisdate=now category=XMMCCF fullpath=yes
else echo "ccf.cif file found, so cifbuild will not be run.\nIf you want to run it then delete or rename ccf.cif, and rerun this script.\n"
endif

setenv SAS_CCF ${odfdir}/ccf.cif

#if SUM.SAS doesn't exist, then run odfingest
set test=`ls *SUM* | grep "SUM.SAS" | wc -l`
if ( $test == 0 ) then
    echo "No SUM.SAS file found, so running odfingest...\n"
    odfingest odfdir=$SAS_ODF outdir=$SAS_ODF
else echo "SUM.SAS file found, so odfingest will not be run.\nIf you want to run it then delete or rename SUM.SAS, and rerun this script.\n"
endif

#Commented out because epchain doesn't like the SAS_ODF variable pointing to the SUM.SAS file
#It must just point to the odf directory
#set sumfile=`ls -1 *SUM.SAS`
#setenv SAS_ODF ${odfdir}/${sumfile}

exit
