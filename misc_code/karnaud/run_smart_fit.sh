#!/bin/sh
# Script to start xspec and do smart fitting automatically
# A. Ptak 12/01

if test $# -lt 2; then
    echo "Usage: $0 xcmfile dchi (extraargs_for_smart_fit)"
    echo "Computes errors on paramters in xcmfile using smart_fit"
    echo "Assumes that smart_fit, etc. tcl scripts are installed in $HOME/.xspec"
    echo "Errors are computed for delta fit statistic = dchi (dchi=4.605 corresponds"
    echo "to the 90% confidence intervals assuming 2 interesting parameters)"
    echo "All other params are passed to smart_fit"
    exit
fi

if test ! -d $HOME/.xspec; then
    echo "Error, $HOME/.xspec not found"
fi

xspec_tcl_list="dof.tcl get_stat.tcl get_xspec_home.tcl interpolate.tcl linfit.tcl mean.tcl qdp1.tcl qdp2.tcl setup_mod.tcl sim_model.tcl _smart_error_nextstep.tcl smart_error.tcl smart_fit.tcl smart_step.tcl"

# Make setup file
xcmfile=$1
dchi=$2
if test ! -f $xcmfile; then
    xcmfile="$xcmfile.xcm"
fi
if test ! -f $xcmfile; then
    echo "Error, cannot find $xcmfile"
fi
root=`basename $xcmfile .xcm`
echo "root = $root"
setup=$root\_$$.xcm
echo "# Tempfile created at `date`" > $setup
for tcl in $xspec_tcl_list; do
    tclpath=$HOME/.xspec/$tcl
    if test ! -f $tclpath; then
	echo "Error, cannot find $tclpath"
	\rm -f $setup
    fi
    echo "source $HOME/.xspec/$tcl" >> $setup
done
echo "@$xcmfile" >> $setup
# avoid inf. loop if a file loaded by xcmfile is not found
echo "none" >> $setup 
# Make sure fitting is possible
echo "tclout dof" >> $setup
echo "if {\$xspec_tclout < 1} {" >> $setup
echo "  puts \"Bad fit, exiting\"" >> $setup
echo "  exit" >> $setup
echo "  y" >> $setup
echo "" >> $setup
echo "" >> $setup
echo "" >> $setup
echo "}" >>  $setup
echo "query y" >> $setup
echo "fit 200 0.0001" >> $setup
echo "smart_fit none logroot=$root dchi=$dchi $*" >> $setup
echo "  exit" >> $setup
echo "  y" >> $setup
echo "" >> $setup
echo "" >> $setup
echo "" >> $setup

xspec < $setup > /tmp/$root\_$$.log
\rm $setup




