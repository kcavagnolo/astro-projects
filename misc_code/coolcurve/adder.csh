#!/bin/csh -xv
nice +10
#-----------------------------------------------------------------------
#  csh adder.csh <lambda_file1.dat> <lambda_file2.dat> <lambda_file3>
#
#  Adds emissivity from file 2 from emissivity from file 1, and writes
#  the net emissivity to file 3, along with the correct temperature axis.
#
#-----------------------------------------------------------------------
#
#  Command line options:

set p_inp1=$1
if ( ! -e $p_inp1 ) then
  echo "  Error: Input file" $p_inp1 "does not exist."
  exit 1
endif

set p_inp2=$2
if ( ! -e $p_inp2 ) then
  echo "  Error: Input file" $p_inp2 "does not exist."
  exit 1
endif

set p_out=$3

#-----------------------------------------------------------------------
#
#  Main work area:

#  Check that the number of lines in each of the input files are the same.
set pnl1=`wc -l $p_inp1 | awk '{print $1}'`
set pnl2=`wc -l $p_inp2 | awk '{print $1}'`
if ( $pnl1 != $pnl2 ) then
  echo "  Error: Input files have different number of lines..."
  echo "    "$p_inp1 "has" $pnl1 "lines"
  echo "    "$p_inp2 "has" $pnl2 "lines"
  exit 1
endif

if ( -e $p_out ) rm $p_out
paste --delimiters=" " $p_inp1 $p_inp2 | \
    awk '{printf"%9.3e  %9.3e\n",$1,$2+$4}' > $p_out

#-----------------------------------------------------------------------
#
#  Clean up and exit.
echo "Finished: Wrote emissivity data to" $p_out
exit 0
#
