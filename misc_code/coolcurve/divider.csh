#!/bin/csh -xv
nice +10
#-----------------------------------------------------------------------
#  csh divider.csh <lambda_file1.dat> <lambda_file2.dat> <ratio_file.dat>
#
#  Divides emissivity in file 1 by emissivity in file 2, and writes
#  the ratio to file 3, along with the correct temperature axis.
#  Undefined values will be given the value -1.0.
#
#  Dave Strickland: 28.04.04
#  Last modified:   28.04.04
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
    awk '{if($4 <= 0.0){printf"%9.3e  %9.3e\n",$1,-1.0} else {printf"%9.3e  %9.3e\n",$1,$2/$4}}' > $p_out

#-----------------------------------------------------------------------
#
#  Clean up and exit.
echo "Finished: Wrote emissivity data to" $p_out
exit 0
#
