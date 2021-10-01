#!/bin/csh -xv
nice +10
#-----------------------------------------------------------------------
#  csh multipler.csh <lambda_file1.dat> <number> <out_file.dat>
#
#  Multiplies emissivity in file 1 by <number>, and writes
#  the ratio to out_file.dat, along with the correct temperature axis.
#  Undefined values will be given the value -1.0.
#
#  Dave Strickland: 30.08.05
#  Last modified:   30.08.05
#
#-----------------------------------------------------------------------
#
#  Command line options:

set p_inp1=$1
if ( ! -e $p_inp1 ) then
  echo "  Error: Input file" $p_inp1 "does not exist."
  exit 1
endif

set p_num=$2

set p_out=$3

#-----------------------------------------------------------------------
#
#  Main work area:

#  Check that the number of lines in each of the input files are the same.
set pnl1=`wc -l $p_inp1 | awk '{print $1}'`

if ( -e $p_out ) rm $p_out
touch $p_out
set i=1
while ( $i <= $pnl1 )
  set p_line=`head -$i $p_inp1 | tail -1`
  set p_logt=`echo $p_line | awk '{print $1}'`
  set p_nulam=`echo $p_num $p_line | awk '{print $1*$3}'`
  echo $p_logt $p_nulam | awk '{printf"%9.3e  %9.3e\n",$1,$2}' >> $p_out
  @ i++
end

#-----------------------------------------------------------------------
#
#  Clean up and exit.
echo "Finished: Wrote emissivity data to" $p_out
exit 0
#
