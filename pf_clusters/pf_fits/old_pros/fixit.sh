#!/bin/sh

# this script adds the Rin column and sorts the Tx components
# to have the high Tx as Tx1 and the lower Tx as Tx2
# usage: fixit.sh <input filename> <output filename>

awk 'BEGIN{obsid=0.3}{if(!(obsid==$2)) rin="0.00";if(!($1=="#")) print($1"            "$2"  "rin"  "$3"   "$4"  "$5"  "$6"  "$7"  "$8"  "$9"  "$10"  "$11"  "$12"   "$13"  "$14"  "$15"  "$16"   "$17"  "$18"  "$19"   "$20"    "$21);else print($0);obsid=$2;rin=$3}' ${1} > ${2}

echo '#! /usr/bin/perl -w' > temp.pl
echo 'die "Wrong number of command line arguments\n" if (@ARGV != 1);' >> temp.pl
echo 'open(DATAFILE, ">junk.dat");' >> temp.pl
echo 'while(<>){' >> temp.pl
echo '    $line = $_;' >> temp.pl
echo '    @data = split;' >> temp.pl
echo '    if ($data[0] =~ /#/){' >> temp.pl
echo '	print DATAFILE "$line";' >> temp.pl
echo '	next;}' >> temp.pl
echo '    if ($data[7] > $data[14]){' >> temp.pl
echo '	printf DATAFILE "%-18s %6s %6s %6s %5.2f %5.2f %5.2f %5.2f %5.2f %5.2f %5.2f %5.2f %5.2f %10.2e %5.2f %5.2f %5.2f %10.2e %7.4f %6.3f %6.2f %6i\n",$data[0],$data[1],$data[2],$data[3],$data[4],$data[5],$data[6],$data[7],$data[8],$data[9],$data[10],$data[11],$data[12],$data[13],$data[14],$data[15],$data[16],$data[17],$data[18],$data[19],$data[20],$data[21],$data[22];}' >> temp.pl
echo '    if ($data[7] < $data[14]){' >> temp.pl
echo '	$thot=$data[14];' >> temp.pl
echo '	$thothi=$data[15];' >> temp.pl
echo '	$thotlo=$data[16];' >> temp.pl
echo '	$thotnorm=$data[17];' >> temp.pl
echo '	$tcool=$data[7];' >> temp.pl
echo '	$tcoolhi=$data[8];' >> temp.pl
echo '	$tcoollo=$data[9];' >> temp.pl
echo '	$tcoolnorm=$data[13];' >> temp.pl
echo '	$data[14]=$tcool;' >> temp.pl
echo '	$data[15]=$tcoolhi;' >> temp.pl
echo '	$data[16]=$tcoollo;' >> temp.pl
echo '	$data[17]=$tcoolnorm;' >> temp.pl
echo '	$data[7]=$thot;' >> temp.pl
echo '	$data[8]=$thothi;' >> temp.pl
echo '	$data[9]=$thotlo;' >> temp.pl
echo '	$data[13]=$thotnorm;' >> temp.pl
echo '	printf DATAFILE "%-18s %6s %6s %6s %5.2f %5.2f %5.2f %5.2f %5.2f %5.2f %5.2f %5.2f %5.2f %10.2e %5.2f %5.2f %5.2f %10.2e %7.4f %6.3f %6.2f %6i\n",$data[0],$data[1],$data[2],$data[3],$data[4],$data[5],$data[6],$data[7],$data[8],$data[9],$data[10],$data[11],$data[12],$data[13],$data[14],$data[15],$data[16],$data[17],$data[18],$data[19],$data[20],$data[21],$data[22];' >> temp.pl
echo '    }' >> temp.pl
echo '}' >> temp.pl
echo 'close DATAFILE;' >> temp.pl

chmod +x temp.pl
#rehash
perl temp.pl ${2}
rm -f temp.pl

sed 's/       ObsID/ObsID/;s/Rout/ Rin   Rout/' junk.dat > junk2.dat
rm -f junk.dat

mv -f junk2.dat ${2}
