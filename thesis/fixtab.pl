#! /usr/bin/perl -w
# script to remove dataset tags
# 1E0657 56 & \dataset [ADS/Sa.CXO\#obs/03184] {3184} & 06:58:29.622 & -55:56:39.79 & 87.5 & VF & I3 & 0.296 & 52.48\\

open(A,"eband_tables.tex");
open(B,">out.tex");
while(<A>){
    chomp;
    $line = $_;
    if ($line =~ /\\dataset/) {
	$line =~ s/\\dataset.*\{//;
	$line =~ s/\}//;
	print B "$line\n";
    } else {
	print B "$line\n";
    }
}
close A;
close B;
exit 0;
