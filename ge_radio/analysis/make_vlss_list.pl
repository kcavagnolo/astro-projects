#! /usr/bin/perl -w
# format:
#12 34 56.78 -00 12 34.5 15 720 My Star

use Cwd;
use FindBin qw($Bin);

open(A,"sample_info.dat");
open(B,">vlss_query.list");
while(<A>){
    chomp;
    next if (/^\#/);
    next if (/^$/);
    @line=split;
    $ra = $line[17];
    $ra =~ s/:/ /g;
    $dec = $line[17];
    $dec =~ s/:/ /g;
    printf B "%-15s %15s %6i %6i %15s\n",
    $ra, $dec, 600, 720, $line[0];
}
close A;
close B;
exit 0;
