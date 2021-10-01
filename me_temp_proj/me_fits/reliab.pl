#! /usr/bin/perl -w

$cut = 0.0;
open(A,"fak_ccncc.log");
while(<A>){
    chomp;
    next if (/^\#/);
    next if (/^$/);
    @line = split;
    $ncc++ if (($line[2] eq "NCC") && ($line[5] >= $cut));
    $cc++ if (($line[2] eq "CC") && ($line[5] >= $cut));
}
print "NCC w/ prob. >= ${cut}: $ncc\n";
print "CC w/ prob. >= ${cut}: $cc\n";
close A;
exit 0;
