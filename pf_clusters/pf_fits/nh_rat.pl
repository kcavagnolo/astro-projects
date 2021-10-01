#! /usr/bin/perl -w

use Cwd;
use FindBin qw($Bin);

open(A,"done.list");
while(<A>){
    chomp;
    next if (/^\#/);
    next if (/^$/);
    @line=split;
    $ref{$line[1]} = $line[7]*(10**20);
}
close A;

open(A,"nh_lab.dat");
open(B,">out");
while(<A>){
    chomp;
    next if (/^\#/);
    next if (/^$/);
    @line = split;
    unless (exists $ref{$line[1]}) {
	print "Missing $line[1]\n";
	next;
    }
    $onh = sprintf("%.2e",$ref{$line[1]});
    $nnh = sprintf("%.2e",$line[4]);
    $rat = sprintf("%.2f",$onh/$nnh);
    print B "$_"."   $onh   $rat\n";
}
close A;
close B;
exit 0;
