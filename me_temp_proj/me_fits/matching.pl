#! /usr/bin/perl -w

open(A,"dat/matching_chandra_r2500-50_77.dat");
while(<A>){
    chomp;
    next if (/^\#/);
    @line=split;
    $ref{$line[1]}=$line[1];
}

open(B,"$ARGV[0]");
open(LOG,">out");
while(<B>){
    chomp;
    if (/^\#/){
	print LOG "$_\n";
	next;
    }
    @line=split;
    if (exists $ref{$line[1]}){
	print LOG "$_\n";
    }
}
