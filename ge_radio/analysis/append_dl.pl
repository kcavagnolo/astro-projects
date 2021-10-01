#! /usr/bin/perl -w

use Cwd;
use FindBin qw($Bin);

open(A,"ge_distances.dat");
while(<A>){
    chomp;
    next if (/^\#/);
    next if (/^$/);
    @line=split;
    $a{$line[0]}=$line[1];
}
close A;

open(B,"wenss_cats.dat");
open(C,">pjn_dl_cats.dat");
while(<B>) {
    chomp;
    if (/^\#/) {
	print C "$_\n";
	next;
    }
    next if (/^$/);
    @line=split;    
    if (exists $a{$line[0]}){
	print C "$_   $a{$line[0]}\n";
    } else {
        print C "$_   -999\n";
    }
}
close B;
close C;
exit 0;
