#! /usr/bin/perl -w

open(A,"reflex_obsids");
open(B,">junk");
while(<A>){
    chomp;
    next if (/^\#/);
    next if (/^$/);
    @line=split;
    print B "$line[0], ";
    push @unsorted, $line[0];
}
close A;
close B;
open(A,">sorted");
@sorted = sort { $b <=> $a } @unsorted;
foreach $a (@sorted) {
    print A "$a\t\n";
}
close A;
exit 0;
