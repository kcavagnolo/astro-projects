#! /usr/bin/perl -w

open(A,"$ARGV[0]");
while(<A>){
    chomp;
    next if (/^\#/);
    next if (/^$/);
    @line=split;
    $a{$line[0]}=$_;
}
close A;

open(A,"$ARGV[1]");
open(B,">out");
while(<A>){
    chomp;
    @line=split("--");
    $name = $line[1]."_".$line[2];
    print B "$_\n" if (exists $a{$name});
}
close A;
close B;
exit 0;
