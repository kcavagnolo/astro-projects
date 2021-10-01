#! /usr/bin/perl -w

open(A,"out");
open(B,">bax");
while(<A>){
    chomp;
    next if (/^\#/);
    next if (/^$/);
    @line=split;
    $ra=$line[17];
    $dec=$line[18];
    @ra=split(/:/,$ra);
    @dec=split(/:/,$dec);
    $ra[0].='h';
    $ra[1].='mn';
    $ra[2]=~s/\..*//;
    $ra[2].='s';
    $dec[0].='d';
    $dec[1].='mn';
    $dec[2]=~s/\..*//;
    $dec[2].='s';
    $ra = join("",@ra);
    $dec = join("",@dec);
    print B "$line[0] $ra $dec\n";
}
close A;
close B;
exit 0;
