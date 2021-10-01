#! /usr/bin/perl -w

open(A,">out");
printf A "%-25s %8s %8s %8s %8s %8s %8s %12s\n","#Name","ObsID","l","b","ExpT","Mode","Inst","PI";
while(<>){
    chomp;
    @a=split;
    next if (/^\#/);
    printf A "%-25s %8s %8.3f %8.3f %8.2f %8s %8s %12s\n",
    $a[6],$a[1],$a[8],$a[9],$a[5],$a[11],$a[2],$a[7];
}
close A;
