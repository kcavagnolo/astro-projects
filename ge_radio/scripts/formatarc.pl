#! /usr/bin/perl -w

use Cwd;
use FindBin qw($Bin);

open(A,"arcobs");
open(B,">vla_obs");
printf B "%-30s %10s %10s %5s %25s %25s %15s %15s %15s %15s %10s\n",
    "#ArcFile","Status","ProjID","Seg","ProjStart","ProjStop","Size(kB)","Tele:arr:sub","Bands","Format","Type";
while(<A>){
    chomp;
    next if (/^\#/);
    next if (/^$/);
    s/^\s+//;
    s/\s+$//;
    @a=split(/\t/);
    printf B "%-30s %10s %10s %5s %25s %25s %15s %15s %15s %15s %10s\n",
    $a[0],$a[1],$a[2],$a[3],$a[4],$a[5],$a[6],$a[7],$a[8],$a[9],$a[10];
}
close A;
close B;
exit 0;
