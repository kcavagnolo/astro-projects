#! /usr/bin/perl -w

use Cwd;
use FindBin qw($Bin);

open(A,"mrk231_ned.dat");
open(OUT, ">mrk231_sed.dat");
printf OUT "%-25s %15s %15s %15s %10s %10s\n","#ID","Freq","Flux","Ferr","Type","Line";
printf OUT "%-25s %15s %15s %15s %10s %10s\n","#--","Hz","Jy","Jy","--","--";
while(<A>){
    chomp;
    next if (/^\#/);
    next if (/^$/);
    @line=split(/\t/);
    $line[6] = $line[7] if ($line[6] eq "");
    $line[7] = 0.0 if ($line[7] eq "");
    printf OUT "%-25s %15.4e %15.4e %15.4e %10s %10s\n",$line[1], $line[5], $line[6], $line[7], "cont", "none";
}
close A;
close OUT;
exit 0;
