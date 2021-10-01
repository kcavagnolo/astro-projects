#! /usr/bin/perl -w

use Cwd;
use FindBin qw($Bin);

open(A,"sample_info.dat");
open(B,">cats_query.list");
while(<A>){
    chomp;
    next if (/^\#/);
    next if (/^$/);
    @line=split;
    printf B "%-12s %15s %15s %8s\n",
    $line[0], $line[17], $line[18],"2000";
}
close A;
close B;
exit 0;
