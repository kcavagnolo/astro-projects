#! /usr/bin/perl -w

open(A,"filter.list");
while(<A>){
    chomp;
    next if (/^\#/);
    @line=split;
    $info{$line[1]}=$line[1];
}
close A;

open(B,"../me_paper/full_sample.list");
open(LOG,">log");
while(<B>){
    chomp;
    @line=split;
    if (exists $info{$line[1]}) {
	print LOG "$_\n";
    }
}
close B;

open(B,"../info/filter.list");
$j=0;
while(<B>){
    chomp;
    next if (/^\#/);
    @line=split;
    $info{$line[1]}=$line[1];
    $j++;
}
close B;

open(A,"full_sample.list");
$i=0;
while(<A>){
    chomp;
    next if (/^\#/);
    @line=split;
    @dec = split ":",$line[18];
    if (($line[6] >= 0.15) && ($line[6] <= 0.3) && ($line[8] >= 6.5) && ($dec[0] >= -20)) {
        if (!exists $info{$line[1]}){
            print "$line[0] : $line[1] is found to fit filter crit in full_sample.list, but did not in filter.pro\n";
            $i++;
        }
    }
}
close A;
print "$i clusters found in full_sample\n";
