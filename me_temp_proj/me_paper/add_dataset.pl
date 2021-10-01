#! /usr/bin/perl -w

open(A,"junk");
open(B,">out");
while(<A>){
    chomp;
    next if (/^\#/);
    next if (/^$/);
    @line=split("&",$_);
    $obs = $line[1];
    $obs =~ s/^\s+//;
    $obs =~ s/\s+$//;
    $new = " \\dataset [ADS/Sa\.CXO\#obs/0".$obs."] {".$obs."} ";
    $line[1] = $new;
    $out = join('&',@line);
    print B "$out\n";
}
close A;
close B;
exit 0;
