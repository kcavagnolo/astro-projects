#! /usr/bin/perl -w

open(A,"sample.tex");
open(B,">sample_dataset.tex");
while(<A>){
    chomp;
    if (/^\\/) {
	print B "$_\n";
	next;
    }
    @line=split("&",$_);
    $obs = $line[1];
    $obs =~ s/^\s+//;
    $obs =~ s/\s+$//;
    $new = " \\dataset[ADS/Sa\.CXO\#obs/".$obs."]{".$obs."} ";
    $line[1] = $new;
    unless (/^\s+\&/) {
	$obj = $line[0];
	$obj =~ s/^\s+//;
	$obj =~ s/\s+$//;
	$new = " \\object{".$obj."} ";
	$line[0] = $new;
    }
    $out = join('&',@line);
    print B "$out\n";
}
close A;
close B;
exit 0;
