#! /usr/bin/perl -w
$oldcluster = "";
while(<>){
    chomp;
    next if (/^\#/);
    @line=split;
    $newcluster = $line[0];
    if (($line[2] >= 5000) || ($oldcluster eq $newcluster)) {
	if ($oldcluster eq $newcluster) {
	    $ref{$oldline} = $oldcts;
	}
	$ref{$line[1]} = $line[2];
    }
    $oldcluster = $newcluster;
    $oldline = $line[1];
    $oldcts = $line[2];
}
open(OUT,">out");
open(A,"../me_info/ref_loc.list");
while(<A>){
    chomp;
    next if (/^\#/);
    @line=split;
    if (exists $ref{$line[1]}) {
	print OUT "$_\n";
    }
}
exit 0;
