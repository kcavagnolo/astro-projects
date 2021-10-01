#! /usr/bin/perl -w

use Cwd;
use FindBin qw($Bin);

open(A,"ned_batch_output.dat");
open(B,">out");
print B "RA\tDEC\n";
$new = "y";
while(<A>){
    chomp;
    next if (/^\#/);
    next if (/^$/);
    s/^\s+//;
    s/\s+$//;
    if (/^POSITION/) {
	print B "$ra\t$dec\n" unless ($new eq "y");
	$new = "y";
#	@line = split;
#	$name = $line[1].'_'.$line[2];
    }
    if (/^equatorial\s+J2000\.0/ && $new eq "y") {
	$new = "n";
	@line = split(/\: /);
	@line = split(/\s+/,$line[1]);
	$ra = $line[0];
	$ra =~ s/\s+//;
	$ra =~ s/h/ /;
	$ra =~ s/m/ /;
	$ra =~ s/s//;
	$dec = $line[1];
	$dec =~ s/\s+//;
	$dec =~ s/d/ /;
	$dec =~ s/m/ /;
	$dec =~ s/s//;
    }
}
close A;
close B;
exit 0;
