#! /usr/bin/perl -w

use Cwd;
use FindBin qw($Bin);

open(A,"iras09.bbl");
open(B,">short.bbl");
$count = 0;
$check = "n";
$data = "";
while(<A>){
    chomp;
    s/^\s+//;
    s/\s+$//;
    next if (/^\#/);
    next if (/^\{/);
    if (/^$/) {
	printf B "$data\n";
	$check = "n";
	$count++;
	$data = "[$count] ";
	next;
    }
    if (/\\bibitem/) {
	$check = "y";
	next;
    }
    if ($check eq "y") {
	$a = $_;
	$a =~ s/\{//g;
	$a =~ s/\}//g;
	$a =~ s/\~//g;
	$a =~ s/\\newblock//g;
	$data .= $a;
    }
}
close A;
close B;
exit 0;
