#! /usr/bin/perl -w
@tables = qw(insmall.tex insmall2.tex insmall4.tex huge.tex huge2.tex outer.tex);
use Cwd;
use FindBin qw($Bin);
open(OUT,">cavities.tex");
print OUT 'PS & W & ';
foreach $tab (@tables) {
    open(A,"$tab");
    while(<A>){
	chomp;
	next if (/^\#/);
	next if (/^$/);
	$b = $_;
	$b =~ s/\s+//;
	$b =~ s/\n//;
	$b =~ s/\&/ \& /;
	$b =~ s/\\pm/ \\pm /;
	$b =~ s/\\\\/\\\\\nPS & W \& /;
	print OUT "$b";
    }
    close A;
}
close OUT;
exit 0;
