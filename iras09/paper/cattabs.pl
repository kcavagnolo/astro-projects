#! /usr/bin/perl -w

use Cwd;
use FindBin qw($Bin);

@tables = qw(ellcavities.tex cylcavities.tex);

open(OUT,">cavities.tex");
print OUT '\begin{landscape}'."\n";
open(A,"$tables[0]");
while(<A>){
    chomp;
    next if (/^\#/);
    next if (/^$/);
    print OUT "$_\n";
    last if ($_ =~ /startdata/);
}
close A;

$data = "no";
foreach $a (@tables) {
    open(A,"$a");
    while(<A>){
	chomp;
	if ($_ =~ /enddata/) {
	    $data = "no";
	    last;
	}
	if ($data eq "yes") {
	    $b = $_;
	    $b =~ s/\s+//;
	    push @line, $b; 
	    if ($b =~ /\\\\/) {
		$outline = join(" ",@line);
		print OUT "$outline\n";
		undef @line;
	    }
	}
	$data = "yes" if ($_ =~ /startdata/);
    }
    close A;
}
print OUT '\enddata'."\n";
print OUT '\end{deluxetable}'."\n";
print OUT '\clearpage'."\n";
print OUT '\end{landscape}'."\n";
close OUT;
exit 0;
