#!/usr/local/bin/perl
#
# Makes the model, fixing the link command to include GSL.
# Any argument will cause it to remove the files produced by initpackage
# and hmake, then start the process with initpackage.

use strict;

if ($ARGV[0]) {
    &rmglob ("*.o libclmass.so lpack_clmass.* clmassFunctionMap.*"
	     . " Makefile pkgIndex.tcl");
    my $initcmd = "initpackage clmass model.dat " . $ENV {"PWD"};
    system $initcmd || die "Did you myheainit?  Failed $initcmd\n";
}

my $cmd = "hmake";
open (HMA, "$cmd|") || die "Failed $cmd\n";
my $linkcmd;
while (<HMA>) {
    print;
    if (/^g\+\+ .* -o libclmass.so/) {
	chomp;
	$linkcmd = $_;
    }
}
defined ($linkcmd) || die "*** No link command seen\n";

# Edit this for another system
my $GSLlink = " -lgsl -lgslcblas -L/usr/local/";
$linkcmd .= $GSLlink;
print "$linkcmd\n";
system $linkcmd;

############################################################
# Remove files
sub rmglob {
    my @raw = glob shift;
    # Ensure the entries are unique and that the files exist
    my %uniq;
    foreach my $f (@raw) {
	if (-f $f) {
	    $uniq {$f} = 1;
	}
    }
    my @rmlist = keys %uniq;

    (unlink @rmlist) == @rmlist || die "rmglob: mismatched count: @rmlist\n";
}
