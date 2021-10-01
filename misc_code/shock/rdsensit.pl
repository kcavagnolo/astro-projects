#!/usr/local/bin/perl
# The xspec11 script sensit.xcm will make a log file giving source
# count vs kT.  This script extracts the rates.

use strict;
my $lowe;
my $highe;
my $kt;
my $modok;

while (<>) {
    if (/XSPEC>\s*ignore/  && !/XSPEC>\s*ignore\s*bad/) {
	my @pcs = split;
	$pcs[1] =~ /\*\*-(\d*\.?\d*)/ || die "Expecting low energy range\n";
	$lowe = $1;
	$pcs[2] =~ /(\d*\.?\d*)-\*\*/ || die "Expecting high energy range\n";
	$highe = $1;
    } elsif (/XSPEC>\s*model\s*wabs\s*\*\s*mekal/) {
	$modok = 0;
	&chkmod;
    } elsif (/XSPEC>\s*show\s+rates/) {
	&getrate;
    } elsif (/XSPEC>\s*newpar\s+2\s+(\d*\.?\d*)/) {
	$kt = $1;
    }
}

############################################################

sub chkmod {
    while (<>) {
	if (/2\s+2\s+2\s+mekal\s+kT\s+keV\s+(\d*\.?\d*)/) {
	    $modok = 1;
	    $kt = $1;
	    return;
	}
    }
    die "Model not as expected\n";
}

sub getrate {
    while (<>) {
	if (/Model predicted rate :\s+(\d*\.?\d*)/) {
	    printf "%f %f %f %f\n",$lowe,$highe,$kt,$1;
	    return;
	}
    }
}
