#! /usr/bin/perl -w
# (c) 2007 Chris Waters <watersc1@pa.msu.edu>

use IPC::Open3;
use IO::Handle;

sub gen_plot_spec {
    my $db = shift(@_);
    my $query = shift(@_);
    $query =~ s/sselect/select/g;
    my $spec = '"< sqlite3 ' . "$db '$query'\"";
#    print STDERR "DEBUG:: $db -> $query -> $spec\n";
    return($spec);
}

#$gp_pid = open3(\*GP_IN,\*GP_OUT,\*GP_ERR,
#		"gnuplot");
open(GP_IN,"|gnuplot");
GP_IN->autoflush(1);

$db = shift(@ARGV);

print GP_IN "set term x11\n";

while (1) {
    print "sqlplot> ";
    chomp($input = <STDIN>);
    if ($input eq 'quit') {
	exit(0);
    }
    elsif ($input eq 'help') {
	print "Commands:\n";
	print " quit          : exits sqlite_plot.pl\n";
	print " help          : this help.\n";
	print " select        : plot data from a select statement\n";
	print " sselect       : splot data from a select statement\n";
	print " <GP_CMD>      : pass through gnuplot command.\n";
    }
    elsif ($input =~ /^sselect/) {
	$query = $input;
	$spec = gen_plot_spec($db,$query);
	$cmd = "splot $spec title \"$query\"\n";
#	print STDERR "AA: $cmd\n";
	print GP_IN $cmd;
    }
    elsif ($input =~ /^select/) {
	$query = $input;
	$spec = gen_plot_spec($db,$query);
	$cmd = "plot $spec  title \"$query\"\n";
#	print STDERR "AA: $cmd\n";
	print GP_IN $cmd;
    }
    else {
	print GP_IN "$input\n";
    }
}
	
