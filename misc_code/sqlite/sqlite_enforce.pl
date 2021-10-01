#! /usr/bin/perl -w
# (c) 2007 Chris Waters <watersc1@pa.msu.edu>

use DBI;
use DBD::SQLite;
use Getopt::Std;
use IPC::Open2;

getopts('d:',\%opt);


if (exists($opt{d})) {
    $dbname = $opt{d};
}
else {
    die "No database specified with -d option.\n";
}

# Load the database, and read off the tables.
$dbh = DBI->connect("dbi:SQLite:dbname=${dbname}","","") ||
    die "Unable to open database $dbname: $DBI::errstr\n";

@tables = $dbh->tables('','','%','');
@tables = grep(!/sql/, @tables);

foreach $t (@tables) {
    $tables{$t} = 1;
}
$dbh->disconnect;

$pid = open2(\*READ,\*WRITE,"sqlite3 $opt{d}");


if ($#ARGV == -1) {
    foreach $t (keys(%tables)) {
	print "ENFORCING $t.";
	print WRITE "CREATE TEMP TABLE tmp AS SELECT * FROM ${t};\n";
	print ".";
	print WRITE "DROP TABLE ${t};\n";
	print ".";
	print WRITE "CREATE TABLE ${t} AS SELECT * FROM tmp;\n";
	print ".";
	print WRITE "DROP TABLE tmp;\n";
	print ".\n";
    }
}
else {
    foreach $t (@ARGV) {
	if(exists($tables{$t})) {
	    print "ENFORCING $t.";
	    print WRITE "CREATE TEMP TABLE tmp AS SELECT * FROM ${t};\n";
	    print ".";
	    print WRITE "DROP TABLE ${t};\n";
	    print ".";
	    print WRITE "CREATE TABLE ${t} AS SELECT * FROM tmp;\n";
	    print ".";
	    print WRITE "DROP TABLE tmp;\n";
	    print ".\n";
	}
	else {
	    print "Table $t does not exist in this database.  Skipping...\n";
	}
    }
}
print WRITE "VACUUM;\n";
print WRITE ".exit\n";
close(READ);
close(WRITE);
waitpid($pid,0);

	
