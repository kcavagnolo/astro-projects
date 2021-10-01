#! /usr/bin/perl -w
# (c) 2007 Chris Waters <watersc1@pa.msu.edu>

use Getopt::Std;
use IPC::Open2;

getopts('s:d:h',\%opt);
unless((exists($opt{s}))&&(exists($opt{d}))) {
    $opt{h} = 1;
}
if (exists($opt{h})) {
    print "Usage:  sqlite_table_cp.pl -s <source.db> -d <destination.db> table1 table2 ...\n";
    exit(10);
}

$pid = open2(\*READ,\*WRITE,'sqlite3');

print WRITE "ATTACH \"$opt{s}\" as SOURCE;\n";
print       "WRITE: ATTACH \"$opt{s}\" as SOURCE;\n";

print WRITE "ATTACH \"$opt{d}\" as DESTINATION;\n";
print "WRITE: ATTACH \"$opt{d}\" as DESTINATION;\n";
foreach $t (@ARGV) {
    print WRITE "CREATE TABLE DESTINATION.${t} AS ";
    print WRITE "SELECT * FROM SOURCE.${t};\n";
    print "WRITE: CREATE TABLE DESTINATION.${t} AS ";
    print "SELECT * FROM SOURCE.${t};\n";
}

print WRITE ".exit\n";
print "WRITE .exit\n";
close(READ);
close(WRITE);
waitpid($pid,0);    
