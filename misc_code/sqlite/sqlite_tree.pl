#! /usr/bin/perl -w
# (c) 2007 Chris Waters <watersc1@pa.msu.edu>

use DBI;
use DBD::SQLite;

%tags = ('NORM'     => '|-- ',
	 'LAST'     => '`-- ',
	 'BLANK'    => '    ',
	 'SKIP'     => '|   ');

$dbname = shift(@ARGV);

$dbh = DBI->connect("dbi:SQLite:dbname=${dbname}","","") ||
    die "Unable to open database $dbname: $DBI::errstr\n";

@tables = $dbh->tables('','','%','');
@tables = grep(!/sql/, @tables);
print "$dbname\n";

for ($ti = 0; $ti <= $#tables; $ti++) {
    $t = $tables[$ti];
    $t =~ s/\"//g;

    $sth = $dbh->prepare(qq{ SELECT * FROM $t });
    $sth->execute();
    $rows = 0;
    $rr = '';
    while ($rr = $sth->fetchrow_arrayref) {
	$rows++;
    }
    $sth->finish();
    
    if ($ti == $#tables) {
	print "$tags{LAST}$t ($rows)\n";
    }
    else {
	print "$tags{NORM}$t ($rows)\n";
    }


    $sth = $dbh->prepare(qq{ SELECT *  FROM $t });
    $sth->execute();
    @cols = @{ $sth->{NAME} };
    @types = @{ $sth->{TYPE} } ;

    for ($ci = 0; $ci <= $#cols; $ci++) {
	$c = $cols[$ci];
	$pad = (50 - length($c));
	$pad = " " x $pad;
	if ($ti == $#tables) {
	    if ($ci == $#cols) {
		print "$tags{BLANK}$tags{LAST}$c$pad$types[$ci]\n";
	    }
	    else {
		print "$tags{BLANK}$tags{NORM}$c$pad$types[$ci]\n";
	    }
	}
	else {
	    if ($ci == $#cols) {
		print "$tags{SKIP}$tags{LAST}$c$pad$types[$ci]\n";
	    }
	    else {
		print "$tags{SKIP}$tags{NORM}$c$pad$types[$ci]\n";
	    }
	}
    }
    $sth->finish();
}

$dbh->disconnect();

