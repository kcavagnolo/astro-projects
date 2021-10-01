#! /usr/bin/perl -w

$myhome = $ENV{'HOME'};
open(A,"junk");
$pname = 'dfsf';
while(<A>){
    chomp;
    next if (/^\#/);
    next if (/^$/);
    @line=split;
    next if ($line[0] eq $pname);
    @a = glob("$myhome/research/pf_clusters/pf_fits/s_results/$line[1]_*");
    @a = glob("$myhome/research/pf_clusters/pf_fits/s_results/*_$line[1]_*") unless (@a);
    @b = glob("$myhome/research/pf_clusters/pf_fits/$line[1]_dof*");
    @b = glob("$myhome/research/pf_clusters/pf_fits/*_$line[1]_dof*") unless (@b);
    $out = $a[0];
    @out = split("/",$out);
    $out = pop @out;
    if (@a && @b) {
	system("paste $a[0] $b[0] > $out");
    }
}
close A;
exit 0;
