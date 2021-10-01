#! /usr/bin/perl -w

system("rm -f all_results.table");
open(A,"/home/cavagnolo/research/pf_clusters/pf_info/rafferty.list");
open(C,">sfha.list");
while(<A>){
    chomp;
    next if (/^\#/); # skip comment lines
    next if (/^$/);  # skip blank lines
    @a = split;
    $name = $a[0];
    $ha = $a[15];
    $bg1 = $a[17]-$a[18];
    $bg2 = $a[19]-$a[20];
    open(B,"/home/cavagnolo/research/pf_clusters/pf_info/done.list");
    undef @obs;
    undef @lines;
    while(<B>){
	chomp;
        next if (/^\#/); # skip comment lines
        next if (/^$/);  # skip blank lines
	@b = split;
	push @obs, $b[1] if ($b[0] eq $name);
	push @lines, $_ if ($b[0] eq $name);
    }
    close B;
    if (@obs) {
	next if ($ha eq "UK");
	if (($bg1 > 0.0 || $bg2 > 0.0) && $ha eq "Y") {
	    $add = "SFHA\n";
	} elsif ($ha eq "Y") {
	    $add = "HAON\n";
	} else {
	    $add = "NONE\n";
	}
	foreach $line (@lines) {
	    printf C "$line"."    $add";
	}
    }
}
close A;
close C;
exit 0;
