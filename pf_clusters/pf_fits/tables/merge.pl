#! /usr/bin/perl -w

system("rm -f all_results.table");
open(A,"/home/cavagnolo/research/pf_clusters/pf_info/rafferty.list");
open(D,">bg.dat");
open(E,">nonbg.dat");
$bgnum = 0;
$nbgnum = 0;
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
    while(<B>){
	chomp;
        next if (/^\#/); # skip comment lines
        next if (/^$/);  # skip blank lines
	@b = split;
	push @obs, $b[1] if ($b[0] eq $name);
    }
    close B;
    if (@obs) {
	$obsid = join("_",@obs);
	$file = "${obsid}_table.dat";
	open(C,$file);
	while(<C>){
	    chomp;
	    next if (/^Source/);
	    next if (/^Cosmo/);
	    next if (/^Number/);
	    next if (/^\//);
	    $_ .= " : Halpha = $ha" if (/^\w/);
	    if ($bg1 > 0.0 || $bg2 > 0.0) {
		$bgnum = sprintf("%-7i",$bgnum+1.0);
		$bgnline = $bgnum."$_\n";
		print D "$bgnline";
	    } else {
		$nbgnum = sprintf("%-7i",$nbgnum+1.0);
		$nbgnline = $nbgnum."$_\n";
		print E "$nbgnline";
	    }
	}
	close C;
	if ($bg1 > 0.0 || $bg2 > 0.0) {
	    print D "\n";
	} else {
	    print E "\n";
	}
    } else {
	print "No $name\n";
    }
}
close A;
close D;
close E;
exit 0;
