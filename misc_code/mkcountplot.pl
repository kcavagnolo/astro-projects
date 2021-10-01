#! /usr/local/bin/perl -w
# USAGE: mkcountplot.pl <source file> <outfile>

$evtfile = $ARGV[0];
$outfile = $ARGV[1];
open (A, ">$outfile");
printf A "# Region file format: DS9 version 4.0\n",
    "global color=green font=\"helvetica 10 normal\" select=1 highlite=1 edit=1 move=1 delete=1 include=1 fixed=0 source\n",
    "fk5\n";
@list = split("\n",$evtfile);
for($i=0;$i<=$#list;$i++) {
    system("fkeypar $list[$i] NAXIS2");
    $naxis2 = `pget fkeypar value`; chop($naxis2);
    
    # Get Parameter
    for($j=1; $j<=$naxis2; $j++) {
	system("ftabpar $list[$i] RA $j");
	$ra = `pget ftabpar value`; chop($ra);
	system("ftabpar $list[$i] DEC $j");
	$dec = `pget ftabpar value`; chop($dec);
	system("ftabpar $list[$i] NET_COUNTS $j");
	$netcounts = `pget ftabpar value`; chop($netcounts);		
	$netcounts = $netcounts + 0.5; 
##	@temp = split(".",$netcounts);
##	print "$j, $x, $y, $netcounts\n";
	### Make event files
	##    $circle="text\\\($ra\\\,$dec\\\,\\\"$netcounts\\\"\\\)";
	  ##  print "echo $circle\n";
###	    system("echo $circle");
##	    system("echo $circle >> $outfile");
	printf (A "text\(%f\,%f\,\"%i\"\)\n", $ra, $dec, $netcounts);
    }
}
exit 0;

