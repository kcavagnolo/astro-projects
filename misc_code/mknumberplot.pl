#! /usr/local/bin/perl -w
print "USAGE: mkcountplot.pl <source file> <outfile>\n";

$evtfile = $ARGV[0];

### Background
$outfile = $ARGV[1];

$test="\\\#\ format\\\:\ \ \ \ \ \ \ degrees \\\(ICRS\\\)";
##print "echo $test\n";
system("echo $test > $outfile");
open (outfile, ">>$outfile");

#print $file;

$ftoolsdir = '/bulk/pkg/dftools/SunOS_5.5_sparc/bin';

@list = split("\n",$evtfile);

### For each files
for($i=0;$i<=$#list;$i++)
{
    system("fkeypar $list[$i] NAXIS2");
    $naxis2 = `$ftoolsdir/pget fkeypar value`; chop($naxis2);
    
    ### Get Parameter
    for($j=1;$j<=$naxis2;$j++)
    {
	system("ftabpar $list[$i] RA $j");
	$ra = `$ftoolsdir/pget ftabpar value`; chop($ra);

	system("ftabpar $list[$i] DEC $j");
	$dec = `$ftoolsdir/pget ftabpar value`; chop($dec);

##	system("ftabpar $list[$i] NET_COUNTS $j");
##	$netcounts = `$ftoolsdir/pget ftabpar value`; chop($netcounts);		
##	$netcounts = $netcounts + 0.5; 
##	@temp = split(".",$netcounts);

##	print "$j, $x, $y, $netcounts\n";
	### Make event files

	$k = $j*1;
##	print $k, "\n";
	##    $circle="text\\\($ra\\\,$dec\\\,\\\"$netcounts\\\"\\\)";
	  ##  print "echo $circle\n";
###	    system("echo $circle");
##	    system("echo $circle >> $outfile");
	printf (outfile "text\(%f\,%f\,\"%i\"\)\n", $ra, $dec, $j);

    }

    
}
