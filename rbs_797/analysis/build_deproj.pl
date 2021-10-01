#! /usr/bin/perl -w

$geterrs = "no";
$nsims = 1000;
$fconf = 2.71;
$lconf = 90;
$name = "RBS_0797";
$obsid = "7902";
$runname = "annuli";
$emin = "0.7";
$emax = "7.0";
$lmin = "0.001";
$lmax = "100.0";
$nh = "0.022";
$tx = "7.44";
$fe = "0.30";
$z  = "0.354";

# initialize
use Cwd;
use FindBin qw($Bin);
use POSIX qw(strftime);

# read options
if (@ARGV != 6) {
    print "\n";
    print "Usage:\n";
    print "[prompt]% build_deproj <num_annuli> <cool_annuli> <nhpar> <zpar> <model> <output_file>\n";
    print "num_annuli:  number of annuli to use in fitting\n";
    print "cool_annuli: number of annuli to have cflow comp; only used if model is mekal+mkclfow\n";
    print "nhpar:       shall NH be free or frozen\n";
    print "zpar:        shall redshift be free or frozen\n";
    print "models:      mekal or mekal+mkclfow\n";
    print "output_file: name of output XCM file to be run in Xspec\n";
    print "\n";
    die "## ERROR: Wrong number of command line arguments\n";
}
$numann  = $ARGV[0];
$coolann = $ARGV[1];
$nhmode  = $ARGV[2];
$zmode   = $ARGV[3];
$model   = $ARGV[4];
$out     = $ARGV[5];

# throw errors
die "## ERROR: Number of total annuli must be >= 2.\n" if ($numann <= 1);
die "## ERROR: Number of mkcflow annuli must be >= 1.\n" if ($coolann <= 0 && $model =~ "cflow");
die "## ERROR: Please enter a model to use.\n" unless ($model eq "mekal+mkcflow" || $model eq "mekal");

# general options
if ($nhmode eq "frozen") {
    $nhv = ",0";
} else {
    $nhv = "";
}
if ($zmode ne "frozen") {
    $zv = ",1";
} else {
    $zv = "";
}

# start writing to file
open(A,">${out}");
print A "query no\n";
print A "cosmo 70 0 0.7\n";

# load the data
for ($i=1; $i <= $numann; $i++) {
    print A "data ${i}:${i} ${name}_${obsid}_${runname}${i}_src1_grp.pi\n";
}
print A
    "ignore bad\n"
    ."ignore **-${emin} ${emax}-**\n"
    ."set tcl_precision 12\n"
    ."set xs_echo_script 1\n"
    ."chatter 10\n"
    ."weight \"standard\"\n"
    ."model wabs*($model)\n";

if ($model eq "mekal+mkcflow") {
    $nhpar = 1;
    $txpar = 2;
    $fepar = 4;
    $zpar  = 5;
#    $cnpar = 13;
    $npars = 13;
    # the first model comp
    print A "${nh}${nhv} & ${tx} & 1 & ${fe},1 & ${z}${zv} & 0 & 1 & 0.1,0 & =${txpar} & =${fepar} & =${zpar} & 0 & 1\n";
    # the annuli with cflow needed
    for ($i=2; $i <= $coolann; $i++) {
	$ctxpar = $txpar+(($i-1)*$npars);
	$cfepar = $fepar+(($i-1)*$npars);
#	$cnormpar = $cnpar+(($i-1)*$npars);
	print A "=${nhpar} & ${tx} & 1 & ${fe},1 & =${zpar} & 0 & 1 & 0.1,0 & =${ctxpar} & =${cfepar} & =${zpar} & 0 & 1\n";
    }
    # the annuli not requiring cflow
    for ($i=$coolann+1; $i <= $numann; $i++) {
	print A "=${nhpar} & ${tx} & 1 & ${fe},1 & =${zpar} & 0 & 1 & 0,0 & 0,0 & 0,0 & ${z} & 0 & 0,0\n";
    }
} elsif ($model eq "mekal") {
    $nomnhpar = 1;
    $nomzpar  = 5;
    $nhpar = 4;
    $zpar  = 7;
    $txpar = 5;
    $fepar = 7;
    $nmpar = 10;
    $npars = 10; # after addition of 3 pars for projct

    # the first model comp
    print A "${nh}${nhv} & ${tx} & 1 & ${fe},1 & ${z}${zv} & 0 & 1\n";

    # add nh if left free
    push @parameters, $nhpar unless ($nhmode eq "frozen");
    push @parameters, $zpar unless ($zmode eq "frozen");
    push @parameters, $txpar;
    push @parameters, $fepar;
    push @parameters, $nmpar;

    # the other annuli
    for ($i=2; $i <= $numann; $i++) {
	print A "=${nomnhpar} & ${tx} & 1 & ${fe},1 & =${nomzpar} & 0 & 1\n";
	$txpar = $txpar+$npars;
	$fepar = $fepar+$npars;
	$nmpar = $nmpar+$npars;
	push @parameters, $txpar;
	push @parameters, $fepar;
	push @parameters, $nmpar;
    }
} else {
    die "## ERROR: You gave me a bad model -- $model\n";
}

# perform the fit
print A "fit 1000\n";

# add the projct comp
$pars = "0.0";
for ($i=1; $i <= $numann*3; $i++) {
    $pars .= " & 0.0";
}
print A "addcomp 1 projct $pars\n";

# perform the fit
print A "fit 10000 0.01\n";

# open an output file
$mytime = strftime("%Y-%m-%d %X", localtime);
$filler = 'eval puts \$fileid \"-------------------------"'."\n";
print A 'set fileid [open params.log a]'."\n";
print A 'eval puts \$fileid \"'."Name: $name\"\n";
print A 'eval puts \$fileid \"'."Obsid: $obsid\"\n";
print A 'eval puts \$fileid \"'."Runname: $runname\"\n";
print A 'eval puts \$fileid \"'."Numann: $numann\"\n";
print A 'eval puts \$fileid \"'."Fit confidence: $fconf\"\n";
print A 'eval puts \$fileid \"'."Fit emin-emax: $emin-$emax keV\"\n";
print A 'eval puts \$fileid \"'."Flux/Lum confidence: $lconf\"\n";
print A 'eval puts \$fileid \"'."Flux/Lum emin-emax: $lmin-$lmax keV\"\n";
print A 'eval puts \$fileid \"'."NH: $nhmode\"\n";
print A 'eval puts \$fileid \"'."redshift: $zmode\"\n";
print A 'eval puts \$fileid \"'."Created: $mytime\"\n";
print A $filler;

# write rin and rout for annuli
print A "set rin 0.0\n";
print A 'eval puts \$fileid \"rin rout"'."\n";
print A 'eval puts \$fileid \"arcmin"'."\n";
print A $filler;
for ($i=1; $i <= $numann; $i++) {
    print A "tclout xflt $i\n";
    print A 'scan $xspec_tclout "%f %f" a rout'."\n";
    print A 'eval puts \$fileid \"Radius '."$i".' $rin $rout"'."\n";
    print A 'set rin $rout'."\n";
}
print A $filler;

# get errors for all free params
if ($geterrs eq "yes") {
    print A 'eval puts \$fileid \"name param bf lo hi"'."\n";
    print A $filler;
    foreach $par (@parameters) {
	print A "tclout pinfo $par\n";
	print A 'scan $xspec_tclout "%s" name'."\n";
	print A "tclout param $par\n";
	print A 'scan $xspec_tclout "%f" bfpar'."\n";
	print A "error stopat 100 0.05 maximum 10.0 $fconf $par\n";
	print A "tclout error $par\n";
	print A 'scan $xspec_tclout "%f %f" lopar hipar'."\n";
	print A 'eval puts \$fileid \"$name '."$par".' $bfpar $lopar $hipar"'."\n";
    }
    print A $filler;
}

# get stats for fit, do after 'error' since it modifies chisq
print A "tclout stat\n";
print A 'scan $xspec_tclout "%f" stat'."\n";
print A 'eval puts \$fileid \"chisq $stat"'."\n";
print A "tclout dof\n";
print A 'scan $xspec_tclout "%f" dof'."\n";
print A 'eval puts \$fileid \"dof $dof"'."\n";
print A 'goodness '."$nsims".' sim'."\n";
print A "tclout goodness\n";
print A 'scan $xspec_tclout "%f" good'."\n";
print A 'eval puts \$fileid \"goodness $good of '."$nsims".' simed realizations have a fit statistic < $stat"'."\n";
print A $filler;

# make diagonal response
print A "dummyrsp $lmin $lmax 3000\n";

# get flux for each annulus
print A 'eval puts \$fileid \"ergs/cm2/sec"'."\n";
print A "flux $lmin $lmax $z err 100 $lconf\n";
for ($i=1; $i <= $numann; $i++) {
    print A "tclout flux $i\n";
    print A 'scan $xspec_tclout "%f %f %f" fl fllo flhi'."\n";
    print A 'eval puts \$fileid \"Flux '."$i".' $fl $fllo $flhi"'."\n";
}
print A $filler;

# get lumin for each annulus
print A 'eval puts \$fileid \"ergs/sec"'."\n";
print A "lumin $lmin $lmax $z err 100 $lconf\n";
for ($i=1; $i <= $numann; $i++) {
    print A "tclout lumin $i\n";
    print A 'scan $xspec_tclout "%f %f %f" lum lumlo lumhi'."\n";
    print A 'eval puts \$fileid \"Lumin '."$i".' $lum $lumlo $lumhi"'."\n";
}
print A $filler;

# get count rate for each annulus
print A 'eval puts \$fileid \"cts/sec"'."\n";
for ($i=1; $i <= $numann; $i++) {
    print A "tclout rate $i\n";
    print A 'scan $xspec_tclout "%f %f" rate rerr'."\n";
    print A 'eval puts \$fileid \"Rate '."$i".' $rate $rerr"'."\n";
}
print A $filler;

# close the files
print A 'close $fileid'."\n";
print A "exit\n";
close A;

# print some ingo
$dir = cwd;
print "## STATUS: Now run start xspec11 and type:\n";
print "\@${dir}/${out}\n";

# exit clean
exit 0;
