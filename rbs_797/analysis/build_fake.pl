#! /usr/bin/perl -w

$out = "fake_cflow.xcm";
$root = "cflow_fak";
$name = 'RBS_0797';
$exp = "100000";
$obsid = qw(7902);
$runname = 'rcool';
$emin = "0.5";
$emax = "8.0";
$bfnh = "0.0222";
$bftx = "6.59";
$bffe = "0.43";
$bfz = "0.354";
$bfnorm = "6.05e-3";
$bfcr = "0.591";
$cflow = "0.1";
@cfnorm = qw(5 10 20 40 80 160 320);

# initialize
use Cwd;
use FindBin qw($Bin);
use POSIX qw(strftime);

# start writing to file
$rmf = "${name}_${obsid}_${runname}_src1.wrmf";
$arf = "${name}_${obsid}_${runname}_src1.warf";
$bgd = "${name}_${obsid}_${runname}_bgd.pi";
open(A,">${out}");
print A "query no\n";
print A "cosmo 70 0 0.7\n";
print A "set tcl_precision 12\n";
print A "set xs_echo_script 1\n";
print A "chatter 10\n";
print A "weight \"standard\"\n";

# create model
$nhpar = 1;
$txpar = 2;
$fepar = 4;
$zpar  = 5;
$cnpar = 13;
$npars = 13;
foreach $cnorm (@cfnorm) {
    print A "model wabs(mekal+mkcflow) & ${bfnh} & ${bftx} & 1 & ${bffe} & ${bfz} & 0 & ${bfnorm} & ${cflow} & =${txpar} & =${fepar} & =${zpar} & 0 & ${cnorm}\n";
    $fout = $root."_$cnorm";
    print A "fakeit $bgd & $rmf & $arf & y & & $fout & $exp\n";
}

# # perform the fit
# print A "fit 10000 0.01\n";

# # open an output file
# $mytime = strftime("%Y-%m-%d %X", localtime);
# $filler = 'eval puts \$fileid \"-------------------------"'."\n";
# print A 'set fileid [open params.log a]'."\n";
# print A 'eval puts \$fileid \"'."Name: $name\"\n";
# print A 'eval puts \$fileid \"'."Obsid: $obsid\"\n";
# print A 'eval puts \$fileid \"'."Runname: $runname\"\n";
# print A 'eval puts \$fileid \"'."Numann: $numann\"\n";
# print A 'eval puts \$fileid \"'."Fit confidence: $fconf\"\n";
# print A 'eval puts \$fileid \"'."Fit emin-emax: $emin-$emax keV\"\n";
# print A 'eval puts \$fileid \"'."Flux/Lum confidence: $lconf\"\n";
# print A 'eval puts \$fileid \"'."Flux/Lum emin-emax: $lmin-$lmax keV\"\n";
# print A 'eval puts \$fileid \"'."NH: $nhmode\"\n";
# print A 'eval puts \$fileid \"'."redshift: $zmode\"\n";
# print A 'eval puts \$fileid \"'."Created: $mytime\"\n";
# print A $filler;

# # get errors for all free params
# print A 'eval puts \$fileid \"name param bf lo hi"'."\n";
# print A $filler;
# foreach $par (@parameters) {
#     print A "tclout pinfo $par\n";
#     print A 'scan $xspec_tclout "%s" name'."\n";
#     print A "tclout param $par\n";
#     print A 'scan $xspec_tclout "%f" bfpar'."\n";
#     print A "error stopat 100 0.05 maximum 10.0 $fconf $par\n";
#     print A "tclout error $par\n";
#     print A 'scan $xspec_tclout "%f %f" lopar hipar'."\n";
#     print A 'eval puts \$fileid \"$name '."$par".' $bfpar $lopar $hipar"'."\n";
# }
# print A $filler;

# # get stats for fit, do after 'error' since it modifies chisq
# print A "tclout stat\n";
# print A 'scan $xspec_tclout "%f" stat'."\n";
# print A 'eval puts \$fileid \"chisq $stat"'."\n";
# print A "tclout dof\n";
# print A 'scan $xspec_tclout "%f" dof'."\n";
# print A 'eval puts \$fileid \"dof $dof"'."\n";
# print A 'goodness '."$nsims".' sim'."\n";
# print A "tclout goodness\n";
# print A 'scan $xspec_tclout "%f" good'."\n";
# print A 'eval puts \$fileid \"goodness $good of '."$nsims".' simed realizations have a fit statistic < $stat"'."\n";
# print A $filler;

# # make diagonal response
# print A "dummyrsp $lmin $lmax 3000\n";

# # get flux for each annulus
# print A 'eval puts \$fileid \"ergs/cm2/sec"'."\n";
# print A "flux $lmin $lmax $z err 100 $lconf\n";
# for ($i=1; $i <= $numann; $i++) {
#     print A "tclout flux $i\n";
#     print A 'scan $xspec_tclout "%f %f %f" fl fllo flhi'."\n";
#     print A 'eval puts \$fileid \"Flux '."$i".' $fl $fllo $flhi"'."\n";
# }
# print A $filler;

# # get lumin for each annulus
# print A 'eval puts \$fileid \"ergs/sec"'."\n";
# print A "lumin $lmin $lmax $z err 100 $lconf\n";
# for ($i=1; $i <= $numann; $i++) {
#     print A "tclout lumin $i\n";
#     print A 'scan $xspec_tclout "%f %f %f" lum lumlo lumhi'."\n";
#     print A 'eval puts \$fileid \"Lumin '."$i".' $lum $lumlo $lumhi"'."\n";
# }
# print A $filler;

# # get count rate for each annulus
# print A 'eval puts \$fileid \"cts/sec"'."\n";
# for ($i=1; $i <= $numann; $i++) {
#     print A "tclout rate $i\n";
#     print A 'scan $xspec_tclout "%f %f" rate rerr'."\n";
#     print A 'eval puts \$fileid \"Rate '."$i".' $rate $rerr"'."\n";
# }
# print A $filler;

# # close the files
# print A 'close $fileid'."\n";
# print A "exit\n";
# close A;

# print some ingo
$dir = cwd;
print "## STATUS: Now run start xspec11 and type:\n";
print "\@${dir}/${out}\n";

# exit clean
exit 0;
