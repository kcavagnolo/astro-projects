#!/usr/bin/perl -w

# This script takes the XSPEC fit results .dat file
# and makes a latex table; to use this script:
# perl table_kfit.pl ../pf_fits/s_results/all_results.log kfit.tex

# useful settings for this table
$pvalfile = "$ENV{'HOME'}/research/pf_clusters/pf_fits/dat/pval.info";
$pholder = '-';
$complete = "no";
@items = ("cluster","mode","ann","rmax","k0","k0sig","k100","plaw","dof","chisq","prob");
$tabcomments = 'Col. (1) Cluster name; '
    .'col. (2) CDA observation identification number; '
    .'col. (3) method of $T_X$ interpolation (discussed in \S\ref{sec:kpr}); '
#    .'col. (4) minimum radius for fit; '
    .'col. (4) maximum radius for fit; '
    .'col. (5) number of radial bins included in fit; '
    .'col. (6) best-fit core entropy; '
    .'col. (7) number of sigma \\kna\\ is away from zero; '
    .'col. (9) best-fit entropy at 100 kpc; '
    .'col. (10) best-fit power-law index; '
    .'col. (11) degrees of freedom in fit; '
    .'col. (12) \chisq\ statistic of best-fit model; '
    .'and col. (13) probability of worse fit given \chisq\ and degrees of freedom.';
$caption = "Summary of Entropy Profile Fits" . '\label{tab:kfits}';
@tabcolumns = ('l');
foreach (@items) {
    push @tabcolumns, "c";
}
pop @tabcolumns;
$tabcolumns = join("",@tabcolumns);

# read in p-values
open(PVAL,$pvalfile) || die "Can't open $pvalfile for reading\n";
while(<PVAL>){
    chomp;
    next if (/^\#/);
    next if (/^$/);
    @line = split;
    $pinfo{$line[0]} = "$_";
}
close PVAL;

# read in file
open(INFILE,$ARGV[0]) || die "Can't open $ARGV[0] for reading\n";
@lines = <INFILE>;

# open latex file
open(OUTFILE,">$ARGV[1]") || die "Can't open $ARGV[1] for writing\n";

# print header information
print_header();

$oldcluster = 'fdskjfhskdjfh';
$iter = 0;
foreach $line (@lines) {

  # check for blank lines and comments
  next if ($line =~ /^\#/);
  next if ($line =~ /^$/);
  ($cluster,$obsid,$mode,$rmin,$rmax,$ann,
   $k0,$k0err,$k100,$k100err,$plaw,$plawerr,
   $chisq,$prob,$dof) = split(" ",$line);
  if ($k0err > 0.0) {
      $k0sig = $k0/$k0err;
  } else {
      $k0sig = 0.0;
  }

  # get pvalues
  $pdata = $pinfo{$cluster} || die "## ERROR: no external p-value info for $cluster\n";
  @pvals = split(/\s+/,$pdata);
  @pvalues = ($pvals[1],$pvals[2],$pvals[4],$pvals[5]);
  $rawcluster = $cluster;
  $cluster =~ s/\_00/ /g unless ($cluster =~ /MS/ || $cluster =~ /PKS/ || $cluster =~ /Zw/ || $cluster =~ /ZW/);
  $cluster =~ s/\_0/ /g unless ($cluster =~ /MS/ || $cluster =~ /PKS/ || $cluster =~ /Zw/ || $cluster =~ /ZW/);
  $cluster =~ s/\_/ J/g if ($cluster =~ /MS/);
  $cluster =~ s/JJ/J/g;
  $cluster =~ s/\_/ /g;
  $cluster =~ s/BELL/bell/g;
  $cluster =~ s/YGNUS/ygnus/g;
  $cluster =~ s/YDRA/ydra/g;
  $cluster =~ s/ENTAURUS/entaurus/g;
  $cluster =~ s/ERCULES/ercules/g;
  $cluster =~ s/PHIUCHUS/phiuchus/g;
  $cluster =~ s/WICKY/wicky/g;
  $cluster =~ s/ERSIC/ersic/g;
#  $cluster .= "\$\\dagger\$" if ($k0sig < 1.0);
#  $cluster = "\{\\bfseries\{\\em\{".$cluster."\}\}\}"  if ($k0sig < 3.0);
  $rmin  = sprintf("%6.2f",$rmin);
  $rmax  = sprintf("%6.2f",$rmax);
  $ann   = sprintf("%6i",$ann);
  if ($rawcluster eq $oldcluster) {
      $cluster = '';
      $rmin = $pholder;
      $rmax = $pholder;
      $ann  = $pholder;
      $oldcluster = $oldcluster;
      $iter++;
  } else {
      $oldcluster = $rawcluster;
      $iter = 0;
  }
#  unless (($iter == 0) || ($iter == 2)) {
#      $rmin = $pholder;
#      $rmax = $pholder;
#      $ann  = $pholder;
#  }
  $obsid =~ s/\_/\+/g;
  $obsid = sprintf("%20s",$obsid);
  $mode  = 'extr' if ($mode eq "itpl");
  $mode  = $pholder if (($iter == 1) || ($iter == 3));
  $mode  = sprintf("%6s",$mode);
  $k0    = sprintf("%6.1f",$k0);
  if ($k0err < 0.1) {
      $k0err = sprintf("%6.2f",$k0err);
  } else {
      $k0err = sprintf("%6.1f",$k0err);
  }
  $k0    = $k0 . ' $\pm$ ' . $k0err if ($k0err != 0);
  $k0sig = sprintf("%6.1f",$k0sig);
  $k0sig = $pholder if (($iter == 1) || ($iter == 3));
  $k100  = sprintf("%6.1f",$k100);
  $k100err = sprintf("%6.1f",$k100err);
  $k100  = $k100 . ' $\pm$ ' . $k100err;
  $plaw  = sprintf("%6.2f",$plaw);
  $plawerr = sprintf("%6.2f",$plawerr);
  $plaw  = $plaw . ' $\pm$ ' . $plawerr;
  $chisq = sprintf("%6.2f",$chisq*$dof);
  $prob = $pvalues[$iter];
  $prob  = sprintf("%6.2e",$prob);
  $dof   = sprintf("%6i",$dof);

  # make hash to hold values
  %values = (
      "cluster" => $cluster,
      "obsid"   => $obsid,
      "mode" => $mode,
      "rmin" => $rmin,
      "rmax" => $rmax,
      "ann" => $ann,
      "k0" => $k0,
      "k0sig" => $k0sig,
      "k0err" => $k0err,
      "k100" => $k100,
      "k100err" => $k100err,
      "plaw" => $plaw,
      "plawerr" => $plawerr,
      "chisq" => $chisq,
      "prob" => $prob,
      "dof" => $dof
      );

  @out = @values{@items};
  print OUTFILE join(" & ",@out),"\\\\\n";
#  print OUTFILE "\\hline\\\\\n" if ($iter == 3);
}

print_footer();

############################################
############################################

sub print_colhead {
    my ($line,$out);
    @out = @_;
    foreach $out (@out) {
	$out = '\colhead{' . $out . '}';
    }
    $line = join(' & ',@out);
    return $line;
}

############################################
############################################

sub print_header {

    # define some useful hashes
    %colhead = (
	"cluster" => "Cluster",
	"obsid"   => "Obs.ID",
	"mode" => "Method",
	"rmin" => "\$r\_{min}\$",
	"rmax" => "\$r\_{max}\$",
	"ann" => "\$N_{bins}\$",
	"k0" => "\\kna",
	"k0sig" => "\$\\sigma\_{\\kna} > 0\$",
	"k0err" => "\$\\sigma\\kna\$",
	"k100" => "\\khun",
	"k100err" => "\$\\sigma\\khun\$",
	"plaw" => "\$\\alpha\$",
	"plawerr" => "\$\\sigma\\alpha\$",
	"chisq" => "\\chisq",
	"prob" => "p-value",
	"dof" => "DOF"
	);

    %units = (
	"cluster" => "",
	"obsid"   => "",
	"mode" => "",
	"rmin" => "Mpc",
	"rmax" => "Mpc",
	"ann" => "",
	"k0" => "\\ent",
	"k0sig" => "",
	"k0err" => "\\ent",
	"k100" => "\\ent",
	"k100err" => "\\ent",
	"plaw" => "",
	"plawerr" => "",
	"chisq" => "",
	"prob" => "",
	"dof" => ""
	);

    # make first header row
    @header1 = @colhead{@items};
    @header2 = @units{@items};

    # make row of column numbers
    $nhead = @header1;
    @header3 = map { '{(' . $_ . ')}'} (1..$nhead);

    # print the table header
    if ($complete eq "yes") {
	print OUTFILE (
	    '\documentclass[apj]{emulateapj}'."\n",
	    '\usepackage{apjfonts,graphicx,here,common,longtable}'."\n",
	    '\begin{document}'."\n",
	    '\LongTables'."\n",
	    );
    }
    print OUTFILE (
	'\begin{deluxetable}{' . $tabcolumns . '}',"\n",
	'\tablewidth{0pt}',"\n",
	'\tabletypesize{\scriptsize}',"\n",
	'\tablecaption{' . $caption . '}',"\n",
	'\tablehead{',
	print_colhead(@header1),'\\\\',"\n",
	print_colhead(@header2),'\\\\',"\n",
	print_colhead(@header3),"\n",
	'}',"\n",
	'\startdata',"\n"
	);
}

############################################
############################################

sub print_footer{
    print OUTFILE (
	'\enddata',"\n",
	'\tablecomments{'.$tabcomments.'}',"\n",
	'\end{deluxetable}',"\n"
	);
    if ($complete eq "yes") {
	print OUTFILE (
	    '\end{document}',"\n"
	    );
    }
}

############################################
############################################

