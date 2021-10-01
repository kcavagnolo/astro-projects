#!/usr/bin/perl -w
# perl table_beta.pl ../pf_fits/dat/betafits.dat betafits.tex

# useful settings for this table
$complete = "no";
$type = "aastex";
$caption = "Summary of \$\\beta\$-Model Fits" . '\label{tab:betafits}';
@items = ("cluster","s01","rc1","beta1","s02","rc2","beta2","dof","chi");
@tabcolumns = ("l");
foreach $a (@items) {
    push @tabcolumns, "c";
}
pop @tabcolumns;
$tabcolumns = join("",@tabcolumns);
$tabcomments = 'Col. (1) Cluster name;'
    .' col. (2) central surface brightness of first component;'
    .' col. (3) core radius of first component;'
    .' col. (4) $\beta$ parameter of first component;'
    .' col. (5) central surface brightness of second component;'
    .' col. (6) core radius of second component;'
    .' col. (7) $\beta$ parameter of second component;'
    .' col. (8) model degrees of freedom;'
    .' and col. (9) reduced chi-squared statistic for best-fit model.';

# open latex file
$out = pop(@ARGV);
open(OUTFILE,">$out") || die "Can't open $out for writing\n";

# print header information
print_header();

# read file
%file = read_file($ARGV[0]);

foreach $key (sort keys %file) {

  @dat = split(/\s+/,$file{$key});

  # read data
  $cluster = $dat[0];
  $obsid = $dat[1];
  $s01   = $dat[2];
  $s01err = $dat[3];
  $s02   = $dat[4];
  $s02err = $dat[5];
  $rc1   = $dat[6];
  $rc1err = $dat[7];
  $rc2   = $dat[8];
  $rc2err = $dat[9];
  $beta1 = $dat[10];
  $b1err = $dat[11];
  $beta2 = $dat[12];
  $b2err = $dat[13];
  $dof   = $dat[14];
  $chi   = $dat[15]/$dof;

  # format
  $cluster =~ s/\_00/ /g unless ($cluster =~ /MS/ || $cluster =~ /PKS/);
  $cluster =~ s/\_0/ /g unless ($cluster =~ /MS/ || $cluster =~ /PKS/);
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
  $obsid  = sprintf("%4s",$obsid);
  $s01    = sprintf("%5.2f",$s01/1e-6);
  $s01err = sprintf("%5.2f",$s01err/1e-6);
  $s02    = sprintf("%5.2f",$s02/1e-6);
  $s02err = sprintf("%5.2f",$s02err/1e-6);
  $rc1    = sprintf("%5.1f",$rc1);
  $rc1err = sprintf("%5.1f",$rc1err);
  $rc2    = sprintf("%5.1f",$rc2);
  $rc2err = sprintf("%5.1f",$rc2err);
  $beta1  = sprintf("%5.2f",$beta1);
  $b1err  = sprintf("%5.2f",$b1err);
  $beta2  = sprintf("%5.2f",$beta2);
  $b2err  = sprintf("%5.2f",$b2err);
  $dof    = sprintf("%5i",$dof);
  $chi    = sprintf("%5.2f",$chi);

#   if ($rc2 > 0 && $rc2 < $rc1) {
#     $temp = $s01;
#     $s01 = $s02;
#     $s02 = $temp;
#     $temp = $s01err;
#     $s01err = $s02err;
#     $s02err = $temp;
#     $temp = $rc1;
#     $rc1 = $rc2;
#     $rc2 = $temp;
#     $temp = $rc1err;
#     $rc1err = $rc2err;
#     $rc2err = $temp;
#     $temp = $beta1;
#     $beta1 = $beta2;
#     $beta2 = $temp;
#     $temp = $b1err;
#     $b1err = $b2err;
#     $b2err = $temp;
#   }

  if ($s02 == 0) {
    $s02 = "--";
    $rc2 = "--";
    $beta2 = "--";
  }

  $s01    = $s01 . ' $\pm$ ' . $s01err;
  $s02    = $s02 . ' $\pm$ ' . $s02err if ($s02err != 0);
  $rc1    = $rc1 . ' $\pm$ ' . $rc1err;
  $rc2    = $rc2 . ' $\pm$ ' . $rc2err if ($rc2err != 0);
  $beta1  = $beta1 . ' $\pm$ ' . $b1err;
  $beta2  = $beta2 . ' $\pm$ ' . $b2err if ($b2err != 0);

  # make hash to hold values
  %values = (
	     "cluster" => $cluster,
	     "obsid"  => $obsid,
	     "s01"    => $s01,
	     "s01err" => $s01err,
	     "s02"    => $s02,
	     "s02err" => $s02err,
	     "rc1"    => $rc1,
	     "rc1err" => $rc1err,
	     "rc2"    => $rc2,
	     "rc2err" => $rc2err,
	     "beta1"  => $beta1,
	     "b1err"  => $b1err,
	     "beta2"  => $beta2,
	     "b2err"  => $b2err,
	     "dof"    => $dof,
	     "chi"    => $chi
	    );

  @out = @values{@items};
  print OUTFILE join(" & ",@out),"\\\\\n";
}

# close latex file
print_footer();

#######################
#######################
##   Sub-Routines    ##
#######################
#######################

sub read_file {

  my($infile) = @_;
  my(@data,$name,%info);
  open(INFILE,$infile) || die "\n## Can't open $infile\n";
  while (<INFILE>) {
    chomp;
    next if (/^\#/); # skip comment lines
    next if (/^$/);  # skip blank lines
    s/^\s+//;        # trim leading whitespace
    s/\s+$//;        # trim trailing whitespace
    @data = split;
    $name = join "_", $data[0],$data[1],$data[2];
    $info{$name} = $_;
  }
  close INFILE;
  return %info;
}

#######################
#######################

sub print_colhead {
  my ($line,$out);
  @out = @_;
  foreach $out (@out) {
    $out = '\colhead{' . $out . '}';
  }
  $line = join(' & ',@out);
  return $line;
}

#######################
#######################

sub print_header {

  # define some useful hashes
  %colhead = (
	      "cluster" => "Cluster",
	      "obsid"  => "Obs. ID",
	      "s01"    => '$S_{01}$',
	      "s01err" => '\sigma$_{01}$',
	      "s02"    => '$S_{02}$',
	      "s02err" => '\sigma S$_{02}$',
	      "rc1"    => '$r_{c1}$',
	      "rc1err" => '\sigma r$_{c1}$',
	      "rc2"    => '$r_{c2}$',
	      "rc2err" => '\sigma r$_{c2}$',
	      "beta1"  => '$\beta_{1}$',
	      "b1err"  => '$\sigma \beta_{1}$',
	      "beta2"  => '$\beta_{2}$',
	      "b2err"  => '$\sigma \beta_{2}$',
	      "dof"    => 'D.O.F.',
	      "chi"    => '$\chi_{\\mathrm{red}}^2$'
	     );

  %units = (
	    "cluster" => "--",
	    "obsid"  => "--",
	    "s01"    => '10$^{-6}$ cts/s/asec$^{2}$',
	    "s01err" => '10$^{-6}$ cts/s/asec$^{2}$',
	    "s02"    => '10$^{-6}$ cts/s/asec$^{2}$',
	    "s02err" => '10$^{-6}$ cts/s/asec$^{2}$',
	    "rc1"    => '\\arcsec',
	    "rc1err" => '\\arcsec',
	    "rc2"    => '\\arcsec',
	    "rc2err" => '\\arcsec',
	    "beta1"  => "--",
	    "b1err"  => "--",
	    "beta2"  => "--",
	    "b2err"  => "--",
	    "dof"    => "--",
	    "chi"    => "--"
	   );

  # make first header row
  @header1 = @colhead{@items};
  @header2 = @units{@items};

  # make row of column numbers
  $nhead = @header1;
  @header3 = map { '{(' . $_ . ')}'} (1..$nhead);

  # print the table header
  # print the table header
  if ($complete eq "yes") {
    print OUTFILE (
                   '\documentclass{'.$type.'}'."\n",
                   '\usepackage{apjfonts}'."\n",
                   '\usepackage{graphicx}'."\n",
                   '\usepackage{here,deluxetable,lscape}'."\n",
                   '\begin{document}'."\n",
                   '\begin{landscape}'."\n"
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

#######################
#######################

sub print_footer{
  print OUTFILE (
		 '\enddata',"\n",
		 '\tablecomments{'.$tabcomments.'}',"\n",
		 '\end{deluxetable}',"\n",
		);
  if ($complete eq "yes") {
    print OUTFILE (
                   '\end{landscape}',"\n",
                   '\end{document}',"\n"
                  );
  }
}

#######################
#######################
