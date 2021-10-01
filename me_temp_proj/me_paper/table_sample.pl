#!/usr/bin/perl -w

# This script takes the XSPEC fit results .dat file
# and makes a latex table; to use this script:
# perl table_sample.pl full_sample_L-50.list sample.tex

# useful settings for this table
$type = "aastex";
#@items = ("cluster","obsid","ra","dec","exp","mode","z","nh","tx","fe","chip","lbol","eobs","rmax","robs");
#$tabcolumns = 'lcrrccccccccccc';

@items = ("cluster","obsid","ra","dec","exp","mode","chip","z","lbol");
$tabcolumns = 'lcccccccc';

$tabcomments = '(1) Cluster name, (2) CDA observation identification'
  .' number, (3) R.A. of cluster center, (4) Dec. of cluster center, (5) nominal exposure'
  .' time, (6) observing mode, (7) CCD location of centroid, (8) redshift,'
  .' (9) NRAO absorbing Galactic neutral hydrogen column density,'
  .' (10) bolometric luminosity.'
  .' $\dagger$ indicates clusters analyzed within R$_{5000}$ only.';
$caption = "Summary of Sample" . '\label{tab:sample}';

# read in file
open(INFILE,$ARGV[0]) || die "Can't open $ARGV[0] for reading\n";
@lines = <INFILE>;

# open latex file
open(OUTFILE,">$ARGV[1]") || die "Can't open $ARGV[1] for writing\n";

# print header information
print_header();

foreach $line (@lines) {

  # check for blank lines and comments
  next if ($line =~ /^\#/);
  next if ($line =~ /^$/);

  ($cluster,$obsid,$x,$y,$rmax,$mincts,$z,$nh,$tx,$fe,$lbol,$lbollo,
   $lbolhi,$chip,$eobs,$diff,$robs,$ra,$dec,$exp,$mode,$r12,$r45,$r67,
   $rat12,$rat45,$rat67) = split(" ",$line);

  # format the name
  if ($cluster =~ /\_dag$/) {
      $cluster =~ s/\_dag$//;
      $cluster .= ' $\dagger$';
  }
  $cluster =~ s/\_/ /g;
  $obsid = sprintf("%4s",$obsid);
  $x = sprintf("%4i",$x);
  $y = sprintf("%4i",$y);
  $rmax = sprintf("%3i",$rmax);
  $mincts = sprintf("%6i",$mincts);
  $z = sprintf("%5.3f",$z);
  $nh = sprintf("%3.2f",$nh);
  $tx = sprintf("%3.2f",$tx);
  $fe = sprintf("%3.2f",$fe);
  $lbol = sprintf("%5.2f",$lbol);
  $lbollo = sprintf("%5.2f",$lbollo);
  $lbolhi = sprintf("%5.2f",$lbolhi);
  $chip =~ s/s/S/g;
  $chip =~ s/i/I/g;
  $chip = sprintf("%2s",$chip);
  $eobs = sprintf("%3.2f",$eobs);
  $diff = sprintf("%3s",$diff);
  $robs = sprintf("%3i",$robs);
  $ra =  sprintf("%10s",$ra);
  $dec = sprintf("%10s",$dec);
  $exp = sprintf("%3.1f",$exp);
  $mode =~ s/^FAINT/F/g;
  $mode =~ s/^VFAINT/VF/g;
  $mode = sprintf("%2s",$mode);
  $r12 = sprintf("%5.4f",$r12);
  $r45 = sprintf("%5.4f",$r45);
  $r67 = sprintf("%5.4f",$r67);
  $rat12 = sprintf("%5.4f",$rat12);
  $rat45 = sprintf("%5.4f",$rat45);
  $rat67 = sprintf("%5.4f",$rat67);

  # make hash to hold values
  %values = (
	     "cluster" => $cluster,
	     "obsid"   => $obsid,
	     "x" => $x,
	     "y" => $y,
	     "rmax" => $rmax,
	     "mincts" => $mincts,
	     "z" => $z,
	     "nh" => $nh,
	     "tx" => $tx,
	     "fe" => $fe,
	     "lbol" => $lbol,
	     "lbolo" => $lbollo,
	     "lbolhi" => $lbolhi,
	     "chip" => $chip,
	     "eobs" => $eobs,
	     "diff" => $diff,
	     "robs" => $robs,
	     "ra" => $ra,
	     "dec" => $dec,
	     "exp" => $exp,
	     "mode" => $mode,
	     "r12" => $r12,
	     "r45" => $r45,
	     "r67" => $r67,
	     "rat12" => $rat12,
	     "rat45" => $rat45,
	     "rat67" => $rat67
	    );

  @out = @values{@items};
  print OUTFILE join(" & ",@out),"\\\\\n";
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
	      "x" => "Phys. X",
	      "y" => "Phys. Y",
	      "rmax" => "\$R\_{max}\$",
	      "mincts" => "Min. Cts.",
	      "z" => "z",
	      "nh" => "N\$\_{H}\$",
	      "tx" => "\$k\$T\$\_{X}\$",
	      "fe" => "Z",
	      "lbol" => "L\$\_{bol.}\$",
	      "chip" => "ACIS",
	      "eobs" => "\$\E_{obs}\$",
	      "diff" => "Diffuse",
	      "robs" => "\$R\_{break}\$",
	      "ra" => "R.A.",
	      "dec" => "Dec.",
	      "exp" => "ExpT",
	      "mode" => "Mode",
	      "r12" => "R12",
	      "r45" => "R45",
	      "r67" => "R67",
	      "rat12" => "R12\/Src",
	      "rat45" => "R45\/Src",
	      "rat67" => "R67\/Src"
	     );

  %units = (
	    "cluster" => " ",
	    "obsid"   => " ",
	    "x" => "pix",
	    "y" => "pix",
	    "rmax" => "pix",
	    "mincts" => "counts",
	    "z" => " ",
	    "nh" => "\$10^{20} cm^{-2}\$",
	    "tx" => "keV",
	    "fe" => "Z\$\_\{\\sun\}\$",
	    "lbol" => "\$10^{44}\$ ergs s\$^{-1}\$",
	    "chip" => " ",
	    "eobs" => "keV",
	    "diff" => " ",
	    "robs" => "pix",
	    "ra" => "hr\:min\:sec",
	    "dec" => "\$\\degr\:\\arcmin\:\\arcsec\$",
	    "exp" => "ksec",
	    "mode" => " ",
	    "r12" => "cts\/sec",
	    "r45" => "cts\/sec",
	    "r67" => "cts\/sec",
	    "rat12" => " ",
	    "rat45" => " ",
	    "rat67" => " "
	   );

  # make first header row
  @header1 = @colhead{@items};
  @header2 = @units{@items};

  # make row of column numbers
  $nhead = @header1;
  @header3 = map { '{(' . $_ . ')}'} (1..$nhead);

  # print the table header
  print OUTFILE (
#		 '\documentclass{'.$type.'}',"\n",
#		 '\usepackage{deluxetable,lscape}',"\n",
#		 '\usepackage{deluxetable}',"\n",
#		 '\begin{document}',"\n",
#		 '\begin{landscape}',"\n",
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
  print OUTFILE
    '\enddata',"\n",
      '\tablecomments{'.$tabcomments.'}',"\n",
	'\end{deluxetable}',"\n",
#	  '\end{landscape}',"\n",
#	    '\end{document}',"\n";
}
