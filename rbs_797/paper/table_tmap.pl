#!/usr/bin/perl -w
# perl table_tmap.pl tmap_30d0.dat tmap.tex
# perl table_tmap.pl /mnt/DROBO/7902/tempmap/cbin_50/xspec_allregs.params cbin_tmap.tex

# useful settings for this table
$complete = "no";
$type = "aastex";
@items = ("bin","nh20","tx","fe","chisq","dof","src");
$tabcomments = 
    'Col. (1) Bin number (see Fig. \ref{fig:tmap} for reference); '
    .'col. (2) absorbing, Galactic neutral hydrogen column density; '
    .'col. (3) best-fit temperature; '
    .'col. (4) best-fit metallicity; '
    .'col. (5) reduced \chisq\ for best-fit model; '
    .'col. (6) degrees of freedom for best-fit model; '
    .'col. (7) percentage of emission attributable to source.';
$caption = "Summary of Temperature Map Spectral Fits" . '\label{tab:tmapfits}';
@tabcolumns = ('l');
foreach (@items) {
    push @tabcolumns, "c";
}
pop @tabcolumns;
$tabcolumns = join("",@tabcolumns);

# open latex file
$out = pop(@ARGV);
open(OUTFILE,">$out") || die "Can't open $out for writing\n";

# print header information
print_header();

# read file
%file = read_file($ARGV[0]);

$oldcluster = "fkjhsdfkjsh";
foreach $key (sort keys %file) {

  @dat = split(/\s+/,$file{$key});

  $cluster = $dat[0];
  $nh = $dat[4];
  $nlo = $dat[5];
  $nhi = $dat[6];
  if ($cluster eq $oldcluster) {
    $cluster = ' ';
    $nh = sprintf("%3s","...") if ($nhi == 0);
    $oldcluster = $oldcluster;
  } else {
    $oldcluster = $cluster;
  }
  $obsid = $dat[1];
  $bin = $dat[2];
  $rout = $dat[3];
  $tx = $dat[7];
  $txlo = $dat[8];
  $txhi = $dat[9];
  $fe = $dat[10];
  $felo = $dat[11];
  $fehi = $dat[12];
  $norm = $dat[13];
  $normlo = $dat[14];
  $normhi = $dat[15];
  $tx2 = $dat[16];
  $tx2lo = $dat[17];
  $tx2hi = $dat[18];
  $norm2 = $dat[19];
  $norm2lo = $dat[20];
  $norm2hi = $dat[21];
  $z = $dat[22];
  $cr = $dat[23];
  $src = $dat[24];
  $chisq = $dat[25];
  $dof = $dat[26];

  # format
  $cluster =~ s/\_/ /g;
  $obsid = sprintf("%4s",$obsid);
  $bin   = sprintf("%3i",$bin);
  $rout  = sprintf("%3.2f",$rout);
  $chisq = sprintf("%3.2f",$chisq);
  $dof   = sprintf("%3i",$dof);
  $src   = sprintf("%3i",$src);
  if ($nhi != 0 && $nlo != 0) {
    $nhi = sprintf("%-4.2f",$nhi - $nh);
    $nlo = sprintf("%-4.2f",$nh - $nlo);
    $nh = $nh . '$^{+' . $nhi . '}_{-' . $nlo . '}$ ';
  }
  $tx  = sprintf("%-4.2f",$tx);
  $thi = sprintf("%-4.2f",$txhi - $tx);
  $tlo = sprintf("%-4.2f",$tx  - $txlo);
  $thi = '\infty' if ($txhi > 30);
  $tx  = $tx  . '$^{+' . $thi . '}_{-' . $tlo . '}$ ';
  $tx2  = sprintf("%-4.2f",$tx2);
  $t2hi = sprintf("%-4.2f",$tx2hi - $tx2);
  $t2lo = sprintf("%-4.2f",$tx2  - $tx2lo);
  $t2hi = '\infty' if ($tx2hi > 30);
  $tx2  = $tx2  . '$^{+' . $t2hi . '}_{-' . $t2lo . '}$ ';
  $norm   = sprintf("%-7.2e",$norm);
  $normhi = sprintf("%-7i",(($normhi/$norm)*100));
  $normlo = sprintf("%-7i",(($normlo/$norm)*100));
  $norm   = $norm  . '$^{+' . $normhi . '\%}_{-' . $normlo . '\%}$ ';
  if ($norm2 != 0) {
    $norm2   = sprintf("%-7.2e",$norm2);
    $norm2hi = sprintf("%-7i",(($norm2hi-$norm2)/$norm2)*100);
    $norm2lo = sprintf("%-7i",(($norm2-$norm2lo)/$norm2)*100);
    $norm2   = $norm2  . '$^{+' . $norm2hi . '\%}_{-' . $norm2lo . '\%}$ ';
  }
  if ($fehi != 0) {
    $fe   = sprintf("%-4.2f",$fe);
    $fehi = sprintf("%-4.2f",$fehi - $fe);
    $felo = sprintf("%-4.2f",$fe   - $felo);
    $fe   = $fe . '$^{+' . $fehi . '}_{-' . $felo . '}$ ';
  }

  # make hash to hold values
  %values = (
      "cluster" => $cluster,
      "obsid" => $obsid,
      "bin"   => $bin,
      "rout"  => $rout,
      "nh20"  => $nh,
      "tx"    => $tx,
      "tx2"   => $tx2,
      "fe"    => $fe,
      "z"     => $z,
      "norm"  => $norm,
      "norm2" => $norm2,
      "cr"    => $cr,
      "chisq" => $chisq,
      "dof"   => $dof,
      "src"   => $src
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
	      "obsid"   => "Obs.ID",
	      "bin"     => 'Bin',
	      "rout"    => 'R$_{out}$ ',
	      "nh20"    => 'N$_{HI}$',
	      "tx"      => 'T$_{X}$',
	      "tx2"     => 'T$_{X,2}$',
	      "fe"      => 'Z',
	      "z"       => 'z',
	      "norm"    => '\$\eta\$',
	      "norm2"   => '\$\eta_2\$',
	      "cr"      => 'c.r.',
	      "chisq"   => '$\chi^2_{red.}$',
	      "dof"     => 'DOF',
	      "src"     => "\\% Source"
	     );

  %units = (
	    "cluster" => " ",
	    "obsid"   => " ",
	    "bin"     => " ",
	    "rout"    => "kpc",
	    "nh20"    => "\$10^{20}\$ cm\$^{-2}\$",
	    "tx"      => "keV",
	    "tx2"     => "keV",
            "fe"      => "Z\$\_\{\\sun\}\$",
	    "z"       => " ",
	    "norm"    => " ",
	    "norm2"   => " ",
	    "cr"      => "\$s^{-1}\$",
	    "chisq"   => " ",
	    "dof"     => " ",
	    "src"     => " "
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
