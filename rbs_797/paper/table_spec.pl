#!/usr/bin/perl -w
# perl table_spec.pl ../data/global_nhfree_zfree_grp.dat ../data/global_nhfree_zfro_grp.dat ../data/global_nhfro_zfree_grp.dat ../data/global_nhfro_zfro_grp.dat ../data/arms.dat specfits.tex ; make clean ; make

# useful settings for this table
$complete = "no";
$type = "aastex";
@items = ("reg","rin","rout","nh20","tx","fe","z","chisq","dof","src");
$tabcomments = 
    'Col. (1) Name of region used for spectral extraction; '
    .'col. (2) inner radius of extraction region; '
    .'col. (3) outer radius of extraction region; '
    .'col. (4) absorbing, Galactic neutral hydrogen column density; '
    .'col. (5) best-fit temperature; '
    .'col. (6) best-fit metallicity; '
    .'col. (7) best-fit redshift; '
    .'col. (8) reduced \chisq\ for best-fit model; '
    .'col. (9) degrees of freedom for best-fit model; '
    .'col. (10) percentage of emission attributable to source.';
$caption = "Summary of Global Spectral Properties" . '\label{tab:specfits}';
@tabcolumns = ('l');
foreach (@items) {
    push @tabcolumns, "c";
}
pop @tabcolumns;
$tabcolumns = join("",@tabcolumns);

# open latex file
$out = pop(@ARGV);
open(OUTFILE,">$out") || die "## ERROR: Cannot open $out for writing\n";
print_header();

# loop through file
foreach $file (@ARGV) {
    open(A,"$file");
    while (<A>) {
	chomp;
	next if (/^$/);  # skip blank lines
	s/^\s+//;        # trim leading whitespace
	s/\s+$//;        # trim trailing whitespace
	if (/^#\s+Model/) {
	    @line = split;
	    $reg = $line[4];
	    next;
	} elsif (/^#/ || $reg eq "rlx-core") {
	    next;
	} else {
	    @dat = split(/\s+/);
	    $cluster = $dat[0];
	    $obsid = $dat[1];
	    $rin = $dat[2];
	    $rout = $dat[3];
	    $nh = $dat[4];
	    $nlo = $dat[5];
	    $nhi = $dat[6];
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
	    $zlo = $dat[23];
	    $zhi = $dat[24];
	    $cr = $dat[25];
	    $src = $dat[26];
	    $chisq = $dat[27];
	    $dof = $dat[28];

	    # get kpc values
	    # write all IDL commands to a file and then run
	    unless (defined $da) {
		open(PROFILE,">temp.pro");
		print PROFILE "\!quiet=1\n";
		print PROFILE "cosmology,'$z',result,/silent\n";
		print PROFILE "conv = result[4]\n";
		print PROFILE "openw,1,'temp.log'\n";
		print PROFILE "printf,1,strcompress(conv,/remove_all)\n";
		print PROFILE "close,1\n";
		print PROFILE "exit \n";
		close PROFILE;
		system("\$IDL_DIR/bin/idl temp.pro");
		open(FILE,"temp.log");
		while(<FILE>) {
		    chomp;
		    next if (/^\#/);
		    next if (/^$/);
		    s/^\s+//;  # trim leading whitespace
		    s/\s+$//;  # trim trailing whitespace
		    @vals = split;
		    $da = $vals[0];
		}
		close FILE;
		unlink("temp.log");
		unlink("temp.pro");
	    }
	    unless ($reg =~ m/Arm/) {
		$rin = $rin*60.*$da;
		$rout = $rout*60.*$da;
	    } else {
		$rin = 0.0;
		$rout = 0.0;
	    }

	    # format
	    $cluster =~ s/\_/ /g;
	    $obsid = sprintf("%4s",$obsid);
	    $reg   =~ s/r/\$R\_\{/ unless ($reg =~ m/Arm/);
	    $reg   =~ s/-50/-CORE/;
	    $reg   = $reg.'}$' unless ($reg =~ m/Arm/);
	    $rin   = sprintf("%3i",$rin);
	    $rin   = '\nodata' if ($rin == 0);
	    $rout  = sprintf("%3i",$rout);
	    $rout  = '\nodata' if ($rout == 0);
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
	    if ($zhi != 0 && $zlo != 0) {
		$zhi = sprintf("%-4.4f",$zhi - $z);
		$zlo = sprintf("%-4.4f",$z - $zlo);
		$z = $z . '$^{+' . $zhi . '}_{-' . $zlo . '}$ ';
	    }
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
		"reg"   => $reg,
		"cluster" => $cluster,
		"obsid" => $obsid,
		"rin"   => $rin,
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
    }
    print OUTFILE "\\hline\n";
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
      "reg" => "Region",
      "cluster" => "Cluster",
      "obsid"   => "Obs.ID",
      "rin"     => '$R_{in}$',
      "rout"    => '$R_{out}$ ',
      "nh20"    => '$N_{HI}$',
      "tx"      => '$T_{X}$',
      "tx2"     => '$T_{X,2}$',
      "fe"      => '$Z$',
      "z"       => 'redshift',
      "norm"    => '\$\eta\$',
      "norm2"   => '\$\eta_2\$',
      "cr"      => 'c.r.',
      "chisq"   => '$\chi^2_{red.}$',
      "dof"     => 'D.O.F.',
      "src"     => "\\% Source"
      );

  %units = (
      "reg" => " ",
      "cluster" => " ",
      "obsid"   => " ",
      "rin"     => "kpc",
      "rout"    => "kpc",
      "nh20"    => "\$10^{20}\$ cm\$^{-2}\$",
      "tx"      => "keV",
      "tx2"     => "keV",
      "fe"      => "\$Z\_\{\\sun\}\$",
      "z"       => " ",
      "norm"    => " ",
      "norm2"   => " ",
      "cr"      => "\cts sec\$^{-1}\$",
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
