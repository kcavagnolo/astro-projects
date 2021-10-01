#!/usr/bin/perl -w
# perl table_spec.pl ../data/global_nhfro.dat ../data/blobs_fefree.dat specfits.tex
# perl table_spec.pl ../data/global_fefro.dat ../data/allblobs.dat specfits.tex

# useful settings for this table
$datdir = "/mnt/DROBO";
$rootdir = "reprocessed";
$complete = "no";
$type = "mnras"; #aastex, mnras
@items = ("reg","rin","rout","tx","lbol","fe","chisq","dof","src","norm","cr");
$tabcomments = 
    'A dagger ($\dagger$) indicates core-excised regions fit with $Z$ fixed'.
    'at the iteratively determined value for'.
    '$R_{500-\mathrm{Core}}$. Bolometric luminosities were determined using'.
    'a diagonalized response function over the energy range 0.01-100.0 keV'.
    'with 5000 linearly spaced energy channels. All quoted uncertainties'.
    'are 90\% confidence. Col. (1) Region used for spectral extraction;'.
    'Col. (2) Inner radius of extraction region; Col. (3) Outer radius of'.
    'extraction region; Col. (4) Best-fit ICM X-ray temperature; Col. (5)'.
    'Unabsorbed bolometric (0.01-100.0 keV) ICM luminosity; Col. (6) Best-fit'.
    'ICM metallicity; Col. (7) Reduced \chisq\ for best-fit model; Col. (8)'.
    'Degrees of freedom in best-fit model; Col. (9) Percentage of emission'.
    'attributable to source; Col. (10) Best-fit model normalization;'.
    'Col. (11) Background-subtracted spectrum count rate.';
$label = "specfits";
$caption = "Summary of Global Spectral Fits";
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
	    $norm = $dat[13]/1e-4;
	    $normlo = $dat[14]/1e-4;
	    $normhi = $dat[15]/1e-4;
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

	    # find and open lumin dat
	    $lfile = "$datdir/$obsid/$rootdir/${obsid}_${reg}_lumin.dat";
	    unless (-e $lfile) {
		print "## ERROR: Missing $lfile\n";
		$lbol = -1.0;
		$lbolhi = -1.0;
		$lbollo = -1.0;
	    } else {
		open(LUM,"$lfile");
		while(<LUM>){
		    chomp;
		    next if (/^\#/);
		    next if (/^$/);
		    s/^\s+//;
		    s/\s+$//;
		    if (/^Lbol/) {
			@dat = split;
			$lbol = $dat[1];
			$lbollo = $dat[2];
			$lbolhi = $dat[3];
		    }
		}
		close LUM;
	    }
	    $lx = 10.0;
	    $lxlo = 1.0;
	    $lxhi = 100.0;

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
	    unless ($reg =~ m/blob/) {
		$rin = $rin*60.*$da;
		$rout = $rout*60.*$da;
	    } else {
		$rin = 0.0;
		$rout = 0.0;
	    }

	    # format
	    $cluster =~ s/\_/ /g;
	    $obsid = sprintf("%4s",$obsid);
	    $reg   =~ s/r/\$R\_\{/ unless ($reg =~ m/blob/);
	    $reg   =~ s/-50/-CORE/;
	    $reg   =~ s/core/\\mathrm\{core\}/;
	    $reg   = $reg.'}$' unless ($reg =~ m/blob/);
	    $reg   = 'Eastern Blob' if ($reg =~ m/east/);
	    $reg   = 'Northwest Spur' if ($reg =~ m/west/);
	    $rin   = sprintf("%3i",$rin);
	    $rout  = sprintf("%3i",$rout);
	    if ($type eq "aastex") {
		$rin   = '\nodata' if ($rin == 0);
		$rout  = '\nodata' if ($rout == 0);
	    } else {
		$rin   = '--' if ($rin == 0);
		$rout  = '--' if ($rout == 0);
	    }
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
	    $norm   = sprintf("%-7.2f",$norm);
	    $normhi = sprintf("%-7i",((($normhi-$norm)/$norm)*100));
	    $normlo = sprintf("%-7i",((($norm-$normlo)/$norm)*100));
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
	    if ($lbolhi != 0) {
		$lbol   = sprintf("%-4.2f",$lbol);
		$lbolhi = sprintf("%-4.2f",$lbolhi - $lbol);
		$lbollo = sprintf("%-4.2f",$lbol   - $lbollo);
		$lbol   = $lbol . '$^{+' . $lbolhi . '}_{-' . $lbollo . '}$ ';
	    }
	    if ($lxhi != 0) {
		$lx   = sprintf("%-4.2f",$lx);
		$lxhi = sprintf("%-4.2f",$lxhi - $lx);
		$lxlo = sprintf("%-4.2f",$lx   - $lxlo);
		$lx   = $lx . '$^{+' . $lxhi . '}_{-' . $lxlo . '}$ ';
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
		"lbol"  => $lbol,
		"lx"    => $lx,
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
	if ($type eq "aastex") {
	    $out = '\colhead{' . $out . '}';
	}
    }
    $line = join(' & ',@out);
    return $line;
}

#######################
#######################

sub print_header {

  # define some useful hashes
  %colhead = (
      "reg"     => "Region",
      "cluster" => "Cluster",
      "obsid"   => "Obs.ID",
      "rin"     => '$R_{\mathrm{in}}$',
      "rout"    => '$R_{\mathrm{out}}$ ',
      "nh20"    => '$N_{H}$',
      "tx"      => '\tx',
      "tx2"     => '\tx2',
      "lbol"    => '\lbol',
      "lx"      => '\lx',
      "fe"      => '$Z$',
      "z"       => 'redshift',
      "norm"    => '$\eta$',
      "norm2"   => '$\eta_2$',
      "cr"      => 'Ct. Rate',
      "chisq"   => '\redchisq',
      "dof"     => 'D.O.F.',
      "src"     => '\% Source'
      );

  %units = (
      "reg"     => '-',
      "cluster" => '-',
      "obsid"   => '-',
      "rin"     => 'kpc',
      "rout"    => 'kpc',
      "nh20"    => '$10^{20}$ cm$^{-2}$',
      "tx"      => 'keV',
      "tx2"     => 'keV',
      "lbol"    => '$10^{44}$ erg s$^{-1}$',
      "lx"      => '$10^{44}$ erg s$^{-1}$',
      "fe"      => '$Z_{\sun}$',
      "z"       => '-',
      "norm"    => '$10^{-4}$ cm$^{-5}$',
      "norm2"   => '$10^{-4}$ cm$^{-5}$',
      "cr"      => 'ct s$^{-1}$',
      "chisq"   => '-',
      "dof"     => '-',
      "src"     => '-'
      );

  # make first header row
  @header1 = @colhead{@items};
  @header2 = @units{@items};

  # make row of column numbers
  $nhead = @header1;
  if ($type eq "aastex") {
      @header3 = map { '{(' . $_ . ')}'} (1..$nhead);
  } else {
      @header3 = map { '(' . $_ . ')'} (1..$nhead);
  }

  # print the table header
  # print the table header
  if ($type eq "aastex") {
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
	  '\begin{deluxetable}{'.${tabcolumns}.'}',"\n",
	  '\tablewidth{0pt}',"\n",
	  '\tabletypesize{\scriptsize}',"\n",
	  '\tablecaption{'.${caption}.'}',"\n",
	  '\tablehead{',
	  print_colhead(@header1),'\\\\',"\n",
	  print_colhead(@header2),'\\\\',"\n",
	  print_colhead(@header3),"\n",
	  '}',"\n",
	  '\startdata',"\n"
	  );
  } else {
      print OUTFILE (
	  '\begin{table*}',"\n",
	  '\caption{\sc '.${caption}.' \label{tab:'.${label}.'}}',"\n",
	  '\begin{tabular}{'.${tabcolumns}.'}',"\n",
	  '\hline',"\n",
	  '\hline',"\n",
	  print_colhead(@header1),'\\\\',"\n",
	  print_colhead(@header2),'\\\\',"\n",
	  print_colhead(@header3),'\\\\',"\n",
	  '\hline',"\n",
	  );
  }
}

#######################
#######################

sub print_footer{
  if ($type eq "aastex") {
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
  } else {
      print OUTFILE (
	  '\hline',"\n",
	  '\end{tabular}',"\n",
	  '\begin{quote}',"\n",
	  "$tabcomments\n",
	  '\end{quote}',"\n",
	  '\end{table*}',"\n",
	  );
  }
}

#######################
#######################
