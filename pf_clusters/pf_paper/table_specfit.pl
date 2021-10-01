#!/usr/bin/perl -w
# perl table_specfit.pl ../pf_fits/dat/pf_temp_profs.dat specfits.tex

# useful settings for this table
$complete = "no";
$type = "aastex";
$dor = "no";
$caption = "Summary of Spectral Fits" . '\label{tab:specfits}';
@items = ("cluster","ann","rin","rout","nh20","tx","fe","chisq","dof","src");
$tabcolumns = 'lccccccccc';
$tabcomments = 'Col. (1) Cluster name; col. (2) excluded core region in kpc; col. (3) R$_{5000}$ in kpc; col. (3) absorbing, Galactic neutral'
  .' hydrogen column density; cols. (4) and (5) best-fit MeKaL temperatures; col. (6) best-fit'
  .' 77 MeKaL abundance; col. (7) T$_{0.7-7.0}$/T$_{2.0-7.0}$ also called T$_{HFR}$;'
  .' cols. (8) and (9) reduced $\chi^2$ statistics; col. (10) percent of emission attributable to source.'
  .' A dagger ($\dagger$) indicates cluster with multiple independent, simultaneously fit spectra.';

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
    $ann++;
    $cluster = ' ';
    $nh = sprintf("%3s","...") if ($nhi == 0);
    $oldcluster = $oldcluster;
  } else {
    $ann = 1;
    $oldcluster = $cluster;
  }
  $obsid = $dat[1];
  $rin = $dat[2];
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

  # get kpc values
  undef @vals;
  if ($dor eq "yes") {
    if ($obsid =~ /\+/) {
      @obs = split('\+',$obsid);
      $tobs = $obs[0];
    } else {
      $tobs = $obsid;
    }
    $reg = "/Volumes/GALACTUS/${tobs}/reprocessed/${cluster}_${tobs}_annuli${ann}.reg";
    open(REG,$reg);
    while(<REG>){
      chomp;
      next if (/^\#/);
      next if (/^$/);
      s/^\s+//;
      s/\s+$//;
      if (/^annulus/) {
	@vals = split(",",$_);
	$rin = $vals[2];
	$rin =~ s/\)//g;
	$rout = $vals[3];
	$rout =~ s/\)//g;
      }
      if (/^circle/) {
	@vals = split(",",$_);
	$rin  = 0.00;
	$rout = $vals[2];
	$rout =~ s/\)//g;
      }
    }
    close REG;

    # write all IDL commands to a file and then run
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
    $rin = $rin*60.*$da*0.492;
    $rout = $rout*60.*$da*0.492;
  }

  # format
  $cluster =~ s/\_/ /g;
  $obsid = sprintf("%4s",$obsid);
  $ann   = sprintf("%2i",$ann);
  $rin   = sprintf("%3.2f",$rin);
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
	     "ann"   => $ann,
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
	      "ann"     => 'Ann.',
	      "rin"     => 'R$_{in}$',
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
	      "dof"     => 'd.o.f',
	      "src"     => "\\% Source"
	     );

  %units = (
	    "cluster" => " ",
	    "obsid"   => " ",
	    "ann"     => " ",
	    "rin"     => "kpc",
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
