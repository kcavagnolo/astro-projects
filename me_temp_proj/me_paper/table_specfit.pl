#!/usr/bin/perl -w
# perl table_specfit.pl ../me_fits/dat/culled_r2500-50_7-7.dat ../me_fits/dat/culled_r2500-50_2-7.dat r2500.tex
# perl table_specfit.pl ../me_fits/dat/culled_r5000-50_7-7.dat ../me_fits/dat/culled_r5000-50_2-7.dat r5000.tex

# useful settings for this table
$type = "aastex";
$dor = "yes";
$ap = "r2500-50";
$aptex = "2500";
$caption = "Summary of Excised R\$\_\{2500\}\$ Spectral Fits" . '\label{tab:r2500specfits}';
@items = ("Cluster","rin","rout","nh20","T77","T27","Tfrac","Fe77","chisq77","chisq27","src");
$tabcolumns = 'lcccccccccc';
$tabcomments = 'Note: \"77\" refers to 0.7-7.0 keV band and \"27\"'
    .' refers to 2.0-7.0 keV band. (1) Cluster name, (2) size of excluded'
    .' core region in kpc, (3) $R_{2500}$ in kpc, (4) absorbing Galactic'
    .' neutral hydrogen column density, (5,6) best-fit {\textsc{MeKaL}}'
    .' temperatures, (7) best-fit 77 {\textsc{MeKaL}} abundance, (8)'
    .' $T_{0.7-7.0}$/$T_{2.0-7.0}$ also called $T_{HBR}$, (9,10) respective'
    .' reduced $\chi^2$ statistics, and (11) percent of emission attributable'
    .' to source. A ($\ddagger$) indicates a cluster which has multiple'
    .' observations. Each observation has an independent spectrum extracted'
    .' along with an associated WARF, WRMF, and normalized background'
    .' spectrum. Each independent spectrum is then fit simultaneously with'
    .' the same spectral model to produce the final.';

# open latex file
$out = pop(@ARGV);
open(OUTFILE,">$out") || die "Can't open $out for writing\n";

# print header information
print_header();

# read file1
%file1 = read_file($ARGV[0]);
%file2 = read_file($ARGV[1]);

foreach $key (sort keys %file1) {

  @dat1 = split(/\s+/,$file1{$key});
  @dat2 = split(/\s+/,$file2{$key});

  $cluster = $dat1[0];
  $obsid = $dat1[1];
  $rin = $dat1[2];
  $rout = $dat1[3];
  $nh = $dat1[4];
  $nlo = $dat1[5];
  $nhi = $dat1[6];
  $tx = $dat1[7];
  $txlo = $dat1[8];
  $txhi = $dat1[9];
  $fe = $dat1[10];
  $felo = $dat1[11];
  $fehi = $dat1[12];
  $norm = $dat1[13];
  $normlo = $dat1[14];
  $normhi = $dat1[15];
  $z = $dat1[22];
  $cr = $dat1[23];
  $src = $dat1[24];
  $chisq = $dat1[25];
  $dof = $dat1[26];

  $tx2 = $dat2[7];
  $tx2lo = $dat2[8];
  $tx2hi = $dat2[9];
  $fe2 = $dat2[10];
  $fe2lo = $dat2[11];
  $fe2hi = $dat2[12];
  $norm2 = $dat2[13];
  $norm2lo = $dat2[14];
  $norm2hi = $dat2[15];
  $cr2 = $dat2[23];
  $chisq2 = $dat2[25];
  $dof2 = $dat2[26];
  $tfrac = $tx2/$tx;
  $tfrachi = ($tx2/$tx)*(sqrt((($tx2hi-$tx2)/$tx2)**2+(($txhi-$tx)/$tx)**2));
  $tfraclo = ($tx2/$tx)*(sqrt((($tx2-$tx2lo)/$tx2)**2+(($tx-$txlo)/$tx)**2));

  # get kpc value for r____-__
  undef @vals;
  if ($dor eq "yes") {
    if ($obsid =~ /\+/) {
      @obs = split('\+',$obsid);
      $tobs = $obs[0];
    } else {
      $tobs = $obsid;
    }
    $reg = "/mnt/SINISTER/${tobs}/reprocessed/${tobs}_${ap}.reg";
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
    $rin = $rin*$da*0.492;
    $rout = $rout*$da*0.492;
#    if ($rin <= 50) {
#	$rin = $rin + 22.0;
#    } elsif ($rin <=60 && $rin > 50) {
#	$rin = $rin + 12.0;
#    }
  }

  # format the name
  $cluster =~ s/\_/ /g;
  $cluster .= ' $\ddagger$' if ($obsid =~ /\+/);
  $obsid = sprintf("%4s",$obsid);
  $rin   = sprintf("%5i",$rin);
  $rout  = sprintf("%5i",$rout);
  $chisq = $chisq/$dof if ($chisq > 2);
  $chisq = sprintf("%3.2f",$chisq);
  $dof   = sprintf("%4i",$dof);
  $chisq2 = $chisq2/$dof2 if ($chisq2 > 2);
  $chisq2 = sprintf("%3.2f",$chisq2);
  $dof2   = sprintf("%4i",$dof2);
  $src   = sprintf("%3i",$src);

  # format nh
  if ($nhi == 0 && $nlo == 0) {
    $nh = sprintf("%-5.2f",$nh);
  } else {
    $nhi = sprintf("%-7.2f",$nhi - $nh);
    $nlo = sprintf("%-7.2f",$nh - $nlo);
    $nh = $nh . '$^{+' . $nhi . '}_{-' . $nlo . '}$ ';
  }

  # format the temperature
  $tx  = sprintf("%-7.2f",$tx);
  $thi = sprintf("%-7.2f",$txhi - $tx);
  $tlo = sprintf("%-7.2f",$tx  - $txlo);
  $thi = '\infty' if ($txhi > 30);
  $tx2  = sprintf("%-7.2f",$tx2);
  $t2hi = sprintf("%-7.2f",$tx2hi - $tx2);
  $t2lo = sprintf("%-7.2f",$tx2  - $tx2lo);
  $t2hi = '\infty' if ($tx2hi > 30);
  $tfrac  = sprintf("%-7.2f",$tfrac);
  $tfrachi = sprintf("%-7.2f",$tfrachi);
  $tfraclo = sprintf("%-7.2f",$tfraclo);

  # format the normalization
  $nhi = $normhi - $norm;
  $nlo = $norm - $normlo;
  $n2hi = $norm2hi - $norm2;
  $n2lo = $norm2 - $norm2lo;
  if ($nhi gt $nlo) {
    $signorm = sprintf("%-7.2e",$nhi);
  } else {
    $signorm = sprintf("%-7.2e",$nlo);
  }
  if ($n2hi gt $n2lo) {
    $signorm2 = sprintf("%-7.2e",$n2hi);
  } else {
    $signorm2 = sprintf("%-7.2e",$n2lo);
  }

  # assign t1, t2, norm1, and norm2 their +/- values
  $t  = $tx  . '$^{+' . $thi . '}_{-' . $tlo . '}$ ';
  $t2 = $tx2 . '$^{+' . $t2hi . '}_{-' . $t2lo . '}$ ';
  $tf = $tfrac . '$^{+' . $tfrachi . '}_{-' . $tfraclo . '}$ ';
  $n  = sprintf("%-7.2e",$norm);
  $n2 = sprintf("%-7.2e",$norm2);
  $n  = $norm  . '$^{+' . $nhi . '}_{-' . $nlo . '}$ ';
  $n2 = $norm2 . '$^{+' . $n2hi . '}_{-' . $n2lo . '}$ ';

  # set zeroes to appear as "--" if no Tx2 or norm2
  $t2 = " " if ($tx2 == 0);
  $n2 = " " if ($tx2 == 0);

  # format abundance
  if ($fehi != 0) {
    $fehi = sprintf("%-7.2f",$fehi - $fe);
    $felo = sprintf("%-7.2f",$fe   - $felo);
    $fe = $fe . '$^{+' . $fehi . '}_{-' . $felo . '}$ ';
  }
  if ($fe2hi != 0) {
    $fe2hi = sprintf("%-7.2f",$fe2hi - $fe2);
    $fe2lo = sprintf("%-7.2f",$fe2   - $fe2lo);
    $fe2 = $fe2 . '$^{+' . $fe2hi . '}_{-' . $fe2lo . '}$ ';
  }

  # make hash to hold values
  %values = (
	     "Cluster" => $cluster,
	     "ObsID" => $obsid,
	     "rin"   => $rin,
	     "rout"  => $rout,
	     "nh20"  => $nh,
	     "T77"    => $t,
	     "T27"    => $t2,
	     "Tfrac"  => $tf,
	     "Fe77"    => $fe,
	     "Fe27"    => $fe2,
	     "z"     => $z,
	     "Norm77"  => $n,
	     "Norm27"  => $n2,
	     "sigNorm77" => $signorm,
	     "sigNorm27" => $signorm2,
	     "Cr77"    => $cr,
	     "Cr27"    => $cr2,
	     "chisq77" => $chisq,
	     "dof77"   => $dof,
	     "chisq27" => $chisq2,
	     "dof27"   => $dof2,
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
    $name = join "_", $data[0],$data[1];
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
	      "Cluster" => "Cluster",
	      "ObsID"   => "Obs.ID",
	      "rin"     => 'R$_{\mathrm{CORE}}$',
	      "rout"    => 'R$_{'.$aptex.'}$ ',
	      "nh20"    => 'N$_{HI}$',
	      "T77"     => 'T$_{77}$',
	      "T27"     => 'T$_{27}$',
	      "Tfrac"   => 'T$_{HBR}$',
	      "Fe77"    => 'Z$_{77}$',
	      "Fe27"    => 'Z$_{27}$',
	      "z"       => "z",
	      "Norm77"  => '\$K_{77}\$',
	      "Norm27"  => '\$K_{27}\$',
	      "sigNorm77" => '$\sigma_{K,77}$',
	      "sigNorm27" => '$\sigma_{K,27}$',
	      "Cr77"    => 'c.r.$_{77}$',
	      "Cr27"    => 'c.r.$_{27}$',
	      "chisq77" => '$\chi^2_{red,77}$',
	      "chisq27" => '$\chi^2_{red,27}$',
	      "dof77"   => 'd.o.f$_{77}$',
	      "dof27"   => 'd.o.f$_{27}$',
	      "src"     => "\\% Source"
	     );

  %units = (
	    "Cluster" => " ",
	    "ObsID"   => " ",
	    "rin"     => "kpc",
	    "rout"    => "kpc",
	    "nh20"    => "\$10^{20}\$ cm\$^{-2}\$",
	    "T77"     => "keV",
	    "T27"     => "keV",
	    "Tfrac"   => " ",
            "Fe77"    => "Z\$\_\{\\sun\}\$",
            "Fe27"    => "Z\$\_\{\\sun\}\$",
	    "z"       => " ",
	    "Norm77"  => " ",
	    "Norm27"  => " ",
	    "sigNorm77" => " ",
	    "sigNorm27" => " ",
	    "Cr77"    => "\$s^{-1}\$",
	    "Cr27"    => "\$s^{-1}\$",
	    "chisq77" => " ",
	    "dof77"   => " ",
	    "chisq27" => " ",
	    "dof27"   => " ",
	    "src"     => " "
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

#######################
#######################

sub print_footer{
  print OUTFILE
    '\enddata',"\n",
      '\tablecomments{'.$tabcomments.'}',"\n",
        '\end{deluxetable}',"\n",
#          '\end{landscape}',"\n",
#            '\end{document}',"\n";
}

#######################
#######################
