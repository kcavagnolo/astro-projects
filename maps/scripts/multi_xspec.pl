#!/usr/bin/perl -w
#
# NAME:
#     subrot_xspec.pl
#
# PURPOSE:
#     Fit XSPEC models to spectra extracted from Chandra data.
#     The currently accepted models are MEKAL
#
# EXPLANATION:
#
# CALLING SEQUENCE:
#     subrot_xspec.pl @passdata
#
# INPUTS:
#     xxx.pi file = the spectral data needed for fitting
#     xxx.arf or xxx.warf = the Auxiliary Response file either weighted or not
#     xxx.rmf or xxx.wrmf = the Redistribution Matrix file either weighted or not
#
# OUTPUTS:
#
# MODIFICATION HISTORY:
#
#######################
#######################
##   Main Program    ##
#######################
#######################

# read in the passed "reference file"
@data       = @ARGV;
$name       = $data[0];
$obsid      = "$data[1]_$data[2]_$data[3]_$data[4]";
$z          = $data[9];
$nh         = $data[10]*1e-2;
$tx         = $data[11];
$fe         = $data[12];
$root       = $data[19];
$locat      = $data[20];
$model      = $data[21];
$abund      = $data[22];
$freeze_nh  = $data[23];
$freeze_fe  = $data[24];
$wordy      = $data[25];
$makegif    = $data[26];
$timeout    = $data[27];
$conlevel   = $data[28];
$emin       = $data[29];
$emax       = $data[30];
$crmin      = $data[31];
$crmax      = $data[32];
$nhmod      = $data[33];
$syserr     = $data[34];
$call       = $data[35];
$stat       = $data[36];
$ntrial     = $data[37];
$toler      = $data[38];
$origin     = $data[39];
$outfile    = $data[40];

# define the state name
$statname = "chisq" if ($stat eq "chi");
$statname = "cash"  if ($stat eq "cstat");
$statname = "bayes" if ($stat eq "lstat");

# make perl STFU about call not being used
undef $call;

# open up a logfile
open(LOGFILE,">>fit_spectra.log") || die "## ERROR: Cannot open fit_spectra.log\n";
print LOGFILE "######Starting fitting ",scalar(localtime),"######\n";

# change directory
chdir("$locat");
unless (-e "${outfile}") {
    open(FITFILE,">${outfile}");
    print_info();
    close FITFILE;
}

# xcmfile to create and run
$xcmfile = "${root}.xcm";

# get spectra
if ($statname eq "cash") {
    @spex = glob("*${root}_src1.pi");
} else {
    @spex = glob("*${root}_src1_grp.pi");
}
$len = $#spex;

# print model to XCMFILE, get parameters for con limit determination
@clpar = make_xcmfile($nhmod, $model, $xcmfile);

# run xspec (see subroutine run_xspec) with a timeout
do_xspec($xcmfile,@clpar);

# format output and put into results file
format_output(@clpar);

# close files and exit
print LOGFILE "######Finished fitting ",scalar(localtime),"######\n";
close(LOGFILE);


#######################
#######################
##   Sub-Routines    ##
#######################
#######################


sub make_xcmfile {

  # input vaules
  my($nhmod,$model,$xcmfile) = @_;

  # other local variables
  my(@clpar,$modstring,$line,@conpar);

  # clean up any files left over from previous runs
  unlink "$xcmfile";
  unlink <*.fit> ;
  unlink "model.dat";

  # print to XCM file
  open(XCMFILE,">$xcmfile");
  print XCMFILE "query yes\n";
  print XCMFILE "cosmo 70 0.3 0.7\n";
  print XCMFILE "data ";
  for ($i=0; $i <= $len; $i++) {
      $dum = $i+1;
      print XCMFILE "${dum}:${dum} ".$spex[$i]." ";
      print "## Loaded $spex[$i]\n";
  }
  print XCMFILE "\n";
  $dum = $len+1;
  print XCMFILE "ignore bad\n";
  print XCMFILE "ignore 1-$dum:**-${emin} ${emax}-**\n";
  print XCMFILE "statistic ${stat}\n";

  # MEKAL model
  if ($model eq "mekal") {
      ($nhpar,$txpar,$fepar,$normpar) = (1,2,4,7);
      $modstring = "${nhmod}(mekal)";
      $line = "& $nh & $tx & 1.0 & $fe,0 & $z & 0 & 0.1";
      for ($i=0; $i <= $len-1; $i++) {
	  $line .= " & =1 & =2 & =3 & =4 & =5 & =6 & 0.1";
      }
      $line .= "\n";
  }
  # MEKAL2T model
  elsif ($model eq "mekal2t") {
      ($nhpar,$txpar,$fepar,$normpar,$txpar2,$normpar2) = (1,2,4,7,8,13);
      $modstring = "${nhmod}(mekal+mekal)";
      $line = "& $nh & $tx & 1.0 & $fe,0 & $z & 0 & 0.1 & 1.0 & 1.0 & =4 & =5 & 0 & 0.1";
      for ($i=0; $i <= $len-1; $i++) {
	  $line .= " & =1 & =2 & =3 & =4 & =5 & =6 & 0.1 & =8 & =9 & =10 & =11 & =12 & 0.1";
      }
      $line .= "\n";
  }
  # MEKAL+APEC/b
  elsif ($model eq "mekal+apec/b") {
      ($nhpar,$txpar,$fepar,$normpar) = (1,2,4,7);
      $modstring = "${nhmod}(mekal)+apec/b";
      $line = "& $nh & $tx & 1.0 & $fe,0 & $z & 0 & 0.1 & $softx[0],0 & $soffe[0] & 0 & $snorm[0],0";
      for ($i=0; $i <= $len-1; $i++) {
	  $dum = $i+1;
	  $line .= " & =1 & =2 & =3 & =4 & =5 & =6 & 0.1 & $softx[$dum],0 & $soffe[$dum] & 0 & $snorm[$dum],0";
      }
      $line .= "\n";
  }
  # MEKAL+MEKAL/b
  elsif ($model eq "mekal+mekal/b") {
      ($nhpar,$txpar,$fepar,$normpar) = (1,2,4,7);
      $modstring = "${nhmod}(mekal)+mekal/b";
      $line = "& $nh & $tx & 1.0 & $fe,0 & $z & 0 & 0.1 & $softx[0],0 & 1.0 & $soffe[0] & 0 & 0 & $snorm[0],0";
      for ($i=0; $i <= $len-1; $i++) {
	  $dum = $i+1;
	  $line .= " & =1 & =2 & =3 & =4 & =5 & =6 & 0.1 & $softx[$dum],0 & 1.0 & $soffe[$dum] & 0 & 0 & $snorm[$dum],0";
      }
      $line .= "\n";
  }
  else {
      die "Unknown model: $model\n";
  }

  # print out model
  print XCMFILE "model $modstring $line\n" ;
  print XCMFILE "abund $abund\n";
  print XCMFILE "freeze $nhpar\n" if ($freeze_nh eq "yes");
  print XCMFILE "thaw $nhpar\n" if ($freeze_nh eq "no");
  print XCMFILE "freeze $fepar\n" if ($freeze_fe eq "yes");
  print XCMFILE "thaw $fepar\n" if ($freeze_fe eq "no");
  print XCMFILE "systematic $syserr\n" unless ($syserr == 0);

  # xspec no understand neg. norm
  if ($model =~ /apec\/b/ || $model =~ /mekal\/b/) {
      $snum = 0;
      foreach $s (@snorm) {
          if ($s < 0.00) {
              if ($model =~ /apec\/b/) {
                  $parnum = 11+(11*$snum);
                  $parnum = sprintf("%i",$parnum);
                  print XCMFILE "newpar $parnum 1 $s $s $s $s $s\n";
              } elsif ($model =~ /mekal\/b/) {
                  $parnum = 13+(13*$snum);
                  $parnum = sprintf("%i",$parnum);
                  print XCMFILE "newpar $parnum 1 $s $s $s $s $s\n";
              } else {
                  die "## ERROR: what in the $s is this!\n";
              }
          }
          $snum++;
      }
  }
  close XCMFILE;

  # get the parameters to get con limits for
  @clpar = ();
  push @clpar,$nhpar if ($freeze_nh eq "no");
  push @clpar,$txpar;
  push @clpar,$normpar;
  push @clpar,$fepar if ($freeze_fe eq "no");
  if ($model eq "mekal2t") {
      push @clpar,$txpar2;
      push @clpar,$normpar2;
  }

  # return the parameters you want confidence limits for
  return @clpar;
}

#######################
#######################

sub myprint {

  my $line;
  foreach $line (@_) {
    print $line if ($wordy > 1);
    print XSPEC_INPUT $line;
  }
}

#######################
#######################

sub run_xspec {

  my ($xcmfile,@parameters) = @_;
  my ($parameters,$line,$done,$conlim,$params);
  undef %low;
  undef %high;

  # starts xspec (perl and read and write from/to it)
  # $pid is a global variable that can be used to kill xspec
  use IPC::Open3;
  $pid = open3(*XSPEC_INPUT,*XSPEC_OUTPUT,*XSPEC_ERR,"xspec11") ||
      die "## ERROR: Cannot run xspec11\n";

  # open the xcmfile written earlier and feed to xspec
  myprint "\@${xcmfile}\n";

  # delete files
  unlink( ("${xcmfile}.model"));
  unlink("${xcmfile}.log");

  # fit the data
  myprint "fit 1000 0.001\n" ;

  # get the confidence limits for interesting parameters
  $parameters = join " ", @parameters;
  myprint "query no\n";
  myprint "error stopat $ntrial $toler maximum 10.0 $conlevel $parameters\n";

 MAIN: while (<XSPEC_OUTPUT>) {
    chomp($line = $_);
    print "xspec: $line\n" if ($wordy > 1);

    # chisq to high can't calculate errors
    if ($line =~ /^.*?\s+\> maximum/) {
      print "## $obsid: $statname too large to estimate confidence intervals\n";
      print LOGFILE "$obsid: $statname too large to estimate confidence intervals\n";

      # set errors to zero
      foreach $parameter (@parameters) {
	$low{$parameter} = 0;
	$high{$parameter} = 0;
      };
		
      # write things to a log file
      myprint
	  "save model ${xcmfile}.model\n",
	  "log $xcmfile.log\n",
	  "show fit\n",
	  "notice **-**\n",
	  "ignore **-$crmin $crmax-**\n",
	  "tclout rate\n",
	  'puts  "Rate $xspec_tclout"',"\n",
	  "log none\n"
	  ;
	
      # all done quit XSPEC
      myprint "exit\n\n";
    }

    # err in fitting, can't calculate errors
    if ($line =~ /sigma indicates possible error/) {
      print "## $obsid: no fit, confidence intervals unreal\n";
      print LOGFILE "## $obsid: no fit, confidence intervals unreal\n";

      # set errors to zero
      foreach $parameter (@parameters) {
	$low{$parameter} = 0;
	$high{$parameter} = 0;
      };

      # write things to a log file
      myprint
	  "save model ${xcmfile}.model\n",
	  "log $xcmfile.log\n",
	  "show fit\n",
	  "notice **-**\n",
	  "ignore **-$crmin $crmax-**\n",
	  "tclout rate\n",
	  'puts  "Rate $xspec_tclout"',"\n",
	  "log none\n";

      # all done quit XSPEC
      myprint "exit\n\n";
    }

    # error calculation actually works
    if ($line =~ m/^.*?\s+Parameter\s+Confidence/) {
      $done=0;
      # what to do in various cases, need to update
      while ( $done < @parameters) {
	chomp ($line=<XSPEC_OUTPUT>);
	print "xspec: $line\n" if ($wordy > 1);

	# error found a new minimum
	if ($line =~ m/when model parameter/) {
	  print LOGFILE "$name $obsid: check fit\n";
	  myprint "fit 100000 .001\n" ;
	  myprint "fit 100000 .01\n" ;
	  myprint "error stopat 100 0.0001 maximum 10.0 $conlevel $parameters\n";
	  next MAIN;
	}

	# error worked, get output
	if ($line =~ m/^\s+\d+\s+/) {
	  ($parameter, $low, $high) = split(" ",$line);
	  $done++;
	  # put into a hash
	  $low{$parameter} = $low;
	  $high{$parameter} = $high;
	}
      }

      # make a gif image of the spectrum
      if ($makegif eq "yes") {
	  unlink "${root}.spec.ps";
	  myprint "setplot rebin 10 10\n" if ($statname eq "cash");
	  myprint
	      "setplot energy\n",
	      "iplot ldata resid\n",
	      "label top $name $obsid $root\n",
	      "time off\n",
	      "cpd ${root}.spec.ps/vcps\n",
	      "plot\n",
	      "cpd /null\n",
	      "exit\n";
	  open(LIST,">>list");
	  print LIST "${root}.spec.ps\n";
	  close LIST;
      }

      # write things to a log file
      myprint
	  "save model ${xcmfile}.model\n",
	  "log $xcmfile.log\n",
	  "show fit\n",
	  "notice **-**\n",
	  "ignore **-$crmin $crmax-**\n",
	  "tclout rate\n",
	  'puts  "Rate $xspec_tclout"',"\n",
	  "log none\n"
	  ;

      # all done quit XSPEC
      myprint "quit\ny\n";
    }
  }

  close XSPEC_INPUT;
  close XSPEC_OUTPUT;
  close XSPEC_ERR;
  waitpid($pid,0);
}

#######################
#######################

sub format_output {

  my(@parameters) = @_;
  my($nh,$tx,$fe,$norm,$cr,$nlo,$nhi,$felo,$fehi,$tlo,$thi,$tx2,$norm2,$tlo2,$thi2,@data);

  # get model parameter values
  # go through model file produced by xspec sub
  open(MODFILE,("$xcmfile.model")) || die "## ERROR: Cannot open $xcmfile.model\n";
  @model = <MODFILE>;
  @model = splice(@model,6);
  $mcount = 1;
  foreach $modparam (@model) {	
    @moddata = split(" ",$modparam);
    $nh   = $moddata[0] if ($mcount == $nhpar);
    $tx   = $moddata[0] if ($mcount == $txpar);
    $fe   = $moddata[0] if ($mcount == $fepar);
    $norm = $moddata[0] if ($mcount == $normpar);
    if ($model eq "mekal2t") {
      $tx2  = $moddata[0] if ($mcount == $txpar2);
      $norm2 = $moddata[0] if ($mcount == $normpar2);
    }
    $mcount++;
  }

  # get model parameter errors
  # go through parameters variable input from run_xspec
  if (defined $low{$nhpar}) {
    ($nlo,$nhi) = ($low{$nhpar}, $high{$nhpar});
  } else {
    ($nlo,$nhi) = (0,0);
  }
  if (defined $low{$txpar}) {
    ($tlo,$thi) = ($low{$txpar}, $high{$txpar});
  } else {
    ($tlo,$thi) = (0,0);
  }
  if (defined $low{$normpar}) {
    ($normlo,$normhi) = ($low{$normpar}, $high{$normpar});
  } else {
    ($normlo,$normhi) = (0,0);
  }
  if (defined $low{$fepar}) {
    ($felo,$fehi) = ($low{$fepar}, $high{$fepar});
  } else {
    ($felo,$fehi) = ($fe,$fe);
  }

  # handle the mekal2t models params
  if ($model eq "mekal2t") {
    if (defined $low{$txpar2}) {
      ($tlo2,$thi2) = ($low{$txpar2}, $high{$txpar2});
    } else {
      ($tlo2,$thi2) = (0,0);
    }
    if (defined $low{$normpar2}) {
      ($normlo2,$normhi2) = ($low{$normpar2}, $high{$normpar2});
    } else {
      ($normlo2,$normhi2) = (0,0);
    }
  } else {
    ($tx2,$tlo2,$thi2) = (0,0,0);
    ($norm2,$normlo2,$normhi2) = (0,0,0);
  }

  # go through log file produced by xspec sub
  open(MODFILE,"$xcmfile.log") || die "## ERROR: Cannot open $xcmfile.log\n";
  my(@dof) = ();
  my(@stats) = ();
  if ($stat eq "chi") {
      $phrase = "Reduced chi-squared";
      $ind1 = 3;
      $ind2 = 5;
  } elsif ($stat eq "cstat") {
      $phrase = "C-statistic";
      $ind1 = 2;
      $ind2 = 4;
  } elsif ($stat eq "lstat") {
      $phrase = "L-statistic";
      $ind1 = 2;
      $ind2 = 4;
  }
  while (<MODFILE>) {
    if (/$phrase/) {
      @data = split(" ",$_);
      push @stats,sprintf("%7.2f",$data[$ind1]);
      push @dof,$data[$ind2];
    }
    if (/^Rate/) {
      @data = split(" ",$_);
      $cr = $data[1];
    }
    if (/Net count rate/) {
      @data = split("/",$_);
      $ptot = $data[2];
      $ptot =~ s/^.*\(//;
      $ptot =~ s/\%.*//;
      chomp $ptot;
    }
  }

  # print out the info
  open(FILE,">${root}.temp");
  printf FILE "%f\n",$tx;
  close FILE;

  open(FILE,">${root}.fe");
  printf FILE "%f\n",$fe;
  close FILE;

  open(FILE,">${root}.chi");
  printf FILE "%f\n",$stats[0];
  close FILE;

  open(FILE,">${root}.norm");
  printf FILE "%f\n",$norm;
  close FILE;

  open(FILE,">${root}.normerr");
  printf FILE "%f\n",$nlo;
  printf FILE "%f\n",$nhi;
  close FILE;

  open(FILE,">${root}.temperr");
  printf FILE "%f\n",$tlo;
  printf FILE "%f\n",$thi;
  close FILE;

  open(FILE,">${root}.feerr");
  printf FILE "%f\n",$felo;
  printf FILE "%f\n",$fehi;
  close FILE;

  # get region number
  if ($origin eq "wvt") {
      $rin = 000;
      @regnum = split("\\.",$root);
      $regnum = $regnum[3];
  } elsif ($origin eq "cbin") {
      $rin = 000;
      @regnum = split("\_",$root);
      $regnum = $regnum[1];
  } else {
      $regnum = -99;
      $rin = -99;
  }	  

  # print out the info
  open(FITFILE,">>${outfile}");
  printf FITFILE "%-25s %6s %6i %6.2f %5.2f %5.2f %5.2f %5.2f %5.2f %5.2f %5.2f %5.2f %5.2f %10.2e %10.2e %10.2e %5.2f %5.2f %5.2f %10.2e %10.2e %10.2e %7.4f %6.3f %6s %6.2f %6i\n",
  $name,"junk",$regnum,$rin,$nh*1e2,$nlo*1e2,$nhi*1e2,$tx,$tlo,$thi,$fe,$felo,$fehi,$norm,$normlo,$normhi,$tx2,$tlo2,$thi2,$norm2,$normlo2,$normhi2,$z,$cr,$ptot,$stats[0],$dof[0];
  close FITFILE
}

#######################
#######################

sub print_info {

    print FITFILE "# Origin: $origin; Model: $nhmod($model); Abund: $abund; Fit Range = $emin - $emax; Count Rates = $crmin - $crmax keV\n";
    print FITFILE "# Nh frozen = $freeze_nh; Fe frozen = $freeze_fe; Confidence Levels = $conlevel; Systematic error = $syserr\n";
    printf FITFILE "%-25s %6s %6s %6s %5s %5s %5s %5s %5s %5s %5s %5s %5s %10s %10s %10s %5s %5s %5s %10s %10s %10s %7s %6s %6s %6s %6s\n",
    "# Cluster","ObsID","Bin","Rin","nh20","nlo","nhi","Tx","Tlo","Thi","Fe","Felo","Fehi",
    "Norm","Normlo","Normhi","Tx2","Tlo2","Thi2","Norm2","Normlo2","Normhi2","z","Cr","Src\%",$statname,"dof";
}

#######################
#######################

sub do_xspec {

    $SIG{ALRM} = sub { die "## ERROR: Timeout" };
    eval {
	alarm($timeout);
	run_xspec(@_);
	alarm(0);
    };
    if ($@) { # trap the timeout
	if ($@ =~ /timeout/) {
	    print(LOGFILE "$name $obsid:  timed out\n");
	    kill 15, $pid;  # relies of $pid being a global variable
	    next;
	}
	else { # something else happended
	    print LOGFILE "$name $obsid exited early\n";
	    next;
	}
    }
}

#######################
#######################
