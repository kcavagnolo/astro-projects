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
$obsid      = $data[1];
$z          = $data[6];
$nh         = $data[7]*1e-2;
$tx         = $data[8];
$fe         = $data[9];
$root       = $data[16];
$pifile     = $data[17];
$locat      = $data[18];
$model      = $data[19];
$freeze_nh  = $data[20];
$freeze_fe  = $data[21];
$wordy      = $data[22];
$makegif    = $data[23];
$timeout    = $data[24];
$conlevel   = $data[25];
$emin       = $data[26];
$emax       = $data[27];
$crmin      = $data[28];
$crmax      = $data[29];
$nhmod      = $data[30];
$syserr     = $data[31];
$call       = $data[32];
$stat       = $data[33];
$ntrial     = $data[34];
$toler      = $data[35];
$origin     = $data[36];
$outfile    = $data[37];

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

# print model to XCMFILE, get parameters for con limit determination
@clpar = make_xcmfile($nhmod, $model, $pifile, $xcmfile);

# run xspec (see subroutine run_xspec) with a timeout
do_xspec($xcmfile,@clpar);

# format output and put into results file
format_output($pifile,@clpar);

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
  my($nhmod,$model,$file,$xcmfile) = @_;

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
  print XCMFILE "data $file\n";
  print XCMFILE "statistic ${stat}\n";
  print XCMFILE "ignore **-${emin} ${emax}-**\n";
  print XCMFILE "ignore bad\n";

  # Model information
  # Raymond-Smith #######################################################
  if ($model eq "ray") {
    ($nhpar,$txpar,$fepar,$normpar) = (1,2,3,5);
    $modstring = "${nhmod}(ray)";
    $line      = "& $nh & $tx & $fe,0 & $z & 0.1";
  }
  # MEKAL model#########################################################
  elsif ($model eq "mekal") {
    ($nhpar,$txpar,$fepar,$normpar) = (1,2,4,7);
    $modstring = "${nhmod}(mekal)";
    $line      = "& $nh & $tx & 1.0 & $fe,0 & $z & 0 & 0.1";
  }
  # APEC model##########################################################
  elsif ($model eq "apec") {
    ($nhpar,$txpar,$fepar,$normpar) = (1,2,3,5);
    $modstring = "${nhmod}(apec)";
    $line      = "& $nh & $tx & $fe,0 & $z & 0.1";
  }
  elsif ($model eq "mekal2t") {
    ($nhpar,$txpar,$fepar,$normpar,$txpar2,$normpar2) = (1,2,4,7,8,13);
    $modstring = "${nhmod}(mekal + mekal)";
    $line      = "& $nh & $tx & 1.0 & $fe,0 & $z & 0 & 0.1 & 0.1 & 1.0 & =4 & =5 & 0 & 0.01";
  }
  else {
    die "## ERROR: Unknown model -- $model\n";
  }

  # print out model
  print XCMFILE "model $modstring $line\n" ;
  print XCMFILE "freeze $nhpar\n" if ($freeze_nh eq "yes");
  print XCMFILE "thaw $nhpar\n" if ($freeze_nh eq "no");
  print XCMFILE "freeze $fepar\n" if ($freeze_fe eq "yes");
  print XCMFILE "thaw $fepar\n" if ($freeze_fe eq "no");
  print XCMFILE "systematic $syserr\n" unless ($syserr == 0);
  close XCMFILE;

  # get the parameters to get con limits for
  @clpar = ();
  push @clpar,$nhpar if ($freeze_nh eq "no");
  push @clpar,$txpar;
  push @clpar,$normpar;
  push @clpar,$fepar if ($freeze_fe eq "no");
  push @clpar,$txpar2 if ($model eq "mekal2t");

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
	  myprint "fit 1000 .001\n" ;
	  myprint "fit 1000 .01\n" ;
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

  my($file,@parameters) = @_;
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
      $regnum = $regnum[2];
  } else {
      $regnum = -99;
      $rin = -99;
  }	  

  # print out the info
  open(FITFILE,">>${outfile}");
  printf FITFILE "%-25s %6s %6i %6.2f %5.2f %5.2f %5.2f %5.2f %5.2f %5.2f %5.2f %5.2f %5.2f %10.2e %10.2e %10.2e %5.2f %5.2f %5.2f %10.2e %10.2e %10.2e %7.4f %6.3f %6s %6.2f %6i\n",
  $name,$obsid,$regnum,$rin,$nh*1e2,$nlo*1e2,$nhi*1e2,$tx,$tlo,$thi,$fe,$felo,$fehi,$norm,$normlo,$normhi,$tx2,$tlo2,$thi2,$norm2,$normlo2,$normhi2,$z,$cr,$ptot,$stats[0],$dof[0];
  close FITFILE
}

#######################
#######################

sub print_info {

  print FITFILE "# Origin: $origin; Model: $nhmod($model); Fit Range = $emin - $emax; Count Rates = $crmin - $crmax keV\n";
  print FITFILE "# Nh frozen = $freeze_nh; Fe frozen = $freeze_fe; Confidence Levels = $conlevel; Systematic error = $syserr\n";

  printf FITFILE "%-25s %6s %6s %6s %5s %5s %5s %5s %5s %5s %5s %5s %5s %10s %10s %10s %5s %5s %5s %10s %10s %10s %7s %6s %6s %6s %6s\n",
    "# Cluster","ObsID","Bin","Rin","nh20","nlo","nhi","Tx","Tlo","Thi","Fe","Felo","Fehi",
      "Norm","Normlo","Normhi","Tx2","Tlo2","Thi2","Norm2","Normlo2","Normhi2","z","Cr","Src\%",$statname,"dof";
}

#######################
#######################

sub nh {

  # uses nh ftool to return weighted average nh in units of 10^20 cm^-2
  # usage nh $ra $dec
  my($ra,$dec) = @_;
  my(@nh,$nh);

  #run nh command get get Weighted line & trim off newline
  chomp($_ = `nh 2000 $ra $dec | grep Weighted`);

  # trim whitescape and split
  s/^\s+//;
  s/\s+$//;
  @nh = split(/\s+/, $_);

  # get last value of array and print
  $nh = pop(@nh);
  return $nh/1e22 ;
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
