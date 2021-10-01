#! /usr/bin/perl -w
$myhome = $ENV{'HOME'};

# read in ha values
open(HA,"$myhome/research/pf_clusters/pf_fits/dat/pf_halum.dat");
while(<HA>){
  chomp;
  next if (/\#/);
  @a = split;
  $ha{$a[0]}=$_;
}
close HA;

# read in radio values
open(RAD,"$myhome/research/pf_clusters/pf_fits/dat/pf_radio.dat");
while(<RAD>){
  chomp;
  next if (/\#/);
  @a = split;
  $rad{$a[0]}=$_;
}
close RAD;

# read in lx values
open(LX,"$myhome/research/pf_clusters/pf_fits/dat/lx.dat");
while(<LX>){
  chomp;
  next if (/\#/);
  @a = split;
  $lbol{$a[0]}=$_;
}
close LX;

# read alt lx values
open(PF,"$myhome/research/pf_clusters/pf_info/pf_pars.dat");
while(<PF>){
  chomp;
  next if (/\#/);
  @a = split;
  $pf{$a[0]}=$_;
}
close LX;

# do everything else
open(IN,$ARGV[0]);
open(B,">master.table");
printf B "%-20s %15s %15s %8s %8s %8s %8s %8s %8s %8s %8s %10s %10s %6s %10s %6s\n",
    "#Name","RA","Dec","z","TX","K0","K0err","K100","K100err","alpha","aerr","LX","LHa","Type","LRad","Type";
printf B "%-20s %15s %15s %8s %8s %8s %8s %8s %8s %8s %8s %10s %10s %6s %10s %6s\n",
    "#--","h:m:s","d:m:s","--","keV","\\ent","\\ent","\\ent","\\ent","--","--","ergs/s","ergs/s","--","ergs/s","--";
$prevname = 'kjsfhksd';
while(<IN>){
  chomp;
  next if (/\#/);
  @line = split;
  $name = $line[0];
  $name =~ s/\_dag//;
  next if ($name eq $prevname);
  $ra = $line[17];
  $dec = $line[18];
  $ra =~ s/:/ /g;
  $dec =~ s/:/ /g;
  $z = $line[6];
  $tx = $line[8];
  $expt = $line[19];

#  next if ($z lt 0.45);

  # get LX values
  if ($line[10] <= 0.0) {
      if (exists $pf{$line[0]}) {
	  @data = split(/\s+/,$pf{$line[0]});
	  $lx = $data[7];
      } elsif (exists $lbol{$line[0]}) {
	  @data = split(/\s+/,$lbol{$line[0]});
	  $lx = $data[12];
	  $part = $lx-44;
	  $part = 10**$part;
	  $lx = $part*10**44;
      } else {
	  $lx = -1.00;
      }
  } else{
      $lx = $line[10]*10**44;
  }

  # get K0 fit values or else
  @a = glob("$myhome/research/pf_clusters/pf_fits/s_results/$line[1]_*");
  @a = glob("$myhome/research/pf_clusters/pf_fits/s_results/*_$line[1]_*") unless (@a);
  if (@a) {
    open(C,"$a[0]");
    undef @k0;
    undef @k0err;
    undef @k100;
    undef @k100err;
    undef @alpha;
    undef @alphaerr;
    undef @chisq;
    undef @prob;
    while(<C>){
      chomp;
      next if (/\#/);
      @b = split;
      push @k0, $b[6];
      push @k0err, $b[7];
      push @k100, $b[8];
      push @k100err, $b[9];
      push @alpha, $b[10];
      push @alphaerr, $b[11];
      push @chisq, $b[12];
      push @prob, $b[13];
    }
    close C;
  } else {
    @k0 = (-99.99,-99.99,-99.99,-99.99);
    @k0err = @k0;
    @k100 = @k0;
    @k100err = @k0;
    @alpha = @k0;
    @alphaerr = @k0;
    @chisq = @k0;
    @prob = @k0;
  }

  # get ha value or else
  if (exists $ha{$line[0]}) {
    @data = split(/\s+/,$ha{$line[0]});
    $halum = $data[6]*10**40;
    $hatype = $data[2];
  } else {
    $halum = -1.0;
    $hatype = "UK";
  }

  # get rad value or else
  if (exists $rad{$line[0]}) {
      @data = split(/\s+/,$rad{$line[0]});
      $radlum = $data[6]*10**40;
      $radtype = $data[2];
  } else {
      $radlum = -1.0;
      $radtype = "UK";
  }

  # print it all out
  $ra =~ s/ /:/g;
  $dec =~ s/ /:/g;
  $pname = $name;
#  $pname =~ s/\_/ /g;
  printf B "%-20s %15s %15s %8.4f %8.2f %8.2f %8.2f %8.2f %8.2f %8.2f %8.2f %10.2e %10.2e %6s %10.2e %6s\n",
  $pname,$ra,$dec,$z,$tx,$k0[2],$k0err[2],$k100[2],$k100err[2],$alpha[2],$alphaerr[2],$lx,$halum,$hatype,$radlum,$radtype;
  $prevname = $name;
}
close B;

# create a descending sorted table of K0
# Name = 0
# RA = 1
# Dec = 2
# z = 3
# TX = 4
# K0 = 5
# K0err = 6
# K100 = 7
# K100err = 8
# alpha = 9
# aerr = 10
# LX = 11
# LHa = 12
# Type = 13
# LRad = 14
# Type = 15

$column = 11;
open(A,"master.table");
open(B,">sorted_master.table");
while(<A>) {
    chomp;
    if (/\#/) {
	printf B "$_\n";
	next;
    }
    @a = split;
    push @sort_col, $a[$column];
    push @lines, "$_\n";
}
close A;
@sorted = @lines[sort { $sort_col[$a] <=> $sort_col[$b] }0..$#sort_col];
foreach $l (@sorted) {
    printf B "$l";
}
close B;
exit 0;
