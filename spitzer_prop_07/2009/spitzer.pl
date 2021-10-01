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

# read in radio values
open(SPZ,"spitzer.summary");
$i = 0;
$irac = 'N';
$mips = 'N';
$irs  = 'N';
$obz  = 'NONE';
while(<SPZ>){
  chomp;
  next if (/^\#/);
  next if (/^$/);
  s/^\s+//;
  s/\s+$//;
  chomp($input = $_);
  if ($input =~ /^Search/) {
    if ($i > 0) {
      $out = join(':',$irac,$mips,$irs,$obz);
      $spz{$name} = $out;
      $irac = 'N';
      $mips = 'N';
      $irs  = 'N';
      $obz  = 'NONE';
    }
    @line = split(': ',$_);
    @line = split(',',$line[1]);
    @line = split(/\s+/,$line[0]);
    $name = join('_',@line);
    $i++;
  }
  $irac = 'Y' if ($input =~ /IRAC/);
  $mips = 'Y' if ($input =~ /MIPS/);
  $irs  = 'Y' if ($input =~ /IRS/);
  $obz  = 'Y' if ($input =~ m/IRAC|MIPS|IRS/);
  if ($input =~ /^\d+/) {
    @line = split(/\s+/,$input);
    push @iracpids, $line[0] if ($input =~ /IRAC/);
    push @mipspids, $line[0] if ($input =~ /MIPS/);
    push @irspids, $line[0] if ($input =~ /IRS/);
  }
}
close SPZ;
$out = join(':',$irac,$mips,$irs,$obz);
$spz{$name} = $out;

# do everything else
open(IN,$ARGV[0]);
open(A,">spitzer.list");
open(B,">spitzer.table");
print A "COORD_SYSTEM: Equatorial    \# Equatorial, Galactic, or Ecliptic\n";
print A "EQUINOX: J2000              \# B1950, J2000, or blank for Galactic\n";
print A "NAME-RESOLVER: NED          \# Name resolution by either NED or SIMBAD\n";
print A "RADIUS: 5\n";
printf A "%-20s %18s %18s\n","#Name","RA","Dec";
printf B "%-20s %18s %18s %8s %8s %8s %8s %10s %10s %6s %10s %6s %5s %5s %5s %7s\n",
  "#Name","RA","Dec","z","TX","K0itpl","K0flat","LX","LHa","Type","LRad","Type","IRAC","MIPS","IRS","Data?";
printf B "%-20s %18s %18s %8s %8s %8s %8s %10s %10s %6s %10s %6s %5s %5s %5s %7s\n",
  "#---","h:m:s","d:m:s","---","keV","keV cm^2","keV cm^2","ergs/s","ergs/s","---","ergs/s","---","---","---","---","---";
$oldname = 'dsdsd';
while(<IN>){
  chomp;
  next if (/\#/);
  @line = split;
  $name = $line[0];
  $name =~ s/\_dag//;
  next if ($name eq $oldname);
  $ra = $line[17];
  $dec = $line[18];
  $ra =~ s/:/ /g;
  $dec =~ s/:/ /g;
  $z = $line[6];
  $tx = $line[8];

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

  # get obs values
  if (exists $spz{$name}) {
      @inst = split(/:/,$spz{$name});
  } else {
      @inst = ("?","?","?","?");
  }

  # get K0 value or else
  @a = glob("$myhome/research/pf_clusters/pf_fits/s_results/$line[1]_*");
  @a = glob("$myhome/research/pf_clusters/pf_fits/s_results/*_$line[1]_*") unless (@a);
  if (@a) {
    open(C,"$a[0]");
    undef @k0;
    while(<C>){
      chomp;
      next if (/\#/);
      @b = split;
      push @k0, $b[6];
    }
    close C;
  } else {
    @k0 = (-99.99,-99.99,-99.99,-99.99);
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
  printf A "%-20s %18s %18s\n","\"".$name."\"","\"".$ra."\"","\"".$dec."\"";
  $ra =~ s/ /:/g;
  $dec =~ s/ /:/g;
  printf B "%-20s %18s %18s %8.4f %8.2f %8.2f %8.2f %10.2e %10.2e %6s %10.2e %6s %5s %5s %5s %7s\n",
    $name,$ra,$dec,$z,$tx,$k0[0],$k0[2],$lx,$halum,$hatype,$radlum,$radtype,$inst[0],$inst[1],$inst[2],$inst[3];
  $oldname = $name;
}
close A;
close B;
undef %saw;
@ipids = grep(!$saw{$_}++, @iracpids);
undef %saw;
@spids = grep(!$saw{$_}++, @irspids);
undef %saw;
@mpids = grep(!$saw{$_}++, @mipspids);
@ipids = sort {$a <=> $b} @ipids;
@spids = sort {$a <=> $b} @spids;
@mpids = sort {$a <=> $b} @mpids;
open(TEMP1,">irac");
printf TEMP1 "%-12s\n","IRAC";
foreach $pid (@ipids) {
  printf TEMP1 "%-12s\n",$pid;
}
close TEMP1;
open(TEMP2,">mips");
printf TEMP2 "%-12s\n","MIPS";
foreach $pid (@mpids) {
  printf TEMP2 "%-12s\n",$pid;
}
close TEMP3;
open(TEMP3,">irs");
printf TEMP3 "%-12s\n","IRS";
foreach $pid (@spids) {
  printf TEMP3 "%-12s\n",$pid;
}
close TEMP3;
system("paste mips irs irac > spitzer.pids ; rm -f mips irac irs");
exit 0;

