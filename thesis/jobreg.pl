#! /usr/bin/perl -w

# set the AAS reg url, may change in time
$lastad = 27231;
$addr = 'http://members.aas.org/JobReg/JobDetailPage.cfm?JobID=';

# stuff
no warnings('once');
use Cwd;
use FindBin qw($Bin);

# open a log
open(LOG, ">AASJobReg.log");
printf LOG "%-10s %-120s %-20s\n",
  "#Num", "Name", "Due";

# check for wget
print "## STATUS: Checking for 'wget'\n\n";
system("wget -V");
if ($? == -1) {
    die "## ERROR: Program \'wget\' not on system.\n";
}
print "## STATUS: Found wget.\n\n";

# iterate over ad numbers
$begin = 'n';
$unpub = 'no';
$adnum = $lastad+1;
$iter = 0;

# begin loop until no ad found
while ($iter <= 5) {

  # get url
  @args = ("wget","-q","\'${addr}${adnum}\'","-Otemp.dat");
  $command = join(" ",@args);
  system($command);

  # open html file, stop after 5 blanks
  open(B,"temp.dat");
  while(<B>){
    chomp;

    # remove comment lines, blank lines, and white space
    next if (/\<\!/);
    next if (/^$/);
    s/^\s+//;
    s/\s+$//;

    #no title = no ad
    if (/\<title/) {
      @line = split;
      $a = join("",@line);
      $a =~ s/<.*?\-//g;
      $a =~ s/<.*?>//g;
      if ($a eq "") {
	print "## STATUS: Found possible end of ads at AdNum $adnum.\n";
	$iter++;
      } else {
	$iter = 0;
      }
    }

    # set when to start parsing content
    if (/\<div id=\"content/) {
      $begin = 'y';
      next;
    }

    # unpublished?
    $unpub = "y" if (/Warning/);

    # find the ad title
    if (/\<h1/ && $begin eq 'y') {
      @line = split;
      $a = join("",@line);
      $a =~ s/<.*?>//g;
      $a =~ s/,//g;
      $adname = $a;
    }

    # find due date
    if (/<p>.*closing date/) {
      @line = split;
      $a = join("",@line);
      $a =~ s/<.*?\://g;
      $a =~ s/<.*?>//g;
      $addate = $a;
    }
  }

  # clean-up
  close B;
  system("rm -f temp.dat");

  # print to logfile
  if ($unpub eq 'y') {
    printf LOG "%-10i %-120s %-20s\n",
      $adnum, $adname, $addate;
  }

  # advance ad num
  $adnum++;
  $unpub = 'no';
}

# amuse ken
print "Thank you, Satan >:)\n";

# close shop
exit 0;
