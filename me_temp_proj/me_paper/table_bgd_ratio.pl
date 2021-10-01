#!/usr/bin/perl -w

# This script takes the XSPEC fit results .dat file
# and makes a latex table for either 1T or 2T fits
# to use this script:
# make_table.pl <location of .dat file> <output .tex filename> <mode>

# reference array for all chip ids
%chip_id = ("0"=>"i0", "1"=>"i1", "2"=>"i2", "3"=>"i3",
	     "4"=>"s0", "5"=>"s1", "6"=>"s2", "7"=>"s3", "8"=>"s4","9"=>"s5");

@items = ("cluster","obsid","bgccd","obsdeep");
$tabcolumns = 'lcccc';
$caption = "Summary of Background Rates" . '\label{tab:tablebdgrates}';
$tabcomments = 'cluster, obsid, bgccd, obsdeep';

# read in file
open(INFILE,$ARGV[0]) || die "Can't open $ARGV[0] for reading\n";
@lines = <INFILE>;

# open latex file
open(OUTFILE,">$ARGV[1]") || die "Can't open $ARGV[1] for writing\n";

# print header information
print_header();

foreach $line (@lines) {

  # check for blank lines and comments
  next if ($line =~ /^\#/);
  next if ($line =~ /^$/);

  ($cluster,$obsid,$bgccd,$obscts,$deepcts,$obsqui,$deepqui,$obsdeep) = split(" ",$line);

  # format the name
  $cluster =~ s/\_/ /g;

  # format values
  $obsid   = sprintf("%4s",$obsid);
  $bgccd   = sprintf("%2s",$chip_id{$bgccd});
  $obscts  = sprintf("%5.4s",$obscts);
  $deepcts = sprintf("%5.4s",$deepcts);
  $obsqui  = sprintf("%5.4s",$obsqui);
  $deepqui = sprintf("%5.4s",$deepqui);
  $obsdeep = sprintf("%5.4s",$obsdeep);


  # make hash to hold values
  %values = (
	     "cluster" => $cluster,
	     "obsid"   => $obsid,
	     "bgccd"   => $bgccd,
	     "obscts"  => $obscts,
	     "deepcts" => $deepcts,
	     "obsqui"  => $obsqui,
	     "deepqui" => $deepqui,
	     "obsdeep" => $obsdeep
	    );

  @out = @values{@items};
  print OUTFILE join(" & ",@out),"\\\\\n";
}

print_footer();

############################################
############################################

sub print_colhead {
  my ($line,$out);
  @out = @_;
  foreach $out (@out) {
    $out = '\colhead{' . $out . '}';
  }
  $line = join(' & ',@out);
  return $line;
}

############################################
############################################

sub print_header {

  # define some useful hashes
  %colhead = (
	      "cluster" => "Name",
	      "obsid" => "Obs.ID",
	      "bgccd" => "ACIS",
	      "obscts" => "Obs. Cts",
	      "deepcts" => "Deep Cts",
	      "obsqui" => "Obs./Qui.",
	      "deepqui" => "Deep/Qui.",
	      "obsdeep" => "Obs./Bgd. Rate"
	     );

  %units = (
	      "cluster" => " ",
	      "obsid" => " ",
	      "bgccd" => " ",
	      "obscts" => "counts",
	      "deepcts" => "counts",
	      "obsqui" => " ",
	      "deepqui" => " ",
	      "obsdeep" => " "
	   );

  # make first header row
  @header1 = @colhead{@items};
  @header2 = @units{@items};

  # make row of column numbers
  $nhead = @header1;
  @header3 = map { '{(' . $_ . ')}'} (1..$nhead);

  # print the table header
  print OUTFILE (
		 '\documentclass{article}',"\n",
		 '\usepackage{deluxetable}',"\n",
		 '\begin{document}',"\n",
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

############################################
############################################

sub print_footer{
  print OUTFILE
    '\enddata',"\n",
      '\tablecomments{'.$tabcomments.'}',"\n",
	'\end{deluxetable}',"\n",
	  '\end{document}',"\n";
}
