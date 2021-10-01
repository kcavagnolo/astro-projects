#!/usr/local/bin/perl
#
# Usage:
#       submit_prepare.pl [-x] [-r #] [-t] <filename>
#
# Options:
#       -x   prepare for ArXiV submission. This involves recompressing
#            all figures to a smaller size in order to meet the < 1MB limit.
#       -r   What resolution to use for JPEGs during ArXiV preparation.
#            Default is 150 dpi; make it smaller to shrink files more.
#       -t   automatically tar things up into submit.tar.gz
#
# Outputs:
#       A directory called "submit" which contains all the relevant stuff.
#
# Requirements:
#       jpeg2ps
#       gs
#
# History:
#
###########################
###########################

# load useful libraries
use Getopt::Std;
%options = ();
getopts("xr:t",\%options);

# check that # of params is correct
die "# ERROR: Usage: submit_prepare.pl [-x] [-r #] [-t] <filename>\n" if ($#ARGV < 0);

# read which options are set
if (defined $options{r}) {
  $resolution = $options{r};
  print "# Using resolution = $resolution\n";
} else {
  $resolution = 150;
}
$arxiv_mode = defined $options{x};

# read the input tex file
$inputtex = $ARGV[0];
$inputtex .= ".tex" unless ($inputtex =~ /\.tex$/);
print "# Input file is $inputtex\n";
die "# ERROR: Couldn't find that input file!\n" unless (-e $inputtex);

# create dir to story new files
mkdir("submit",0777) unless (-d "submit");

# open the tex file
open(IN,$inputtex) || die "# ERROR: $!";
open(OUT,">submit/ms.tex") || die "# Can't open output file.\n";

# start the loop
$figcounter = 1;
while(<IN>){
  s/^%.*\n|([^\\])%.*\n/$1/go;
  if (/\\plotone{([^\}]*)}/) {
    $fn = $1;
    s/$fn/f$figcounter.eps/;
    prepare_figure($fn);
  }
  if (/\\includegraphics(.*){([^\}]*)}/) {
    $args = $1;
    $fn = $2;
    prepare_figure($fn);
  }
  print OUT;
}

# copy the BBL file
$bblname = $inputtex;
$bblname =~ s/\.tex/\.bbl/;
system("cp $bblname submit/ms.bbl");
print "# Output written to submit/ms.tex\n";

# check the size of the output for arxiv mode
if ($arxiv_mode) {
  $size = (split(/\s+/,`du -sk submit`))[0];
  print "# Total size of submit directory:  $size kB\n\n";
  if ($size >= 1024) {print "**** Warning! Size > 1 MB! ****\n"; }
}

# tar-up the files if required
if (defined $options{t}) {
  system('tar cvzf submit.tar.gz submit');
  $size = (split(/\s+/,`ls -l submit.tar.gz`))[4];
  $size = int($size/1024);
  system("mv submit.tar.gz submit/");
  print "# Created submit.tar.gz with $size kB\n";
}

# exit cleanly
exit 0;

###########################
###########################

sub prepare_figure {
  my ($fn);
  $fn = shift;
  print "# Output figure: $fn => f$figcounter.eps\n";
  $fn .= ".eps" unless ($fn =~ /\.eps$/);
  die "I can't find the file $fn!" unless (-e $fn);
  if ($arxiv_mode) {
    system("gs -r$resolution -dEPSCrop -dTextAlphaBits=4 -sDEVICE=jpeg "
	   ."-sOutputFile=submit/f$figcounter.jpg -dBATCH -dNOPAUSE $fn");
    system("jpeg2ps submit/f$figcounter.jpg > submit/f$figcounter.eps");
    system("rm submit/f$figcounter.jpg");
    $oldsize = -s "$fn";
    $newsize = -s "submit/f$figcounter.eps";
    print "\tOld size:\t$oldsize\tNew size:\t$newsize\n";
    system("cp $fn.eps submit/f$figcounter.eps") if ($oldsize < $newsize);
  } else  {
    system("cp $fn submit/f$figcounter.eps");
  }
  $figcounter++;
}

###########################
###########################
