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
die "# ERROR: Usage: prepare_arxiv.pl [-x] [-r #] [-t] <filename>\n" if ($#ARGV < 0);

# read which options are set
if (defined $options{r}) {
    $resolution = $options{r};
    print "# Using resolution = $resolution\n";
} else {
    $resolution = 150;
}

# read the input tex file
$inputtex = $ARGV[0];
$inputtex .= ".tex" unless ($inputtex =~ /\.tex$/);
print "# Input file is $inputtex\n";
die "# ERROR: Could not find that input file!\n" unless (-e $inputtex);

# create dir to story new files
mkdir("arxiv",0777) unless (-d "arxiv");

# open the tex file
open(IN,$inputtex) || die "# ERROR: $!";
open(OUT,">arxiv/arxiv.tex") || die "# Cannot open output file.\n";

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
system("cp $bblname arxiv/arxiv.bbl");
print "# Output written to arxiv/arxiv.tex\n";

# check the size of the output for arxiv mode
$size = (split(/\s+/,`du -sk arxiv`))[0];
print "# Total size of arxiv directory:  $size kB\n\n";
if ($size >= 1024) {print "**** Warning! Size > 1 MB! ****\n"; }

# tar-up the files if required
if (defined $options{t}) {
    system('tar -cvzf arxiv.tar.gz arxiv');
    $size = (split(/\s+/,`ls -l arxiv.tar.gz`))[4];
    $size = int($size/1024);
    system("mv arxiv.tar.gz arxiv/");
    print "# Created arxiv.tar.gz with $size kB\n";
}

# exit cleanly
exit 0;

###########################
###########################

sub prepare_figure {
    my ($fn);
    $fn = shift;
    print "# Output figure: $fn => arx_$fn\n";
    die "I cannot find the file $fn!" unless (-e $fn);
    system("convert -density 400 $fn arxiv/junk$figcounter.jpg");
    system("jpeg2ps arxiv/junk$figcounter.jpg > arxiv/arx_$fn");
    system("rm arxiv/junk$figcounter.jpg");
    $figcounter++;
}

###########################
###########################
