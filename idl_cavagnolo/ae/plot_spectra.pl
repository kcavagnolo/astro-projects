#!/usr/bin/perl
# $Id: plot_spectra.pl,v 1.2 2008-10-15 19:37:03 cavagnolo Exp $
# plot_spectra.pl - print 12 large .EPS images to a sheet with latex commands
#
# Usage: plot_spectra.pl 182*/grp0_wabs_apec_cstat_3free.ps
#

@epsfile = (@ARGV);
$i = 0;
open (FOO, ">spectra.tex") || die "Can't open spectra.tex: $!\n";
printf STDERR "opened file spectra.tex\n";
printf FOO "% Latex form to plot multiple figures per page\n";
printf FOO "% $Id: plot_spectra.pl,v 1.2 2008-10-15 19:37:03 cavagnolo Exp $\n\n";

# We need the aastex class to get includegraphics below to not complain
printf FOO "\\documentclass{aastex}\n";
printf FOO "\\usepackage{graphics}\n";
#printf FOO "\\addtolength{\\topmargin}{-0.75in}\n";
printf FOO "\\addtolength{\\oddsidemargin}{-0.75in}\n";
printf FOO "\\addtolength{\\textwidth}{1.5in}\n";

printf FOO "\\begin{document}\n\n";


while ($epsfile[$i]) {
  if ($i != 0) {printf FOO "\\clearpage\n";}
  printf FOO "\\begin{figure*}[!ht]\n";
  printf FOO "\\centering\n";
  
  $num_this_page = scalar(@epsfile) - $i;
  if ($num_this_page > 12) {$num_this_page = 12;}
  print "$num_this_page \n";
  
  for ($jj=0; $jj < $num_this_page; $jj++) {
	# The rotation spec MUST come before the width spec!
    printf FOO "\\includegraphics[angle=-90,width=0.32\\textwidth]{$epsfile[$i]}";$i++;
    if( $jj%3 == 0) {printf FOO " \\hfill";}
    if( $jj%3 == 1) {printf FOO " \\hfill";}
    if( $jj%3 == 2) {printf FOO " \\\\";}
    printf FOO " \n";
	if( $jj%3 == 2) {printf FOO "\\vspace*{0.5in}\n";}
  }

  printf FOO "\\end{figure*}\n";
}
#printf FOO "\\scriptsize\n";
#printf FOO "\\noindent\n";
#printf FOO "THE END\n";
printf FOO "\\end{document}\n";
close(FOO);
system("latex spectra.tex;dvips spectra -o spectra.ps; open -a Preview spectra.ps");
