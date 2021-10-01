#!/usr/bin/perl
#===============================================================================
#
# FILE NAME:    $Id: acis_make_img_match_emap.plx,v 1.2 2008-10-15 19:35:50 cavagnolo Exp $
#
# DESCRIPTION:  A script to create an ACIS exposure map.  The default operation
#               of this script is to prompt the user before overwriting a file.
#               This behavior can be overridden by using the --clobber flag.
#
# AUTHOR:       Scott Koch (tsk@astro.psu.edu) & Patrick Broos
#               Copyright (C) 2000, Pennsylvania State University
# 
# NOTES:        
#
#
# REVISION HISTORY:
#
# July 24, 2000 - tsk - Handle EXPOSURE[0-9] keywords in a completely different
#                       way.  Instead of summing total CCD exposure then 
#                       weighting the exposure map by that, simply multiply
#                       exposure map for ccd N by the keyword EXPOSUR{N}.
#
# Aug 27, 2000  - tsk - Add a prompt so user can indicate whether to use a
#                       spectrum file with mkinstmap.  The default is NONE.
#===============================================================================
sub USAGE {
    printf  "\n\nUsage:  acis_make_img_match_emap.plx event_list emap_file -outpath foo/bar -clobber\n\n";
}
#===============================================================================
#
use File::Basename;
use Getopt::Long;

$result = GetOptions("clobber", "outpath=s", \$outpath);

  
#-------------------------------------------------------------------------------
# Extract command line arguments
#-------------------------------------------------------------------------------
$event_file = $ARGV[0] or die USAGE();
#unless(-e $event_file) {die "\nFile not found:  $event_file\n\n";}

$emap_file = $ARGV[1] or die USAGE();
unless(-e $emap_file) {die "\nFile not found:  $emap_file\n\n";}

#-------------------------------------------------------------------------------
# Look up the binning parameters from the exposure map.
#-------------------------------------------------------------------------------
`dmkeypar $emap_file XMIN`;
chop($xmin   = `pget dmkeypar value`);

`dmkeypar $emap_file XMAX`;
chop($xmax   = `pget dmkeypar value`);

`dmkeypar $emap_file YMIN`;
chop($ymin   = `pget dmkeypar value`);

`dmkeypar $emap_file YMAX`;
chop($ymax   = `pget dmkeypar value`);

`fkeypar "${emap_file}[0]" NAXIS1`;
chop($naxis1= `pget fkeypar value`);

`fkeypar "${emap_file}[0]" NAXIS2`;
chop($naxis2= `pget fkeypar value`);

#-------------------------------------------------------------------------------
# Create a filename to hold the results.
#-------------------------------------------------------------------------------
if (! $outpath)
  {
  ($outpath,$dir,$suffix) = fileparse($emap_file,'\.emap');
  }
  
$sky_image  = "${outpath}.img";
$norm_image = "${outpath}.flux.img";

#-------------------------------------------------------------------------------
# Now create an image of the sky binned the same as the exposure map
#-------------------------------------------------------------------------------
printf "\nCreating a sky image...\n";
if(OverwriteFile($sky_image)) {
  SpawnCommand("dmcopy \"$event_file\[bin x=$xmin:$xmax:#$naxis1,y=$ymin:$ymax:#$naxis2\]\" $sky_image clobber=yes");
}

               
#-------------------------------------------------------------------------------
# Finally, divide the sky image by the exposure map.
#-------------------------------------------------------------------------------
printf "\nNormalizing $sky_image by $emap_file...\n";
if(OverwriteFile($norm_image)) {
  SpawnCommand("dmimgcalc infile=$sky_image infile2=$emap_file outfile=$norm_image operation=div weight=1.0 weight2=1.0 verbose=0 clobber=yes");
}

printf "\nacis_make_img_match_emap complete.\n\n";
exit;

#===============================================================================
sub OverwriteFile {

  local $ret    = 1;
  local $file   = $_[0];
  local $answer = "";

  if ($opt_clobber) {
    return 1;
  }

  if (-e $file) {
    printf "$file exists.  Press 1 (one) to overwrite:  ";
    $answer = <STDIN>;
    chomp $answer;

    if($answer != 1) {
      $ret = 0;
    }
  }

  return $ret;
}

#===============================================================================
sub SpawnCommand {
  local $command = $_[0];

  printf "  $command\n";
  return `$command`;
}
