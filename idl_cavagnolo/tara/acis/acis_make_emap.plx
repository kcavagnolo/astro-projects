#!/usr/bin/perl
#===============================================================================
#
# FILE NAME:    $Id: acis_make_emap.plx,v 1.2 2008-10-15 19:35:50 cavagnolo Exp $
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
#===============================================================================
sub USAGE {
    printf  "\nUsage:  acis_make_emap.plx xmin xmax num_x_pixels ymin ymax num_y_pixels event_list aspect_file pbkfile scene_name -spectrum=? -monoenergy=? -clobber -cache -ccdlist=? -onlytime\n\n";
    printf "aspect_file can be:\n  a single asol file\n  a reference to a list of them of the form '\@my_asols.list'\n  or NONE when -cache used\n\n";
    printf "pbkfile is the 'parameter block file' input to mkinstmap.\n";
    printf "spectrum & monoenergy are parameters to mkinstmap.\n";
    printf "use -onlytime to create maps of exposure time only, with QE=1.\n\n";
}

#===============================================================================
#
use File::Basename;
use Getopt::Long;

$ID = q$Revision: 1.2 $;

printf "\nacis_make_emap.plx, $ID\n";

$spectrumfile = "NONE";
$monoenergy   = 1;
$result = GetOptions("clobber", "cache", "onlytime", "spectrum=s", \$spectrumfile, "monoenergy=s", \$monoenergy,
"ccdlist=s", \$ccdlist);

if ($spectrumfile != "NONE")
  {
  unless(-e $spectrumfile) {die "\nFile not found:  $spectrumfile\n\n";}
  }

#-------------------------------------------------------------------------------
# Extract command line arguments
#-------------------------------------------------------------------------------
$xmin         = $ARGV[0] or die USAGE();
$xmax         = $ARGV[1] or die USAGE();
$num_x_pixels = $ARGV[2] or die USAGE();
$ymin         = $ARGV[3] or die USAGE();
$ymax         = $ARGV[4] or die USAGE();
$num_y_pixels = $ARGV[5] or die USAGE();

#-------------------------------------------------------------------------------
# Check that the event file exists
#-------------------------------------------------------------------------------
$event_file = $ARGV[6] or die USAGE();
unless(-e $event_file) {die "\nFile not found:  $event_file\n\n";}


#-------------------------------------------------------------------------------
# Check that the aspect solution file exists
#-------------------------------------------------------------------------------
$aspect_file = $ARGV[7] or die USAGE();
$pbkfile     = $ARGV[8] or die USAGE();
$scene_name  = $ARGV[9] or die USAGE();

if ($opt_cache)
  {
  printf "\n%s\nWARNING! Using previously computed aspect histograms if available.\n%s\n", "-" x 80, "-" x 80, ;
  }

print SpawnCommand("plist ardlib | grep ACIS |grep BADPIX");

if (! $opt_onlytime)
  {
  print SpawnCommand("plist ardlib | grep ACIS |grep QEU");
  }
printf "\nPAUSING SO YOU CAN REVIEW CONTENTS OF ARDLIB.PAR!!!!\n";
sleep 10;
    

SpawnCommand("punlearn asphist; punlearn mkinstmap; punlearn mkexpmap; punlearn dmregrid; punlearn dmhedit");

#-------------------------------------------------------------------------------
# Create a directory to hold the scratch results.
#-------------------------------------------------------------------------------
$scratch = "asphist";
mkdir( $scratch, 0777 );

$deltaX = ($xmax-$xmin)/$num_x_pixels;
$deltaY = ($ymax-$ymin)/$num_y_pixels;

printf "\nYour exposure map pixels are %9.5f X %9.5f sky pixels.\n", 
	$deltaX, $deltaY;

unless($deltaX == $deltaY) {die "\nPixel width must equal pixel height! \n\n";}

#-------------------------------------------------------------------------------
# determine which CCDs are in this dataset
#-------------------------------------------------------------------------------
if (defined($ccdlist))
  {
  $detnam = "ACIS-$ccdlist";
  }
else
  {
  `dmkeypar $event_file DETNAM`;
  chomp($detnam = `pget dmkeypar value`);
  }

# Parse the detector keyword and extract the active CCD_IDs for this
# observation.  The value should be something like 'ACIS-abcd' (with single
# quotes).
$active_ccds = [];
for ($ii=5; $ii<length($detnam); $ii++) {
  $ccd = substr($detnam,$ii,1);
  unless ( ($ccd >= 0) && ($ccd <=9) )  {
    die "\nError extracting active CCDs from DETNAM keyword:  $detnam\n\n";
  }
  @active_ccds = (@active_ccds, $ccd);
}
$num_ccds = (scalar @active_ccds);

printf "\nActive CCDs are:";
for ($ii=0; $ii<$num_ccds; $ii++) {
  printf "  $active_ccds[$ii]";
}
printf "\n";


#-------------------------------------------------------------------------------
# Create an aspect histogram file. Make an instrument map and exposure map.
#-------------------------------------------------------------------------------
@emap_list     = ();
@exposure_times = ();

CCD: for ($ii=0; $ii<$num_ccds; $ii++) {

  $ccd = $active_ccds[$ii];

#-------------------------------------------------------------------------------
# Grab the exposure time from the event file.
#-------------------------------------------------------------------------------
  `dmkeypar $event_file EXPOSUR${ccd}`;
  chomp($exposure_times[$ii] = `pget dmkeypar value`);

  printf "\n%s\nCreating exposure map for CCD %d\n", "-" x 80, $ccd;
  
  if ($exposure_times[$ii] <= 0) 
  {
    printf "EXPOSUR${ccd} keyword missing; skipping this CCD!!";
    next CCD;
  }

#-------------------------------------------------------------------------------
# printf "  Creating aspect histogram file ...\n";
#-------------------------------------------------------------------------------
  $asphist = "$scratch/ccd${ccd}.asphist";
  if((! $opt_cache) || (! -e $asphist)) 
  {
    SpawnCommand("asphist infile=\'$aspect_file\' evtfile=\'$event_file\[ccd_id=$ccd\]\' outfile=$asphist dtffile=\'\' clobber=yes");
  }

  # Compare EXPOSUR? and sum of aspect histogram.
  $_ = `dmstat \"${asphist}\[cols DURATION\]\"`;
  /[.\n]*sum\D+(\d+\.\d*)/;
  $hist_total = $1;

  printf "  EXPOSUR${ccd} = %9.1f; sum of aspect histogram = %9.1f\n", $exposure_times[$ii], $hist_total;

#-------------------------------------------------------------------------------
# printf "\n  Creating instrument map file ...\n";
#-------------------------------------------------------------------------------
  $instmap = "$scratch/ccd${ccd}.instmap";
  if($opt_onlytime) 
  {
    SpawnCommand("mkinstmap obsfile=\'$asphist\[asphist\]\' pbkfile=$pbkfile outfile=$instmap det=\"ACIS-${ccd};IDEAL\" mirror=\"HRMA;AREA=1\" pixelgrid=\"1:1024:#1024,1:1024:#1024\" spectrumfile=NONE            monoenergy=1           maskfile=NONE grating=NONE verbose=0 clobber=yes");
  }
  else
  {
    SpawnCommand("mkinstmap obsfile=\'$asphist\[asphist\]\' pbkfile=$pbkfile outfile=$instmap det=ACIS-${ccd}                                  pixelgrid=\"1:1024:#1024,1:1024:#1024\" spectrumfile=${spectrumfile} monoenergy=$monoenergy maskfile=NONE grating=NONE verbose=0 clobber=yes");
  }

#-------------------------------------------------------------------------------
# printf "\n  Creating exposure map file ...\n";
#-------------------------------------------------------------------------------
# Use of normalize=no produces a map with units of s*cm^2.
  $expmap = "$scratch/ccd${ccd}.emap";
  push (@emap_list, $expmap); 
  if(OverwriteFile($expmap)) {
    SpawnCommand("mkexpmap instmapfile=$instmap outfile=$expmap asphistfile=$asphist  xygrid=\"$xmin:$xmax:#$num_x_pixels,$ymin:$ymax:#$num_y_pixels\" normalize=no useavgaspect=no verbose=0 clobber=yes");
  }

} #for



#-------------------------------------------------------------------------------
# Now combine the exposure maps into a single binned exposure map image.
#-------------------------------------------------------------------------------
$expmap_all = "${scene_name}.emap";
printf "\nCombining individual exposure maps into a single exposure map...\n";

open(EMAP_LIST, ">emap.lis") || die "\ncannot open emap.lis\n";
foreach $expmap (@emap_list)
  {
  printf(EMAP_LIST "$expmap\n");
  }
close(EMAP_LIST);

if(OverwriteFile($expmap_all)) 
  {
  SpawnCommand("dmregrid \@emap.lis $expmap_all rotang=0 npts=1 bin='1:$num_x_pixels:1,1:$num_y_pixels:1' xoffset=0 yoffset=0 rotxcenter=0 rotycenter=0 clobber=yes");
  }
                   
#-------------------------------------------------------------------------------
# Record the "scene" parameters for the use of acis_make_img_match_emap.plx.
#-------------------------------------------------------------------------------
`dmhedit $expmap_all filelist=none operation=add key=XMIN value=$xmin`;
`dmhedit $expmap_all filelist=none operation=add key=XMAX value=$xmax`;
`dmhedit $expmap_all filelist=none operation=add key=YMIN value=$ymin`;
`dmhedit $expmap_all filelist=none operation=add key=YMAX value=$ymax`;


printf "\nacis_make_emap complete.\n\n";
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
