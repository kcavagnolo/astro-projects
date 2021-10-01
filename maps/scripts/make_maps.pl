#! /usr/bin/perl -w
#
# NAME:
#     make_tempmap.pl
#
# PURPOSE:
#
# EXPLANATION:
#
# CALLING SEQUENCE:
#
# INPUTS:
#
# OUTPUTS:
#
# MODIFICATION HISTORY:
#
#######################
#######################
##    Set Options    ##
#######################
#######################

# what should the script do?
$evtext          = "clean";  # extension of evt file to use
$imbin           = 1;         # !!! MUST MATCH BINNING OF EXPMAP !!!
$make_src        = "";  # 'custom' or blank
$custom          = "";        # extension of custom region
$temp_sn         = "90d0";    # target signal to noise for map making
$flux_sn         = "9d0";     # signal/noise for fluxed image
$color_sn        = "90d0";    # signal/noise for hardness ratio
$create_ctsfile  = "yes";     # make a new counts (signal) file?
$create_maskfile = "yes";     # make a new mask file?
$create_binning  = "yes";     # run the wvt binning algorithm?
$binsrc          = "yes";     # bin src?
$binbgd          = "yes";     # bin bgd?
$split           = "yes";     # cut up the evt2 file into bins?
$extract_spectra = "yes";     # extract spectra for all the bins?
$fit_spec        = "yes";     # should the ext spec be fit?
$eval_tempmap    = "yes";     # generate the final temperature map
$create_fluxed   = "yes";     # generate flux calibrated, binned image
$create_color    = "yes";     # generate binned color image
%ebands          = ("soft"=>"500:1500", "hard"=>"1501:7000");

# set parameters which are used for IDL binning
$rootdir = "reprocessed/";   # name of directory storing evt files
$tmapdir = "tempmap/";       # name of directory storing temperature map files
$verb    = 1;                # How verbose the script should be

# spec ext params
$binspec = "25";             # counts per bin
$wmaplo  = 300;              # Lower bound for WMAP energy
$wmaphi  = 2000;             # Upper bound for WMAP energy
$wmapbin = "det=8";          # Binning specification for WMAP

# set parameters which will be fed to spectral fitter
$xoutfile  = "xspec_allregs.params";
$fit_prog  = "xspec";        # fitting program to use: sherpa or xspec
$src       = "src1";         # extension of spectral files: sou, src1, src1_grp, ...
$model     = "mekal";        # name of model to be used in spectral fitting
$freeze_nh = "yes";          # fix nh to the galactic value? "yes" or "no"
$freeze_fe = "no";           # keep fe at specified level, shouldn't be "yes" often
$xverb     = 1;              # set to > 1 to see XSPEC running
$makegif   = "yes";          # make a GIF of the source spectrum
$timeout   = 18000;          # seconds until kills xspec job
$ntrial    = 10000;          # number of trials in the error calculation
$toler     = 0.01;           # tolerance for the fit statistic
$conlevel  = 2.71;           # compute confidence intervals, 1.0=68%, 2.71=90%, 6.63=99%
$emin      = "0.7";          # min and max energy ranges for fitting,
$emax      = "7.0";          # needs to be a string so PERL doesn't cut off decimal pts.
$crmin     = "0.5";          # min for count rate reporting
$crmax     = "2.0";          # max for count rate reporting
$nhmod     = "wabs";         # e.g., wabs,tbabs, ph
$syserr    = "0.00";         # % systematic error to add
$stat      = "cstat";

#######################
#######################

# check the number of arguments given
use Cwd;
use FindBin qw($Bin);
die "## ERROR: ciao is not loaded\n" unless ($ENV{'ASCDS_BIN'});
die "## ERROR: Wrong number of command line arguments\n" if (@ARGV != 1);

# read in the reference file
%refdata = get_data($ARGV[0]);

# go through each cluster and extract events, images
foreach $key (sort keys %refdata) {

    # split up the data line
    @data = split(/\s+/,$refdata{$key});
    $name   = $data[0];

    # check for multiple obs
    if (exists $check{$name}){
	print "## WARNING: $name has multiple entries skipping.\n";
	next;
    }
    $num = check_obs($name,$ARGV[0]);
    %check = ();
    if ($num > 1) {
	print "## WARNING: There are multiple entries for $name, skipping.\n";
	unless (exists $check{$name}) {
	    open(ERR,">>err_multi");
	    print ERR "$name\n";
	    close ERR;
	}
	$check{$name} = $name;
	next;
    }

    # read rest of data line
    $obsid  = $data[1];
    $id     = $data[11];
    $datadir = $data[15];

    # set the ccd_ids for each array
    if ($id =~ /^i/) {
	$ccds = "0,1,2,3";
    } else {
	$ccds = "7";
    }

    # define file names
    $evtfile   = "${obsid}_${evtext}.fits";
    $bgevtfile = "${obsid}_bgevt.fits";
    $maskfile  = "${obsid}_mask.fits";
    $binfile   = "${obsid}_${temp_sn}";
    $ctsfile   = "${obsid}_cts.fits";
    $bgdctsfile= "${obsid}_bgdcts.fits";
    $expmap    = "${obsid}_expmap.fits";
    $mapname   = "${obsid}_${temp_sn}_map";
    $binroot   = "evt2.${temp_sn}.fits.";
    $bgdroot   = "bgd.${temp_sn}.fits.";
    $normfile  = "${obsid}_tempnorm.fits";
    $chipfits  = "${obsid}_chipregs.fits";

    # change directory to location of tempmaps
    chdir("$datadir/$obsid/");

    # check that all files exist
    print "## STATUS: Looking for needed files...\n";
    @files = ($evtfile, $bgevtfile, $expmap);
    foreach $a (@files) {
	die "## ERROR: No $a found.\n" unless (-e "${rootdir}/".$a);
    }
    print "## STATUS: All files found.\n";

    # make dirs if needed
    $base = cwd;
    $rdir = "${base}/${rootdir}";
    $customreg = "${rdir}/${obsid}_${custom}.reg";
    mkdir(${tmapdir},0777) unless (-d ${tmapdir});
    chdir(${tmapdir}) || die "## ERROR: Can't change to ${tmapdir}/ directory\n";
    mkdir(${temp_sn},0777) unless (-d ${temp_sn});
    chdir(${temp_sn}) || die "## ERROR: Can't change to ${temp_sn}/ directory\n";

    # set badpix file
    &set_ardlib($evtfile);

    # create a counts FITS file
    &create_ctsfile($evtfile,$bgevtfile,$ctsfile,$bgdctsfile) if ($create_ctsfile eq "yes");

    # create a mask
    &create_maskfile($evtfile,$expmap,$maskfile) if ($create_maskfile eq "yes");

    # start the binning IDL batch file
    &create_binning($evtfile, $bgevtfile, $maskfile, $ctsfile, $bgdctsfile,
		    $temp_sn, $binfile, $split, $binroot, $bgdroot) if ($create_binning eq "yes");

    # get the asol file or files
    $asol = get_asolfile();
    print "## Using aspect solution: $asol\n";

    # extract the spectra for each bin
    &extract_spectra($asol, $binroot) if ($extract_spectra eq "yes");

    # fit spectra
    &fit_spectra() if ($fit_spec eq "yes");

    # start the map creation/evaluation IDL batch file
    &eval_tempmap($maskfile, $mapname, $binfile, $binroot) if ($eval_tempmap eq "yes");

    # make a flux calibrated, binned image
    &create_fluxed($evtfile, $normfile, $ctsfile, $maskfile, $flux_sn) if ($create_fluxed eq "yes");

    # make a flux calibrated, binned image
    &create_color($evtfile, $maskfile, $color_sn) if ($create_color eq "yes");

    # clean-up
    system("rm -f test* *.pro $normfile temp*.fits");

    # change back to original directory
    chdir("$Bin");
  }
exit 0;

#######################
#######################
##   Sub-Routines    ##
#######################
#######################

sub get_data {

  my($infile) = @_;
  my(@data,$name,%info);
  open(INFILE,$infile) || die "## ERROR: Cannot open $infile\n";
  while (<INFILE>) {
    chomp;
    next if (/^\#/); # skip comment lines
    next if (/^$/);  # skip blank lines
    s/^\s+//;        # trim leading whitespace
    s/\s+$//;        # trim trailing whitespace
    @data = split;
    $name = join "_", $data[0],$data[1];
    $info{$name} = $_;
  }
  close INFILE;
  return %info;
}

#######################
#######################

sub check_obs {

    my($name,$infile) = @_;
    my(@a,$num);
    $num = 0;
    open(A,$infile);
    while(<A>){
        chomp;
        next if (/^\#/); # skip comment lines
        next if (/^$/);  # skip blank lines
        s/^\s+//;        # trim leading whitespace
        s/\s+$//;        # trim trailing whitespace
        @a = split;
        $num++ if ($a[0] eq $name);
    }
    close A;
    return $num;
}

#######################
#######################

sub set_ardlib {

  my($chips,@chips,$evtfile,$d,$curdir,$indir);
  $indir = cwd();
  $evtfile = shift;
  system("punlearn ardlib.par");

  # change to appropriate dir (they sometimes change the locations)
  # glob for matching file names
  chdir "${base}";
  $curdir = cwd();
  @dir = qw(reprocessed primary secondary);
  @infile = ();
  foreach $dir (@dir) {
    chdir $dir;
    @globlist = glob("*bpix1*");
    @globlist = map { "$dir/" . $_} @globlist if (@globlist);
    push @infile, @globlist;
    chdir "../";	
  }
  die "## ERROR: No bpix file for $obsid.\n" unless (@infile);
  $bpix   = shift @infile;

  # unzip if necesary
  if ($bpix =~ /gz$/) {
    system("gunzip -f $bpix");
    $bpix =~ s/\.gz$//;
  }

  # return the name
  chdir($curdir);

  # get the chips to use
  chdir("${rdir}");
  $chips = `dmkeypar $evtfile DETNAM ; pget dmkeypar value`;
  chdir("$base");
  chomp($chips);
  $chips =~ s/ACIS\-//;
  @chips = split('', $chips);
  foreach $d (@chips) {
    $command = "pset ardlib AXAF_ACIS${d}_BADPIX_FILE=${curdir}/$bpix\[BADPIX${d}]";
    system($command);
  }

  chdir("$indir");
}

#######################
#######################

sub create_ctsfile {

  my($evt,$bgd,$cts,$bgdcts) = @_;

  # change datatype depending on binning
  # binning too high overwhelms dmcopy and
  # requires 4-byte instead of 2-byte
  if ($imbin <= 64) {
      $dtype = "i4";
      $mtype = "135";
  } else {
      $dtype = "i2";
      $mtype = "50";
  }

  # create the counts image needed for bin creation
  if ($make_src ne "custom") {
      $command = "punlearn dmcopy; dmcopy \"${rdir}/$evt\[sky=region(${rdir}/${chipfits}[ccd_id=${ccds}])\][bin sky=$imbin\]"
	  ."[opt type=$dtype]\[opt mem=$mtype\]\" $cts clob+ verbose=${verb}";
      system($command);
      $command = "punlearn dmcopy; dmcopy \"${rdir}/$bgd\[sky=region(${rdir}/${chipfits}[ccd_id=${ccds}])\][bin sky=$imbin\]"
	  ."[opt type=$dtype]\[opt mem=$mtype\]\" $bgdcts clob+ verbose=${verb}";
      system($command);
  } else {
      $command = "punlearn dmcopy; dmcopy \"${rdir}/$evt\[sky=region($customreg)][bin sky=$imbin\]"
	  ."[opt type=$dtype]\[opt mem=$mtype\]\" $cts clob+ verbose=${verb}";
      system($command);
      $command = "punlearn dmcopy; dmcopy \"${rdir}/$bgd\[sky=region($customreg)][bin sky=$imbin\]"
	  ."[opt type=$dtype]\[opt mem=$mtype\]\" $bgdcts clob+ verbose=${verb}";
      system($command);
  }
}

#######################
#######################

sub create_maskfile {

    my($evt,$exp,$mask) = @_;

    # make an empty image of the proper dimensions
    $short = "${rdir}/$evt\[sky=region(${rdir}/${chipfits}[ccd_id=${ccds}])\][bin sky=$imbin\][opt type=$dtype]\[opt mem=$mtype\]";
    $command = "punlearn dmimgcalc; dmimgcalc \"$short\" \"$short\" zero.fits sub clob+ verbose=${verb}";
    system($command);
    $command = "punlearn dmimgthresh; dmimgthresh infile=zero.fits outfile=temp.fits expfile=$rdir/$exp cut=\":40%\" value=1 clob+ verbose=${verb}";
    system($command);
    die "## ERROR: dmimgthresh did not work.\n" unless (-e "temp.fits");

    # create the mask
    if ($make_src eq "custom") {
	$command = "punlearn dmcopy; dmcopy \"temp.fits\[sky=region($customreg)]\" $mask clob+ verbose=${verb}";
	system($command);
    } else {
	system("mv -f temp.fits $mask");
    }
    system("rm -f zero.fits");
}

#######################
#######################

sub create_binning {

  my ($evt,$bgd,$mask,$cts,$bgdcts,$sn,$bn,$splt,$root,$bgdroot) = @_;

  # find the evt2 file
  $evt = "${rdir}/${evt}";
  $bgd = "${rdir}/${bgd}";
  if ($splt eq "yes") {
    $splt = 1;
  } else {
    $splt = 0;
  }

  # CIAO and IDL use the DYLD_BIND_AT_LAUNCH environment variable.
  # By unsetting this variable, it is possible to get IDL to run
  delete $ENV{'DYLD_BIND_AT_LAUNCH'};

  if ($binsrc eq "yes") {
    # write all IDL commands to a file and then run
    open(PROFILE,">bin.pro");
    print PROFILE "wvt_temperaturemap, '$evt', '$mask', '$cts', '$sn', '$bn', '$splt', '$root'\n";
    print PROFILE "exit \n";
    close PROFILE;
    system("\$IDL_DIR/bin/idl bin.pro");
  }

  # create matching bgds
  if ($binbgd eq "yes") {
    if ($splt == 1) {
      $bins = "${binfile}_abinned_binnum.fits";
      open(PRO,">bgd.pro");
      print PRO "bin = mrdfits('$bins',0)\n";
      print PRO "hdr = mrdfits('$bgdcts',0,header)\n";
      print PRO "wvt_evtsplit, '$bgd', bin, header, root='$bgdroot'\n";
      print PRO "exit \n";
      close PRO;
      system("\$IDL_DIR/bin/idl bgd.pro");
    }
  }
}

#######################
#######################

sub extract_spectra {

  my($asol,$root) = @_;
  my($nbins,$i,$evt,$out,$bgd,$backscal,$indir);
  $indir = cwd();

  # determine the number of bins
  $nbins = &get_value("${binfile}_nbins.dat");

  # loop through all the bins
  $i = 1;
  while ($i < $nbins+1) {

    # set some names
    $evt = "${root}${i}";
    $bgd = $evt;
    $bgd =~ s/evt2/bgd/;

    # extract a spectrum of the bgd
    $command = "punlearn dmextract; dmextract infile=\"${bgd}\[bin pi\]\" "
	."outfile=${evt}_bgd.pi wmap=\"[energy=${wmaplo}:${wmaphi}][bin $wmapbin]\" clob+";
    system($command);
    
    # customize the bgd
    $ebackscal = &get_value("${evt}.BACKSCAL");
    $bbackscal = &get_value("${bgd}.BACKSCAL");

    # extract the spectra
    $command = "punlearn mkwarf; pset mkwarf asolfile=$asol";
    system($command);
    $command = "punlearn specextract; specextract infile=\"$evt\" "
	."outroot=${evt} bkgfile=\"\" grouptype=\"NUM_CTS\" energy_wmap=${wmaplo}:${wmaphi} binwmap=$wmapbin "
	."binspec=$binspec ptype=PI clob+ verbose=$verb";
    system($command);

    # delete grouping keyword which confuses XSPEC
    # delete quality keyword which confuses XSPEC
    # set the BACKSCAL header keywords to account for the different areas
    $command = "punlearn dmhedit; dmhedit infile=${evt}_src1_grp.pi operation=del key=GROUPING file=\"\"";
    system($command);
    $command = "punlearn dmhedit; dmhedit infile=${evt}_src1_grp.pi operation=del key=QUALITY file=\"\"";
    system($command);
    $command = "punlearn dmhedit; dmhedit infile=${evt}_src1_grp.pi filelist=none operation=add key=BACKSCAL value=${ebackscal}";
    system($command);
    $command = "punlearn dmhedit; dmhedit infile=${evt}_bgd.pi filelist=none operation=add key=BACKSCAL value=${bbackscal}";
    system($command);
    $command = "punlearn dmhedit; dmhedit infile=${evt}_src1_grp.pi filelist=none operation=add key=BACKFILE value=${evt}_bgd.pi";
    system($command);
    $command = "punlearn dmhedit; dmhedit infile=${evt}_src1_grp.pi filelist=none operation=add key=ANCRFILE value=${evt}_src1.warf";
    system($command);
    $command = "punlearn dmhedit; dmhedit infile=${evt}_src1_grp.pi filelist=none operation=add key=RESPFILE value=${evt}_src1.wrmf";
    system($command);

    $i++;
  }
  chdir("$indir");
}

#######################
#######################

sub fit_spectra {

    # get spectra to work with
    my(@spex,$evt,@passdata,$indir,%spex,$file,$anum);
    $indir = cwd();

    # search for annuli file in current directory
    @spex = glob("${binroot}*${src}.pi");
    
    # want to make sure end up sorted numerically
    # create hash where key is number and value is file name
    undef %spex;
    foreach $file (@spex) {
	$anum = $file;
	if (scalar(@spex) == 1) {
	    $anum = 1;
	} else {
	    $anum =~ s/.*${binroot}(\d+)\_${src}.pi/$1/;
	}
	$spex{$anum} = $file;
    }
    
    # fit every bin
    foreach $anum (sort {$a <=> $b} keys %spex) {
	
	# run xspec to perform the fitting
	if ($fit_prog eq "xspec") {
	    $spec = $spex{$anum};
	    $root = $spec;
	    $root =~ s/_${src}.pi//;
	    @othervars = ($root, $spec, $indir, $model, $freeze_nh, $freeze_fe, $xverb,
			  $makegif, $timeout, $conlevel, $emin, $emax, $crmin, $crmax, 
			  $nhmod, $syserr, $Bin, $stat, $ntrial, $toler, "wvt", $xoutfile);
	    @passdata = (@data, @othervars);
	    chdir("$Bin");
	    $command = "$Bin/subrot_xspec.pl @passdata";
	    print "## Running XSPEC on ${root}...\n";
	    system($command);
	    chdir("$Bin");
	    chdir("$indir");
	} else {
	    die "## ERROR: Xspec is currently the only accepted fitting program.\n";
	}
    }

    # put all spec together into one file
    if ($makegif eq "yes") {
	system("cat list | pscat 4 ${obsid}_${temp_sn}_allspec.ps");
	system("rm -f list");
    }
}

#######################
#######################

sub eval_tempmap {

  my ($mask, $mapname, $bn, $root) = @_;
  my ($linenum, $indir);
  $indir = cwd();

  # this is the line number which contains the data needed for the map
  # should always be 1, but then again...
  $linenum = "1";

  # CIAO and IDL use the DYLD_BIND_AT_LAUNCH environment variable.
  # By unsetting this variable, it is possible to get IDL to run
  delete $ENV{'DYLD_BIND_AT_LAUNCH'};

  # write all IDL commands to a file and then run
  open(PROFILE,">evalmap.pro");
  print PROFILE "wvt_evaltemperaturemap, '$linenum', '$mask', '$mapname', '$bn', '$root'\n";
  print PROFILE "exit \n";
  close PROFILE;
  system("\$IDL_DIR/bin/idl evalmap.pro");

  chdir("$indir");
}

#######################
#######################

sub create_fluxed {

  my ($evt, $norm, $cts, $mask, $sn) = @_;
  my ($indir);
  $indir = cwd();

  # create normalized image
  $short = "${rdir}/$evt\[sky=region(${rdir}/${chipfits}[ccd_id=${ccds}])\][bin sky=$imbin\][opt type=$dtype]\[opt mem=$mtype\]";
  $command = "punlearn dmimgcalc; dmimgcalc infile=\"$short\" infile2=\"${rdir}/${expmap}\" "
      ."outfile=temp.fits operation=div clob+ verbose=${verb}";
  system($command);
  if ($make_src eq "custom") {
      $command = "punlearn dmcopy; dmcopy \"temp.fits[sky=region($customreg)]\" $norm clob+ verbose=${verb}";
      system($command);
  } else {
      system("mv -f temp.fits $norm");
  }

  # CIAO and IDL use the DYLD_BIND_AT_LAUNCH environment variable.
  # By unsetting this variable, it is possible to get IDL to run
  delete $ENV{'DYLD_BIND_AT_LAUNCH'};

  # write all IDL commands to a file and then run
  open(PROFILE,">flux.pro");
  print PROFILE "evtfile   = '${norm}'\n";
  print PROFILE "mask      = mrdfits('${mask}',0)\n";
  print PROFILE "ctsimage  = mrdfits('${cts}',0,header)\n";
  print PROFILE "image     = ctsimage\n";
  print PROFILE "noise     = sqrt(image)\n";
  print PROFILE "origdim   = size(image,/dimension)\n";
  print PROFILE "max_area  = total(mask)/10.\n";
  print PROFILE "abinout   = '${obsid}_${sn}_flux_abinned.fits'\n";
  print PROFILE "snrout    = '${obsid}_${sn}_flux_abinned_snr.fits'\n";
  print PROFILE "dataout   = '${obsid}_${sn}_flux_abinned_data.fits'\n";
  print PROFILE "binnumout = '${obsid}_${sn}_flux_abinned_binnum.fits'\n";
  print PROFILE "nbinout   = '${obsid}_${sn}_flux_nbins.dat'\n";
  print PROFILE "targetSN  = ${sn}\n";
  print PROFILE "file_delete, 'stopmenow',/quiet\n";
  print PROFILE "wvt_image, image, noise, targetSN, binnedimage, xnode, ynode, weight, \$\n";
  print PROFILE "           snbin=snbin, mask=mask, ctsimage=ctsimage, binnumber=binnumber, \$\n";
  print PROFILE "           binvalue=binvalue, save_all=save_all, max_area=max_area\n";
  print PROFILE "file_delete, [abinout, snrout, dataout, binnumout],/quiet\n";
  print PROFILE "mwrfits, binnedimage, abinout, header\n";
  print PROFILE "snbin = [0,snbin]\n";
  print PROFILE "mwrfits, mask*(snbin[binnumber]), snrout, header\n";
  print PROFILE "mwrfits, save_all, dataout\n";
  print PROFILE "mwrfits, mask*binnumber, binnumout, header\n";
  print PROFILE "exit\n";
  close PROFILE;
  system("\$IDL_DIR/bin/idl flux.pro");
  chdir("$indir");
}

#######################
#######################

sub create_color {

  my ($evt, $mask, $sn) = @_;
  my ($indir);
  $indir = cwd();

  # set some data-type and memory options for making high-res images
  if ($imbin <= 64) {
      $dtype = "i4";
      $mtype = "135";
  } else {
      $dtype = "i2";
      $mtype = "50";
  }

  # make images and noise of each band
  foreach $band (keys %ebands) {
      $command = "punlearn dmcopy ; dmcopy \"${rdir}/${evt}[energy=$ebands{$band}]\" temp.fits clob+ verbose=$verb";
      system($command);
      $command = "punlearn dmcopy; dmcopy \"temp.fits[sky=region(${rdir}/${chipfits}[ccd_id=${ccds}])\][bin sky=$imbin\]"
	  ."[opt type=$dtype]\[opt mem=$mtype\]\" temp2.fits clob+ verbose=${verb}";
      system($command);
      $command = "punlearn dmimgcalc; dmimgcalc temp2.fits ${rdir}/$expmap temp_${band}.fits operation=div clob+ verbose=${verb}";
      system($command);
      $command = "punlearn dmimgcalc; dmimgcalc temp2.fits none temp3.fits op=\"imgout=sqrt(img1)\" clob+ verbose=${verb}";
      system($command);
      $command = "punlearn dmimgcalc; dmimgcalc temp3.fits ${rdir}/$expmap temp_noise${band}.fits operation=div clob+ verbose=${verb}";
     system($command);
  }

  # chop out sub-region
  if ($make_src eq "custom") {
      foreach $band (keys %ebands) {
	  $command = "punlearn dmcopy; dmcopy \"temp_${band}.fits[sky=region($customreg)]\" ${obsid}_${band}.fits clob+ verbose=${verb}";
	  system($command);
	  $command = "punlearn dmcopy; dmcopy \"temp_noise${band}.fits[sky=region($customreg)]\" ${obsid}_${band}noise.fits clob+ verbose=${verb}";
	  system($command);
      }
  } else {
      foreach $band (keys %ebands) {
	  system("mv -f temp_${band}.fits ${obsid}_${band}.fits");
	  system("mv -f temp_noise${band}.fits ${obsid}_${band}noise.fits");
      }
  }
  system("rm -f temp*.fits");

  # CIAO and IDL use the DYLD_BIND_AT_LAUNCH environment variable.
  # By unsetting this variable, it is possible to get IDL to run
  delete $ENV{'DYLD_BIND_AT_LAUNCH'};

  # write all IDL commands to a file and then run
  open(PROFILE,">color.pro");
  print PROFILE "soft      = mrdfits('${obsid}_soft.fits',0)\n";
  print PROFILE "hard      = mrdfits('${obsid}_hard.fits',0)\n";
  print PROFILE "softnoise = mrdfits('${obsid}_softnoise.fits',0)\n";
  print PROFILE "hardnoise = mrdfits('${obsid}_hardnoise.fits',0)\n";
  print PROFILE "mask      = mrdfits('${mask}',0)\n";
  print PROFILE "ctsimage  = mrdfits('${ctsfile}',0,header)\n";
  print PROFILE "targetSN  = ${sn}\n";
  print PROFILE "abinout   = '${obsid}_${sn}_color_abinned.fits'\n";
  print PROFILE "snrout    = '${obsid}_${sn}_color_abinned_snr.fits'\n";
  print PROFILE "dataout   = '${obsid}_${sn}_color_abinned_data.fits'\n";
  print PROFILE "binnumout = '${obsid}_${sn}_color_abinned_binnum.fits'\n";
  print PROFILE "nbinout   = '${obsid}_${sn}_color_nbins.dat'\n";
  print PROFILE "targetSN  = ${sn}\n";
  print PROFILE "wvt_xraycolor, soft, hard, softnoise, hardnoise, targetSN, binnedimage, \$\n";
  print PROFILE "           xnode, ynode, snbin=snbin, mask=mask, ctsimage=ctsimage, binnumber=binnumber, \$\n";
  print PROFILE "           binvalue=binvalue, save_all=save_all\n";
  print PROFILE "file_delete, [abinout, snrout, dataout, binnumout],/quiet\n";
  print PROFILE "mwrfits, binnedimage, abinout, header\n";
  print PROFILE "snbin = [0.,snbin]\n";
  print PROFILE "mwrfits, snbin[binnumber], snrout, header\n";
  print PROFILE "mwrfits, save_all, dataout\n";
  print PROFILE "mwrfits, mask*binnumber, binnumout, header\n";
  print PROFILE "exit\n";
  close PROFILE;
  system("\$IDL_DIR/bin/idl color.pro");
  chdir("$indir");
  system("rm -f temp.fits");
  system("rm -f temp2.fits");
}

#######################
#######################

sub get_value {

  my($file) = @_;
  my(@data,$value);
  open(INFILE,$file) || die "## ERROR: Cannot open $file\n";
  while (<INFILE>) {
    chomp;
    next if (/^\#/); # skip comment lines
    next if (/^$/);  # skip blank lines
    s/^\s+//;        # trim leading whitespace
    s/\s+$//;        # trim trailing whitespace
    @data = split;
    $value = "";
    $value = $data[0];
  }
  close INFILE;
  return $value;
}

#######################
#######################

sub get_asolfile {

  # get the asol file or files
  my($curdir,@infile,$asol,@dir,@globlist);
  $curdir = cwd();

  # change to appropriate dir (they sometimes change the locations)
  @dir = qw(primary secondary);
  @infile = ();
  foreach $dir (@dir) {
    chdir "${base}";
    chdir "$dir";
    @globlist =  <pcad*asol*.fits*>;
    @globlist = map { "${base}/$dir/" . $_} @globlist if (@globlist);
    push @infile, @globlist;
    chdir $curdir;
  }
  die "## ERROR: No asol files found for $obsid.\n" unless (@infile);
  $asol = join(",",@infile);

  # return the name(s)
  chdir($curdir);
  return $asol;
}

#######################
#######################
