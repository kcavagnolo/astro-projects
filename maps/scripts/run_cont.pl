#! /usr/bin/perl -w
#
# NAME:
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
# TO DO:
#    1) compute distance from fiducial (x,y) to center of bin and store in fit file
#
#######################
#######################
##    Set Options    ##
#######################
#######################

# general params
$evtext    = "clean";                 # extension of FITS file to use
$rootdir   = "reprocessed";           # location of FITS files
$tmapdir   = "tempmap";               # directory to write temperature map files
$dobin     = "yes";                   # run the contour binning?
$doext     = "yes";                   # extract spectra for each bin?
$dofit     = "yes";                   # fit spectra for each bin?
$domaps    = "yes";                   # create final maps of tx, fe, norm...

# binning params
$emin      = 700;                     # minimum energy to filter events
$emax      = 7000;                    # maximum energy to filter events
$binning   = "1";                     # binning of output image
$sn        = "30";                    # signal to noise of final contour bins
$ssn       = "10";                    # smoothing S/N for binning
$cval      = "2";                     # ?
$sbinning  = 25;                      # Binning specification for spectra
$wmaplo    = 300;                     # Lower bound for WMAP energy
$wmaphi    = 2000;                    # Upper bound for WMAP energy
$wmapbin   = "det=8";                 # Binning specification for WMAP

# spec fit params
$xoutfile  = "xspec_allregs.params";  # name out output file for bf params
$fit_prog  = "xspec";                 # fitting program to use: sherpa or xspec
$src       = "src1_grp";              # extension of spectral files: sou, src1, src1_grp, ...
$model     = "mekal";                 # name of model to be used in spectral fitting
$freeze_nh = "yes";                   # fix nh to the galactic value? "yes" or "no"
$freeze_fe = "no";                    # keep fe at specified level, shouldn't be "yes" often
$xverb     = 1;                       # set to > 1 to see XSPEC running
$makegif   = "yes";                   # make a GIF of the source spectrum
$timeout   = 18000;                   # seconds until kills xspec job
$ntrial    = 10000;                   # number of trials in the error calculation
$toler     = 0.01;                    # tolerance for the fit statistic
$conlevel  = 2.71;                    # compute confidence intervals, 1.0=68%, 2.71=90%, 6.63=99%
$xemin     = "0.7";                   # min and max energy ranges for fitting,
$xemax     = "7.0";                   # needs to be a string so PERL doesn't cut off decimal pts.
$crmin     = "0.5";                   # min for count rate reporting
$crmax     = "2.0";                   # max for count rate reporting
$nhmod     = "wabs";                  # e.g., wabs,tbabs, ph
$syserr    = "0.00";                  # % systematic error to add
$stat      = "chi";                   # statistic to use for fitting

#######################
#######################
##   Main Program    ##
#######################
#######################

# check for ciao env loaded
use Cwd;
use FindBin qw($Bin);
use IPC::Open3;
die "## ERROR: ciao is not loaded\n" unless ($ENV{'ASCDS_BIN'});
die "## Wrong number of command line arguments\n" if (@ARGV != 1);

# read in the reference file
%refdata = get_data($ARGV[0]);

# go through each cluster and extract events, images
foreach $key (sort keys %refdata) {

    # split up the data line
    @data = split(/\s+/,$refdata{$key});
    $name    = $data[0];

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

    # read rest of dataline
    $obsid   = $data[1];
    $x       = $data[2];
    $y       = $data[3];
    $fidz    = $data[6];
    $fidtx   = $data[8];
    $datadir = $data[15];

    # set some names
    $evtfile  = "${obsid}_${evtext}.fits";
    $bgdfile  = "${obsid}_bgevt.fits";
    $maskreg  = "${obsid}_cbin_mask.reg";
    $maskfile = "${obsid}_mask.fits";
    $imgfile  = "${obsid}_img.fits";
    $sndir    = "cbin_$sn";
    $binroot  = "evt_";

    # change directory
    chdir("$datadir/$obsid");
    $base = cwd;
    $rdir = "${base}/${rootdir}";

    # check that all files exist
    @files = ($evtfile, $bgdfile);
    foreach $a (@files) {
        die "## ERROR: No $a found.\n" unless (-e "${rootdir}/".$a);
    }
    $asol = get_asolfile();

    # make dirs if needed
    mkdir(${tmapdir},0777) unless (-d $tmapdir);
    chdir(${tmapdir}) || die "## ERROR: Cannot change to ${tmapdir}/ directory\n";
    mkdir(${sndir},0777);
    chdir(${sndir}) || die "## ERROR: Cannot change to ${sndir}/ directory\n";

    # set badpix file
    &set_ardlib($evtfile);

    # run the contour binning
    if ($dobin eq "yes") {
	&sub_make_mask;
	&sub_make_bins;
    }

    # extract spectra
    &sub_ext_spec if ($doext eq "yes");

    # fit spectra
    &sub_fit_spec if ($dofit eq "yes");

    # make maps
    &sub_make_maps if ($domaps eq "yes");
}

# CIAO and IDL use the DYLD_BIND_AT_LAUNCH environment variable.
# By unsetting this variable, it is possible to get IDL to run
delete $ENV{'DYLD_BIND_AT_LAUNCH'};

# amuse Ken
print "## Thank you, Satan! >:)\n";

# exit cleanly
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

sub sub_make_mask {

    my($command, $pid, @dat, $fx, $fy, $dx, $dy, $rot);
    system("ds9 -version");
    if ($? == -1) {
	die "## ERROR: Program \'ds9\' not on system.\n## Create mask by hand and save as <obsid>_cbin_mask.reg.\n";
    }
    print "## Found DS9!\n";
    print "\nOpening DS9 to generate mask.\n";
    print "\nINSTRUCTION:\n\n";
    print "NB: Use ctrl+c to kill the script if needed.\n";
    print "1) Create a **SQUARE** region centered on the area of interest and with any size.\n";
    print "2) Set region type using Region -> File Format -> Ciao\n";
    print "3) Save region using Region -> Save Regions... Hit enter and okay the overwrite.\n";
    print "4) Close window.\n\n";
    unless (-e $maskreg) {
	open(MASK,">$maskreg");
	print MASK "# Region file format: CIAO version 1.0\n";
	print MASK "rotbox($x,$y,50,50,0)\n";
	close MASK;
    }
    $command = "ds9 ${rdir}/$evtfile -region $maskreg -scale sqrt -cmap b";
    $pid = system($command);
    waitpid($pid,0);

    # get xmin and ymin from region
    open(MASK,"$maskreg");
    while (<MASK>) {
	chomp;
	next if (/^\#/);
	next if (/^$/);
	next if (/^\-/);
        s/^\s+//;
	s/\s+$//;
	@dat = split(/\(/,$_);
	($fx,$fy,$dx,$dy,$rot) = split(/,/,$dat[1]);
	$dy =~ s/\)//;
	$minx = sprintf("%i",$fx-($dx/2.0));
	$miny = sprintf("%i",$fy-($dy/2.0));
    }
    close MASK;
}

#######################
#######################

sub sub_make_bins {

    # get rid of old stuff
    system("rm -rf *xaf*.reg");
    system("rm -rf contbin*");
    system("rm -rf bin_*");

    # filter on energy and mask
    $command = "punlearn dmcopy; dmcopy \"${rdir}/${evtfile}\[energy=${emin}:${emax}\]\" tempimg.fits clob+";
    system($command);
    $command = "punlearn dmcopy; dmcopy \"tempimg.fits\[sky=region(${maskreg})\]\[bin sky=$binning\]\" ${imgfile} clob+";
    system($command);
    $command = "rm tempimg.fits";
    system($command);
    
    # make a maks with all ones
    system("rm -rf $maskfile");
    $command = "farith ${imgfile} 0 temp.fits MUL";
    system($command);
    $command = "farith temp.fits 1 ${maskfile} ADD";
    system($command);
    $command = "rm temp.fits";
    system($command);

    # contour bin
    system("contbin -V");
    if ($? == -1) {
	die "## ERROR: Program \'contbin\' not on system.\n## Ummm... install it?.\n";
    }
    print "\n--Running Jeremy Sanders' contour binning program--\n";
    $command = "contbin --mask=$maskfile --sn=$sn --smoothsn=$ssn --constrainfill --constrainval=$cval $imgfile";
    system($command);
    
    # create regions for spec extraction
    system("make_region_files -V");
    if ($? == -1) {
	die "## ERROR: Program \'make_region_files\' not on system.\n## Ummm... install it?.\n";
    }
    print "## STATUS: Making regions for spectral extraction\n";
    $command = "make_region_files --minx=${minx} --miny=${miny} --bin=${binning} --outdir='.' contbin_binmap.fits";
    system($command);
}

#######################
#######################

sub sub_ext_spec {

    # remove old stuff
    system("rm -rf evt_*xaf*");

    my($reg, $bin, $root, $command);
    print "## STATUS: Cutting-up event file for spectral extraction\n";
    system("ls -1 xaf*.reg > reg");
    $command = "punlearn mkwarf; pset mkwarf asolfile=$asol";
    system($command);
    open(IN,"reg");
    while(<IN>) {
	chomp;
	next if (/^\#/); # skip comment lines
	next if (/^$/);  # skip blank lines
	s/^\s+//;        # trim leading whitespace
	s/\s+$//;        # trim trailing whitespace
	$reg = $_;
	$bin = $reg;
	$bin =~ s/\.reg//;
	$root = "${binroot}${bin}";
	print "## STATUS: Extracting spectrum for $root\n";
	$command = "punlearn specextract; specextract infile=\"${rdir}/${evtfile}\[sky=region($reg)]\" "
	    ."outroot=$root bkgfile=\"\" grouptype=\"NUM_CTS\" energy_wmap=${wmaplo}:${wmaphi} binwmap=$wmapbin "
	    ."binspec=$sbinning ptype=PI clob+";
	system($command);
	$command = "punlearn dmextract; dmextract infile=\"${rdir}/${bgdfile}\[sky=region($reg)\]\[bin pi\]\" "
	    ."outfile=${root}_bgd.pi wmap=\"[energy=${wmaplo}:${wmaphi}][bin $wmapbin]\" clob+";
	system($command);
	$command = "punlearn dmhedit; dmhedit infile=${root}_src1_grp.pi operation=add key=BACKFILE value=${root}_bgd.pi filelist=\"\"";
	system($command);
	$command = "punlearn dmhedit; dmhedit infile=${root}_src1_grp.pi operation=del key=GROUPING file=\"\"";
	system($command);
	$command = "punlearn dmhedit; dmhedit infile=${root}_src1_grp.pi operation=del key=QUALITY file=\"\"";
	system($command);
    }
    close IN;
    system("rm -rf reg");
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

sub sub_fit_spec {

    # get spectra to work with
    my(@spex,$evt,@passdata,$indir,%spex,$file,$anum);
    $indir = cwd();

    # search for annuli file in current directory
    # want to make sure end up sorted numerically
    # create hash where key is number and value is file name
    @spex = glob("*xaf*${src}.pi");
    undef %spex;
    foreach $file (@spex) {
        @anum = split("_",$file);
	$anum = $anum[2];
        $spex{$anum} = $file;
    }

    # fit every bin
    system("rm -rf $xoutfile");
    foreach $anum (sort {$a <=> $b} keys %spex) {
        if ($fit_prog eq "xspec") {
            $spec = $spex{$anum};
            $root = $spec;
            $root =~ s/_${src}.pi//;
	    print "$root\n";
            @othervars = ($root, $spec, $indir, $model, $freeze_nh, $freeze_fe, $xverb,
                          $makegif, $timeout, $conlevel, $xemin, $xemax, $crmin, $crmax, 
			  $nhmod, $syserr, $Bin, $stat, $ntrial, $toler, "cbin", $xoutfile);
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
	system("cat list | pscat 4 ${obsid}_${sn}_allspec.ps");
	system("rm -f list");
    }
}

#######################
#######################

sub sub_make_maps {

    my(@reg, $file, @a, @b, $binnum, $num, @line);

    # get area of all regions
    @reg = glob("*xaf*.reg");
    open(OUT,">areas");
    foreach $file (@reg) {
        @a = split("_",$file);
        @b = split("\\.",$a[1]);
        $binnum = $b[0];
	$num = 0;
	open(A,"$file");
	while (<A>) {
	    chomp;
	    next unless (/^rotbox/);
	    s/^\s+//;
	    s/\s+$//;
	    @line = split(",",$_);
	    $num += $line[2]*$line[3];
	}
	close A;
	print OUT "$binnum $num\n";
    }
    close OUT;

    # write IDL file
    print "## STATUS: Creating final 2D maps\n";
    system("rm -rf map.pro *_map.fits");
    open(PRO,">map.pro");
    print PRO
	"PRO map\n",
	"readcol, '${xoutfile}', FORMAT='A,A,I,F,F,F,F,F,F,F,F,F,F,F,F,F,F,F,F,F,F,F,F,F,F,F', comment='#', /silent, \$\n",
	"clusters, obs, xbin, rin, nh, nhlo, nhhi, tx, txlo, txhi, fe, felo, fehi, norm, normlo, normhi,\$\n",
	"t2, t2lo, t2hi, norm2, norm2lo, norm2hi, z, cr, src, chisq, dof\n",
	"readcol, 'areas', FORMAT='(F,F)',\$\n",
	"         abins, area\n",
	"cosmology,'$fidz',result,/silent\n",
	"da = result[3]\n",
	"ang = result[4]\n",
	"rvir = rdelta(200,'$fidz','$fidtx',/silent)\n",
	"normlo = norm - normlo\n",
	"normhi = normhi - norm\n",
	"txlo = tx - txlo\n",
	"txhi = txhi - tx\n",
	"FOR i=0,n_elements(xbin)-1 DO BEGIN\n",
	"   ord = where(abins EQ xbin[i], count)\n",
	"   IF count LE 0 THEN \$\n",
	"      vol = -99.0 \$\n",
	"   ELSE \$\n",
	"      vol = (area[ord]*0.492^2.*ang^2.*3.08d21^2.)*(2*rvir*3.08d24)\n",
	"   push, nelec, sqrt((4.0*!pi*norm[i]*(da*3.08d24*(1.0+z[i]))^2.)/(1.18*1d-14*vol))\n",
	"   push, neleclo, sqrt((0.5*normlo[i]/norm[i])^2.0)*nelec[i]\n",
	"   push, nelechi, sqrt((0.5*normhi[i]/norm[i])^2.0)*nelec[i]\n",
	"   push, pres, 2.0*nelec[i]*tx[i]*1.60219E-9\n",
	"   push, preslo, sqrt((neleclo[i]/nelec[i])^2.0+(txlo[i]/tx[i])^2.0)*pres[i]\n",
	"   push, preshi, sqrt((nelechi[i]/nelec[i])^2.0+(txhi[i]/tx[i])^2.0)*pres[i]\n",
	"   push, ent, tx[i]*(nelec[i])^(-2./3.)\n",
	"   push, entlo, sqrt((txlo[i]/tx[i])^2.0+((2./3.)*neleclo[i]/nelec[i])^2.0)*ent[i]\n",
	"   push, enthi, sqrt((txhi[i]/tx[i])^2.0+((2./3.)*nelechi[i]/nelec[i])^2.0)*ent[i]\n",
	"ENDFOR\n",
	"a = mrdfits('contbin_binmap.fits',0,hdr)\n",
	"ninds = n_elements(xbin)\n",
	"dim = size(a,/dimensions)\n",
	"outnh = fltarr(dim[0],dim[1])\n",
	"outtx = fltarr(dim[0],dim[1])\n",
	"outfe = fltarr(dim[0],dim[1])\n",
	"outdens = fltarr(dim[0],dim[1])\n",
	"outp = fltarr(dim[0],dim[1])\n",
	"outk = fltarr(dim[0],dim[1])\n",
	"outnorm = fltarr(dim[0],dim[1])\n",
	"outchisq = fltarr(dim[0],dim[1])\n",
	"dx = dim[0]-1\n",
	"dy = dim[1]-1\n",
	"FOR i = 0, dx DO BEGIN\n",
	"   FOR j = 0, dy DO BEGIN\n",
	"      IF ((a[i,j] GT ninds) OR (a[i,j] LT 0)) THEN BEGIN\n",
	"         a[i,j] = -1.0 \n",
	"         print, 'Do not understand bin = '+a[i,j]\n",
	"      ENDIF ELSE BEGIN \n",
	"         ind = a[i,j]\n",
	"         outnh[i,j] = nh[ind]\n",
	"         outtx[i,j] = tx[ind]\n",
	"         outfe[i,j] = fe[ind]\n",
	"         outp[i,j] = pres[ind]\n",
	"         outk[i,j] = ent[ind]\n",
	"         outdens[i,j] = nelec[ind]\n",
	"         outnorm[i,j] = norm[ind]\n",
	"         outchisq[i,j] = chisq[ind]\n",
	"      ENDELSE\n",
	"   ENDFOR\n",
	"ENDFOR\n",
	"print, 'Creating N_H map...'\n",
	"mwrfits,outnh,'nh_map.fits',hdr\n",
	"print, 'Creating Temperature map...'\n",
	"mwrfits,outtx,'tx_map.fits',hdr\n",
	"print, 'Creating Abundance map...'\n",
	"mwrfits,outfe,'fe_map.fits',hdr\n",
	"print, 'Creating Normalization map...'\n",
	"mwrfits,outnorm,'norm_map.fits',hdr\n",
	"print, 'Creating Chisq map...'\n",
	"mwrfits,outchisq,'chisq_map.fits',hdr\n",
	"print, 'Creating Density map...'\n",
	"mwrfits,outdens,'dens_map.fits',hdr\n",
	"print, 'Creating Entropy map...'\n",
	"mwrfits,outk,'k_map.fits',hdr\n",
	"print, 'Creating Pressure map...'\n",
	"mwrfits,outp,'p_map.fits',hdr\n",
	"END\n";
    close PRO;
    $idldir = $ENV{'IDL_DIR'};
    $pid = open3(\*IDL_INPUT, \*IDL_OUTPUT, \*IDL_ERR, "${idldir}/bin/idl") ||
	die "## ERROR: Cannot start IDL\n";
    print IDL_INPUT "print, '--Now in IDL--'\n";
    print IDL_INPUT "map\n";
    while (<IDL_OUTPUT>) {
	chomp;
	print "idl: $_\n";
	print IDL_INPUT "exit\n";
    }	
    print "idl: --Exiting IDL--\n";
    close IDL_INPUT;
    close IDL_OUTPUT;
    close IDL_ERR;
    waitpid($pid,0);
}

#######################
#######################
