#! /usr/bin/perl -w

#######################
#######################
##    Set Options    ##
#######################
#######################

# filename prefs
$extrspec  = "no";
$xspecfile = "therm.qdp";
$specfile  = "therm.dat";
$imgfile   = "plaw.fits";
$simdir    = "sims/";
$outdir    = "therm/";
$verb      = "no";
$psnum     = 1;

# basic stuff
$con      = 1;
$expt     = 20000;
$binsize  = "10";
$det      = "ACIS-S";
$grating  = "NONE";
$spectype = "FILE";
$src      = "IMAGE";                   # BETA or IMAGE
$userfile = "";
$userargs = "";
$nann     = 5;
$binning  = 25;                       # Binning specification for spectra
$wmaplo   = 300;                      # Lower bound for WMAP energy
$wmaphi   = 2000;                     # Upper bound for WMAP energy
$wmapbin  = "det=8";                  # Binning specification for WMAP
$cverb    = 2;


# XSPEC stuff
$xspecver = "xspec11";
$nhmod    = "wabs";
$thermod  = "mekal";
@cosmo    = ("70", "0.3", "0.7");
@zpars    = (0.05, 0.05, 0.01);   # start, stop, step
@txpars   = (4.0, 4.0, 4.0);     # start, stop, step
@beta     = (0.6, 20.0);         # beta, rc
#@plawpars = ("10.", "-0.42");    # norm, alpha
#                                 # for power-law K(r) means S(r) ~ r^(-5/6),
#                                 # which means Marx input 1/2*(-5/6)
#@xpars    = ("0.03", "0.3",      # nh, fe
#	     "0.1", "0.01",      # norm, emin
#	     "100.0", "3000",    # emax, ebins
#	     "lin");             # escale

# for PSF study only
 @plawpars = ("10.", "-0.42");    # norm, alpha
                                  # for power-law K(r) means S(r) ~ r^(-5/6),
                                  # which means Marx input 1/2*(-5/6)
 @xpars    = ("0.03", "0.3",      # nh, fe
 	     "6e-2", "0.05",     # norm, emin
 	     "12.0", "3000",     # emax, ebins
 	     "lin");             # escale

#######################
#######################
##   Main Program    ##
#######################
#######################

# check for CIAO being loaded
die "## ERROR: ciao is not loaded\n" unless ($ENV{'ASCDS_BIN'});
use POSIX qw(log10);
use Cwd;
use FindBin qw($Bin);

# make a dir to work within
mkdir($simdir,0777) unless (-d $simdir);
chdir($simdir) || die "\n## ERROR: No $simdir directory\n";

# get rid of old param files and get new ones
system("rm -rf $outdir z*.fits");
system("rm -rf xautosav.xcm temp.* *.par pgplot.ps list z*tx*.ps therm*");

# begin looping through redshift space
$txcount = 1;
$tx = $txpars[0];
while ($tx <= $txpars[1]) {
    $zcount = 1;
    $z = $zpars[0];
    while ($z <= $zpars[1]) {

	# define some filenames
	$xcmfile = "temp.xcm";
	$fak     = "temp.fak";
	$sbreg   = "temp.reg";
	$root    = "z${zcount}_tx${txcount}";
	$outfile = $root."_evt.fits";
	$outprof = $root."_sbprof_".$binsize."pix.fits";
	$outps   = $root."_sb.ps";
	$outasol = $root."_asol.fits";
	$outasp  = $root."_asphist.fits";
	system("rm -rf *.par");
	system("cp $ENV{MARX_PAR}/*.par .");

	# pick the image to use
	if ($det eq "ACIS-S") {
	    $arf = "$Bin/aciss_aimpt_cy10.arf";
	    $rmf = "$Bin/aciss_aimpt_cy10.rmf";
	    $detfile = "$Bin/img_aciss.fits";
	    $dim = 1024;
	    $rmax = 500;
	} elsif ($det eq "ACIS-I") {
	    $arf = "$Bin/acisi_aimpt_cy10.arf";
	    $rmf = "$Bin/acisi_aimpt_cy10.rmf";
	    $detfile = "$Bin/img_acisi.fits";
	    $dim = 2700;
	    $rmax = 900;
	} else {
	    die "## ERROR: You gave me a bad chip... dummy.\n";
	}

	# create new XCM file for running xspec
	$eta = $xpars[2] if ($zcount == 1);
	if ($src eq 'BETA') {
	    $crit = 200;
	    while ($crit > $con) {
		&sub_xcmfile();
		&sub_run_xspec();
		$target = $lum0/(1.0+($z-$zpars[0]))**(4.0);
		$frac = $lum/$target;
		$crit = abs($frac-1)*100;
		last if ($crit <= $con);
		
		# check that the last iteration did not satisfy the convergance criterion
		# for cases where $crit is greater than 0
		if ($frac > 1) {
		    if ($frac >= 1.5)       { $eta = $eta - 0.75*$eta;
		    } elsif ($frac >= 1.2)  { $eta = $eta - 0.40*$eta;
		    } elsif ($frac >= 1.1)  { $eta = $eta - 0.15*$eta;
		    } elsif ($frac >= 1.05) { $eta = $eta - 0.10*$eta;
		    } elsif ($frac < 1.05)  { $eta = $eta - 0.05*$eta;
		    } else { die "## ERROR: Don't know how to increment $frac\n";
		    }
		}
		if ($frac < 1) {
		    if ($frac <= 0.5)       { $eta = $eta + 0.75*$eta;
		    } elsif ($frac <= 0.8)  { $eta = $eta + 0.40*$eta;
		    } elsif ($frac <= 0.9)  { $eta = $eta + 0.15*$eta;
		    } elsif ($frac <= 0.95) { $eta = $eta + 0.10*$eta;
		    } elsif ($frac > 0.95)  { $eta = $eta + 0.05*$eta;
		    } else { die "## ERROR: Don't know how to increment $frac\n";
		    }
		}

		# echo the progress of the fitter in quiet mode
		print "## Lum0:  $lum0 10^44 ergs/s\n";
		print "## Lum\':  $lum 10^44 ergs/s\n";
		$out1 = sprintf("%.3f",$target);
		print "## Target: $out1 10^44 ergs/s\n";
		$out2 = sprintf("%.1f",$crit);
		print "## Ratio: $out2\%\n";
	    }
	} else {
	    &sub_xcmfile();
	    &sub_run_xspec();
	    $target = 0.0;
	    $crit = 0.0;
	}
	
	# print stuff
	print "## Lum0:  $lum0 10^44 ergs/s\n";
	print "## Lum\':  $lum 10^44 ergs/s\n";
	$out1 = sprintf("%.3f",$target);
	print "## Target: $out1 10^44 ergs/s\n";
	$out2 = sprintf("%.1f",$crit);
	print "## Ratio: $out2\%\n";
	print "\n#### SPECTRAL INFORMATION ####\n";
	print "##\n";
	print "## redshift: $z\n";
	print "## Photon Flux:   $pflux photons/cm^2/sec\n";
	print "## CGS Flux:      $eflux ergs/cm^2/sec\n";
	print "## Baseline Lum:  $lum0 10^44 ergs/s\n";
	print "## Luminosity:    $lum 10^44 ergs/s\n";
	print "##\n";
	print "##############################\n\n";

	# convert xspec output into marx output
	unlink("$specfile");
	system("$ENV{MARX_HOME}/lib/marx/xspec2marx $xspecfile > $specfile");

	# set model options, run marx, and then convert output to FITS
	# format
	&sub_set_opt();
	system("marx"); die "## ERROR: No FITS files to work with\n" unless (-d $outdir);
	system("marx2fits $outdir ${outfile}");

	# make spectra?
	if ($extrspec eq "yes"){

	    # create asphist
	    &sub_asphist();

	    # make regions, extract spectra, and fit
	    &sub_spectra();
	}

	# extract sur bri profile
	&sub_extr_sbr();

	# update header with spec info
	&sub_upheader();

	# plot the sbr profs
	&sub_plot_sbr();

	# increment counters
	$z = $z + $zpars[2];
	$zcount++;
    }
    # increment counters
    $tx = $tx + $txpars[2];
    $txcount++;
}

# clean-up
system("ls -1ct z*tx*.ps > list");
system("cat list | perl $ENV{HOME}/research/redux/scripts/pscat.pl $psnum all.ps");
system("rm -rf xautosav.xcm temp.* *.par pgplot.ps list therm*");

# exit cleanly
exit 0;

#######################
#######################
##   Sub-Routines    ##
#######################
#######################

sub sub_myprint {
    my $line;
    foreach $line (@_) {
	print $line;
	print XSPEC_INPUT $line;
    }
}

#######################
#######################

sub sub_xcmfile {

    # print to XCM file
    unlink "$xcmfile";
    unlink "$fak";
    open(XCMFILE,">$xcmfile");
    print XCMFILE "query yes\n";
    print XCMFILE "cosmo $cosmo[0] $cosmo[1] $cosmo[2]\n";
    my $modstring = "${nhmod}($thermod)";
    my $moline = "& $xpars[0] & $tx & 1.0 & $xpars[1],0 & $z & 0 & $eta /*";
    print XCMFILE "model $modstring $moline\n";
    print XCMFILE "dummyrsp $xpars[3] $xpars[4] $xpars[5] $xpars[6]\n";
    print XCMFILE "tclout rate\n";
    print XCMFILE "puts \"Rate \$xspec_tclout\"\n";
    print XCMFILE "lumin $xpars[3] $xpars[4] $z\n";
    print XCMFILE "tclout lumin\n";
    print XCMFILE "puts \"Lumin \$xspec_tclout\"\n";
    print XCMFILE "flux\n";
    print XCMFILE "tclout flux\n";
    print XCMFILE "puts \"Flux \$xspec_tclout\"\n";
    print XCMFILE "puts \"Data now\"\n";
    close XCMFILE;
}

#######################
#######################

sub sub_run_xspec {

    # get rid of old files
    unlink("$fak");
    unlink("$xspecfile");

    # starts xspec (perl and read and write from/to it)
    # $pid is a global variable that can be used to kill xspec
    use IPC::Open3;
    $pid = open3(*XSPEC_INPUT,*XSPEC_OUTPUT,*XSPEC_ERR,"$xspecver") || die "Can't run xspec\n";

    # open the xcmfile written earlier and feed to xspec
    sub_myprint "\@${xcmfile}\n";
    sub_myprint "query no\n";

    while (<XSPEC_OUTPUT>) {
	chomp;
	$line = $_;
	print "xspec: $line\n" if ($verb eq "yes");
	if ($line =~ m/^Rate/) {
	    @fakerate = split(/\s+/,$line);
	    $fakerate = $fakerate[1];
	    $fakerate =~ s/\+\/\-//;
	}
	if ($line =~ m/^Lumin/) {
	    @line = split(" ",$line);
	    $lum = sprintf("%.3f",$line[1]);
	}
	if ($line =~ m/\s+Model\s+flux/) {
	    @line = split(" ",$line);
	    $pflux = sprintf("%.3e",$line[2]); # MARX wants flux in photons/s/cm**2
	}
	if ($line =~ m/^Flux/) {
	    @line = split(" ",$line);
	    $eflux = sprintf("%.3e",$line[1]);
	}
 	if ($line =~ m/^Data/) {
 	    sub_myprint
		"cpd /vcps\n",
		"iplot model\n",
		"wdata $xspecfile\n",
		"cpd /null\n",
		"exit\n",
		"exit\n\n";
 	}
    }
    $lum0 = $lum if ($zcount == 1);

    # close the pipe
    close XSPEC_INPUT;
    close XSPEC_OUTPUT;
    close XSPEC_ERR;
    waitpid($pid,0);
}

#######################
#######################

sub sub_set_opt {

    # set basic params
    $command = "pset marx.par "
	."ExposureTime=$expt "
	."OutputDir=\'$outdir\' "
	."GratingType=\'$grating\' "
	."DetectorType=\'$det\' "
	."SpectrumType=\'$spectype\' "
	."SpectrumFile=\'$specfile\' "
	."SourceType=\'$src\' "
	."SourceFlux=-1 "
	."Verbose=\'$verb\'";

    # set options for several source type
    if ($src eq "BETA") {
	open(PROFILE,">temp.pro");
	print PROFILE "\!quiet=1\n";
	print PROFILE "cosmology,'$z',result,/silent\n";
	print PROFILE "openw,1,'temp.log'\n";
	print PROFILE "printf,1,strcompress(result[4],/remove_all)\n";
	print PROFILE "close,1\n";
	print PROFILE "exit \n";
	close PROFILE;
	system("\$IDL_DIR/bin/idl temp.pro");
	open(FILE,"temp.log");
	while(<FILE>) {
	    chomp;
	    next if (/^\#/);
	    next if (/^$/);
	    s/^\s+//;
	    s/\s+$//;
	    @line = split;
	    $conv = $line[0];
	}
	close FILE;
	delete $ENV{'DYLD_BIND_AT_LAUNCH'};
	$rc = $beta[1]/($conv*0.492);
	$command .= " S-BetaCoreRadius=$rc S-BetaBeta=$beta[0]";
    }

    # for an image
    if ($src eq "IMAGE") {
      open(PROFILE,">temp.pro");
      print PROFILE "\!quiet=1\n";
      print PROFILE "make_plaw,'$detfile','$imgfile',$dim,$plawpars[0],$plawpars[1]\n";
      print PROFILE "exit \n";
      close PROFILE;
      system("\$IDL_DIR/bin/idl temp.pro");
      $command .= " S-ImageFile=\'$imgfile\'";
      chomp($ranom = `dmkeypar $imgfile RA_NOM ; pget dmkeypar value`);
      chomp($decnom = `dmkeypar $imgfile DEC_NOM ; pget dmkeypar value`);
      $rollnom = 0.0;
      $command .= " SourceRA=$ranom SourceDEC=$decnom";
      $command .= " RA_Nom=$ranom Dec_Nom=$decnom Roll_Nom=$rollnom";
    }
    $command .=	" UserSourceFile=\'$userfile\' UserSourceArgs=\'$userargs\'" if ($src eq "USER");
    print "## Running MARX...\n";
    system($command);
}

#######################
#######################

sub sub_asphist {

    my $command = "marxasp MarxDir=\"${outdir}\" OutputFile=\"${outasol}\" TimeDel=0.256 RA_Sigma=0.12 Dec_Sigma=0.12 Roll_Sigma=0.12";
    system($command);
    $command = "asphist infile=\"${outasol}\" outfile=\"${outasp}\" evtfile=\"${outfile}\" dtffile=\"\" verbose=$cverb clobber=yes";
    system($command);
}

#######################
#######################

sub sub_spectra {

    my($width,$srcra,$srcdec,
       $ra1,$ra2,$ra3,
       $dec1,$dec2,$dec3);

    # find center of regions
    chomp($srcra = `dmkeypar $outfile RA_TARG ; pget dmkeypar value`);
    chomp($srcdec = `dmkeypar $outfile DEC_TARG ; pget dmkeypar value`);
    $srcra =~ sprintf("%f",$srcra);
    $srcdec =~ sprintf("%f",$srcdec);
    $ra1 = sprintf("%i",$srcra/15.0);
    $ra2 = sprintf("%i",($srcra-($ra1*15.0))*4.0);
    $ra3 = sprintf("%.2f",($srcra-$ra1*15.0-$ra2/4.0)*240.0);
    $ra = join(":",$ra1,$ra2,$ra3);
    $dec1 = sprintf("%i",$srcdec);
    $dec2 = sprintf("%i",(abs($srcdec-$dec1)*60.0));
    $dec3 = sprintf("%.2f",((abs($srcdec-$dec1)*60.0)-$dec2)*60.0);
    $dec = join(":",$dec1,$dec2,$dec3);

    # determine width of ann
    $width = sprintf("%.2f",$rmax/$nann);
    $command = "punlearn mkwarf; pset mkwarf asolfile=$outasol";
    system($command);
    my $rin = 0.0;
    for ($i = 1; $i <= $nann; $i++) {
	$num = sprintf("%i",$i);
	$outspec = $root."_ann".$num;
	$rout = $rin+$width;
	my $pri = $rin*0.492/60.0;
	my $pro = $rout*0.492/60.0;
	$command = "punlearn specextract; specextract infile=\"$outfile\[sky=annulus($ra,$dec,${pri}\',${pro}\')]\" "
	    ."outroot=$outspec bkgfile=\"\" energy_wmap=${wmaplo}:${wmaphi} binwmap=$wmapbin "
	    ."grouptype=\"NUM_CTS\" binspec=$binning ptype=PI clobber=yes verbose=$cverb";
	system($command);
    }

    # TODO: fit the spectra with Xspec
}

#######################
#######################

sub sub_extr_sbr {

    my(@lines,$rin,$rold,$totcts,$line,
       $pixel,$srcra,$srcdec,
       $ra1,$ra2,$ra3,
       $dec1,$dec2,$dec3);

    # make the surface brightness region
    chomp($srcra = `dmkeypar $outfile RA_TARG ; pget dmkeypar value`);
    chomp($srcdec = `dmkeypar $outfile DEC_TARG ; pget dmkeypar value`);
    $srcra =~ sprintf("%f",$srcra);
    $srcdec =~ sprintf("%f",$srcdec);
    $ra1 = sprintf("%i",$srcra/15.0);
    $ra2 = sprintf("%i",($srcra-($ra1*15.0))*4.0);
    $ra3 = sprintf("%.2f",($srcra-$ra1*15.0-$ra2/4.0)*240.0);
    $ra = join(":",$ra1,$ra2,$ra3);
    $dec1 = sprintf("%i",$srcdec);
    $dec2 = sprintf("%i",(abs($srcdec-$dec1)*60.0));
    $dec3 = sprintf("%.2f",((abs($srcdec-$dec1)*60.0)-$dec2)*60.0);
    $dec = join(":",$dec1,$dec2,$dec3);

    # calculate annuli to use
    @lines = ();
    $rin = 0;
    $rold = 0;
    while ($rin < $rmax) {
	$rout = $rin+$binsize;
	$pri = $rin*0.492/60.0;
	$pro = $rout*0.492/60.0;
	$line = "annulus($ra,$dec,${pri}\',${pro}\')\n";
	push @lines,$line;
	$rold = $rin;
	$rin = $rout;
    }

    # reset final outer bin to rmax
    pop @lines;
    $pri = $rold*0.492/60.0;
    $pro = $rmax*0.492/60.0;
    push @lines,"annulus($ra,$dec,${pri}\',${pro}\')\n";

    # write region file for annuli extraction
    open(SBREG,">$sbreg");
    print SBREG @lines;
    close SBREG;

    # extract the profile
    system("punlearn dmextract ; dmextract \"${outfile}\[bin sky=\@$sbreg\]\" ${outprof} clobber=yes verbose=$cverb");
    system("punlearn dmtcalc ; dmtcalc infile=${outprof} outfile=${outprof} expression=\"rmid=0.5*(R[0]+R[1])\" clobber=yes verbose=$cverb");
    system("punlearn dmtcalc ; dmtcalc infile=${outprof} outfile=${outprof} expression=\"rin=R[0]\" clobber=yes verbose=$cverb");
    system("punlearn dmtcalc ; dmtcalc infile=${outprof} outfile=${outprof} expression=\"rout=R[1]\" clobber=yes verbose=$cverb");
}

#######################
#######################

sub sub_upheader {

    open(PROFILE,">temp.pro");
    print PROFILE "\!quiet=1\n"
	."a = mrdfits('${outprof}',1)\n"
	."cts = a.net_counts\n"
	."cts = total(cts)\n"
	."hout = headfits('${outfile}',/ext)\n"
	."hprof = headfits('${outprof}',/ext)\n"
	."sxaddpar,hout,\'FAKCTS\',cts,'Total cts'\n"
	."sxaddpar,hprof,\'FAKCTS\',cts,'Total cts'\n"
	."sxaddpar,hout,\'FAKZ\',${z},'Redshift'\n"
	."sxaddpar,hprof,\'FAKZ\',${z},'Redshift'\n"
	."sxaddpar,hout,\'FAKN\',${eta},'Normalization'\n"
	."sxaddpar,hprof,\'FAKN\',${eta},'Normalization'\n"
	."sxaddpar,hout,\'FAKTX\',${tx},'Tx [keV]'\n"
	."sxaddpar,hprof,\'FAKTX\',${tx},'Tx [keV]'\n"
	."sxaddpar,hout,\'FAKFE\',$xpars[1],'Z/Z_solar'\n"
	."sxaddpar,hprof,\'FAKFE\',$xpars[1],'Z/Z_solar'\n"
	."sxaddpar,hout,\'FAKNH\',$xpars[0],'N_HI [10^20 cm^-3]'\n"
	."sxaddpar,hprof,\'FAKNH\',$xpars[0],'N_HI [10^20 cm^-3]'\n"
	."sxaddpar,hout,\'FAKLUM\',${lum},'0.01-100 keV Lum [10^44 e/s]'\n"
	."sxaddpar,hprof,\'FAKLUM\',${lum},'0.01-100 keV Lum [10^44 e/s]'\n";
    if ($src eq 'BETA') {
	print PROFILE "sxaddpar,hout,\'FAKEFLUX\',${eflux},'CGS Flux [e/s/cm^2]'\n"
	    ."sxaddpar,hprof,\'FAKEFLUX\',${eflux},'CGS Flux [e/s/cm^2]'\n"
	    ."sxaddpar,hout,\'FAKPFLUX\',${pflux},'Photon Flux [ph/s/cm^2]'\n"
	    ."sxaddpar,hprof,\'FAKPFLUX\',${pflux},'Photon Flux [ph/s/cm^2]'\n"
	    ."sxaddpar,hout,\'FAKPRC\',$beta[1],'r_core [kpc]'\n"
	    ."sxaddpar,hprof,\'FAKPRC\',$beta[1],'r_core [kpc]'\n"
	    ."sxaddpar,hout,\'FAKRC\',${rc},'r_core [pixels]'\n"
	    ."sxaddpar,hprof,\'FAKRC\',${rc},'r_core [pixels]'\n"
	    ."sxaddpar,hout,\'FAKBETA\',$beta[0],'beta'\n"
	    ."sxaddpar,hprof,\'FAKBETA\',$beta[0],'beta'\n";
    }
    print PROFILE "modfits,'${outfile}',0,hout,/ext\n"
	."modfits,'${outprof}',0,hprof,/ext\n"
	."exit \n";
    close PROFILE;
    system("\$IDL_DIR/bin/idl temp.pro");
    system("rm -f temp.pro");
}

#######################
#######################

sub sub_plot_sbr {

    open(PROFILE,">temp.pro");
    print PROFILE "\!quiet=1\n"
	."fits   = mrdfits('${outprof}',1)\n"
	."head   = headfits('${outprof}',ext=1)\n"
	."rmid   = fits.rmid*0.492\n"
	."exp    = fits.exposure\n"
	."surbri = fits.sur_bri/exp/(0.492^2.)\n"
	."sbrerr = fits.sur_bri_err/exp/(0.492^2.)\n"
	."t = sxpar(head,'FAKTX')\n"
	."z = sxpar(head,'FAKZ')\n"
	."n = sxpar(head,'FAKN')\n"
	."c = sxpar(head,'FAKCTS')\n"
	."l = sxpar(head,'FAKLUM')\n"
	."f = sxpar(head,'FAKEFLUX')\n"
	."set_plot, \'PS\'\n"
	."device, filename = '${outps}', \$\n"
	."        /color, \$\n"
	."        /portrait, \$\n"
	."        /helvetica\n"
	."        !FANCY    = 4\n"
	."        !LINETYPE = 0\n"
	."        !P.FONT   = 0\n"
	."        !X.THICK  = 3\n"
	."        !Y.THICK  = 3\n"
	."        !Z.THICK  = 3\n"
	."!xtitle = textoidl(\'R_{mid} [arcsec]\')\n"
	."!ytitle = textoidl(\'Surface Brightness [cts arcsec^{-2} sec^{-1}]\')\n"
	."!mtitle = textoidl(\'Redshift: ${z}   T_X: ${tx} keV\')\n"
	."xmin = 0.90*min(rmid)\n"
	."xmax= 1.1*max(rmid)\n"
	."ord = where(surbri-sbrerr GT 0.)\n"
	."yt = surbri[ord]-sbrerr[ord]\n"
	."ymin = 0.8*min(yt)\n"
	."ymax = 1.2*max(surbri+sbrerr)\n"
	."yticks = LOGLEVELS([ymin,ymax],/fine)\n"
	."ynticks = N_Elements(yticks)\n"
	."xticks = LOGLEVELS([xmin,xmax],/fine)\n"
	."xnticks = N_Elements(xticks)\n"
	."plotsym, 0, 0.5, /fill, color=0\n"
	."plot, rmid, surbri, \$\n"
	."      /nodata, \$\n"
	."      xran = [xmin,xmax], \$\n"
	."      yran = [ymin,ymax], \$\n"
	."      /xlog, /ylog, \$\n"
	."      /xsty, /ysty, \$\n"
	."      yticks = ynticks-1, \$\n"
	."      ytickv = yticks, \$\n"
	."      xticks = xnticks-1, \$\n"
	."      xtickv = xticks, \$\n"
	."      charsize = 0.8\n"
	."oplot, rmid, surbri, psym=8, color=0\n"
	."oplot, rmid, surbri, psym=0, color=0\n"
	."oploterror, rmid, surbri, sbrerr, psym=8, errcolor=0\n"
	."items = [textoidl('T_X: '+num2str(t)+' keV'),\$\n"
	."         textoidl('\\eta: '+num2str(n)),\$\n"
	."         textoidl('Counts: '+num2str(c)),\$\n"
	."         textoidl('flux: '+num2str(f)+' ergs/sec/cm^2'),\$\n"
	."         textoidl('L_{bol}: '+num2str(l)+' 10^{44} ergs/sec')]\n"
	."linearr = replicate(-99,n_elements(items))\n"
	."psyarr = replicate(-99,n_elements(items))\n"
	."legend, items, linestyle=linearr, psym=psyarr, box=0, charsize=0.8, /top, /right\n";


    if ($src eq "BETA") {
	print PROFILE "y = max(surbri)*(1.0+(rmid/${rc})^2.0)^(-3.0*$beta[0]+0.5)\n";
	print PROFILE "oplot, rmid, y, linestyle=2\n";
    }
    if ($src eq "IMAGE") {
	print PROFILE "y = max(surbri)*rmid^(-5.0/6.0)\n";
	print PROFILE "oplot, rmid, y, linestyle=2\n";
    }

    # close device and exit
    print PROFILE "device, /close\n";
    print PROFILE "exit \n";
    close PROFILE;
    system("\$IDL_DIR/bin/idl temp.pro");
}

#######################
#######################

#	if ($src eq 'BETA') {
#	    $eta = 10**(-2.25879*log10($z)-4.47889);
#	} else {
#    my $fakline = "fakeit none & $rmf & $arf & y & \\n & $fak & $expt /*";
#      chomp($simx = `dmkeypar $imgfile SIM_X ; pget dmkeypar value`);
#      chomp($simy = `dmkeypar $imgfile SIM_Y ; pget dmkeypar value`);
#      chomp($srcra = `dmkeypar $imgfile RA_TARG ; pget dmkeypar value`);
#      chomp($srcdec = `dmkeypar $imgfile DEC_TARG ; pget dmkeypar value`);
#      chomp($simz = `dmkeypar $imgfile SIM_Z ; pget dmkeypar value`);
#      $srcra   = 250.15037;
#      $srcdec  = -53.750179;
#      $simz    = 0.0;
#      $command .= " DetOffsetX=$simx DetOffsetY=$simy DetOffsetZ=$simz";
#      $command .= " SourceRA=$srcra SourceDEC=$srcdec";
#      $command .= " DetOffsetX=0 DetOffsetY=0 DetOffsetZ=0";
