sub extract_spectra {
  my($bgevt,$evt) = @_;
  my($nbins,$i,$backscal);

  # ensure the background file is in the working directory
  $command = "cp ../$rootdir/${bgevt} .";
  &myprint($command);
  system($command);

  # extract a spectrum of the bgd for use by this script
  $command = "punlearn dmextract; dmextract infile=\"${bgevt}\[bin pi\]\" outfile=mark_bgd.pi clobber=yes";
  myprint($command);
  system($command);

  # dump the nh and z values into a file for sherpa to use
  open(NHFILE,">n_h.dat") || die "Can't open n_h.dat\n";
  print NHFILE "${nh20}";
  close NHFILE;

  open(ZFILE,">redshift.dat") || die "Can't open redshift.dat\n";
  print ZFILE "${z}";
  close ZFILE;

  # determine the number of bins
  $nbins = &get_value(${binfile});

  # loop through all the bins
  $i = 1;
  while ($i < $nbins+1) {
    $evt = "evt2.fits.${i}";
    if ($oneresp eq "yes") { #&& -e "test_sou.wrmf") {
      print "Using only one response file to save time.\n";
      $command = "punlearn dmextract; dmextract infile=\"${evt}[bin pi]\" outfile=test_sou_unbinned.pi clobber=yes verbose=2";
      system($command);
      $command = "punlearn dmgroup; dmgroup infile=test_sou_unbinned.pi outfile=test_sou.pi grouptype=NUM_CTS grouptypeval=25 xcolumn=channel ycolumn=counts clobber=yes verbose=2 binspec=\'\'";
    } else {
      $command = "punlearn acisspec; acisspec soufile1=\"$evt\" root=test bgfile1=\"\" gtype=\"NUM_CTS\" gspec=25 clobber=yes verbose=2";
      system($command);
      $backscal = &get_value("${evt}.BACKSCAL");
      $command = "punlearn dmhedit; dmhedit infile=test_sou.pi filelist=none operation=add key=BACKSCAL value=${backscal}";
      system($command);
      $command = "punlearn dmhedit; dmhedit infile=test_sou.pi filelist=none operation=add key=BACKSCAL value=${backscal}";
      system($command);
      $command = "punlearn dmhedit; dmhedit infile=test_sou.pi filelist=none operation=add key=ANCRFILE value=test_sou.warf";
      system($command);
      $command = "punlearn dmhedit; dmhedit infile=test_sou.pi filelist=none operation=add key=RESPFILE value=test_sou.wrmf";
      system($command);
      $command = "punlearn dmhedit; dmhedit infile=mark_bgd.pi filelist=none operation=add key=ANCRFILE value=test_sou.warf";
      system($command);
      $command = "punlearn dmhedit; dmhedit infile=mark_bgd.pi filelist=none operation=add key=RESPFILE value=test_sou.wrmf";
      system($command);
    }

    # Execute the sherpa script
    $command = "sherpa --slscript fitsingle_speed.sl";
    system($command);
    $command = "cp singletemp.dat ${evt}.temp";
    system($command);
    $command = "singletempchi.dat ${evt}.chi";
    system($command);
    $command = "singletempdelta.dat ${evt}.delta";
    system($command);
    $command = "singletempcov.dat ${evt}.tempcov";
    system($command);
    $command = "singletemp.shp ${evt}.temp.shp";
    system($command);
    $command = "single_pwl.shp ${evt}.pwl.shp";
    system($command);
    $command = "test_sou.pi ${evt}.pi";
    system($command);
    $command = "test_sou.warf ${evt}.warf";
    system($command);
    $command = "test_sou.wrmf ${evt}.wrmf";
    system($command);

    $i = $i+1;
  }

}
