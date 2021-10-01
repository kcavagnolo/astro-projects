#!/bin/csh
# REQUIREMENTS: 
#  - CIAO contributed scripts have to be installed (>3.1)

source ciao.csh -o

# Start the bin index n at "1"
set n
@ n=1

# Stop when n reaches "NUMBER_OF_BINS
set NUMBER_OF_BINS 
@ NUMBER_OF_BINS = `more nbins.dat`


while ($n < `expr $NUMBER_OF_BINS + 1`) 
  setenv EVTFILE evt2.fits.${n}

  #Run the acisspec script
  punlearn acisspec 
  if ( -f use_one_rmf && -f test_sou.wrmf ) then
    echo Using only one response file, for time saving.
    punlearn dmextract
    dmextract infile="${EVTFILE}[bin pi]" outfile=test_sou_unbinned.pi clobber=yes verbose=2
    # Group to a minimum of 20 counts per bin
    punlearn dmgroup
    dmgroup infile=test_sou_unbinned.pi outfile=test_sou.pi grouptype=NUM_CTS grouptypeval=20 xcolumn=channel ycolumn=counts clobber=yes verbose=2 binspec=''
  else
    acisspec soufile1="$EVTFILE" root=test gtype="NUM_CTS" gspec=20 clobber=yes verbose=2
  endif

  # Set the BACKSCAL header keywords to account for the different areas
  set BACKSCAL=`more ${EVTFILE}.BACKSCAL`
  dmhedit infile=test_sou.pi filelist=none operation=add key=BACKSCAL value=$BACKSCAL
  punlearn dmhedit
  dmhedit infile=test_sou.pi filelist=none operation=add key=BACKSCAL value=$BACKSCAL
  punlearn dmhedit
  dmhedit infile=test_sou.pi filelist=none operation=add key=ANCRFILE value=test_sou.warf
  punlearn dmhedit
  dmhedit infile=test_sou.pi filelist=none operation=add key=RESPFILE value=test_sou.wrmf
  punlearn dmhedit
  dmhedit infile=mark_bgd.pi filelist=none operation=add key=ANCRFILE value=test_sou.warf
  punlearn dmhedit
  dmhedit infile=mark_bgd.pi filelist=none operation=add key=RESPFILE value=test_sou.wrmf

  # Execute the sherpa script
  sherpa --slscript fitsingle_speed.sl

  /bin/cp singletemp.dat ${EVTFILE}.temp
  /bin/cp singletempchi.dat ${EVTFILE}.chi
  /bin/cp singletempdelta.dat ${EVTFILE}.delta
  /bin/cp singletempcov.dat ${EVTFILE}.tempcov
  /bin/cp singletemp.shp ${EVTFILE}.temp.shp
  /bin/cp single_pwl.shp ${EVTFILE}.pwl.shp
  /bin/cp test_sou.pi ${EVTFILE}.pi
  /bin/cp test_sou.warf ${EVTFILE}.warf
  /bin/cp test_sou.wrmf ${EVTFILE}.wrmf

  @ n=($n + 1)
end








