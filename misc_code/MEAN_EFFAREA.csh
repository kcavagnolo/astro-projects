#!/bin/tcsh -f

if ( $#argv != 3 ) then
 cat <<EOF 

This program determines the average effective area of an arf
over a range of energies.  If the input energy range is terminates 
in the middle of a energy bin, the bin is given a fractional 
weighting proportional to the amount of the bin included.

use MEAN_EFFAREA.csh arf low_energy high_energy

Energies in keV

EOF
 exit
endif



set arf = $1
set elo = $2
set ehi = $3

fcalc \
 infile = $arf \
 outfile = /tmp/temparf1.fits \
 clname = SPECRESP \
 expr = "SPECRESP*(ENERG_LO.gt.$elo.AND.ENERG_HI.lt.$ehi)" \
 clobber = yes mode=q
 
fcalc \
 infile = $arf \
 outfile = /tmp/temparf2.fits \
 clname = USE \
 expr = "1*(ENERG_LO.gt.$elo.AND.ENERG_HI.lt.$ehi)" \
 clobber = yes mode=q

set TOT_AREA = `fstatistic /tmp/temparf1.fits colname=SPECRESP rows = - | grep sum |  awk '{print $8}'` 
set TOT_BINS = `fstatistic /tmp/temparf2.fits colname=USE rows = - | grep sum |  awk '{print $8}'`

# NOW CHECK THE ENDS
fcalc \
 infile = $arf \
 outfile = /tmp/temparf1a.fits \
 clname = SPECRESP \
 expr = "SPECRESP*(ENERG_LO.lt.$elo.AND.ENERG_HI.gt.$elo)" \
 clobber = yes mode=q

set LOW_AREA = `fstatistic /tmp/temparf1a.fits colname=SPECRESP rows = - | grep sum |  awk '{print $8}'`
set check = `echo "scale=2; $LOW_AREA" | bc -l`
if ( $check != 0 ) then
 fcalc \
  infile = $arf \
  outfile = /tmp/temparf2a.fits \
  clname = USE \
  expr = "ENERG_LO*(ENERG_LO.lt.$elo.AND.ENERG_HI.gt.$elo)" \
  clobber = yes mode=q
 set LOW_CHANNEL = `fstatistic /tmp/temparf2a.fits colname=USE rows = - | grep sum |  awk '{print $8}'`
 fcalc \
  infile = $arf \
  outfile = /tmp/temparf2a.fits \
  clname = USE \
  expr = "ENERG_HI*(ENERG_LO.lt.$elo.AND.ENERG_HI.gt.$elo)" \
  clobber = yes mode=q
 set HI_CHANNEL = `fstatistic /tmp/temparf2a.fits colname=USE rows = - | grep sum |  awk '{print $8}'`
 set CHANNEL_WIDTH = `echo $HI_CHANNEL $LOW_CHANNEL | awk '{print $1 - $2}'`
 set USED_WIDTH = `echo $HI_CHANNEL $elo  | awk '{print $1 - $2}'`
 set WEIGHT = `echo $USED_WIDTH $CHANNEL_WIDTH | awk '{print $1/$2}'`
 set WEIGHT_AREA = `echo $LOW_AREA $WEIGHT | awk '{print $1*$2}'`

 set TOT_AREA = `echo $TOT_AREA $WEIGHT_AREA | awk '{print $1+$2}'`
 set TOT_BINS = `echo $TOT_BINS $WEIGHT | awk '{print $1+$2}'`
endif

fcalc \
 infile = $arf \
 outfile = /tmp/temparf1b.fits \
 clname = SPECRESP \
 expr = "SPECRESP*(ENERG_LO.lt.$ehi.AND.ENERG_HI.gt.$ehi)" \
 clobber = yes mode=q
set HIGH_AREA = `fstatistic /tmp/temparf1b.fits colname=SPECRESP rows = - | grep sum |  awk '{print $8}'`
set check = `echo "scale=2; $HIGH_AREA" | bc -l`
if ( $check != 0 ) then
 fcalc \
  infile = $arf \
  outfile = /tmp/temparf2b.fits \
  clname = USE \
  expr = "ENERG_LO*(ENERG_LO.lt.$ehi.AND.ENERG_HI.gt.$ehi)" \
  clobber = yes mode=q
 set LOW_CHANNEL = `fstatistic /tmp/temparf2b.fits colname=USE rows = - | grep sum |  awk '{print $8}'`
 fcalc \
  infile = $arf \
  outfile = /tmp/temparf2b.fits \
  clname = USE \
  expr = "ENERG_HI*(ENERG_LO.lt.$ehi.AND.ENERG_HI.gt.$ehi)" \
  clobber = yes mode=q

 set HI_CHANNEL = `fstatistic /tmp/temparf2b.fits colname=USE rows = - | grep sum |  awk '{print $8}'`
 set CHANNEL_WIDTH = `echo $HI_CHANNEL $LOW_CHANNEL | awk '{print $1 - $2}'`
 set USED_WIDTH = `echo $ehi $LOW_CHANNEL | awk '{print $1 - $2}'`
 set WEIGHT = `echo $USED_WIDTH $CHANNEL_WIDTH | awk '{print $1/$2}'`
 set WEIGHT_AREA = `echo $HIGH_AREA $WEIGHT | awk '{print $1*$2}'`

 set TOT_AREA = `echo $TOT_AREA $WEIGHT_AREA | awk '{print $1+$2}'`
 set TOT_BINS = `echo $TOT_BINS $WEIGHT | awk '{print $1+$2}'`
endif



set NET_AREA = `echo $TOT_AREA $TOT_BINS | awk '{print $1/$2}'`

echo $NET_AREA
 
