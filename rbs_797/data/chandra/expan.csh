punlearn dmextract; dmextract infile="0.7-2.0_evt.fits[bin sky=@pan10.reg]" outfile=rprofile.fits bkg="" clob+
punlearn dmtcalc; dmtcalc infile=rprofile.fits outfile=pan10.fits expression="ang=(0.5*(ROTANG[0]+ROTANG[1]))" clob+
punlearn dmextract; dmextract infile="0.7-2.0_evt.fits[bin sky=@pan25.reg]" outfile=rprofile.fits bkg="" clob+
punlearn dmtcalc; dmtcalc infile=rprofile.fits outfile=pan25.fits expression="ang=(0.5*(ROTANG[0]+ROTANG[1]))" clob+
punlearn dmextract; dmextract infile="0.7-2.0_evt.fits[bin sky=@pan50.reg]" outfile=rprofile.fits bkg="" clob+
punlearn dmtcalc; dmtcalc infile=rprofile.fits outfile=pan50.fits expression="ang=(0.5*(ROTANG[0]+ROTANG[1]))" clob+
punlearn dmextract; dmextract infile="0.7-2.0_evt.fits[bin sky=@sbpan.reg]" outfile=rprofile.fits bkg="" clob+
punlearn dmtcalc; dmtcalc infile=rprofile.fits outfile=a.fits expression="ang=(0.5*(ROTANG[0]+ROTANG[1]))" clob+
punlearn dmtcalc; dmtcalc infile=a.fits outfile=pansb.fits expression="rmid=(0.5*(R[0]+R[1]))" clob+
frm a.fits
frm rprofile.fits
