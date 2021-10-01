punlearn dmextract; dmextract infile="../data/sub.fits[bin sky=@pan10-10.reg]" outfile=rprofile.fits bkg="" clob+
punlearn dmtcalc; dmtcalc infile=rprofile.fits outfile=pan10.fits expression="ang=(0.5*(ROTANG[0]+ROTANG[1]))" clob+
punlearn dmextract; dmextract infile="../data/sub.fits[bin sky=@pan10-20.reg]" outfile=rprofile.fits bkg="" clob+
punlearn dmtcalc; dmtcalc infile=rprofile.fits outfile=pan20.fits expression="ang=(0.5*(ROTANG[0]+ROTANG[1]))" clob+
punlearn dmextract; dmextract infile="../data/sub.fits[bin sky=@pan10-30.reg]" outfile=rprofile.fits bkg="" clob+
punlearn dmtcalc; dmtcalc infile=rprofile.fits outfile=pan30.fits expression="ang=(0.5*(ROTANG[0]+ROTANG[1]))" clob+
frm a.fits
frm rprofile.fits
