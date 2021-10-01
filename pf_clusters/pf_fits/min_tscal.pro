PRO min_tscal, dat1

;#################################
;#################################
;# usage:                       ;#
;# IDL> min_tscal, 'done.list'  ;#
                                ;#
myhome   = GETENV('HOME')       ;#
!QUIET   = 1                    ;# stop the screen vomit, =0 keeps it
ksigma   = 1.0                  ;# sigma above zero to report
logcolor = 'yes'                 ;# colored log space plots
plkmm    = 'no'                 ;# plot the KMM distros
pltc0    = 'no'                 ;# overplot best-fit t0 onto k0 dist
space    = 'fit'                ;# type of data to use: 'obs' or 'fit'
ktype    = 'flat'               ;# type of K to use: 'flat' or 'itpl'
model    = 'nonzero'            ;# type of model to use: 'zero' or 'nonzero'
gaplim   = 50.                  ;# place to divide the sample when doing stats
rc       = 20.                  ;# radius to evaluate K(rc) = K0 + K100*(rc/100)^alpha
gapmin   = 30.                  ;# minimum K0 to log about cluster being in gap
gapmax   = 60.                  ;# maximum K0 to log about cluster being in gap
runkmm   = 'no'                 ;# run the kmm test inside this script?
k0bin    = 0.15                 ;# binning factor for K0 histogram **LOG SPACE**
krcbin   = 0.15                 ;# binning factor for K0 histogram **LOG SPACE**
obsbin   = 0.15                 ;# binning factor for tcool histogram **LOG SPACE**
tcbin    = 0.20                 ;# binning factor for tcool histogram **LOG SPACE**
abinning = 0.05                 ;# binning factor for alpha histogram **LOG SPACE**
zfilter  = 'no'                 ;# make a cut in redshift space
zmin     = 0.40                 ;# redshift to cut at
zmax     = 10.40                 ;# redshift to cut at
txfilter = 'no'                 ;# make a cut in redshift space
txmin    = 0.50                 ;# redshift to cut at
txmax    = 14.0                 ;# redshift to cut at
dofilter = 'no'                 ;# should the K0 values be filtered for histogram plotting?
filter0  = 50.                  ;# first filter: for K0 less than XX err must be...
filter1  = 10.                  ;# second filter: ...K0 err must be less than YY keV cm**2 else...
filter2  = 20.                  ;# third filter: ...for all other K0, err must be less than ZZ%
csize    = 1.0                  ;# size of characters in plots
hcolor   = 90                   ;# color to fill histograms
kstep    = 1                    ;# step size for cumulative profile
obsstep  = 10                   ;# step size for cumulative profile
tcstep   = 10                   ;# step size for cumulative profile
                                ;# name of file containing temperature profiles
txfile   = myhome+'/research/pf_clusters/pf_fits/dat/pf_temp_profs.dat'
                                ;# name of file containing best-fit t0
tc0file  = myhome+'/research/pf_clusters/pf_fits/dat/t0_fits.dat'
                                ;#
;#################################
;#################################

;# setup the fancy plotting options
!FANCY    = 4
!LINETYPE = 0
!P.FONT   = 0
!X.THICK  = 3
!Y.THICK  = 3
!Z.THICK  = 3

;# load all the templates needed to read data
restore,myhome+'/research/redux/scripts/xspectemp_rin_normerr_src.sav'
restore,myhome+'/research/redux/scripts/s_tabletemplate.sav'
restore,myhome+'/research/redux/scripts/s_resultstemplate.sav'
restore,myhome+'/research/redux/scripts/tcooltemplate.sav'

;# read in list of clusters to work with
readcol, dat1, FORMAT='A,A,F,F,F,F,F,F,F,F,F,A,F,A,F,A', comment='#', $
         cluster,obsids,x,y,rmax,mincts,zs,nhs,txs,fes,lbols,chips,eobss,diffs,robs,loc
readcol, tc0file, FORMAT='A,A,A,F,F,F,F,F,F,F,F,F,F,F', comment='#', $
         tc0clusters,tc0obsids,tc0modes,tc0rmins,tc0rmaxs,tc0anns,tc0s,tc0errs,t100s,$
         t100errs,tcplaws,tcplawerrs,tcchisqs,tcprobs
fitfile = read_ascii(txfile,template=xspectemp_rin_normerr_src)

;# get values specific to model
IF (ktype EQ 'flat') THEN BEGIN
   IF (model EQ 'nonzero') THEN ind = 2 ELSE ind = 3
ENDIF
IF (ktype EQ 'itpl') THEN BEGIN
   IF (model EQ 'nonzero') THEN ind = 0 ELSE ind = 1
ENDIF

;# open a log to send k0 values to
OPENW, KMM, "kmm_k0.in", /GET_LUN
OPENW, KMM2, "kmm_tc.in", /GET_LUN
OPENW, GAP, "gapk0.log", /GET_LUN
OPENW, ODD, "odd.log", /GET_LUN
printf, GAP, "# We're in your gap, screwin' with the bimodality"
printf, GAP, FORMAT='(A-20,A20,2A10)',"#Name","Obsid","K0","K0err"
printf, ODD, format='(A-20,A20,4A10,4A10)',"#Name","Obsid","K0","K0err","K100","K100err","Alpha","Alphaerr","Chi1","Chi2"

;# begin loop through each cluster
zf1 = 0
zf2 = 0
numplaw = 0
FOR i = 0,n_elements(obsids)-1 DO BEGIN
   obsid = strcompress(obsids[i],/remove_all)
   name = strcompress(cluster[i],/remove_all)
   ord = where(cluster EQ name)
   multi  = 'no'
   IF n_elements(ord) EQ 1 THEN obsid = obsid ELSE BEGIN
      multi = 'yes'
      temp = obsids[ord]
      FOR j=1,n_elements(temp)-1 DO BEGIN
         obsid = obsid+'_'+strcompress(temp[j],/remove_all)
      ENDFOR
   ENDELSE

   ;# check for file existance
   file1 = myhome+'/research/pf_clusters/pf_fits/tables/'+obsid+'_table.dat'
   check = findfile(file1,count=count1)
   IF (count1 NE 1) THEN GOTO,ERROR
   file2 = myhome+'/research/pf_clusters/pf_fits/s_results/'+obsid+'_results.log'
   check = findfile(file2,count=count2)
   IF (count2 NE 1) THEN GOTO,ERROR
   file3 = myhome+'/research/pf_clusters/pf_fits/tables/'+obsid+'_tcool.dat'
   check = findfile(file3,count=count3)
   IF (count3 NE 1) THEN GOTO,ERROR
   IF multi EQ 'yes' THEN $
      file4 = loc[i]+'/merged/'+name+'/'+obsid+'_exclude.fits' $
   ELSE $
      file4 = loc[i]+'/'+obsid+'/reprocessed/'+obsid+'_exclude.fits'
   head = headfits(file4,ext=1)
   texp = sxpar(head,'LIVETIME')

   ;# redshift filter
   zf1++
   IF (zfilter EQ 'yes') THEN BEGIN
      IF ((zs[i] LT zmin) OR (zs[i] GT zmax)) THEN GOTO,ERROR
   ENDIF
   IF (txfilter EQ 'yes') THEN BEGIN
      IF ((txs[i] LT txmin) OR (txs[i] GT txmax)) THEN GOTO,ERROR
   ENDIF
   zf2++

   ;# read in data
   dataobs  = read_ascii(file1, template = s_tabletemplate)
   datafit  = read_ascii(file2, template = s_resultstemplate)
   datacool = read_ascii(file3, template = tcooltemplate)

   ;# get central entropy value, either obs or fit
   IF space EQ 'obs' THEN BEGIN
      IF (ktype EQ 'flat') THEN k = dataobs.k_flat ELSE k = dataobs.k
      num = n_elements(k)       ;# remember, they're reversed
      k0 = k[num-1]
      k0err = dataobs.k_err[num-1]
      chi1 = 0.0
      chi2 = 0.0
   ENDIF ELSE IF space EQ 'fit' THEN BEGIN
      IF (ktype EQ 'itpl') THEN BEGIN
         chi1 = datafit.chisq[0]
         chi2 = datafit.chisq[1]
      ENDIF ELSE BEGIN
         chi1 = datafit.chisq[2]
         chi2 = datafit.chisq[3]
      ENDELSE
      k0 = datafit.k0[ind]
      k0err = datafit.k0err[ind]
   ENDIF ELSE BEGIN
      print, '## ERROR: something is wrong, I do not understand ',space,' as type of data to acquire.'
      exit
   ENDELSE

   ;# define value arrays
   tx    = txs[i]
   z     = zs[i]
   rv    = rdelta(180, z, tx, /silent)*1000.
   k100  = datafit.k100[ind]
   k100err  = datafit.k100err[ind]
   alpha    = datafit.plaw[ind]
   alphaerr = datafit.plawerr[ind]

   ;# throw warnings
   IF (k0err LE 0.0) THEN BEGIN
      print,''
      print, '## WARNING: negative K0 error for:'
      print, format='(A-20,A20,4F10.3,4F10.4)',name, obsid, k0, k0err, k100, k100err, alpha, alphaerr, chi1, chi2
      printf, ODD, format='(A-20,A20,4F10.3,4F10.4)',name, obsid, k0, k0err, k100, k100err, alpha, alphaerr, chi1, chi2
      print, '## Skipping ',name
      GOTO, ERROR
   ENDIF
   IF (chi2 LE chi1 AND space EQ 'fit') THEN BEGIN
      print,''
      print, '## WARNING: power-law better fit for:'
      print, format='(A-20,A20,4F10.3,4F10.4)',name, obsid, k0, k0err, k100, k100err, alpha, alphaerr, chi1, chi2
      printf, ODD, format='(A-20,A20,4F10.3,4F10.4)',name, obsid, k0, k0err, k100, k100err, alpha, alphaerr, chi1, chi2
      print, '## Setting ',name,' K0 to 2-sigma upper-limit'
      k0 = k0+2*k0err
      k0err = -1d-6
      push, az, z
      push, ak, k0
      push, at, tx
   ENDIF
   IF ((k0 LE 0) OR (k0-ksigma*k0err LE 0.)) THEN BEGIN
      print,''
      print, '## WARNING: K0 <= 0 for:'
      print, format='(A-20,A20,4F10.3,4F10.4)',name, obsid, k0, k0err, k100, k100err, alpha, alphaerr, chi1, chi2
      printf, ODD, format='(A-20,A20,4F10.3,4F10.4)',name, obsid, k0, k0err, k100, k100err, alpha, alphaerr, chi1, chi2
      print, '## Setting ',name,' K0 to 2-sigma upper-limit'
      numplaw++
      k0 = k0+2*k0err
      k0err = -1d-6
      push, az, z
      push, ak, k0
      push, at, tx
   ENDIF
   IF ((k100 LE 0) OR (k100 GT 1000)) THEN BEGIN
      print,''
      print, '## WARNING: K100 > 1000 or <= 0 for:'
      print, format='(A-20,A20,4F10.3,4F10.4)',name, obsid, k0, k0err, k100, k100err, alpha, alphaerr, chi1, chi2
      printf, ODD, format='(A-20,A20,4F10.3,4F10.4)',name, obsid, k0, k0err, k100, k100err, alpha, alphaerr, chi1, chi2
   ENDIF
   IF (alpha GT 2) THEN BEGIN
      print,''
      print, '## WARNING: alpha greater than 2 for:'
      print, format='(A-20,A20,4F10.3,4F10.4)',name, obsid, k0, k0err, k100, k100err, alpha, alphaerr, chi1, chi2
      printf, ODD, format='(A-20,A20,4F10.3,4F10.4)',name, obsid, k0, k0err, k100, k100err, alpha, alphaerr, chi1, chi2
   ENDIF

   ;# build an array of numbers to label clusters
;   IF k0 LE 4. THEN GOTO,ERROR
   IF (k0 LE 5. OR z GE 0.6) THEN BEGIN
      push, xn, z
      push, yn, k0
   ENDIF

   ;# deal with the temperature profiles
   cosmology, z, result, /silent
   rinmpc  = reverse(dataobs.rin_mpc)
   routmpc = reverse(dataobs.rout_mpc)
   rmean   = ((rinmpc+routmpc)/2.)*1000.
   tc0  = tc0s[where(tc0clusters EQ name)]
   rin  = fitfile.rin[where(fitfile.obsid EQ obsid)]
   rout = fitfile.rout[where(fitfile.obsid EQ obsid)]
   rtx  = (((rin+rout)/2.)*60.)*result[4]
   t0   = fitfile.tx[where(fitfile.obsid EQ obsid)]
   t0hi = fitfile.thi[where(fitfile.obsid EQ obsid)]
   t0lo = fitfile.tlo[where(fitfile.obsid EQ obsid)]
   txbins = n_elements(where(fitfile.obsid EQ obsid))
   void, t0_err
   FOR ss=0,n_elements(t0)-1 DO BEGIN
      IF (t0[ss]-t0lo[ss] GT t0hi[ss]-t0[ss]) THEN $
         push, t0_err, t0[ss]-t0lo[ss] ELSE $
            push, t0_err, t0hi[ss]-t0[ss]
   ENDFOR

   ;# interpolate tx to dens grid
   temp1 = interpol(t0, rtx, rmean)
   temp2 = interpol(t0_err, rtx, rmean)
   wcentral = where((rmean GE 0.0) AND (rmean LE min(rtx)),nwcentral)
   IF (t0[0] LT t0[1] AND nwcentral GT 0) THEN BEGIN
      wouter = where(rmean GT min(rtx))
      wmintx = where(rtx EQ min(rtx))
      xc     = findgen(nwcentral)+1.0
      xc     = xc/xc
      t_central    = xc * t0(wmintx[0])
      terr_central = xc * t0_err(wmintx[0])
      txitpl = [t_central, temp1(wouter)]
      txerr  = [terr_central, temp2(wouter)]
   ENDIF ELSE BEGIN
      txitpl = temp1
      txerr  = temp2
   ENDELSE
   t0   = txitpl[0]
   terr = txerr[0]

   ;# to log
   IF (k0 GE gapmin AND k0 LE gapmax) THEN $
      printf, GAP, FORMAT='(A-20,A20,2F10.3)', name, obsid, k0, k0err

   ;# build final arrays
   push, outnames, name
   push, kr, k0+k100
   push, krc, k0+k100*(rc/100.)^(alpha)
   push, tcool, 100.*(k0/10.)^(3./2.)*(t0/5.)^(-1.)
   push, tcoolerr, (10.^2.*(k0/10.)^(3./2.)*(t0/5.)^(-1.))*sqrt((9./4.)*((k0err/10.)/(k0/10.))^2.+((terr/5.)/(t0/5.))^2.)
   push, obscool, datacool.tc32[0]*1000.
   push, obscoolerr, datacool.tc32err[0]*1000.
   push, zall, z
   push, tall, tx
   push, tc0all, tc0*1000.
   push, k0all, k0
   push, k0errall, k0err
   push, k100all, k100
   push, k100errall, k100err
   push, aall, alpha
   push, aerrall, alphaerr
   push, atexp, texp
   push, atxbins, txbins

ERROR:
ENDFOR

;# print out some information
print, ''
print, format='(A-35,I10)','Number clusters before z-filter:',zf1
print, format='(A-35,I10)','Number clusters after z-filter:',zf2
print, ''
print, format='(A-50,I10)','Number clusters with plaw @ '+num2str(ksigma)+'-sigma:',numplaw
print, ''
FREE_LUN, GAP
FREE_LUN, ODD

;# adhoc filter
IF dofilter EQ 'yes' THEN BEGIN
   print, format='(A-30,I10)','Number clusters pre err-filter:',n_elements(k0all)
   FOR i=0,n_elements(k0errall)-1 DO BEGIN
      IF k0all[i] LT filter0 THEN BEGIN
         IF k0errall[i] LT filter1 THEN BEGIN
            push, goodkr, kr[i]
            push, goodkrc, krc[i]
            push, goodtcool, tcool[i]
            push, goodtcoolerr, tcoolerr[i]
            push, goodobscool, obscool[i]
            push, goodobscoolerr, obscoolerr[i]
            push, goodtall, tall[i]
            push, goodtc0all, tc0all[i]
            push, goodk0all, k0all[i]
            push, goodk0errall, k0errall[i]
            push, goodk100all, k100all[i]
            push, goodk100errall, k100errall[i]
            push, goodaall, aall[i]
            push, goodaerrall, aerrall[i]
            push, goodatexp, atexp[i]
            push, goodatxbins, atxbins[i]
         ENDIF
      ENDIF ELSE BEGIN
         IF k0errall[i]/k0all[i] LE filter2 THEN BEGIN
            push, goodkr, kr[i]
            push, goodkrc, krc[i]
            push, goodtcool, tcool[i]
            push, goodtcoolerr, tcoolerr[i]
            push, goodobscool, obscool[i]
            push, goodobscoolerr, obscoolerr[i]
            push, goodtall, tall[i]
            push, goodtc0all, tc0all[i]
            push, goodk0all, k0all[i]
            push, goodk0errall, k0errall[i]
            push, goodk100all, k100all[i]
            push, goodk100errall, k100errall[i]
            push, goodaall, aall[i]
            push, goodaerrall, aerrall[i]
            push, goodatexp, atexp[i]
            push, goodatxbins, atxbins[i]
         ENDIF
      ENDELSE
   ENDFOR
   kr = goodkr
   krc = goodkrc
   tcool = goodtcool
   tcoolerr = goodtcoolerr
   obscool = goodobscool
   obscoolerr = goodobscoolerr
   tall = goodtall
   tc0all = goodtc0all
   k0all = goodk0all
   k0errall = goodk0errall
   k100all = goodk100all
   k100errall = goodk100all
   aall = goodaall
   aerrall = goodaall
   atexp = goodatexp
   atxbins = goodtxbins
   print, format='(A-30,I10)','Number clusters post err-filter:',n_elements(k0all)
   print, ''
ENDIF

;# stuff for KMM
FOR j=0,n_elements(k0all)-1 DO printf, KMM, num2str(alog10(k0all[j]))
FOR j=0,n_elements(obscool)-1 DO printf, KMM2, num2str(alog10(obscool[j]))
FREE_LUN, KMM
FREE_LUN, KMM2

;# quick stats for all
print, '## All K0'
a = alog10(aall)
print, 'Total clusters: ',n_elements(aall)
amean = 10.^(mean(a)+0.5*(stddev(a)^2.))
dev = sqrt(amean^2.*10.^(stddev(a)^2.-1))
print, format='(A-30,F10.3,A4,F8.3)','Mean alpha:',amean,'+/-',dev
a = alog10(k0all)
amean = 10.^(mean(a)+0.5*(stddev(a)^2.))
dev = sqrt(amean^2.*10.^(stddev(a)^2.-1))
print, format='(A-30,F10.2,A4,F8.2,A10)','Mean K0:',amean,'+/-',dev,'keV cm**2'
a = alog10(krc)
amean = 10.^(mean(a)+0.5*(stddev(a)^2.))
dev = sqrt(amean^2.*10.^(stddev(a)^2.-1))
print, format='(A-30,F10.2,A4,F8.2,A10)','Mean KRC:',amean,'+/-',dev,'keV cm**2'
a = alog10(k100all)
amean = 10.^(mean(a)+0.5*(stddev(a)^2.))
dev = sqrt(amean^2.*10.^(stddev(a)^2.-1))
print, format='(A-30,F10.2,A4,F8.2,A10)','Mean K100:',amean,'+/-',dev,'keV cm**2'
a = zall
amean = mean(a)
dev = stddev(a)
print, format='(A-30,F10.4,A4,F8.4)','Mean z:',amean,'+/-',dev
print, format='(A-30,F10.4)','Min z:',min(a)
print, format='(A-30,F10.4)','Max z:',max(a)
a = tall
amean = mean(a)
dev = stddev(a)
print, format='(A-30,F10.4,A4,F8.4)','Mean Tx:',amean,'+/-',dev
print, format='(A-30,F10.4)','Min Tx:',min(a)
print, format='(A-30,F10.4)','Max Tx:',max(a)
print, ''

;# quick stats for k0 < gap
;# remember: distros are log-normal, so do stats in log space
ord = where(k0all LE gaplim,num)
IF num LE 1 THEN BEGIN
   print, '## WARNING: No clusters with K0 <', gaplim
   GOTO,SKIP1
ENDIF
print, '## K0 < ',num2str(gaplim)

a = alog10(aall[ord])
print, 'Total clusters: ',n_elements(a)
amean = 10.^(mean(a)+0.5*(stddev(a)^2.))
dev = sqrt(amean^2.*10.^(stddev(a)^2.-1))
print, format='(A-30,F10.2,A4,F8.2)','Mean alpha:',amean,'+/-',dev

a = alog10(k0all[ord])
amean = 10.^(mean(a)+0.5*(stddev(a)^2.))
dev = sqrt(amean^2.*10.^(stddev(a)^2.-1))
print, format='(A-30,F10.2,A4,F8.2,A10)','Mean K0:',amean,'+/-',dev,'keV cm**2'

a = alog10(krc[ord])
amean = 10.^(mean(a)+0.5*(stddev(a)^2.))
dev = sqrt(amean^2.*10.^(stddev(a)^2.-1))
print, format='(A-30,F10.2,A4,F8.2,A10)','Mean KRC:',amean,'+/-',dev,'keV cm**2'

a = alog10(k100all[ord])
amean = 10.^(mean(a)+0.5*(stddev(a)^2.))
dev = sqrt(amean^2.*10.^(stddev(a)^2.-1))
print, format='(A-30,F10.2,A4,F8.2,A10)','Mean K100:',amean,'+/-',dev,'keV cm**2'

a = zall[ord]
amean = mean(a)
dev = stddev(a)
print, format='(A-30,F10.4,A4,F8.4)','Mean z:',amean,'+/-',dev
print, format='(A-30,F10.4)','Min z:',min(a)
print, format='(A-30,F10.4)','Max z:',max(a)

a = tall[ord]
amean = mean(a)
dev = stddev(a)
print, format='(A-30,F10.4,A4,F8.4)','Mean Tx:',amean,'+/-',dev
print, format='(A-30,F10.4)','Min Tx:',min(a)
print, format='(A-30,F10.4)','Max Tx:',max(a)
print, ''

;# first skip point
SKIP1:

;# quick stats for k0 > gap
ord = where(k0all GT gaplim,num)
IF num LE 1 THEN BEGIN
   print, '## WARNING: No clusters with K0 >', gaplim
   GOTO,SKIP2
ENDIF
print, '## K0 > ',num2str(gaplim)

a = alog10(aall[ord])
print, 'Total clusters: ',n_elements(a)
amean = 10.^(mean(a)+0.5*(stddev(a)^2.))
dev = sqrt(amean^2.*10.^(stddev(a)^2.-1))
print, format='(A-30,F10.2,A4,F8.2)','Mean alpha:',amean,'+/-',dev

a = alog10(k0all[ord])
amean = 10.^(mean(a)+0.5*(stddev(a)^2.))
dev = sqrt(amean^2.*10.^(stddev(a)^2.-1))
print, format='(A-30,F10.2,A4,F8.2,A10)','Mean K0:',amean,'+/-',dev,'keV cm**2'

a = alog10(krc[ord])
amean = 10.^(mean(a)+0.5*(stddev(a)^2.))
dev = sqrt(amean^2.*10.^(stddev(a)^2.-1))
print, format='(A-30,F10.2,A4,F8.2,A10)','Mean KRC:',amean,'+/-',dev,'keV cm**2'

a = alog10(k100all[ord])
amean = 10.^(mean(a)+0.5*(stddev(a)^2.))
dev = sqrt(amean^2.*10.^(stddev(a)^2.-1))
print, format='(A-30,F10.2,A4,F8.2,A10)','Mean K100:',amean,'+/-',dev,'keV cm**2'

a = zall[ord]
amean = mean(a)
dev = stddev(a)
print, format='(A-30,F10.4,A4,F8.4)','Mean z:',amean,'+/-',dev
print, format='(A-30,F10.4)','Min z:',min(a)
print, format='(A-30,F10.4)','Max z:',max(a)

a = tall[ord]
amean = mean(a)
dev = stddev(a)
print, format='(A-30,F10.4,A4,F8.4)','Mean Tx:',amean,'+/-',dev
print, format='(A-30,F10.4)','Min Tx:',min(a)
print, format='(A-30,F10.4)','Max Tx:',max(a)
print, ''

;# first skip point
SKIP2:

;# stats on tcool
print, '## Observed cooling times'
ord = where(obscool LT 700., num)
IF num LE 1 THEN BEGIN
   print, '## WARNING: No clusters with obscool < 700 Myr'
   GOTO,SKIP4
ENDIF
a = alog10(obscool[ord])
amean = 10.^(mean(a)+0.5*(stddev(a)^2.))
dev = sqrt(amean^2.*10.^(stddev(a)^2.-1))
print, format='(A-30,I10,A5,I8,A4)','Mean obscool < 700:',amean,'+/-',dev,'Myr'
SKIP4:

ord = where(obscool GE 700. AND obscool LT 6000., num)
IF num LE 1 THEN BEGIN
   print, '## WARNING: No clusters with 700 <= obscool < 6000 Myr'
   GOTO,SKIP5
ENDIF
a = alog10(obscool[ord])
amean = 10.^(mean(a)+0.5*(stddev(a)^2.))
dev = sqrt(amean^2.*10.^(stddev(a)^2.-1))
print, format='(A-30,I10,A5,I8,A4)','Mean obscool >=700 & < 6000:',amean,'+/-',dev,'Myr'
SKIP5:

ord = where(obscool GE 6000., num)
IF num LE 1 THEN BEGIN
   print, '## WARNING: No clusters with obscool > 6000 Myr'
   GOTO,SKIP6
ENDIF
a = alog10(obscool[ord])
amean = 10.^(mean(a)+0.5*(stddev(a)^2.))
dev = sqrt(amean^2.*10.^(stddev(a)^2.-1))
print, format='(A-30,I10,A5,I8,A4)','Mean obscool > 6000:',amean,'+/-',dev,'Myr'
SKIP6:

;# run KMM
IF runkmm EQ 'yes' THEN BEGIN
   SPAWN, 'bimodality -i 1000 -t kmm_k0.in'
   SPAWN, 'bimodality -i 1000 -m kmm_k0.in'
ENDIF

;# perform the scaling calculation
gamma = -1.0
gammax = 1.0
gamstep = 0.001
WHILE gamma LE gammax DO BEGIN
    knew  = alog10(kr*(tall^gamma))
    kmean = 10.^(mean(knew)+0.5*(stddev(knew)^2.))
    kdev = sqrt(kmean^2.*10.^(stddev(knew)^2.-1))
    push, gamall, gamma
    push, kdevall, kdev/kmean
    gamma = gamma+gamstep
ENDWHILE

;# make bins for K(r) cum profs
kmin = min(k0all)
kmax = max(k0all)
kiter = kmin/2.
total = float(n_elements(k0all))
print, '## STATUS: Working on K(r) cumulative profile...'
WHILE kiter LE kmax DO BEGIN
   num = float(n_elements(where(k0all LE kiter)))
   push, k0cumpr, num/total
   num = float(n_elements(where(krc LE kiter)))
   push, krccumpr, num/total
   push, kx, kiter
   kiter = kiter + kstep
ENDWHILE
print, 'Done.'
push, kx, kmax
push, k0cumpr, 1.0
push, krccumpr, 1.0
cxmin = 0.8*min(kx)
cxmax = 1.2*max(kx)
cymin = 0.8*min(k0cumpr)
cymax = 1.1
push, kx, cxmax
push, k0cumpr, 1.0
push, krccumpr, 1.0

;# make bins for obscool cum prof
obsmin = min(obscool)
obsmax = max(obscool)
obsiter = obsmin/2.
total = float(n_elements(obscool))
print, '## STATUS: Working on ObsCool cumulative profile...'
WHILE obsiter LE obsmax DO BEGIN
   num = float(n_elements(where(obscool LE obsiter)))
   push, obscumpr, num/total
   push, obsx, obsiter
   obsiter = obsiter + obsstep
ENDWHILE
print, 'Done.'
push, obsx, obsmax
push, obscumpr, 1.0
obsxmin = 0.8*min(obsx)
obsxmax = 1.2*max(obsx)
obsymin = 0.8*min(obscumpr)
obsymax = 1.1
push, obsx, obsxmax
push, obscumpr, 1.0

;# make bins for tcool cum prof
tcmin = min(tcool)
tcmax = max(tcool)
tciter = tcmin/2.
total = float(n_elements(tcool))
print, '## STATUS: Working on K0 Cool cumulative profile...'
WHILE tciter LE tcmax DO BEGIN
   num = float(n_elements(where(tcool LE tciter)))
   push, tccumpr, num/total
   push, tcx, tciter
   tciter = tciter + tcstep
ENDWHILE
print, 'Done.'
push, tcx, tcmax
push, tccumpr, 1.0
tcxmin = 0.8*min(tcx)
tcxmax = 1.2*max(tcx)
tcymin = 0.8*min(tccumpr)
tcymax = 1.1
push, tcx, tcxmax
push, tccumpr, 1.0

;# make plots of data
set_plot, 'PS'
device, filename='mintscal.eps', $
        /color, $
        /encapsulated, $
        /portrait, $
        set_font='Times-Roman'
xmin = min(gamall)
xmax = max(gamall)
ymin = 0.98*min(kdevall)
ymax = 1.02*max(kdevall)
plot,gamall,kdevall, $
     /xsty, /ysty, $
     xtitle = textoidl('\gamma'), $
     ytitle = textoidl('\sigma_{K^*_{\gamma}}/K^*_{\gamma}'), $
     xrange = [xmin,xmax], $
     yrange = [ymin,ymax], $
     charsize= csize
get = where(kdevall EQ min(kdevall))
n = n_elements(get)
n = indgen(n)+1
n = median(n)
IF (n EQ 2 OR n EQ 1) THEN n = 0
get = get[n]
print, format='(A-30,F10.3)','Minimum gamma:',gamall[get]
print, format='(A-30,F10.3)','Minimum K100 dev:',kdevall[get]
items = [textoidl('Min \sigma_{K^*_{\gamma}}/K^*_{\gamma}: '+num2str(kdevall[get],3)), $
         textoidl('Min \gamma: '+num2str(gamall[get],3))]
linearr = replicate(-99,n_elements(items))
psyarr = replicate(-99,n_elements(items))
legend, items, linestyle=linearr,psym=psyarr,/top,box=0,/right_legend,charsize=0.8
plotsym, 3, /fill
oplot, [gamall[get],-100], [kdevall[get],-100], psym=8
oplot, [gamall[get],gamall[get]], [-100,kdevall[get]], linestyle=2
oplot, [-100,gamall[get]], [kdevall[get],kdevall[get]], linestyle=2
device,/close

IF logcolor EQ 'yes' THEN BEGIN

   ;# histogram of K0 dist.
   set_plot, 'PS'
   device, filename='k0hist.eps', $
           /color, $
           /encapsulated, $
           /portrait, $
           set_font='Times-Roman'
   histoplot, alog10(k0all), k0bin, $
              /drawlines, /colorfill, fillcolor = hcolor, $
              ;/fraction, $
              xtitle = textoidl('log K_0 [keV cm^2]'), $
              ytitle = textoidl('Number of clusters'), $
              position = ASPECT(1.0), $
              charsize = csize, $
              bins = bins, $
              omax = xu, $
              omin = xl
   xv = alog10(30.0)
   oplot, [xv,xv],[0,1d6], linestyle=2, thick=3
   xv = alog10(60.0)
   oplot, [xv,xv],[0,1d6], linestyle=2, thick=3

   ;# Hetero
   ;# full sample sans NGC and K0 < 5
   p1 = [0.402973, 1.17808, 0.189338]
   p2 = [0.597027, 2.10729, 0.29789]
   ;# full sample sans NGC
   p3 = [0.534882, 1.24805, 0.376761]
   p4 = [0.465118, 2.18718, 0.254217]
  ;# primary HIFL sans K0 < 5
;      p1 = [0.370723, 1.03398, 0.121266]
;      p2 = [0.629277, 2.04624, 0.333523]
   ;# primary HIFL
;      p3 = [0.495953, 1.01232, 0.357314]
;      p4 = [0.504047, 2.12227, 0.282399]

   ;# Homo
   ;# full sample sans NGC and K0 < 5
;      p1 = [0.454477, 1.22937, 0.253993]
;      p2 = [0.545523, 2.15205, 0.253993]
   ;# full sample sans NGC
;      p3 = [0.454407, 1.15696, 0.307994]
;      p4 = [0.545593, 2.12705, 0.307994]
   ;# primary HIFL sans K0 < 5
;      p1 = [0.43743, 1.10126, 0.255622]
;      p2 = [0.56257, 2.11321, 0.255622]
   ;# primary HIFL
;      p3 = [0.458627, 0.962724, 0.318419]
;      p4 = [0.541373, 2.08916, 0.318419]

   x  = maken(-10,10,10000)
   N  = n_elements(k0all)
   A  = N/(bins/(xu-xl))
   y1 = p1[0]*exp(-(x-p1[1])^2./(2.*p1[2]^2.))
   y2 = p2[0]*exp(-(x-p2[1])^2./(2.*p2[2]^2.))
   y3 = A*(y1+y2)
   y4 = p3[0]*exp(-(x-p3[1])^2./(2.*p3[2]^2.))
   y5 = p4[0]*exp(-(x-p4[1])^2./(2.*p4[2]^2.))
   y6 = A*(y4+y5)

   IF plkmm EQ 'yes' THEN BEGIN
;      oplot, x, y1, linestyle=0, thick=3, color=100
;      oplot, x, y2, linestyle=0, thick=3, color=100
      oplot, x, y3, linestyle=0, thick=3, color=50
;      oplot, x, y4, linestyle=2, thick=3, color=150
;      oplot, x, y5, linestyle=2, thick=3, color=150
      oplot, x, y6, linestyle=2, thick=3, color=250
   ENDIF
   device, /close

   ;# histogram of KRC dist.
   set_plot, 'PS'
   device, filename='krchist.eps', $
           /color, $
           /encapsulated, $
           /portrait, $
           set_font='Times-Roman'
   histoplot, alog10(krc), krcbin, $
              /drawlines, /colorfill, fillcolor = hcolor, $
              xtitle = textoidl('log K_{'+num2str(rc)+'} [keV cm^2]'), $
              ytitle = textoidl('Number of clusters'), $
              position= ASPECT(1.0), $
              charsize= csize
   xv = alog10(30.0)
   oplot, [xv,xv],[0,1d6], linestyle=2, thick=2
   device, /close
   
   ;# histogram of obscool dist.
   set_plot, 'PS'
   device, filename='obscool.eps', $
           /color, $
           /encapsulated, $
           /portrait, $
           set_font='Times-Roman'
   histoplot, alog10(obscool), obsbin, $
              /drawlines, /colorfill, fillcolor = hcolor, $
              xtitle = textoidl('log Core cooling time [Myr]'), $
              ytitle = textoidl('Number of clusters'), $
              position= ASPECT(1.0), $
              charsize= csize
   device, /close

    ;# histogram of obscool dist.
    set_plot, 'PS'
    device, filename='k0cool.eps', $
            /color, $
            /encapsulated, $
            /portrait, $
            set_font='Times-Roman'
;    histoplot, alog10(tcool), tcbin, $
;               /drawlines, /colorfill, fillcolor = hcolor, $
;               xtitle = textoidl('log t_{c0}(K_0) [Myr]'), $
;               ytitle = textoidl('Number of clusters'), $
;               position= ASPECT(1.0), $
;               charsize= csize
    histoplot, alog10(tc0all), tcbin, $
               /drawlines, /colorfill, fillcolor = hcolor, $
               ;/fraction, $
               xtitle = textoidl('log t_{c0} [Myr]'), $
               ytitle = textoidl('Number of clusters'), $
               position= ASPECT(1.0), $
               charsize= csize
    device, /close

    ;# histogram of power law dist.
    set_plot, 'PS'
    device, filename='plawhist.eps', $
            /color, $
            /encapsulated, $
            /portrait, $
            set_font='Times-Roman'
    histoplot, aall, abinning, $
               /drawlines, /colorfill, fillcolor = hcolor, $
               xtitle = textoidl('\alpha'), $
               ytitle = textoidl('Number of clusters'), $
               position= ASPECT(1.0), $
               charsize= csize
    oplot, [1.1,1.1],[0,1d6], linestyle=2, thick=2
    device, /close

ENDIF ELSE BEGIN

   ;# histogram of K0 dist.
   ;# and cumulative plot
   set_plot, 'PS'
   device, filename='k0hist.eps', $
           /color, $
           /encapsulated, $
           /portrait, $
           set_font='Times-Roman'
   multiplot,[1,2], $
             mxtitle=textoidl('K_0 [keV cm^2]')
   histoplot, k0all, k0bin, $
              /log, $
              /xsty, $
              ;/fraction, $
              xrange = [cxmin,cxmax], $
              ytitle = textoidl('Number of clusters'), $
              charsize= 0.8*csize, $
              histdata=hdata, $
              locations=ldata
   multiplot
   ytex = textoidl('Fractional number of clusters')
   plot, kx, k0cumpr, $
         linestyle = 0, $
         xrange = [cxmin,cxmax], $
         yrange = [cymin,cymax],$
         ytitle = ytex, $
         /xsty, /ysty, $
         /xlog, $
         charsize = 0.8*csize, $
         psym = 10
   device, /close
   multiplot,/reset

   ;# histogram of KRC dist.
   ;# and cumulative plot 
   set_plot, 'PS'
   device, filename='krchist.eps', $
           /color, $
           /encapsulated, $
           /portrait, $
           set_font='Times-Roman'
   multiplot,[1,2], $
             mxtitle=textoidl('K_{'+num2str(rc)+'} [keV cm^2]')
   histoplot, krc, krcbin, $
              /log, $
              /xsty, $
              ;/fraction, $
              xrange = [cxmin,cxmax], $
              ytitle = textoidl('Number of clusters'), $
              charsize= csize
   ytex = textoidl('Fractional number of clusters')
   multiplot
   plot, kx, krccumpr, $
         linestyle = 0, $
         xrange = [cxmin,cxmax], $
         yrange = [cymin,cymax],$
         ytitle = ytex, $
         /xsty, /ysty, $
         /xlog, $
         charsize = csize, $
         psym = 10
   device, /close
   multiplot,/reset

   ;# histogram of obscool dist.
   set_plot, 'PS'
   device, filename='obscool.eps', $
           /color, $
           /encapsulated, $
           /portrait, $
           set_font='Times-Roman'
   multiplot,[1,2], $
             mxtitle=textoidl('Core Cooling Time [Myr]')
   xmin = 0.8*min(obscool)
   xmax = 1.2*max(obscool)
   histoplot, obscool, obsbin, $
              /log, $
              ;/fraction, $
              /xsty, $
              xrange = [obsxmin,obsxmax], $
              ytitle = textoidl('Number of clusters'), $
              charsize= csize
   ytex = textoidl('Fractional number of clusters')
   multiplot
   plot, obsx, obscumpr, $
         linestyle = 0, $
         xrange = [obsxmin,obsxmax], $
         yrange = [obsymin,obsymax],$
         ytitle = ytex, $
         /xsty, /ysty, $
         /xlog, $
         charsize = csize, $
         psym = 10
   device, /close
   multiplot,/reset

   ;# histogram of tcool dist.
   set_plot, 'PS'
   device, filename='k0cool.eps', $
           /color, $
           /encapsulated, $
           /portrait, $
           set_font='Times-Roman'
   multiplot,[1,2], $
             mxtitle=textoidl('t_{c0}(K_0) [Myr]')
   tcxmin = 6.
   tcxmax = 60000.
;   histoplot, tc0all, 0.2, $
;              /log, /fraction, $
;              /xsty, xrange = [tcxmin,tcxmax], $
;              ytitle = textoidl('Fractional Number of Clusters'), $
;              charsize= 0.8
   histoplot, tcool, tcbin, $
              /log, $
              /xsty, $
              ;/fraction, $
              xrange = [tcxmin,tcxmax], $
;              /overplot, linestyle=2, thick=3
              ytitle = textoidl('Number of clusters'), $
              charsize= 0.8
   ytex = textoidl('Fractional number of clusters')
   multiplot
   plot, tcx, tccumpr, $
         linestyle = 0, $
         xrange = [tcxmin,tcxmax], $
         yrange = [tcymin,tcymax],$
         ytitle = ytex, $
         /xsty, /ysty, $
         /xlog, $
         charsize = 0.8, $
         psym = 10
   device, /close
   multiplot,/reset

   ;# histogram of power law dist.
   set_plot, 'PS'
   device, filename='plawhist.eps', $
           /color, $
           /encapsulated, $
           /portrait, $
           set_font='Times-Roman'
   histoplot, aall, abinning, $
              /log, $
              xtitle = textoidl('\alpha'), $
              ytitle = textoidl('Number of clusters'), $
              charsize= csize
   oplot, [1.1,1.1],[0,1d6], linestyle=2, thick=2
   device, /close

   ;# plot of tx vs. K0
   x = tall
   y = k0all
   yerr = k0errall
   xtex = textoidl('T_{cluster} [keV]')
   ytex = textoidl('K_0 [keV cm^2]')
   set_plot, 'PS'
   loadct, 13, /silent
   xmin = 0.8*min(x)
   xmax = 1.2*max(x)
   ymin = 0.8*min(y)
   ymax = 1.2*max(y)
   device, filename='txk0.eps', $
           /color, $
           /encapsulated, $
           /portrait, $
           set_font='Times-Roman'
   plot, x, y, $
         /nodata, $
         linestyle = 0, $
         xrange = [xmin,xmax], $
         yrange = [ymin,ymax],$
         xtitle = xtex, $
         ytitle = ytex, $
         /xsty, /ysty, $
         /xlog, /ylog, $
         charsize = csize
   plotsym, 1, 1.5
   IF n_elements(at) GT 1 THEN $
      oplot, at, ak, psym=8, color=0
   plotsym, 0, 0.8, /fill
   oploterror, x, y, yerr, psym=8, errcolor=0, color=0
   oplot, x, y, color=0, psym=8
   plotsym, 0, 0.8*0.8, /fill
   ord = where(y LE 30.,num)
   IF num GT 0 THEN $
      oplot, x[ord], y[ord], color=50, psym=8
   ord = where(y GT 30. AND y LE 60.,num)
   IF num GT 0 THEN $
      oplot, x[ord], y[ord], color=150, psym=8
   ord = where(y GT 60.,num)
   IF num GT 0 THEN $
      oplot, x[ord], y[ord], color=250, psym=8
   oplot,[1d-4,1d2],[40,40],linestyle=2, psym=0
   device, /close

   ;# plot of K0 vs. z
   x = zall
   y = k0all
   yerr = k0errall
   xtex = textoidl('Cluster Redshift')
   ytex = textoidl('K_0 [keV cm^2]')
   set_plot, 'PS'
   loadct, 13, /silent
   xmin = 0.8*min(x)
   xmax = 1.2*max(x)
   ymin = 0.8*min(y)
   ymax = 1.2*max(y)
   device, filename='k0res.eps', $
           /color, $
           /encapsulated, $
           /portrait, $
           set_font='Times-Roman'
   plot, x, y, $
         /nodata, $
         linestyle = 0, $
         xrange = [xmin,xmax], $
         yrange = [ymin,ymax],$
         xtitle = xtex, $
         ytitle = ytex, $
         /xsty, /ysty, $
         /xlog, /ylog, $
         charsize = csize
   IF n_elements(az) GT 1 THEN BEGIN
      plotsym, 1, 1.5
      oplot, az, ak, psym=8, color=0
   ENDIF
   plotsym, 0, 0.8, /fill
   oploterror, x, y, yerr, psym=8, errcolor=0, color=0
   oplot, x, y, color=0, psym=8
   plotsym, 0, 0.8*0.8, /fill
   oplot, x, y, color=225, psym=8
   IF n_elements(az) GT 1 THEN $
      oplot, az, ak, color=0, psym=8
   IF n_elements(xn) GT 1 THEN BEGIN
      nums = num2str(indgen(n_elements(xn))+1)
      ord = sort(xn)
      xn = xn[ord]
      yn = yn[ord]
      xyouts, xn, yn, nums, charsize=0.8, alignment=1.0
   ENDIF
   device, /close

ENDELSE

set_plot, 'PS'
x = obscool
y = k0all
xerr = obscoolerr
yerr = k0errall
xtex = textoidl('Core Cooling Time [Myr]')
ytex = textoidl('K_0 [keV cm^2]')
xmin = 0.8*min(x)
xmax = 1.2*max(x)
ymin = 0.8*min(y)
ymax = 1.2*max(y)
device, filename='k0obscool.eps', $
        /color, $
        /encapsulated, $
        /portrait, $
        set_font='Times-Roman'
plot, x, y, $
      /nodata, $
      linestyle = 0, $
      xrange = [xmin,xmax], $
      yrange = [ymin,ymax],$
      xtitle = xtex, $
      ytitle = ytex, $
      /xsty, /ysty, $
      /xlog, /ylog, $
      charsize = csize
plotsym, 0, 0.8, /fill
oploterror, x, y, xerr, yerr, psym=8, errcolor=0, color=0
oplot, x, y, color=0, psym=8
plotsym, 0, 0.8*0.8, /fill
oplot, x, y, color=225, psym=8
device, /close

set_plot, 'PS'
x = tall
y = aall
xtex = textoidl('T_{cluster} [keV]')
ytex = textoidl('Power law index, \alpha')
xmin = 0.8*min(x)
xmax = 1.2*max(x)
ymin = 0.8*min(y)
ymax = 1.2*max(y)
device, filename='alphatx.eps', $
        /color, $
        /encapsulated, $
        /portrait, $
        set_font='Times-Roman'
plot, x, y, $
      /nodata, $
      linestyle = 0, $
      xrange = [xmin,xmax], $
      yrange = [ymin,ymax],$
      xtitle = xtex, $
      ytitle = ytex, $
      /xsty, /ysty, $
      /xlog, /ylog, $
      charsize = csize
plotsym, 0, 0.8, /fill
oplot, x, y, color=0, psym=8
plotsym, 0, 0.8*0.8, /fill
oplot, x, y, color=225, psym=8
device, /close

set_plot, 'PS'
x = k0all
y = atexp/1000.
xerr = k0errall
yerr = replicate(0,n_elements(y))
xtex = textoidl('K_0 [keV cm^2]')
ytex = textoidl('Exposure Time [ksec]')
xmin = 0.7*min(x)
xmax = 1.25*max(x)
ymin = 0.8*min(y)
ymax = 1.2*max(y)
device, filename='texpk0.eps', $
        /color, $
        /encapsulated, $
        /portrait, $
        set_font='Times-Roman'
plot, x, y, $
      /nodata, $
      linestyle = 0, $
      xrange = [xmin,xmax], $
      yrange = [ymin,ymax],$
      xtitle = xtex, $
      ytitle = ytex, $
      /xsty, /ysty, $
      /xlog, /ylog, $
      position = ASPECT(1.0), $
      charsize = csize
plotsym, 0, 0.5, /fill
oploterror, x, y, xerr, yerr, psym=8, /nohat
oplot, x, y, psym=8
device, /close

set_plot, 'PS'
x = k0all
y = atxbins
xerr = k0errall
yerr = replicate(0,n_elements(y))
xtex = textoidl('K_0 [keV cm^2]')
ytex = textoidl('Number of bins in T_X(r)')
xmin = 0.7*min(x)
xmax = 1.25*max(x)
ymin = 0.8*min(y)
ymax = 1.2*max(y)
device, filename='ntxbins_k0.eps', $
        /encapsulated, $
        /portrait, $
        set_font='Times-Roman'
plot, x, y, $
      /nodata, $
      linestyle = 0, $
      xrange = [xmin,xmax], $
      yrange = [ymin,ymax],$
      xtitle = xtex, $
      ytitle = ytex, $
      /xsty, /ysty, $
      /xlog, /ylog, $
      position = ASPECT(1.0), $
      charsize = csize
plotsym, 0, 0.5, /fill
oploterror, x, y, xerr, yerr, psym=8, /nohat
oplot, x, y, psym=8
device, /close

set_plot,'X'
!QUIET = 1

END

