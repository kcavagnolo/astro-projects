;#######################
;#######################
;#
;# NAME:
;#     kfitter.pro
;#
;# PURPOSE:
;#     This program deprojects surface brightness profile, converts it 
;#     to an interpolated electron density using spectral information,
;#     calculates radial entropy, fits two models, and returns a bunch
;#     of plots, tables, and information.
;#     
;# EXPLANATION:
;#     Monte-Carlo the surface brightness profiles in order to obtain
;#     an estimate of the 1-sigma errors on the density profile with
;#     deprojection (no systematics obviously). This program is
;#     re-organized so that the deprojection happens many many times,
;#     but interpolation only happens once.
;#
;# CALLING SEQUENCE:
;#     IDL> kfitter, 'sb.fits', 'tempfits.dat', 'clname', cltemp, redshift, nmc, 'obsid', 'expcorr.fits', fake
;#
;# INPUTS:
;#     sb.fits: the surface brightness profile for the cluster; this
;#              is presumed to be in the format output
;#              from CIAO's dmextract, i.e. there is are
;#              columns for r_mid, sur_bri, sur_bri_err, etc
;#     tempfits.dat: the ASCII file containing the temperture profile
;#                   fits; there is a specific format to this file
;#                   based on the spectral extraction script extract_spectra.pl
;#     clname: the cluster name
;#     cltemp: the cluster temperature
;#     redshift: the cluster redshift
;#     nmc: the number of monte carlo sims to execute
;#     obsid: the obsid(s) for the cluster
;#     expcorr.fits: a radial profile of the exp map ON THE SAME GRID
;#                   AS THE SURFACE BRIGHTNESS PROFILE
;#     fake: is this a fake set of data?
;#
;# OUTPUTS:
;#     Log of the two power law fit results: <obsid>_results.log
;#     Plot of the entropy profile:          <obsid>_splot.ps
;#     ASCII table of radial values:         <obsid>_table.dat
;#
;#######################
;#######################

;# Function to calculate a deprojected volume element using the
;# effective radius of McLaughlin 1999 AJ 117 2398

FUNCTION vv, rii, rjj
IF rii le rjj then RETURN,0.0
vvv = 4.*!PI/3. * (rii^2. - rjj^2.)^(1.5)
RETURN,vvv
END

;#######################
;#######################
;##   Main Program    ##
;#######################
;#######################

PRO kfitter, filename, tempfitfile, clustername, tkeV, z, nmc, obs, exfile, imfake

print, ''
print, '## Cluster Info:'
print, format='(A-25,A-30)','## Cluster: ',clustername
print, format='(A-25,A-30)','## ObsID: ',obs
print, format='(A-25,F-30.2)','## Temperature (keV): ',tkeV
print, format='(A-25,F-30.4)','## Redshift: ',z
print, ''

;# open a log
!QUIET = 1

;# makes all plots look nice
!FANCY    = 4
!LINETYPE = 0
!X.THICK  = 2
!Y.THICK  = 2
!Z.THICK  = 2

;# Set-up cosmogony
omega_m = 0.27
lambda  = 0.73
h0      = 70.

;# read in the temperature fits
restore,'~/research/redux/scripts/xspectemp_rin_normerr_src.sav'        
fitfile = read_ascii(tempfitfile,template=xspectemp_rin_normerr_src)

;# check for file existance
notx = 'no'
check  = findfile(filename,count=count)
IF (count NE 1) THEN GOTO,ERROR
IF (imfake NE 'yes') THEN BEGIN
   ord = where(fitfile.obsid EQ obs)
   IF ord[0] EQ -1 THEN BEGIN
      notx = 'yes'
      GOTO, ERROR
   ENDIF
ENDIF

;# read in sur bri profile assuming units CTS/PIXEL**2
print, '## Opening ',filename

;# read all the columns into a structure named fits
fits       = mrdfits(filename,1,hdr,/silent)
exposure   = fits.exposure
sbrpix     = fits.sur_bri
sbr_errpix = fits.sur_bri_err
rinpix     = fits.rin
routpix    = fits.rout
IF imfake EQ 'yes' THEN BEGIN
   common fakepars, fakesbr
   sn     = fits.net_counts/fits.net_counts
   snerr  = sn
ENDIF ELSE BEGIN
   sn     = fits.net_counts/sqrt(fits.bg_counts)
   snerr  = sn*sqrt((fits.net_err/fits.net_counts)^2.+(1./4.)*(fits.bg_err/fits.bg_counts)^2.)
ENDELSE
maxindex   = n_elements(rinpix)-1
print, '## Read in ',num2str(maxindex+1),' surface brightness bins.'

;# check for zero bins
check = where(sbrpix LE 0., znum)
IF znum GT 0 THEN BEGIN
   print, ''
   print, '## WARNING'
   print, '## WARNING'
   print, '## WARNING'
   print, '## ',num2str(znum),' bins with no counts were removed'
   print, '## WARNING'
   print, '## WARNING'
   print, '## WARNING'
   print, ''
   ord = where(sbrpix GT 0.)
   sbrpix     = sbrpix[ord]
   sbr_errpix = sbr_errpix[ord]
   rinpix     = rinpix[ord]
   routpix    = routpix[ord]
   sn         = sn[ord]
   snerr      = snerr[ord]
   maxindex   = n_elements(rinpix)-1
ENDIF

;# gut check using King model
;# this is for testing purposes ONLY
useking = 'no'
If useking EQ 'yes' THEN BEGIN
   print, ''
   print, '## WARNING'
   print, '## WARNING'
   print, '## WARNING'
   print, '## This is only a gut check with King beta-model'
   print, '## WARNING'
   print, '## WARNING'
   print, '## WARNING'
   print, ''
   print, ''
   rmean = (rinpix+routpix)/2.
   sbrpix = sbrpix[0]*(1+(rmean/70)^2.)^(-3.0*0.6+0.5)
ENDIF

;# format the cluster names for plotting
cname = strcompress(clustername,/remove_all)
name  = cname
title = cname+'   '+strcompress(obs,/remove_all)

;# convert radii units from pixels to arcseconds
pixtoarcsec = 0.492             ;# ACIS scale is 0.492"/pixel
rin   = pixtoarcsec * rinpix
rout  = pixtoarcsec * routpix
rmean = 0.5*(rin+rout)
sbr   = sbrpix/(pixtoarcsec)^2.
sbr_err = sbr_errpix/(pixtoarcsec)^2.

;# calculate the radii in Mpc
cosmology, z, result, hubcon=h0, matdens=omega_m, cosdens=lambda, /silent
da       = result[3]                ;# cosmology returns da in Mpc
dang     = result[4]                ;# cosmology returns dang in kpc/"
rinmpc   = rin * dang/1000.
routmpc  = rout * dang/1000.
rmeanmpc = (rinmpc+routmpc)/2.

;#######################
;#######################
;# Exposure Correction #
;#######################
;#######################
;# Read in exposure correction vectors, normalize to the maximum bin,
;# divide the sbr vector by the normalized exposure correction, then
;# report the average and dispersion of the surface brightness
;# correction.

;# handle the instance where there is an exp map
IF exfile NE 'nihil' THEN BEGIN
   expprof = mrdfits(exfile,1,/silent)
   expcorr = expprof.sur_bri
   expcorrerr = expprof.sur_bri_err
   expcorrerr = expcorrerr/max(expcorr)
   expcorr = expcorr/max(expcorr)
   expcorr_ave = mean(expcorr)
   expcorr_dev = stddev(expcorr)
   print, '## Mean exp. corr.: ',num2str(expcorr_ave,3), ' +/- ',num2str(expcorr_dev,3)
   origsbr = sbr
   sbr = sbr/expcorr
   erinpix = expprof.rin
   eroutpix = expprof.rout

   ;# plot the exposure correction, we expect the deviation from one to be small
   emean = (erinpix+eroutpix)/2
   xmin = 0.9*min(emean)
   xmax = 1.1*max(emean)
   ymin = 0.9*min(expcorr)
   ymax = 1.1*max(expcorr)
   plot, emean, expcorr, $
         title = title, $
         xtitle = textoidl('R_{mid} [Pixels]'), $
         ytitle = textoidl('Norm. Exp. Correction [sbr/exp/max(exp)]'), $
         psym = 10, $
         linestyle = 0, $
         /xlog, /ylog, $
         /xsty, /ysty, $
         xran = [xmin,xmax], $
         yran = [ymin,ymax]
   plotsym, 0, 0.8, /fill
   oplot, emean, expcorr, psym=8
   oploterr, emean, expcorr, expcorrerr
   oplot, emean, replicate(expcorr_ave,n_elements(emean)), linestyle=2

   ;# make a hardcopy of the expcorr plot?
   yesno = ''
   read, '## INPUT: Want a hardcopy (postscript) plot of this? (yes=y)', yesno
   IF (yesno eq 'y') THEN BEGIN
      plotfile = obs+'_expcorr.ps'
      set_plot, 'PS'
      device, filename = plotfile, $
              /color, $
              /encapsulated, $
              /portrait, $
              /helvetica
      !FANCY    = 4
      !LINETYPE = 0
      !P.FONT   = 0
      !X.THICK  = 3
      !Y.THICK  = 3
      !Z.THICK  = 3
      plot, emean, expcorr, $
            linestyle = 0, $
            psym = 10, $
            /xsty, /ysty, $
            /XLOG, /YLOG, $
            xran = [xmin,xmax], $
            yran = [ymin,ymax], $
            title = title, $
            xtitle = textoidl('R_{mid} [Pixels]'), $
            ytitle = textoidl('Norm. Exp. Correction [sbr/exp/max(exp)]')
      plotsym, 0, 0.8, /fill
      oplot, emean, expcorr, psym=8
      oplot, emean, replicate(expcorr_ave,n_elements(emean)), linestyle=2
      items = [name, obs, num2str(expcorr_ave,3)+' +/- '+num2str(expcorr_dev,3)]
      legend, items, box=0, charsize=0.8, /bottom, /left
      device, /close
      set_plot, 'x'
   ENDIF

   ;# plot original sbr vs. expcorr sbr
   xmin = 0.9*min(rmean)
   xmax = 1.1*max(rmean)
   ymin = 0.9*min(origsbr)
   ymax = 1.1*max(origsbr)
   plot, rmean, origsbr, $
         title = title, $
         xtitle = textoidl('R_{mid} [arcsec]'), $
         ytitle = textoidl('SBR and ExpCorr SBR [cts/arcsec^2]'), $
         psym = 10, $
         /xsty, /ysty, $
         /XLOG, /YLOG, $
         xran = [xmin,xmax], $
         yran = [ymin,ymax], $
         linestyle=0
   oplot, rmean, sbr, linestyle=2, psym=10
   yesno = ''
   read, '## INPUT: Press enter to continue', yesno
ENDIF ELSE $
   print, '## No exposure map provided; did you already correct this sur bri profile? I hope so.'

;###################
;###################
;# Time Correction #
;###################
;###################
;# the following step is only necessary if the 
;# input surface brightness profile is *NOT* normalized by time
print, ''
print, '## Assuming input surface brightness units assumed to be CTS/AREA'
print, '## Time normalization being applied'
sbr = sbr/exposure[0]
sbr_err = sbr_err/exposure[0]
print, '## Exposure time: '+num2str(exposure[0])+' seconds'
print, ''

;################################
;################################
;# Surface Brightness Smoothing #
;################################
;################################

;# sort in DESCENDING radial order
;# needed for DEPROJECTION to run correctly
rev = reverse(sort(rout))
sbr = sbr[rev]
sbr_err = sbr_err[rev]
rin = rin[rev]
rout = rout[rev]
rmean = rmean[rev]
rinmpc = rinmpc[rev]
routmpc = routmpc[rev]
rmeanmpc = rmeanmpc[rev]

;# plot the surface brightness profile and associated error as a
;# function of radius in arcseconds
rsbr = rmean
xmin = 0.9*min(rsbr)
xmax = 1.1*max(rsbr)
ymax = 1.1*max(sbr)
ymin = 0.9*min(sbr)
plot, rsbr, sbr, $
      title = title, $
      xtitle = textoidl('R_{mid} [arcsec]'), $
      ytitle = textoidl('Sur. Bri. [cts/sec/arcsec^2]'), $
      psym = 10, $
      /xsty, /ysty, $
      /XLOG, /YLOG, $
      xran = [xmin,xmax], $
      yran = [ymin,ymax], $
      linestyle = 0
plotsym, 0, 0.8, /fill
oplot, rsbr, sbr, psym=8
oploterr, rsbr, sbr, sbr_err
ord = where(sn LE 10.)
get = ord[0]
IF get EQ -1 THEN get = 0
print, '## S/N <= 10 @:', rsbr[get]
oplot, replicate(rsbr[get],n_elements(sbr)), sbr, linestyle=2

;# remove the central bin if unwanted
yesno = ''
print, '## Skip analyzing this cluster?'
read, '## INPUT: "yes" or just hit "enter": ', yesno
IF yesno EQ 'yes' THEN GOTO,ERROR
yesno = ''
read, '## INPUT: Remove some central bins? (yes=y): ', yesno
IF (yesno eq 'y') THEN BEGIN
   read, '## INPUT: Enter 0->max R (in arcsec): ', maxr
   use = where(rsbr GT maxr)
   sbr = sbr[use]
   rsbr = rsbr[use]
   rin = rin[use]
   rout = rout[use]
   rmean = rmean[use]
   rinmpc = rinmpc[use]
   routmpc = routmpc[use]
   rmeanmpc = rmeanmpc[use]
   sbr_err = sbr_err[use]
   maxindex = n_elements(rin)-1   
ENDIF

;# create a smoothed version of the sbr profile if wanted
origsbr = sbr
yesno = ''
read, '## INPUT: Smooth this? (yes=y): ', yesno
IF (yesno eq 'y') THEN BEGIN
   REPEAT BEGIN
      read, '## INPUT: Input smoothing length (integer): ',nsm
      sbr = smooth(origsbr,nsm)
      plot, rsbr, origsbr, $
            title = title, $
            xtitle = textoidl('R_{mid} [arcsec]'), $
            ytitle = textoidl('Sur. Bri. [cts/sec/arcsec^2]'), $
            psym = 10, $
            /xsty, /ysty, $
            /XLOG, /YLOG, $
            xran = [xmin,xmax], $
            yran = [ymin,ymax], $
            linestyle = 0
      oploterr, rsbr, sbr, sbr_err
      oplot, rsbr, sbr
      read, '## INPUT: Re-try smoothing (no=n): ', yesno
   ENDREP UNTIL (yesno eq 'n')
ENDIF

;##################
;##################
;# Regularization #
;##################
;##################

;# now fit a function to profile and use fit in lieu of data
read, '## INPUT: Perform model fitting? ', yesno
mtype  = ''
usefit = ''
refit = ''
IF ((yesno EQ 'y') OR (yesno EQ 'yes')) THEN BEGIN
   refit = ''
   REPEAT BEGIN
      void, pararr
      void, arrs
      void, allchi
      void, results
      void, resultserr
      print, '## SELECT A MODEL FOR FITTING, chose: a -- b -- c'
      print, '## a) single-beta'
      print, '## b) double-beta'
      print, '## c) Vikhlinin 2006'
      read, '## INPUT: Make selection: ', mtype
      IF ((mtype NE 'a') AND (mtype NE 'b') AND (mtype NE 'c')) THEN BEGIN
         print, '## ERROR: You gave me a bad model type; try again'
         GOTO,REDO
      ENDIF
      read, '## INPUT: Enter min and max R (in arcsec): ', minfit, maxfit
      use = where(rsbr LE maxfit AND rsbr GT minfit)
      endpt = n_elements(sbr[use])-1
      junk = sbr[use]
      endpt = junk[endpt]
      maxbin = where(sbr eq endpt)

      ;# Single-beta model
      IF mtype EQ 'a' THEN BEGIN
         parnames = ['S0','rc','beta','bgd']
         fname = 'single_beta'
         parinfo = replicate({value:0.D, $
                              fixed:0, $
                              limited:[1,1], $
                              limits:[0.D,0.D], $
                              step:[0.D], $
                              tied:['']}, n_elements(parnames))
         ;# initial guesses
         ina = [sbr[maxbin], $
                rsbr[(maxbin/2)], $
                0.5D, $
                sbr[maxbin/1.1]]
         ;# limits
         parinfo(0).limits = [0.1*sbr[maxbin],1.1*sbr[maxbin]] ;# S0
         parinfo(1).limits = [0.D,max(rsbr)]                   ;# rc
         parinfo(2).limits = [0.D,5.D]                         ;# beta
         parinfo(3).limits = [0.D,sbr[maxbin/2]]

      ;# Double-beta model
      ENDIF ELSE IF mtype EQ 'b' THEN BEGIN
         parnames = ['S01','S02','rc1','rc2','beta1','beta2']
         fname = 'double_beta'
         parinfo = replicate({value:0.D, $
                              fixed:0, $
                              limited:[1,1], $
                              limits:[0.D,0.D], $
                              step:[0.D], $
                              tied:['']}, n_elements(parnames))
         ;# initial guesses
         ina = [sbr[maxbin], $
                sbr[(maxbin/4)], $
                rsbr[maxbin/1.1], $
                rsbr[maxbin/4], $
                0.6D, $
                0.9D]
         ;# boundary conditions
         parinfo(0).limits  = [0.6*sbr[maxbin],1.1*sbr[maxbin]]  ;# S01
         parinfo(1).limits  = [0.D,0.5*sbr[maxbin]]              ;# S02
         parinfo(2).limits  = [0.D,rsbr[maxbin/2]]               ;# rc1
         parinfo(3).limits  = [rsbr[maxbin/2],10*max(rsbr)]      ;# rc2
         parinfo(4).limits  = [0.D,5.D]                          ;# beta1
         parinfo(5).limits  = [0.D,5.D]                          ;# beta2

      ;# Vikhlinin 2006 modified-beta model
      ENDIF ELSE IF mtype EQ 'c' THEN BEGIN
         parnames = ['S01','S02','rc1','rs','rc2','alpha','beta1','beta2','gamma','epsilon']
         fname = 'sbprof'
         parinfo = replicate({value:0.D, $
                              fixed:0, $
                              limited:[0,1], $
                              limits:[0.D,0.D], $
                              step:[0.D], $
                              tied:['']}, n_elements(parnames))
         ;# initial guesses
         ina = [sbr[maxbin], $
                sbr[(maxbin/3)], $
                rsbr[maxbin/1.25], $
                rsbr[maxbin/1.25], $
                rsbr[maxbin/4], $
                0.5D, $
                0.3D, $
                0.6D, $
                3.0D, $
                2.0D]
         yesno = ''
         read, '## INPUT: Make alpha strictly positive?: ', yesno
         IF ((yesno EQ 'y') OR (yesno EQ 'yes')) THEN ind = [0,1,2,3,4,5,6,7,9] ELSE ind = [0,1,2,3,4,6,7,9]
         FOR i=0,n_elements(ind)-1 DO BEGIN
            ix = ind[i]
            parinfo(ix).limited(0) = 1
            parinfo(ix).limits(0) = 0.D
         ENDFOR
         parinfo(0).limits = [0.8*sbr[maxbin],1.2*sbr[maxbin]]
         parinfo(1).limits = [0.1*sbr[maxbin],0.8*sbr[maxbin]]
         parinfo(2).limits = [0.D,rsbr[maxbin/2]]
         parinfo(3).limits = [1.D,rsbr[maxbin/2]]
         parinfo(4).limits = [rsbr[maxbin/2],1000.D]
         parinfo(5).limits = [0.D,2.D]
         parinfo(6).limits = [0.D,2.D]
         parinfo(7).limits = [0.D,2.D]
         parinfo(9).limited(1) = 1   ;# epsilon bounded on the top
         parinfo(9).limits(1)  = 5.D ;# epsilon must be less than
      ENDIF
      
      ;# run the fitter
      weights = 1./sbr_err[use]^2.
      parinfo(*).value = ina
      print, ''
      print, "## STATUS: Running mpcurvefit..."
      result = mpcurvefit(rsbr[use], sbr[use], weights, ina, sigma, FUNCTION_NAME=fname, $
                          ITMAX=5000, CHISQ=chisq, PARINFO=parinfo, STATUS=status, $
                          YERROR=residrms, ERRMSG=errmsg, DOF=dof, /DOUBLE, /QUIET, /NODERIVATIVE)
      IF status LT 0 THEN BEGIN
         message, errmsg
         GOTO, FITERR
      ENDIF
      print, ''
      print, "## Fitting complete with no errors."
      print, '## Fit parameters:'

      ;# *If* you can assume that the true reduced chi-squared
      ;# value is unity -- meaning that the fit is implicitly
      ;# assumed to be of good quality -- then the estimated
      ;# parameter uncertainties can be computed by scaling SIGMA
      ;# by the measured chi-squared value.
      ord = where(sigma LE 0, num)
      IF num GT 0 THEN sigma[ord] = 0.1*ina[ord]
      sigma = sigma*sqrt(chisq/dof)
      resid = sbr[use]-result
      FOR jj=0,n_elements(sigma)-1 DO BEGIN
         print, format='(A-10,E10.3,A7,E10.3)',parnames[jj]+': ',ina[jj],' +/- ',sigma[jj]
      ENDFOR
      print, format='(A-10,E10.3,A7,E10.3)','Residuals:',mean(resid),' +/- ',residrms
      print, format='(A-10,I10)','DOF:',dof
      print, format='(A-10,F10.3)','ChiSq:',chisq
      print, format='(A-10,F10.3)','RChiSq:',chisq/dof    

      ;# check for error from too few bins and skip ahead
      IF n_elements(result) LT n_elements(rsbr[use]) THEN BEGIN
         message, "mpcurvefit choked on something. Check by hand if you want a fit."
         result = replicate(0.0,n_elements(rsbr))
         GOTO, FITERR
      ENDIF

      ;# plot the results and the residuals
      erase
      multiplot,[1,2]
      xmin = 0.8*min(rsbr[use])
      xmax = 1.1*max(rsbr[use])
      ymax = 1.1*max(sbr[use]+sbr_err[use])
      ymin = 0.9*min(sbr[use]-sbr_err[use])
      plotsym, 0
      plot, rsbr[use], sbr[use], $
            title  = title, $
            ytitle = textoidl('Sur. Bri. [cts/sec/arcsec^2]'), $
            /xsty, /ysty, $
            /XLOG, /YLOG, $
            xran = [xmin,xmax], $
            yran = [ymin,ymax], $
            psym = 8, $
            linestyle = 0
      oploterror, rsbr, sbr, sbr_err, psym=8
      oplot, rsbr[use], result, linestyle=5
      pline = 1
      IF fname EQ 'double_beta' THEN BEGIN
         yb = ina[0]*(1.+(rsbr[use]/ina[2])^2.)^(-3.*ina[4]+0.5)
         oplot, rsbr[use], yb, linestyle=1
         yb = ina[1]*(1.+(rsbr[use]/ina[3])^2.)^(-3.*ina[5]+0.5)
         oplot, rsbr[use], yb, linestyle=2
      ENDIF
      plotsym, 0, /fill
      multiplot & $
         plot, rsbr[use], resid, $
               psym=8, $
               /xlog, $
               xtitle = textoidl('R_{mid} [arcsec]'), $
               ytitle = textoidl('Residuals'), $
               xran = [xmin,xmax], $
               /xsty, $
               linestyle = 0
      oplot, maken(1d-4,2*max(rsbr),10), replicate(0,1000), linestyle=2
      multiplot, /reset

      ;# trim the corresponding arrays
      print, '## INPUT REQUIRED:'
      read, '## INPUT: Re-try fit (no=n): ', refit
      REDO:
   ENDREP UNTIL (refit eq 'n')
   print, ''
   print, '## INPUT REQUIRED:'
   read, '## INPUT: Use fit (y/n)?', usefit
   IF usefit EQ 'y' THEN BEGIN
      GET_LUN, FITLUN
      OPENW, FITLUN, 'betafits.dat', /APPEND
      IF mtype EQ 'a' THEN BEGIN
         printf, FITLUN, format='(A-20,A20,14E12.3)', $
                 name,obs,ina[0],sigma[0],0.0,0.0,ina[1],sigma[1],0.0,0.0,ina[2],sigma[2],0.0,0.0,dof,chisq
         x    = rsbr
         s0   = ina[0]
         rc   = ina[1]
         beta = ina[2]
         ds0  = sigma[0]*abs((1.0+(x/rc)^2.)^(-3.0*beta+0.5))
         dr   = sigma[1]*abs(2.0*s0*x*rc^(-2)*(-3.0*beta+0.5)*(1.0+(x/rc)^2.)^(-3.0*beta-0.5))
         db   = sigma[2]*abs(-3.0*s0*alog10(1.0+(x/rc)^2.)*(1.0+(x/rc)^2.)^(-3.0*beta+0.5))
         sbr_err = sqrt(ds0^2. + dr^2. + db^2.)
      ENDIF ELSE IF mtype EQ 'b' THEN BEGIN
         printf, FITLUN, format='(A-20,A20,14E12.3)', $
                 name,obs,ina[0],sigma[0],ina[1],sigma[1],ina[2],sigma[2],ina[3],sigma[3],ina[4],sigma[4],ina[5],sigma[5],dof,chisq
         x   = rsbr
         s1  = ina[0]
         s2  = ina[1]
         r1  = ina[2]
         r2  = ina[3]
         b1  = ina[4]
         b2  = ina[5]
         ds1 = sigma[0]*abs((1.0+(x/r1)^2.)^(-3.0*b1+0.5))
         ds2 = sigma[1]*abs((1.0+(x/r2)^2.)^(-3.0*b2+0.5))
         dr1 = sigma[2]*abs(2.0*s1*x*r1^(-2)*(-3.0*b1+0.5)*(1.0+(x/r1)^2.)^(-3.0*b1-0.5))
         dr2 = sigma[3]*abs(2.0*s2*x*r2^(-2)*(-3.0*b2+0.5)*(1.0+(x/r2)^2.)^(-3.0*b2-0.5))
         db1 = sigma[4]*abs(-3.0*s1*alog10(1.0+(x/r1)^2.)*(1.0+(x/r1)^2.)^(-3.0*b1+0.5))
         db2 = sigma[5]*abs(-3.0*s2*alog10(1.0+(x/r2)^2.)*(1.0+(x/r2)^2.)^(-3.0*b2+0.5))
         sbr_err = sqrt(ds1^2. + dr1^2. + db1^2. + ds2^2. + dr2^2. + db2^2.)
      ENDIF ELSE IF mtype EQ 'c' THEN BEGIN
         printf, FITLUN, format='(A-20,A20,4E12.3,F10.3,E10.3,F10.3,E10.3,F10.3,E10.3,F10.3,E10.3,F10.3,E10.3,F10.3,E10.3,F10.3,E10.3,F10.3,E10.3,E10.3,E10.3)',$
                 name,obs,ina[0],sigma[0],ina[1],sigma[1],ina[2],sigma[2],ina[3],sigma[3],$
                 ina[4],sigma[4],ina[5],sigma[5],ina[6],sigma[6],ina[7],sigma[7],ina[8],sigma[8],ina[9],sigma[9],dof,chisq
         sbr_err = result*sqrt((sigma[0]/ina[0])^2.+4.*(sigma[1]/ina[1])^.2+(sigma[2]/ina[2])^.2)
      ENDIF
      FREE_LUN, FITLUN
      sbr = result
      rin = rin[use]
      rout = rout[use]
      rmean = rmean[use]
      rinmpc = rinmpc[use]
      routmpc = routmpc[use]
      rmeanmpc = rmeanmpc[use]
      sbr_err = sbr_err[use]
      maxindex = n_elements(rin)-1
   ENDIF
ENDIF
FITERR:

;################
;################
;# Deprojection #
;################
;################
;# Compute the counts/second/cm^3 (called CC below), which is the
;# emissivity, assuming spherical symmetry and the emissivity in an
;# annular bin is constant. This calculation also assumes that any
;# emission outside the outermost bin is negligible. See Kriss et
;# al. (1983ApJ...272..439K)

;# calculate sur bri in units of cts/sec
sbrint = sbr * !PI * (rout^2. - rin^2.)
sbrint_error = sbr_err * !PI * (rout^2. - rin^2.)

;# generate arrays to hold values
cc = fltarr(maxindex+1)
vsum = fltarr(maxindex+1,maxindex+1)

;# outer radius is defined by m = 0
;# for simplicity, create one r vector with maxindex+1 entries
r = [routmpc[0], rinmpc]
FOR m = 0,maxindex DO BEGIN
   FOR i = 0,maxindex DO BEGIN
      v1 = vv(r[i],r[m+1])
      v2 = vv(r[i+1],r[m+1])
      v3 = vv(r[i],r[m])
      v4 = vv(r[i+1],r[m])
      vsum[i,m] = ((v1-v2)-(v3-v4))
   ENDFOR
ENDFOR
cc[0] = sbrint[0] / vsum[0,0]
FOR m = 0,maxindex DO BEGIN
   interv = 0.0
   FOR i = 0,m-1 DO BEGIN
      interv = interv + cc[i] * vsum[i,m]
   ENDFOR
   lastv = vsum[m,m]
   cc[m] = (sbrint[m] - interv) / lastv
ENDFOR

;# warn on negative cc values
ord = where(cc LE 0.,ndum)
num = n_elements(ord)-1
ord = ord[num]
IF ndum LE 0 THEN BEGIN
   minfit = 0.0
   maxfit = max(rmeanmpc)
   print, ''
   print, '## YAY! Deprojection is all positive'
   print, '## STATUS: Automatically using full radial range',minfit,' <= R <= ',maxfit
   print, ''
ENDIF ELSE BEGIN
   ;# check for deproj not positive anywhere
   num = n_elements(where(rmeanmpc LE rmeanmpc[ord+1]))
   IF num LT 0.1*n_elements(rmeanmpc) THEN BEGIN
      print, ''
      print, '## WARNING'
      print, '## WARNING'
      print, '## WARNING'
      print, '## Number of bins interior to non-negative deprojection is less than 10% of the original bins'
      print, '## WARNING'
      print, '## WARNING'
      print, '## WARNING'
      print, ''
   ENDIF
   IF num LT 5 THEN $
      MESSAGE, '## ERROR: Too few bins for analysis; check deprojection'
   minfit = 0.0
   maxfit = rmeanmpc[ord+1]
   print, ''
   print, '## WARNING'
   print, '## WARNING'
   print, '## WARNING'
   print, '## OH NO! Deprojection goes negative at ', rmeanmpc[ord]
   print, '## WARNING'
   print, '## WARNING'
   print, '## WARNING'
   print, ''
   print, '## STATUS: Automatically trimming radial range to ',minfit,' <= R <= ',maxfit
   print, ''
ENDELSE

;# plot the results
erase
xmin = 0.9*min(rmeanmpc)
xmax = 1.1*max(rmeanmpc)
ymax = 1.1*max(cc)
ccran = cc[where(cc GT 0.)]
ymin = 0.9*min(ccran)
plot, rmeanmpc, cc, $
      title = title, $
      xtitle = textoidl('R [Mpc]'), $
      ytitle = textoidl('Emission Density (CC) [cts/s/cm^{-3}]'), $
      psym=10, $
      /xsty, /ysty, $
      /YLOG, /XLOG, $
      xran = [xmin,xmax], $
      yran = [ymin,ymax], $
      linestyle = 0
oplot, rmeanmpc, cc, psym=2

;# trim deprojected arrays
yesno = ''
;read, '## INPUT: Enter min R *AND* max R (in Mpc): ', minfit, maxfit
read, '## INPUT: Press enter to continue', yesno
use = where((rmeanmpc LE maxfit) * (rmeanmpc GE minfit))
cc = cc[use]
sbr = sbr[use]
sbr_err = sbr_err[use]
rin = rin[use]
rout = rout[use]
rmean = rmean[use]
rinmpc = rinmpc[use]
routmpc = routmpc[use]
rmeanmpc = rmeanmpc[use]
maxindex = n_elements(rin)-1
sbrint = sbr * !PI * (rout^2. - rin^2.)
sbrint_error = sbr_err * !PI * (rout^2. - rin^2.)

;# recalculate vsum... or else
oldvsum = vsum
vsum = fltarr(maxindex+1,maxindex+1)
r = [routmpc[0], rinmpc]
FOR m = 0,maxindex DO BEGIN
   FOR i = 0,maxindex DO BEGIN
      v1 = vv(r[i],r[m+1])
      v2 = vv(r[i+1],r[m+1])
      v3 = vv(r[i],r[m])
      v4 = vv(r[i+1],r[m])
      vsum[i,m] = ((v1-v2)-(v3-v4))
   ENDFOR
ENDFOR

;###############
;###############
;# Monte Carlo #
;###############
;###############

;# Create MC arrays
print, ''
print, '## Now to calculate errors for electron density'
print, '## STATUS: Performing '+num2str(nmc)+' Monte Carlo simulations...'
sbrint_array = fltarr(nmc,maxindex+1)
cc_array = fltarr(nmc,maxindex+1)
cc_mean  = fltarr(maxindex+1)
cc_error = fltarr(maxindex+1)

;# Populate MC array with gaussian variance:
;# RANDOMN which returns "one or more normally-distributed,
;# floating-point, pseudo-random numbers with a mean of zero and a
;# standard deviation of one."
;# WARNING: I suspect the random draws in IDL are not truly random (so
;# don't run like 1d6 MC draws!!!), but they are pseudo-random which is
;# good enough for most any statistician.  Someday I'll upgrade this to
;# use truly random numbers from random.org:
;# wget -q -O - 'http://www.random.org/integers/?num=100&min=0&max=1000&col=1&base=10&format=plain&rnd=new'
;# and check my quota using:
;# wget -q -O - 'http://www.random.org/quota/?ip=127.000.00.00&format=plain'
FOR imc = 0,nmc-1 DO BEGIN
   IF usefit EQ 'y' THEN $
      ranarr = randomu(k,maxindex+1,/double) $
   ELSE $
      ranarr = randomn(k,maxindex+1,/double)   
   sbrint_array[imc,*] = sbrint + (sbrint_error * ranarr)
   cc_array[imc,0] = sbrint_array[imc,0] / vsum[0,0]
   FOR m = 0,maxindex DO BEGIN
      interv = 0.0
      FOR i = 0, m-1 DO BEGIN
         interv = interv + cc_array[imc,i] * vsum[i,m]
      ENDFOR
      lastv = vsum[m,m]
      cc_array[imc,m] = (sbrint_array[imc,m] - interv)/lastv
   ENDFOR
   ord = where(cc_array[imc,*] LE 0., num)
   IF num GT 0 THEN BEGIN
      FOR dd=0,n_elements(ord)-1 DO BEGIN
         cc_array[imc,ord[dd]] = abs(cc_array[imc,ord[dd]])
      ENDFOR
   ENDIF
   IF (imc EQ (nmc-1)/4) THEN print, '## STATUS: 25% of MC'
   IF (imc EQ (nmc-1)/2) THEN print, '## STATUS: 50% of MC'
   IF (imc EQ 3*(nmc-1)/4) THEN print, '## STATUS: 75% of MC'
ENDFOR
print, '## STATUS: 100% of MC'
print, ''

;# Compute the mean and dispersion using native routines
;# An alternate method:
;# cc_mean(jmc) = abs(total(cc_array(*,jmc))/float(nmc))
;# cc_error(jmc) = sqrt(total((cc_array(*,jmc)-cc_mean(jmc))^2.)/(nmc-1.0))
FOR jmc = 0,maxindex DO BEGIN
   cc_mean[jmc]  = mean(cc_array[*,jmc])
   cc_error[jmc] = stddev(cc_array[*,jmc])
ENDFOR

;# Plot the results
erase
xmin = 0.9*min(rmeanmpc)
xmax = 1.1*max(rmeanmpc)
ymax = 1.1*max(cc_mean+cc_error)
ccran = cc_mean[where(cc_mean-cc_error GT 0.)]
ymin = 0.9*min(ccran-cc_error)
plot, rmeanmpc, cc_mean, $
      title  = title, $
      xtitle = textoidl('R [Mpc]'), $
      ytitle = textoidl('MC Mean CC [cts/s/cm^{-3}]'), $
      psym   = 10, $
      /xsty, /ysty, $
      /YLOG, /XLOG, $
      xran = [xmin,xmax], $
      yran = [ymin,ymax], $
      linestyle = 0
oploterror, rmeanmpc, cc_mean, cc_error, psym=1
yesno = ''
read, '## INPUT: Press enter to continue', yesno

;##########################
;##########################
;# Spectral Interpolation #
;##########################
;##########################
;# 1) k_spec = norm_xspec/count_rate_xspec
;# 2) norm_xspec is the normalization from the spectral fit.
;# 3) Xspec count rate **MUST BE** for same energy bandpass as the surface brightness profile.

;# If fake run, then make T(r) to be a power-law
mtype = ''
IF (imfake EQ 'yes') THEN BEGIN
   void, tx_err
   void, rspec
   void, cr
   void, fakcumpr

   ;# use Hydra A as a good test case (high-res T(r) and well behaved)
   cr    = fitfile.cr[where(fitfile.cluster EQ 'HYDRA_A')]
   rspec = fitfile.rout[where(fitfile.cluster EQ 'HYDRA_A')]
   nor0  = fitfile.norm[where(fitfile.cluster EQ 'HYDRA_A')]
   z0    = fitfile.z[where(fitfile.cluster EQ 'HYDRA_A')]
   tx    = fitfile.tx[where(fitfile.cluster EQ 'HYDRA_A')]
   txhi  = fitfile.thi[where(fitfile.cluster EQ 'HYDRA_A')]
   txlo  = fitfile.tlo[where(fitfile.cluster EQ 'HYDRA_A')]
   cosmology, z0[0], r0, /silent
   em0 = (4.0*!PI*nor0*(r0[3]*3.08d24)^2.*(1.+z0)^2.)/1d-14
   cosmology, z, rp, /silent
   nor = (em0*1d-14)/(4.0*!PI*(rp[3]*3.08d24)^2.*(1.+z)^2.)
   FOR ss=0,n_elements(tx)-1 DO BEGIN
      IF (tx[ss]-txlo[ss] GT txhi[ss]-tx[ss]) THEN $
         push, tx_err, tx[ss]-txlo[ss] ELSE $
            push, tx_err, txhi[ss]-tx[ss]
   ENDFOR

;;    ;# read-in all the mock obs data
;;    fak = mrdfits(fakesbr,1,head)
;;    fakexpt = sxpar(head,'EXPOSURE')
;;    fakcts = fak.net_counts
;;    fakrmid = fak.rmid*0.492/60.
;;    cts = 0.0
;;    iter = 0.0
;;    FOR zz=0,n_elements(fakcts)-1 DO BEGIN
;;       cts = fakcts[zz]+cts
;;       iter = iter+fakcts[zz]
;;       IF iter GE 5000 THEN BEGIN
;;          push, rspec, fakrmid[zz]
;;          push, cr, iter/fakexpt
;;          iter = 0.0
;;       ENDIF
;;       push, fakcumpr, cts
;;    ENDFOR
;;    nann = n_elements(rspec)
;;    IF nann LT 3 THEN BEGIN
;;       MESSAGE, '## ERROR: Mock observation has too few Tx annuli'
;;    ENDIF

;;    ;# build the actual profile
;;    print, ''
;;    print, ''
;;    print, '## WARNING WARNING WARNING ##'
;;    print, '## WARNING WARNING WARNING ##'
;;    print, ''
;;    print, '## This is a **MOCK** T(r)'
;;    print, ''
;;    print, '## WARNING WARNING WARNING ##'
;;    print, '## WARNING WARNING WARNING ##'
;;    print, ''
;;    print, ''
;;    print, '## Select a model for Tx(r): a -- b -- c -- d'
;;    print, '## a) isothermal'
;;    print, '## b) linear'
;;    print, '## c) asymptotic'
;;    print, '## d) Vikhlinin 2006'
;;    read, '## INPUT: Make selection: ', mtype
;;    IF mtype EQ 'a' THEN BEGIN
;;       tx = replicate(tkev,n_elements(rspec))
;;       nor = replicate(1d-3,n_elements(rspec))
;;       FOR er=0,n_elements(tx)-1 DO $
;;          push, tx_err, 0.1*tx[er]*randomu(k,/double)
;;    ENDIF ELSE IF mtype EQ 'b' THEN BEGIN
;;       read, '## INPUT: Minimum temperature in keV: ', mintx
;;       tx = maken(mintx,tkev,n_elements(rspec))
;;       nor = maken(1d-4,1d-3,n_elements(rspec))
;;       FOR er=0,n_elements(tx)-1 DO $
;;          push, tx_err, 0.1*tx[er]*randomu(k,/double)
;;    ENDIF ELSE IF mtype EQ 'c' THEN BEGIN
;;       tx = tkev-((rspec-rspec[nann-1])+(1.0/tkev))^(-1.0)
;;       nor = 1d-3-((rspec-rspec[nann-1])+(1.0/1d-3))^(-1.0)
;;       tx[nann-1] = 0.5*tx[nann-2]
;;       nor[nann-1] = 0.5*nor[nann-2]
;;       FOR er=0,n_elements(tx)-1 DO $
;;          push, tx_err, 0.1*tx[er]*randomu(k,/double)
;;    ENDIF ELSE IF mtype EQ 'd' THEN BEGIN
;;       read, '## INPUT: Minimum temperature in keV (try 1d-4): ', mintx
;;       read, '## INPUT: Maximum temperature in keV (try 10): ', t0
;;       read, '## INPUT: Cooling region in Mpc      (try 0.1-3.0; 1.0): ', rt
;;       read, '## INPUT: Cooling region in kpc      (try 16-200; 50): ', rcool
;;       read, '## INPUT: Slope cooling region       (try 0.08-10.0; 1.5): ', acool
;;       rcool  = rcool/dang/60.     ;# fake cooling region
;;       rt     = rt*1000./dang/60.  ;# fake cooling region
;;       ap     = 0.08               ;# how quickly profile turns over
;;       b      = 3.00               ;# how flat the profile is at large radii
;;       c      = 0.5                ;# how flat the profile is at large radii
;;       t0     = 0.5*tkev           ;# max temp in keV
;;       tmint0 = mintx/t0           ;# Tmin/T0
;;       num1   = ((rspec/rcool)^acool)+(tmint0)
;;       den1   = 1.+(rspec/rcool)^acool
;;       num2   = (rspec/rt)^(-ap)
;;       den2   = (1.+(rspec/rt)^b)^(c/b)
;;       term1  = num1/den1
;;       term2  = num2/den2
;;       tx     = t0*term1*term2
;;       FOR er=0,n_elements(tx)-1 DO $
;;          push, tx_err, 0.1*tx[er]*randomu(k,/double)
;;       plot, rspec, tx
;;    ENDIF ELSE GOTO, ERROR
   k_xspec_spectral = (nor/cr)
   k_xspec_interp = interpol(k_xspec_spectral, rspec, rout/60.)
ENDIF ELSE BEGIN
   cr    = fitfile.cr[where(fitfile.obsid EQ obs)]
   rspec = fitfile.rout[where(fitfile.obsid EQ obs)]
   nor   = fitfile.norm[where(fitfile.obsid EQ obs)]
   tx    = fitfile.tx[where(fitfile.obsid EQ obs)]
   txhi  = fitfile.thi[where(fitfile.obsid EQ obs)]
   txlo  = fitfile.tlo[where(fitfile.obsid EQ obs)]
   nor2  = fitfile.norm2[where(fitfile.obsid EQ obs)]
   tx2   = fitfile.tx2[where(fitfile.obsid EQ obs)]
   txhi2 = fitfile.thi2[where(fitfile.obsid EQ obs)]
   txlo2 = fitfile.tlo2[where(fitfile.obsid EQ obs)]

   ;# this is only useful if the mekal2t model was used
   print, 'Checking for mekal2t model'
   FOR tt=0,n_elements(tx)-1 DO BEGIN
      IF ((tx2[tt] GT 0) AND (tx2[tt] LT tx[tt])) THEN BEGIN
         print, 'mekal2t model found: Tx1 = ',tx[tt],' > Tx2 = ',tx2[tt]
         push, tout, tx2[tt]
         push, tloout, txlo2[tt]
         push, thiout, txhi2[tt]
         push, nout, nor2[tt]
      ENDIF ELSE BEGIN 
         push, tout, tx[tt]
         push, tloout, txlo[tt]
         push, thiout, txhi[tt]
         push, nout, nor[tt]
      ENDELSE
   ENDFOR
   tx = tout
   txhi = thiout
   txlo = tloout
   nor = nout

;print, '@#$@#$@#$@#$@#$@#$$!#@$!@#$ TESTING!!!!!!!'
;tx = [tx[0],tx[4],tx[8]]
;txhi = [txhi[0],txhi[4],txhi[8]]
;txlo = [txlo[0],txlo[4],txlo[8]]
;cr = [cr[0],cr[4],cr[8]]
;nor = [nor[0],nor[4],nor[8]]
;rspec = [rspec[0],rspec[4],rspec[8]]

   ;# Handle the norm interp
   k_xspec_spectral = (nor/cr)
   k_xspec_interp = interpol(k_xspec_spectral, rspec, rout/60.)
    
   ;# Fill err array
   FOR ss=0,n_elements(tx)-1 DO BEGIN
      IF (tx[ss]-txlo[ss] GT txhi[ss]-tx[ss]) THEN $
         push, tx_err, tx[ss]-txlo[ss] ELSE $
            push, tx_err, txhi[ss]-tx[ss]
   ENDFOR
ENDELSE

;# Check for tx that aren't peaked in the center
;print, ''
;print, '## YOU ARE TESTING!!!'
;print, ''
IF tx[0] GE tx[1] THEN flatten = 'no' ELSE flatten = 'yes'

;# Plot the profile
origtx = tx
xmin = 0.9*min(rspec)
xmax = 1.1*max(rspec)
ymax = 1.1*max(tx)
ymin = 0.9*min(tx)
plot, rspec, origtx, $
      title = title, $
      psym = 10, $
      xtitle = textoidl('R [arcmin]'), $
      ytitle = textoidl('Temperature [keV]'), $
      /xsty, /ysty, $
      xran = [xmin,xmax], $
      yran = [ymin,ymax]
oploterr, rspec, tx, tx_err

;# smoothing the temp profile eliminates possible artifacts from
;# deprojection in Xspec; this is not needed with projected temp
yesno = ''
read, '## INPUT: Smooth this? (yes=y): ', yesno
IF (yesno eq 'y') THEN BEGIN
   REPEAT BEGIN
      read, '## INPUT: Input smoothing length (integer): ',nsm
      tx = smooth(origtx,nsm)
      plot, rspec, origtx, $
            title = title, $
            xtitle = textoidl('R [arcmin]'), $
            ytitle = textoidl('Temperature [keV]'), $
            /xsty, /ysty, $
            xran = [xmin,xmax], $
            yran = [ymin,ymax]
      oploterr, rspec, tx, tx_err
      oplot, rspec, tx, linestyle=2
      read, '## INPUT: Re-try smoothing (no=n): ', yesno
   ENDREP UNTIL (yesno eq 'n')
ENDIF

;# interpolate the tx onto the sbr grid
tx_interp = interpol(tx, rspec, rout/60.)
tx_err_interp = interpol(tx_err, rspec, rout/60.)

;################################
;################################
;# Central Temperature Assembly #
;################################
;################################

IF (imfake NE 'yes') THEN BEGIN

   ;# only flatten profiles that aren't highest in center
   IF flatten EQ 'yes' THEN BEGIN
      ;# remember indices are 'flipped' (inner is outer).
      ;# create a vector where the central bins have Tx(rmin).
      wcentral = where((rout GE 0.0) AND (rout/60. LE min(rspec)),nwcentral)
      IF nwcentral LE 0 THEN nwcentral = 1
      wouter = where(rout/60. GT min(rspec))
      wmintx = where(rspec EQ min(rspec))
      xc = findgen(nwcentral)+1.0
      xc = xc/xc
      terr_central = xc * tx_err(wmintx[0])
      t_central = xc * tx(wmintx[0])
      tx_interp_flatmin = [tx_interp(wouter), t_central]
      tx_err_interp = [tx_err_interp(wouter), terr_central]
   ENDIF ELSE BEGIN
      tx_interp_flatmin = tx_interp
      tx_err_interp = tx_err_interp
   ENDELSE

   ;# plot temperature profiles
   xmin = 0.9*min(rout/60.)
   xmax = 1.1*max(rout/60.)
   ymax = 1.1*max(tx_interp)
   ymin = 0.9*min(tx_interp)
   plot, rout/60., tx_interp, $
         title = title, $
         xtitle = textoidl('R_{out} [arcmin]'), $
         ytitle = textoidl('Temperature [keV]'), $
         /xsty, /ysty, $
         /xlog, $
         xran = [xmin,xmax], $
         yran = [ymin,ymax]
   oplot, rout/60., tx_interp_flatmin, linestyle=1
   yesno = ''
   read, '## INPUT: Press enter to continue', yesno
ENDIF ELSE BEGIN
   tx_interp_flatmin = tx_interp
   tx_err_interp = tx_err_interp
ENDELSE

;#############################################
;#############################################
;# Electron Density and Entropy Calculations #
;#############################################
;#############################################

;# we can use xspec normalization or the interpolated version:
;# k_xspec or k_xspec_interp. cc must be in counts/sec/Mpc^3
ion        = 1.4/1.2
nelec_itpl = sqrt(ion * k_xspec_interp * cc * 4.0 * !PI * da^2. * (1.0+z)^2. / 3.0856d10)
nelec_mc   = sqrt(ion * k_xspec_interp * cc_mean * 4.0 * !PI * da^2. * (1.0+z)^2. / 3.0856d10)
nelec_err  = 0.5 * nelec_mc/cc_mean * cc_error

;# check to see if cc converges to cc_mean reliably
theta_c = 30.0 * (!PI/180./60./60.) ;# arcseconds to radians
beta    = 0.592                     ;# have to assume sbr prof behavior
a1      = ((1.d-14/k_xspec_interp(maxindex)) * sqrt(!PI) * (gamma(3.*beta-0.5)*theta_c))/(4.*!PI*((1.+z)^3.)*gamma(3.*beta))
sxo     = sbr(maxindex) * (180.*60.*60./!PI)^2. ;# cts/s/steradian
ne0     = sqrt(sxo/a1/da/3.0856d24)
print, '## Estimate of central electron density'
print, FORMAT='(A-10,E8.2)','## ne0 = ',ne0

;# plot the electron density profile
xmin = 0.9*min(routmpc)
xmax = 1.1*max(routmpc)
ymax = 1.1*max(nelec_itpl)
ymin = 0.9*min(nelec_itpl)
plot, routmpc, nelec_itpl, $
      title = title, $
      xtitle = textoidl('R_{out} [Mpc]'), $
      ytitle = textoidl('n_{elec} [cm^{-3}]'), $
      /xsty, /ysty, $
      /xlog, /ylog, $
      psym = 10, $
      xran = [xmin,xmax], $
      yran = [ymin,ymax]
plotsym, 0, 0.8, /fill
oplot, routmpc, nelec_itpl, psym=8
oploterr, routmpc, nelec_itpl, nelec_err

;# smooth n_e if you dare
onelec = nelec_itpl
nelec_sm = onelec
yesno = ''
read, '## INPUT: Smooth electron density profile? (yes=y): ', yesno
IF (yesno eq 'y') THEN BEGIN
   REPEAT BEGIN
      read, '## INPUT: Input smoothing length (integer): ',nsm
      nelec_sm = smooth(onelec,nsm)
      plot, routmpc, nelec_sm, $
            title = title, $
            xtitle = textoidl('R_{out} [Mpc]'), $
            ytitle = textoidl('Smoothed n_{elec} [cm^{-3}]'), $
            /xsty, /ysty, $
            /xlog, /ylog, $
            xran = [xmin,xmax], $
            yran = [ymin,ymax], $
            psym=0, $
            linestyle=2
      plotsym, 0, 0.8, /fill
      oplot, routmpc, onelec, psym=8
      oploterr, routmpc, onelec, nelec_err
      read, '## INPUT: Continue smoothing (no=n): ', yesno
   ENDREP UNTIL (yesno eq 'n')
ENDIF

;####################
;####################
;# Mass Derivations #
;####################
;####################
;# 1) Assume hydrostatic equilibrium:
;#       M(<r) = kTr/(mu m G) * (dlog ne/dlog r + dlog T/dlog r)
;# 2) Assume dlog T/dlog r << dlog ne/dlog r
;# 3) Convert to running differential:
;#       delta log ne / delta log r = log (ni/ni+1)/ log(ri/ri+1)
;# 4) Estimate a running dlog ne/ dlog r slope piecewise
;# 5) Heavily smooth ne(r)

dlognedlogr = fltarr(maxindex+1)
FOR i=0,maxindex-1 DO BEGIN
   dlognedlogr(i) = alog10(nelec_sm(i+1)/nelec_sm(i))/alog10(rout(i+1)/rout(i))
ENDFOR
dlognedlogr(maxindex) = dlognedlogr(maxindex-1)
dlognedlogr = smooth(dlognedlogr,4)

;# this differential function is very jagged - smooth it heavily 
xmin = 0.9*min(rout)
xmax = 1.1*max(rout)
ymax = 1.1*max(dlognedlogr)
ymin = 0.9*min(dlognedlogr)
plot, rout, dlognedlogr, $
      title = title, $
      xtitle = textoidl('R_{out} [arcsec]'), $
      ytitle = textoidl('d(ln n_e)/d(ln r)'), $
      /xsty, /ysty, $
      xran = [xmin,xmax], $
      yran = [ymin,ymax]
odnedr = dlognedlogr
yesno = ''
print, '## This diff. func requires HEAVY smoothing.'
read, '## INPUT: Smooth this? (yes=y): ', yesno
IF (yesno eq 'y') THEN BEGIN
    REPEAT BEGIN
        read, '## INPUT: Input smoothing length (integer): ',nsm
        dlognedlogr = smooth(odnedr,nsm)
        plot, rout, dlognedlogr, $
              title = title, $
              xtitle = textoidl('R_{out} [arcsec]'), $
              ytitle = textoidl('d(ln n_e)/d(ln r)'), $
              /xsty, /ysty, $
              xran = [xmin,xmax], $
              yran = [ymin,ymax]
        oplot, rout, odnedr, linestyle=2
        read, '## INPUT: Continue smoothing (no=n): ', yesno
    ENDREP UNTIL (yesno eq 'n')
ENDIF

;# calculate final mass
Mgrav = -3.7d13 * tx_interp * routmpc * dlognedlogr
M_err = tx_err_interp/tx_interp * Mgrav * 1.1/1.5
print, '## M_grav(',num2str(max(routmpc),3),' Mpc) = ',num2str(max(Mgrav)),' M_solar'

;# plot the mass profile
xmin = 0.9*min(routmpc)
xmax = 1.1*max(routmpc)
ymax = 1.1*max(Mgrav)
ymin = 0.9*min(Mgrav)
plot, routmpc, Mgrav, $
      title = title, $
      xtitle = textoidl('R_{out} [Mpc]'), $
      ytitle = textoidl('M_{tot}(< r) [M_{solar}]'), $
      /xsty, /ysty, $
      /xlog, /ylog, $
      xran = [xmin,xmax], $
      yran = [ymin,ymax]
read, '## INPUT: Press enter to continue', yesno

;# gas mass profile
routcm = routmpc*3.08d24
rincm = rinmpc*3.08d24
vol = 4.0*!pi*(routcm^3.-rincm^3.)/3.0
mu    = 0.597                      ;# mean molecular weight of ICM assuming primordial gas comp
mp    = 1.672621637d-27            ;# mass proton in kgrams
msun  = 1.9891d30                  ;# mass sun in kgrams
tm = (nelec_sm*mu*mp)*vol/msun
tm = reverse(tm)
dm = 0.0
void, mdep
FOR i=0,n_elements(tm)-1 DO BEGIN
   dm = dm+tm[i]
   push, mdep, dm
ENDFOR
mgas = reverse(mdep)

;# semi-useful values
;r500 = (rdelta(500,z,tkev,/silent))
;r200 = (rdelta(200,z,tkev,/silent))
;rvir = 0.5*(rdelta(180,z,tkev,/silent))
;print, '## R200 = ',r200,' Mpc h70^-1'
;print, '## R500 = ',r500,' Mpc h70^-1'
;print, '## R_virial = ',rvir,' Mpc h70^-1'

;#######################
;#######################
;# Pressure Derivation #
;#######################
;#######################

;# electron gas pressure only
p      = tx_interp * nelec_sm
p_flat = tx_interp_flatmin * nelec_sm
p_err  = p * sqrt((tx_err_interp/tx_interp)^2. + (nelec_err/nelec_sm)^2.)

;# plot the pressure profile
xmin = 0.9*min(rmeanmpc)
xmax = 1.1*max(rmeanmpc)
ymax = 1.1*max(p)
ymin = 0.9*min(p)
plot, rmeanmpc, p, $
      title = title, $
      xtitle = textoidl('R_{mean} [Mpc]'), $
      ytitle = textoidl('Pressure [keV cm^{-3}]'), $
      /xsty, /ysty, $
      /xlog, /ylog, $
      xran = [xmin,xmax], $
      yran = [ymin,ymax]
plotsym, 0, 0.8, /fill
oplot, rmeanmpc, p, psym=8
oploterr, rmeanmpc, p, p_err
read, '## INPUT: Press enter to continue', yesno

;######################
;######################
;# Entropy Derivation #
;######################
;######################

s      = tx_interp/nelec_sm^(2./3.)
s_flat = tx_interp_flatmin/nelec_sm^(2./3.)
s_err  = s * sqrt((tx_err_interp/tx_interp)^2. + 4./9.*(nelec_err/nelec_sm)^2.)

xmin = 0.9*min(rmeanmpc)
xmax = 1.1*max(rmeanmpc)
ymax = 1.1*max(s)
ymin = 0.9*min(s)
plot, rmeanmpc, s, $
      title  = title, $
      xtitle = textoidl('R_{mean} [Mpc]'), $
      ytitle = textoidl('Entropy [keV cm^2]'), $
      /xlog, /ylog, $
      /xsty, /ysty, $
      xran = [xmin,xmax], $
      yran = [ymin,ymax], $
      psym = 0
plotsym, 0, 0.8, /fill
oplot, rmeanmpc, s, psym=8
oploterror, rmeanmpc, s, s_err

;##################################
;##################################
;# Fitting, Plotting, and Logging #
;##################################
;##################################

;# Read limits for fitting profile
read, '## INPUT: Enter R limits (in Mpc) as <Rmin>,<Rmax>: ', Rminfit, Rmaxfit
wfit = where((rmeanmpc GE Rminfit) * (rmeanmpc LE Rmaxfit))

;# Open a logfile to store fit info
savefits = ''
logfile = ''
read, '## INPUT: Want to save fits results? (yes=y): ', savefits
IF (savefits eq 'y') OR (savefits eq 'yes') THEN BEGIN
    logfile = obs+'_results.log'
    GET_LUN, RESLUN
    OPENW, RESLUN, logfile
    wnam  = strcompress(clustername,/remove_all)
    wobs  = strcompress(obs,/remove_all)
    wrmin = strcompress(sigfig(Rminfit,2),/remove_all)
    wrmax = strcompress(sigfig(Rmaxfit,2),/remove_all)
    wann  = strcompress(n_elements(wfit),/remove_all)
    printf, RESLUN, format='(A-20,A20,A8,A8,A8,A6,8A15,A10)',$
            '#Cluster','Obsid','Tmode','Rmin','Rmax','Ann','K0',$
            'K0_err','K100','K100_err','Power-law','Plaw_err','Chisq','Prob','DOF'
ENDIF

;# fit profile using Markwardt's Levenburg-Marquardt non-linear least squares algorithm
;# K0 != 0
tmode = 'itpl'
a = [10., 150.0, 1.5]
weights = 1./s_err[wfit]^2.
parinfo = replicate({value:0.D, $
                     fixed:0, $
                     limited:[0,0], $
                     limits:[0.D,0.D], $
                     step:[0.D], $
                     tied:['']}, n_elements(a))
;parinfo(0).limited(0) = 1
;parinfo(1).limited(0) = 1
parinfo(*).value = a
result = mpcurvefit(rmeanmpc[wfit], s[wfit], weights, a, sigma_entropy, $
                    FUNCTION_NAME='fit_form_100', $
                    ITMAX = 100, $
                    CHISQ = chisq_entropy, $
                    PARINFO = parinfo, $
                    STATUS = status, $
                    ERRMSG = errmsg, $
                    DOF = dof, $
                    /AUTODERIVATIVE, $
                    /QUIET)
prob_est = 1.0-IGAMMA((0.5*n_elements(rmeanmpc[wfit])-3), 0.5*chisq_entropy)
print, '## ITPL, K0 != 0'
print, format='(A10,A10,A10,A10,A10,A10,A10)', 'K0','K0err','K100','Plaw','Chisq', 'DOF', 'RedChisq'
print, format='(F10.3,F10.3,F10.3,F10.3,F10.3,I10,F10.3)', a[0],sigma_entropy[0],a[1],a[2],chisq_entropy, dof, chisq_entropy/dof
IF (savefits eq 'y') THEN $
   printf, RESLUN, format='(A-20,A20,A8,A8,A8,A6,7F15.5,E15.5,I10)',$
           wnam,wobs,tmode,wrmin,wrmax,wann,a[0],sigma_entropy[0],a[1],$
           sigma_entropy[1],a[2],sigma_entropy[2],chisq_entropy,prob_est,dof

;# Fix K0 = 0
a = [0.0, a[1], a[2]]
weights = 1./s_err[wfit]^2.
parinfo = replicate({value:0.D, $
                     fixed:0, $
                     limited:[0,0], $
                     limits:[0.D,0.D], $
                     step:[0.D], $
                     tied:['']}, n_elements(a))
parinfo(0).fixed = 1
parinfo(*).value = a
;parinfo(1).limited(0) = 1
result_s0 = mpcurvefit(rmeanmpc[wfit], s[wfit], weights, a, sigma_entropy, $
                    FUNCTION_NAME='fit_form_100', $
                    ITMAX = 100, $
                    CHISQ = chisq_entropy, $
                    PARINFO = parinfo, $
                    STATUS = status, $
                    ERRMSG = errmsg, $
                    DOF = dof, $
                    /AUTODERIVATIVE, $
                    /QUIET)
prob_est = 1.0-IGAMMA((0.5*n_elements(rmeanmpc[wfit])-3), 0.5*chisq_entropy)
print, '## ITPL, K0 = 0'
print, format='(A10,A10,A10,A10,A10,A10,A10)', 'K0','K0err','K100','Plaw','Chisq', 'DOF', 'RedChisq'
print, format='(F10.3,F10.3,F10.3,F10.3,F10.3,I10,F10.3)', a[0],sigma_entropy[0],a[1],a[2],chisq_entropy, dof, chisq_entropy/dof
IF (savefits eq 'y') THEN $
  printf, RESLUN, format='(A-20,A20,A8,A8,A8,A6,7F15.5,E15.5,I10)',$
          wnam,wobs,tmode,wrmin,wrmax,wann,a[0],sigma_entropy[0],a[1],$
          sigma_entropy[1],a[2],sigma_entropy[2],chisq_entropy,prob_est,dof

;# Assume flat central temperature behavior
;# K0 != 0
tmode = 'flat'
a = [10, 150.0, 1.5]
weights = 1./s_err[wfit]^2.
parinfo = replicate({value:0.D, $
                     fixed:0, $
                     limited:[0,0], $
                     limits:[0.D,0.D], $
                     step:[0.D], $
                     tied:['']}, n_elements(a))
;parinfo(0).limited(0) = 1
;parinfo(1).limited(0) = 1
parinfo(*).value = a
result_flat = mpcurvefit(rmeanmpc[wfit], s_flat[wfit], weights, a, sigma_entropy, $
                    FUNCTION_NAME='fit_form_100', $
                    ITMAX = 100, $
                    CHISQ = chisq_entropy, $
                    PARINFO = parinfo, $
                    STATUS = status, $
                    ERRMSG = errmsg, $
                    DOF = dof, $
                    /AUTODERIVATIVE, $
                    /QUIET)
prob_est = 1.0-IGAMMA((0.5*n_elements(rmeanmpc[wfit])-3), 0.5*chisq_entropy)
print, '## FLAT, K0 != 0'
print, format='(A10,A10,A10,A10,A10,A10,A10)', 'K0','K0err','K100','Plaw','Chisq', 'DOF', 'RedChisq'
print, format='(F10.3,F10.3,F10.3,F10.3,F10.3,I10,F10.3)', a[0],sigma_entropy[0],a[1],a[2],chisq_entropy, dof, chisq_entropy/dof
IF (savefits eq 'y') THEN $
  printf, RESLUN, format='(A-20,A20,A8,A8,A8,A6,7F15.5,E15.5,I10)',$
          wnam,wobs,tmode,wrmin,wrmax,wann,a[0],sigma_entropy[0],a[1],$
          sigma_entropy[1],a[2],sigma_entropy[2],chisq_entropy,prob_est,dof

;# Assume flat central temperature behavior and K0 = 0
a = [0.0, a[1], a[2]]
weights = 1./s_err[wfit]^2.
parinfo = replicate({value:0.D, $
                     fixed:0, $
                     limited:[0,0], $
                     limits:[0.D,0.D], $
                     step:[0.D], $
                     tied:['']}, n_elements(a))
parinfo(0).fixed = 1
parinfo(*).value = a
;parinfo(1).limited(0) = 1
result_s0_flat = mpcurvefit(rmeanmpc[wfit], s_flat[wfit], weights, a, sigma_entropy, $
                    FUNCTION_NAME='fit_form_100', $
                    ITMAX = 100, $
                    CHISQ = chisq_entropy, $
                    PARINFO = parinfo, $
                    STATUS = status, $
                    ERRMSG = errmsg, $
                    DOF = dof, $
                    /AUTODERIVATIVE, $
                    /QUIET)
prob_est = 1.0-IGAMMA((0.5*n_elements(rmeanmpc[wfit])-3), 0.5*chisq_entropy)
print, '## FLAT, K0 != 0'
print, format='(A10,A10,A10,A10,A10,A10,A10)', 'K0','K0err','K100','Plaw','Chisq', 'DOF', 'RedChisq'
print, format='(F10.3,F10.3,F10.3,F10.3,F10.3,I10,F10.3)', a[0],sigma_entropy[0],a[1],a[2],chisq_entropy, dof, chisq_entropy/dof
IF (savefits eq 'y') THEN BEGIN
    printf, RESLUN, format='(A-20,A20,A8,A8,A8,A6,7F15.5,E15.5,I10)',$
            wnam,wobs,tmode,wrmin,wrmax,wann,a[0],sigma_entropy[0],a[1],$
            sigma_entropy[1],a[2],sigma_entropy[2],chisq_entropy,prob_est,dof
    FREE_LUN, RESLUN
ENDIF

;# Make arrays for plotting radii vs. entropy, pressure, mass
ord      = where((rmeanmpc LE Rmaxfit) AND (rmeanmpc GE Rminfit))
rpl      = rmeanmpc[ord]*1000.
rmeankpc = rmeanmpc*1000.
spl      = s[ord]
serrpl   = s_err[ord]
sflpl    = s_flat[ord]

;# Define the min and max ranges to plot
xmin     = 0.75*min(rpl)
xmax     = 1.25*max(rpl)
ymax     = 1.25*max(spl+serrpl)
IF (min(spl-serrpl) GT min(result_s0) OR min(spl-serrpl) LE 0) THEN $
  ymin = 0.9*min(result_s0) ELSE $
  ymin = 0.9*min(spl-serrpl)

;# Plot the profiles
plotsym, 4, 0.8, /fill
plot, rpl, spl, $
  /xlog, /ylog, $
  /xsty, /ysty, $
  xtitle = textoidl('R_{mid} [kpc]'), $
  ytitle = textoidl('K [keV cm^2]'), $
  title = name, $
  psym = 8, $
  charsize = 0.8, $
  xrange = [xmin,xmax], $
  yrange = [ymin,ymax]
oploterror, rpl, spl, serrpl, psym=8
plotsym, 8, 0.8, /fill
oplot, rpl, sflpl, psym=8
oploterror, rpl, sflpl, serrpl, psym=8
oplot, rmeankpc[wfit], result, linestyle=1, psym=0
oplot, rmeankpc[wfit], result_s0, linestyle=2, psym=0
oplot, rmeankpc[wfit], result_flat, linestyle=3, psym=0
oplot, rmeankpc[wfit], result_s0_flat, linestyle=4, psym=0

;# draw the legend
items = [name, $
         obs,$
         'Intpl',$
         'Flat',$
         textoidl('Intpl K0 \neq 0'),$
         textoidl('Intpl K0 = 0'),$
         textoidl('Flat K0 \neq 0'),$
         textoidl('Flat K0 = 0')]
parr = [0,0,5,6,0,0,0,0]
larr = [-99,-99,0,0,1,2,3,4]
legend, items, linestyle=larr, psym=parr, box=0, charsize=0.8, /top, /left, /fill

;# create hardcopy
yesno=''
read,'Want a hardcopy (postscript) plot of this? (yes=y)', yesno
IF (yesno eq 'y') OR (yesno EQ 'yes') THEN BEGIN
    plotfile = obs+'_splot.ps'
    set_plot,'PS'
    device,filename = plotfile, $
           /color, $
           /encapsulated, $
           /portrait, $
           /helvetica
    !FANCY    = 4
    !LINETYPE = 0
    !P.FONT   = 0
    !X.THICK  = 3
    !Y.THICK  = 3
    !Z.THICK  = 3
    plotsym, 4, 0.8, /fill
    plot, rpl, spl, $
          /xlog,$
          /ylog, $
          /xsty, $
          /ysty, $
          xtitle = textoidl('R_{mid} [kpc]'), $
          ytitle = textoidl('K [keV cm^2]'), $
          title = name, $
          psym = 8, $
          charsize = 0.8, $
          xrange = [xmin,xmax], $
          yrange = [ymin,ymax]
    oploterror, rpl, spl, serrpl, psym=8
    plotsym, 8, 0.8, /fill
    oplot, rpl, sflpl, psym=8
    oploterror, rpl, sflpl, serrpl, psym=8
    oplot, rmeankpc[wfit], result, linestyle=1, psym=0
    oplot, rmeankpc[wfit], result_s0, linestyle=2, psym=0
    oplot, rmeankpc[wfit], result_flat, linestyle=3, psym=0
    oplot, rmeankpc[wfit], result_s0_flat, linestyle=4, psym=0

    ;# draw the legend
    legend, items, linestyle=larr, psym=parr, box=0, charsize=0.8, /top, /left, /fill

    device,/close
    set_plot,'X'
ENDIF       

;# Dump results to a table
yesno=''
read,'Want an ASCII table?', yesno
IF (yesno eq 'y') OR (yesno eq 'yes') THEN BEGIN
    tablefile=obs+'_table.dat'
    GET_LUN, LOGLUN
    OPENW, LOGLUN, tablefile
    printf, LOGLUN, clustername
    printf, LOGLUN, 'Cosmology', h0, omega_m, lambda
    printf, LOGLUN, 'Number of MC iterations:', nmc
    printf, LOGLUN, 'Source FITS files: ', strcompress(filename,/remove_all)
    printf, LOGLUN, format='(12A15)','#RIN_MPC','ROUT_MPC','N_elec(cm-3)','Sigma_Ne','K(KeV cm2)','K_flat(KeV cm2)','K_err','P','P_flat','P_err','Mgrav','Merr'
    outarray = [ [rinmpc], [routmpc], [nelec_sm], [nelec_err], [s], [s_flat], [s_err], [p], [p_flat], [p_err], [Mgrav], [M_err] ]
    outarray = transpose(outarray)
    printf, LOGLUN, format='(2F15.5,2E15.5,3F15.5,5E15.5)', outarray 
    FREE_LUN, LOGLUN
ENDIF

ERROR:
IF (count NE 1) THEN BEGIN
    GET_LUN, ERRLUN
    OPENW, ERRLUN, 'log_kfitter', /APPEND
    print, '## ERROR: ',filename,' does not exist, exiting.'
    printf, ERRLUN, '## ERROR: ',filename,' does not exist, exiting.'
    FREE_LUN, ERRLUN
ENDIF

IF (notx EQ 'yes') THEN BEGIN
    GET_LUN, ERRLUN
    OPENW, ERRLUN, 'log_kfitter', /APPEND
    print, '## ERROR: ',obs,' does not have temperature profile, exiting.'
    printf, ERRLUN, '## ERROR: ',obs,' does not have temperature profile, exiting.'
    FREE_LUN, ERRLUN
ENDIF
RETURN
!QUIET = 0
END
