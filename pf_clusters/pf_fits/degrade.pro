;#######################
;#######################

FUNCTION vv, rii, rjj
;# deprojected volume element using the effective radius of
;# McLaughlin 1999 AJ 117 2398

IF rii le rjj then RETURN,0.0
vvv = 4.*!PI/3. * (rii^2 - rjj^2)^(1.5)

RETURN,vvv
END

;#######################
;#######################

PRO degrade, filename, tempfitfile, clustername, tkeV, z, nmc, obs, exfile, Rminfit, Rmaxfit

;#######################
;#######################
;##   Main Program    ##
;#######################
;#######################

;# makes all plots look nice
!FANCY    = 4
!LINETYPE = 0
!X.THICK  = 2
!Y.THICK  = 2
!Z.THICK  = 2

;# take alternative assumption regarding geometry
Omega_m = 0.3
lambda  = 0.7
H0      = 70.
yousuck = 'no'

;# read in the temperature fits
restore,'~/research/redux/scripts/xspectemp_rin_normerr_src.sav'        
fitfile = read_ascii(tempfitfile,template=xspectemp_rin_normerr_src)

;# check for file existance
check  = findfile(filename,count=count)
ord    = where(fitfile.obsid EQ obs)
notx   = 'no'
IF (count NE 1) THEN GOTO,ERROR
IF ord[0] EQ -1 THEN BEGIN
    notx = 'yes'
    GOTO, ERROR
ENDIF

;# read in sur bri profile assuming units CTS/PIXEL**2
print, '## Opening ',filename

;# read all the columns into a structure named fits
fits       = mrdfits(filename,1,hdr,/silent)
exposure   = fits.exposure
bgexposure = fits.bg_exposure
sbrpix     = fits.sur_bri
sbr_errpix = fits.sur_bri_err
rinpix     = fits.rin
routpix    = fits.rout
sn         = fits.net_counts/sqrt(fits.bg_counts)
snerr      = sn*sqrt((fits.net_err/fits.net_counts)^2.+(1./4.)*(fits.bg_err/fits.bg_counts)^2.)
maxindex   = n_elements(rinpix)-1

;# check for zero bins
IF sbrpix[0] LE 0. THEN BEGIN
   ord = where(rinpix GT rinpix[0])
   sbrpix     = sbrpix[ord]
   sbr_errpix = sbr_errpix[ord]
   rinpix     = rinpix[ord]
   routpix    = routpix[ord]
   sn         = sn[ord]
   snerr      = snerr[ord]
   maxindex   = n_elements(rinpix)-1
ENDIF

;# format the cluster names for plotting
cname = strcompress(clustername,/remove_all)
name  = cname
title = cname+'   '+strcompress(obs,/remove_all)

;# convert radii units from pixels to arcseconds
pixtoarcsec = 0.492             ;# ACIS scale is 0.492"/pixel
rin = pixtoarcsec * rinpix
rout = pixtoarcsec * routpix
rmean = (rin+rout)/2.
sbr = sbrpix / (pixtoarcsec)^2
sbr_err = sbr_errpix / (pixtoarcsec)^2

;# calculate the radii in Mpc
cosmology, z, result, /silent
da = result[3]                  ;# cosmology returns da in Mpc
rinmpc = rin * result[4]/1000.  ;# cosmology returns dang in kpc/"
routmpc = rout * result[4]/1000.
rmeanmpc = (rinmpc+routmpc)/2.

;#######################
;#######################
;# Exposure Correction #
;#######################
;#######################

IF exfile NE 'nihil' THEN BEGIN
   expprof = mrdfits(exfile,1,/silent)
   expcorr = expprof.sur_bri
   expcorrerr = expprof.sur_bri_err
   expcorrerr = expcorrerr/max(expcorr) ;# alternate normalization
   expcorr = expcorr/max(expcorr)       ;# alternate normalization
   expcorr_ave = mean(expcorr)
   expcorr_dev = stddev(expcorr)
   origsbr = sbr
   sbr = sbr/expcorr
   erinpix = expprof.rin
   eroutpix = expprof.rout
ENDIF

;################################
;################################
;# Surface Brightness Smoothing #
;################################
;################################

;# sort in DESCENDING radial order needed for DEPROJECTION
s = reverse(sort(rout))
sbr = sbr[s]
rin = rin[s]
rout = rout[s]
rmean = rmean[s]
rinmpc = rinmpc[s]
routmpc = routmpc[s]
rmeanmpc = rmeanmpc[s]
sbr_err = sbr_err[s]
sbr = sbr/exposure[0]
sbr_err = sbr_err/exposure[0]

;################
;################
;# Deprojection #
;################
;################

;# calculate sur bri in units of cts/sec
sbrint = sbr * !PI * (rout^2 - rin^2)

;# generate arrays to hold values
cc = fltarr(maxindex+1)
vsum = fltarr(maxindex+1,maxindex+1)

;# outer radius is defined by m = 0
;# for simplicity, create one r vector with maxindex+1 entries
r = [routmpc(0), rinmpc]        ;# units are Mpc
FOR m = 0,maxindex DO BEGIN
    FOR i = 0,maxindex DO BEGIN
        v1 = vv(r(i),r(m+1))
        v2 = vv(r(i+1),r(m+1))
        v3 = vv(r(i),r(m))
        v4 = vv(r(i+1),r(m))
        vsum(i,m)= ((v1-v2)-(v3-v4))
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

;## warn on negative cc values
ord = where(cc LE 0.)
num = n_elements(ord)-1
ord = ord[num]
IF ord EQ -1 THEN BEGIN
    topr = 9.d10
ENDIF ELSE BEGIN
    topr = rmeanmpc[ord]
ENDELSE
use = where((rmeanmpc LT topr) * (rmeanmpc GE 0.))
sbr_err = sbr_err[use]
rin = rin[use]
rout = rout[use]
rmean = rmean[use]
rinmpc = rinmpc[use]
routmpc = routmpc[use]
rmeanmpc = rmeanmpc[use]
maxindex = n_elements(rin)-1
cc = cc[use]

;###############
;###############
;# Monte Carlo #
;###############
;###############

;# set up arrays
sbrint_array = fltarr(nmc,maxindex+1)
cc_array = fltarr(nmc,maxindex+1)
cc_mean = fltarr(maxindex+1)
cc_error = fltarr(maxindex+1)

;# populate that array with gaussian variance. 
;# Multiply that error by idl's native routine RANDOMN
;# with mean of zero and a standard deviation of one.
sbrint_error = sbr_err  * !PI * (rout^2 - rin^2)
FOR imc = 0,nmc-1 DO BEGIN
    sbrint_array(imc,*) = sbrint + sbrint_error * RANDOMN(seed,maxindex+1)
    cc_array(imc,0) = sbrint_array(imc,0) / vsum(0,0)
    FOR m = 1 ,maxindex DO BEGIN
        interv = 0.0
        FOR i = 0, m-1 DO BEGIN
            interv = interv + cc_array(imc,i) * vsum(i,m)
        ENDFOR
        lastv = vsum(m,m)
        cc_array(imc,m) = (sbrint_array(imc,m) - interv)/lastv
    ENDFOR
ENDFOR

;# compute the mean and dispersion
FOR jmc = 0,maxindex DO BEGIN
    cc_mean(jmc) = abs(total(cc_array(*,jmc))/float(nmc))
    cc_error(jmc) = sqrt(total((cc_array(*,jmc)-cc_mean(jmc))^2)/(nmc-1.0))
ENDFOR

;##########################
;##########################
;# Spectral Interpolation #
;##########################
;##########################

cr    = fitfile.cr[where(fitfile.obsid EQ obs)]
rspec = fitfile.rout[where(fitfile.obsid EQ obs)]
nor   = fitfile.norm[where(fitfile.obsid EQ obs)]
tx    = fitfile.tx[where(fitfile.obsid EQ obs)]
txhi  = fitfile.thi[where(fitfile.obsid EQ obs)]
txlo  = fitfile.tlo[where(fitfile.obsid EQ obs)]
k_xspec_spectral = (nor/cr)
k_xspec_interp = interpol(k_xspec_spectral, rspec, rout/60.)

;# fill err array
FOR ss=0,n_elements(tx)-1 DO BEGIN
    IF (tx[ss]-txlo[ss] GT txhi[ss]-tx[ss]) THEN $
      push, tx_err, tx[ss]-txlo[ss] ELSE $
      push, tx_err, txhi[ss]-tx[ss]
ENDFOR

tx_interp = interpol(tx, rspec, rout/60.)
tx_err_interp = interpol(tx_err, rspec, rout/60.)

;################################
;################################
;# Central Temperature Assembly #
;################################
;################################

;# the central error should be that of the last data point
wcentral = where((rout GE 0.0) AND (rout/60. LE min(rspec)),nwcentral)
IF nwcentral LE 0 THEN nwcentral = 1
wouter = where(rout/60. GT min(rspec))
wmintx = where(rspec EQ min(rspec))
xc = findgen(nwcentral)+1.0
xc = xc/xc
terr_central = xc * tx_err(wmintx[0])
t_central = xc * tx(wmintx[0])

;# remember indices are 'flipped' (inner is outer)
;# a vector where the central pts=tx(rmin)
tx_interp_flatmin = [tx_interp(wouter), t_central]

;# assembles the 'correct' tx err vector
tx_err_interp = [tx_err_interp(wouter), terr_central]

;#############################################
;#############################################
;# Electron Density and Entropy Calculations #
;#############################################
;#############################################

;# we can use xspec normalization or the interpolated version:
;# k_xspec or k_xspec_interp. cc must be in counts/sec/Mpc^3
ion = 1.4/1.2
nelec_itpl = sqrt(ion * k_xspec_interp * cc * 4.0 * !PI * da^2. * (1+z)^2 / 3.0856e10)
nelec_mc   = sqrt(ion * k_xspec_interp * cc_mean * 4.0 * !PI * da^2. * (1+z)^2 / 3.0856e10)
nelec_err  = 0.5 * nelec_mc/cc_mean * cc_error

;# this stuff doesn't matter

Mgas   = replicate(0,n_elements(routmpc))
M_err  = Mgas*0.0
p      = Mgas*0.0
p_flat = Mgas*0.0
p_err  = Mgas*0.0

;######################
;######################
;# Entropy Derivation #
;######################
;######################

s      = tx_interp/nelec_itpl^(2./3.)
s_flat = tx_interp_flatmin/nelec_itpl^(2./3.)
s_err  = s * sqrt((tx_err_interp/tx_interp)^2 + 4./9.*(nelec_err/nelec_itpl)^2 )

;##################################
;##################################
;# Fitting, Plotting, and Logging #
;##################################
;##################################

wfit = where((rmeanmpc GE Rminfit) * (rmeanmpc LE Rmaxfit))
IF (n_elements(rmeanmpc[wfit]) LT 3) THEN BEGIN
   print, '## ERROR: Too few entropy bins for fitting'
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
   read, '## INPUT: Enter R limits (in Mpc) as <Rmin>,<Rmax>: ', Rminfit, Rmaxfit
   wfit = where((rmeanmpc GE Rminfit) * (rmeanmpc LE Rmaxfit))
ENDIF

IF (n_elements(rmeanmpc[wfit]) LT 3) THEN BEGIN
   print, '## ERROR: *Still* too few entropy bins for fitting'
   GOTO,ERROR
ENDIF

;# Open a logfile to store fit info
logfile = obs+'_results.log'
GET_LUN, RESLUN
OPENW, RESLUN, logfile
wnam  = strcompress(clustername,/remove_all)
wobs  = strcompress(obs,/remove_all)
wrmin = strcompress(sigfig(Rminfit,2),/remove_all)
wrmax = strcompress(sigfig(Rmaxfit,2),/remove_all)
wann  = strcompress(n_elements(wfit),/remove_all)
printf, RESLUN, format='(A-20,A20,A8,A8,A8,A6,8A15)',$
        '#Cluster','Obsid','Tmode','Rmin','Rmax','Ann','K0',$
        'K0_err','K100','K100_err','Power-law','Plaw_err','Chisq','Prob'

;# fit profile using Markwardt's Levenburg-Marquardt non-linear
;least squares algorithm
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
printf, RESLUN, format='(A-20,A20,A8,A8,A8,A6,7F15.5,E15.5)',$
        wnam,wobs,tmode,wrmin,wrmax,wann,a[0],sigma_entropy[0],a[1],$
        sigma_entropy[1],a[2],sigma_entropy[2],chisq_entropy/dof,prob_est

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
printf, RESLUN, format='(A-20,A20,A8,A8,A8,A6,7F15.5,E15.5)',$
        wnam,wobs,tmode,wrmin,wrmax,wann,a[0],sigma_entropy[0],a[1],$
        sigma_entropy[1],a[2],sigma_entropy[2],chisq_entropy/dof,prob_est

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
printf, RESLUN, format='(A-20,A20,A8,A8,A8,A6,7F15.5,E15.5)',$
        wnam,wobs,tmode,wrmin,wrmax,wann,a[0],sigma_entropy[0],a[1],$
        sigma_entropy[1],a[2],sigma_entropy[2],chisq_entropy/dof,prob_est

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
printf, RESLUN, format='(A-20,A20,A8,A8,A8,A6,7F15.5,E15.5)',$
        wnam,wobs,tmode,wrmin,wrmax,wann,a[0],sigma_entropy[0],a[1],$
        sigma_entropy[1],a[2],sigma_entropy[2],chisq_entropy/dof,prob_est
FREE_LUN, RESLUN

;# Make arrays for plotting radii vs. entropy, pressure, mass
ord      = where((rmeanmpc LE Rmaxfit) AND (rmeanmpc GE Rminfit))
rpl      = rmeanmpc[ord]*1000.
rmeankpc = rmeanmpc*1000.
spl      = s[ord]
serrpl   = s_err[ord]
sflpl    = s_flat[ord]
xmin     = 0.75*min(rpl)
xmax     = 1.25*max(rpl)
ymax     = 1.25*max(spl+serrpl)
IF (min(spl-serrpl) GT min(result_s0) OR min(spl-serrpl) LE 0) THEN $
  ymin = 0.9*min(result_s0) ELSE $
  ymin = 0.9*min(spl-serrpl)

;# create hardcopy
plotfile = obs+'_splot.ps'
set_plot,'PS'
device,filename = plotfile, $
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
items = [name, $
         obs,$
         'Intpl',$
         'Flat',$
         textoidl('Intpl S0 \neq 0'),$
         textoidl('Intpl S0 = 0'),$
         textoidl('Flat S0 \neq 0'),$
         textoidl('Flat S0 = 0')]
parr = [0,0,5,6,0,0,0,0]
larr = [-99,-99,0,0,1,2,3,4]
legend, items, linestyle=larr, psym=parr, box=0, charsize=0.8, /top, /left, /fill
device,/close
set_plot,'X'

;# Dump results to a table
tablefile=obs+'_table.dat'
GET_LUN, LOGLUN
OPENW, LOGLUN, tablefile
printf, LOGLUN, clustername
printf, LOGLUN, 'Cosmology', h0, omega_m, lambda
printf, LOGLUN, 'Number of MC iterations:', nmc
printf, LOGLUN, 'Source FITS files: ', strcompress(filename[0],/remove_all)
printf, LOGLUN, format='(12A15)','#RIN_MPC','ROUT_MPC','N_elec(cm-3)','Sigma_Ne','K(KeV cm2)','K_flat(KeV cm2)','K_err','P','P_flat','P_err','Mgas','Merr'   
outarray = [ [rinmpc], [routmpc], [nelec_itpl], [nelec_err], [s], [s_flat], [s_err], [p], [p_flat], [p_err], [Mgas], [M_err] ]
outarray = transpose(outarray)
printf, LOGLUN, format='(2F15.5,2E15.5,3F15.5,5E15.5)', outarray 
FREE_LUN, LOGLUN

ERROR:
IF (count NE 1) THEN $
   print, '## ERROR: ',filename,' does not exist, exiting.'
IF (notx EQ 'yes') THEN $
   print, '## ERROR: ',obs,' does not have temperature profile, exiting.'
IF yousuck EQ 'yes' THEN  FILE_DELETE, logfile
RETURN
END
