;****************************************************************
;****************************************************************

FUNCTION fit_results_100, x, a
; given x and a output y
; a = [s0, s100, beta]

s0 = a[0]
s100 = a[1]
beta = a[2]

; assumes x is in MPC!

y = s0 + s100*(x/0.100)^beta
RETURN,y
END

;****************************************************************
;****************************************************************

FUNCTION fit_form_100, x, a

; function fit is s(r) = s0 + s100*(r/0.100)^beta
; a = [s0, s100, beta]
; returns [ s(r), ds/ds0, ds/ds100, ds/dbeta]

s0=a(0) & s100=a(1) & beta=a(2)

sr = fit_results_100(x, a)

dsds0 = x/x
dsds100 = (x/0.100)^(beta)
dsdbeta = s100*(x/0.100)^beta * alog(x/0.100)

vector = [ sr, dsds0, dsds100, dsdbeta ]
RETURN,vector
END

;****************************************************************
;****************************************************************

FUNCTION fit_results, x, a
; given x and a output y
; a = [s0, rs, beta]

s0=a(0)
rs=a(1)
beta=a(2)

y = s0 + (x/rs)^beta
RETURN,y
END

;****************************************************************
;****************************************************************

FUNCTION fit_form, x, a

; function fit is s(r) = s0 + (r/rs)^beta
; a = [s0, rs, beta]
; returns [ s(r), ds/ds0, ds/drs, ds/dbeta]


s0=a(0) & rs=a(1) & beta=a(2)

sr = fit_results(x, a)

dsds0 = x/x
dsdrs = -beta * (x/rs)^(beta-1.0)*(x/rs^2.0)
dsdbeta = (x/rs)^beta * alog(x/rs)

vector = [ sr, dsds0, dsdrs, dsdbeta ]
RETURN,vector
END

;****************************************************************
;****************************************************************

FUNCTION d_proper_Mpc,z
; COMPUTE PROPER DISTANCE IN MPC GIVEN Z AND Q0 AND H0
common cosmology, Omega_m, H0, lambda

; in this function q0 defined to be Omega_m / 2.0

q0= Omega_m/2.0

IF (q0 ne 0)*(lambda eq 0.0) THEN BEGIN
    RETURN, 2.998E5*(q0*z+(q0-1.)*(sqrt(1.+2.*q0*z)-1.))/q0^2/H0/(1.+z)
ENDIF

IF (q0 eq 0) THEN BEGIN
    RETURN, 2.998E5*z*(1.+z/2.)/H0/(1.+z)
ENDIF

IF (lambda ne 0.0)*(2.*q0+lambda eq 1.0) THEN BEGIN
    nz = n_elements(z)
    IF nz gt 1 THEN BEGIN
        dp = fltarr(nz)
        FOR i = 0,nz-1 do begin
            zint = findgen(100)/99. * z(i)
            dz_int = 1.0/sqrt((1.+zint)^2.*(1.+2.0*q0*zint) - zint*(2.+zint)*lambda)
            dp(i) = 2.998E5/H0 *int_tabulated(zint,dz_int)
        ENDFOR
        RETURN, dp

    ENDIF ELSE BEGIN
        zint = findgen(100)/99. * z
        dz_int = 1.0/sqrt((1.+zint)^2.*(1.+2.0*q0*zint) - zint*(2.+zint)*lambda)
        dp = 2.998E5/H0 *int_tabulated(zint,dz_int)
        RETURN, dp
    ENDELSE
ENDIF
RETURN,'ERROR in proper distance, non-flat Lambda universe!'

END

;****************************************************************
;****************************************************************

FUNCTION dl, z
;COMPUTE LUMINOSITY DISTANCE GIVEN Z
common cosmology, Omega_m, H0, lambda
dl = (1+z)*d_proper_Mpc(z)
RETURN,dl
END

;****************************************************************
;****************************************************************

FUNCTION da,z
;COMPUTE ANGULAR DISTANCE IN MPC GIVEN Z
common cosmology, Omega_m, H0, lambda
da = d_proper_Mpc(z)/(1+z)
RETURN,da
END

;****************************************************************
;****************************************************************

FUNCTION dangarcmin,z
;COMPUTE ANGULAR DISTANCE IN MPC/ARCMIN GIVEN Z
common cosmology, q0, H0, lambda
dd = 2.908882E-4 * d_proper_Mpc(z)/(1+z)
RETURN,dd
END

;****************************************************************
;****************************************************************

FUNCTION vv, rii, rjj
IF rii le rjj then RETURN,0.0
pi = 3.14159265359
vvv = 4.*pi/3. * (rii^2 - rjj^2)^(1.5)
RETURN,vvv
END

;****************************************************************
;****************************************************************

function sigfig, inarr, nfigs
outarr = string(inarr)
sgn = inarr lt 0
numbers = double(inarr)
for ii = 0, n_elements(inarr)-1 do begin 
    if numbers[ii] eq 0 then begin
        firstfig = 0
        round = 0
    endif else begin
        firstfig = floor(alog10(abs(numbers[ii])))
        round = round(numbers[ii]*1d1^(nfigs-firstfig-1), /l64)*$
          1d1^(firstfig-nfigs+1)
    endelse
    i = (firstfig-nfigs) ge 0 ? 'I' : 'F'
    ndec = strcompress((-(firstfig-nfigs)-1 > 0), /rem)
    fstr = '('+i+'40.'+ndec+')'
    string = strcompress(string(round, format = fstr), /rem)
    outarr[ii] = string
endfor
return, outarr
end

;****************************************************************
;****************************************************************



pro kfitter, filename, tempfitfile, clustername, tkeV, nmc, obs

;****************************************************************
;****************************************************************
;
; NAME:
;     kfitter.pro
;
; PURPOSE:
;     This program will deproject a possibly smoothed surface
;     brightness profile, convert it to an interpolated electron
;     density using pre-measured annular spectra, then calculate
;     K (entropy), and fit that to two power laws.
;     
; EXPLANATION:
;     Monte-Carlo the surface brightness profiles in order to obtain
;     an estimate of the 1-sigma errors on the density profile with
;     deprojection (no systematics obviously). This program is
;     re-organized so that the deprojection happens many many times,
;     but interpolation only happens once.
;
;     This script assumes the following directory structure:
;     all pertinent radial profiles are in a dir:
;     ../radprofs/<pixel size>pixs/
;
;     New files will be placed in the directory from which
;     this pro is being run.
;
; CALLING SEQUENCE:
;     IDL> kfitter, "blah.fits", "abell_blah", 5.5, 1000
;     or, use the calling pro run_kfit.pro, i.e.:
;     IDL> .run run_kfit
;
; INPUTS:
;     <reference list> = file containing information about each cluster
;     in the case of X and Y being the centroid position in pixels and Rmax in pixels
;     #Name                          ObsID        X        Y   Rmax     MinCts         z  Nh20     Tx     Fe  Lbol ChipID
;     ABELL_0644                      2211   3908.5   4332.5  243.9       5000    0.0704  6.41   8.64   0.35 45.00     s3
;     ABELL_1651                      4185   4222.5   4034.5  132.3       5000    0.0844  1.88   5.97   0.30 45.00     i2
;
; OUTPUTS:
;     Log of the two power law fit results: <obsid>_results.log
;     Plot of the entropy profile:          <obsid>_splot.ps
;     ASCII table of the power law fits:    <obsid>_table.dat
;
; MODIFICATION HISTORY:
;     Modified fit so it reports s_100 rather than R_s
;
;****************************************************************
;****************************************************************

;################
;# Main Program #
;################
common cosmology, Omega_m, H0, lambda
pi = 3.14159265359

; makes all plots look nice :)
!fancy = 4

; take alternative assumption regarding geometry
Omega_m = 0.3
lambda  = 0.7
H0      = 70.

; check for file existance
check = findfile(filename,count=count)
check2 = findfile(tempfitfile,count=count2)
IF (count EQ 1 AND count2 EQ 1) THEN GOTO,MAIN ELSE GOTO,ERROR

MAIN:
; read in sur bri profile assuming units CTS/PIXEL**2
print, "Opening ",filename

; reads all the columns into a structure named fits
fits = mrdfits(filename,1,hdr)
sbrpix = fits.sur_bri
rinpix = fits.rin
routpix = fits.rout
sbr_errpix = fits.sur_bri_err
maxindex = n_elements(rinpix)-1
print,'Read in ',strcompress(maxindex+1),' annuli.'

; format the cluster names for plotting
cname = strcompress(clustername,/remove_all)
;cname = str_replace(cname,'ABELL','Abell')
;cname = str_replace(cname,'_',' ')
;cname = str_replace(cname,'CENTAURUS','Centaurus')
;cname = str_replace(cname,'COMA','Coma')
name  = cname

; read in the temperature fits
restore,"../scripts/xspecfittemplate_ext.sav"
fitfile = read_ascii(tempfitfile,template=xspecfittemplate_ext)

; get the redshift for this cluster based on obsid
junk = fitfile.z[where(fitfile.obsid EQ obs)]
z = junk[0]

; convert radii units from pixels to arcseconds
pixtoarcsec = 0.492             ; ACIS scale is 0.492"/pixel
rin = pixtoarcsec * rinpix
rout = pixtoarcsec * routpix
sbr = sbrpix / (pixtoarcsec)^2
sbr_err = sbr_errpix / (pixtoarcsec)^2

;#######################
;# Exposure Correction #
;#######################
; the exposure map is input to dmextract which then calculates
; a column labeled sur_flux which is in units of cts/cm^2/sec/pixel^2,
; use this column to properly calculate a correction to the sur_bri
; convert to cts/cm^2/sec/arcsec^2 to match sbr
;surflux = fits.sur_flux / (pixtoarcsec)^2
;surflux_err = fits.sur_flux_err / (pixtoarcsec)^2

; calculate the exposure correction
;expcorr = sbr/surflux
;expcorr = expcorr/expcorr[0]
;expcorr_ave = total(expcorr)/n_elements(expcorr)
;print,"Average exposure correction is: ",expcorr_ave
;print,"St.dev correction is: ", sqrt( total((expcorr_ave-expcorr)^2)/(n_elements(expcorr)-1))
;origsbr = sbr
;sbr = sbr/expcorr

; plot the exposure correction, we expect the deviation from one to be small
;plot, rinpix, expcorr, $
;  title = filename, $
;  xtitle = "R_in (pixels)", $
;  ytitle = "normalized exp. corr. (sbr/sfl/exp0)", $
;  psym = 0, $
;  linestyle = 0

; make a hardcopy of the expcorr plot?
;yesno = ""
;read, "Want a hardcopy (postscript) plot of this? (yes=y)", yesno
;IF (yesno eq "y") THEN BEGIN
;    plotfile = obs+'_expcorr.ps'
;    set_plot, 'PS'
;    device, filename = plotfile
;    plot, rinpix, expcorr, $
;     linestyle = 0, $
;     psym = 1, $
;     title = filename, $
;     xtitle = "R_in (pixels)", $
;     ytitle = "normalized exp. corr. (sbr/sfl/exp0)"

; draw the legend
;    items = [name, obs, expcorr_ave]
;    legend, items, box=0, charsize=0.8, /bottom, /left
;    device, /close
;    set_plot, 'x'
;ENDIF

;****************************************************************
;****************************************************************
;; Old exposure correction:
;; read in exposure correction vectors and normalize to the central bin, 
;; then divide the sbr vector by the normalized exposure correction,
;; report the average and dispersion of the surface brightness correction.
;; expcorr = fits.sur_bri
;; expcorr = expcorr/expcorr(0) ; normalization
;; expcorr_ave = total(expcorr)/n_elements(expcorr)
;; print, "Average exposure correction is: ",expcorr_ave
;; print, "St.dev correction is: ", sqrt( total((expcorr_ave-expcorr)^2)/(n_elements(expcorr)-1))
;; sbr = sbr/expcorr
;****************************************************************
;****************************************************************

; plot original sbr vs. expcorr sbr
;plot, rin, origsbr, $
;  title = filename, $
;  xtitle = "R_in (arcseconds)", $
;  ytitle = "SBR and ExpCorr SBR (cts/arcsec!E2!N)", $
;  psym = 0, $
;  linestyle = 0
;oplot, rin, sbr
;yesno = ""
;read, "Press enter to continue", yesno

;################################
;# Surface Brightness Smoothing #
;################################
; sort in DESCENDING radial order for DEPROJECTION ORDER
s = reverse(sort(rout))
sbr = sbr(s)
rin = rin(s)
rout = rout(s)
sbr_err = sbr_err(s)

; plot the surface brightness profile and associated error as
; a function of radius in arcseconds
xmax = max(rin)+0.1*max(rin)
xmin = 0.492
ymax = max(sbr)+0.1*max(sbr)
ymin = min(sbr)-0.1*min(sbr)
plot, rin, sbr, $
  title = filename, $
  xtitle = "Rin (arcseconds)", $
  ytitle = "Exp. Corr. SB (counts/arcsec^2)", $
  psym = 10, $
  /xsty, $
  /ysty, $
  /XLOG, $
  /YLOG, $
  xran=[xmin,xmax], $
  yran=[ymin,ymax], $
  linestyle = 0
oploterr, rin, sbr, sbr_err

; create a smoothed version of the sbr profile using
; idl's native smoother SMOOTH. SMOOTH performs an equally
; weighted smoothing using a square neighborhood of an
; arbitrary width: smooth(<function>,<width>). Iterate the smoothing
; to see which length works "best".
origsbr = sbr
yesno = ""
read, "Smooth this? (yes=y): ", yesno
IF (yesno eq "y") THEN BEGIN
    REPEAT BEGIN
        read, "Input smoothing length (integer): ",nsm
        sbr = smooth(origsbr,nsm)
        plot, rin, origsbr, $
          title = filename, $
          xtitle = "Rin (arcseconds)", $
          ytitle = "Exp. Corr. SB (counts/arcsec^2)", $
          psym = 10, $
          /xsty, $
          /ysty, $
          /XLOG, $
          /YLOG, $
          xran=[xmin,xmax], $
          yran=[ymin,ymax], $
          linestyle = 0
        oploterr, rin, sbr, sbr_err
        oplot, rin, sbr
        read, "Re-try smoothing (no=n): ", yesno
    ENDREP UNTIL (yesno eq "n")
ENDIF

;###################
;# Time Correction #
;###################
; the following step is only necessary if the 
; input surface brightness profile is *NOT* normalized by time
; retrieve the exposure time from the header
print, '## WARNING: TIME NORMALIZATION APPLIED.'
print, '## Input surface brightness units assumed to be CTS/ARCSEC**2'
fitshead = headfits(filename,ext=1)
texp = sxpar(fitshead,'EXPOSURE')
sbr = sbr/texp
sbr_err = sbr_err/texp
print, 'Exposure time: '+strcompress(texp,/remove_all)+' seconds'

;################
;# Deprojection #
;################
; calculate the radii in Mpc
rinmpc = dangarcmin(z)* rin / 60.
routmpc = dangarcmin(z) * rout / 60.

; compute the counts/second/cm^3 (CC) assuming spherical symmetry and that
; the emissivity in an annular bin is constant.
; This calculation also assumes that any emission outside the
; outermost bin is negligible.

; calculate sur bri in units of cts/sec
sbrint = sbr * pi * (rout^2 - rin^2)

; generate arrays
cc = fltarr(maxindex+1)
vsum = fltarr(maxindex+1,maxindex+1)

; outer radius is defined by m = 0
; for simplicity, create one r vector with maxindex+1 entries
r = [routmpc(0), rinmpc]        ; units are Mpc
FOR m = 0,maxindex DO BEGIN
    FOR i = 0,maxindex DO BEGIN
        v1 = vv(r(i),r(m+1))
        v2 = vv(r(i+1),r(m+1))
        v3 = vv(r(i),r(m))
        v4 = vv(r(i+1),r(m))
        vsum(i,m)= ( (v1-v2) - (v3-v4) )
    ENDFOR
ENDFOR

cc(0) = sbrint(0) / vsum(0,0)
FOR m = 1,maxindex DO BEGIN
    interv = 0.0
    FOR i = 0,m-1 DO BEGIN
        interv = interv + cc(i) * vsum(i,m)
    ENDFOR
    lastv = vsum(m,m)
    cc(m) = (sbrint(m) - interv) / lastv
ENDFOR

;###############
;# Monte Carlo #
;###############
; set up an array where each row is a different version of sbrint
sbrint_array = fltarr(nmc,maxindex+1)
cc_array = fltarr(nmc,maxindex+1)
cc_mean = fltarr(maxindex+1)
cc_error = fltarr(maxindex+1)

; populate that array with gaussian variance. 
; for now take the errors from dmextract.
; Multiply that error by idl's native routine RANDOMN.
; RANDOMN function returns one or more normally-distributed,
; floating-point, pseudo-random numbers with a
; mean of zero and a standard deviation of one.
; RANDOMN uses the Box-Muller method for generating
; normally-distributed (Gaussian) random numbers.
sbrint_error = sbr_err  * !pi * (rout^2 - rin^2)
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

; compute the mean and dispersion of the profiles now they are all computed
FOR jmc = 0,maxindex do begin
        cc_mean(jmc) = total(cc_array(*,jmc))/float(nmc)
        cc_error(jmc) = sqrt( total( (cc_array(*,jmc)-cc_mean(jmc))^2/(nmc-1.0) ))
ENDFOR

;##########################
;# Spectral Interpolation #
;##########################
; the k_spec = norm_xspec/count_rate_xspec bandpass variable can be interpolated.
; norm_xspec is the normalization from the spectral fit.
; count_rate_xspec is the number of counts per second in the spectrum in the
; same energy bandpass as the surface brightness profile.
; k_xspec in principle could be a vector if there is a temperature gradient
; across the profile.
; can also interpolate k_xspec to go beyond the average 
; emission weighted spectrum to a deprojected emission weighted spectrum.
; interpol takes an array of x and y values along with interpolation
; points (interpolates) and constructs data points between each.
cr = fitfile.cr[where(fitfile.obsid EQ obs)]
rspec = fitfile.rout[where(fitfile.obsid EQ obs)]
nor = fitfile.norm[where(fitfile.obsid EQ obs)]
temp = fitfile.tx[where(fitfile.obsid EQ obs)]
temp_hi = fitfile.thi[where(fitfile.obsid EQ obs)]
temp_lo = fitfile.tlo[where(fitfile.obsid EQ obs)]
temp_err = (temp_hi-temp_lo)
k_xspec_spectral = (nor/cr)
k_xspec_interp = interpol(k_xspec_spectral, rspec, rout/60.)
; smoothing the temp profile eliminates possible artifacts from
; deprojection in Xspec; this is not needed with 
; projected temp; change 1 to higher value if necessary.
temp_interp = interpol(smooth(temp,1), rspec, rout/60.)
temp_err_interp = interpol(temp_err, rspec,rout/60.)

;################################
;# Central Temperature Assembly #
;################################
; the central error should be that of the last data point
wcentral = where((rout GE 0.0)*(rout/60. le min(rspec)),nwcentral)
wouter = where(rout/60. GT min(rspec))
wmintemp = where(rspec EQ min(rspec))
xc = findgen(nwcentral)+1.0
xc = xc/xc
terr_central = xc * temp_err(wmintemp[0])
t_central = xc * temp(wmintemp[0])

; remember indices are "flipped" (inner is outer)
temp_err_interp = [ temp_err_interp(wouter), terr_central] ; assembles the "correct" temp err vector
temp_interp_flatmin = [ temp_interp(wouter), t_central ] ; a vector where the central pts=temp(rmin)

;#############################################
;# Electron Density and Entropy Calculations #
;#############################################
; we can use xspec normalization or the interpolated version:
; k_xspec or k_xspec_interp. cc should be in counts/sec/Mpc^3
ion = 1.4/1.2                   ; fully ionized plasma
nelec_interp = sqrt( ion * k_xspec_interp * cc * 4.0 * pi * da(z)^2 * (1+z)^2 / 3.0856e10)
nelec_mean_nmc = sqrt( ion * k_xspec_interp * cc_mean * 4.0 * pi * da(z)^2 * (1+z)^2 / 3.0856e10)
nelec_error_nmc = 0.5*nelec_mean_nmc/cc_mean * cc_error

; cc converges to cc_mean reliably
; to check
eps_0 = 1.e-14/k_xspec_interp(maxindex) 
beta = 0.8
theta_c = 30.0 * (pi/180./60./60.) ; arcseconds to radians
a1 = eps_0 * sqrt(pi)* gamma(3.*beta-0.5)*theta_c/(4.*pi*(1.+z)^3*gamma(3.*beta))
sxo = sbr(maxindex) * (180.*60.*60./pi)^2 ; cps/sr
ne0 = sqrt(sxo/a1/da(z)/3.0856e24)
print,'General estimate of central electron density per cubic cm, Ne_0 = ', ne0
 
; assume hydrostatic equilibrium:
; M(<r)=kTr/(mu m G) * dlog ne/dlog r
; assume log Te/dlog r is small.
; running differential:
; delta log ne / delta log r = log (ni/ni+1)/ log(ri/ri+1)
; estimate a running d log ne/ d log r slope piecewise.
; smooth ne(r) then perhaps smooth the result
nelec_sm = smooth(nelec_interp,3)
dlognedlogr = fltarr(maxindex+1)
FOR i=0,maxindex-1 DO BEGIN
    dlognedlogr(i) = alog10(nelec_sm(i+1)/nelec_sm(i))/alog10(rout(i+1)/rout(i))
ENDFOR
dlognedlogr(maxindex) = dlognedlogr(maxindex-1)

; this differential function is very jagged - smooth it heavily 
plot, rout, dlognedlogr, $
  xran=[min(rout),max(rout)], $
  yran=[min(dlognedlogr),max(dlognedlogr)], $
  /xsty, $
  /ysty, $
  title = filename, $
  xtitle='Rout [arcsec]', $
  ytitle='dlognedlogr'
odnedr = dlognedlogr
yesno = "y"
print, "This diff. func requires HEAVY smoothing."
IF (yesno eq "y") THEN BEGIN
    REPEAT BEGIN
        read, "Input smoothing length (integer): ",nsm
        dlognedlogr = smooth(odnedr,nsm)
        plot, rout, dlognedlogr, $
          xran=[min(rout),max(rout)], $
          yran=[min(dlognedlogr),max(dlognedlogr)], $
          /xsty, $
          /ysty, $
          title = filename, $
          xtitle='Rout [arcsec]', $
          ytitle='dlognedlogr', $
          psym=0, $
          linestyle=2
        oplot, rout, odnedr
        read, "Continue smoothing (no=n): ", yesno
    ENDREP UNTIL (yesno eq "n")
ENDIF
M_lt_R = -3.7d13* temp_interp * routmpc * dlognedlogr
M_err = temp_err_interp/temp_interp * M_lt_R *1.1/1.5 ; 2.0 would probably be better (the temp errors
                                                      ; are the full 90% difference bet. thi and tlo)
                                                      ; but interpolation & deproj have errors too.
                                                      ; slope is probably only ok to 10%
print, "Mass_gas(",strcompress(max(routmpc),/remove_all),"M= ,"strcompress(total(M_lt_R),/remove_all),"10**13 M_solar"

; first 5-6 points look like garbage -- in the noise on the
; outer part of the profile where we don't measure the local
; power law index very well.
r200_kpc = 790. * sqrt(tkev) * (h0/70.)^(-1)
print, "R200 = ",r200_kpc," kpc h70^-1"

; make entropy profiles and fit to a couple profiles
s = temp_interp/nelec_interp^(2./3.)
s_flat = temp_interp_flatmin/nelec_interp^(2./3.) ; temperature profile assumed to flatten inside inner most aperture
s_err = s * sqrt((temp_err_interp/temp_interp)^2 + 4./9.*(nelec_error_nmc/nelec_interp)^2 ) ; electron density errors from m.c.

; make pressure profiles
p = temp_interp*nelec_interp
p_flat = temp_interp_flatmin*nelec_interp
p_err = p * sqrt((temp_err_interp/temp_interp)^2 + (nelec_error_nmc/nelec_interp)^2 )

;##################################
;# Fitting, Plotting, and Logging #
;##################################
rmean = (rinmpc+routmpc)/2.0
plot, rmean, s, $
  title = filename, $
  ytitle = "Entropy", $
  xtitle = "R_mean (Mpc)", $
  psym = 0
oploterror, rmean, s, s_err

savefits = "y"
logfile = ""
read, "Enter R limits (in Mpc) as <Rmin>,<Rmax>: ", Rminfit, Rmaxfit
read, "Want to save fits results? (yes=y): ", savefits
wfit = where((rmean GE Rminfit) * (rmean le Rmaxfit))
IF (savefits eq 'y') THEN BEGIN
    close, 2
    logfile = obs+'_results.log'
    openw,  2, logfile
    wnam = strcompress(clustername,/remove_all)
    wobs = strcompress(obs,/remove_all)
    wrmin = strcompress(sigfig(Rminfit,2),/remove_all)
    wrmax = strcompress(sigfig(Rmaxfit,2),/remove_all)
    wann = strcompress(n_elements(wfit),/remove_all)
    printf, 2, format='(A-,A22,A8,A8,A8,A6,8A15)',"#Cluster","Obsid","Tmode","Rmin","Rmax","Ann","K0 (keV mc^2)","K0_err","S100","K100_err","Power-law","Plaw_err","Chisq","Prob"
ENDIF

; fit profile using Levenburg-Marquardt non-linear least squares algorithm
; set the temperature mode
tmode = "itpl"
a = [10, 150.0, 1.5]
result = lmfit(rmean(wfit), s(wfit), a, measure_errors=s_err(wfit), chisq=chisq_entropy, sigma=sigma_entropy, function_name="fit_form_100")
prob_est = 1.0-IGAMMA((0.5*n_elements(rmean(wfit))-3), 0.5*chisq_entropy)
IF (savefits EQ 'y') THEN $
  printf, 2, format='(A-,A20,A8,A8,A8,A6,8F15.3)',wnam,wobs,tmode,wrmin,wrmax,wann,a[0],sigma_entropy[0],a[1],sigma_entropy[1],a[2],sigma_entropy[2],chisq_entropy,prob_est

; fit profile using Levenburg-Marquardt non-linear least squares
; algorithm, but fix a=0.0
a = [0.0, a(1), a(2)]
fita = [0, 1, 1]
result_s0 = lmfit( rmean(wfit), s(wfit), a, measure_errors=s_err(wfit), fita=fita, chisq=chisq_entropy, sigma=sigma_entropy, function_name="fit_form_100")
prob_est = 1.0-IGAMMA((0.5*n_elements(rmean(wfit))-2), 0.5*chisq_entropy)
IF (savefits EQ 'y') THEN $
  printf, 2, format='(A-,A20,A8,A8,A8,A6,8F15.3)',wnam,wobs,tmode,wrmin,wrmax,wann,a[0],sigma_entropy[0],a[1],sigma_entropy[1],a[2],sigma_entropy[2],chisq_entropy,prob_est

; fit profile using Levenburg-Marquardt non-linear least squares
; algorithm, but use the flat central temperature value
tmode = "flat"
a = [10, 150.0, 1.5]
result_flat = lmfit( rmean(wfit), s_flat(wfit), a, measure_errors=s_err(wfit), chisq=chisq_entropy,sigma=sigma_entropy, function_name="fit_form_100")
prob_est = 1.0-IGAMMA((0.5*n_elements(rmean(wfit))-3), 0.5*chisq_entropy)
IF (savefits EQ 'y') THEN $
  printf, 2, format='(A-,A20,A8,A8,A8,A6,8F15.3)',wnam,wobs,tmode,wrmin,wrmax,wann,a[0],sigma_entropy[0],a[1],sigma_entropy[1],a[2],sigma_entropy[2],chisq_entropy,prob_est

; fit profile using Levenburg-Marquardt non-linear least squares
; algorithm, but use the flat central temperature value and fix a=0.0
a = [0.0, a(1), a(2)]
fita = [0, 1, 1]
result_s0_flat = lmfit( rmean(wfit), s_flat(wfit), a, measure_errors=s_err(wfit), fita=fita, chisq=chisq_entropy,sigma=sigma_entropy, function_name="fit_form_100")
prob_est = 1.0-IGAMMA((0.5*n_elements(rmean(wfit))-2), 0.5*chisq_entropy)
IF (savefits EQ 'y') THEN BEGIN
  printf, 2, format='(A-,A20,A8,A8,A8,A6,8F15.3)',wnam,wobs,tmode,wrmin,wrmax,wann,a[0],sigma_entropy[0],a[1],sigma_entropy[1],a[2],sigma_entropy[2],chisq_entropy,prob_est
  close, 2
ENDIF

; make arrays for plotting radii vs. entropy, pressure, mass
FOR i=0,n_elements(rmean)-1 DO BEGIN
    IF (rmean[i] LE Rmaxfit AND rmean[i] GE Rminfit) THEN $
      IF (n_elements(rpl) EQ 0) THEN BEGIN
        rpl = rmean[i]
        spl = s[i]
        serrpl = s_err[i]
        sflpl = s_flat[i]
    ENDIF ELSE BEGIN
        rpl = [rpl, rmean[i]]
        spl = [spl, s[i]]
        serrpl = [serrpl, s_err[i]]
        sflpl = [sflpl, s_flat[i]]
    ENDELSE
ENDFOR

; define the min and max ranges to plot
rpl = rpl*1000.
rmeankpc = rmean*1000.
xmin = min(rpl)-0.25*min(rpl)
xmax = max(rpl)+0.25*max(rpl)
IF (min(spl-serrpl) GT min(result_s0) OR min(spl-serrpl) LE 0) THEN ymin=min(result_s0)-0.1*min(result_s0) ELSE ymin=min(spl-serrpl)-0.1*min(spl-serrpl)
ymax = max(spl+serrpl)+0.25*max(spl+serrpl)

; plot the profiles
plotsym, 4, 0.8, /fill          ; triangle
plot, rpl, spl, $
  /xlog,$
  /ylog, $
  /xsty, $
  /ysty, $
  xtitle = 'R (kpc)', $
  ytitle = 'K (keV cm!u2!n)', $
  title = name, $
  psym = 8, $
  charsize = 0.8, $
  xrange = [xmin,xmax], $
  yrange = [ymin,ymax]
oploterror, rpl, spl, serrpl, psym=8
plotsym, 8, 0.8, /fill          ; squares
oplot, rpl, sflpl, psym=8
oploterror, rpl, sflpl, serrpl, psym=8
oplot, rmeankpc(wfit), result, linestyle=1, psym=0
oplot, rmeankpc(wfit), result_s0, linestyle=2, psym=0
oplot, rmeankpc(wfit), result_flat, linestyle=3, psym=0
oplot, rmeankpc(wfit), result_s0_flat, linestyle=4, psym=0

; draw the legend
items = [name, obs,"Intpl","Flat","Intpl S0!=0","Intpl S0=0","Flat S0!=0","Flat S0=0"]
parr = [0,0,5,6,0,0,0,0]
larr = [-99,-99,0,0,1,2,3,4]
legend, items, linestyle=larr, psym=parr, box=0, charsize=0.8, /top, /left, /fill

; create hardcopy
yesno="y"
read,"Want a hardcopy (postscript) plot of this? (yes=y)", yesno
IF (yesno eq "y") THEN BEGIN
    plotfile = obs+'_splot.ps'
    set_plot,'PS'
    device,filename = plotfile
    plotsym, 4, 0.8, /fill      ; triangle
    plot, rpl, spl, $
      /xlog,$
      /ylog, $
      /xsty, $
      /ysty, $
      xtitle = 'R (kpc)', $
      ytitle = 'K (keV cm!u2!n)', $
      title = name, $
      psym = 8, $
      charsize = 0.8, $
      xrange = [xmin,xmax], $
      yrange = [ymin,ymax]
    oploterror, rpl, spl, serrpl, psym=8
    plotsym, 8, 0.8, /fill      ; squares
    oplot, rpl, sflpl, psym=8
    oploterror, rpl, sflpl, serrpl, psym=8
    oplot, rmeankpc(wfit), result, linestyle=1, psym=0
    oplot, rmeankpc(wfit), result_s0, linestyle=2, psym=0
    oplot, rmeankpc(wfit), result_flat, linestyle=3, psym=0
    oplot, rmeankpc(wfit), result_s0_flat, linestyle=4, psym=0

    ; draw the legend
    items = [name, obs,"Intpl","Flat","Intpl S0!=0","Intpl S0=0","Flat S0!=0","Flat S0=0"]
    parr = [0,0,5,6,0,0,0,0]
    larr = [-99,-99,0,0,1,2,3,4]
    legend, items, linestyle=larr, psym=parr, box=0, charsize=0.8, /top, /left, /fill

    device,/close
    set_plot,'x'                ; return to default plot output
ENDIF       

yesno="y"
read,"Want an ASCII table? (yes=y)", yesno
IF (yesno eq "y") THEN BEGIN
    tablefile=obs+'_table.dat'
    close,1
    openw,1,tablefile
    printf,1,clustername
    printf,1,"Cosmology", h0, omega_m, lambda
    printf,1,"Number of MC iterations:", nmc
    printf,1,"Source FITS files: ", filename
    printf,1,"RIN_MPC     ROUT_MPC      N_elec (cm-3)  Sigma_Ne   K (KeV cm2)  K_flat (KeV cm2)  K_err"   
    outarray = [ [rinmpc], [routmpc], [nelec_interp], [nelec_error_nmc], [s], [s_flat], [s_err] ]
    outarray = transpose(outarray)
    printf,1,format='(7F)',outarray 
    close,1
ENDIF

ERROR:
IF (count NE 1) THEN print, "## ERROR: ",filename," does not exist, exiting."
IF (count2 NE 1) THEN print, "## ERROR: ",tempfitfile," does not exist, exiting."

RETURN

END
