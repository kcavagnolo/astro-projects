; I'm going to attempt to monte-carlo the sb profiles 
; in order to obtain an estimate of the 1-sigma errors
; on the density profile with deprojection (no systematics
; obviously). So this program is re-organized so that the
; deprojection happens many many times, but interpolation only
; happens once.
;****************************************************************

function d_proper_Mpc,z
;COMPUTE PROPER DISTANCE IN MPC GIVEN Z AND Q0 AND H0
common cosmology, Omega_m, H0, lambda

;in this function q0 defined to be Omega_m / 2.0

q0= Omega_m/2.0

if (q0 ne 0)*(lambda eq 0.0) then begin
    return, 2.998E5*(q0*z+(q0-1.)*(sqrt(1.+2.*q0*z)-1.))/q0^2/H0/(1.+z)
endif

if (q0 eq 0) then begin
    return, 2.998E5*z*(1.+z/2.)/H0/(1.+z)
endif

if (lambda ne 0.0)*(2.*q0+lambda eq 1.0) then begin
    nz = n_elements(z)

    if nz gt 1 then begin
        dp = fltarr(nz)

        for i = 0,nz-1 do begin
            zint = findgen(100)/99. * z(i)
            dz_int = 1.0/sqrt((1.+zint)^2.*(1.+2.0*q0*zint) - zint*(2.+zint)*lambda)
            dp(i) = 2.998E5/H0 *int_tabulated(zint,dz_int)
        endfor

        return, dp

    endif else begin
        zint = findgen(100)/99. * z
        dz_int = 1.0/sqrt((1.+zint)^2.*(1.+2.0*q0*zint) - zint*(2.+zint)*lambda)
        dp = 2.998E5/H0 *int_tabulated(zint,dz_int)
        return, dp
    endelse
endif
return,'ERROR in proper distance, non-flat Lambda universe!'

END

;****************************************************************

function dl, z
;COMPUTE LUMINOSITY DISTANCE GIVEN Z
common cosmology, Omega_m, H0, lambda
dl = (1+z)*d_proper_Mpc(z)
return,dl
END

;****************************************************************

function da,z
;COMPUTE ANGULAR DISTANCE IN MPC GIVEN Z
common cosmology, Omega_m, H0, lambda
da = d_proper_Mpc(z)/(1+z)
return,da
END

;****************************************************************

function dangarcmin,z
;COMPUTE ANGULAR DISTANCE IN MPC/ARCMIN GIVEN Z
common cosmology, q0, H0, lambda
dd = 2.908882E-4 * d_proper_Mpc(z)/(1+z)
return,dd
END

;****************************************************************

function vv, rii, rjj
if rii le rjj then return,0.0
pi = 3.14159265359
vvv = 4.*pi/3. * (rii^2 - rjj^2)^(1.5)
return,vvv
END

;****************************************************************

pro rad_ent, dat1, dat2, nmc
; rad_ent, '1tpr_nhfrozen_fefree.fits', 'reference_all.fits', 1000

; restore the fit template and read some variables
fitfile = mrdfits(dat1, 1)
ref = mrdfits(dat2, 1)

; this program will deproj a possibly smoothed sbr profile
; convert it to nelec_interp using pre-measured annular spectra
; then calculate S entropy and fit that to 2 power laws
; right now it doesn't return anything, but that could change
; call it in IDL> sb_fits_mc, "blah.fits", "abell_blah", 5.5, 1000
; 1000 monte-carlo simulations

common cosmology, Omega_m, H0, lambda
pi = 3.14159265359

; take alternative assumption regarding geometry
Omega_m = 0.3
lambda = 0.7
H0 = 70.

cname = strcompress(ref.cluster,/remove_all)
cname = str_replace(cname,'ABELL','Abell')
cname = str_replace(cname,'CYGNUS_A','Cygnus A')
cname = str_replace(cname,'_',' ')
cname = str_replace(cname,'OPHIUCHUS','Ophiuchus')

; check for existence of FITS file first
IF (FILE_TEST('ent_mc.fits') EQ 1) THEN FILE_DELETE,'ent_mc.fits'

; read in sb profiles, ASSUMES units CTS/PIXEL
; inner outer radii, units PIXEL (same as in sb profile)
; FITS file produced by dmextract command
; define the obsid
FOR i=0,n_elements(ref.obsid)-1 DO BEGIN
    clustername = strcompress(ref.cluster[i], /remove_all)
    name = cname[i]
    obs = strcompress(ref.obsid[i], /remove_all)
    chip = strcompress(ref.chip[i], /remove_all)
; change the chipid from eg: 'i0' to 'i0123'
    a = 'i'
    IF (STRCMP(chip,a,1) EQ 1 ) THEN chip = 'i0123'
    tkev = strcompress(ref.tx[i], /remove_all)
    z = strcompress(ref.z[i], /remove_all)

; define the filename for the radial profile
    filename = '../acis/'+obs+'/reprocessed/'+clustername+'_'+obs+'_'+chip+'_radprof_5pix_norm.fits'
    print, "Opening ", filename

; assuming the table is in extension 1
; reads all the columns into a structure named fits
    fits = mrdfits(filename,1)
    sbr = fits.sur_bri
    rin = transpose(fits.r(0,*))
    rout = transpose(fits.r(1,*))
    sbr_err = transpose(fits.sur_bri_err)
    maxindex = n_elements(rin)-1
    print,'Read in ',maxindex+1,' annuli.'

; convert radii units from pixels to arcseconds
    pixtoarcsec = 0.492         ; ACIS scale is 0.492"/pixel
    rin = pixtoarcsec * rin
    rout = pixtoarcsec * rout
    sbr = sbr / (0.492)^2
    sbr_err = sbr_err / (0.492)^2 ; converted from per pixel to per square arcsecond

; sort in DESCENDING radial order for DEPROJECTION ORDER
    s = reverse(sort(rout))
    sbr = sbr(s)
    rin = rin(s)
    rout = rout(s)
    sbr_err = sbr_err(s)

; convert radii to Mpc from arcseconds -- note that the FITS file
; units may be PIXELS instead of ARCSECONDS
    print,'Radii converted from pixels to arcseconds'

; the following step is only necessary if the 
; input surface brightness profile is not normalized by time
; retrieve the exposure time from the header
    print, 'WARNING: time normalization used: input is assumed to be cts/Pixel'
    fitshead = headfits(filename,ext=1)
    texp = sxpar(fitshead,'EXPOSURE')
    sbr = sbr/texp
    sbr_err = sbr_err/texp

; convert the radii to Mpc
    rinmpc = dangarcmin(z)* rin / 60.
    routmpc = dangarcmin(z) * rout/60.

; compute the counts/second/cm^3 CC assuming spherical symmetry and that
; the emissivity in an annular bin is constant
; this calculation also assumes that any emission outside the
; outermost bin is negligible.
    sbrint = sbr*pi* (rout^2 - rin^2) ; sbrint units cts/sec
    cc= fltarr(maxindex+1)
    vsum = fltarr(maxindex+1,maxindex+1)

; for simplicity, create one r vector with maxindex+1 entries
    r = [routmpc(0), rinmpc]    ; units are Mpc
    for m = 0,maxindex do begin
        for i = 0,maxindex do begin
            v1 = vv(r(i),r(m+1))
            v2 = vv(r(i+1),r(m+1))
            v3 = vv(r(i),r(m))
            v4 = vv(r(i+1),r(m))
            vsum(i,m) = ( (v1-v2) - (v3-v4) )
        endfor
    endfor

;outer radius m=0
    cc(0) = sbrint(0) / vsum(0,0)
    for m=1,maxindex do begin
        interv = 0.0
        for i=0,m-1 do begin
            interv = interv + cc(i)*vsum(i,m)
        endfor
        lastv = vsum(m,m)
        cc(m) = (sbrint(m) - interv)/lastv
    endfor

; set up an array where each row is a different version of sbrint
    sbrint_array=fltarr(nmc,maxindex+1)
    cc_array=fltarr(nmc,maxindex+1)
    cc_mean = fltarr(maxindex+1)
    cc_error= fltarr(maxindex+1)

; populate that array with gaussian variance 
; for now take the errors from dmextract
    sbrint_error = sbr_err  * !pi * (rout^2 - rin^2)
    for imc = 0,nmc-1 do begin
        sbrint_array(imc,*) = sbrint + sbrint_error*randomn(seed,maxindex+1)
        cc_array(imc,0) = sbrint_array(imc,0) / vsum(0,0)
        for m = 1,maxindex do begin
            interv = 0.0
            for i = 0,m-1 do begin
                interv = interv + cc_array(imc,i)*vsum(i,m)
            endfor
            lastv = vsum(m,m)
            cc_array(imc,m) = (sbrint_array(imc,m) - interv)/lastv
        endfor
    endfor


; compute the mean and dispersion of the profiles now they are all computed
    for jmc = 0,maxindex do begin
        cc_mean(jmc) = total(cc_array(*,jmc))/float(nmc)
        cc_error(jmc) = sqrt( total( (cc_array(*,jmc)-cc_mean(jmc))^2/(nmc-1.0) ))
    endfor
    
; the k_spec=norm_xspec/count_rate_xspec bandpass variable can be interpolated
; norm_xspec is the normalization from the spectral fit.
; count_rate_xspec is the number of counts per second in the spectrum in the
; same energy bandpass as the surface brightness profile.
; k_xspec in principle could be  a vector if there is a temperature gradient
; across the profile.
; can also interpolate k_xspec to go beyond the average 
; emission weighted spectrum to a deprojected emission weighted spectrum.
; specifically, for 2A0335+096:
; thawed NH, deprojected spectra rspec=rout 
; cr from 0.7-2.0 keV -- close enough
; the 2A0335+096 projected temperature is more or less constant; use that fit instead
; NH was fixed, projected fits (uncorrelated, too, so that's good.)

    cr = fitfile.cr[where(strmatch(fitfile.cluster,clustername))]
    rspec = fitfile.rout[where(strmatch(fitfile.cluster,clustername))]
    nor = fitfile.norm[where(strmatch(fitfile.cluster,clustername))]
    temp = fitfile.tx[where(strmatch(fitfile.cluster,clustername))]
    temp_hi = fitfile.thi[where(strmatch(fitfile.cluster,clustername))]
    temp_lo = fitfile.tlo[where(strmatch(fitfile.cluster,clustername))]
    temp_err = (temp_hi-temp_lo)

; the change in the ratio is sensitive to the Fe abundance, which
; is about a factor of two higher in the center (0.55 solar) than
; it is in the outskirts (0.24 solar). The temp does not change that
; much

    k_xspec_spectral = (nor/cr)
    k_xspec_interp = interpol (k_xspec_spectral,rspec,rout/60.)
    temp_interp = interpol (smooth(temp,1), rspec, rout/60.) ; NOT smoothed the temp profile to get rid
                                ; of possible artifacts (not needed with 
                                ; project temp) change 1 to higher value if nec.
    temp_err_interp = interpol(temp_err, rspec,rout/60.)
    
; the central error should be that of the last data point
    wcentral = where( (rout ge 0.0) * (rout/60. le min(rspec)),nwcentral)
    wouter = where( rout/60. gt min(rspec))
    wmintemp = where(rspec eq min(rspec))
    xc = findgen(nwcentral)+1.0
    xc = xc/xc                  ; desperately seeking a unit vector
    terr_central = xc * temp_err(wmintemp[0]) ; if I don't turn this into a element, IDL does an array mult!!!
    t_central = xc * temp(wmintemp[0])

; remember indices are "flipped" (inner is outer)
    temp_err_interp = [ temp_err_interp(wouter), terr_central] ; assembles the "correct" temp err vector
    temp_interp_flatmin = [ temp_interp(wouter), t_central ] ; a vector where the central pts=temp(rmin)

; I'm not as worried about the conversion ratio changing - but we could do the same thing to that if desired.
; cc should be in counts/sec/Mpc^3
    nedivnp=1.4/1.2             ; fully ionized
    ;;nelec = sqrt( nedivnp * k_xspec * cc * 4.0 * pi * da(z)^2 * (1+z)^2 / 3.0856e10  )
    nelec_interp = sqrt( nedivnp * k_xspec_interp * cc * 4.0 * pi * da(z)^2 * (1+z)^2 / 3.0856e10  )
    nelec_mean_nmc = sqrt( nedivnp * k_xspec_interp * cc_mean * 4.0 * pi * da(z)^2 * (1+z)^2 / 3.0856e10  )
    nelec_error_nmc = 0.5*nelec_mean_nmc/cc_mean * cc_error

; cc converges to cc_mean reliably (checked)
; to check
    eps_0 = 1.e-14/k_xspec_interp(maxindex) 
    beta = 0.8
    theta_c = 30.0 * (pi/180./60./60.) ; arcseconds to radians
    a1 = eps_0 * sqrt(pi)* gamma(3.*beta-0.5)*theta_c/(4.*pi*(1.+z)^3*gamma(3.*beta))
    sxo = sbr(maxindex) * (180.*60.*60./pi)^2 ; cps/sr
    ne0 = sqrt(sxo/a1/da(z)/3.0856e24)
    print,'General estimate of central electron density per cubic cm, Ne_0 = ', ne0

; assumption - hydrostatic equilibrium
; M(<r)=kTr/(mu m G) * dlog ne/dlog r assume log Te/dlog r is small
; running differential delta log ne / delta log r =
; log (ni/ni+1)/ log(ri/ri+1) ...?
; estimate a running d log ne/ d log r slope piecewise, smooth ne(r) 
; then perhaps smooth the result

    nelec_sm = smooth(nelec_interp,3) 
    dlognedlogr=fltarr(maxindex+1)
    for i=0,maxindex-1 do begin
        dlognedlogr(i) = alog10( nelec_sm(i+1)/nelec_sm(i))/alog10(rout(i+1)/rout(i))
    endfor
    dlognedlogr(maxindex)=dlognedlogr(maxindex-1)

; this differential function is very jagged - smooth it heavily 
; from the smooth
    dlognedlogr=smooth(dlognedlogr,9)
    M_lt_R = -3.7d13* temp_interp * routmpc * dlognedlogr
    M_err = temp_err_interp/temp_interp * m_lt_r *1.1/1.5 ; 2.0 would probably be better (the temp errors
                                ; are the full 90% difference bet. thi and tlo)
                                ; but interpolation & deproj have errors too.
                                ; slope is probably only ok to 10%

; first 5-6 points look like garbage -- in the noise on the
; outer part of the profile where we don't measure the local
; power law index very well.
    r200_kpc = 790. * sqrt(tkev) * (h0/70.)^(-1)
    print,"R200 = ",r200_kpc," kpc h70^-1"

; make entropy profiles and fit to a couple profiles
    s = temp_interp/nelec_interp^(2./3.)
    s_flat = temp_interp_flatmin/nelec_interp^(2./3.) ; temperature profile assumed to flatten inside inner most aperture
    ; s_err^2/s^2 = sig_T^2/T^2 + 4/9 * sig_ne^2/ne^2
    s_err = s * sqrt( (temp_err_interp/temp_interp)^2 + 4./9.*(nelec_error_nmc/nelec_interp)^2 ) ; electron density errors from m.c.

; plot entropy profile and set limits on the fit
    rmean = (rinmpc+routmpc)/2.0

; write a FITS file with rmean, entropy
    tarr = {rmean:rmean, s:s, serr:s_err}
    hdr = ['RMEAN = mean radius in Mpc', 'S = entropy in keV cm^2', 'SERR = entropy error', 'filler=filler']
    mwrfits, tarr, 'ent_mc.fits', hdr, /silent
ENDFOR
END
