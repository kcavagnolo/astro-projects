;+
; NAME:
;   COSMOLOGY
;
; PURPOSE:
;   This procedure returns cosmographic quantities for the input redshift.
;
; CALLING SEQUENCE:
;   COSMOLOGY, z, result, [, hubcon= , matdens= , cosdens= , raddens= , curve= , dubu= ]
;
; INPUTS:
;   z:      the redshift of the cluster or object
;   result: the name of the array which will be output
;
; OPTIONAL KEYWORD INPUTS:
;    hubcon:  Hubble constant in km/s/Mpc, default is 70
;    matdens: normalized matter density, default is 0.3, CANNOT BE NEGATIVE
;    cosdens: normalized cosmological constant, default is 0.7
;    raddens: normalized radiation density, defualt is ~0.0
;    curve:   normalized curvature constant, default is 0.0
;    dubu:    equation of state parameter, w, default is -0.8, only
;             used for single-component, spatially flat Universes
;
; OUTPUTS:
;    Line-of-sight comoving distance (proper distance) in Mpc
;    Transverse comoving distance in Mpc
;    Luminosity distance in Mpc
;    Angular diameter distance in Mpc
;    Angular diameter distance in kpc/arcsec
;    Comoving volume in Gpc**3
;    Lookback time in Gyr
;
; EXAMPLE:
;   cosmology, 0.2, result
;      Line-of-sight comoving distance: 816.717 Mpc
;      Transverse comoving distance: 816.717 Mpc
;      Luminosity distance: 980.060 Mpc
;      Angular diameter distance: 680.597 Mpc
;      Angular diameter distance in arcmin: 3.29963 kpc/arcsec
;      Comoving volume: 2.28193 Gpc**3
;      Lookback time: 2.43203 Gyrs
;
; MODIFICATION HISTORY:
;   Written by: KWC, MSU-PA, Nov, 2004.
;   Full header added and integration tweeked Nov, 2006.
;-

;#####################
;#####################

FUNCTION dlos, z
; line-of-sight comoving distance
common constants, H0, omega_m, lambda, omega_r, k, w, c
omega = omega_m + lambda + omega_r

IF (omega EQ 0. AND (k GT 0. OR k NE -1.)) THEN BEGIN ; bad model
    print, 'Positively curved, empty Universes are not permitted! >:|'
    return, 0.
ENDIF ELSE IF (omega EQ 0. AND k EQ -1.) THEN BEGIN ; curvature only
    return, c*ALOG(1.+z)/H0
ENDIF ELSE IF (k EQ 0. AND $    ; spatially flat, single component
               ((omega EQ 1. AND omega_r EQ 0. AND lambda EQ 0.) OR $
                (omega EQ 0. AND omega_r EQ 1. AND lambda EQ 0.) OR $
                (omega EQ 0. AND omega_r EQ 0. AND lambda EQ 1.))) THEN BEGIN
    IF (omega_m EQ 1.) THEN w = 0 ; matter only
    IF (lambda EQ 1.) THEN  w = -1. ; lambda only
    IF (omega_r EQ 1.) THEN w = 1./3. ; radiation only
    return, (c/H0)*(2./(1+3.*w))*(1.-(1.+z)^(-(1.+3.*w)/2.))
ENDIF ELSE IF (omega NE 0.) THEN BEGIN ; integration of comoving distance
    dc = z*0.
    FOR i = 0,n_elements(z)-1 DO BEGIN
        zint = findgen(100)/99. * z[i]
        dz_int = 1.0/sqrt(omega_m*(1.+zint)^3.+omega_r*((1.+zint)^2.)+lambda)
        dc[i] = (c/H0)*int_tabulated(zint,dz_int,/double)
    ENDFOR
    return,dc
ENDIF ELSE BEGIN
    print, 'ERROR: do not understand given model.'
    return, 0.
ENDELSE
END

;#####################
;#####################

FUNCTION dtrans, z
; transverse comoving distance
common constants, H0, omega_m, lambda, omega_r, k, w, c

; positive curvature
IF (k GT 0.) THEN BEGIN
    dc = dlos(z)
    term = (sqrt(k)*H0*dc)/c
    dm = (c/H0)*(1./sqrt(k))*SINH(term)
    RETURN, dm
ENDIF

; flat
IF (k EQ 0.) THEN BEGIN
    dc = dlos(z)
    dm = dc
    RETURN, dm
ENDIF

; negative curvature
IF (k LT 0.) THEN BEGIN
    dc = dlos(z)
    term = (sqrt(ABS(k))*H0*dc)/c
    dm = (c/H0)*(1./sqrt(ABS(k)))*SINH(term)
    RETURN, dm
ENDIF

END

;#####################
;#####################

FUNCTION dlum, z
; luminosity distance
common constants, H0, omega_m, lambda, omega_r, k, w, c

dlum = (1.+z)*dtrans(z)
RETURN,dlum
END

;#####################
;#####################

FUNCTION dang, z
; angular diameter distance
common constants, H0, omega_m, lambda, omega_r, k, w, c

dang = dtrans(z)/(1.+z)
RETURN,dang
END

;#####################
;#####################

FUNCTION dangarc, z
; ang diam distance per arcsec
common constants, H0, omega_m, lambda, omega_r, k, w, c

dangarc = 1000.*(1./(!radeg*3600.))* dtrans(z)/(1.+z)
RETURN,dangarc
END

;#####################
;#####################

FUNCTION volume,z
; compute the total comoving volume out to redshift z
common constants, H0, omega_m, lambda, omega_r, k, w, c

dh = c/H0
; positive curvature
IF (k GT 0.) THEN BEGIN
    dm = dtrans(z)
    term1 = ((4.*!pi*dh^3.)/(2.*k))
    term2 = (dm/dh)*sqrt(1.+(k*dm^2./dh^2.))
    term3 = (1./sqrt(abs(k)))*ASINH(sqrt(abs(k))*dm/dh)
    vc = term1*(term2-term3)
    RETURN, vc/1.e9
ENDIF

; flat
IF (k EQ 0.) THEN BEGIN
    dm = dtrans(z)
    vc = 4.*!pi*dm^3./3.
    RETURN, vc/1.e9
ENDIF

; negative curvature
IF (k LT 0.) THEN BEGIN
   dm = dtrans(z)
   term1 = ((4.*!pi*dh^3.)/(2.*k))
   term2 = (dm/dh)*sqrt(1.+(k*dm^2./dh^2.))
   term3 = sqrt(abs(k))^(-1.)*ASIN(sqrt(abs(k))*dm/dh)
   vc = term1*(term2-term3)
   RETURN, vc/1.e9
ENDIF
END

;#####################
;#####################

FUNCTION lookback,z
; calculate the lookback time in Gyr
common constants, H0, omega_m, lambda, omega_r, k, w, c
omega = omega_m + omega_r + lambda

; integration of comoving distance
IF (omega EQ 0.) THEN return, 0.
tl = z*0.
FOR i = 0,n_elements(z)-1 DO BEGIN
    zint = findgen(100)/99. * z[i]
    dz_int = ((1.+zint)*(sqrt(omega_m*(1.+zint)^3.+omega_r*((1.+zint)^2.)+lambda)))^(-1.)
    tl[i] = int_tabulated(zint,dz_int,/double)
ENDFOR
tl = tl*3.08568025E19/31556926./1.E9/H0
return, tl
END

;#####################
;#####################

PRO cosmology, z, result, hubcon=hubcon, matdens=matdens, cosdens=cosdens, raddens=raddens, curve=curve, dubu=dubu, silent=silent

; return to caller on error
ON_ERROR, 2
IF (n_params() LT 1) THEN BEGIN
   doc_library, 'cosmology'
   RETURN
ENDIF

; set defaults if not input by user
common constants, H0, omega_m, lambda, omega_r, k, w, c
c = 2.9979e5                    ; speed of light in km/s because H0 is in km/s/Mpc
IF (n_elements(hubcon)  EQ 0) THEN H0=70.1 ELSE H0=hubcon
IF (n_elements(matdens) EQ 0) THEN omega_m=0.279 ELSE omega_m=matdens
IF (n_elements(cosdens) EQ 0) THEN lambda=0.721 ELSE lambda=cosdens
IF (n_elements(raddens) EQ 0) THEN omega_r=0. ELSE omega_r=raddens
IF (n_elements(curve)   EQ 0) THEN k=0. ELSE k=curve
IF (n_elements(dubu)    EQ 0) THEN w=-0.8 ELSE w=dubu

losd = dlos(z)
if losd EQ 0. then GOTO, err
transd = dtrans(z)
lumd = dlum(z)
angd = dang(z)
arcd = dangarc(z)
vol = volume(z)
look = lookback(z)
result = [losd,transd,lumd,angd,arcd,vol,look]

IF NOT keyword_set(silent) THEN BEGIN
   print, FORMAT='(A-10,F7.2)','H_0:',H0
   print, FORMAT='(A-10,F7.2)','Omega_M:',omega_m
   print, FORMAT='(A-10,F7.2)','Lambda:',lambda
   print, FORMAT='(A-10,F7.2)','Omega_R:',omega_r
   print, FORMAT='(A-10,F7.2)','k:',k
   print, FORMAT='(A-10,F7.2)','w:',w 
   print, FORMAT='(A-35,F10.3,A-13)','Line-of-sight comoving distance',losd,' Mpc'
   print, FORMAT='(A-35,F10.3,A-13)','Transverse comoving distance',transd,' Mpc'
   print, FORMAT='(A-35,F10.3,A-13)','Luminosity distance',lumd,' Mpc'
   print, FORMAT='(A-35,F10.3,A-13)','Angular diameter distance',angd,' Mpc'
   print, FORMAT='(A-35,F10.3,A-13)','Angular diameter distance in arcsec',arcd,' kpc/arcsec'
   print, FORMAT='(A-35,F10.3,A-13)','Comoving volume',vol,' Gpc**3'
   print, FORMAT='(A-35,F10.3,A-13)','Lookback time',look,' Gyrs'
ENDIF

err: 
if losd EQ 0. then begin
    result = [0.,0.,0.,0.,0.,0.,0.]
    print, 'COSMOLOGY: ERROR'
endif

END
