;+
; NAME:
;   LUMIN
;
; PURPOSE:
;   This function returns luminoisty in ergs/sec for input
;   redshift and flux [default ergs/sec/cm^2]
;
; CATEGORY:
;   Cosmology
;
; CALLING SEQUENCE:
;   result = lumin(z, flux, [/MILLIJY, /MICROJY, /SILENT])
;
; INPUTS:
;   z:      the redshift of the object
;   flux:   the flux of the object in question (assumed to be in ergs/sec/cm^2)
;   result: the name of the array which will be output
;
; OPTIONAL KEYWORD INPUTS:
;   /SILENT - If set, the program will not display adopted
;             cosmological parameters at the terminal.
;   H0:       Hubble parameter  in km/s/Mpc, default is 70
;   omega_m:  Matter density, normalized to the closure density,
;             default is 0.3. Must be non-negative
;   lambda0:  Cosmological constant, normalized to the closure
;             density, default is 0.7
;   JY:       set this if the flux is in Janskys
;   MILLIJY:  set this if the flux is in milli Janskys
;   MILLIJY:  set this if the flux is in micro Janskys
;   POWER:    set this to calculate nu L_nu, MUST SUPPLY nu0
;   NU0:      central frequency for power calculation
;
;   note: 1Jy = 10**-23 ergs sec^-1 cm^-2 Hz^-1
;
; OUTPUTS:
;   Luminosity in ergs/sec h_{70}**-1
;
; EXAMPLE:
;
; PROCEDURES CALLED:
;   COSMOLOGY
;
; MODIFICATION HISTORY:
;   Written by KWC, UWaterloo, Dec, 2008.
;-

;#####################
;#####################

FUNCTION lumin, z, flux, h0=h0, omega_m=omega_m, lambda0=lambda0, $
                jy=jy, millijy=millijy, microjy=microjy, power=power, nu0=nu0, $
                silent=silent

jan = 'no'
Jy = 1.0d-23                    ;# 1 Jy  = 10**-23 ergs sec**-1 cm**-2 Hz**-1
cmMpc = 3.08d24                 ;# 1 Mpc = 3.08x10**24 cm

IF (n_params() LT 2) THEN BEGIN
   doc_library, 'lumin'
   RETURN, -1
ENDIF

IF ((flux LE 0.) OR (z LE 0.)) THEN BEGIN
   print, 'ERROR - input parameters cannot be less than or equal to zero. You gave me:'
   print, 'z = ',z
   print, 'flux = ',flux
   return, -1
ENDIF

; Check for input cosmo variables
IF n_elements(h0) EQ 0 THEN h0 = 70.
IF n_elements(omega_m) EQ 0 THEN omega_m = 0.27
IF n_elements(lambda0) EQ 0 THEN lambda0 = 0.73
IF NOT keyword_set(silent) THEN $
   print, f='(A,I3,A,F5.2,A,F5.2)', $
          'LUMIN params: H0:', h0, ' Omega_m:', omega_m, ' Lambda0', lambda0

;# compute lum dist
cosmology, z, hubcon=h0, matdens=omegam, cosdens=lambda0, result, /silent
dl = result[2]

;# take care of flux in Jy
IF keyword_set(jy) THEN $
   jan = 'yes'
IF keyword_set(millijy) THEN BEGIN
   flux = flux*1e-3
   jan = 'yes'
ENDIF
IF keyword_set(microjy) THEN BEGIN
   flux = flux*1e-6
   jan = 'yes'
ENDIF

;# calculate lumin
IF jan EQ 'yes' THEN BEGIN
   ;# using L = 4 * !PI * lum_dist^2 * f * Jy
   lum = 4 * !PI * (dl * cmMpc)^2 * flux * Jy
ENDIF ELSE BEGIN
   ;# using L = 4 * !PI * lum_dist^2 * f
   lum = 4 * !PI * (dl * cmMpc)^2 * flux
ENDELSE

;# calculate power
IF keyword_set(power) THEN $
   lum = lum * nu0

return, lum

END
