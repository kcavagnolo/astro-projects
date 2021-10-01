;+
; NAME:
;
; PURPOSE:
;
; CALLING SEQUENCE:
;   mag2lum, appm, z, filter, errs=errs
;
; INPUTS:
;
; OPTIONAL KEYWORD INPUTS:
;
; OUTPUTS:
;
; EXAMPLE:
;
; MODIFICATION HISTORY:
;
;-

PRO mag2lum, appm, z, filter, errs=errs

;# set defaults if not input by user
lsun = 3.839d33                 ;# luminosity of sun in ergs/s
filters = ['GFUV', 'GNUV', 'U', 'B', 'V', 'R', 'I', 'J', 'H', 'K', 'F300W', 'F450W', 'F555W', 'F606W', 'F702W', 'F814W', 'UVW1', 'UVM2']
Msun = [16.02, 10.18, 5.61, 5.48, 4.83, 4.42, 4.08, 3.64, 3.32, 3.28, 7.52, 5.22, 4.83, 4.74, 4.58, 4.57, 10.18, 10.18]

;# return to caller on error
ON_ERROR, 2
IF (n_params() LT 3) THEN BEGIN
   doc_library, 'mag2lum'
   print, 'Available filters:'
   print, filters
   RETURN
ENDIF

;# check if errs supplied
IF (n_elements(errs) EQ n_elements(appm)) THEN doerr='y' ELSE doerr='n'

;# take care of errs
IF doerr EQ 'y' THEN $
   errper = abs(errs/appm)

;# loop through each entry
print, FORMAT='(A-30)', '#---------------------------------------#'
FOR i=0,n_elements(appm)-1 DO BEGIN
   cosmology, z[i], cout, /silent ;# compute lum distance
   dl = cout[2]*1d6               ;# lum distance in pc
   M = appm[i]-(5.0*(alog10(dl)-1.0))
   ind = where(filters EQ filter[i], count)
   IF count LT 1 THEN BEGIN
      print, 'ERROR: do not know of filter ',filter[i]
      GOTO, SKIP
   ENDIF
   slum = 10.0^((Msun[ind]-M)/2.5)
   lum = slum*lsun
   flux = lum/(4.0*!PI*(dl*3.08568025d18)^2.)
   IF doerr EQ 'y' THEN BEGIN
      slumerr = abs(errper[i]*slum)
      lumerr = abs(errper[i]*lum)
      fluxerr = abs(errper[i]*flux)
      Merr = abs(errper[i]*M)
      appmerr = abs(errper[i]*appm[i])
   ENDIF ELSE BEGIN
      slumerr = 0.0
      lumerr = 0.0
      fluxerr = 0.0
      Merr = 0.0
      appmerr = 0.0
   ENDELSE
   print, FORMAT='(A-30,A12)','Filter: ', filter[i]
   print, FORMAT='(A-30,F12.3,A6,F12.3)','App. Mag. [mags]: ', appm[i], '+/-', appmerr
   print, FORMAT='(A-30,F12.3,A6,F12.3)','Abs. Mag. [mags]: ', M, '+/-', Merr
   print, FORMAT='(A-30,E12.3,A6,E12.3)','Flux [erg/s/cm**2]: ', flux, '+/-', fluxerr
   print, FORMAT='(A-30,E12.3,A6,E12.3)','Luminosity [erg/s]: ', lum, '+/-', lumerr
   print, FORMAT='(A-30,E12.3,A6,E12.3)','Luminosity [L_sun]: ', slum, '+/-', slumerr
   SKIP:
   print, FORMAT='(A-30)', '#---------------------------------------#'
ENDFOR

END
