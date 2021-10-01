;+
; use: a = lum2flux(lum, z, H0, mat, lam, /silent])
;-

FUNCTION lum2flux, lum, z, h0, mat, lam, silent=silent

; return to caller on error
ON_ERROR, 2
IF (n_params() LT 1) THEN BEGIN
   doc_library, 'flux2lum'
   RETURN, -99
ENDIF

cosmology, z, out, hubcon=h0, matdens=mat, cosdens=lam, /silent ;# get D_lum
dl = out[2]                                                     ;# D_lum in Mpc
dl = dl*3.08568025d24           ;# 1 Mpc = 3.08568025×10^24 centimeters
flux = lum/(4.0*!pi*dl^2.)

IF NOT keyword_set(silent) THEN BEGIN
   print, 'Lum: ', lum, ' erg/sec'
   print, 'Flux: ', flux, ' erg/cm^2/sec'
ENDIF

RETURN, flux

END
