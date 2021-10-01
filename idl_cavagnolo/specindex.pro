;+
; use: a = specindex(flux1, flux2, nu1, nu2, [errs=errs, /silent])
;-

FUNCTION specindex, s1, s2, nu1, nu2, errs=errs, silent=silent

; return to caller on error
ON_ERROR, 2
IF (n_params() LT 4) THEN BEGIN
   doc_library, 'specindex'
   RETURN, -9d2
ENDIF

alpha = alog10(s1/s2)/alog10(nu1/nu2)

IF (n_elements(errs) EQ 4) THEN BEGIN
   err = 0.434*((errs[0]/s1)+(errs[1]/s2)+(errs[2]/nu1)+(errs[3]/nu2))
ENDIF ELSE err=-1

IF NOT keyword_set(silent) THEN BEGIN
   print, 'alpha: ', alpha, ' +/- ', err
ENDIF

RETURN, alpha

END
