;+
; NAME:
;
; PURPOSE:
;
; CATEGORY:
;
; CALLING SEQUENCE:
;
; INPUTS:
;
; OPTIONAL KEYWORD INPUTS:
;
; OUTPUTS:
;
; PROCEDURE:
;
; EXAMPLE:
;
; PROCEDURES CALLED:
;
; MODIFICATION HISTORY:
;   Written by: KWC, Dec, 2009.
;
;-

FUNCTION c12, alpha, nu1, nu2

;# handle errors
ON_ERROR, 2
IF (n_params() LT 1) THEN BEGIN
   doc_library, 'c12'
   RETURN, -1
ENDIF

;# handle bad input
IF ((nu1 LT 0.) OR (nu2 LT 0.)) THEN BEGIN
   print, 'ERROR - nu1 and nu2 cannot be less than zero. You gave me:'
   print, 'alpha = ',alpha
   print, 'nu1 = ',nu1
   print, 'nu2 = ',nu2
   return, -1
ENDIF

;# change sense of alpha
alpha = -alpha

;# define some constants
qe = 4.80320427d-10             ;# electron charge in statC = g^1/2 cm^3/2 s^-1
me = 9.10938215d-28             ;# electron mass in g
c = 2.99792458d10               ;# speed light in cm/s

c1 = (3.*qe)/(4.*!PI*me^3.*c^5.)
c2 = (2.*qe^4.)/(3*me^4.*c^7.)

num = (2*alpha-2)*(nu1^(0.5*(1.-2.*alpha))-nu2^(0.5*(1.-2.*alpha)))
den = (2*alpha-1)*(nu1^(1.-alpha)-nu2^(1.-alpha))
c12 = c2^(-1.)*c1^(1./2.)*num/den

return, c12

END
