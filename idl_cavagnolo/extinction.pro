;+
; NAME:
;
; PURPOSE:
;
; CALLING SEQUENCE:
;   extinction, lambda, result
;
; INPUTS:
;   lambda = wavelength in microns
;   result = array output
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

;#####################
;#####################

FUNCTION ir, x
common constants, c, rv

print, FORMAT='(F10.3,A40)', x, 'um^-1 is in the FIR regime.'
a = 0.574*x^(1.61)
b = -0.527*x^(1.61)
print, FORMAT='(A-20,F10.3)', 'a(x) =', a
print, FORMAT='(A-20,F10.3)', 'b(x) =', b
ext = a+b/rv
return, ext

END

;#####################
;#####################

FUNCTION opt, x
common constants, c, rv

print, FORMAT='(F10.3,A40)', x, 'um^-1 is in the Optical/NIR regime.'
y = (x-1.82)
a = 1. + 0.17699*y - 0.50447*y^2. - 0.02427*y^3. + 0.72085*y^4. + 0.01979*y^5. - 0.77530*y^6. + 0.32999*y^7.
b = 1.41338*y + 2.29305*y^2. + 1.07233*y^3. - 5.38434*y^4. - 0.62251*y^5. + 5.30260*y^6. - 2.09002*y^7.
print, FORMAT='(A-20,F10.3)', 'a(x) =', a
print, FORMAT='(A-20,F10.3)', 'b(x) =', b
ext = a+b/rv
return, ext

END

;#####################
;#####################

FUNCTION uv, x
common constants, c, rv

print, FORMAT='(F10.3,A40)', x, 'um^-1 is in the NUV regime.'
IF x LT 5.9 THEN BEGIN
   fa = 0
   fb = 0
ENDIF ELSE IF x GE 5.9 THEN BEGIN
   fa = -0.04473*(x-5.9)^2. - 0.009779*(x-5.9)^3.
   fb = 0.2130*(x-5.9)^2. + 0.1207*(x-5.9)^3.
ENDIF

a = 1.752 - 0.316*x - (0.104/((x-4.67)^2. + 0.341)) + fa
b = -3.090 + 1.825*x + (1.206/((x-4.62)^2. + 0.263)) + fb
print, FORMAT='(A-20,F10.3)', 'a(x) =', a
print, FORMAT='(A-20,F10.3)', 'b(x) =', b
ext = a+b/rv
return, ext

END

;#####################
;#####################

FUNCTION fuv, x
common constants, c, rv

print, FORMAT='(F10.3,A40)', x, 'um^-1 is in the FUV regime.'
a = -1.073 - 0.628*(x-8.) + 0.137*(x-8.)^2. - 0.070*(x-8.)^3.
b = 13.670 + 4.257*(x-8.) -0.420*(x-8.)^2. + 0.374*(x-8.)^3.
print, FORMAT='(A-20,F10.3)', 'a(x) =', a
print, FORMAT='(A-20,F10.3)', 'b(x) =', b
ext = a+b/rv
return, ext

END

;#####################
;#####################

PRO extinction, lambda, result

; return to caller on error
ON_ERROR, 2
IF (n_params() LT 1) THEN BEGIN
   doc_library, 'extinction'
   RETURN
ENDIF

; set defaults if not input by user
common constants, c, rv
c = 2.9979e5                    ; speed of light in km/s because H0 is in km/s/Mpc
rv = 3.1                        ; assumed value for R_V which is related to grain sizes in MW
x = 1./lambda                   ; inverse wavelength which are units used for fitting in Cardelli 1989

FOR i=0,n_elements(lambda)-1 DO BEGIN
   print, FORMAT='(A-30)', '-----------------------------------------'
   tx = x[i]
   IF ((tx LT 0.3) OR (tx GT 10.)) THEN BEGIN
      print, 'ERROR: ', num2str(tx), ' um^-1 is outside the acceptable range for Cardelli 1989 fits.'
      text = -1.0
   ENDIF
   IF ((tx LE 1.1) AND (tx GE 0.3)) THEN text = ir(tx)
   IF ((tx LE 3.3) AND (tx GT 1.1)) THEN text = opt(tx)
   IF ((tx LE 8.0) AND (tx GT 3.3)) THEN text = uv(tx)
   IF ((tx LE 10.) AND (tx GE 8.0)) THEN text = fuv(tx)
   print, FORMAT='(A-20,F10.3)','<A(lambda)/A(V)>: ', text
   print, FORMAT='(A-30)', '-----------------------------------------'
   push, result, text
ENDFOR

END
