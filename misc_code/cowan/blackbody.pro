FUNCTION blackbody, lambda, T
; Nick Cowan
; July 2009
; Just a modified version of planck.pro
;
; INPUTS:
; lambda (scalar of wavelength in Angstroms)
; T (one or two-dimensional array of temperatures in Kelvin)
;
; OUTPUTS:
; BBFLUX[n_T] (one or two-dimensional array giving the blackbody flux
;              (i.e. !pi*Intensity) in erg/cm^2/s/A in at the
;              specified wavelength and temperatures.)

 On_error,2

 if ( N_elements(lambda) NE 1 ) then begin
     print,'Syntax - bbflux = blackbody( lambda, T)'
     return,0
  endif    

  if ( N_elements( T ) LT 1 ) then $
      read,'Enter a blackbody temperature', T

  bbflux = T*0.

; Gives the blackbody flux (i.e. PI*Intensity) ergs/cm2/s/a

  w = lambda / 1.E8                              ; Angstroms to cm    
;constants appropriate to cgs units.
  c1 =  3.7417749d-5                ; =2*!DPI*h*c*c       
  C2 =  1.4387687d                  ; =h*c/k
  val =  c2/w/T  
  mstr = machar(double = (size(val,/type) EQ 5) )  ;Get machine precision      
  good = where( val LT alog(mstr.xmax), Ngood )    ;Avoid floating underflow

  if ( Ngood GT 0 ) then  $
      bbflux[ good ] =  C1 / ( w[good]^5 * ( exp( val[good])-1. ) )

  return, bbflux*1.E-8              ; Convert to ergs/cm2/s/A

;BBFLUX = 0.0*T

;FOR index=0, N_ELEMENTS(T)-1 DO BBFLUX[index] = PLANCK(lambda, T[index])

;RETURN, BBFLUX

END
