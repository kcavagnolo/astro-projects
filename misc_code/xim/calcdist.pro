;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; CALCDIST: CALCULATE ANGULAR AND LUMINOSITY DISTANCE ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

pro calcdist,z

common cosmo,omegam,omegal,omegar,omega,h0,pdist,adist,ldist

; first, get proper distance
pdist=float(3.e5/h0*$
            qsimp('ddp',1.e/(1.e + z),$
                  1.e,/double,eps=1.e-3,jmax=100))

; distinguish cosmologies
if omega ne 1.e then R0=3.e5/h0/sqrt(abs(omega-1.e))
if omega gt 1.e then sk=R0*sin(pdist/R0) else $
  if omega lt 1.e then sk=R0*sinh(pdist/R0) else $
  sk=pdist

; calculate angular / luminosity distance in kpc
pdist*=1.e3
ldist=sk*1.e3
adist=sk/(1.e + z)*1.e3

end
