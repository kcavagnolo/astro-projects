;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; DPP: CALCULATE PROPER DISTANCE FOR COSMOLOGY ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

function ddp,a

common cosmo,omegam,omegal,omegar,omega,h0,pdist,adist,ldist

; return proper distance in units of c/H0
return,1.e/(a*sqrt(omegar/a^2 + omegam/a + omegal*a^2 + (1.e - omega)))

end
