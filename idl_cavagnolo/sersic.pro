PRO sersic, x, a, f, pder

np   = n_params(0)
i0   = double(a[0])
ibgd = double(a[1])
k    = double(a[2])
n    = double(a[3])
tx   = alog(x)
f    = exp(alog(i0) - (k*x^(1./n))) + ibgd

IF (np GE 4) THEN BEGIN
   print, 'Seriously? You want to calculate partial derivatives? Get bent >:)~'
   message, 'Fuck off'
;   pder[*,0] = 0.
;   pder[*,1] = 0.
;   pder[*,2] = 0.
;   pder[*,3] = 0.
ENDIF

END
