PRO nuker, x, a, f, pder

np    = n_params(0)
irb   = double(a[0])
ibgd  = double(a[1])
alpha = double(a[2])
beta  = double(a[3])
gamma = double(a[4])
rb    = double(a[5])
r = x/rb
ep = (beta-gamma)/alpha
e = (gamma-beta)/alpha
f = (irb*(2.0^ep)*(r^(-gamma))*((1.0+r^alpha)^e))+ibgd

IF (np GE 4) THEN BEGIN
   print, 'Seriously? You want to calculate partial derivatives? Get bent >:)~'
   message, 'Fuck off'
;   pder[*,0] = (1.0+(x/rc)^2.)^(-3.0*beta+0.5)
;   pder[*,1] = 2.0*s0*x*rc^(-2)*(-3.0*beta+0.5)*(1.0+(x/rc)^2.)^(-3.0*beta-0.5)
;   pder[*,2] = -3.0*s0*alog10(1.0+(x/rc)^2.)*(1.0+(r/rc)^2.)^(-3.0*beta+0.5)
;   pder[*,3] = 1.0
ENDIF

END
