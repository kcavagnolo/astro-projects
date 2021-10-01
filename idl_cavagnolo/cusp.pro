PRO cusp, x, a, f, pder

np    = n_params(0)
rhogc = double(a[0])
rc    = double(a[1])
beta  = double(a[2])
alpha = doubel(a[3])
bgd   = double(a[3])

f = rhogc*2.0^((3.0*beta/2.0)-(alpha/2.0))*(x/rc)^(-alpha)*(1.0+(x/rc)^2.0)^((-3.0*beta/2.0)+alpha/2.0)

IF (np GE 5) THEN BEGIN
   pder[*,0] = 1.0
ENDIF

END
