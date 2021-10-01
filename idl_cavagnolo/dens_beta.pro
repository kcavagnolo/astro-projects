PRO dens_beta, x, a, f, pder

np   = n_params(0)
rho0   = double(a[0])
rc   = double(a[1])
beta = double(a[2])
bgd  = double(a[3])
f    = (rho0*(1.0+(x/rc)^2.0)^(-3.0*beta/2.0))+bgd

IF (np GE 4) THEN BEGIN
   pder[*,0] = (1.0+(x/rc)^2.)^(-3.0*beta/2.0)
   pder[*,1] = 2.0*rho0*x*rc^(-2)*(-3.0*beta/2.0)*(1.0+(x/rc)^2.)^(-3.0*beta/2.0)
   pder[*,2] = -3.0*rho0*alog10(1.0+(x/rc)^2.)*(1.0+(r/rc)^2.)^(-3.0*beta/2.0)
   pder[*,3] = 1.0
ENDIF

END
