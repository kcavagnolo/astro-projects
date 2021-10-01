PRO fix_nvss

indat = 'pjn_dl_nvss.dat'
readcol, indat, format='A,A,F,D,F,F,D', $
         name, type, z, dl, flux, ferr, nu0

;# some constants
Jy     = 1.0d-23                ;# 1Jy = 10**-23 ergs sec**-1 cm**-2 Hz**-1
cmMpc  = 3.08568025d24          ;# 1 Mpc = 3.08x10**24 cm
alpha  = 0.8

;# do calc
FOR i=0,n_elements(name)-1 DO BEGIN
   pow    = 4.0 * !PI * (dl[i] * cmMpc)^2 * (1.0+z[i])^(alpha-1) * flux[i] * nu0[i] * Jy
   powerr = 4.0 * !PI * (dl[i] * cmMpc)^2 * (1.0+z[i])^(alpha-1) * ferr[i] * nu0[i] * Jy
   pow = pow/1d40
   powerr = powerr/1d40
   print, name[i], '   ', type[i], pow, powerr
ENDFOR

END
