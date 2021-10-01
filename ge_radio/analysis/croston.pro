PRO croston

;# constants
nu0 = 1.4d9                     ;# 1.4 GHz freq
gamma1 = 4./3.                  ;# spec heat ratio for lobes
gamma2 = 5./3.                  ;# spec heat ratio for ICM
mu = 0.62                       ;# mean molec weight of ICM
secyr = 3.1556926d7             ;# seconds in a year
cmkpc = 3.08568025d21           ;# cm**3 per kpc**3
ergkeV = 1.60217646d-9          ;# ergs per keV
mh = 1.007825*1.660538782d-24   ;# mass hydrogen ATOM in g
Jy = 1.0d-23                    ;# 1Jy = 10**-23 ergs sec**-1 cm**-2 Hz**-1
cmMpc = 3.08568025d24           ;# 1 Mpc = 3.08x10**24 cm

;# read files
file1 = 'croston.dat'
readcol, file1, format='A,F,F,F,F,F', comment='#', $
         name, pv, fD, fkt, z, flux, ferr

;# calc tcs
D = fD/2.0                       ;# want half-sizes
D = D*cmkpc                      ;# convert from kpc to cm
kt = fkt*ergkeV                  ;# convert temps to ergs
pv = pv-51.0                     ;# put in units of 10^51 J
tcs = D/sqrt((gamma2*kt)/(mu*mh)) ;# sound speed of amb gas

;# calc pcav
E = 10.0^(pv)
E = (E*1d58)/4.0
pcav = E/tcs
pcerr = 0.1*pcav

;# using L = 4 * !PI * lum_dist^2 * f * nu0
FOR i=0,n_elements(z)-1 DO BEGIN
   cosmology, z[i], result, /silent
   dl = result[2]
   push, pow, 4 * !PI * (dl * cmMpc)^2 * flux[i] * Jy * nu0 * (1.0+z[i])^(-1.0)
   push, powerr, 4 * !PI * (dl * cmMpc)^2 * ferr[i] * Jy * nu0 * (1.0+z[i])^(-1.0)
ENDFOR

;# log file
OPENW, PLOG, "croston_pcav.dat", /GET_LUN
printf, PLOG, FORMAT='(A-20, 9A15)', '#Name', 'z', 'D', 'kT', 'Ecav', 'tcs', 'Pcav', 'err', 'P1400', 'err'
printf, PLOG, FORMAT='(A-20, 9A15)', '#--', '--', 'kpc', 'keV', 'erg', 's', 'erg/s', 'erg/s', 'erg/s', 'erg/s'
FOR j=0,n_elements(pcav)-1 DO $
   printf, PLOG, FORMAT='(A-20, F15.4, I15, F15.2, 6E15.2)', name[j], z[j], fD[j], fkt[j], E[j], tcs[j], pcav[j], pcerr[j], pow[j], powerr[j]
FREE_LUN, PLOG

END
