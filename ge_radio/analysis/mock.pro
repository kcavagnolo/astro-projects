PRO mock

;# read files
file1 = 'lobe_props.dat'
readcol, file1, format='A,F,F,F,F,F,F,F', comment='#', $
         lname, fr, fD, fkt, rho0, frc, beta, p0
file2 = 'radlum_nvss_withz.dat'
readcol, file2, format='A,A,A,F,F,F,F,F,F,F,A', comment='#', $
         rname, obs, rtype, z, flux, ferr, prad, perr, lum, lerr, surv

;# pick-out objects
FOR i=0,n_elements(lname)-1 DO BEGIN
   ord = where(rname EQ lname[i])
   push, arr, ord[0]
ENDFOR
prad = prad[arr]
perr = perr[arr]
z = z[arr]

;# for 1.4 GHz relation
bcesa = 0.60                    ;# lower limit
bcesb = 1.51                    ;# lower limit
ipcav = (10.^(bcesb))*(prad^(bcesa))
ipcav = ipcav/2.0
ipcav = ipcav*1d42
gamma1 = 4./3.
gamma2 = 5./3.                  ;# spec heat ratio for ICM
mu = 0.62                       ;# mean molec weight of ICM

;# physical constants
secyr = 3.1556926d7             ;# seconds in a year
cmkpc = 3.08568025d21           ;# cm**3 per kpc**3
ergkeV = 1.60217646d-9          ;# ergs per keV
mh = 1.007825*1.660538782d-24   ;# mass hydrogen ATOM in g
eta = gamma1/(gamma1-1)

;# make some conversions
kt = fkt*ergkeV                  ;# convert temps to ergs
rc = frc*cmkpc                   ;# convert from kpc to cm
r = fr*cmkpc                     ;# convert from kpc to cm
D = fD*cmkpc                     ;# convert from kpc to cm
tcs = D/sqrt((gamma2*kt)/(mu*mh)) ;# sound speed of amb gas

;# fixed lobe V
V = (4./3.)*!PI*r^3.             ;# lobe volume
ip = (eta*ipcav*tcs)/V           ;# ideal pressure in erg/s
irho = ip/(2.0*kt)
irho0 = irho*((1.0+(D/rc)^2.)^(3.0*beta/2.0))
print, '------------'
print, 'Fixed Volume'
print, '------------'
FOR i=0,n_elements(ip)-1 DO BEGIN
   print, FORMAT='(A-30)', lname[i]
   print, FORMAT='(A-30,E10.2,A12)', 'Needed p(D):', ip[i], 'erg/cm^3'
   print, FORMAT='(A-30,E10.2,A12)', 'Implies rho0:', irho0[i], 'cm^-3'
   print, '------------'
ENDFOR
print, ''
print, ''

;# fixed p(r) and V_radio
pbkg = 1d-13
dens = rho0*(1.0+(D/rc)^2.)^(-3.0*beta/2.0) ;# beta dens profile
bp = (2.0*dens*kt)+pbkg                     ;# rad p prof
E = eta*bp*V
pcav = (2.0*E)/tcs
pcerr = 0.1*pcav
prad = prad*1d40
perr = perr*1d40

;# log file
OPENW, PLOG, "jetted_pcav.dat", /GET_LUN
printf, PLOG, FORMAT='(A-20, 9A15)', '#Name', 'z', 'D', 'kT', 'Ecav', 'tcs', 'Pcav', 'err', 'P1400', 'err'
printf, PLOG, FORMAT='(A-20, 9A15)', '#--', '--', 'kpc', 'keV', 'erg', 's', 'erg/s', 'erg/s', 'erg/s', 'erg/s'
FOR j=0,n_elements(pcav)-1 DO BEGIN
;   dumr = maken(1,1d6,1d6)
;   dumr = dumr*cmkpc
;   dens = rho0[j]*(1.0+(dumr/rc[j])^2.)^(-3.0*beta[j]/2.0) ;# beta dens profile
;   dkt = replicate(kt[j],n_elements(dumr))
;   bp = (2.0*dens*dkt)+1d-13
;   plot, dumr, bp, /xlog, /ylog
;   print, lname[j]
;   stop
   printf, PLOG, FORMAT='(A-20, F15.4, I15, F15.2, 6E15.2)', lname[j], z[j], fD[j], fkt[j], E[j], tcs[j], pcav[j], pcerr[j], prad[j], perr[j]
ENDFOR
FREE_LUN, PLOG

;# idealized volumes
iV = (ipcav*tcs)/(eta*bp)                   ;# needed volume
nr = ((3.0*iV)/(4.0*!PI))^(1./3.)           ;# implied radius
nr = nr/cmkpc
print, '--------------'
print, 'Fixed Pressure'
print, '--------------'
FOR i=0,n_elements(nr)-1 DO BEGIN
   print, FORMAT='(A-30)', lname[i]
   print, FORMAT='(A-30,I10,A12)', 'Needed radius:', nr[i], 'kpc'
   print, '--------------'
ENDFOR
print, ''
print, ''

;# fixed tcs
itcs = (eta*bp*V)/ipcav
ikt = ((mu*mh)*(D/itcs)^2.)/gamma2
itcs = itcs/secyr
ikt = ikt/ergkeV
print, '--------'
print, 'Fixed pV'
print, '--------'
FOR i=0,n_elements(itcs)-1 DO BEGIN
   print, FORMAT='(A-30)', lname[i]
   print, FORMAT='(A-30,E10.2,A12)', 'Needed tcs:', itcs[i], 'yrs'
   print, FORMAT='(A-30,E10.2,A12)', 'Implies kT:', ikt[i], 'keV'
   print, '--------------'
ENDFOR
print, ''
print, ''

END
