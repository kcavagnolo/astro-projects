PRO strip

;# constants
vdisp = 1109.
msun = 1.9891d30                              ;# solar mass [kg]
G    = 6.67300d-11*msun/2.93800656d58         ;# G in kpc^3/Msol/s^2
mu   = 0.597                                  ;# mean molecular weight of ICM assuming primordial gas comp
mh   = 1.007825*1.660538782d-27               ;# atomic mass of hydrogen atom x atomic mass unit [kg]
cmkpc = 3.08568025d21                         ;# cm's in 1 kpc

;# ICM dens profile
myhome   = GETENV('HOME')
restore, myhome+'/research/redux/scripts/s_tabletemplate.sav'
file1    = myhome+'/research/iras09/data/10445_509_table.dat'
dataobs  = read_ascii(file1, template = s_tabletemplate)
rin      = dataobs.rin_mpc*1000.
rout     = dataobs.rout_mpc*1000.
rdens    = (0.5*(rin^(3./2.)+rout^(3./2.)))^(2./3.)
nelec    = dataobs.n_elec
nelecerr = dataobs.sigma_ne
rdens    = reverse(rdens)
nelec    = reverse(nelec)
nelecerr = reverse(nelecerr)
rho      = (1.92*mu*mh)*nelec*cmkpc^3./msun
rhoerr   = rho*(nelecerr/nelec)

;# gal props
M = 1d12
sigs = 400.*(1000)^2.
r200 = 200.
w = 0.5
z = 1.5
r = sqrt(w^2.+z^2.)
c = 10.0

;# constants
rs = r200/c
num = G*M*z*sigs*((alog(1.0+(r/rs))/r)-(1.0/(r+rs)))
den = rho*r^2.0*(alog(1.0+c)-(c/(c+1.0)))
v = sqrt(num/den)
v = v*(cmkpc/1d5)
print, 'Minimum velocity: ',min(v),' km/s'
ord = where(v LE vdisp)
print, 'Minimum radius: ', max(rdens[ord]), ' kpc'

;# plot the results
plot, rdens, v, $
      /xlog, /ylog

oplot, maken(1d-10,1d10,10), replicate(vdisp,10)

END
