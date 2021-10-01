PRO pv
!quiet=1
;#################################
;#################################

myhome = GETENV('HOME')
restore, myhome+'/research/redux/scripts/s_tabletemplate.sav'
pfile = myhome+'/research/iras09/data/10445_509_table.dat'

;# bolometric lum of QSO
tbon = 1.0                      ;# tx at Rbon in keV
mgas = 1d10                     ;# cd gas mass in CO?
lqso = 8d46
lqsoerr = 0.0*lqso
tsync = 30d6
time = 'sonic'                  ;# time scale for calculation of Pcav: sonic, buoy, refill
w_bh = 'yes'
sigma_bh = 'no'
bband_bh = 'no'
rband_bh = 'no'
kband_bh = 'no'
dust_bh  = 'no'
pvmethod = 'int'                ;# int or mid
shape = 'cylinder'              ;# ellipse or cylinder
dfrac = 1.0                     ;# fraction of distance to leading edge of cav to use for age calculation (0.0 < dfrac <= 1.0)
kna = 12.6                      ;# central entropy
knaerr = 2.9                    ;# cent ent err
z     = 0.441844                ;# cluster redshift
MB    = -22.68                  ;# corr B-band absolute mag Hyperleda
MBerr = 0.511                   ;# err on MB
MR    = -23.81                  ;# corr R-band absolute mag SDSS
MRerr = 0.013                   ;# err on MR
rlum  = 1.962d11
MK    = -27.88                  ;# corr K-band abs mag 2MASS
MKerr = 0.198                   ;# K-band err
tvir = 7.54                     ;# estimate of Tvir
tvirerr = 1.76                  ;# err on Tvir
nelec = 4.10844d-2              ;# electron dens @ mid-radius of bubble
nelecerr = 3.26302d-3           ;# err on nelec
kt = 4.77                       ;# temp @ mid-radius of bubble
kterr = 0.35                    ;# err on kT
nelec0 = 9.18824d-02            ;# central electron dens
nelec0err = 8.86557d-03         ;# err on central nelec
kt0 = 3.76                      ;# central temp
kt0err = 0.30                   ;# err on central kT
eff = 0.1                       ;# assumed efficieny of converting mass to energy
gamma1 = (4./3.)                ;# assumed adiba const for cav contents
gamma2 = (5./3.)                ;# assumed adiba const for ICM
mu = 0.597                      ;# assumed mean molecular weight of ICM
drag = 0.75                     ;# assumed drag coefficient from Churazov 2001
cosmology, z, result, /silent   ;# calc cosmology
ang = result[4]                 ;# get D_A in kpc/"

IF shape EQ 'ellipse' THEN BEGIN
;##########
;# ellipses
;# NW cav
   a1 = ang*(5.1*0.492)
   b1 = ang*(7.1*0.492)
   c1 = ang*(5.1*0.492)
   dist1 = ang*(10.2*0.492)
   da1 = 0.1*a1
   db1 = 0.1*b1
   dc1 = 0.1*c1
;# SE cav
   a2 = ang*(4.7*0.492)
   b2 = ang*(6.2*0.492)
   c2 = ang*(4.7*0.492)
   dist2 = ang*(15.7*0.492)
   da2 = 0.1*a2
   db2 = 0.1*b2
   dc2 = 0.1*c2
;##########
ENDIF

IF shape EQ 'cylinder' THEN BEGIN
;##########
;# cylinders
;# NW
   r1 = ang*(0.5*4.54*0.492)
   l1 = ang*(20.7*0.492)
   dist1 = l1*dfrac
   dr1 = 0.1*r1
   dl1 = 0.1*l1
;# SE
   r2 = ang*(0.5*4.83*0.492)
   l2 = ang*(22.7*0.492)
   dist2 = l2*dfrac
   dr2 = 0.1*r2
   dl2 = 0.1*l2
;##########
ENDIF

;#################################
;#################################
;# Main Program
;#################################
;#################################

;# physical constants
secyr = 3.1556926d7             ;# seconds in a year
cmkpc = 3.08568025d21           ;# cm per kpc
ergkeV = 1.60217646d-9          ;# erg per keV
mh = 1.007825*1.660538782d-24   ;# mass hydrogen ATOM in g
mp = 1.67262158d-24             ;# mass of PROTON in g
c = 2.99792458d10               ;# speed of light in cm/s
msun = 1.9891d33                ;# mass of sun in g
lsun = 3.839d33                 ;# luminosity of sun in erg/s
unvG = 6.672d-8                 ;# universal grav const in cm^3/g/s^2

;# announce shape
print, '**********'
print, '**********'
print, ''
print, 'Assuming the cavities are ',shape
print, 'Using pV method of ',pvmethod
print, ''
print, '**********'
print, '**********'

;# convert temps to erg
kt = kt*ergkeV
kterr = kterr*ergkeV

;# take a stab at gal vel disp using Xue and Wu 2000
gvdisp = 10.^(2.51)*tvir^0.61
gvdisperr = gvdisp*sqrt((0.01/2.51)^2.+(tvirerr/tvir)^2.+(0.01/0.61)^2.)
print, FORMAT='(A-30,I10,A6,I10,A12)', 'Est. gal vel disp:', gvdisp, '+/-', gvdisperr, 'km/s'

;# take a stab at the stellar vel disp using Faber-Jackson
svdisp = 10.0^(-0.1*MB+0.2)
svdisperr = svdisp*sqrt((MBerr/MB)^2.)
print, FORMAT='(A-30,I10,A6,I10,A12)', 'Est. stell vel disp:', svdisp, '+/-', svdisperr, 'km/s'

;# take a stab at black hole masses
vmbh = (10.^(8.13+4.02*alog10(svdisp/200.)))/1d9
vmbherr = vmbh*sqrt((0.06/8.13)^2.+(svdisperr/svdisp)^2.+(0.32/4.02)^2.)
print, FORMAT='(A-30,F10.2,A6,F10.2,A12)', 'Est. M_BH(sigma):', vmbh, '+/-', vmbherr, '10**9 M_sol'

bmbh = (10.^(-0.40*(MB+19.5)+8.27))/1d9
bmbherr = bmbh*sqrt((0.05/0.4)^2.+(0.08/8.27)^2.+(MBerr/MB)^2.)
print, FORMAT='(A-30,F10.2,A6,F10.2,A12)', 'Est. M_BH(M_B):', bmbh, '+/-', bmbherr, '10**9 M_sol'

rmbh = (10.^(-0.38*(MR+21)+8.12))/1d9
rmbherr = rmbh*sqrt((0.04/0.38)^2.+(0.08/8.12)^2.+(MRerr/MR)^2.)
print, FORMAT='(A-30,F10.2,A6,F10.2,A12)', 'Est. M_BH(M_R):', rmbh, '+/-', rmbherr, '10**9 M_sol'

mkmbh = (10.^(-0.33*(MK+24)+8.33))/1d9
mkmbherr = mkmbh*sqrt((0.09/0.33)^2.+(0.15/8.33)^2.+(MKerr/MK)^2.)
print, FORMAT='(A-30,F10.2,A6,F10.2,A12)', 'Est. M_BH(M_K):', mkmbh, '+/-', mkmbherr, '10**9 M_sol'

dustmbh = (10.^(-0.30*(MR+21)+7.96))/1d9
dustmbherr = dustmbh*sqrt((0.06/0.3)^2.+(0.1/7.96)^2.+(MRerr/MR)^2.)
print, FORMAT='(A-30,F10.2,A6,F10.2,A12)', 'Est. M_BH(M_R-dust):', dustmbh, '+/-', dustmbherr, '10**9 M_sol'

smbh = [vmbh, bmbh, rmbh, mkmbh, dustmbh]
smbherr = [vmbherr, bmbherr, rmbherr, mkmbherr, dustmbherr]
weights = 1./smbherr
wmbh = wtd_mean(smbh,weights)
wmbherr = sqrt(1.0/total(weights))
mmbh = mean(smbh)
smmbh = stddev(smbh)
print, FORMAT='(A-30,F10.2,A6,F10.2,A12)', 'Weighted SMBH:', wmbh, '+/-', wmbherr, '1d9 Msol'
print, FORMAT='(A-30,F10.2,A6,F10.2,A12)', 'Mean SMBH:', mmbh, '+/-', smmbh, '1d9 Msol'
IF sigma_bh EQ "yes" THEN BEGIN
   fidmbh = vmbh
   fidmbherr = vmbherr
   mname = '*** USING SIGMA_MBH ***'
ENDIF
IF bband_bh EQ "yes" THEN BEGIN
   fidmbh = bmbh
   fidmbherr = bmbherr
   mname = '*** USING BBAND_MBH ***'
ENDIF
IF rband_bh EQ "yes" THEN BEGIN
   fidmbh = rmbh
   fidmbherr = rmbherr
   mname = '*** USING RBAND_MBH ***'
ENDIF
IF kband_bh EQ "yes" THEN BEGIN
   fidmbh = mkmbh
   fidmbherr = mkmbherr
   mname = '*** USING KBAND_MBH ***'
ENDIF
IF dust_bh EQ "yes" THEN BEGIN
   fidmbh = dustmbh
   fidmbherr = dustmbherr
   mname = '*** USING DUSTK_MBH ***'
ENDIF
IF w_bh EQ "yes" THEN BEGIN
   fidmbh = wmbh
   fidmbherr = wmbherr
   mname = '*** USING WEIGHTED_MBH ***'
ENDIF
print, 'Max Mbh:', max(smbh+smbherr)
print, 'Min Mbh:', min(smbh-smbherr)
fidmbhlo = fidmbh-min(smbh-smbherr)
fidmbhhi = max(smbh+smbherr)-fidmbh
print, FORMAT='(A-30,F10.2,A4,F5.2,A2,F5.2,A12)', mname, fidmbh, '+', fidmbhhi, '-', fidmbhlo, '1d9 Msol'

;# calculate total gas pressure
p = kt*nelec
pran = p*sqrt((kterr/kt)^2.+(nelecerr/nelec)^2.)
psys = p*sqrt((0.1)^2.+(0.1)^2.)
perr = sqrt(pran^2.+psys^2.)
print, FORMAT='(A-30,E10.2,A6,E10.2,A12)', 'Ambient pressure:', p, '+/-', perr, 'erg/cm**3'

;# calc the volumes
IF (shape EQ 'ellipse') THEN BEGIN
   vol1 = (4.*!pi*a1*b1*c1)/3.
   vol1err = vol1*sqrt((da1/a1)^2.+(db1/b1)^2.+(dc1/c1)^2.)
   vol2 = (4.*!pi*a2*b2*c2)/3.
   vol2err = vol2*sqrt((da2/a2)^2.+(db2/b2)^2.+(dc2/c2)^2.)
   cs1 = !pi*b1*c1
   cs2 = !pi*b2*c2
   rcav1 = sqrt(a1*b1)
   rcav2 = sqrt(a2*b2)
ENDIF
IF ((shape EQ 'cylinder') OR (shape EQ 'amorph')) THEN BEGIN
   vol1 = !pi*r1^2.*l1
   vol1err = vol1*sqrt((dr1/r1)^2.+(dl1/l1)^2.)
   vol2 = !pi*r2^2.*l2
   vol2err = vol2*sqrt((dr2/r2)^2.+(dl2/l2)^2.)
   cs1 = !pi*r1^2.
   cs1err = cs1*(dr1/r1)
   cs2 = !pi*r2^2.
   cs2err = cs2*(dr2/r2)
   rcav1 = sqrt(r1*l1)
   rcav1err = rcav1*sqrt((dr1/r1)^2.+(dl1/l1)^2.)
   rcav2 = sqrt(r2*l2)
   rcav2err = rcav2*sqrt((dr2/r2)^2.+(dl2/l2)^2.)
ENDIF   
dist1err = 0.1*dist1
dist2err = 0.1*dist2

;# print info
IF (shape EQ 'ellipse') THEN BEGIN
   print, FORMAT='(A-30,F10.2,A6,F10.2,A6,F10.2,A6,F10.2)', 'bubble 1 -- a:', a1, 'b:', b1, 'c:', c1, 'dist:', dist1
   print, FORMAT='(A-30,F10.2,A6,F10.2,A6,F10.2,A6,F10.2)', 'bubble 2 -- a:', a2, 'b:', b2, 'c:', c2, 'dist:', dist2
ENDIF ELSE BEGIN
   print, FORMAT='(A-30,F10.2,A6,F10.2,A6,F10.2)', 'bubble 1 -- r:', r1, 'l:', l1, 'dist:', dist1
   print, FORMAT='(A-30,F10.2,A6,F10.2,A6,F10.2)', 'bubble 2 -- r:', r2, 'l:', l2, 'dist:', dist2
ENDELSE
print, FORMAT='(A-30,I10,A6,I10,A12)', 'Volume bubble 1:', vol1, '+/-', vol1err, 'kpc**3'
print, FORMAT='(A-30,I10,A6,I10,A12)', 'Volume bubble 2:', vol2, '+/-', vol2err, 'kpc**3'
vol1 = vol1*cmkpc^3.
vol1err = vol1err*cmkpc^3.
vol2 = vol2*cmkpc^3.
vol2err = vol2err*cmkpc^3.
;print, FORMAT='(A-30,I10,A6,I10,A12)', 'Cross-sec bubble 1:', cs1, '+/-', cs1err, 'kpc**2'
;print, FORMAT='(A-30,I10,A6,I10,A12)', 'Cross-sec bubble 2:', cs2, '+/-', cs2err, 'kpc**2'
cs1 = cs1*cmkpc^2.
cs1err = cs1err*cmkpc^2.
cs2 = cs2*cmkpc^2.
cs2err = cs2err*cmkpc^2.
;print, FORMAT='(A-30,I10,A6,F10.2,A12)', 'Eff. radius bubble 1:', rcav1, '+/-', rcav1err, 'kpc'
;print, FORMAT='(A-30,I10,A6,F10.2,A12)', 'Eff. radius bubble 2:', rcav2, '+/-', rcav2err, 'kpc'
rcav1 = rcav1*cmkpc
rcav1err = rcav1err*cmkpc
rcav2 = rcav2*cmkpc
rcav2err = rcav2err*cmkpc
print, FORMAT='(A-30,I10,A6,F10.2,A12)', 'distance bubble 1:', dist1, '+/-', dist1err, 'kpc'
print, FORMAT='(A-30,I10,A6,F10.2,A12)', 'distance bubble 2:', dist2, '+/-', dist2err, 'kpc'
dist1 = dist1*cmkpc
dist1err = dist1err*cmkpc
dist2 = dist2*cmkpc
dist2err = dist2err*cmkpc

;# calculate sound speed age
tcs1 = dist1/(sqrt((gamma2*kt)/(mu*mh)))
ran = tcs1*sqrt((0.5*(kterr/kt)^2.+(dist1err/dist1)^2.))
sys = 0.1*tcs1
tcs1err = sqrt(ran^2.+sys^2.)
tcs2 = dist2/(sqrt((gamma2*kt)/(mu*mh)))
ran = tcs2*sqrt((0.5*(kterr/kt)^2.+(dist2err/dist2)^2.))
sys = 0.1*tcs2
tcs2err = sqrt(ran^2.+sys^2.)
gascs = (sqrt((gamma2*kt)/(mu*mh)))
print, FORMAT='(A-30,F10.2,A6,F10.2,A12)', 't_sonic bubble 1:', tcs1/secyr/1d6, '+/-', tcs1err/secyr/1d6, 'Myr'
print, FORMAT='(A-30,F10.2,A6,F10.2,A12)', 't_sonic bubble 2:', tcs2/secyr/1d6, '+/-', tcs2err/secyr/1d6, 'Myr'

;# calculate buoyant age
g1 = 2.0*(svdisp*1d5)^2./dist1
g1err = g1*sqrt(2.0*(svdisperr/svdisp)^2.+(dist1err/dist1)^2.)
tb1 = dist1*sqrt((cs1*drag)/(2.0*g1*vol1))
tb1err = tb1*sqrt(0.5*(cs1err/cs1)^2.+0.5*(vol1err/vol1)^2.)
g2 = 2.0*(svdisp*1d5)^2./dist2
g2err = g2*sqrt(2.0*(svdisperr/svdisp)^2.+(dist2err/dist2)^2.)
tb2 = dist2*sqrt((cs2*drag)/(2.0*g2*vol2))
tb2err = tb2*sqrt(0.5*(cs2err/cs2)^2.+0.5*(vol2err/vol2)^2.)
print, FORMAT='(A-30,F10.2,A6,F10.2,A12)', 't_buoy bubble 1:', tb1/secyr/1d6, '+/-', tb1err/secyr/1d6, 'Myr'
print, FORMAT='(A-30,F10.2,A6,F10.2,A12)', 't_buoy bubble 2:', tb2/secyr/1d6, '+/-', tb2err/secyr/1d6, 'Myr'

;# calculate refilling age
tr1 = 2.0*sqrt(rcav1/g1)
tr1err = tr1*sqrt(0.5*(rcav1err/rcav1)^2.+0.5*(g1err/g1)^2.)
tr2 = 2.0*sqrt(rcav2/g2)
tr2err = tr2*sqrt(0.5*(rcav2err/rcav2)^2.+0.5*(g2err/g2)^2.)
print, FORMAT='(A-30,F10.2,A6,F10.2,A12)', 't_refill bubble 1:', tr1/secyr/1d6, '+/-', tr1err/secyr/1d6, 'Myr'
print, FORMAT='(A-30,F10.2,A6,F10.2,A12)', 't_refill bubble 2:', tr2/secyr/1d6, '+/-', tr2err/secyr/1d6, 'Myr'

;# decide which time to use
IF time EQ 'sonic' THEN BEGIN
   tcav1 = tcs1
   tcav1err = tcs1err
   tcav2 = tcs2
   tcav2err = tcs2err
   mname = '*** USING SOUND SPEED AGE ***'
ENDIF ELSE IF time EQ 'buoy' THEN BEGIN
   tcav1 = tb1
   tcav1err = tb1err
   tcav2 = tb2
   tcav2err = tb2err
   mname = '*** USING BUOYANCY AGE ***'
ENDIF ELSE IF time EQ 'refill' THEN BEGIN
   tcav1 = tr1
   tcav1err = tr1err
   tcav2 = tr2
   tcav2err = tr2err
   mname = '*** USING REFILLING AGE ***'
ENDIF ELSE BEGIN
   print, '## ERROR: You gave me a bad time designation, pick sound, buoy, or refill'
   RETURN
ENDELSE
print, mname
tcav = mean([tcav1,tcav2])
tcaverr = stddev([tcav1,tcav2])
print, FORMAT='(A-30,F10.2,A6,F10.2,A12)', 'Mean t_cav:', tcav/secyr/1d6, '+/-', tcaverr/secyr/1d6, 'Myr'

;# calculate pV
IF pvmethod EQ 'mid' THEN BEGIN
   pv1 = p*vol1
   pv1err = pv1*sqrt((perr/p)^2.+(vol1err/vol1)^2.)
   pv2 = p*vol2
   pv2err = pv2*sqrt((perr/p)^2.+(vol2err/vol2)^2.)
   print, FORMAT='(A-30,E10.2,A6,E10.2,A12)', 'pV bubble 1:', pv1, '+/-', pv1err, 'erg'
   print, FORMAT='(A-30,E10.2,A6,E10.2,A12)', 'pV bubble 2:', pv2, '+/-', pv2err, 'erg'
ENDIF ELSE IF pvmethod EQ 'int' THEN BEGIN
   dataobs = read_ascii(pfile, template = s_tabletemplate)
   rmean = ((dataobs.rin_mpc + dataobs.rout_mpc)/2.)*1000.
   rmlo  = rmean-(dataobs.rin_mpc*1000.)
   rmhi  = (dataobs.rout_mpc*1000.)-rmean
   p     = dataobs.p*ergkev
   perr  = dataobs.p_err*ergkev
   ord   = where((p EQ p) AND (p GT 0.))
   rp    = rmean[ord]*cmkpc
   rplo  = rmlo[ord]*cmkpc
   rphi  = rmhi[ord]*cmkpc
   p     = p[ord]
   perr  = perr[ord]
   rint  = maken(min(rplo+rp),max(rphi+rp),1000)
   pint  = interpol(p, rp, rint)
   perrint = interpol(perr, rp, rint)
   rin  = 0.0*cmkpc
   rout = l1*cmkpc
   r    = r1*cmkpc
   pv1  = 0.0
   pv1err = 0.0
   FOR i=0,n_elements(rint)-1 DO BEGIN
      IF ((rint[i] LT rin) OR (rint[i] GT rout)) THEN GOTO,SKIP1
      pm = (pint[i]+pint[i+1])/2.0
      pmerr = (perrint[i]+perrint[i+1])/2.0
      vm = (!PI*r^2.*(rint[i+1]-rint[i]))
      pv1 = pv1+(pm*vm)
      pv1err = pv1err+(pmerr*vm)
      SKIP1:
   ENDFOR
   print, FORMAT='(A-30,E10.2,A6,E10.2,A12)', 'pV bubble 1:', pv1, '+/-', pv1err, 'erg'
   rout = l2*cmkpc
   r = r2*cmkpc
   pv2 = 0.0
   pv2err = 0.0
   FOR i=0,n_elements(rint)-1 DO BEGIN
      IF ((rint[i] LT rin) OR (rint[i] GT rout)) THEN GOTO,SKIP2
      pm = (pint[i]+pint[i+1])/2.0
      pmerr = (perrint[i]+perrint[i+1])/2.0
      vm = (!PI*r^2.*(rint[i+1]-rint[i]))
      pv2 = pv2+(pm*vm)
      pv2err = pv2err+(pmerr*vm)
      SKIP2:
   ENDFOR
   print, FORMAT='(A-30,E10.2,A6,E10.2,A12)', 'pV bubble 2:', pv2, '+/-', pv2err, 'erg'
ENDIF

;# calc energy
ecav1 = (gamma1/(gamma1-1.))*pv1
ecav1err = ecav1*(pv1err/pv1)
ecav2 = (gamma1/(gamma1-1.))*pv2
ecav2err = ecav2*(pv2err/pv2)
print, FORMAT='(A-30,E10.2,A6,E10.2,A12)', 'Ecav bubble 1:', ecav1, '+/-', ecav1err, 'erg'
print, FORMAT='(A-30,E10.2,A6,E10.2,A12)', 'Ecav bubble 2:', ecav2, '+/-', ecav2err, 'erg'
ecav = ecav1+ecav2
ecaverr = ecav*sqrt((ecav1err/ecav1)^2.+(ecav2err/ecav2)^2.)
print, FORMAT='(A-30,E10.2,A6,E10.2,A12)', 'Ecav total:', ecav, '+/-', ecaverr, 'erg'

;# calc Pcav
pcav1 = ecav1/tcav1
pcav1err = pcav1*sqrt((ecav1err/ecav1)^2.+(tcav1err/tcav1)^2.)
pcav2 = ecav2/tcav2
pcav2err = pcav2*sqrt((ecav2err/ecav2)^2.+(tcav2err/tcav2)^2.)
print, FORMAT='(A-30,E10.2,A6,E10.2,A12)', 'Pcav bubble 1:', pcav1, '+/-', pcav1err, 'erg/sec'
print, FORMAT='(A-30,E10.2,A6,E10.2,A12)', 'Pcav bubble 2:', pcav2, '+/-', pcav2err, 'erg/sec'
pcav = mean([pcav1,pcav2])
pcaverr = stddev([pcav1,pcav2])
tpcav = total([pcav1,pcav2])
tpcaverr = tpcav*sqrt((pcav1err/pcav1)^2.+(pcav2err/pcav2)^2.)
print, FORMAT='(A-30,E10.2,A6,E10.2,A12)', 'Mean Pcav:', pcav, '+/-', pcaverr, 'erg/sec'
print, FORMAT='(A-30,E10.2,A6,E10.2,A12)', 'Total Pcav:', tpcav, '+/-', tpcaverr, 'erg/sec'

;# if t_sonic = some other age
testcs = dist1/(tsync*secyr)
M = testcs/gascs
testpcav = ecav/(tsync*secyr)
mtestpcav = testpcav*M^3.
print, FORMAT='(A-30,F10.2,A28)', 'tsync:', tsync/1d6, 'Myr'
print, FORMAT='(A-30,E10.2,A28)', 'cs(tcs = tsync):', testcs, 'cm/s'
print, FORMAT='(A-30,E10.2,A28)', 'ICM cs:', gascs, 'cm/s'
print, FORMAT='(A-30,F10.2)', 'Mach:', M
print, FORMAT='(A-30,E10.2,A28)', 'Pcav(tcs = tsync):', testpcav, 'erg/sec'
print, FORMAT='(A-30,E10.2,A28)', 'Pcav(M & tsync):', mtestpcav, 'erg/sec'
print, FORMAT='(A-30,E10.2,A28)', 'Ecav(M & tsync):', mtestpcav*tsync*secyr, 'erg'

;# calc mass accretion
macc = ecav/(eff*c^2.)/msun
maccerr = macc*(ecaverr/ecav)
print, FORMAT='(A-30,E10.2,A6,E10.2,A12)', 'Total accreted mass:', macc, '+/-', maccerr, 'Msol'

;# calc mass accretion rate
dmacc = macc/(tcav/secyr)
dmaccerr = dmacc*(pcaverr/pcav)
print, FORMAT='(A-30,F10.3,A6,F10.3,A12)', 'Mean mass accret rate:', dmacc, '+/-', dmaccerr, 'Msol/yr'

;# calc black hole mass change
dmbh = (1.0-eff)*macc
dmbherr = (1.0-eff)*maccerr
print, FORMAT='(A-30,E10.2,A6,E10.2,A12)', 'Delta(M_BH) total:', dmbh, '+/-', dmbherr, 'Msol'

;# calc black hole mass rate of change
ddmbh = (1.0-eff)*dmacc
ddmbherr = ddmbh*(dmaccerr/dmacc)
print, FORMAT='(A-30,F10.3,A6,F10.3,A12)', 'dot-Delta(M_BH) total:', ddmbh, '+/-', ddmbherr, 'Msol/yr'

;# calc frac mbh change
;mbhrat = dmbh/((fidmbh*1d9)-dmbh)*100.
;mbhraterr = mbhrat*sqrt((dmbherr/dmbh)^2.+(fidmbherr/fidmbh)^2.)
;print, FORMAT='(A-30,F10.2,A6,F10.2,A12)', 'BH grew by:', mbhrat, '+/-', mbhraterr, '%'
mbhrat = dmbh/((fidmbh*1d9)-dmbh)*100.
mbhratlo = mbhrat*sqrt((dmbherr/dmbh)^2.+(fidmbhlo/fidmbh)^2.)
mbhrathi = mbhrat*sqrt((dmbherr/dmbh)^2.+(fidmbhhi/fidmbh)^2.)
print, FORMAT='(A-30,F10.2,A6,F10.2,F10.2,A12)', 'BH grew by:', mbhrat, '+/-', mbhratlo, mbhrathi, '%'

;# calc Eddington accretion rate
;medd = 2.2*fidmbh/eff
;medderr = 2.2*fidmbherr/eff
;eddrat = dmacc/medd
;eddraterr = eddrat*sqrt((dmaccerr/dmacc)^2.+(medderr/medd)^2.)
;print, FORMAT='(A-30,F10.2,A6,F10.2,A12)', 'Eddington Macc:', medd, '+/-', medderr, 'Msol/yr'
;print, FORMAT='(A-30,F10.4,A6,F10.4,A12)', 'Eddington Ratio:', eddrat, '+/-', eddraterr, ''
medd = 2.2*fidmbh/eff
meddlo = medd*(fidmbhlo/fidmbh)
meddhi = medd*(fidmbhhi/fidmbh)
eddrat = dmacc/medd
eddrathi = dmacc/(medd-meddlo)
eddratlo = dmacc/(medd+meddhi)
print, FORMAT='(A-30,F10.2,A6,F10.2,F10.2,A12)', 'Eddington Macc:', medd, '+/-', meddlo, meddhi, 'Msol/yr'
print, FORMAT='(A-30,F10.4,A6,F10.4,F10.4,A12)', 'Eddington Ratio:', eddrat, '+/-', eddratlo, eddrathi, '--'

;# calc Bondi accretion rate
mbon = 0.013*(kt0)^(-3./2.)*nelec0*fidmbh^2.
;mbonerr = mbon*sqrt((nelec0err/nelec0)^2.+(3./2.)*(kt0err/kt0)^2.+2.*(fidmbherr/fidmbh)^2.)
mbonlo = mbon*sqrt((nelec0err/nelec0)^2.+(3./2.)*(kt0err/kt0)^2.+2.*(fidmbhlo/fidmbh)^2.)
mbonhi = mbon*sqrt((nelec0err/nelec0)^2.+(3./2.)*(kt0err/kt0)^2.+2.*(fidmbhhi/fidmbh)^2.)
mbonent = 0.013*kna^(-3./2.)*fidmbh^2.
;mbonenterr = mbonent*sqrt((3./2.)*(knaerr/kna)^2.+2.*(fidmbherr/fidmbh)^2.)
mbonentlo = mbonent*sqrt((3./2.)*(knaerr/kna)^2.+2.*(fidmbhlo/fidmbh)^2.)
mbonenthi = mbonent*sqrt((3./2.)*(knaerr/kna)^2.+2.*(fidmbhhi/fidmbh)^2.)
conbon = (2.0*unvG*mu*mh*1d9*msun)/(gamma2*ergkeV*cmkpc)*1000.
rbon = conbon*kt0^(-1.0)*fidmbh
rbonerr = rbon*sqrt((fidmbherr/fidmbh)^2.+(kt0err/kt0)^2.)
bonrat = dmacc/mbonent
;bonraterr = dmaccerr/mbonerr
bonratlo = bonrat+(dmacc/(mbonent-mbonentlo))
bonrathi = bonrat-(dmacc/(mbonent+mbonenthi))
;print, FORMAT='(A-30,E10.2,A6,E10.2,A12)', 'Bondi Macc(rho0,T0):', mbon, '+/-', mbonerr, 'Msol/yr'
;print, FORMAT='(A-30,E10.2,A6,E10.2,A12)', 'Bondi Macc(K0):', mbonent, '+/-', mbonenterr, 'Msol/yr'
print, FORMAT='(A-30,E10.2,A6,E10.2,E10.2,A12)', 'Bondi Macc(rho0,T0):', mbon, '+/-', mbonlo, mbonhi, 'Msol/yr'
print, FORMAT='(A-30,E10.2,A6,E10.2,E10.2,A12)', 'Bondi Macc(K0):', mbonent, '+/-', mbonentlo, mbonenthi, 'Msol/yr'
print, FORMAT='(A-30,E10.2,A6,E10.2,E10.2,A12)', 'Bondi Ratio:', bonrat, '+/-', bonrathi, bonratlo, '--'
print, FORMAT='(A-30,F10.1,A6,F10.1,A12)', 'Bondi Radius:', rbon, '+/-', rbonerr, 'pc'

;# kbon needed for macc
dummbh = fidmbh+fidmbhhi
needk0 = (dmacc/0.013/dummbh^2.)^(-2./3.)
tcool = 1d8*(needk0/10.)^(3./2.)*(tbon/5.)^(-1.)/1d6
needne = (tbon/needk0)^(3./2.)
needr = ((mgas*2d33)/(4./3.*!pi*0.4*1.92*0.62*1.7d-24))^(1./3.)/cmkpc
print, FORMAT='(A-30,F10.2,A28)', 'For Mbh:', dummbh, '1d9 Msol'
print, FORMAT='(A-30,F10.2,A28)', 'For Tbon:', tbon, 'keV'
print, FORMAT='(A-30,E10.2,A28)', 'and M_gas:', mgas, 'Msol'
print,''
print, FORMAT='(A-30,F10.2,A28)', 'Need KBon:', needk0, 'keV cm^2'
print, FORMAT='(A-30,F10.2,A28)', 'Need nelec:', needne, 'e/cm^3'
print, FORMAT='(A-30,F10.2,A28)', 'Need r:', needr, 'kpc'
print, FORMAT='(A-30,F10.2,A28)', 'Implied tcool:', tcool, 'Myr'

;# calc qso numbers
qsomacc = lqso/(0.1*c^2.)/msun*secyr
qsomaccerr = qsomacc*sqrt((lqsoerr/lqso)^2.)
effedd = lqso/(1.38d38*fidmbh*1d9)
;effedderr = effedd*sqrt((lqsoerr/lqso)^2.+(fidmbherr/fidmbh)^2.)
effeddlo = effedd*sqrt((lqsoerr/lqso)^2.+(fidmbhlo/fidmbh)^2.)
effeddhi = effedd*sqrt((lqsoerr/lqso)^2.+(fidmbhhi/fidmbh)^2.)
print, FORMAT='(A-30,E10.2,A6,E10.2,A12)', 'L_QSO:', lqso, '+/-', lqsoerr, 'erg/s'
print, FORMAT='(A-30,E10.2,A6,E10.2,A12)', 'QSO Macc:', qsomacc, '+/-', qsomaccerr, 'M_sol/yr'
print, FORMAT='(A-30,E10.2,A28)', 'QSO Edd:', qsomacc/medd, '--'
print, FORMAT='(A-30,E10.2,A6,E10.2,E10.2,A12)', 'Eff Edd:', effedd, '+/-', effeddlo, effeddhi, '--'

;# calc bulge mass
mbul = rlum*6.3
print, FORMAT='(A-30,E10.2,A6,A10,A12)', 'M_bulge:', mbul, '', '', 'Msol'

;# create a tex table part 1
IF (shape EQ 'ellipse') THEN BEGIN
   outtex = 'ellcavities'
   title = 'Ellipsoidal Cavity Properties.'
   openw, /get_lun, lun, outtex+'.tex'
   printf, lun, '\begin{landscape}'
   printf, lun, '\begin{deluxetable}{cccccccccccccccc}'
   printf, lun, '\tabletypesize{\scriptsize}'
   printf, lun, '\tablecaption{'+title+'\label{tab:'+outtex+'}}'
   printf, lun, '\tablewidth{0pt}'
   printf, lun, '\tablehead{'
   printf, lun, $
;           '\colhead{$a$} &', $
;           ' \colhead{$b$} &', $
;           ' \colhead{$c$} &', $
           ' \colhead{$D$} &', $
           ' \colhead{$t_\mathrm{{sonic}}$} &', $
           ' \colhead{$t_\mathrm{{buoyant}}$} &', $
           ' \colhead{$t_\mathrm{{refill}}$} &', $
           ' \colhead{$pV$} &', $
           ' \colhead{$H_\mathrm{{cav}}$} &', $
           ' \colhead{$P_\mathrm{{cav}}$} &', $
;           ' \colhead{$M_{BH,\sigma}$} &', $
;           ' \colhead{$M_{BH,L_K}$} &', $
;           ' \colhead{$\Delta M_{BH}$} &', $
;           ' \colhead{$\dot{M}_{BH}$} &', $
;           ' \colhead{Bondi Ratio} &', $
;           ' \colhead{Edd. Ratio}\\'
   printf, lun, $
;           '\colhead{kpc} &', $
;           ' \colhead{kpc} &', $
;           ' \colhead{kpc} &', $
           ' \colhead{kpc} &', $
           ' \colhead{$10^7$ yr} &', $
           ' \colhead{$10^7$ yr} &', $
           ' \colhead{$10^7$ yr} &', $
           ' \colhead{$10^{59}$ erg} &', $
           ' \colhead{$10^{60}$ erg} &', $
           ' \colhead{$10^{45}$ erg s$^{-1}$} &', $
           ' \colhead{$10^9 \Msol$} &', $
           ' \colhead{$10^9 \Msol$} &', $
           ' \colhead{$10^6 \Msol$} &', $
           ' \colhead{$\Msol \pyr$} &', $
           ' \colhead{$10^3$} &', $
           ' \colhead{$10^{-3}$}\\'
   printf, lun, $
           '\colhead{(1)} &', $
           ' \colhead{(2)} &', $
           ' \colhead{(3)} &', $
           ' \colhead{(4)} &', $
           ' \colhead{(5)} &', $
           ' \colhead{(6)} &', $
           ' \colhead{(7)} &', $
           ' \colhead{(8)} &', $
           ' \colhead{(9)} &', $
           ' \colhead{(10)} &', $
           ' \colhead{(11)} &', $
           ' \colhead{(12)} &', $
           ' \colhead{(13)} &', $
           ' \colhead{(14)} &', $
           ' \colhead{(15)} &', $
           ' \colhead{(16)}}'
ENDIF ELSE BEGIN
   outtex = 'cylcavities'
   title = 'Cylindrical Cavity Properties.'
   openw, /get_lun, lun, outtex+'.tex'
;   printf, lun, '\begin{deluxetable}{lccccccccccccccc}'
   printf, lun, '\begin{deluxetable}{lccccccccc}'
   printf, lun, '\tabletypesize{\scriptsize}'
   printf, lun, '\tablecaption{'+title+'\label{tab:'+outtex+'}}'
   printf, lun, '\tablewidth{0pt}'
   printf, lun, '\tablehead{'
   printf, lun, $
           '\colhead{ID} &', $
           ' \colhead{$r$} &', $
           ' \colhead{$l$} &', $
           ' \colhead{$D$} &', $
           ' \colhead{$t_\mathrm{{sonic}}$} &', $
           ' \colhead{$t_\mathrm{{buoyant}}$} &', $
           ' \colhead{$t_\mathrm{{refill}}$} &', $
           ' \colhead{$pV$} &', $
           ' \colhead{$H_\mathrm{{cav}}$} &', $
           ' \colhead{$P_\mathrm{{cav}}$}\\'
;           ' \colhead{$M_{BH,\sigma}$} &', $
;           ' \colhead{$M_{BH,L_K}$} &', $
;           ' \colhead{$\Delta M_{BH}$} &', $
;           ' \colhead{$\dot{M}_{BH}$} &', $
;           ' \colhead{Bondi Ratio} &', $
;           ' \colhead{Edd. Ratio}\\'
   printf, lun, $
           '\colhead{--} &', $
           ' \colhead{kpc} &', $
           ' \colhead{kpc} &', $
           ' \colhead{kpc} &', $
           ' \colhead{$10^7$ yr} &', $
           ' \colhead{$10^7$ yr} &', $
           ' \colhead{$10^7$ yr} &', $
           ' \colhead{$10^{58}$ erg} &', $
           ' \colhead{$10^{59}$ erg} &', $
           ' \colhead{$10^{44}$ erg s$^{-1}$}\\'
;           ' \colhead{$10^9 \Msol$} &', $
;           ' \colhead{$10^9 \Msol$} &', $
;           ' \colhead{$10^6 \Msol$} &', $
;           ' \colhead{$\Msol \pyr$} &', $
;           ' \colhead{$10^3$} &', $
;           ' \colhead{$10^{-3}$}\\'
   printf, lun, $
           '\colhead{(1)} &', $
           ' \colhead{(2)} &', $
           ' \colhead{(3)} &', $
           ' \colhead{(4)} &', $
           ' \colhead{(5)} &', $
           ' \colhead{(6)} &', $
           ' \colhead{(7)} &', $
           ' \colhead{(8)} &', $
           ' \colhead{(9)} &', $
           ' \colhead{(10)}}'
;           ' \colhead{(11)} &', $
;           ' \colhead{(12)} &', $
;           ' \colhead{(13)} &', $
;           ' \colhead{(14)} &', $
;           ' \colhead{(15)} &', $
;           ' \colhead{(16)}}
ENDELSE
printf, lun, '\startdata'
IF (shape EQ 'ellipse') THEN BEGIN
   out1 = [a1, b1, c1, dist1/cmkpc, tcs1/secyr/1.0d7, tb1/secyr/1.0d7, tr1/secyr/1.0d7, pv1/1.0d59, ecav1/1.0d60, pcav/1.0d44, vmbh, fidmbh, dmbh/1.0d6, ddmbh, bonrat/1.0d3, eddrat/1.0d-3]
   out1err = [da1, db1, dc1, dist1err/cmkpc, tcs1err/secyr/1.0d7, tb1err/secyr/1.0d7, tr1err/secyr/1.0d7, pv1err/1.0d59, ecav1err/1.0d60, pcaverr/1.0d44, vmbherr, fidmbherr, dmbherr/1.0d6, ddmbherr, bonraterr/1.0d3, eddraterr/1.0d-3]
ENDIF ELSE BEGIN
   out1 = ['NW', sigfig(r1,3), sigfig(l1,3), sigfig(dist1/cmkpc,3), sigfig(tcs1/secyr/1.0d7,3), sigfig(tb1/secyr/1.0d7,3), sigfig(tr1/secyr/1.0d7,3), sigfig(pv1/1.0d58,3), sigfig(ecav1/1.0d59,3), sigfig(pcav1/1.0d44,3)];, vmbh, fidmbh, dmbh/1.0d6, ddmbh, bonrat/1.0d3, eddrat/1.0d-3]
   out1err = ['', sigfig(dr1,3), sigfig(dl1,3), sigfig(dist1err/cmkpc,3), sigfig(tcs1err/secyr/1.0d7,3), sigfig(tb1err/secyr/1.0d7,3), sigfig(tr1err/secyr/1.0d7,3), sigfig(pv1err/1.0d58,3), sigfig(ecav1err/1.0d59,3), sigfig(pcav1err/1.0d44,3)];, vmbherr, fidmbherr, dmbherr/1.0d6, ddmbherr, bonraterr/1.0d3, eddraterr/1.0d-3]
ENDELSE
form = replicate('(A)',n_elements(out1))
FOR i=0,n_elements(out1)-1 DO BEGIN
   IF i NE 0 THEN printf, lun, format='(A)',' & '
   printf, lun, format='(A)', '${'
   printf, lun, format=form[i], out1[i]
   printf, lun, format='(A)', ' \pm '
   printf, lun, format=form[i], out1err[i]
   printf, lun, format='(A)', '}$'
ENDFOR
printf, lun, format='(A)','\\'
void, line
IF (shape EQ 'ellipse') THEN BEGIN
   out2 = [a2, b2, c2, dist2/cmkpc, tcs2/secyr/1.0d7, tb2/secyr/1.0d7, tr2/secyr/1.0d7, pv2/1.0d59, ecav2/1.0d60, pcav/1.0d44, vmbh, fidmbh, dmbh/1.0d6, ddmbh, bonrat/1.0d3, eddrat/1.0d-3]
   out2err = [da2, db2, dc2, dist2err/cmkpc, tcs2err/secyr/1.0d7, tb2err/secyr/1.0d7, tr2err/secyr/1.0d7, pv2err/1.0d59, ecav2err/1.0d60, pcaverr/1.0d44, vmbherr, fidmbherr, dmbherr/1.0d6, ddmbherr, bonraterr/1.0d3, eddraterr/1.0d-3]
ENDIF ELSE BEGIN
   out2 = ['SE', sigfig(r2,3), sigfig(l2,3), sigfig(dist2/cmkpc,3), sigfig(tcs2/secyr/1.0d7,3), sigfig(tb2/secyr/1.0d7,3), sigfig(tr2/secyr/1.0d7,3), sigfig(pv2/1.0d58,3), sigfig(ecav2/1.0d59,3), sigfig(pcav2/1.0d44,3)];, vmbh, fidmbh, dmbh/1.0d6, ddmbh, bonrat/1.0d3, eddrat/1.0d-3]
   out2err = ['', sigfig(dr2,3), sigfig(dl2,3), sigfig(dist2err/cmkpc,3), sigfig(tcs2err/secyr/1.0d7,3), sigfig(tb2err/secyr/1.0d7,3), sigfig(tr2err/secyr/1.0d7,3), sigfig(pv2err/1.0d58,3), sigfig(ecav2err/1.0d59,3), sigfig(pcav2err/1.0d44,3)];, vmbherr, fidmbherr, dmbherr/1.0d6, ddmbherr, bonraterr/1.0d3, eddraterr/1.0d-3]
ENDELSE
FOR i=0,n_elements(out2)-1 DO BEGIN
   IF i NE 0 THEN printf, lun, format='(A)',' & '
   printf, lun, format='(A)', '${'
   printf, lun, format=form[i], out2[i]
   printf, lun, format='(A)', ' \pm '
   printf, lun, format=form[i], out2err[i]
   printf, lun, format='(A)', '}$'
ENDFOR
printf, lun, format='(A)','\\'
printf, lun, '\enddata'
printf, lun, '\tablecomments{Col. (1) Cavity location; Col. (2) Radius of excavated cylinder; Col. (3) Length of excavated cylinder; Col. (4) Distance to leading edge of cavity; Col. (5) Sound speed age; Col. (6) Buoyant rise time age; Col. (7) Volume refilling age; Col. (8) $pV$ work; Col. (9) Cavity enthalpy; Col. (10) Cavity power using sonic age.}'
printf, lun, '\end{deluxetable}'
close, lun

END
