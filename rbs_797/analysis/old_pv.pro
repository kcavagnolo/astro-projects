PRO pv

;# set options
caverr = 0.25
myhome = GETENV('HOME')
pfile = myhome+'/research/rbs_797/data/2202_7902_table.dat'
txfile = myhome+'/research/rbs_797/data/tx_2.5K/proj_1T_nhfro.dat'
name = 'RBS_0797'
ctype = 'outer'                 ;# birzan, small, small2, small4, big, big2, big4, huge, huge2, outer
time = 'buoy'                   ;# time scale for calculation of Pcav
w_bh = 'yes'
sigma_bh = 'no'
bband_bh = 'no'
rband_bh = 'no'
kband_bh = 'no'
dust_bh  = 'no'
pvmethod = 'mid'                ;# int or mid
z = 0.354                       ;# cluster redshift
MB = -23.42                     ;# B-band absolute mag
MBerr = 0.13                    ;# err on MB
MR = -24.03                     ;# R-band absolute mag
MRerr = 0.28                    ;# err on MR
MK = -26.19                     ;# corr K-band mag from 2MASS
MKerr = 0.17                    ;# K-band err
tvir = 7.54                     ;# estimate of Tvir
tvirerr = 1.76                  ;# err on Tvir
eff = 0.1                       ;# assumed efficieny of converting mass to energy
drag = 0.75                     ;# assumed drag coefficient from Churazov 2001
nmc = 5000                      ;# number of MC sims
cosmology, z, result, /silent   ;# calc cosmology
ang = result[4]                 ;# get D_A in kpc/"
!quiet = 1

;# physical constants
gamma1 = (4./3.)                ;# assumed adiba const for cav contents
gamma2 = (5./3.)                ;# assumed adiba const for ICM
mu = 0.597                      ;# assumed mean molecular weight of ICM
secyr = 3.1556926d7             ;# seconds in a year
cmkpc = 3.08568025d21           ;# cm per kpc
ergkev = 1.60217646d-9          ;# erg per keV
mh = 1.007825*1.660538782d-24   ;# mass hydrogen ATOM in g
mp = 1.67262158d-24             ;# mass of PROTON in g
c = 2.99792458d10               ;# speed of light in cm/s
msun = 1.9891d33                ;# mass of sun in g
lsun = 3.839d33                 ;# luminosity of sun in erg/s
unvG = 6.672d-8                 ;# universal grav const in cm^3/g/s^2

;# cavity geometries
IF ctype EQ 'birzan' THEN BEGIN
   outtex = 'birzan.tex'
   a1 = 9.7
   b1 = 9.7
   c1 = 9.7
   d1 = 19.5
   a2 = 13.4
   b2 = 8.5
   c2 = 10.95
   d2 = 23.8
ENDIF
IF ((ctype EQ 'small') OR (ctype EQ 'small2') OR (ctype EQ 'small4')) THEN BEGIN
   outtex = 'insmall.tex'
   a1 = ang*(5.0901967*0.492)   ;# W cav
   b1 = ang*(4.1784994*0.492)
   d1 = ang*(7.70972*0.492)
   a2 = ang*(6.9907708*0.492)   ;# E cav
   b2 = ang*(4.3397831*0.492)
   d2 = ang*(10.2608*0.492)
ENDIF
IF ctype EQ 'small' THEN $
   outtex = 'insmall.tex'
IF ctype EQ 'small2' THEN BEGIN
   outtex = 'insmall2.tex'
   d1 = 2.0*d1
   d2 = 2.0*d2
ENDIF
IF ctype EQ 'small4' THEN BEGIN
   outtex = 'insmall4.tex'
   d1 = 4.0*d1
   d2 = 4.0*d2
ENDIF
IF ((ctype EQ 'big') OR (ctype EQ 'big2') OR (ctype EQ 'big4')) THEN BEGIN
   a1 = ang*(11.2*0.492)        ;# W cav
   b1 = ang*(7.4*0.492)
   d1 = ang*(11.7*0.492)
   a2 = ang*(11.741584*0.492)   ;# E cav
   b2 = ang*(8.5958821*0.492)
   d2 = ang*(9.4*0.492)
ENDIF
IF ctype EQ 'big' THEN $
   outtex = 'inbig.tex'
IF ctype EQ 'big2' THEN BEGIN
   outtex = 'inbig2.tex'
   d1 = 2.0*d1
   d2 = 2.0*d2
ENDIF
IF ctype EQ 'big4' THEN BEGIN
   outtex = 'inbig4.tex'
   d1 = 4.0*d1
   d2 = 4.0*d2
ENDIF
IF ((ctype EQ 'huge') OR (ctype EQ 'huge2') OR (ctype EQ 'huge4')) THEN BEGIN
   a1 = ang*(22.280819*0.492)        ;# W cav
   b1 = ang*(9.3208165*0.492)
   d1 = ang*(24.9261*0.492)
   a2 = a1
   b2 = b1
   d2 = d1
ENDIF
IF ctype EQ 'huge' THEN $
   outtex = 'huge.tex'
IF ctype EQ 'huge2' THEN BEGIN
   outtex = 'huge2.tex'
   d1 = 2.0*d1
   d2 = d1
ENDIF
IF ctype EQ 'huge4' THEN BEGIN
   outtex = 'huge4.tex'
   d1 = 4.0*d1
   d2 = d1
ENDIF
IF ctype EQ 'outer' THEN BEGIN
   outtex = 'outer.tex'
;   a1 = ang*(9.2468902*0.492)   ;# W
;   b1 = ang*(5.2482073*0.492)
;   d1 = ang*(48.0598*0.492)
;   a2 = ang*(9.5012195*0.492)   ;# E
;   b2 = ang*(7.5017439*0.492)
;   d2 = ang*(31.1778*0.492)
   a1 = 60.0
   b1 = 60.0
   c1 = b1
   d1 = 65.0+a1
   a2 = a1
   b2 = b1
   c2 = b1
   d2 = d1
ENDIF
da1 = 20.0/sqrt(a1)
db1 = 20.0/sqrt(b1)
dc1 = 20.0/sqrt(c1)
dd1 = 20.0/sqrt(d1)
da2 = 20.0/sqrt(a2)
db2 = 20.0/sqrt(b2)
dc2 = 20.0/sqrt(c2)
dd2 = 20.0/sqrt(d2)

;#################################
;#################################
;# Main Program
;#################################
;#################################

;# pressure get data
restore, myhome+'/research/redux/scripts/s_tabletemplate.sav'
dataobs = read_ascii(pfile, template = s_tabletemplate)
prin  = dataobs.rin_mpc*1000.
prout = dataobs.rout_mpc*1000.
nelec = dataobs.n_elec
nelecerr = dataobs.sigma_ne
p    = dataobs.p_flat
perr = dataobs.p_err
kna    = dataobs.k_flat
knaerr = dataobs.k_err
prin = reverse(prin)
prout = reverse(prout)
p = reverse(p)
perr = reverse(perr)
nelec = reverse(nelec)
nelecerr = reverse(nelecerr)
kna = reverse(kna)
knaerr = reverse(knaerr)
rp   = (0.5*(prin^(3./2.)+prout^(3./2.)))^(2./3.)
rplo = rp-prin
rphi = prout-rp
ord  = where((p EQ p) AND (p GT 0.))
rp   = rp[ord]*cmkpc
rplo = rplo[ord]*cmkpc
rphi = rphi[ord]*cmkpc
p    = p[ord]
perr = perr[ord]
p    = 2.4*p                    ;# calculate total gas pressure
pran = 2.4*perr                 ;# random error
psys = 0.1*p                    ;# systematic error
perr = sqrt(pran^2.+psys^2.)    ;# total error
p    = p*ergkev
perr = perr*ergkev
nelec0 = nelec[0]               ;# central electron dens
nelec0err = nelecerr[0]         ;# err on central nelec
kna = kna[0]                    ;# central entropy
knaerr = knaerr[0]              ;# err on central entropy

;# read spectral fits
restore, myhome+'/research/redux/scripts/xspectemp_rin_normerr_src.sav'
tfit  = read_ascii(txfile,template=xspectemp_rin_normerr_src)
trin  = tfit.rin[where(tfit.cluster EQ name)]*60.*ang
trout = tfit.rout[where(tfit.cluster EQ name)]*60.*ang
kt    = tfit.tx[where(tfit.cluster EQ name)]
kthi  = tfit.thi[where(tfit.cluster EQ name)]
ktlo  = tfit.tlo[where(tfit.cluster EQ name)]
FOR i=0,n_elements(kt)-1 DO $
   IF kthi[i]-kt[i] GT kt[i]-ktlo[i] THEN push, kterr, kthi[i]-kt[i] ELSE push, kterr, kt[i]-ktlo[i]
kt = kt*ergkev
kterr = kterr*ergkev

;# get central kt and mid kt
ord1 = where(trin GE d1)
ktmid1 = kt[ord1[0]-1]
ktmid1err = kterr[ord1[0]-1]
ord2 = where(trin GE d2)
ktmid2 = kt[ord2[0]-1]
ktmid2err = kterr[ord2[0]-1]
kt0 = kt[0]/ergkev
kt0err = kterr[0]/ergkev

;# take a stab at gal vel disp using Xue and Wu 2000
gvdisp = 10.^(2.51)*tvir^0.61
gvdisperr = gvdisp*sqrt((0.01/2.51)^2.+(tvirerr/tvir)^2.+(0.01/0.61)^2.)
print, FORMAT='(A-30,I10,A6,I10,A12)', 'Est. gal vel disp:', gvdisp, '+/-', gvdisperr, 'km/s'

;# take a stab at the stellar vel disp using Faber-Jackson
svdisp = 10.0^(-0.1*MB+0.2)
svdisperr = svdisp*sqrt((MBerr/MB)^2.)
IF ctype EQ 'birzan' THEN BEGIN
   svdisp = 289.0
   svdisperr = 28.9
ENDIF
print, FORMAT='(A-30,I10,A6,I10,A12)', 'Est. stell vel disp:', svdisp, '+/-', svdisperr, 'km/s'

;# take a stab at black hole masses
IF (svdisp GT 0) THEN BEGIN
   vmbh = (10.^(8.13+4.02*alog10(svdisp/200.)))/1d9
   vmbherr = vmbh*sqrt((0.06/8.13)^2.+(svdisperr/svdisp)^2.+(0.32/4.02)^2.)
   print, FORMAT='(A-30,F10.2,A6,F10.2,A12)', 'Est. M_BH(sigma):', vmbh, '+/-', vmbherr, '1d9 Msol'
   push, smbh, vmbh
   push, smbherr, vmbherr
ENDIF
IF (MB NE 0) THEN BEGIN
   bmbh = (10.^(-0.40*(MB+19.5)+8.27))/1d9
   bmbherr = bmbh*sqrt((0.05/0.4)^2.+(0.08/8.27)^2.+(MBerr/MB)^2.)
   print, FORMAT='(A-30,F10.2,A6,F10.2,A12)', 'Est. M_BH(M_B):', bmbh, '+/-', bmbherr, '1d9 Msol'
   push, smbh, bmbh
   push, smbherr, bmbherr
ENDIF
IF (MR NE 0) THEN BEGIN
   rmbh = (10.^(-0.38*(MR+21)+8.12))/1d9
   rmbherr = rmbh*sqrt((0.04/0.38)^2.+(0.08/8.12)^2.+(MRerr/MR)^2.)
   print, FORMAT='(A-30,F10.2,A6,F10.2,A12)', 'Est. M_BH(M_R):', rmbh, '+/-', rmbherr, '1d9 Msol'
   push, smbh, rmbh
   push, smbherr, rmbherr
   dustmbh = (10.^(-0.30*(MR+21)+7.96))/1d9
   dustmbherr = dustmbh*sqrt((0.06/0.3)^2.+(0.1/7.96)^2.+(MRerr/MR)^2.)
   print, FORMAT='(A-30,F10.2,A6,F10.2,A12)', 'Est. M_BH(M_R-dust):', dustmbh, '+/-', dustmbherr, '1d9 Msol'
   push, smbh, dustmbh
   push, smbherr, dustmbherr
ENDIF
IF (MK NE 0) THEN BEGIN
   kmbh = (10.^(-0.33*(MK+24)+8.33))/1d9
   kmbherr = kmbh*sqrt((0.09/0.33)^2.+(0.15/8.33)^2.+(MKerr/MK)^2.)
   print, FORMAT='(A-30,F10.2,A6,F10.2,A12)', 'Est. M_BH(M_K):', kmbh, '+/-', kmbherr, '1d9 Msol'
   push, smbh, kmbh
   push, smbherr, kmbherr
ENDIF
weights = 1./smbherr
wmbh = wtd_mean(smbh,weights)
wmbherr = sqrt(1.0/total(weights))
print, FORMAT='(A-30,F10.2,A6,F10.2,A12)', 'Weighted SMBH:', wmbh, '+/-', wmbherr, '1d9 Msol'
IF sigma_bh EQ "yes" THEN BEGIN
   fidmbh = vmbh
   fidmbherr = vmbherr
   mname = 'USING SIGMA_MBH'
ENDIF
IF bband_bh EQ "yes" THEN BEGIN
   fidmbh = bmbh
   fidmbherr = bmbherr
   mname = 'USING BBAND_MBH'
ENDIF
IF rband_bh EQ "yes" THEN BEGIN
   fidmbh = rmbh
   fidmbherr = rmbherr
   mname = 'USING RBAND_MBH'
ENDIF
IF kband_bh EQ "yes" THEN BEGIN
   fidmbh = kmbh
   fidmbherr = kmbherr
   mname = 'USING KBAND_MBH'
ENDIF
IF dust_bh EQ "yes" THEN BEGIN
   fidmbh = dustmbh
   fidmbherr = dustmbherr
   mname = 'USING DUSTK_MBH'
ENDIF
IF w_bh EQ "yes" THEN BEGIN
   fidmbh = wmbh
   fidmbherr = wmbherr
   mname = 'USING WEIGHTED_MBH'
ENDIF
print, '####################'
print, '####################'
print, mname
print, '####################'
print, '####################'

;# calc the volumes
;; IF ctype NE 'birzan' THEN BEGIN
;;    mcarr = maken(b1,a1,1d5)
;;    itmc = 0
;;    WHILE (itmc LT nmc) DO BEGIN
;;       ord = randomind(1d5,1)
;;       c1 = mcarr[ord]
;;       IF ((a1/c1 GE 2.5) OR (b1/c1 GT 2.5)) THEN GOTO,NOMC1
;;       vol = (4.*!pi*a1*b1*c1)/3.
;;       cs = !pi*b1*c1
;;       push, mcvol, vol
;;       push, mccs, cs
;;       push, mcc1, c1
;;       itmc++
;;       NOMC1:
;;    ENDWHILE
;;    dc1 = stddev(mcc1)
;;    c1 = mean(mcc1)
;;    vol1 = mean(mcvol)
;;    vol1err = stddev(mcvol)
;;    cs1 = mean(mccs)
;;    cs1err = stddev(mccs)
;;    mcarr = maken(b2,a2,1d5)
;;    itmc = 0
;;    WHILE (itmc LT nmc) DO BEGIN
;;       ord = randomind(1d5,1)
;;       c2 = mcarr[ord]
;;       IF ((a2/c2 GE 2.5) OR (b2/c2 GT 2.5)) THEN GOTO,NOMC2
;;       vol = (4.*!pi*a2*b2*c2)/3.
;;       cs = !pi*b2*c2
;;       push, mcvol, vol
;;       push, mccs, cs
;;       push, mcc2, c2
;;       itmc++
;;       NOMC2:
;;    ENDWHILE
;;    dc2 = stddev(mcc2)
;;    c2 = mean(mcc2)
;;    vol2 = mean(mcvol)
;;    vol2err = stddev(mcvol)
;;    cs2 = mean(mccs)
;;    cs2err = stddev(mccs)
;; ENDIF ELSE BEGIN
;;    vol1 = (4.*!pi*a1*b1*c1)/3.
;;    vol1err = caverr*vol1
;;    cs1 = !pi*b1*c1
;;    cs1err = caverr*cs1
;;    vol2 = (4.*!pi*a2*b2*c2)/3.
;;    vol2err = caverr*vol2
;;    cs2 = !pi*b2*c2
;;    cs2err = caverr*cs2
;; ENDELSE
rcav1 = sqrt(a1*b1)
rcav1err = rcav1*sqrt((da1/a1)^2.+(db1/b1)^2.)
rcav2 = sqrt(a2*b2)
rcav2err = rcav2*sqrt((da2/a2)^2.+(db2/b2)^2.)
IF ((ctype NE 'birzan') AND (ctype NE 'outer')) THEN BEGIN
   c1 = rcav1
   dc1 = rcav1err
   c2 = rcav2
   dc2 = rcav2err
ENDIF
vol1 = (4.*!pi*a1*b1*c1)/3.
vol1err = caverr*vol1
cs1 = !pi*b1*c1
cs1err = caverr*cs1
vol2 = (4.*!pi*a2*b2*c2)/3.
vol2err = caverr*vol2
cs2 = !pi*b2*c2
cs2err = caverr*cs2

;# print info
print, FORMAT='(A-20,5F10.1,A8)', '(a,b,c,reff,D) bubble 1:', a1, b1, c1, rcav1, d1, 'kpc'
print, FORMAT='(A-20,5F10.1,A8)', '(a,b,c,reff,D) bubble 2:', a2, b2, c2, rcav2, d2, 'kpc'
print, FORMAT='(A-30,F10.2,A6,F10.2,A12)', 'Tx-mid bubble 1:', ktmid1/ergkev, '+/-', ktmid1err/ergkev, 'keV'
print, FORMAT='(A-30,F10.2,A6,F10.2,A12)', 'Tx-mid bubble 2:', ktmid2/ergkev, '+/-', ktmid2err/ergkev, 'keV'
d1 = d1*cmkpc
dd1 = dd1*cmkpc
d2 = d2*cmkpc
dd2 = dd2*cmkpc
print, FORMAT='(A-30,I10,A6,I10,A12)', 'Volume bubble 1:', vol1, '+/-', vol1err, 'kpc**3'
print, FORMAT='(A-30,I10,A6,I10,A12)', 'Volume bubble 2:', vol2, '+/-', vol2err, 'kpc**3'
vol1 = vol1*cmkpc^3.
vol1err = vol1err*cmkpc^3.
vol2 = vol2*cmkpc^3.
vol2err = vol2err*cmkpc^3.
cs1 = cs1*cmkpc^2.
cs1err = cs1err*cmkpc^2.
cs2 = cs2*cmkpc^2.
cs2err = cs2err*cmkpc^2.
rcav1 = rcav1*cmkpc
rcav1err = rcav1err*cmkpc
rcav2 = rcav2*cmkpc
rcav2err = rcav2err*cmkpc

;# calculate sound speed age
tcs1 = d1/(sqrt((gamma2*ktmid1)/(mu*mh)))
ran = tcs1*sqrt((0.5*(ktmid1err/ktmid1)^2.+(dd1/d1)^2.))
sys = 0.1*tcs1
tcs1err = sqrt(ran^2.+sys^2.)
tcs2 = d2/(sqrt((gamma2*ktmid2)/(mu*mh)))
ran = tcs2*sqrt((0.5*(ktmid2err/ktmid2)^2.+(dd2/d2)^2.))
sys = 0.1*tcs2
tcs2err = sqrt(ran^2.+sys^2.)
gascs = (sqrt((gamma2*ktmid2)/(mu*mh)))/1d5
gascserr = gascs*(ktmid2err/ktmid2)
print, FORMAT='(A-30,E10.2,A6,E10.2,A12)', 'ICM sound speed:', gascs, '+/-', gascserr, 'km/s'
print, FORMAT='(A-30,E10.2,A6,E10.2,A12)', 't_sonic bubble 1:', tcs1/secyr, '+/-', tcs1err/secyr, 'yr'
print, FORMAT='(A-30,E10.2,A6,E10.2,A12)', 't_sonic bubble 2:', tcs2/secyr, '+/-', tcs2err/secyr, 'yr'
tcs = (tcs1+tcs2)/2.
tcserr = tcs*sqrt((tcs1err/tcs1)^2.+(tcs2err/tcs2)^2.)
print, FORMAT='(A-30,E10.2,A6,E10.2,A12)', 't_sonic avg.:', tcs/secyr, '+/-', tcserr/secyr, 'yr'

;# calculate buoyant age
g1 = 2.0*(svdisp*1d5)^2./d1
g1err = g1*sqrt(2.0*(svdisperr/svdisp)^2.+(dd1/d1)^2.)
tb1 = d1*sqrt((cs1*drag)/(2.0*g1*vol1))
tb1err = tb1*sqrt(0.5*(cs1err/cs1)^2.+0.5*(vol1err/vol1)^2.)
g2 = 2.0*(svdisp*1d5)^2./d2
g2err = g2*sqrt(2.0*(svdisperr/svdisp)^2.+(dd2/d2)^2.)
tb2 = d2*sqrt((cs2*drag)/(2.0*g2*vol2))
tb2err = tb2*sqrt(0.5*(cs2err/cs2)^2.+0.5*(vol2err/vol2)^2.)
print, FORMAT='(A-30,E10.2,A6,E10.2,A12)', 't_buoy bubble 1:', tb1/secyr, '+/-', tb1err/secyr, 'yr'
print, FORMAT='(A-30,E10.2,A6,E10.2,A12)', 't_buoy bubble 2:', tb2/secyr, '+/-', tb2err/secyr, 'yr'
tb = (tb1+tb2)/2.
tberr = tb*sqrt((tb1err/tb1)^2.+(tb2err/tb2)^2.)
print, FORMAT='(A-30,E10.2,A6,E10.2,A12)', 't_buoy avg.:', tb/secyr, '+/-', tberr/secyr, 'yr'

;# calculate refilling age
tr1 = 2.0*sqrt(rcav1/g1)
tr1err = tr1*sqrt(0.5*(rcav1err/rcav1)^2.+0.5*(g1err/g1)^2.)
tr2 = 2.0*sqrt(rcav2/g2)
tr2err = tr2*sqrt(0.5*(rcav2err/rcav2)^2.+0.5*(g2err/g2)^2.)
print, FORMAT='(A-30,E10.2,A6,E10.2,A12)', 't_refill bubble 1:', tr1/secyr, '+/-', tr1err/secyr, 'yr'
print, FORMAT='(A-30,E10.2,A6,E10.2,A12)', 't_refill bubble 2:', tr2/secyr, '+/-', tr2err/secyr, 'yr'
tr = (tr1+tr2)/2.
trerr = tr*sqrt((tr1err/tr1)^2.+(tr2err/tr2)^2.)
print, FORMAT='(A-30,E10.2,A6,E10.2,A12)', 't_refill avg.:', tr/secyr, '+/-', trerr/secyr, 'yr'

;# decide which time to use
IF time EQ 'sound' THEN BEGIN
   tcav = tcs
   tcaverr = tcserr
   tcav1 = tcs1
   tcav1err = tcs1err
   tcav2 = tcs2
   tcav2err = tcs2err
ENDIF ELSE IF time EQ 'buoy' THEN BEGIN
   tcav = tb
   tcaverr = tberr
   tcav1 = tb1
   tcav1err = tb1err
   tcav2 = tb2
   tcav2err = tb2err
ENDIF ELSE IF time EQ 'refill' THEN BEGIN
   tcav = tr
   tcaverr = trerr
   tcav1 = tr1
   tcav1err = tr1err
   tcav2 = tr2
   tcav2err = tr2err
ENDIF ELSE BEGIN
   print, '## ERROR: You gave me a bad time designation, pick sound, buoy, or refill'
   RETURN
ENDELSE

;# calculate pV
IF pvmethod EQ 'mid' THEN BEGIN
   ord1 = where(prin GE d1/cmkpc)
   p1 = p[ord1[0]-1]
   p1err = perr[ord1[0]-1]
   ord2 = where(prin GE d2/cmkpc)
   p2 = p[ord2[0]-1]
   p2err = perr[ord2[0]-1]
   print, FORMAT='(A-30,E10.2,A6,E10.2,A12)', 'P-mid bubble1:', p1, '+/-', p1err, 'erg/cm**3'
   print, FORMAT='(A-30,E10.2,A6,E10.2,A12)', 'P-mid bubble2:', p2, '+/-', p2err, 'erg/cm**3'
   pv1 = p1*vol1
   pv1err = pv1*sqrt((p1err/p1)^2.+(vol1err/vol1)^2.)
   pv2 = p2*vol2
   pv2err = pv2*sqrt((p2err/p2)^2.+(vol2err/vol2)^2.)
   print, FORMAT='(A-30,E10.2,A6,E10.2,A12)', 'pV bubble 1:', pv1, '+/-', pv1err, 'erg'
   print, FORMAT='(A-30,E10.2,A6,E10.2,A12)', 'pV bubble 2:', pv2, '+/-', pv2err, 'erg'
ENDIF ELSE IF pvmethod EQ 'int' THEN BEGIN
   print, '## ERROR: I am broken, do not use me.'
   return
   rint = maken(min(rplo+rp),max(rphi+rp),1d4)
   pint = interpol(p, rp, rint)
   perrint = interpol(perr, rp, rint)
   rin = 0.0*cmkpc
   rout = a1*cmkpc
   r = b1*cmkpc
   pv1 = 0.0
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
   rout = a2*cmkpc
   r = b2*cmkpc
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
ecav1err = (gamma1/(gamma1-1.))*pv1err
ecav2 = (gamma1/(gamma1-1.))*pv2
ecav2err = (gamma1/(gamma1-1.))*pv2err
print, FORMAT='(A-30,E10.2,A6,E10.2,A12)', 'Ecav bubble 1:', ecav1, '+/-', ecav1err, 'erg'
print, FORMAT='(A-30,E10.2,A6,E10.2,A12)', 'Ecav bubble 2:', ecav2, '+/-', ecav2err, 'erg'
ecav = ecav1+ecav2
ecaverr = ecav1err+ecav2err
print, FORMAT='(A-30,E10.2,A6,E10.2,A12)', 'Ecav total:', ecav, '+/-', ecaverr, 'erg'

;# calc Pcav
pcav1 = ecav1/tcav1
pcav1err = pcav1*sqrt((ecav1err/ecav1)^2.+(tcav1err/tcav1)^2.)
pcav2 = ecav2/tcav2
pcav2err = pcav2*sqrt((ecav2err/ecav2)^2.+(tcav2err/tcav2)^2.)
print, FORMAT='(A-30,E10.2,A6,E10.2,A12)', 'Pcav bubble 1:', pcav1, '+/-', pcav1err, 'erg/s'
print, FORMAT='(A-30,E10.2,A6,E10.2,A12)', 'Pcav bubble 2:', pcav2, '+/-', pcav2err, 'erg/s'
pcav = pcav1+pcav2
pcaverr = pcav*sqrt((pcav1err/pcav1)^2.+(pcav2err/pcav2)^2.)
print, FORMAT='(A-30,E10.2,A6,E10.2,A12)', 'Pcav total:', pcav, '+/-', pcaverr, 'erg/s'

;# calc mass accretion
macc1 = ecav1/(eff*c^2.)/msun
macc1err = ecav1err/(eff*c^2.)/msun
macc2 = ecav2/(eff*c^2.)/msun
macc2err = ecav2err/(eff*c^2.)/msun
macc = macc1+macc2
maccerr = macc*sqrt((macc1err/macc1)^2.+(macc2err/macc2)^2.)
print, FORMAT='(A-30,E10.2,A6,E10.2,A12)', 'Accreted Mass total:', macc, '+/-', maccerr, 'Msol'

;# calc mass accretion rate
dmacc1 = macc1/(tcav1/secyr)
dmacc1err = dmacc1*sqrt((macc1err/macc1)^2.+(tcav1err/tcav1)^2.)
dmacc2 = macc2/(tcav2/secyr)
dmacc2err = dmacc2*sqrt((macc2err/macc2)^2.+(tcav2err/tcav2)^2.)
dmacc = dmacc1+dmacc2
dmaccerr = dmacc*sqrt((dmacc1err/dmacc1)^2.+(dmacc2err/dmacc2)^2.)
print, FORMAT='(A-30,E10.2,A6,E10.2,A12)', 'Mass accret rate:', dmacc, '+/-', dmaccerr, 'Msol/yr'

;# calc black hole mass change
dmbh1 = (1.0-eff)*macc1
dmbh1err = (1.0-eff)*macc1err
dmbh2 = (1.0-eff)*macc2
dmbh2err = (1.0-eff)*macc2err
dmbh = dmbh1+dmbh2
dmbherr = dmbh*sqrt((dmbh1err/dmbh1)^2.+(dmbh2err/dmbh2)^2.)
print, FORMAT='(A-30,E10.2,A6,E10.2,A12)', 'Delta(M_BH) total:', dmbh, '+/-', dmbherr, 'Msol'

;# calc black hole mass rate of change
ddmbh1 = dmbh1/(tcav1/secyr)
ddmbh1err = ddmbh1*sqrt((dmbh1err/dmbh1)^2.+(tcav1err/tcav1)^2.)
ddmbh2 = dmbh2/(tcav2/secyr)
ddmbh2err = ddmbh2*sqrt((dmbh2err/dmbh2)^2.+(tcav2err/tcav2)^2.)
ddmbh = ddmbh1+ddmbh2
ddmbherr = ddmbh*sqrt((ddmbh1err/ddmbh1)^2.+(ddmbh2err/ddmbh2)^2.)
print, FORMAT='(A-30,E10.2,A6,E10.2,A12)', 'dot-Delta(M_BH) total:', ddmbh, '+/-', ddmbherr, 'Msol/yr'

;# calc frac mbh change
mbhrat = dmbh/((fidmbh*1d9)-dmbh)*100.
mbhraterr = mbhrat*sqrt((dmbherr/dmbh)^2.+(fidmbherr/fidmbh)^2.)*100.
print, FORMAT='(A-30,E10.2,A6,E10.2,A12)', 'BH grew by:', mbhrat, '+/-', mbhraterr, '%'

;# calc Eddington accretion rate
medd = 2.2*fidmbh/eff
medderr = 2.2*fidmbherr/eff
eddrat = dmacc/medd
eddraterr = eddrat*sqrt((dmaccerr/dmacc)^2.+(medderr/medd)^2.)
print, FORMAT='(A-30,E10.2,A6,E10.2,A12)', 'Eddington Macc:', medd, '+/-', medderr, 'Msol/yr'
print, FORMAT='(A-30,E10.2,A6,E10.2,A12)', 'Eddington Ratio:', eddrat, '+/-', eddraterr, ''

;# calc Bondi accretion rate
mbon = 0.013*(kt0)^(-3./2.)*nelec0*fidmbh^2.
mbonerr = mbon*sqrt((nelec0err/nelec0)^2.+(3./2.)*(kt0err/kt0)^2.+2.*(fidmbherr/fidmbh)^2.)
mbonent = 0.013*kna^(-3./2.)*fidmbh^2.
mbonenterr = mbonent*sqrt((3./2.)*(knaerr/kna)^2.+2.*(fidmbherr/fidmbh)^2.)
conbon = (2.0*unvG*mu*mh*1d9*msun)/(gamma2*ergkev*cmkpc)*1000.
rbon = conbon*kt0^(-1.0)*fidmbh
rbonerr = rbon*sqrt((fidmbherr/fidmbh)^2.+(kt0err/kt0)^2.)
bonrat = dmacc/mbon
bonraterr = dmaccerr/mbonerr
print, FORMAT='(A-30,E10.2,A6,E10.2,A12)', 'Bondi Radius:', rbon, '+/-', rbonerr, 'pc'
print, FORMAT='(A-30,E10.2,A6,E10.2,A12)', 'Bondi Macc(rho0,T0):', mbon, '+/-', mbonerr, 'Msol/yr'
print, FORMAT='(A-30,E10.2,A6,E10.2,A12)', 'Bondi Macc(K0):', mbonent, '+/-', mbonenterr, 'Msol/yr'
print, FORMAT='(A-30,E10.2,A6,E10.2,A12)', 'Bondi Ratio:', bonrat, '+/-', bonraterr, ''

;# kbon needed for macc
needk0 = (dmacc/0.013)^(-2./3.)*(fidmbh)^(-4./3.)
tcool = 1d8*(needk0/10.)^(3./2.)*(0.5/5.)^(-1.)/1d6
print, FORMAT='(A-30,E10.2,A28)', 'Need KBon:', needk0, 'keV cm^2'
print, FORMAT='(A-30,E10.2,A28)', 'Implied tcool:', tcool, 'Myr'

;# calc expected optical lumin of acc disk
ldisk = 1.7d12 * fidmbh^(1.27) * (eddrat/0.1)^(0.6) * lsun
ldiskerr = ldisk*sqrt((fidmbherr/fidmbh)^2.+(eddraterr/eddrat)^2.)
print, FORMAT='(A-30,E10.2,A6,E10.2,A12)', 'Opt disk lumin:', ldisk, '+/-', ldiskerr, 'erg/s'

;# create a tex table part 1
openw, /get_lun, lun, outtex
out1 = [a1, b1, c1, d1/cmkpc, tcs1/secyr/1d6, tb1/secyr/1d6, tr1/secyr/1d6, ecav1/1d60, pcav1/1d45]
out1err = [da1, db1, dc1, dd1/cmkpc, tcs1err/secyr/1d6, tb1err/secyr/1d6, tr1err/secyr/1d6, ecav1err/1d60, pcav1err/1d45]
form = replicate('(F10.2)',n_elements(out1))
FOR i=0,n_elements(out1)-1 DO BEGIN
   IF i NE 0 THEN printf, lun, format='(A)','&'
   printf, lun, format='(A)','$'
   printf, lun, format=form[i], out1[i]
   printf, lun, format='(A)','\pm'
   printf, lun, format=form[i], out1err[i]
   printf, lun, format='(A)','$'
ENDFOR
printf, lun, format='(A)','\\'
void, line
out2 = [a2, b2, c2, d2/cmkpc, tcs2/secyr/1d6, tb2/secyr/1d6, tr2/secyr/1d6, ecav2/1d60, pcav2/1d45]
out2err = [da2, db2, dc2, dd2/cmkpc, tcs2err/secyr/1d6, tb2err/secyr/1d6, tr2err/secyr/1d6, ecav2err/1d60, pcav2err/1d45]
form = replicate('(F10.2)',n_elements(out2))
FOR i=0,n_elements(out2)-1 DO BEGIN
   IF i NE 0 THEN printf, lun, format='(A)','&'
   printf, lun, format='(A)','$'
   printf, lun, format=form[i], out2[i]
   printf, lun, format='(A)','\pm'
   printf, lun, format=form[i], out2err[i]
   printf, lun, format='(A)','$'
ENDFOR
printf, lun, format='(A)','\\'
close, lun

END
