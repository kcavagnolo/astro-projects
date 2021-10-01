PRO pv, indat

; return to caller on error
myhome = GETENV('HOME')
ON_ERROR, 2
IF (n_params() LT 1) THEN BEGIN
   print, '# ERROR: Need an input file... gimme gimme gimme gimme gimme'
   RETURN
ENDIF
!QUIET = 1

;# set options
name     = 'RBS_0797'           ;# used to ensure only cluster tx grabbed
prat     = 2.3/1.2              ;# number dens, n = 2.3 n_H and n_H = nelec/1.2
pvmethod = 'mid'                ;# int or mid
time     = 'buoy'               ;# time scale for calculation of Pcav
w_bh     = 'yes'                ;# mbh to use
sigma_bh = 'no'
bband_bh = 'no'
rband_bh = 'no'
kband_bh = 'no'
dust_bh  = 'no'
outtex1  = 'pvout1.tex'         ;# name of output file
outtex2  = 'pvout2.tex'         ;# name of output file
dummbh   = 8.0                  ;# mock Mbh
tbon     = 0.5                  ;# tx at Rbon in keV
mgas     = 1d10                 ;# cd gas mass in CO?
lqso     = 2.26d44              ;# lumin of point source
lqsoerr  = 0.55d44              ;# err on lqso
acis     = 0.492                ;# ACIS 0.492 arcsec/pixel
z        = 0.354                ;# cluster redshift
MB       = -23.43               ;# B-band absolute mag
MBerr    = 0.23                 ;# err on MB
MR       = -24.03               ;# R-band absolute mag
MRerr    = 0.28                 ;# err on MR
rlum     = 2.402d11
MK       = -26.19               ;# corr K-band mag from 2MASS
MKerr    = 0.17                 ;# K-band err
tcl      = 7.54                 ;# estimate of Tcl
tclerr   = 1.76                 ;# err on Tcl
eff      = 0.1                  ;# assumed efficieny of converting mass to energy
drag     = 0.75                 ;# assumed drag coefficient from Churazov 2001
pfile    = myhome+'/research/rbs_797/data/2202_7902_table.dat'
kfile    = myhome+'/research/rbs_797/data/2202_7902_results.log'
txfile   = myhome+'/research/rbs_797/data/tx_2.5K/proj_1T_nhfro.dat'
restore, myhome+'/research/redux/scripts/s_tabletemplate.sav'
restore, myhome+'/research/redux/scripts/s_resultstemplate.sav'
restore, myhome+'/research/redux/scripts/xspectemp_rin_normerr_src.sav'

;#################################
;#################################
;# Main Program
;#################################
;#################################

;# physical constants
cosmology, z, result, /silent   ;# calc cosmology
ang = result[4]                 ;# get D_A in kpc/"
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

;# read data file
readcol, indat, FORMAT='A,D,D,D,D,A', comment='#', $
         cids, a1s, b1s, c1s, d1s, units

;# open the output file
openw, /get_lun, lun1, outtex1
openw, /get_lun, lun2, outtex2
printf, lun1, format='(10A-10)', '#ID','a','b','c','D','tsonic','tbuoy','trefill','Ecav','Pcav'
printf, lun1, format='(10A-10)', '#-','kpc','kpc','kpc','kpc','Myr','Myr','Myr','1d60 erg','1d45 erg/s'

;# take a stab at gal vel disp using Xue and Wu 2000
print, ''
print, '######################'
print, '# General Properties #'
print, '######################'
print, ''
gvdisp = 10.^(2.51)*tcl^0.61
gvdisperr = gvdisp*sqrt((0.01/2.51)^2.+(tclerr/tcl)^2.+(0.01/0.61)^2.)
print, FORMAT='(A-30,I10,A6,I10,A12)', 'Galaxy vel disp:', gvdisp, '+/-', gvdisperr, 'km/s'

;# take a stab at the stellar vel disp using Faber-Jackson
svdisp = 10.0^(-0.1*MB+0.2)
svdisperr = svdisp*sqrt((MBerr/MB)^2.)
print, FORMAT='(A-30,I10,A6,I10,A12)', 'Stellar vel disp:', svdisp, '+/-', svdisperr, 'km/s'

;# take a stab at black hole masses
IF (svdisp GT 0) THEN BEGIN
   vmbh = (10.^(8.13+4.02*alog10(svdisp/200.)))/1d9
   vmbherr = vmbh*sqrt((0.06/8.13)^2.+(svdisperr/svdisp)^2.+(0.32/4.02)^2.)
   print, FORMAT='(A-30,F10.2,A6,F10.2,A12)', 'M_BH(sigma):', vmbh, '+/-', vmbherr, '1d9 Msol'
   push, smbh, vmbh
   push, smbherr, vmbherr
ENDIF
IF (MB NE 0) THEN BEGIN
   bmbh = (10.^(-0.40*(MB+19.5)+8.27))/1d9
   bmbherr = bmbh*sqrt((0.05/0.4)^2.+(0.08/8.27)^2.+(MBerr/MB)^2.)
   print, FORMAT='(A-30,F10.2,A6,F10.2,A12)', 'M_BH(M_B):', bmbh, '+/-', bmbherr, '1d9 Msol'
   push, smbh, bmbh
   push, smbherr, bmbherr
ENDIF
IF (MR NE 0) THEN BEGIN
   rmbh = (10.^(-0.38*(MR+21)+8.12))/1d9
   rmbherr = rmbh*sqrt((0.04/0.38)^2.+(0.08/8.12)^2.+(MRerr/MR)^2.)
   print, FORMAT='(A-30,F10.2,A6,F10.2,A12)', 'M_BH(M_R):', rmbh, '+/-', rmbherr, '1d9 Msol'
   push, smbh, rmbh
   push, smbherr, rmbherr
   dustmbh = (10.^(-0.30*(MR+21)+7.96))/1d9
   dustmbherr = dustmbh*sqrt((0.06/0.3)^2.+(0.1/7.96)^2.+(MRerr/MR)^2.)
   print, FORMAT='(A-30,F10.2,A6,F10.2,A12)', 'M_BH(M_R-dust):', dustmbh, '+/-', dustmbherr, '1d9 Msol'
   push, smbh, dustmbh
   push, smbherr, dustmbherr
ENDIF
IF (MK NE 0) THEN BEGIN
   kmbh = (10.^(-0.33*(MK+24)+8.33))/1d9
   kmbherr = kmbh*sqrt((0.09/0.33)^2.+(0.15/8.33)^2.+(MKerr/MK)^2.)
   print, FORMAT='(A-30,F10.2,A6,F10.2,A12)', 'M_BH(M_K):', kmbh, '+/-', kmbherr, '1d9 Msol'
   push, smbh, kmbh
   push, smbherr, kmbherr
ENDIF
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
   fidmbh = kmbh
   fidmbherr = kmbherr
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
fidmbhlo = fidmbh-min(smbh-smbherr)
fidmbhhi = max(smbh+smbherr)-fidmbh
print, FORMAT='(A-30,F10.2,A4,F5.2,A2,F5.2,A12)', mname, fidmbh, '+', fidmbhhi, '-', fidmbhlo, '1d9 Msol'

;# go through all cavities
FOR ind=0,n_elements(cids)-1 DO BEGIN

   ;# make unique values
   cid = cids[ind]
   a1 = a1s[ind]
   b1 = b1s[ind]
   c1 = c1s[ind]
   d1 = d1s[ind]

   ;# give status
   print,''
   print,'###########################'
   print, '# Working on ', cid
   print,'###########################'
   print,''
   printf, lun1, format='(A)',cid+' & '

   ;# calculate c if needed
   IF c1 LE 0 THEN c1 = sqrt(a1*b1)

   ;# convert to kpc if needed
   IF units[ind] NE 'kpc' THEN BEGIN
      a1 = ang*(a1*acis)
      b1 = ang*(b1*acis)
      c1 = ang*(c1*acis)
      d1 = ang*(d1*acis)
   ENDIF
   
   ;# cavity errors and volumes
   da1 = 0.1*a1
   db1 = 0.1*b1
   dc1 = 0.1*c1
   dd1 = 0.1*d1
   rcav1 = (a1*b1*c1)^(1./3.)
   rcav1err = rcav1*sqrt((da1/a1)^2.+(db1/b1)^2.+(dc1/c1)^2.)
   vol = (4.*!pi*a1*b1*c1)/3.
   volerr = vol*sqrt((da1/a1)^2.+(db1/b1)^2.+(dc1/c1)^2.)
   cs1 = !pi*b1*c1
   cs1err = cs1*sqrt((db1/b1)^2.+(dc1/c1)^2.)
   print, FORMAT='(A-30,F10.1,A6,F10.1,A12)', 'Semi-major r:', a1, '+/-', da1, 'kpc'
   print, FORMAT='(A-30,F10.1,A6,F10.1,A12)', 'Semi-minor r:', b1, '+/-', db1, 'kpc'
   print, FORMAT='(A-30,F10.1,A6,F10.1,A12)', 'Semi-Polar r:', c1, '+/-', dc1, 'kpc'
   print, FORMAT='(A-30,F10.1,A6,F10.1,A12)', 'Effective r:', rcav1, '+/-', rcav1err, 'kpc'
   print, FORMAT='(A-30,F10.1,A6,F10.1,A12)', 'Cross-section:', cs1, '+/-', cs1err, 'kpc**2'
   print, FORMAT='(A-30,E10.2,A6,E10.2,A12)', 'Volume:', vol, '+/-', volerr, 'kpc**3'
   print, FORMAT='(A-30,F10.1,A6,F10.1,A12)', 'Distance:', d1, '+/-', dd1, 'kpc'

   ;# pressure get data
   dataobs = read_ascii(pfile, template = s_tabletemplate)
   prin  = dataobs.rin_mpc*1000.
   prout = dataobs.rout_mpc*1000.
   nelec = dataobs.n_elec
   nelecerr = dataobs.sigma_ne
   p     = dataobs.p_flat
   perr  = dataobs.p_err
   prin  = reverse(prin)
   prout = reverse(prout)
   p     = reverse(p)
   perr  = reverse(perr)
   nelec = reverse(nelec)
   nelecerr = reverse(nelecerr)
   rp    = (0.5*(prin^(3./2.)+prout^(3./2.)))^(2./3.)
   rplo  = rp-prin
   rphi  = prout-rp
   ord   = where((p EQ p) AND (p GT 0.))
   rp    = rp[ord]*cmkpc
   rplo  = rplo[ord]*cmkpc
   rphi  = rphi[ord]*cmkpc
   p     = p[ord]
   perr  = perr[ord]
   rat   = perr/p
   p     = prat*p                ;# calculate total gas pressure
   pran  = p*rat                 ;# random error
   psys  = 0.1*p                 ;# systematic error
   perr  = sqrt(pran^2.+psys^2.) ;# total error
   p     = p*ergkev
   perr  = perr*ergkev
   nelec0 = nelec[0]            ;# central electron dens
   nelec0err = nelecerr[0]      ;# err on central nelec
   kdata = read_ascii(kfile, template = s_resultstemplate)
   kna    = kdata.k0
   knaerr = kdata.k0err
   kna = kna[2]                 ;# central entropy
   knaerr = knaerr[2]           ;# err on central entropy

   ;# read spectral fits
   tfit  = read_ascii(txfile,template=xspectemp_rin_normerr_src)
   trin  = tfit.rin[where(tfit.cluster EQ name)]*60.*ang
   trout = tfit.rout[where(tfit.cluster EQ name)]*60.*ang
   kt    = tfit.tx[where(tfit.cluster EQ name)]
   kthi  = tfit.thi[where(tfit.cluster EQ name)]
   ktlo  = tfit.tlo[where(tfit.cluster EQ name)]
   void, kterr
   FOR i=0,n_elements(kt)-1 DO $
      IF kthi[i]-kt[i] GT kt[i]-ktlo[i] THEN push, kterr, kthi[i]-kt[i] ELSE push, kterr, kt[i]-ktlo[i]
   kt = kt*ergkev
   kterr = kterr*ergkev

   ;# get central kt and mid kt
   ord1 = where(trin GE d1)
   ktmid1 = kt[ord1[0]-1]
   ktmid1err = kterr[ord1[0]-1]
   kt0 = kt[0]/ergkev
   kt0err = kterr[0]/ergkev

   ;# convert everything to cm
   d1 = d1*cmkpc
   dd1 = dd1*cmkpc
   vol = vol*cmkpc^3.
   volerr = volerr*cmkpc^3.
   cs1 = cs1*cmkpc^2.
   cs1err = cs1err*cmkpc^2.
   rcav1 = rcav1*cmkpc
   rcav1err = rcav1err*cmkpc

   ;# calculate sound speed age
   tcs1 = d1/(sqrt((gamma2*ktmid1)/(mu*mh)))
   ran = tcs1*sqrt((0.5*(ktmid1err/ktmid1)^2.+(dd1/d1)^2.))
   sys = 0.1*tcs1
   tcs1err = sqrt(ran^2.+sys^2.)
   gascs = (sqrt((gamma2*ktmid1)/(mu*mh)))/1d5
   gascserr = gascs*(ktmid1err/ktmid1)
   print, FORMAT='(A-30,E10.2,A6,E10.2,A12)', 'ICM sound speed:', gascs, '+/-', gascserr, 'km/s'
   print, FORMAT='(A-30,F10.2,A6,F10.2,A12)', 't_sonic:', tcs1/secyr/1d6, '+/-', tcs1err/secyr/1d6, 'Myr'

   ;# calculate buoyant age
;   svdisp = 289.0
   g1 = 2.0*(svdisp*1d5)^2./d1
   g1err = g1*sqrt(2.0*(svdisperr/svdisp)^2.+(dd1/d1)^2.)
   tb1 = d1*sqrt((cs1*drag)/(2.0*g1*vol))
   tb1err = tb1*sqrt(0.5*(cs1err/cs1)^2.+0.5*(volerr/vol)^2.)
   print, FORMAT='(A-30,F10.2,A6,F10.2,A12)', 't_buoy:', tb1/secyr/1d6, '+/-', tb1err/secyr/1d6, 'Myr'

   ;# calculate refilling age
   tr1 = 2.0*sqrt(rcav1/g1)
   tr1err = tr1*sqrt(0.5*(rcav1err/rcav1)^2.+0.5*(g1err/g1)^2.)
   print, FORMAT='(A-30,F10.2,A6,F10.2,A12)', 't_refill:', tr1/secyr/1d6, '+/-', tr1err/secyr/1d6, 'Myr'

   ;# decide which time to use
   IF time EQ 'sound' THEN BEGIN
      tcav = tcs1
      tcaverr = tcs1err
      mname = '*** USING SOUND SPEED AGE ***'
   ENDIF ELSE IF time EQ 'buoy' THEN BEGIN
      tcav = tb1
      tcaverr = tb1err
      mname = '*** USING BUOYANCY AGE ***'
   ENDIF ELSE IF time EQ 'refill' THEN BEGIN
      tcav = tr1
      tcaverr = tr1err
      mname = '*** USING REFILLING AGE ***'
   ENDIF ELSE BEGIN
      print, '## ERROR: You gave me a bad time designation, pick sound, buoy, or refill'
      RETURN
   ENDELSE
   push, ttcav, tcav
   print, mname

   ;# calculate pV
   print, FORMAT='(A-30,F10.2,A6,F10.2,A12)', 'Tx-mid:', ktmid1/ergkev, '+/-', ktmid1err/ergkev, 'keV'
   IF pvmethod EQ 'mid' THEN BEGIN
      ord1 = where(prin GE d1/cmkpc)
      p1 = p[ord1[0]-1]
      p1err = perr[ord1[0]-1]
      print, FORMAT='(A-30,E10.2,A6,E10.2,A12)', 'P-mid bubble1:', p1, '+/-', p1err, 'erg/cm**3'
      pv = p1*vol
      pverr = pv*sqrt((p1err/p1)^2.+(volerr/vol)^2.)
   ENDIF ELSE IF pvmethod EQ 'int' THEN BEGIN
      print, '## ERROR: I am broken, do not use me.'
      return
      rint = maken(min(rplo+rp),max(rphi+rp),1d4)
      pint = interpol(p, rp, rint)
      perrint = interpol(perr, rp, rint)
      rin = 0.0*cmkpc
      rout = a1*cmkpc
      r = b1*cmkpc
      pv = 0.0
      pverr = 0.0
      FOR i=0,n_elements(rint)-1 DO BEGIN
         IF ((rint[i] LT rin) OR (rint[i] GT rout)) THEN GOTO,SKIP1
         pm = (pint[i]+pint[i+1])/2.0
         pmerr = (perrint[i]+perrint[i+1])/2.0
         vm = (!PI*r^2.*(rint[i+1]-rint[i]))
         pv = pv+(pm*vm)
         pverr = pverr+(pmerr*vm)
         SKIP1:
      ENDFOR
   ENDIF
   
   ;# calc energy
   print, FORMAT='(A-30,E10.2,A6,E10.2,A12)', 'pV:', pv, '+/-', pverr, 'erg'
   ecav = (gamma1/(gamma1-1.))*pv
   ecaverr = (gamma1/(gamma1-1.))*pverr
   print, FORMAT='(A-30,E10.2,A6,E10.2,A12)', 'Ecav:', ecav, '+/-', ecaverr, 'erg'
   push, tecav, ecav
   push, tecaverr, ecaverr

   ;# calc Pcav
   pcav = ecav/tcav
   pcaverr = pcav*sqrt((ecaverr/ecav)^2.+(tcaverr/tcav)^2.)
   print, FORMAT='(A-30,E10.2,A6,E10.2,A12)', 'Pcav:', pcav, '+/-', pcaverr, 'erg/s'
   push, tpcav, pcav
   push, tpcaverr, pcaverr

   ;# enter in tex table
   out1 = [a1, b1, c1, d1/cmkpc, tcs1/secyr/1d6, tb1/secyr/1d6, tr1/secyr/1d6, ecav/1d60, pcav/1d45]
   out1err = [da1, db1, dc1, dd1/cmkpc, tcs1err/secyr/1d6, tb1err/secyr/1d6, tr1err/secyr/1d6, ecaverr/1d60, pcaverr/1d45]
   form = replicate('(F10.1)',n_elements(out1))
   FOR i=0,n_elements(out1)-1 DO BEGIN
      IF i NE 0 THEN printf, lun1, format='(A)','&'
      printf, lun1, format='(A)','$'
      printf, lun1, format=form[i], out1[i]
      printf, lun1, format='(A)','\pm'
      printf, lun1, format=form[i], out1err[i]
      printf, lun1, format='(A)','$'
   ENDFOR
   printf, lun1, format='(A)','\\'

ENDFOR

print, ''
print, '######################'
print, '# System-wide totals #'
print, '######################'
print, ''

;# totals
rat = sqrt(total((tecaverr/tecav)^2.))
tecav = total(tecav)
tecaverr = tecav*rat
rat = sqrt(total((tpcaverr/tpcav)^2.))
tpcav = total(tpcav)
tpcaverr = tpcav*rat
mtcav = mean(ttcav)
mtcaverr = stddev(ttcav)
print, FORMAT='(A-30,E10.2,A6,E10.2,A12)', 'Total Ecav:', tecav, '+/-', tecaverr, 'erg'
print, FORMAT='(A-30,E10.2,A6,E10.2,A12)', 'Total Pcav:', tpcav, '+/-', tpcaverr, 'erg/s'
print, FORMAT='(A-30,E10.2,A28)', 'Mean age:', mtcav/secyr/1d6, 'Myr'
print, FORMAT='(A-30,E10.2,A28)', 'Mean Pcav:', tecav/mtcav, 'erg/s'

;# calc mass accretion
macc = tecav/(eff*c^2.)/msun
maccerr = macc*(tecaverr/tecav)
print, FORMAT='(A-30,E10.2,A6,E10.2,A12)', 'Accreted Mass total:', macc, '+/-', maccerr, 'Msol'

;# calc mass accretion rate
dmacc = tpcav/(eff*c^2.)*secyr/msun
dmaccerr = dmacc*(tpcaverr/tpcav)
print, FORMAT='(A-30,F10.3,A6,F10.3,A12)', 'Mass accret rate:', dmacc, '+/-', dmaccerr, 'Msol/yr'

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
;print, FORMAT='(A-30,F10.4,A6,F10.4,A12)', 'Eddington Ratio:', eddrat, '+/-', eddraterr, '--'
medd = 2.2*fidmbh/eff
meddlo = 2.2*(fidmbh-fidmbhlo)/eff
meddhi = 2.2*(fidmbh+fidmbhhi)/eff
eddrat = dmacc/medd
eddratlo = dmacc/meddlo
eddrathi = dmacc/meddhi
print, FORMAT='(A-30,F10.2,A6,F10.2,F10.2,A12)', 'Eddington Macc:', medd, '+/-', meddlo, meddhi, 'Msol/yr'
print, FORMAT='(A-30,F10.4,A6,F10.4,F10.2,A12)', 'Eddington Ratio:', eddrat, '+/-', eddratlo, eddrathi, '--'

;# calc Bondi accretion rate
mbon = 0.013*(kt0)^(-3./2.)*nelec0*fidmbh^2.
;mbonerr = mbon*sqrt((nelec0err/nelec0)^2.+(3./2.)*(kt0err/kt0)^2.+2.*(fidmbherr/fidmbh)^2.)
mbonlo = mbon*sqrt((nelec0err/nelec0)^2.+(3./2.)*(kt0err/kt0)^2.+2.*(fidmbhlo/fidmbh)^2.)
mbonhi = mbon*sqrt((nelec0err/nelec0)^2.+(3./2.)*(kt0err/kt0)^2.+2.*(fidmbhhi/fidmbh)^2.)
mbonent = 0.013*kna^(-3./2.)*fidmbh^2.
mbonenterr = mbonent*sqrt((3./2.)*(knaerr/kna)^2.+2.*(fidmbherr/fidmbh)^2.)
conbon = (2.0*unvG*mu*mh*1d9*msun)/(gamma2*ergkev*cmkpc)*1000.
rbon = conbon*kt0^(-1.0)*fidmbh
rbonerr = rbon*sqrt((fidmbherr/fidmbh)^2.+(kt0err/kt0)^2.)
bonrat = dmacc/mbon
;bonraterr = dmaccerr/mbonerr
bonratlo = dmaccerr/mbonlo
bonrathi = dmaccerr/mbonhi
;print, FORMAT='(A-30,E10.2,A6,E10.2,A12)', 'Bondi Macc(rho0,T0):', mbon, '+/-', mbonerr, 'Msol/yr'
print, FORMAT='(A-30,E10.2,A6,E10.2,E10.2,A12)', 'Bondi Macc(rho0,T0):', mbon, '+/-', mbonlo, mbonhi, 'Msol/yr'
print, FORMAT='(A-30,E10.2,A6,E10.2,A12)', 'Bondi Macc(K0):', mbonent, '+/-', mbonenterr, 'Msol/yr'
print, FORMAT='(A-30,E10.2,A6,E10.2,E10.2,A12)', 'Bondi Ratio:', bonrat, '+/-', bonratlo, bonrathi, '--'
print, FORMAT='(A-30,F10.1,A6,F10.1,A12)', 'Bondi Radius:', rbon, '+/-', rbonerr, 'pc'

;# kbon needed for macc
needk0 = (dmacc/(0.013*dummbh^2.))^(-2./3.)
tcool = 1d8*(needk0/10.)^(3./2.)*(tbon/5.)^(-1.)/1d6
needne = (needk0/tbon)^(-3./2.)
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

;# calc expected optical lumin of acc disk
ldisk = 1.7d12 * fidmbh^(1.27) * (qsomacc/medd/0.1)^(0.6) * lsun
print, FORMAT='(A-30,E10.2,A28)', 'Opt disk lumin (fidmbh):', ldisk, 'erg/s'
ldisk = 1.7d12 * dummbh^(1.27) * (qsomacc/medd/0.1)^(0.6) * lsun
print, FORMAT='(A-30,E10.2,A28)', 'Opt disk lumin (dummbh):', ldisk, 'erg/s'

;# calc bulge mass
mbul = rlum*6.3
print, FORMAT='(A-30,E10.2,A6,A10,A12)', 'M_bulge:', mbul, '', '', 'Msol'

;# dump to latex
out2 = [tecav/1d60, tpcav/1d45, fidmbh, macc/1d6, dmacc, dmbh/1d6, ddmbh, medd, mbonent/1d-4, eddrat/1d-3, bonrat]
out2err = [tecaverr/1d60, tpcaverr/1d45, fidmbherr, maccerr/1d6, dmaccerr, dmbherr/1d6, ddmbherr, 0, 0, 0, 0]
printf, lun2, format='(11A-12)', '#Ecav','Pcav','Mbh','Macc','dMacc','dMbh','ddMbh','Medd','Mbon','Macc/Medd','Macc/Mbon'
printf, lun2, format='(11A-12)', '#1d60 erg','1d45 erg','1d9 Msol','1d6 Msol','Msol/yr','1d6 Msol','Msol/yr','Msol/yr','1d-4 Msol/yr','1d-3','-'
form = ['(F10.2)','(F10.2)','(F10.2)','(F10.2)','(F10.2)','(F10.2)','(F10.2)','(F10.2)','(F10.2)','(F10.2)','(F10.2)']
FOR i=0,n_elements(out2)-1 DO BEGIN
   IF i NE 0 THEN printf, lun2, format='(A)','&'
   printf, lun2, format='(A)','$'
   printf, lun2, format=form[i], out2[i]
   printf, lun2, format='(A)','\pm'
   printf, lun2, format=form[i], out2err[i]
   printf, lun2, format='(A)','$'
ENDFOR
printf, lun2, format='(A)','\\'
close, lun1
close, lun2

END
