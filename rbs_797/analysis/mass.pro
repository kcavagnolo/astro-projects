PRO mass
myhome = GETENV('HOME')

;# set options
name   = 'RBS_0797'             ;# name of object in tx prof file
txfid  = 11.1                   ;# cluster temp in keV
delta  = 500.                   ;# r_delta for nfw modeling
z      = 0.354                  ;# cluster redshift
nmc   = 10                      ;# number of MC iters
file1  = myhome+'/research/rbs_797/data/2202_7902_table.dat'
txfile = myhome+'/research/rbs_797/data/tx_2.5K/proj_1T_nhfro.dat'
restore, myhome+'/research/redux/scripts/xspectemp_rin_normerr_src.sav'
restore, myhome+'/research/redux/scripts/s_tabletemplate.sav'

;# create block of useful constants
cmkpc = 3.08d21                      ;# cm's in 1 kpc
ergev = 1.60217646d-12               ;# 1 eV in erg
G     = 6.67259d-8                   ;# gravitational constant [cm^3/g/s^2]
mu    = 0.597                        ;# mean molecular weight of ICM assuming primordial gas comp
mh    = 1.007825*1.660538782d-27*1d3 ;# atomic mass of hydrogen atom x atomic mass unit [g]
mp    = 1.672621637d-27*1d3          ;# proton mass [g]
msun  = 1.9891d30*1d3                ;# solar mass [g]

;# nice plotting params
!FANCY    = 4
!LINETYPE = 0
!P.FONT   = 0
!X.THICK  = 1
!Y.THICK  = 1
!Z.THICK  = 1

;# build cosmo
cosmology, z, result, /silent
da = result[3]*1000.*cmkpc      ;# D_A in cm
ang = result[4]                 ;# kpc/arcsec

;# set some r_delta values
r200 = rdelta(200, z, txfid, /silent)*1000.
r500 = rdelta(500, z, txfid, /silent)*1000.
IF delta EQ 500 THEN outerr=r500 ELSE outerr=r200

;# read data for the beta model fitting
dataobs = read_ascii(file1, template = s_tabletemplate)
rin = dataobs.rin_mpc*1000.
rout = dataobs.rout_mpc*1000.
rdens = (0.5*(rin^(3./2.)+rout^(3./2.)))^(2./3.)
nelec = dataobs.n_elec
nelecerr = dataobs.sigma_ne
dmass = dataobs.mgas
dmasserr = dataobs.merr
rdens = reverse(rdens)
nelec = reverse(nelec)
nelecerr = reverse(nelecerr)
rdmass = rdens
dmass = reverse(dmass)
dmasserr = reverse(dmasserr)

;# build the input parameter array
parnames = ['rho0','rc','beta','bgd']
parinfo = replicate({value:0.D, $
                     fixed:0, $
                     limited:[1,1], $
                     limits:[0.D,0.D], $
                     step:[0.D], $
                     tied:['']}, n_elements(parnames))

;# initial guesses
a = [nelec[0], rdens[0]/4., 0.5D, min(nelec)]

;# limits
parinfo(0).limits = [0.1*nelec[0],1.1*nelec[0]] ;# rho0
parinfo(1).limits = [0.D,max(rdens)]            ;# rc
parinfo(2).limits = [0.D,5.D]                   ;# beta
parinfo(3).limits = [0.D,4.0*min(nelec)]        ;# bgd
parinfo(*).value  = a

;# run the fitter
weights = 1./nelecerr^2.
parinfo(*).value = a
print, ''
print, "## STATUS: Running mpcurvefit..."
result = mpcurvefit(rdens, nelec, weights, a, sigma, FUNCTION_NAME='dens_beta', $
                    ITMAX=10000, CHISQ=chisq, PARINFO=parinfo, STATUS=status, $
                    YERROR=residrms, ERRMSG=errmsg, DOF=dof, /DOUBLE, /QUIET, /NODERIVATIVE)
IF status LT 0 THEN BEGIN
   message, errmsg
   EXIT
ENDIF
print, ''
print, "## Fitting complete with no errors."
print, '## Fit parameters:'

;# *If* you can assume that the true reduced chi-squared
;# value is unity -- meaning that the fit is implicitly
;# assumed to be of good quality -- then the estimated
;# parameter uncertainties can be computed by scaling SIGMA
;# by the measured chi-squared value.
sigma = sigma*sqrt(chisq/dof)
resid = nelec-result
prob_est = 1.0-IGAMMA((0.5*n_elements(rdens)-3), 0.5*chisq)
FOR jj=0,n_elements(sigma)-1 DO BEGIN
   print, format='(A-10,E10.3,A7,E10.3)',parnames[jj]+': ',a[jj],' +/- ',sigma[jj]
ENDFOR
print, format='(A-10,E10.3,A7,E10.3)','Residuals:',mean(resid),' +/- ',residrms
print, format='(A-10,I10)','DOF:',dof
print, format='(A-10,F10.3)','ChiSq:',chisq
print, format='(A-10,F10.3)','RChiSq:',chisq/dof
print, format='(A-10,F10.3)','Prob:',prob_est

;# show the fit
xmin = 0.8*min(rdens)
xmax = 1.2*max(rdens)
ymin = 0.8*min(nelec)
ymax = 1.2*max(nelec)
plot, rdens, nelec, $
      psym=1, $
      xtitle='Radius [kpc]', $
      ytitle='Density [cm^-3]', $
      /xlog, /ylog, $
      /xsty, /ysty, $
      xrange = [xmin,xmax], $
      yrange = [ymin,ymax]
oplot, rdens, result, linestyle=2
yesno = ''
;read, '## INPUT: Press enter to continue', yesno

;# convert r to cm and create density profile
rho0 = a[0]                     ;# cent dens in #/cm^-3
rc = a[1]*cmkpc                 ;# core radius in cm
beta = a[2]                     ;# beta param
rdens = maken(1, outerr, 10000)
rdens = rdens*cmkpc
dens = (1.92*mu*mh)*rho0*(1.0+(rdens/rc)^2.)^(-3.0*beta/2.0)
denserr = 0.1*dens

;# calc mgas in each shell and sum moving out
dm = 0.0
dmerr = 0.0
FOR i=0,n_elements(rdens)-2 DO BEGIN
   vol = (4./3.)*!PI*(rdens[i+1]^3.-rdens[i]^3.)
   shm = dens[i]*vol/msun
   shmerr = denserr[i]*vol/msun
   dm = dm+shm
   dmerr = dm*sqrt((dmerr/dm)^2.+(shmerr/shm)^2.)
   push, mgas, dm
   push, mgaserr, dmerr
ENDFOR
last = n_elements(mgas)-1
push, mgas, mgas[last]
push, mgaserr, mgaserr[last]

;# show the fit
xmin = 0.8*min(rdens/cmkpc)
xmax = 1.2*max(rdens/cmkpc)
ymin = 0.8*min(mgas)
ymax = 1.2*max(mgas)
plot, rdens/cmkpc, mgas, $
      xtitle='Radius [kpc]', $
      ytitle='Gas Mass [M_solar]', $
      /xlog, /ylog, $
      /xsty, /ysty, $
      xrange = [xmin,xmax], $
      yrange = [ymin,ymax]
oplot, rdens, result, linestyle=2
yesno = ''
;read, '## INPUT: Press enter to continue', yesno

;# read spectral fits
tfit   = read_ascii(txfile,template=xspectemp_rin_normerr_src)
trin   = tfit.rin[where(tfit.cluster EQ name)]
trout  = tfit.rout[where(tfit.cluster EQ name)]
rtx    = 0.5*(trin+trout)*60.*ang
tx     = tfit.tx[where(tfit.cluster EQ name)]
txhi   = tfit.thi[where(tfit.cluster EQ name)]
txlo   = tfit.tlo[where(tfit.cluster EQ name)]
FOR i=0,n_elements(tx)-1 DO $
   IF txhi[i]-tx[i] GT tx[i]-txlo[i] THEN push, terr, txhi[i]-tx[i] ELSE push, terr, tx[i]-txlo[i]

;# build the input parameter array
;# a = [mintx, t0, rt, rcool, acool, ap, b, c]
funcname = 'viktx'
a = [4., 10., 2000., 100.0, 5.0, 0.0, 2.0, 3.0]
parnames = ['mintx', 't0', 'rt', 'rcool', 'acool', 'ap', 'b', 'c']
parinfo = replicate({value:0.D, $
                     fixed:0, $
                     limited:[0,0], $
                     limits:[0.D,0.D], $
                     step:[0.D], $
                     tied:['']}, n_elements(parnames))
parinfo(*).value = a

;# set some limits
parinfo(0).limited = [1,1]
parinfo(0).limits  = [0., 8.0]
parinfo(1).limited = [1,1]
parinfo(1).limits  = [2., 40.]
parinfo(2).limited = [1,1]
parinfo(2).limits  = [100., 4000.]
parinfo(3).limited = [1,1]
parinfo(3).limits  = [10., 500.]
parinfo(4).limited = [1,1]
parinfo(4).limits  = [0., 10.]
parinfo(5).limited = [1,1]
parinfo(5).limits  = [-0.2, 0.2]
parinfo(6).limited = [1,1]
parinfo(6).limits  = [0., 5.]
parinfo(7).limited = [1,1]
parinfo(7).limits  = [0.4, 10.]

;# run the fitter
weights = 1./terr^2.
parinfo(*).value = a
print, ''
print, "## STATUS: Running mpcurvefit..."
result = mpcurvefit(rtx, tx, weights, a, sigma, FUNCTION_NAME=funcname, $
                    ITMAX=10000, CHISQ=chisq, PARINFO=parinfo, STATUS=status, $
                    YERROR=residrms, ERRMSG=errmsg, DOF=dof, /DOUBLE, /QUIET, /NODERIVATIVE)
IF status LT 0 THEN BEGIN
   message, errmsg
   EXIT
ENDIF
print, ''
print, "## Fitting complete with no errors."
print, '## Fit parameters:'

;# *If* you can assume that the true reduced chi-squared
;# value is unity -- meaning that the fit is implicitly
;# assumed to be of good quality -- then the estimated
;# parameter uncertainties can be computed by scaling SIGMA
;# by the measured chi-squared value.
sigma = sigma*sqrt(chisq/dof)
resid = tx-result
prob_est = 1.0-IGAMMA((0.5*n_elements(rtx)-3), 0.5*chisq)
openw, /get_lun, lun, funcname+'fit'
FOR jj=0,n_elements(sigma)-1 DO BEGIN
   print, format='(A-10,E10.3,A7,E10.3)',parnames[jj]+': ',a[jj],' +/- ',sigma[jj]
   printf, lun, parnames[jj],'     ',a[jj],'     ',sigma[jj]
ENDFOR
close, lun
print, format='(A-10,E10.3,A7,E10.3)','Residuals:',mean(resid),' +/- ',residrms
print, format='(A-10,I10)','DOF:',dof
print, format='(A-10,F10.3)','ChiSq:',chisq
print, format='(A-10,F10.3)','RChiSq:',chisq/dof
print, format='(A-10,F10.3)','Prob:',prob_est

;# show the fit w/ data
xmin = 0.8*min(rtx)
xmax = 1.2*max(rtx)
ymin = 0.8*min([result,tx])
ymax = 1.2*max([result,tx])
plot, rtx, tx, $
      psym=1, $
      xtitle='Radius [kpc]', $
      ytitle='Tx [keV]', $
      /xsty, /ysty, $
      /xlog, $
      xrange = [xmin,xmax], $
      yrange = [ymin,ymax]
oplot, rtx, result, linestyle=2
yesno = ''
;read, '## INPUT: Press enter to continue', yesno

;# keep originals for plotting
ortx = rtx                      
otx = tx

;# set tx to best-fit function
minrtx = min(ortx)*cmkpc
lowtx = min(otx)
rtx = rdens
mintx = a[0]
t0 = a[1]
rt = a[2]*cmkpc
rcool = a[3]*cmkpc
acool = a[4]
ap = a[5]
b = a[6]
c = a[7]
num1  = ((rtx/rcool)^acool)+(mintx/t0)
den1  = 1.+((rtx/rcool)^acool)
num2  = (rtx/rt)^(-ap)
den2  = (1.+(rtx/rt)^b)^(c/b)
term1 = num1/den1
term2 = num2/den2
tx    = t0*term1*term2
txerr = tx*sqrt(total((sigma/a)^2.))/10000.
ord   = where(rtx LE minrtx[0])
tx[ord] = maken((1.0*lowtx),lowtx,n_elements(rtx[ord]))
dog = indgen(10)+max(ord)
cat = reverse((max(ord)-1)-indgen(10))
fix = [cat,dog]
dum = smooth(tx[fix],10)
tx[fix] = dum

;# show the fit only
xmin = 0.8*min(rtx/cmkpc)
xmax = 1.2*max(rtx/cmkpc)
ymin = 0.8*min(tx)
ymax = 1.2*max(tx)
plot, rtx/cmkpc, tx, $
      xtitle = 'Radius [kpc]', $
      ytitle = 'Tx [keV]', $
      /xlog, /ylog, $
      /xsty, /ysty, $
      xrange = [xmin,xmax], $
      yrange = [ymin,ymax]
oplot, ortx, otx, psym=2
yesno = ''
;read, '## INPUT: Press enter to continue', yesno

;# perform MC err est
print, '## STATUS: Performing '+num2str(nmc)+' Monte Carlo simulations...'
maxind = n_elements(tx)
mctx = fltarr(nmc, maxind)
mc_mean  = fltarr(maxind)
mc_error = fltarr(maxind)
FOR imc = 0, nmc-1 DO BEGIN
   ranarr = randomu(k, n_elements(a), /double)
   mca = a + (sigma*ranarr)
   mintx = mca[0]
   t0 = mca[1]
   rt = mca[2]*cmkpc
   rcool = mca[3]*cmkpc
   acool = mca[4]
   ap = mca[5]
   b = mca[6]
   c = mca[7]
   num1  = ((rtx/rcool)^acool)+(mintx/t0)
   den1  = 1.+((rtx/rcool)^acool)
   num2  = (rtx/rt)^(-ap)
   den2  = (1.+(rtx/rt)^b)^(c/b)
   term1 = num1/den1
   term2 = num2/den2
   mctx[imc,*] = t0*term1*term2
   IF (imc EQ (nmc-1)/4) THEN print, '## STATUS: 25% of MC'
   IF (imc EQ (nmc-1)/2) THEN print, '## STATUS: 50% of MC'
   IF (imc EQ 3*(nmc-1)/4) THEN print, '## STATUS: 75% of MC'
ENDFOR
print, '## STATUS: 100% of MC'
FOR jmc = 0, maxind-1 DO BEGIN
   mc_mean[jmc]  = mean(mctx[*,jmc])
   mc_error[jmc] = stddev(mctx[*,jmc])
ENDFOR
txerr = 0.5 * mctx/mc_mean * mc_error

;# calculate gravitating mass assuming hydro equ and spher sym
;# classical hydro: M_total(< r) = [-(k*T(r)*r)/(G*mu*mh)]*(d(ln T)/ d(ln r) + d(ln dens)/d(ln r))
rmass = rtx
lrtx = alog(rtx)
ltx = alog(tx)
lrdens = alog(rdens)
ldens = alog(dens)
txgrad = deriv(lrtx,ltx)
densgrad = deriv(lrdens,ldens)
tdgrad = txgrad+densgrad

plot, rdens/cmkpc, tdgrad, /xlog
yesno = ''
;read, '## INPUT: Press enter to continue', yesno
mgrav = ((-tx*1000.*ergev*rtx)/(G*mu*mp))*tdgrad
mgraverr = 0.1*mgrav

;# build the input parameter array
;# a = [rs, c, z, delta]
parnames = ['R_s', 'c', 'redshift', 'Delta']
parinfo = replicate({value:0.D, $
                     fixed:0, $
                     limited:[0,0], $
                     limits:[0.D,0.D], $
                     step:[0.D], $
                     tied:['']}, n_elements(parnames))

;# initial guesses
ele = n_elements(rmass)
a = [rmass[ele/2], 3.0, z, delta]
parinfo(2).fixed = 1
parinfo(3).fixed = 1
parinfo(*).value = a

;# run the fitter
weights = mgrav/mgrav
parinfo(*).value = a
print, ''
print, "## STATUS: Running mpcurvefit..."

;# get rid of negatives and nans
ord = where(mgrav GT 0.0)
rnfw = rmass[ord]
mnfw = mgrav[ord]
wnfw = weights[ord]

;# fit
result = mpcurvefit(rnfw, mnfw, wnfw, a, sigma, FUNCTION_NAME='nfw', $
                    ITMAX=10000, CHISQ=chisq, PARINFO=parinfo, STATUS=status, $
                    YERROR=residrms, ERRMSG=errmsg, DOF=dof, /DOUBLE, /QUIET, /NODERIVATIVE)
IF status LT 0 THEN BEGIN
   message, errmsg
   EXIT
ENDIF
print, ''
print, "## Fitting complete with no errors."
print, '## Fit parameters:'
a[0] = a[0]/cmkpc
sigma[0] = sigma[0]/cmkpc

;# *If* you can assume that the true reduced chi-squared
;# value is unity -- meaning that the fit is implicitly
;# assumed to be of good quality -- then the estimated
;# parameter uncertainties can be computed by scaling SIGMA
;# by the measured chi-squared value.
sigma = sigma*sqrt(chisq/dof)
resid = mnfw-result
prob_est = 1.0-IGAMMA((0.5*n_elements(rnfw)-3), 0.5*chisq)
FOR jj=0,n_elements(sigma)-1 DO BEGIN
   print, format='(A-10,E10.3,A7,E10.3)',parnames[jj]+': ',a[jj],' +/- ',sigma[jj]
ENDFOR
print, format='(A-10,E10.3,A7,E10.3)','Residuals:',mean(resid),' +/- ',residrms
print, format='(A-10,I10)','DOF:',dof
print, format='(A-10,F10.3)','ChiSq:',chisq
print, format='(A-10,F10.3)','RChiSq:',chisq/dof
print, format='(A-10,F10.3)','Prob:',prob_est
mnfw = result

;# show the grav mass profile and nfw fit
rmass = rmass/cmkpc
mgrav = mgrav/msun
rnfw = rnfw/cmkpc
mnfw = mnfw/msun
plot, rmass, mgrav, $
      xtitle='Radius [kpc]', $
      ytitle='M( < r) [M_sun]', $
      /xlog, /ylog, $
      /xsty, /ysty, $
      xrange = [20,max(rmass)]
oplot, rnfw, mnfw, linestyle=1
yesno = ''
;read, '## INPUT: Press enter to continue', yesno

;# solve for the grav potential using poisson's eqn
rs = a[0]
c = a[1]
gc = (alog(1.0+c)-(c/(1.0+c)))^(-1.0)
phi = -sqrt(G*max(mnfw)/rs)*gc*(alog(1.0+c*rmass)/rmass)

;# plot everything
rdens = rdens/cmkpc
set_plot, 'PS'
xmin = 0.8*min(rdens)
xmax = 1.2*max(rdens)
ymin = 0.8*min([mgas,mgrav])
ymax = 1.2*max([mgas,mgrav])

xmin=10.0
ymin=1d10
ymax=1d16

device, filename='mass.eps', $
        /encapsulated, $
        /portrait, $
        set_font='Times-Roman'
plot, rdens, mgas, $
      /nodata, $
      /xlog, /ylog, $
      xsty=9, /ysty, $
      xtitle = textoidl('R [kpc]'), $
      ytitle = textoidl('M(r < R) [M'+sunsymbol()+']'), $
      xrange = [xmin,xmax], $
      yrange = [ymin,ymax], $
      charsize = 1.0, $
      position= aspect(1.0)
axis, xaxis = 1, $
      xlog = 1, $
      xrange = [xmin/outerr, xmax/outerr], $
      xsty = 1, $
      xtitle = textoidl('R/R_{'+num2str(delta,3)+'}'), $
      charsize = 1.0
oplot, rmass, mgrav, linestyle=0, thick=2
oplot, rdens, mgas, linestyle=1, thick=2
;oplot, rnfw, mnfw, linestyle=2, thick=2
plotsym, 0, 0.4, /fill
oploterror, rdmass, dmass, dmasserr, psym=8
oplot, rdmass, dmass, psym=8

;# draw legend
items = [textoidl('M_{gas}'), $
         textoidl('M_{grav}')];, $
;         textoidl('M_{NFW}')]
larr = [1,0];,2]
parr = [0,0];,0]
legend, items, linestyle=larr, psym=parr, box=0, charsize=1.0, /bottom, /right, /fill
device, /close

;# plot the gas fraction
ord = where(mgrav GT 0.0)
fgas = mgas[ord]/mgrav[ord]
xmin = min(rmass)
xmax = max(rmass)
ymin = 0.8*min(fgas)
ymax = 1.2*max(fgas)

xmin = 10.0
ymax = 0.2
ymon = 0.0

device, filename='fgas.eps', $
        /encapsulated, $
        /portrait, $
        set_font='Times-Roman'
plot, rmass, fgas, $
      /xlog, $
      /xsty, /ysty, $
      xtitle = textoidl('R [kpc]'), $
      ytitle = textoidl('f_{gas}'), $
      xrange = [xmin,xmax], $
      yrange = [ymin,ymax], $
      linestyle = 0, $
      thick = 2, $
      charsize = 1.0
device, /close
set_plot, 'X'

;# print some info, max(mass) is TOTAL mass within outer radius
print, FORMAT='(A-35, E12.2, A4, E12.2, A8)', '## Mgas (r <= r'+num2str(delta)+'):', max(mgas), '+/-', max(mgaserr), ' M_sun'
print, FORMAT='(A-35, E12.2, A4, E12.2, A8)', '## Mgrav (r <= r'+num2str(delta)+'):', max(mgrav), '+/-', max(mgraverr), ' M_sun'

END
