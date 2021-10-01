PRO misslight
z = 0.354
;redname = '../data/hst/814_ellpa.tab'
redname = '../data/hst/iband.tab'

;# setup some constants
rlam = 8332.                    ;# cent wavelength
rflam = 7.07236d-20             ;# photflam keyword erg/ang/cm**2/e-
asecpix = 0.049

;# read red sur bri info
tab_read, redname, tcb, table, header
rsma = tab_val(tcb, table, 1)
rsma[0] = 0.5
rint = tab_val(tcb, table, 2)
rinterr = tab_val(tcb, table, 3)
rflux = tab_val(tcb, table, 18)

;# run a nuker fit on sb prof
ord = where(rsma LE 1d5)
nx = rsma[ord]
ny = rint[ord]
nyerr = rinterr[ord]
plot, nx, ny, /xlog, /ylog, psym=2, $
      /xsty, /ysty, $
      yrange = [1e-2,20]
oploterror, nx, ny, nyerr, psym=2
nyerr = rinterr[ord]
parnames = ['Irb','Ibgd','alpha','beta','gamma','rb']
fname = 'nuker'
parinfo = replicate({value:0.D, $
                     fixed:0, $
                     limited:[1,1], $
                     limits:[0.D,0.D], $
                     step:[0.D], $
                     tied:['']}, n_elements(parnames))
maxbin = n_elements(ny)-1
nukpar = [2.4, $
          0.004, $
          -2.5, $
          -0.3, $
          2.0, $
          0.3]
parinfo(0).limits = [0.01*ny[0],100.*ny[0]]
parinfo(1).limits = [0.D,100.*ny[maxbin]]
parinfo(2).limits = [-10.D,10.D]
parinfo(3).limits = [-10.D,10.D]
parinfo(4).limits = [-10.D,10.D]
parinfo(5).limits = [0.D,max(nx)]
weights = nyerr/nyerr
parinfo(*).value = nukpar
print, ''
print, "## STATUS: Running Nuker mpcurvefit..."
nukerfit = mpcurvefit(nx, ny, weights, nukpar, nsigma, FUNCTION_NAME=fname, $
                      ITMAX=5000, CHISQ=nchisq, PARINFO=parinfo, STATUS=status, $
                      YERROR=residrms, ERRMSG=errmsg, DOF=ndof, /DOUBLE, /QUIET, /NODERIVATIVE)
IF status LT 0 THEN BEGIN
   message, errmsg
ENDIF
print, ''
print, "## Fitting complete with no errors."
print, '## Fit parameters:'
ord = where(nsigma LE 0, num)
IF num GT 0 THEN nsigma[ord] = 0.1*nukpar[ord]
nsigma = nsigma*sqrt(nchisq/ndof)
resid = ny-nukerfit
FOR jj=0,n_elements(nsigma)-1 DO BEGIN
   print, format='(A-10,E10.3,A7,E10.3)',parnames[jj]+': ',nukpar[jj],' +/- ',nsigma[jj]
ENDFOR
nprob_est = 1.0-IGAMMA((0.5*n_elements(x)-n_elements(nukpar)),0.5*nchisq)
print, format='(A-10,E10.3,A7,E10.3)','Residuals:',mean(resid),' +/- ',residrms
print, format='(A-10,I10)','DOF:',ndof
print, format='(A-10,F10.3)','ChiSq:',nchisq
print, format='(A-10,F10.3)','RChiSq:',nchisq/ndof
print, format='(A-10,F10.3)','Prob:',nprob_est

;# run a sersic fit on sb prof
ord = where(rsma GE nukpar[5])
sx = rsma[ord]
sy = rint[ord]
syerr = rinterr[ord]
parnames = ['I0','Ibgd','k','n']
fname = 'sersic'
parinfo = replicate({value:0.D, $
                     fixed:0, $
                     limited:[1,1], $
                     limits:[0.D,0.D], $
                     step:[0.D], $
                     tied:['']}, n_elements(parnames))
maxbin = n_elements(sy)-1
serpar = [sy[0], $
          1d-2, $
          1.4, $
          2.6]
parinfo(0).limits = [0.01*sy[0],100.*sy[0]]
parinfo(1).limits = [0.D,10.*sy[maxbin]]
parinfo(2).limits = [0.D,100.D]
parinfo(3).limits = [0.D,20.D]
weights = syerr/syerr
parinfo(*).value = serpar
print, ''
print, "## STATUS: Running Sersic mpcurvefit..."
sersicfit = mpcurvefit(sx, sy, weights, serpar, ssigma, FUNCTION_NAME=fname, $
                       ITMAX=5000, CHISQ=schisq, PARINFO=parinfo, STATUS=status, $
                       YERROR=residrms, ERRMSG=errmsg, DOF=sdof, /DOUBLE, /QUIET, /NODERIVATIVE)
IF status LT 0 THEN BEGIN
   message, errmsg
ENDIF
print, ''
print, "## Fitting complete with no errors."
print, '## Fit parameters:'
ord = where(ssigma LE 0, num)
IF num GT 0 THEN ssigma[ord] = 0.1*serpar[ord]
ssigma = ssigma*sqrt(schisq/sdof)
resid = alog(sy)-sersicfit
FOR jj=0,n_elements(ssigma)-1 DO BEGIN
   print, format='(A-10,E10.3,A7,E10.3)',parnames[jj]+': ',serpar[jj],' +/- ',ssigma[jj]
ENDFOR
sprob_est = 1.0-IGAMMA((0.5*n_elements(sbsma)-n_elements(serpar)),0.5*schisq)
print, format='(A-10,E10.3,A7,E10.3)','Residuals:',mean(resid),' +/- ',residrms
print, format='(A-10,I10)','DOF:',sdof
print, format='(A-10,F10.3)','ChiSq:',schisq
print, format='(A-10,F10.3)','RChiSq:',schisq/sdof
print, format='(A-10,F10.3)','Prob:',sprob_est

;# build nuker function
nx = maken(0,nukpar[5]+1,1d5)
nx[0] = 1d-6
r = nx/nukpar[5]
ep = (nukpar[3]-nukpar[4])/nukpar[2]
e = (nukpar[4]-nukpar[3])/nukpar[2]
ny = (nukpar[0]*(2.0^ep)*(r^(-nukpar[4]))*((1.0+r^nukpar[2])^e))+nukpar[1]

;# build sersic function
sx = nx
sy = exp(alog(serpar[0])-(serpar[2]*sx^(1./serpar[3]))+serpar[1])

;# plot
oplot, nx, ny, linestyle=2
oplot, sx, sy, linestyle=3

;# do the integrations
nukint = int_tabulated(nx^2., ny)
serint = int_tabulated(sx^2., sy)

;# convert to mags
nflux = nukint*rflam*rlam
nmag = -2.5*alog10(((nukint*rflam)*(rlam)^2.)/2.99792458d18)-48.60
nlum  = flux2lum(nflux, z)/3.839d33
print, 'Nuk mag < r_b: ', nmag

sflux = serint*rflam*rlam
smag = -2.5*alog10(((serint*rflam)*(rlam)^2.)/2.99792458d18)-48.60
slum  = flux2lum(sflux, z)/3.839d33
print, 'Ser mag < r_b: ', smag

diff  = slum-nlum
print, 'Missing light: ', diff, ' L_sol'
print, 'Missing mass: ', 3*diff, ' M_sol'

END
