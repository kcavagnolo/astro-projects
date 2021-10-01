PRO surf_plot, bluename, redname, sbname
;# surf_plot, '../data/hst/606_ellpa.tab', '../data/hst/814_ellpa.tab', '../data/hst/sbprof.tab'
;# surf_plot, '../data/hst/606_ctssb.tab', '../data/hst/814_ctssb.tab', '../data/hst/sbprof.tab'
;# surf_plot, '../data/hst/606_sb_bb25.tab', '../data/hst/814_sb_bb25.tab', '../data/hst/sbprof.tab'
;# surf_plot, '../data/hst/606_sb_bb50.tab', '../data/hst/814_sb_bb50.tab', '../data/hst/sbprof.tab'
;# surf_plot, '../data/hst/606_sb_nocore.tab', '../data/hst/814_sb_nocore.tab', '../data/hst/merged_sb.tab'
;# surf_plot, '../data/hst/606_nomask.tab', '../data/hst/814_nomask.tab', '../data/hst/sbprof.tab'

;# Contents of stsdas ellipse table:
;# row, SMA, INTENS, INT_ERR, PIX_VAR, RMS, ELLIP, ELLIP_E, PA, PA_ERR,
;# X0, X0_ERR, Y0, Y0_ERR, GRAD, GRAD_E, GRAD_R, RSMA, MAG, MAG_LER,
;# MAG_UER, TFLUX_E, TFLUX_C, TMAG_E, TMAG_C, NPIX_E, NPIX_C, A3,
;# A3_ERR, B3, B3_ERR, A4, A4_ERR, B4, B4_ERR, NDATA, NFLAG, NIT, ST,
;# A_BIG, SAREA

;# setup some constants
!quiet=1
galcdiff = 1.05
csize = 1.0                     ;# size of characters
psize = 0.4                     ;# size of points
asecpix = 0.049                 ;# ACS WFC "/pixel
bexp = 1200.                    ;# F606W exposure time
blam = 5907.                    ;# cent wavelength
bflam = 7.8625d-20              ;# photflam keyword erg/ang/cm**2/e-
rexp = 1200.                    ;# F814W exposure time
rlam = 8332.                    ;# cent wavelength
rflam = 7.0723600d-20           ;# photflam keyword erg/ang/cm**2/e-
c = 2.99792458d18               ;# speed of light in Ang/s

;# retrieve the cosmology info
cosmology, 0.354, result, /silent
dl = result[2]*1d6              ;# D_lum in pc
da = result[4]                  ;# D_ang in "/kpc

;# read blue sur bri info
tab_read, bluename, tcb, table, header
bsma = tab_val(tcb, table, 1)
bint = tab_val(tcb, table, 2)
binterr = tab_val(tcb, table, 3)
bell = tab_val(tcb, table, 6)
bellerr = tab_val(tcb, table, 7)
bpa = tab_val(tcb, table, 8)
bpaerr = tab_val(tcb, table, 9)
ord = where(bsma*asecpix*da LE 20.)
bsma = bsma[ord]
bint = bint[ord]
binterr = binterr[ord]
bell = bell[ord]
bellerr = bellerr[ord]
bpa = bpa[ord]
bpaerr = bpaerr[ord]
breff = sqrt(bsma^2.-(bsma^2.*bell))
bsma = (breff*asecpix)+1d-1
ord = where(binterr GT 1d3, num)
IF num GT 0 THEN BEGIN 
   binterr[ord] = 0.0
   binterr[ord] = max(binterr)
ENDIF
bflux = -2.5*alog10(((bint*bflam)*(blam)^2.)/c)-48.60
bfluxerr = bflux*(binterr/bint)
ord = where((bsma*da GE 2) AND (bsma*da LE 10))
mbell = mean(bell[ord])
mbpa = mean(bpa[ord])
dbell = stddev(bell[ord])
dbpa = stddev(bpa[ord])
print, 'Mean ell: ', mbell, ' +/- ', dbell
print, 'Mean PA: ', mbpa, ' +/- ', dbpa

;# read red sur bri info
tab_read, redname, tcb, table, header
rsma = tab_val(tcb, table, 1)
rint = tab_val(tcb, table, 2)
rinterr = tab_val(tcb, table, 3)
rell = tab_val(tcb, table, 6)
rellerr = tab_val(tcb, table, 7)
rpa = tab_val(tcb, table, 8)
rpaerr = tab_val(tcb, table, 9)
ord = where(rsma*asecpix*da LE 20.)
rsma = rsma[ord]
rint = rint[ord]
rinterr = rinterr[ord]
rell = rell[ord]
rellerr = rellerr[ord]
rpa = rpa[ord]
rpaerr = rpaerr[ord]
rreff = sqrt(rsma^2.-(rsma^2.*rell))
rsma = (rreff*asecpix)+1d-1
ord = where(rinterr GT 1d3, num)
IF num GT 0 THEN BEGIN 
   rinterr[ord] = 0.0
   rinterr[ord] = max(rinterr)
ENDIF
rflux = -2.5*alog10(((rint*rflam)*(rlam)^2.)/c)-48.60
rfluxerr = rflux*(rinterr/rint)
ord = where((rsma*da GE 2) AND (rsma*da LE 10))
mrell = mean(rell[ord])
mrpa = mean(rpa[ord])
drell = stddev(rell[ord])
drpa = stddev(rpa[ord])
print, 'Mean ell: ', mrell, ' +/- ', drell
print, 'Mean PA: ', mrpa, ' +/- ', drpa

;# read global sb prof from red+blue image
tab_read, sbname, tcb, table, header
sbsma = tab_val(tcb, table, 1)
sbint = tab_val(tcb, table, 2)
sbinterr = tab_val(tcb, table, 3)
sbell = tab_val(tcb, table, 6)
ord = where(sbsma*asecpix*da LE 20.)
sbsma = sbsma[ord]
sbint = sbint[ord]
sbinterr = sbinterr[ord]
sbell = sbell[ord]
sbreff = sqrt(sbsma^2.-(sbsma^2.*sbell))
sbsma = (sbreff*asecpix)+1d-1
sbsma = sbsma*da
ord = where(sbinterr GT 1d3, num)
IF num GT 0 THEN BEGIN 
   sbinterr[ord] = 0.0
   sbinterr[ord] = max(sbinterr)
   sbint[ord] = 0.0
   sbint[ord] = max(sbint)+0.05*max(sbint)
ENDIF

;# run a nuker fit on sb prof
ord = where(sbsma LE 30.)
x = sbsma[ord]
y = sbint[ord]
yerr = sbinterr[ord]
parnames = ['Irb','Ibgd','alpha','beta','gamma','rb']
fname = 'nuker'
parinfo = replicate({value:0.D, $
                     fixed:0, $
                     limited:[1,1], $
                     limits:[0.D,0.D], $
                     step:[0.D], $
                     tied:['']}, n_elements(parnames))
maxbin = n_elements(y)-1
nukpar = [y[maxbin/3], $
       y[maxbin], $
       1.00, $
       0.66, $
       0.33, $
       x[(maxbin/4)]]
parinfo(0).limits = [0.01*y[0],100.*y[0]]
parinfo(1).limits = [0.D,100.*y[maxbin]]
parinfo(2).limits = [-10.D,10.D]
parinfo(3).limits = [-10.D,10.D]
parinfo(4).limits = [-10.D,10.D]
parinfo(5).limits = [0.D,max(x)]
weights = yerr/yerr
parinfo(*).value = nukpar
print, ''
print, "## STATUS: Running Nuker mpcurvefit..."
nukerfit = mpcurvefit(x, y, weights, nukpar, nsigma, FUNCTION_NAME=fname, $
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
resid = y-nukerfit
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
ord = where((sbsma GE nukpar[5]) AND (sbsma LE 30.))
x = sbsma[ord]
y = sbint[ord]
yerr = sbinterr[ord]
parnames = ['I0','Ibgd','k','n']
fname = 'sersic'
parinfo = replicate({value:0.D, $
                     fixed:0, $
                     limited:[1,1], $
                     limits:[0.D,0.D], $
                     step:[0.D], $
                     tied:['']}, n_elements(parnames))
maxbin = n_elements(y)-1
serpar = [y[0], $
          y[maxbin], $
          2.0, $
          4.0]
parinfo(0).limits = [0.01*y[0],100.*y[0]]
parinfo(1).limits = [0.D,10.*y[maxbin]]
parinfo(2).limits = [0.D,100.D]
parinfo(3).limits = [0.D,20.D]
weights = yerr/yerr
parinfo(*).value = serpar
print, ''
print, "## STATUS: Running Sersic mpcurvefit..."
sersicfit = mpcurvefit(x, y, weights, serpar, ssigma, FUNCTION_NAME=fname, $
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
resid = alog(y)-sersicfit
FOR jj=0,n_elements(ssigma)-1 DO BEGIN
   print, format='(A-10,E10.3,A7,E10.3)',parnames[jj]+': ',serpar[jj],' +/- ',ssigma[jj]
ENDFOR
sprob_est = 1.0-IGAMMA((0.5*n_elements(sbsma)-n_elements(serpar)),0.5*schisq)
print, format='(A-10,E10.3,A7,E10.3)','Residuals:',mean(resid),' +/- ',residrms
print, format='(A-10,I10)','DOF:',sdof
print, format='(A-10,F10.3)','ChiSq:',schisq
print, format='(A-10,F10.3)','RChiSq:',schisq/sdof
print, format='(A-10,F10.3)','Prob:',sprob_est

;# run a linear fit on color grad
rcolor = ((rsma+bsma)/2.)
scolor = bflux-rflux
scolorerr = scolor*sqrt((bfluxerr/bflux)^2.+(rfluxerr/rflux)^2.)
ord = where(rcolor*da LT 10)
gradr = alog10(rcolor[ord])
y = scolor[ord]
err = scolorerr[ord]
parnames = ['G(V-I)','G(0)']
fname = 'colorgrad'
parinfo = replicate({value:0.D, $
                     fixed:0, $
                     limited:[1,1], $
                     limits:[0.D,0.D], $
                     step:[0.D], $
                     tied:['']}, n_elements(parnames))
gradpar = [1.0, $
           max(y)]
parinfo(0).limits = [-10.D,10.D]
parinfo(1).limits = [-10.D,10.D]
weights = err/err
parinfo(*).value = gradpar
print, ''
print, "## STATUS: Running color gradient mpcurvefit..."
gradfit = mpcurvefit(gradr, y, weights, gradpar, gsigma, FUNCTION_NAME=fname, $
                     ITMAX=5000, CHISQ=gchisq, PARINFO=parinfo, STATUS=status, $
                     YERROR=residrms, ERRMSG=errmsg, DOF=gdof, /DOUBLE, /QUIET, /NODERIVATIVE)
IF status LT 0 THEN BEGIN
   message, errmsg
ENDIF
print, ''
print, "## Fitting complete with no errors."
print, '## Fit parameters:'
ord = where(gsigma LE 0, num)
IF num GT 0 THEN gsigma[ord] = 0.1*gradpar[ord]
gsigma = gsigma*sqrt(gchisq/gdof)
resid = y-gradfit
FOR jj=0,n_elements(gsigma)-1 DO BEGIN
   print, format='(A-10,E10.3,A7,E10.3)',parnames[jj]+': ',gradpar[jj],' +/- ',gsigma[jj]
ENDFOR
gprob_est = 1.0-IGAMMA((0.5*n_elements(gradr)-n_elements(gradpar)),0.5*gchisq)
print, format='(A-10,E10.3,A7,E10.3)','Residuals:',mean(resid),' +/- ',residrms
print, format='(A-10,I10)','DOF:',gdof
print, format='(A-10,F10.3)','ChiSq:',gchisq
print, format='(A-10,F10.3)','RChiSq:',gchisq/gdof
print, format='(A-10,F10.3)','Prob:',gprob_est

;# plot sur bri prof and nuker fit to own file
!FANCY    = 4
!LINETYPE = 0
!P.FONT   = 0
!X.THICK  = 1
!Y.THICK  = 1
!Z.THICK  = 1
plotsym, 0, psize, /fill
set_plot, 'PS'
device, filename='surbri.eps', $
        /encapsulated, $
        /portrait, $
        set_font='Times-Roman'
ytex = textoidl('\mu [ct s^{-1} arcsec^{-2}]')
xtex = textoidl('R [kpc]')
xmin = 0.8*min(sbsma)
xmax = 1.2*max(sbsma)
sx = maken(min(sbsma),max(sbsma),1d4)
sy = exp(alog(serpar[0])-(serpar[2]*sx^(1./serpar[3]))+serpar[1])
ymin = 0.8*min(nukpar[1])
ymax = 1.2*max(sy)
plot, sbsma, sbint, $
      /nodata, $
      /xlog, /ylog, $
      /xsty, /ysty, $
      ytitle = ytex, $
      xtitle = xtex, $
      xrange = [xmin,xmax], $
      yrange = [ymin,ymax], $
      charsize = csize
oploterror, sbsma, sbint, sbinterr, psym=8
oplot, sbsma, sbint, psym=8
nx = maken(min(sbsma),max(sbsma),1d4)
r = nx/nukpar[5]
ep = (nukpar[3]-nukpar[4])/nukpar[2]
e = (nukpar[4]-nukpar[3])/nukpar[2]
ny = (nukpar[0]*(2.0^ep)*(r^(-nukpar[4]))*((1.0+r^nukpar[2])^e))+nukpar[1]
oplot, nx, ny, linestyle=1
oplot, sx, sy, linestyle=2
x = maken(1d-4*min(sbsma),1d6,10)
y = replicate(nukpar[1],n_elements(x))
oplot, x, y, linestyle=3
y = maken(1d-4*min(sbint),1d6,10)
x = replicate(nukpar[5],n_elements(y))
oplot, x, y, linestyle=4
items = ['Nuker:', $
         textoidl('I(R_b): '+num2str(nukpar[0])), $
         textoidl('I_{bgd}: '+num2str(nukpar[1])), $
         textoidl('\alpha: '+num2str(nukpar[2])), $
         textoidl('\beta: '+num2str(nukpar[3])), $
         textoidl('\gamma: '+num2str(nukpar[4])), $
         textoidl('R_b: '+num2str(nukpar[5])), $
         textoidl('\chi_{red}^2: '+num2str(nchisq/ndof)), $
         textoidl('Prob_{PDF}: '+num2str(nprob_est)), $
         'Sersic:', $
         textoidl('I_0: '+num2str(serpar[0])), $
         textoidl('I_{bgd}: '+num2str(serpar[1])), $
         textoidl('k: '+num2str(serpar[2])), $
         textoidl('n: '+num2str(serpar[3])), $
         textoidl('\chi_{red}^2: '+num2str(schisq/sdof)), $
         textoidl('Prob_{PDF}: '+num2str(sprob_est))]
linearr = replicate(-99,n_elements(items))
psyarr = replicate(-99,n_elements(items))
legend, items, linestyle=larr, psym=parr, box=0, charsize=0.8, /top, /right, /fill
device, /close

;# Input parameters
!FANCY    = 4
!LINETYPE = 0
!P.FONT   = 0
!X.THICK  = 2
!Y.THICK  = 2
!Z.THICK  = 2
loadct, 39
plotsym, 0, psize;, /fill
set_plot, 'PS'
device, filename='colorsb.eps', $
        /color, $
        /encapsulated, $
        /portrait, $
        set_font='Times-Roman', $
        bits=16
multiplot, [1,2], /square

;# sb prof
ytex = textoidl('\mu [ct s^{-1} pix^{-2}]')
xtex = textoidl('R [kpc]')
xmin = 0.8*min(sbsma)
xmax = 1.2*max(sbsma)
xmax = 1.2*max(rcolor*da)       ;to match other profs
sx = maken(min(sbsma),max(sbsma),1d4)
sy = exp(alog(serpar[0])-(serpar[2]*sx^(1./serpar[3]))+serpar[1])
ymin = 0.8*min(nukpar[1])
ymax = 1.2*max(sy)
ymin = 0.8*min(sbint)
;ymax = 1.2*max(sbint)
plot, sbsma, sbint, $
      /nodata, $
      /xlog, /ylog, $
      /xsty, /ysty, $
      ytitle = ytex, $
;      xtitle = xtex, $
      xrange = [xmin,xmax], $
      yrange = [ymin,ymax], $
      charsize = csize
;oploterror, sbsma, sbint, sbinterr, psym=8
oplot, sbsma, sbint, psym=8
nx = maken(min(sbsma),max(sbsma),1d4)
r = nx/nukpar[5]
ep = (nukpar[3]-nukpar[4])/nukpar[2]
e = (nukpar[4]-nukpar[3])/nukpar[2]
ny = (nukpar[0]*(2.0^ep)*(r^(-nukpar[4]))*((1.0+r^nukpar[2])^e));+nukpar[1]
oplot, nx, ny, linestyle=1
oplot, sx, sy, linestyle=2
x = maken(1d-4*min(sbsma),1d6,10)
y = replicate(nukpar[1],n_elements(x))
oplot, x, y, linestyle=3
y = maken(1d-4*min(sbint),1d6,10)
x = replicate(nukpar[5],n_elements(y))
oplot, x, y, linestyle=1
multiplot

;# color difference
x = rcolor*da
y = scolor
yerr = scolorerr
ytex = textoidl('Color [mag]')
xtex = textoidl('R [kpc]')
xmin = 0.8*min(x)
xmax = 1.2*max(x)
ymin = 0.8*min(y)
ymax = 1.2*max(y)
plot, x, y, $
      /nodata, $
      /xlog, $
      /xsty, /ysty, $
      ytitle = ytex, $
      xtitle = xtex, $
      xrange = [xmin,xmax], $
      yrange = [ymin,ymax], $
      charsize = csize
oploterror, x, y, yerr, psym=8
;ord = where(x LE 10. AND x GE xmin)
;x = x[ord]
;y = y[ord]
;s = n_elements(y)
;colors = round(scale_vector(findgen(s), 250, 50))
;FOR j=0,s-1 DO plots, x[j], y[j], psym=8, color=colors[j]
oplot, x, y, psym=8, color=0
oplot, (10.^(gradr))*da, gradfit, linestyle=2

;# calc gE comparison
;; gebint = [88.98,54.34,33.67,15.91,10.18,137.6,69.96,64.42,83.52]
;; gerint = [134.5,83.8,53.62,26.62,15.17,227.1,110.6,112.7,66.07]
;; ger = maken(1,10,n_elements(gebint))
;; gebflux = -2.5*alog10(((gebint*bflam)*(blam)^2.)/c)-48.60
;; gerflux = -2.5*alog10(((gerint*rflam)*(rlam)^2.)/c)-48.60
;; gecolor = gebflux-gerflux
;; plotsym, 3, 0.6
;; oplot, ger, gecolor, psym=8
;; x = maken(1d-4*min(rcolor),1d6,10)
;; y = replicate(galcdiff,n_elements(x))
;; oplot, x, y, linestyle=3

;; ;# ellipticity
;; plotsym, 0, psize, /fill
;; bx = bsma*da
;; by = bell
;; byerr = bellerr
;; rx = rsma*da
;; ry = rell
;; ryerr = rellerr
;; ytex = textoidl('Ellipticity')
;; xtex = textoidl('R [kpc]')
;; ymin = 0.8*min([by-byerr,ry-ryerr])
;; ymax = 1.1*max([by+byerr,ry+ryerr])
;; plot, rx, ry, $
;;       /nodata, $
;;       /xsty, /ysty, $
;;       /xlog, $
;;       ytitle = ytex, $
;;       xtitle = xtex, $
;;       xrange = [xmin,xmax], $
;;       yrange = [ymin,ymax], $
;;       charsize = csize
;; oploterror, bx, by, byerr, psym=8, color=50
;; oplot, bx, by, psym=8, color=50
;; oploterror, rx, ry, ryerr, psym=8, color=250
;; oplot, rx, ry, psym=8, color=250
;; x = maken(xmin,xmax,10)
;; ;y = replicate(mbell,n_elements(x))
;; ;oplot, x, y, linestyle=3, color=50
;; ;y = replicate(mrell,n_elements(x))
;; ;oplot, x, y, linestyle=3, color=250
;; y = replicate(0.25,n_elements(x))
;; oplot, x, y, linestyle=3, color=0

;; ;# position angle
;; bx = bsma*da
;; by = bpa
;; byerr = bpaerr
;; rx = rsma*da
;; ry = rpa
;; ryerr = rpaerr
;; ytex = textoidl('Position Angle [deg]')
;; xtex = textoidl('R [kpc]')
;; ymin = 1.1*min([by-byerr,ry-ryerr])
;; ymax = 1.1*max([by+byerr,ry+ryerr])
;; plot, rx, ry, $
;;       /nodata, $
;;       /xsty, /ysty, $
;;       /xlog, $
;;       ytitle = ytex, $
;;       xtitle = xtex, $
;;       xrange = [xmin,xmax], $
;;       yrange = [ymin,ymax], $
;;       charsize = csize
;; oploterror, bx, by, byerr, psym=8, color=50
;; oplot, bx, by, psym=8, color=50
;; oploterror, rx, ry, ryerr, psym=8, color=250
;; oplot, rx, ry, psym=8, color=250
;; x = maken(xmin,xmax,10)
;; ;y = replicate(mbpa,n_elements(x))
;; ;oplot, x, y, linestyle=3, color=50
;; ;y = replicate(mrpa,n_elements(x))
;; ;oplot, x, y, linestyle=3, color=250
;; y = replicate(-64,n_elements(x))
;; oplot, x, y, linestyle=3, color=0
device, /close

END
