PRO color, colorname, sbname
;# color, '../data/hst/color.tab', '../data/hst/sbprof.tab'

;# setup some constants
!quiet=1
galcdiff = 1.05
csize = 1.0                     ;# size of characters
psize = 0.4                     ;# size of points
asecpix = 0.049                 ;# ACS WFC "/pixel
c = 2.99792458d18               ;# speed of light in Ang/s

;# retrieve the cosmology info
cosmology, 0.354, result, /silent
dl = result[2]*1d6              ;# D_lum in pc
da = result[4]                  ;# D_ang in "/kpc

;# read blue sur bri info
tab_read, colorname, tcb, table, header
bsma = tab_val(tcb, table, 1)
bint = tab_val(tcb, table, 2)
binterr = tab_val(tcb, table, 3)
bell = tab_val(tcb, table, 6)
bellerr = tab_val(tcb, table, 7)
bpa = tab_val(tcb, table, 8)
bpaerr = tab_val(tcb, table, 9)
rmax = max(bsma*asecpix*da)
ord = where(bsma*asecpix*da LE 20.)
bsma = bsma[ord]
bint = bint[ord]
binterr = binterr[ord]
bell = bell[ord]
bellerr = bellerr[ord]
bpa = bpa[ord]
bpaerr = bpaerr[ord]
breff = sqrt(bsma^2.-(bsma^2.*bell))
bsma = ((breff*asecpix)+1d-1)*da
ord = where(binterr GT 1d3, num)
IF num GT 0 THEN BEGIN 
   binterr[ord] = 0.0
   binterr[ord] = max(binterr)
ENDIF

;# read global sb prof from red+blue image
tab_read, sbname, tcb, table, header
sbsma = tab_val(tcb, table, 1)
sbint = tab_val(tcb, table, 2)
sbinterr = tab_val(tcb, table, 3)
sbell = tab_val(tcb, table, 6)
ord = where(sbsma*asecpix*da LE rmax)
sbsma = sbsma[ord]
sbint = sbint[ord]
sbinterr = sbinterr[ord]
sbell = sbell[ord]
sbreff = sqrt(sbsma^2.-(sbsma^2.*sbell))
sbsma = ((sbreff*asecpix)+1d-1)*da
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
          1.0, $
          3.0]
parinfo(0).limits = [0.01*y[0],100.*y[0]]
parinfo(1).limits = [0.D,y[maxbin]]
parinfo(2).limits = [0.D,50.D]
parinfo(3).limits = [0.D,10.D]
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
rcolor = bsma
scolor = bint
scolorerr = binterr
ord = where(rcolor LT 10)
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

;# Input parameters
!FANCY    = 4
!LINETYPE = 0
!P.FONT   = 0
!X.THICK  = 2
!Y.THICK  = 2
!Z.THICK  = 2
loadct, 39
plotsym, 0, psize
set_plot, 'PS'
device, filename='colorsb.eps', $
        /encapsulated, $
        /portrait, $
        set_font='Times-Roman'
multiplot, [1,2], /square

;# sb prof
ytex = textoidl('\mu [ct s^{-1} pix^{-2}]')
xtex = textoidl('R [kpc]')
xmin = 0.8*min(sbsma)
xmax = 1.2*max(rcolor)
sx = maken(min(sbsma),max(sbsma),1d4)
sy = exp(alog(serpar[0])-(serpar[2]*sx^(1./serpar[3]))+serpar[1])
ymin = 0.8*min(sbint)
ymax = 1.2*max(sy)
plot, sbsma, sbint, $
      /nodata, $
      /xlog, /ylog, $
      /xsty, /ysty, $
      ytitle = ytex, $
      xrange = [xmin,xmax], $
      yrange = [ymin,ymax], $
      charsize = csize
oplot, sbsma, sbint, psym=8
nx = maken(min(sbsma),max(sbsma),1d4)
r = nx/nukpar[5]
ep = (nukpar[3]-nukpar[4])/nukpar[2]
e = (nukpar[4]-nukpar[3])/nukpar[2]
ny = (nukpar[0]*(2.0^ep)*(r^(-nukpar[4]))*((1.0+r^nukpar[2])^e))
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
x = rcolor
y = scolor
yerr = scolorerr
ytex = textoidl('V^{\prime}-I^{\prime} [mag pix^{-2}]')
xtex = textoidl('R [kpc]')
xmin = 0.8*min(x)
xmax = 1.2*max(x)
ymin = 0.95*min(y)
ymax = 1.05*max(y)
plot, x, y, $
      /nodata, $
      /xlog, $
      /xsty, /ysty, $
      ytitle = ytex, $
      xtitle = xtex, $
      xrange = [xmin,xmax], $
      yrange = [ymin,ymax], $
      charsize = csize
oploterror, x, y, yerr, psym=8, /nohat
oplot, x, y, psym=8, color=0
oplot, (10.^(gradr)), gradfit, linestyle=2
device, /close

END
