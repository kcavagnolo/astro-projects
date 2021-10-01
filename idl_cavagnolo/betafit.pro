;+
; NAME:
;
; PURPOSE:
;
; CALLING SEQUENCE:
;   BETAFIT, mode, fitsfile
;
; INPUTS:
;   mode = 'single' or 'double'
;   fitsfile = surface brightness profile extracted using CIAO
;
; OPTIONAL KEYWORD INPUTS:
;
; OUTPUTS:
;
; EXAMPLE:
;   betafit,'single','/mnt/DROBO/merged/RBS_0797/2202_7902_sbprof_2pixell.fits'
;
; MODIFICATION HISTORY:
;   Written by: KWC, UW, Apr, 2010
;-

;#####################
;#####################

PRO betafit, mode, fitsfile

;# return to caller on error
ON_ERROR, 2
IF (n_params() NE 2) THEN BEGIN
   doc_library, 'betafit'
   RETURN
ENDIF

;# read data
a = mrdfits(fitsfile,1)
rsbr = a.rmid
rsbr = rsbr*0.492
sbr = a.sur_bri
expt = a.exposure
expt = expt[0]
sbr = sbr/0.492^2./expt
sbrerr = a.sur_bri_err/0.492^2./expt

;# get rid of negatives
ord = where(sbr GT 0)
rsbr = rsbr[ord]
sbr = sbr[ord]
sbrerr = sbrerr[ord]
weights = 1.D/sbrerr^2.

;# plot the surface brightness profile
xmin = 0.9*min(rsbr)
xmax = 1.1*max(rsbr)
ymax = 1.1*max(sbr)
ymin = 0.9*min(sbr)
plot, rsbr, sbr, $
      xtitle = 'R [arcsec]', $
      ytitle = 'SB [cts/s/arcsec**2]', $
      psym = 10, $
      /xsty, /ysty, $
      /XLOG, /YLOG, $
      xran = [xmin,xmax], $
      yran = [ymin,ymax], $
      linestyle = 0
plotsym, 0, 0.8, /fill
oplot, rsbr, sbr, psym=8
oploterr, rsbr, sbr, sbrerr


;# get limits for fitting
read, '## INPUT: Enter min and max R: ', minfit, maxfit
use = where(rsbr LE maxfit AND rsbr GT minfit)
rsbr = rsbr[use]
sbr = sbr[use]
sbrerr = sbrerr[use]
xmin = 0.9*min(rsbr)
xmax = 1.1*max(rsbr)
ymax = 1.1*max(sbr)
ymin = 0.9*min(sbr)
plot, rsbr, sbr, $
      xtitle = 'R [arcsec]', $
      ytitle = 'SB [cts/s/arcsec**2]', $
      psym = 10, $
      /xsty, /ysty, $
      /XLOG, /YLOG, $
      xran = [xmin,xmax], $
      yran = [ymin,ymax], $
      linestyle = 0
plotsym, 0, 0.8, /fill
oplot, rsbr, sbr, psym=8
oploterr, rsbr, sbr, sbrerr
yesno = ''
read, '## INPUT: Press enter to continue', yesno
endpt = n_elements(sbr)-1
junk = sbr
endpt = junk[endpt]
maxbin = where(sbr eq endpt)

;# create a smoothed version of the sbr profile if wanted
origsbr = sbr
yesno = ''
read, '## INPUT: Smooth this? (yes=y): ', yesno
IF (yesno eq 'y') THEN BEGIN
   REPEAT BEGIN
      read, '## INPUT: Input smoothing length (integer): ',nsm
      sbr = smooth(origsbr,nsm)
      plot, rsbr, origsbr, $
            xtitle = 'R_{mid} [arcsec]', $
            ytitle = 'SB [cts/sec/arcsec**2]', $
            psym = 10, $
            /xsty, /ysty, $
            /XLOG, /YLOG, $
            xran = [xmin,xmax], $
            yran = [ymin,ymax], $
            linestyle = 0
      plotsym, 0, 0.8, /fill
      oplot, rsbr, sbr, psym=8
      oploterr, rsbr, sbr, sbrerr
      read, '## INPUT: Re-try smoothing (no=n): ', yesno
   ENDREP UNTIL (yesno eq 'n')
ENDIF

;# build models
IF mode EQ 'single' THEN BEGIN
   parnames = ['S0','rc','beta','bgd']
   parunits = ['cts/s/arcsec**2','arcsec','-','cts/s/arcsec**2']
   fname = 'single_beta'
   parinfo = replicate({value:0.D, $
                        fixed:0, $
                        limited:[1,1], $
                        limits:[0.D,0.D], $
                        step:[0.D], $
                        tied:['']}, n_elements(parnames))
   ina = [sbr[0], $
          rsbr[(maxbin/10)], $
          0.5D, $
          (min(sbr))]
   parinfo(0).limits = [0.1*ina[0],1.1*ina[0]]
   parinfo(1).limits = [0.D,max(rsbr)]
   parinfo(2).limits = [0.D,5.D]
   parinfo(3).limits = [0.D,0.5*ina[0]]
ENDIF ELSE IF mode EQ 'double' THEN BEGIN
   parnames = ['S01','S02','rc1','rc2','beta1','beta2','bgd']
   parunits = ['cts/s/arcsec**2','cts/s/arcsec**2','arcsec','arcsec','-','-','cts/s/arcsec**2']
   fname = 'double_beta'
   parinfo = replicate({value:0.D, $
                        fixed:0, $
                        limited:[1,1], $
                        limits:[0.D,0.D], $
                        step:[0.D], $
                        tied:['']}, n_elements(parnames))
   ina = [sbr[0], $
          0.5*sbr[0], $
          0.25*max(rsbr), $
          0.75*max(rsbr), $
          0.5D, $
          1.0D, $
          (min(sbr))]
   parinfo(0).limits = [0.6*ina[0],1.4*ina[0]]
   parinfo(1).limits = [0.D,ina[0]]
   parinfo(2).limits = [0.D,0.5*max(rsbr)]
   parinfo(3).limits = [0.5*max(rsbr),max(rsbr)]
   parinfo(4).limits = [0.D,5.D]
   parinfo(5).limits = [0.D,5.D]
   parinfo(6).limits = [0.D,0.5*ina[1]]
ENDIF ELSE BEGIN
   message, 'You gave me a bad mode, need either -single- or -double-.'
   RETURN
ENDELSE

;# run the fitter
parinfo(*).value = ina
print, ''
print, "## STATUS: Running mpcurvefit..."
result = mpcurvefit(rsbr, sbr, weights, ina, sigma, FUNCTION_NAME=fname, $
                    ITMAX=5000, CHISQ=chisq, PARINFO=parinfo, STATUS=status, $
                    YERROR=residrms, ERRMSG=errmsg, DOF=dof, /DOUBLE, /NODERIVATIVE, /QUIET)
IF status LT 0 THEN BEGIN
   message, errmsg
   RETURN
ENDIF
print, ''
print, "## Fitting complete with no errors."
print, '## Fit parameters:'
ord = where(sigma LE 0, num)
IF num GT 0 THEN sigma[ord] = 0.1*ina[ord]
sigma = sigma*sqrt(chisq/dof)
resid = sbr-result
FOR jj=0,n_elements(sigma)-1 DO BEGIN
   print, format='(A-10,E10.3,A7,E10.3,A25)',parnames[jj]+': ',ina[jj],' +/- ',sigma[jj],parunits[jj]
ENDFOR
print, format='(A-10,E10.3,A7,E10.3)','Residuals:',mean(resid),' +/- ',residrms
print, format='(A-10,I10)','DOF:',dof
print, format='(A-10,F10.3)','ChiSq:',chisq
print, format='(A-10,F10.3)','RChiSq:',chisq/dof    

;# check for error from too few bins and skip ahead
IF n_elements(result) LT n_elements(rsbr) THEN BEGIN
   message, "mpcurvefit choked on something. Check by hand if you want a fit."
   RETURN
ENDIF

xmin = 0.9*min(rsbr)
xmax = 1.1*max(rsbr)
ymax = 1.1*max(sbr)
ymin = 0.9*min(sbr)
plot, rsbr, sbr, $
      xtitle = 'R [arcsec]', $
      ytitle = 'SB [cts/s/arcsec**2]', $
      psym = 10, $
      /xsty, /ysty, $
      /XLOG, /YLOG, $
      xran = [xmin,xmax], $
      yran = [ymin,ymax], $
      linestyle = 0
plotsym, 0, 0.8, /fill
oplot, rsbr, sbr, psym=8
oploterr, rsbr, sbr, sbrerr
oplot, rsbr, result, linestyle=0

END
