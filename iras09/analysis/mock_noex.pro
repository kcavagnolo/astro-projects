PRO mock_noex

inim  = '../data/IRAS_09104+4109_norm.fits'
sbdat = 'normsb.dat'
ptnorm = 3.7649358e-06
outim = 'norm_beta.fits'
dim   = 1000

;inim = 'soft_img.fits'
;sbdat = 'softsb.dat'
;ptnorm = 56.
;outim = 'soft_beta.fits'
;dim = 201.
xoff  = 3.7
yoff  = -4.5
alpha = -1.0
xfoff=1
yfoff=2

readcol,sbdat,rsbr,sbr,sbrerr,FORMAT='F,F,F',comment='#'
ord = where(sbr GT 0)
rsbr = rsbr[ord]
sbr = sbr[ord]
sbrerr = sbrerr[ord]
weights = 1./sbrerr^2.
parnames = ['S01','S02','rc1','rc2','beta1','beta2','bgd']
parunits = ['cts/s/pix**2','cts/s/pix**2','pix','pix','-','-','cts/s/pix**2']
fname = 'double_beta'
parinfo = replicate({value:0.D, $
                     fixed:0, $
                     limited:[1,1], $
                     limits:[0.D,0.D], $
                     step:[0.D], $
                     tied:['']}, n_elements(parnames))
ina = [sbr[0], $
       0.8*sbr[0], $
       0.25*max(rsbr), $
       0.50*max(rsbr), $
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
IF n_elements(result) LT n_elements(rsbr) THEN BEGIN
   message, "mpcurvefit choked on something. Check by hand if you want a fit."
   RETURN
ENDIF
xmin = 0.9*min(rsbr)
xmax = 1.1*max(rsbr)
ymax = 1.1*max(sbr)
ymin = 0.9*min(sbr)
plot, rsbr, sbr, $
      xtitle = 'R [pix]', $
      ytitle = 'SB [cts/s/pix**2]', $
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
s01   = ina[0]
s02   = ina[1]
rc1   = ina[2]
rc2   = ina[3]
beta1 = ina[4]
beta2 = ina[5]
bgd   = ina[6]

;# get image info
a = mrdfits(inim,0,hdr)
x = findgen(dim)#replicate(1.,dim)
y = transpose(x)

;# beta model
r = sqrt(((x+xfoff)-(dim/2))^2.+((y+yfoff)-(dim/2))^2.)
term1 = s01*(1.0+(r/rc1)^2.0)^(0.5-3.0*beta1)
term2 = s02*(1.0+(r/rc2)^2.0)^(0.5-3.0*beta2)
img1  = term1+term2+bgd

;# point source
norm = ptnorm
alpha = -2.5
img2 = norm*r^(alpha)
ord = where(img2 EQ max(img2))
img2[ord] = norm

;# excess
r = sqrt(((x+xoff)-(dim/2))^2.+((y+yoff)-(dim/2))^2.)
norm = 3.0*(s01+s02)
img3 = norm*r^(alpha)
ord = where(img3 EQ max(img3))
img3[ord] = norm

;# final image
image = img1;+img2;+img3
mwrfits, image, outim, hdr, /create

END
