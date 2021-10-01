PRO sed

z = 0.4418
mz = 0.042170

z = 0.0
mz = 0.0

;# read data
dat1 = '../data/sed.dat'
readcol, dat1, FORMAT='A,D,D,D,A,A', comment='#', $
         band, freq, flux, ferr, type, eline

;# de-redshift
freq = freq*(1+z)

;# convert to nu*f_nu
rat = ferr/flux
flux = freq*flux*1d-23
ferr = flux*rat

;# plot device
!FANCY    = 4
!LINETYPE = 0
!P.FONT   = 0
!X.THICK  = 3
!Y.THICK  = 3
!Z.THICK  = 3
xmin = 0.25*min(freq)
xmax = 1.75*max(freq)
ymin = 0.1*min(flux)
ymax = 10.0*max(flux)
set_plot, 'PS'
loadct, 39, /silent
device, filename='sed.eps', $
        /color, $
        /encapsulated, $
        /portrait, $
        set_font='Times-Roman'
plot, [xmin,xmax], [ymin,ymax], $
      /nodata, $
      /xlog, /ylog, $
      /xsty, /ysty, $
      xtitle=textoidl('\nu [Hz]'), $
      ytitle=textoidl('\nu f_{\nu} [erg cm^{-2} s^{-1}]'), $
      xrange = [xmin,xmax], $
      yrange = [ymin,ymax], $
      charsize = 1.0

;# plot mrk231 sed
dat1 = '../data/mrk231_sed.dat'
readcol, dat1, FORMAT='A,D,D,D,A,A', comment='#', $
         mband, mfreq, mflux, mferr, mtype, meline
mfreq = mfreq*(1+mz)
mflux = mfreq*mflux*1d-23
oplot, mfreq, mflux*0.014830508, linestyle=0, color=250
;plotsym, 0, 0.5, /fill
;oplot, mfreq, mflux*0.014830508, psym=8, color=250

;# look for upper limits
check = flux-ferr
ulord = where(check EQ 0, ulnum)
IF ulnum GT 0 THEN BEGIN
   ulx = freq[ulord]
   uly = flux[ulord]
ENDIF

;# check for lines
liord = where(type EQ 'line', linum)
IF linum GT 0 THEN BEGIN
   linesx = freq[liord]
   linesy = flux[liord]
   ltype  = eline[liord]
ENDIF

;# destroy entries
IF ulnum GT 0 THEN BEGIN
   flux[ulord] = 0.0
   ferr[ulord] = 0.0
ENDIF

;# plot
plotsym, 0, 0.75, /fill
oplot, freq, flux, psym=8
oploterror, freq, flux, ferr, psym=8

;# plot lines
IF n_elements(linesx) GT 0 THEN BEGIN
   xyouts, 1.15*linesx, 1.5*linesy, ltype, orientation=90, charsize=0.75
ENDIF

;# plot upper limits
IF n_elements(ulx) GT 0 THEN BEGIN
   oplot, ulx, uly, psym=8
   plotsym, 1, 2.0
   oplot, ulx, uly, psym=8
ENDIF

;# plot IR dividers
;; divs = [4.3d11, 3.8d14, 7.9d14, 3d17, 1d20]
;; FOR i=0,n_elements(divs)-1 DO BEGIN
;;    x = replicate(divs[i],2)
;;    y = [1d-100,1d100]
;;    oplot, x, y, linestyle=1
;; ENDFOR

;# read sed
readcol, 'bfseds', FORMAT='A', comment='#', sfiles
FOR i=0,n_elements(sfiles)-1 DO BEGIN
   dat = sfiles[i]
   readcol, dat, FORMAT='F,F', $
            sedfreq, sedflux

   ;# convert to freq, redshift, and correct for d_lum
   sedfreq = 2.99792458d8/(sedfreq*1d-6)
   sedfreq = sedfreq/(1+z)
   cosmology, 0.4418, result, /silent
   dl = result[2]
   sedflux = sedflux*(50./dl)^2.0
   sedflux = sedfreq*sedflux*1d-23

   ;# plot sed
   oplot, sedfreq, sedflux, linestyle=0, color=50
ENDFOR

;; ;# plot radio spec
;; nu = maken(1d2,1d20,1000)
;; fnu = 8d6*nu^(-1.0)
;; oplot, nu*(1+z), fnu, linestyle=1

;; ;# plot xray spec
;; nu = maken(1d2,1d20,1000)
;; fnu = 10^(8.6526562)*nu^(-0.99570045)
;; oplot, nu*(1+z), fnu, linestyle=2

;; ;# plot one plaw
;; nu = maken(1d2,1d20,1000)
;; fnu = 10^(5.2252149)*nu^(-0.81332283)
;; oplot, nu*(1+z), fnu, linestyle=0

;# plot radio spec fits
;; readcol,'../data/radiospec.dat.KP',format='D,F',kfreq,kflux,comment='#'
;; kflux = kfreq*kflux*1d-6*1d-23
;; oplot, kfreq, kflux
;; readcol,'../data/radiospec.dat.JP',format='D,F',jfreq,jflux,comment='#'
;; jflux = jfreq*jflux*1d-6*1d-23
;; oplot, jfreq, jflux
;; readcol,'../data/radiospec.dat.CI',format='D,F',cfreq,cflux,comment='#'
;; cflux = cfreq*cflux*1d-6*1d-23
;; oplot, cfreq, cflux

;# close file
device, /close

END
