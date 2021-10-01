PRO sed

;# options
csize = 1.5
dat1 = 'sed.dat'
dat2 = 'bc95_a_10E9.fits'
z = 0.354
c = 2.99792458d8                ;# m/s
cosmology, z, result, /silent
odl = result[2]*3.08568025d24   ;# D_L in cm
angstrom = STRING(197B)

;# read data
readcol, dat1, FORMAT='A,D,D,D,A,A', comment='#', $
         band, freq, flux, ferr, type, eline
bc = mrdfits(dat2,1)

;# de-redshift, convert from mJy to Jy then to nu*f_nu
rat = ferr/flux
flux = freq*(4.0*!PI*odl^2*flux*1d-26)
ferr = flux*rat

;# normalized the BC template
bcfreq = c/(bc.wavelength*1d-10)/(1+z) ;# convert from Ang to m to 1/s and redshift
bcflux = bc.flux
ord = where(band EQ 'K')
nflux = flux[ord]
nfreq = freq[ord]
ord = where((bcfreq GE 0.995*nfreq[0]) AND (bcfreq LE 1.005*nfreq[0]))
norm = bcflux[ord]
bcflux = (bcflux/norm[0])*nflux[0]

;# plot device
!FANCY    = 4
!LINETYPE = 0
!P.FONT   = 0
!X.THICK  = 3
!Y.THICK  = 3
!Z.THICK  = 3
xmin = 0.5*min(freq)
xmax = 1.5*max(freq)
ymin = 0.5*min(flux)
ymax = 2.0*max(flux)
set_plot, 'PS'
loadct, 39, /silent
xtex1 = textoidl('\nu [Hz]')
ytex = textoidl('\nu L_{\nu} [erg s^{-1}]')
xtex2 = textoidl('\lambda ['+angstrom+']')
device, filename='sed.eps', $
        /color, $
        /encapsulated, $
        /portrait, $
        set_font='Times-Roman'
plot, [xmin,xmax], [ymin,ymax], $
      /nodata, $
      /xlog, /ylog, $
      xsty=9, /ysty, $
      xtitle=xtex1, $
      ytitle=ytex, $
      xrange = [xmin,xmax], $
      yrange = [ymin,ymax], $
      position = aspect(1.0), $
      charsize = csize
axis, xaxis = 1, $
      xlog = 1, $
      xrange = [c/xmin/1d-10, c/xmax/1d-10], $
      xsty = 1, $
      xtitle = xtex2, $
      charsize = csize

;# plot BC & KC gE sed models
oplot, bcfreq, bcflux, linestyle=0, color=250

;# read sed
readcol, 'bfseds', FORMAT='A', comment='#', sfiles
FOR i=0,n_elements(sfiles)-1 DO BEGIN
   dat = sfiles[i]
   readcol, dat, FORMAT='F,F', $
            sedfreq, sedflux

   ;# convert to freq, redshift, and correct for d_lum
   cosmology, z, result, /silent
   dl = result[2]
   sedflux = sedflux*(50./dl)^2.0
   sedfreq = c/(sedfreq*1d-6)/(1+z)
   sedflux = sedfreq*4.0*!PI*odl^2*sedflux*1d-23

   ;# plot sed
   oplot, sedfreq, sedflux, linestyle=0, color=50, thick=2
ENDFOR

;# look for upper limits
check = flux-ferr
ulord = where(check LE 0, ulnum)
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
   xyouts, 1.15*linesx, 1.5*linesy, ltype, orientation=90, charsize=csize
ENDIF

;# plot upper limits
IF n_elements(ulx) GT 0 THEN BEGIN
   oplot, ulx, uly, psym=8
   plotsym, 1, 2.0, thick=2
   oplot, ulx, uly, psym=8
ENDIF

;# some line...?
;oplot, replicate(1.3784d15,2), maken(1d-100,1d100,2), linestyle=1

;# 4000 A break
oplot, replicate(c/4000d-10/(1+z),2), maken(1d-100,1d100,2), linestyle=2, thick=3

;# close file
device, /close

END

;; %%IncludeResource: font Symbol
;; 423.333 /Symbol STDFONT
;; (l) show
;; %%IncludeResource: font Times-Roman
;; 423.333 /Times-Roman STDFONT
;; ( [) show CP
;; %%IncludeResource: font Helvetica-Narrow-BoldOblique
;; 423.333/Times-Roman STDFONT
;; (A) show M X 280 M
;; %%IncludeResource: font Symbol
;; 150 /Symbol STDFONT
;; (   o  ) show X 0 M
;; %%IncludeResource: font Helvetica-Narrow-BoldOblique
;; 423.333 /Times-Roman STDFONT
;; (]) show grestore

;; xfreq = [4.8360d17, 2.4180d18]
;; xmid = 0.5*(xfreq[0]+xfreq[1])
;; xflux = 2.7077d-13
;; xferr = 0.5928d-13
;; xrat = xferr/xflux
;; xflux = 4.0*!PI*odl^2*xflux
;; xferr = xflux*xrat

;; ;# plot X-ray flux
;; oplot, [1d-100, xmid], [1d-100,xflux], psym=8
;; oploterror, [1d-100, xmid], [1d-100,xflux], xferr, psym=8
;; oplot, maken(xfreq[0],xfreq[1],2), replicate(xflux,2
