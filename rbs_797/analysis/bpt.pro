PRO bpt

csize = 1.5
psize = 1.0
x = maken(1d-2,100,1d5)
y = 10^((0.14/(alog10(x)-1.45))+0.83)
ferr = 0.2
oiii5007 = -16.8
oiii5007err = ferr*oiii5007
oiii4959 = -5.14
oiii4959err = ferr*oiii4959
oii = -166
oiierr = ferr*oii
hbabs = -4.6
hbeta = -15.4+hbabs
hbetaerr = ferr*hbeta

x1 = oii/hbeta
x1err = x1*sqrt((oiierr/oii)^2.+(hbetaerr/hbeta)^2.)
x1err = x1err/x1
x1 = alog10(x1)

y1 = oiii5007/hbeta
y1err = x1*sqrt((oiii5007err/oiii5007)^2.+(hbetaerr/hbeta)^2.)
y1err = y1err/y1
y1 = alog10(y1)

r23 = (oiii5007+oiii4959+oii)/hbeta
r23err = r23*sqrt((oiii5007err/oiii5007)^2.+(oiii4959err/oiii4959)^2.+(oiierr/oii)^2.+(hbetaerr/hbeta)^2.)
r23err = r23err/r23
r23 = alog10(r23)

o32 = (oiii5007+oiii4959)/oii
o32err = o32*sqrt((oiii5007err/oiii5007)^2.+(oiii4959err/oiii4959)^2.+(oiierr/oii)^2.)
o32err = o32err/o32
o32 = alog10(o32)

;# Input parameters
!FANCY    = 4
!LINETYPE = 0
!P.FONT   = 0
!X.THICK  = 2
!Y.THICK  = 2
!Z.THICK  = 2

;# BPT
set_plot, 'PS'
device, filename='bpt.eps', $
        /encapsulated, $
        /portrait, $
        set_font='Times-Roman', $
        bits=16
ytex = textoidl('log ([OIII]\lambda5007/H\beta)')
xtex = textoidl('log ([OII]\lambda3727/H\beta)')
xmin = 0.5
xmax = 1.5
ymin = -1.5
ymax = 0.5
plot, alog10(x), alog10(y), $
      /xsty, /ysty, $
      ytitle = ytex, $
      xtitle = xtex, $
      xrange = [xmin,xmax], $
      yrange = [ymin,ymax], $
      POSITION = ASPECT(1.0), $
      thick = 2, $
      charsize = csize
plotsym, 0, psize, /fill
oplot, [x1,-100], [y1,-100], psym=8
oploterror, [x1,0], [y1,0], [x1err,0], [y1err,0], psym=8
XYOUTS, 0.7, -0.5, CharSize=csize, textoidl('Starbursts')
XYOUTS, 1.3, 0.2, CharSize=csize, textoidl('AGN')
device, /close

;# O32-R23
set_plot, 'PS'
device, filename='o32r23.eps', $
        /encapsulated, $
        /portrait, $
        set_font='Times-Roman', $
        bits=16
y = 10^((1.5/(alog10(x)-1.7))+2.4)
ytex = textoidl('log O_{32}')
xtex = textoidl('log R_{23}')
;ytex = textoidl('log ([OIII]\lambda\lambda4959,5007/[OII]\lambda3727)')
;xtex = textoidl('log (([OIII]\lambda\lambda4959,5007+[OII]\lambda3727)/H\beta)')
plot, alog10(x), alog10(y), $
      /xsty, /ysty, $
      ytitle = ytex, $
      xtitle = xtex, $
      xrange = [xmin,xmax], $
      yrange = [ymin,ymax], $
      POSITION = ASPECT(1.0), $
      thick = 2, $
      charsize = csize
plotsym, 0, psize, /fill
oplot, [r23,-100], [o32,-100], psym=8
oploterror, [r23,-100], [o32,-100], [r23err,0], [o32err,0], psym=8
XYOUTS, 0.7, -0.5, CharSize=csize, textoidl('Starbursts')
XYOUTS, 1.3, 0.2, CharSize=csize, textoidl('AGN')
device, /close

END
