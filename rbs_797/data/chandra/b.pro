PRO b

necav = mrdfits('nesb.fits',1,head)
swcav = mrdfits('swsb.fits',1,head)
scale = 0.492

nesb    = necav.sur_bri
nesberr = necav.sur_bri_err
frac    = nesberr/nesb
nebgsb  = necav.bg_sur_bri
nebgsberr = necav.bg_sur_bri_err
nermid  = necav.rmid*scale

nesb    = nesb/max(nesb)
nesberr = frac*nesb

swsb    = swcav.sur_bri
swsberr = swcav.sur_bri_err
frac    = swsberr/swsb
swbgsb  = swcav.bg_sur_bri
swbgsberr = swcav.bg_sur_bri_err
swrmid  = swcav.rmid*scale

swsb    = swsb/max(swsb)
swsberr = frac*swsb

;# Input parameters
!FANCY    = 4
!LINETYPE = 0
!P.FONT   = 0
!X.THICK  = 2
!Y.THICK  = 2
!Z.THICK  = 2
set_plot, 'PS'
device, filename='cavsb.eps', $
        /encapsulated, $
        /portrait, $
        set_font='Times-Roman'
ytex = textoidl('Normalized Surface Brightness')
xtex = textoidl('R_{mid} [arcsec]')
multiplot, [1,2], mxtitle=xtex, mytitle=ytex
plotsym, 0, 0.5, /fill
xmin = 0.0
xmax = 7.0
ymin = 0.3
ymax = 1.1
plot, nermid, nesb, $
      /xsty, /ysty, $
      xrange = [xmin,xmax], $
      yrange = [ymin,ymax], $
      charsize = 1.0
oploterror, nermid, nesb, nesberr, psym=8
items = [textoidl('NE-Cavity')]
linearr = replicate(-99,n_elements(items))
psyarr = replicate(-99,n_elements(items))
legend, items, linestyle=larr, psym=parr, box=0, charsize=0.8, /top, /left, /fill

multiplot
plotsym, 0, 0.5, /fill
plot, swrmid, swsb, $
      /xsty, /ysty, $
      xrange = [xmin,xmax], $
      yrange = [ymin,ymax], $
      charsize = 1.0
oploterror, swrmid, swsb, swsberr, psym=8
items = [textoidl('SW-Cavity')]
linearr = replicate(-99,n_elements(items))
psyarr = replicate(-99,n_elements(items))
legend, items, linestyle=larr, psym=parr, box=0, charsize=0.8, /top, /left, /fill
device, /close

END
