PRO plpan

norm = 'y'

outps = 'pan.eps'
IF norm EQ 'y' THEN outps = 'pannorm.eps'

!FANCY    = 4
!LINETYPE = 0
!P.FONT   = 0
!X.THICK  = 2
!Y.THICK  = 2
!Z.THICK  = 2

a = mrdfits('pan10.fits',1)
x0 = a.ang
y0 = a.sur_bri
y0err = a.sur_bri_err
rat = y0err/y0
IF norm EQ 'y' THEN y0 = y0/max(y0)
y0err = y0*rat

a = mrdfits('pan20.fits',1)
x1 = a.ang
y1 = a.sur_bri
y1err = a.sur_bri_err
rat = y1err/y1
IF norm EQ 'y' THEN y1 = y1/max(y1)
y1err = y1*rat

a = mrdfits('pan30.fits',1)
x2 = a.ang
y2 = a.sur_bri
y2err = a.sur_bri_err
rat = y2err/y2
IF norm EQ 'y' THEN y2 = y2/max(y2)
y2err = y2*rat

set_plot, 'PS'
device, filename=outps, $
        /encapsulated, $
        /portrait, $
        set_font='Times-Roman', $
        bits = 16
xtex = textoidl('Azimuthal Angle [deg.]')
ytex = textoidl('Surface Brightness [ct pix^{-2}]')
IF norm EQ 'y' THEN ytex = textoidl('Normalized Surface Brightness')
multiplot,[3,1], $
          mxtitle = xtex, $
          mytitle = ytex
xmin = 0
xmax = 360
ymin = 0.8*min(y2);-y2err)
ymax = 1.02*max(y2);+y2err)
plotsym, 0, 0.25, /fill
plot, x0, y0, $
      /xsty, /ysty, $
      xrange = [xmin,xmax], $
      yrange = [ymin,ymax], $
      charsize = 1.0, $
      xticks = 4, $
      psym = 10
;oploterror, x0, y0, y0err, psym=8, /nohat
multiplot
plot, x1, y1, $
      /xsty, /ysty, $
      xrange = [xmin,xmax], $
      yrange = [ymin,ymax], $
      charsize = 1.0, $
      xticks = 4, $
      psym = 10
;oploterror, x1, y1, y1err, psym=8, /nohat
multiplot
plot, x2, y2, $
      /xsty, /ysty, $
      xrange = [xmin,xmax], $
      yrange = [ymin,ymax], $
      charsize = 1.0, $
      xticks = 4, $
      psym = 10
;oploterror, x2, y2, y2err, psym=8, /nohat
device, /close

END
