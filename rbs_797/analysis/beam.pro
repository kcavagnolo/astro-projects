PRO beam

beta = maken(0.1, 0.7, 7)                    ; velocity in units 'c' i.e. 0.1c, 0.2c, etc.
alpha = 1.0
theta = maken(0., 90., 100)*(!PI/180.)

!FANCY    = 4
!LINETYPE = 0
!P.FONT   = 0
!X.THICK  = 3
!Y.THICK  = 3
!Z.THICK  = 3
set_plot,'ps'
device, filename='beam.eps', $
        /encapsulated, $
        /portrait, $
        set_font='Times-Roman'
plotsym, 0, 1.0, /fill
xmin = 0
xmax = 90
ymin = 1.
ymax = 200.
plot, [xmin,xmax], [ymin,ymax], $
      /nodata, $
      /xsty, /ysty, $
      /ylog, $
      xtitle = textoidl('\theta [deg]'), $
      ytitle = textoidl('Beaming Ratio'), $
      xrange = [xmin,xmax], $
      yrange = [ymin,ymax], $
      linestyle = 0, $
      position= aspect(1.0)

for i=0,n_elements(beta)-1 do begin
   x = beta[i]*cos(theta)
   num = 1.0+x
   den = 1.0-x
   jrat = (num/den)^(2.0+alpha)
   oplot, theta*(180./!PI), jrat, linestyle=0
endfor

device, /close

END
