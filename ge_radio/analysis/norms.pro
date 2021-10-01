PRO norms

x = [151., 300., 1400., 5000.]
y = [6.46d41, 3.47d43, 8.13d43, 6.20d44]

fx = alog10(x)
fy = alog10(y)
fyerr = [0.1*fy[0], 0.2*fy[1], 0.1*fy[2], 0.1*fy[3]]

expr = 'P[0] + X*P[1]'
p0 = [1.D, 0.5D]
p = mpfitexpr(expr, fx, fy, fyerr, p0, perror=msig, /quiet)
print, 'MPFIT:'
print, FORMAT='(A-15,F10.4,A6,F10.4)', 'slope:', p[1], '+/-', msig[1]
print, FORMAT='(A-15,F10.4,A6,F10.4)', 'y-intercept:', p[0], '+/-', msig[0]

;# plot the log space dist
set_plot, 'PS'
device, filename='norms.eps', $
        /encapsulated, $
        /portrait, $
        set_font='Times-Roman', $
        bits=16
ytex = textoidl('log \eta [erg s^{-1}]')
xtex = textoidl('log \nu [MHz]')
xmin = 2
xmax = 4
ymin = 41
ymax = 45
plotsym, 0, /fill
plot, fx, fy, $
      /xsty, /ysty, $
      ytitle = ytex, $
      xtitle = xtex, $
      xrange = [xmin,xmax], $
      yrange = [ymin,ymax], $
      position= ASPECT(1.0), $
      psym = 8, $
      charsize = 1.0
x = maken(-100,100,10)
y = p[1]*x + p[0]
oplot, x, y, linestyle=0

x = alog10(327.)
y = p[1]*x + p[0]
print, 'For x=327 MHz, norm=',10.0^(y),' erg/s, log10(norm)=',y/42.

x = [x,1d-4]
y = [y,1d-4]
plotsym, 0
oplot, x, y, psym=8

device, /close

END
