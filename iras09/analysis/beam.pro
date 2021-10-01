PRO beam

sa = 0.702d-3
srms = 48d-6
k = 3.0
alpha = 1.0
knot = 0.6359
z = 0.4418
ftime = 7d4
lftime = 0.5d6
ftheta = maken(0., 70., 100)*(!PI/180.)
beta = maken(0., 0.4, 5)
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
xmin = 0.
xmax = 90.
ymin = 1.
ymax = 200.
plot, [xmin,xmax], [ymin,ymax], $
      /nodata, $
      /xsty, /ysty, $
      /ylog, $
      xtitle = textoidl('\theta [deg]'), $
      ytitle = textoidl('Flux Ratio'), $
      xrange = [xmin,xmax], $
      yrange = [ymin,ymax], $
      linestyle = 0, $
      position= aspect(1.0)
for i=0,n_elements(beta)-1 do begin
   x = beta[i]*cos(theta)
   num = 1.0+x
   den = 1.0-x
   jrat = (num/den)^(k+alpha)
   oplot, theta*(180./!PI), jrat, linestyle=0
endfor
oplot, [xmin,xmax], [4,4], linestyle=2
oplot, [34,34], [ymin,ymax], linestyle=2
oplot, [60,60], [ymin,ymax], linestyle=2
device, /close

set_plot,'ps'
device, filename='beam2.eps', $
        /encapsulated, $
        /portrait, $
        set_font='Times-Roman'
plotsym, 0, 1.0, /fill
dtheta = ftheta*180./!PI
xmin = min(dtheta)
xmax = max(dtheta)
ymin = 0.
ymax = 0.6
plot, [xmin,xmax], [ymin,ymax], $
      /nodata, $
      /xsty, /ysty, $
      xtitle = textoidl('\theta [deg]'), $
      ytitle = textoidl('\beta'), $
      xrange = [xmin,xmax], $
      yrange = [ymin,ymax], $
      linestyle = 0, $
      position= aspect(1.0)

cosmology, z, result, /silent
print, FORMAT='(A-20,F10.3,A6)', 'Proj Knot-4 [kpc]:', knot*result[4]
dproj = knot*result[4]*3.08568025d19
dtrue = dproj/sin(ftheta)
time = ftime*3.1556926d7
vel = dtrue/time/2.99792458d8
oband, dtheta, vel, 1d10*dtheta, color=220
ltime = lftime*3.1556926d7
vel = dtrue/ltime/2.99792458d8
oband, dtheta, -1d10*dtheta, vel, color=140

num = ((sa/(3*srms))^(1./(k+alpha)))-1
den = ((sa/(3*srms))^(1./(k+alpha)))+1
bcost = num/den
div = cos(ftheta)
thetamax = atan((dproj/2.99792458d8/time)*(den/num))
vmin = bcost/cos(thetamax)
dmin = dproj/sin(thetamax)/3.08568025d19
oplot, dtheta, bcost/div, linestyle=0, thick=2
plotsym, 0, 1.2, /fill
;oplot, [-1,thetamax*180./!PI], [-1,vmin], psym=8
print, 'k = 3'
print, FORMAT='(A-20,F10.3,A6)', 'b cos(t):', bcost
print, FORMAT='(A-20,F10.3,A6)', 'b(34):', min(bcost/div)
print, FORMAT='(A-20,F10.3,A6)', 'b(60):', max(bcost/div)
print, FORMAT='(A-20,F10.3,A6)', 'theta_max [deg]:', thetamax*180./!PI
print, FORMAT='(A-20,F10.3,A6)', 'v_min [c]:', vmin
print, FORMAT='(A-20,F10.3,A6)', 'd_min [kpc]:', dmin

;; k = 2.0
;; num = ((sa/(3*srms))^(1./(k+alpha)))-1
;; den = ((sa/(3*srms))^(1./(k+alpha)))+1
;; bcost = num/den
;; thetamin = atan((dproj/2.99792458d8/time)*(den/num))
;; vmax = bcost/cos(thetamin)
;; dmax = dproj/sin(thetamin)/3.08568025d19
;; oplot, dtheta, bcost/div, linestyle=2, thick=2
;; plotsym, 0, 1.2, /fill
;; ;oplot, [-1,thetamin*180./!PI], [-1,vmax], psym=8
;; print, 'k = 2'
;; print, FORMAT='(A-20,F10.3,A6)', 'b cos(t):', bcost
;; print, FORMAT='(A-20,F10.3,A6)', 'b(34):', min(bcost/div)
;; print, FORMAT='(A-20,F10.3,A6)', 'b(60):', max(bcost/div)
;; print, FORMAT='(A-20,F10.3,A6)', 'theta_min [deg]:', thetamin*180./!PI
;; print, FORMAT='(A-20,F10.3,A6)', 'v_max [c]:', vmax
;; print, FORMAT='(A-20,F10.3,A6)', 'd_min [kpc]:', dmax

;; psyarr = replicate(0,2)
;; linarr = [2,0]
;; items = [textoidl('k = 2'), $
;;          textoidl('k = 3')]
;; legend, items, linestyle=linarr, box=0, thick=2, charsize=1.0, /top, /left
;; xyouts, 45, 0.13, textoidl('500 > t_{k4} > 70 kyr'), charsize=1.2, alignment=0.5
oplot, [34, 34], [0,1], linestyle=1, thick=4
oplot, [60, 60], [0,1], linestyle=1, thick=4

device, /close

END
