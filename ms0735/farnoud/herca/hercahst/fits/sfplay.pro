pro sfplay, name, mage, re, n

!p.thick=2.5
!P.charthick = 2.5
!x.thick = 2.5
!y.thick = 2.5

set_plot,'x'
set_plot,'ps'
device, filename='hercasbser.eps', /encapsulated, /times, /color, xs=12, ys=11
loadct, 13

readcol,name,rad,ell,ellerr,pa,paerr,mag,maglerr,maguerr,flux,npix, /silent
s = size(rad,/dimensions)

k = (2*n) - 0.331
f = mage + k* ((rad/re)^(1/n) - 1)

;openw,1,'hercaserrb',/append
;for i = 0, (s[0]-1) do begin
;	printf,1,rad[i],mag[i],f[i],npix[i]
;endfor
;close,1

rad = rad * 0.051
ymax = max(mag)+1
ymin = min(mag)-2
plot, rad, mag, psym=symcat(9), /xlog, yrange=[ymax,ymin], symsize=0.5, ytitle='!4l!6!DV,o!N (mag arcsec!E-2!N)',  xtitle='!6Radius (arcsec)', /ystyle
oploterror,rad,mag,maglerr,/lobar,psym=3,/nohat
oploterror,rad,mag,maguerr,/hibar,psym=3,/nohat
oplot, rad,f, thick=2

device, /close
set_plot,'x'
end
