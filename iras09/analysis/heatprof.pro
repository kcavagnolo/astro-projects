PRO HEATPROF

;# options
dat1 = '/mnt/DROBO/10445/reprocessed/10445_fluxprof_sbprof_2pix.dat'
z = 0.4418
pcolor = 250
psize = 0.5
cavheat = 3.41d44
lcool = 1.56d45
rcool = 116.

;# nice plotting params
!FANCY    = 4
!LINETYPE = 0
!P.FONT   = 0
!X.THICK  = 3
!Y.THICK  = 3
!Z.THICK  = 3

;# read data
readcol, dat1, FORMAT='F,F,F,F,F,D,D,D,D', comment='#', $
         ann, cell, rin, rout, rmid, flux, ferr, lum, lerr
cosmology, z, result, /silent

;# calc radii
r   = rmid*0.492*result[4]
rlo = r-(rin*0.492*result[4])
rhi = (rout*0.492*result[4])-r

;# make errs
flo = ferr
fhi = ferr
llo = lerr
lhi = lerr

;# make lum --> energy/volume
;rcm = r*3.08568025d21
;V = (4.0*!pi*rcm^3.)/3.0
;V = (4.0*!pi*r^3.)/3.0
;lum = lum/V
;llo = llo/V
;lhi = lhi/V

;# make lum(r<R)
dum = 0.0
FOR i=0,n_elements(lum)-1 DO BEGIN
   dum = dum+lum[i]
   push, tlum, dum
ENDFOR
tlum = tlum*0.87

;# plot the flux profile
xmin = 0.8*min(r-rlo)
xmax = 1.2*max(r+rhi)
ymin = 0.8*min(flux)
ymax = 1.2*max(flux)
set_plot, 'PS'
loadct, 13, /silent
device, filename='heat_flux.eps', $
        /color, $
        /encapsulated, $
        /portrait, $
        set_font='Times-Roman'
plotsym, 0, psize, /fill
plot, r, flux, $
      /nodata, $
      /xlog, /ylog, $
      /xsty, /ysty, $
      xtitle = textoidl('R [kpc]'), $
      ytitle = textoidl('Flux [erg s^{-1} cm^{-2}]'), $
      xrange = [xmin,xmax], $
      yrange = [ymin,ymax], $
      psym = 8, $
      charsize = 1.0
oploterror, r, flux, rlo, flo, psym=8, /lobar
oploterror, r, flux, rhi, fhi, psym=8, /hibar
oplot, r, flux, psym=8, color=pcolor
device, /close

;# plot the lum profile
xmin = 0.8*min(r-rlo)
xmax = 1.2*max(r+rhi)
ymin = 0.8*min(tlum)
ymax = 1.2*max(tlum)

xmax = 197.064
xmin = 4.52800
ord = where(r LE xmax)
ymax = 1.2*max(tlum[ord])

set_plot, 'PS'
device, filename='heat_lum.eps', $
        /color, $
        /encapsulated, $
        /portrait, $
        set_font='Times-Roman'
plotsym, 0, psize, /fill
plot, r, tlum, $
      /nodata, $
      /xlog, /ylog, $
      /xsty, /ysty, $
      xtitle = textoidl('R [kpc]'), $
      ytitle = textoidl('L_X(r < R) [erg s^{-1}]'), $
      xrange = [xmin,xmax], $
      yrange = [ymin,ymax], $
      psym = 8, $
      charsize = 1.0
oplot, maken(1d-100,1d100,10), replicate(cavheat,10), linestyle=2, thick=4
oplot, maken(1d-100,1d100,10), replicate(lcool,10), linestyle=2, thick=4
;oplot, replicate(rcool,10), maken(1d-100,1d100,10), linestyle=2
oploterror, r, tlum, rlo, llo, psym=8, /lobar
oploterror, r, tlum, rhi, lhi, psym=8, /hibar
oplot, r, tlum, psym=8, color=0
plotsym, 0, psize-0.2*psize, /fill
oplot, r, tlum, psym=8, color=pcolor
device, /close
set_plot, 'X'

END
