PRO uvis

dat4 = 'wfc3_uvis_f218w_002_syn.fits'
dat6 = 'cosncm3_g225mc2233_005_syn.fits'
a = mrdfits(dat4, 1)
f218w_lamb = a.wavelength*1d-10/1d-9
f218w_thru = a.throughput
a = mrdfits(dat6, 1)
g225m_lamb = a.wavelength*1d-10/1d-9
g225m_thru = a.throughput*10.0

!FANCY    = 4
!LINETYPE = 0
!P.FONT   = 0
!X.THICK  = 3
!Y.THICK  = 3
!Z.THICK  = 3
xmin = 190
xmax = 250
ymin = 0.0
ymax = 0.25
set_plot, 'PS'
loadct, 13, /silent
device, filename='uvis.eps', $
        /color, $
        /encapsulated, $
        /portrait, $
        set_font='Times-Roman'
plot, f218w_lamb, f218w_thru, $
      /nodata, $
      /xsty, ystyle=9, $
      xtitle = textoidl('Wavelength [nm]'), $
      ytitle = textoidl('WFC3 Transmissivity'), $
      xrange = [xmin,xmax], $
      yrange = [ymin,ymax], $
      charsize = 1.0, $
      position= aspect(1.0)
axis, yaxis = 1, $
      yrange = [ymin/10., ymax/10.], $
      ysty = 1, $
      ytitle = textoidl('COS Transmissivity'), $
      charsize = 1.0

;$ filters
oplot, f218w_lamb, f218w_thru, linestyle=0, thick=5, color=50
oplot, g225m_lamb, g225m_thru, linestyle=0, thick=5, color=250

;# lines
civ = 1549*1.4418*1d-10/1d-9
oplot, replicate(civ, 10), maken(-1,ymax-0.018,10), linestyle=0, thick=2, color=0
xyouts, civ, ymax-0.016, textoidl('C IV'), charsize=0.7, alignment=0.5

;# legend
items = [textoidl('WFC3/F218W'), $
         textoidl('COS/G225M')]
legend, items, /fill, psym=[6,6], colors=[50,250], $
        box=0, charsize=0.7, thick=8, /top, /left

;# redraw axes
plot, [xmin,xmax], [ymin,ymax], $
      /nodata, $
      /noerase, $
      /xsty, ystyle=9, $
      xrange = [xmin,xmax], $
      yrange = [ymin,ymax], $
      charsize = 1.0, $
      position= aspect(1.0)
axis, yaxis = 1, $
      yrange = [ymin/10., ymax/10.], $
      ysty = 1, $
      charsize = 1.0
device, /close
set_plot, 'X'
END
