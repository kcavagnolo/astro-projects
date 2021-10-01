PRO nic1

dat1 = 'nic1_f090.dat'
dat2 = 'ACS_F814W.dat'
dat3 = 'wfc3_ir_f098m_002_th.fits'
dat4 = 'wfc3_uvis_f218w_002_syn.fits'
dat5 = 'wfpc2_f622w_005_syn.fits'
readcol, dat1, format='F,F', f090n_lamb, f090n_thru
readcol, dat2, format='F,F', f814w_lamb, f814w_thru
a = mrdfits(dat3, 1)
f098m_lamb = a.wavelength*1d-10/1d-6
f098m_thru = -1.0*(a.emissivity-1.0)
a = mrdfits(dat4, 1)
f218w_lamb = a.wavelength*1d-10/1d-6
f218w_thru = a.throughput
a = mrdfits(dat5, 1)
f622w_lamb = a.wavelength*1d-10/1d-6
f622w_thru = a.throughput
f814w_lamb = f814w_lamb*1d-10/1d-6
f090n_lamb = f090n_lamb/1000.
f090n_thru = f090n_thru/100.
!FANCY    = 4
!LINETYPE = 0
!P.FONT   = 0
!X.THICK  = 3
!Y.THICK  = 3
!Z.THICK  = 3
xmin = 0.5
xmax = 1.1
ymin = 0.0
ymax = 1.2
set_plot, 'PS'
loadct, 13, /silent
device, filename='nic1.eps', $
        /color, $
        /encapsulated, $
        /portrait, $
        set_font='Times-Roman'
plot, f090n_lamb, f090n_thru, $
      /nodata, $
      /xsty, /ysty, $
      xtitle = textoidl('Wavelength [microns]'), $
      ytitle = textoidl('Transmissivity'), $
      xrange = [xmin,xmax], $
      yrange = [ymin,ymax], $
      charsize = 1.0, $
      position= aspect(1.0)

;$ filters
oplot, f622w_lamb, f622w_thru, linestyle=0, thick=5, color=62
oplot, f814w_lamb, f814w_thru, linestyle=0, thick=5, color=124
oplot, f090n_lamb, f090n_thru, linestyle=0, thick=5, color=186
oplot, f098m_lamb, f098m_thru, linestyle=0, thick=5, color=248
;oplot, f218w_lamb, f218w_thru, linestyle=0, thick=3, color=0

;# lines
;civ = 1549*1.4418*1d-10/1d-6
;oplot, replicate(civ, 10), maken(-1,ymax-0.1,10), linestyle=0, thick=2, color=0
;xyouts, civ-0.0025, ymax-0.09, textoidl('C IV'), charsize=0.7, alignment=0.5

;oii1 = 3726*1.4418*1d-10/1d-6
;oii2 = 3729*1.4418*1d-10/1d-6
;oplot, replicate(oii1, 10), maken(-1,ymax-0.1,10), linestyle=0, thick=1, color=0
;oplot, replicate(oii2, 10), maken(-1,ymax-0.1,10), linestyle=0, thick=1, color=0
;xyouts, oii2-0.005, ymax-0.09, textoidl('[OII]'), charsize=0.7, alignment=0.5

oiii1 = 4959*1.4418*1d-10/1d-6
oiii2 = 5007*1.4418*1d-10/1d-6
oplot, replicate(oiii1, 10), maken(-1,ymax-0.1,10), linestyle=0, thick=1, color=0
oplot, replicate(oiii2, 10), maken(-1,ymax-0.1,10), linestyle=0, thick=1, color=0
xyouts, oiii2-0.005, ymax-0.09, textoidl('[OIII]'), charsize=0.7, alignment=0.5

ha = 6563*1.4418*1d-10/1d-6
oplot, replicate(ha, 10), maken(-1,ymax-0.1,10), linestyle=0, thick=1, color=0
xyouts, ha, ymax-0.09, textoidl('H\alpha'), charsize=0.7, alignment=0.5

;sii1   = 6716*1.4418*1d-10/1d-6
;sii2   = 6731*1.4418*1d-10/1d-6
;oplot, replicate(sii1, 10), maken(-1,ymax-0.1,10), linestyle=0, thick=1, color=0
;oplot, replicate(sii2, 10), maken(-1,ymax-0.1,10), linestyle=0, thick=1, color=0
;xyouts, sii2-0.005, ymax-0.09, textoidl('[SII]'), charsize=0.7, alignment=0.5

;# legend
items = [textoidl('WFC2/F622W'), $
         textoidl('WFC2/F814W'), $
         textoidl('NIC1/F090M'), $
         textoidl('WFC3/F098M')]
legend, items, /fill, psym=[6,6,6,6], colors=[62,124,186,248], $
        box=0, charsize=0.7, thick=8, /top, /left

;# redraw axes
plot, [xmin,xmax], [ymin,ymax], $
      /nodata, $
      /noerase, $
      /xsty, /ysty, $
      xrange = [xmin,xmax], $
      yrange = [ymin,ymax], $
      charsize = 1.0, $
      position= aspect(1.0)
device, /close
set_plot, 'X'
END
