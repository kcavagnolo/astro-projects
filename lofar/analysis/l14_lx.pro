PRO l14_lx

af = 0.159
bf = 1.97
rscal = 3.16d24
xscal = 1d45

set_plot,'ps'
device, filename='pr_px.eps', $
        /encapsulated, $
        /portrait, $
        set_font='Times-Roman'
plotsym, 0, 1.0, /fill
xmin = 41
xmax = 46
ymin = 16
ymax = 27
lx = maken(1d1^(xmin),1d1^(xmax),1000)
prad = exp(af)*rscal*xscal^(-bf)*lx^(bf)
;prad = rscal*(1d1^(af+(bf*alog10(lx/xscal))))
plot, alog10(lx), alog10(prad), $
      /xsty, /ysty, $
      xtitle = textoidl('Log [L_{(0.5-2.0 keV)} erg s^{-1}]'), $
      ytitle = textoidl('Log [P_{(1.4 GHz)} W Hz^{-1}]'), $
      xrange = [xmin,xmax], $
      yrange = [ymin,ymax], $
      linestyle = 0, $
      thick = 2, $
      position= aspect(1.0), $
      charsize = 1.5
lmin = alog10(2.646d44)
oplot, [lmin,lmin], [0,100], linestyle=1, thick=2
oplot, [43.1,43.1], [0,100], linestyle=2, thick=2
oplot, [41.2,41.2], [0,100], linestyle=2, thick=2
device, /close

lx = 1d43
print, 'For 1d43, L1.4 ~ ', rscal*1d1^(af+(bf*alog10(lx/xscal)))
lx = 1d41
print, 'For 1d41, L1.4 ~ ', rscal*1d1^(af+(bf*alog10(lx/xscal)))

END
