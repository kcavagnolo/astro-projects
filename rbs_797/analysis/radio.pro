PRO radio

!FANCY    = 4
!LINETYPE = 0
!P.FONT   = 0
!X.THICK  = 3
!Y.THICK  = 3
!Z.THICK  = 3

;# Myriam Gitti's #'s
s_g = [26.04, 2.63, 3.02]       
s_gerr = [0.26, 0.03, 0.03]
nu_g = [1400., 4800., 8400.]*1d6

;# Laura Birzan's #'s
s_b = [0.104, 0.021, 0.0042, 0.0026]*1000.
s_berr = [0.006, 0.001, 0.0003, 0.0001]*1000.
nu_b = [327., 1400., 4500., 8500.]*1d6

;# make a plot
set_plot, 'PS'
device, filename='radio.eps', $
        /encapsulated, $
        /portrait, $
        set_font='Times-Roman'
xmin = 0.8*min(nu_b)
xmax = 1.2*max(nu_b)
ymin = 0.8*min([(s_g-s_gerr),(s_b-s_berr)])
ymax = 1.2*max([(s_g+s_gerr),(s_b+s_berr)])
plotsym, 3, 0.8, /fill
plot, nu_b, s_b, $
      /xlog, /ylog, $
      /xsty, /ysty, $
      xtitle = textoidl('Frequency [Hz]'), $
      ytitle = textoidl('Flux [mJy]'), $
      xrange = [xmin,xmax], $
      yrange = [ymin,ymax], $
      psym = 8, $
      charsize = 1.0
oploterror, nu_b, s_b, s_berr, psym=8
plotsym, 8, 0.8
oplot, nu_g, s_g, psym=8
oploterror, nu_g, s_g, s_gerr, psym=8
device, /close

END
