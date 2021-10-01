PRO mock

x  = (findgen(10)*0.05)+0.05

;# for mock linear tx, mint=1.5 keV, constant norm=1d-4, cr=0.1
y1 = [21.7,22.4,24.2,25.9,26.4,27.3,32.1,37.7,37.6,42.8]
y1err=[0.3,0.5,1.1,1.3,1.8,1.5,2.5,2.7,3.6,3.6]

;# for mock linear tx, mint=1.5 keV, variable norm, cr
;y2 = [20.4,19.2,20.0,20.8,20.2,21.3,22.8,22.6,22.8,23.5]
;y2err=[0.5,0.8,0.9,0.9,1.5,2.0,1.4,1.2,0.8,1.2]

set_plot, 'PS'
device, filename = 'idl.ps', $
        /encapsulated, $
        /helvetica
!FANCY    = 4
!LINETYPE = 0
!P.FONT   = 0
!X.THICK  = 3
!Y.THICK  = 3
!Z.THICK  = 3

plotsym, 0, 0.8, /fill
plot, x, y1, $
      /xsty, $
      /ysty, $
      xrange=[0,0.55], $
      yrange=[15,50], $
      psym=8, $
      charsize=1.0, $
      xtitle='Redshift', $
      ytitle=textoidl('K_0 [keV cm^2]')
oploterror, x, y1, y1err, psym=8
;plotsym, 4, 0.8, /fill
;oplot, x, y2, psym=8
;oploterror, x, y2, y2err, psym=8

END
