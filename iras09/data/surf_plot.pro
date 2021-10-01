PRO surf_plot, sbname

;# Contents of stsdas ellipse table:
;# row, SMA, INTENS, INT_ERR, PIX_VAR, RMS, ELLIP, ELLIP_E, PA, PA_ERR,
;# X0, X0_ERR, Y0, Y0_ERR, GRAD, GRAD_E, GRAD_R, RSMA, MAG, MAG_LER,
;# MAG_UER, TFLUX_E, TFLUX_C, TMAG_E, TMAG_C, NPIX_E, NPIX_C, A3,
;# A3_ERR, B3, B3_ERR, A4, A4_ERR, B4, B4_ERR, NDATA, NFLAG, NIT, ST,
;# A_BIG, SAREA

;# setup some constants
csize = 0.8                     ;# size of characters
psize = 0.4                     ;# size of points
asecpix = 0.492                 ;# "/pixel

;# retrieve the cosmology info
cosmology, 0.442, result, /silent
dl = result[2]*1d6              ;# D_lum in pc
da = result[4]                  ;# D_ang in "/kpc

;# read global sb prof from red+blue image
tab_read, sbname, tcb, table, header
sbsma = tab_val(tcb, table, 1)*asecpix+1d-1
sbsma = sbsma*da
sbint = tab_val(tcb, table, 2)
sbinterr = tab_val(tcb, table, 3)
ord = where(sbinterr GT 1d3, num)
IF num GT 0 THEN BEGIN 
   sbinterr[ord] = 0.0
   sbinterr[ord] = max(sbinterr)
   sbint[ord] = sbint[ord+1]
   sbint[ord] = max(sbint)
ENDIF
sbell = tab_val(tcb, table, 6)
sbellerr = tab_val(tcb, table, 7)
ord = where((sbellerr GT 1d3), num)
IF num GT 0 THEN BEGIN 
   sbellerr[ord] = 0.0
   sbellerr[ord] = max(sbellerr)
   sbell[ord] = sbell[ord+1]
   sbell[ord] = max(sbell)
ENDIF
sbpa = tab_val(tcb, table, 8)
sbpaerr = tab_val(tcb, table, 9)
ord = where((sbpaerr GT 1d3), num)
IF num GT 0 THEN BEGIN 
   sbpaerr[ord] = 0.0
   sbpaerr[ord] = max(sbpaerr)
   sbpa[ord] = sbpa[ord+1]
   sbpa[ord] = max(sbpa)
ENDIF


;# Input parameters
!FANCY    = 4
!LINETYPE = 0
!P.FONT   = 0
!X.THICK  = 2
!Y.THICK  = 2
!Z.THICK  = 2
loadct, 39
plotsym, 0, psize, /fill
set_plot, 'PS'
device, filename='xraysb.eps', $
        /color, $
        /encapsulated, $
        /portrait, $
        set_font='Times-Roman', $
        bits=16
!P.MULTI = [0,2,2,0]

;# sb prof
plotsym, 0, psize, /fill
x = sbsma*da
y = sbint
yerr = sbinterr
ytex = textoidl('\mu [cts s^{-1} arcsec^{-2}]')
xtex = textoidl('R [kpc]')
xmin = 0.8*min(x)
xmax = 1.2*max(x)
ymin = 0.8*min(y)
ymax = 1.2*max(y)
plot, x, y, $
      /nodata, $
      /xlog, /ylog, $
      /xsty, /ysty, $
      ytitle = ytex, $
      xtitle = xtex, $
      xrange = [xmin,xmax], $
      yrange = [ymin,ymax], $
      charsize = csize
oploterror, x, y, yerr, psym=8
oplot, x, y, psym=8

;# ellipticity
x = sbsma*da
y = sbell
yerr = sbellerr
ytex = textoidl('Ellipticity')
xtex = textoidl('R [kpc]')
xmin = 0.8*min(x)
xmax = 1.2*max(x)
ymin = 0.8*min(y)
ymax = 1.2*max(y)
plot, x, y, $
      /nodata, $
      /xlog, $
      /xsty, /ysty, $
      ytitle = ytex, $
      xtitle = xtex, $
      xrange = [xmin,xmax], $
      yrange = [ymin,ymax], $
      charsize = csize
oploterror, x, y, yerr, psym=8
oplot, x, y, psym=8

;# position angle
x = sbsma*da
y = sbpa
yerr = sbpaerr
ytex = textoidl('Position Angle [deg]')
xtex = textoidl('R [kpc]')
xmin = 0.8*min(x)
xmax = 1.2*max(x)
ymin = 0.8*min(y)
ymax = 1.2*max(y)
plot, x, y, $
      /nodata, $
      /xlog, $
      /xsty, /ysty, $
      ytitle = ytex, $
      xtitle = xtex, $
      xrange = [xmin,xmax], $
      yrange = [ymin,ymax], $
      charsize = csize
oploterror, x, y, yerr, psym=8
oplot, x, y, psym=8
device, /close

END
