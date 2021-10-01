pro xinorm

norms = 'norms.dat'
temps = '~/research/me_temp_proj/me_fits/dat/culled_r2500-50_7-7.dat'

psize = 0.75
xmin = 0.79
xmax = 1.0
ymin = 0.0
ymax = 0.2

readcol, norms, FORMAT='A,A,F,F,F,F,F,F,F,F', $
         nname, nobs, ntx, t2, xi, c1, c2, n1, n2, rat

;; quik stats
ts = 0.5
WHILE (ts LE 1.0) DO BEGIN
    xis = 80
    WHILE (xis LE 99) DO BEGIN
        ord = where((t2 EQ ts) AND (xi*100 EQ xis))
        print, format='(F5.2,F10.3,F10.3,A4,F5.2)',ts,xis,mean(rat[ord]),'+/-',stddev(rat[ord])
        IF (xis LT 80) THEN etastep = 10
        IF (xis GE 80 AND xis LT 95) THEN etastep = 5
        IF (xis GE 95) THEN etastep = 1
        xis = xis + etastep
    ENDWHILE
    print,''
    ts = ts+0.25
ENDWHILE

restore,"~/research/redux/scripts/xspectemp_rin_normerr_src.sav"        
full = read_ascii(temps, template = xspectemp_rin_normerr_src)
obsids = full.obsid
txs = full.tx

loadct, 13
set_plot,'PS'
device, $
  filename = 'xi_norm.eps', $
  /color, $
  /encapsulated, $
  /portrait, $
  /helvetica
!FANCY    = 4
!LINETYPE = 0
!P.FONT   = 0
!X.THICK  = 3
!Y.THICK  = 3
!Z.THICK  = 3
!X.OMARGIN = [-3.5,0]
!Y.OMARGIN = [-0.75,0]
plot, [xmin,xmax], [ymin,ymax], $
      /nodata, $
      xran=[xmin,xmax], $
      yran=[ymin,ymax], $
      /xsty, /ysty, $
      xtitle = textoidl('\xi'), $
      ytitle = textoidl('(\eta_{2}/\eta_{1})/T_{X}'), $
      charsize = 1.0

FOR i=0,n_elements(obsids)-1 DO BEGIN
    obsid = obsids[i]
    tx = txs[i]
    scale = tx^(1./1.)

    ord = where((t2 EQ 0.5) AND (nobs EQ obsid))
    IF ord[0] EQ -1 THEN GOTO, err

    x = xi[ord]
    y = rat[ord]
    plotsym, 0, psize, /fill
    oplot, x, y/scale, psym=8, color=50
    oplot, x, y/scale, psym=0, color=50

    ord = where((t2 EQ 0.75) AND (nobs EQ obsid))
    x = xi[ord]
    y = rat[ord]
    plotsym, 4, psize, /fill
    oplot, x, y/scale, psym=8, color=150
    oplot, x, y/scale, psym=0, color=150

    ord = where((t2 EQ 1.0) AND (nobs EQ obsid))
    x = xi[ord]
    y = rat[ord]
    plotsym, 8, psize, /fill
    oplot, x, y/scale, psym=8, color=250
    oplot, x, y/scale, psym=0, color=250

ERR:
ENDFOR
device, /close
set_plot, 'X'

END
