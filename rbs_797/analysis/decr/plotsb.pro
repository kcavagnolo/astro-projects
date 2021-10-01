PRO plotsb, file

;# read the file
readcol, file, format='F,F', comment='#', r, dec
dec = 1-dec

;# nice plotting params
!FANCY    = 4
!LINETYPE = 0
!P.FONT   = 0
!X.THICK  = 3
!Y.THICK  = 3
!Z.THICK  = 3
set_plot,'ps'
device, filename='cavdec.eps', $
        /encapsulated, $
        /portrait, $
        set_font='Times-Roman'
plotsym, 0, 1.0, /fill
ord = where(r GE 0)
;xmin = min(r[ord])
;xmax = max(r)
;ymin = min(dec[ord])
;ymax = max(dec)
xmin = 0
xmax = 50
ymin = 0.15
ymax = 0.55
plot, [xmin,xmax], [ymin,ymax], $
      /nodata, $
      /xsty, /ysty, $
      xtitle = textoidl('z [kpc]'), $
      ytitle = textoidl('y'), $
      xrange = [xmin,xmax], $
      yrange = [ymin,ymax], $
      linestyle = 0, $
      position= aspect(1.0), $
      thick = 2, $
      charsize = 1.5

;# plot r vs. dec
FOR i=0,n_elements(r)-1 DO BEGIN
   IF r[i] GE 0 THEN BEGIN
      push, pr, r[i]
      push, pdec, dec[i]
   ENDIF
   IF r[i] LT 0 AND n_elements(pr) GT 1 THEN BEGIN
      oplot, pr, pdec, linestyle=0
      mid = where(pdec GT 0.49 AND pdec LT 0.491)
      mid = mid[0]
      xyouts, pr[mid]-0.4, 0.49, text, orientation=55, charsize=1.0
      void, pr
      void, pdec
   ENDIF
   IF r[i] LT 0 THEN text=num2str(sigfig(-r[i],4))
ENDFOR
;oplot, maken(-100,100,2),replicate(0.47,2), linestyle=2
oplot, maken(-100,100,2),replicate(0.44,2), linestyle=2
;legend, 'E1', box=0, charsize=1.5, /bottom, /right
legend, 'W1', box=0, charsize=1.5, /bottom, /right

;ord = where((r GE 0) AND (dec GE 0))
;oplot, r[ord], dec[ord], linestyle=0
;; ord = where((r LT 0) AND (r GT -999))
;; FOR i=0,n_elements(ord)-1 DO BEGIN
;;    ele = ord[i]
;;    oplot, maken(xmin,xmax,10), replicate(-1*dec[ele],10), linestyle=i+1
;;    oplot, replicate(-1*r[ele],10), maken(xmin,xmax,10), linestyle=i+1
;; ENDFOR
device, /close
set_plot, 'X'

END
