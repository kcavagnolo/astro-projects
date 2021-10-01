PRO ir

ktype = 'flat'
model = 'nonzero'
readcol, 'dat/ir.dat', FORMAT='A,F,A', comment='#', $
         cluster, ir, det
restore,"~/research/redux/scripts/s_resultstemplate.sav"
data = read_ascii('s_results/all_results.log', template = s_resultstemplate)
datname = data.cluster
k0 = data.k0
k0err = data.k0err
IF (ktype EQ "flat") THEN BEGIN
    IF (model EQ "nonzero") THEN ind = 2 ELSE ind = 3
ENDIF
IF (ktype EQ "itpl") THEN BEGIN
    IF (model EQ "nonzero") THEN ind = 0 ELSE ind = 1
ENDIF

FOR i = 0,n_elements(cluster)-1 DO BEGIN
   name = strcompress(cluster[i],/remove_all)
   tir = ir[i]
   tdet = det[i]
   ord = where(datname EQ name, count)
   IF count LT 1 THEN BEGIN
;      print, '## ERROR: No K0 data found for ',name
      GOTO,ERROR
   ENDIF
   tk0 = k0[ord]
   tk0err = k0err[ord]
   IF tdet EQ 'n' THEN BEGIN
      push, nk0, tk0[ind]
      push, nk0err, tk0err[ind]
      push, nir, tir
   ENDIF ELSE BEGIN
      IF tk0[ind]-tk0err[ind] GE 30. THEN $
         print, 'K0 >= 30 ',name
      push, fk0, tk0[ind]
      push, fk0err, tk0err[ind]
      push, fir, tir
   ENDELSE
ERROR:
ENDFOR

set_plot, 'PS'
device, $
   filename = 'k0_ir.eps', $
   /encapsulated, $
   /portrait, $
   /helvetica
!FANCY    = 4
!LINETYPE = 0
!P.FONT   = 0
!X.THICK  = 3
!Y.THICK  = 3
!Z.THICK  = 3

xmin = 0.70*min([nir,fir])
xmax = 1.25*max([nir,fir])
ymin = 0.70*min([nk0,fk0])
ymax = 1.25*max([nk0,fk0])
xtex = textoidl('L_{IR} [10^{44} ergs s^{-1}]')
ytex = textoidl('K_{0} [keV cm^{2}]')
plot, [nir,fir], [nk0,fk0], $
      /nodata, $
      linestyle = 0, $
      xrange = [xmin,xmax], $
      yrange = [ymin,ymax],$
      xtitle = xtex, $
      ytitle = ytex, $
      /xsty, /ysty, $
      /xlog, /ylog, $
      charsize = 1.0
plotsym, 0, 0.1
oploterror, [nir,fir], [nk0,fk0], [nk0err,fk0err], psym=8
plotsym, 0, 0.8, /fill
oplot, fir, fk0, psym=8
;plotsym, 6, 1.0
;oplot, nir, nk0, psym=8
plotsym, 0, 0.8
oplot, nir, nk0, psym=8
device,/close
set_plot, "X"

END
