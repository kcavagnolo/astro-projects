PRO bh

ktype = 'flat'
model = 'nonzero'

readcol, 'dat/bh.dat', FORMAT='A,F,F,F,F,F,F,F,F,F,F,F,F,F,F,F,I,I,I,F,F,F',$
         cluster, mbhm, a, b, mbhs, c, d, mbhl, e, f, $
         dmbh, dmbhlo, dmbhhi, cmbh, cmbhlo, cmbhhi, $
         bondi, bondilo, bondihi, edd, eddlo, eddhi

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
    ord = where(datname EQ name)
    IF ord[0] EQ -1 THEN GOTO,ERROR
    IF dmbh[i] LE 0 THEN GOTO,ERROR
    tk0 = k0[ord]
    tk0err = k0err[ord]
    push, k0all, tk0[ind]
    push, k0allerr, tk0err[ind]
    push, bhm, dmbh[i]
    push, bhmlo, dmbhlo[i]
    push, bhmhi, dmbhhi[i]
    push, cbhm, cmbh[i]
    push, cbhmlo, cmbhlo[i]
    push, cbhmhi, cmbhhi[i]
ERROR:
ENDFOR

loadct, 13
set_plot, 'PS'
device, $
  filename = 'k0_bhm.eps', $
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

xmin = 0.75*min(bhm-bhmlo)
xmax = 1.25*max(bhm+bhmhi)
ymin = 1.0
ymax = 1.25*max(k0all+k0allerr)
xtex = textoidl('\Delta M_{BH} [M'+sunsymbol()+']')
ytex = textoidl('K_{0} [keV cm^{2}]')

plotsym, 0, 0.8, /fill
plot, bhm, k0all, $
      linestyle = 0, $
      xrange = [xmin,xmax], $
      yrange = [ymin,ymax],$
      xtitle = xtex, $
      ytitle = ytex, $
      /xsty, /ysty, $
      /xlog, /ylog, $
      charsize = 0.8, $
      psym = 8
oploterror, bhm, k0all, bhmlo, k0allerr, psym=8, /lobar
oploterror, bhm, k0all, bhmhi, k0allerr, psym=8, /hibar
plotsym, 0, 0.5, /fill
oplot, bhm, k0all, psym=8, color=100

device,/close

device, $
  filename = 'k0_cbhm.eps', $
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

xmin = 0.75*min(cbhm-cbhmlo)
xmax = 1.25*max(cbhm+cbhmhi)
ymin = 1.0
ymax = 1.25*max(k0all+k0allerr)
xtex = textoidl('\delta M_{BH} [M'+sunsymbol()+' yr^{-1}]')
ytex = textoidl('K_{0} [keV cm^{2}]')

plotsym, 0, 0.8, /fill
plot, cbhm, k0all, $
      linestyle = 0, $
      xrange = [xmin,xmax], $
      yrange = [ymin,ymax],$
      xtitle = xtex, $
      ytitle = ytex, $
      /xsty, /ysty, $
      /xlog, /ylog, $
      charsize = 0.8, $
      psym = 8
oploterror, cbhm, k0all, cbhmlo, k0allerr, psym=8, /lobar
oploterror, cbhm, k0all, cbhmhi, k0allerr, psym=8, /hibar
plotsym, 0, 0.5, /fill
oplot, cbhm, k0all, psym=8, color=150

device,/close
set_plot, "X"

END
