PRO sfr

ktype = 'flat'
model = 'nonzero'

readcol, 'dat/sfr.dat', FORMAT='A,F,F,F,F,F,F,F,F,F,F,F,F',$
         cluster, csfr, csfrlo, csfrhi, bsfr, bsfrlo, bsfrhi,$
         cxmm, cxmmlo, cxmmhi, cfus, cfuslo, cfushi

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
    tk0 = k0[ord]
    tk0err = k0err[ord]
    ord = where(cluster EQ name)
    tcsfr = csfr[ord]
    tcsfrlo = csfrlo[ord]
    tcsfrhi = csfrhi[ord]
    ord = where(tcsfr EQ max(tcsfr))
    ord = ord[0]
    IF tcsfrlo[ord] EQ -1 THEN BEGIN
        push, lk0all, tk0[ind]
        push, lk0allerr, tk0err[ind]
        push, lsfr, tcsfr[ord]
        push, lsfrlo, 0
        push, lsfrhi, 0 
    ENDIF ELSE BEGIN
        push, k0all, tk0[ind]
        push, k0allerr, tk0err[ind]
        push, sfr, tcsfr[ord]
        push, sfrlo, tcsfrlo[ord]
        push, sfrhi, tcsfrhi[ord]
    ENDELSE
ERROR:
ENDFOR

loadct, 13
set_plot, 'PS'
device, $
  filename = 'k0_sfr.eps', $
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

xmin = 0.70*min([sfr,lsfr]-[sfrlo,lsfrlo])
xmax = 1.25*max([sfr,lsfr]+[sfrhi,lsfrhi])
ymin = 0.70*min([lk0all,k0all]-[lk0allerr,k0allerr])
ymax = 1.25*max([lk0all,k0all]+[lk0allerr,k0allerr])
xtex = textoidl('SFR [M'+sunsymbol()+' yr^{-1}]')
ytex = textoidl('K_{0} [keV cm^{2}]')

plotsym, 0, 0.8, /fill
plot, sfr, k0all, $
      linestyle = 0, $
      xrange = [xmin,xmax], $
      yrange = [ymin,ymax],$
      xtitle = xtex, $
      ytitle = ytex, $
      /xsty, /ysty, $
      /xlog, /ylog, $
      charsize = 0.8, $
      psym = 8
oploterror, sfr, k0all, sfrlo, k0allerr, psym=8, /lobar
oploterror, sfr, k0all, sfrhi, k0allerr, psym=8, /hibar
plotsym, 0, 0.5, /fill
oplot, sfr, k0all, psym=8, color=100
plotsym, 6, 2.0, /fill, thick=3
oplot, lsfr, lk0all, psym=8
plotsym, 0, 0.8, thick=2
oplot, lsfr, lk0all, psym=8
oploterror, lsfr, lk0all, lk0allerr, psym=8

device,/close
set_plot, "X"

END
