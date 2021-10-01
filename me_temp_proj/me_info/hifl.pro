pro hifl

xtype = "frac"
ytype = "m500"

me = mrdfits('../me_paper/final_r2500-50_fefree.fits',1)
restore,"hiflugcs.sav"
hifl = read_ascii('hiflugcs.dat',template=hiflugcs)
mecl = strcompress(me.cluster,/remove_all)
hicl = strcompress(hifl.cluster,/remove_all)

FOR i=0,n_elements(mecl)-1 DO BEGIN
    name = strcompress(mecl[i],/remove_all)
    num = where(strmatch(hicl,name,/FOLD_CASE) EQ 1)
    IF num[0] NE -1 THEN push,hiord,num[0]
ENDFOR
hicl=hicl[hiord]
FOR i=0,n_elements(hicl)-1 DO BEGIN
    name = strcompress(hicl[i],/remove_all)
    num = where(strmatch(mecl,name,/FOLD_CASE) EQ 1)
    IF num[0] NE -1 THEN push,meord,num[0]
ENDFOR
mecl=mecl[meord]

me27   = me.tx27
me27lo = me27-me.tx27lo
me27hi = me.tx27hi-me27
me77   = me.tx77
me77lo = me77-me.tx77lo
me77hi = me.tx77hi-me77

me27   = me27[meord]
me27lo = me27lo[meord]
me27hi = me27hi[meord]
me77   = me77[meord]
me77lo = me77lo[meord]
me77hi = me77hi[meord]

frac   = me27/me77
frachi = me27/me77*(sqrt((me27hi/me27)^2.+(me77hi/me77)^2.))
fraclo = me27/me77*(sqrt((me27lo/me27)^2.+(me77lo/me77)^2.))

melx   = me.lx
melxlo = melx-me.lxlo
melxhi = me.lxhi-melx
melx   = melx[meord]
melxlo = melxlo[meord]
melxhi = melxhi[meord]

m500   = hifl.m500
m500lo = hifl.m500lo
m500hi = hifl.m500hi
m500   = m500[hiord]
m500lo = m500lo[hiord]
m500hi = m500hi[hiord]

mg500   = hifl.mg500
mg500lo = hifl.mg500lo
mg500hi = hifl.mg500hi
mg500   = mg500[hiord]
mg500lo = mg500lo[hiord]
mg500hi = mg500hi[hiord]

mdot   = hifl.mdot
mdotlo = hifl.mdotlo
mdothi = hifl.mdothi
mdot   = mdot[hiord]
mdotlo = mdotlo[hiord]
mdothi = mdothi[hiord]

th   = hifl.th
thlo = hifl.thlo
thhi = hifl.thhi
th   = th[hiord]
thlo = thlo[hiord]
thhi = thhi[hiord]

tm   = hifl.tm
tmlo = hifl.tmlo
tmhi = hifl.tmhi
tm   = tm[hiord]
tmlo = tmlo[hiord]
tmhi = tmhi[hiord]

hilx   = hifl.lx
hilxlo = hifl.lxerr
hilxhi = hifl.lxerr
hilx   = hilx[hiord]
hilxlo = hilxlo[hiord]
hilxhi = hilxhi[hiord]

IF xtype EQ "frac" THEN BEGIN
    x = frac
    xlo = fraclo
    xhi = frachi
    xmin = 0.6
    xmax = 1.6
    xtex = textoidl('T_{frac}')
ENDIF ELSE IF xtype EQ "hard" THEN BEGIN
    x   = me27
    xlo = me27lo
    xhi = me27hi
    xmin = 1.0
    xmax = 20.0
    xtex = textoidl('T_{2.0-7.0} [keV]')
ENDIF ELSE IF xtype EQ "full" THEN BEGIN
    x   = me77
    xlo = me77lo
    xhi = me77hi
    xmin = 1.0
    xmax = 20.0
    xtex = textoidl('T_{0.7-7.0} [keV]')
ENDIF ELSE IF xtype EQ "lx" THEN BEGIN
    x   = melx
    xlo = melxlo
    xhi = melxhi
    xmin = 0.1
    xmax = 1000.0
    xtex = textoidl('L_{bol} [10^{45} ergs sec^{-1} h_{70}^{-1}]')
ENDIF

IF ytype EQ "m500" THEN BEGIN
    y = m500
    ylo = m500lo
    yhi = m500hi
    ytex = textoidl('M_{500} 10^{14} M_{'+sunsymbol()+'} h_{70}^{-1}')
    ymin = 0.5
    ymax = 25
ENDIF ELSE IF ytype EQ "th" THEN BEGIN
    y = th
    ylo = thlo
    yhi = thhi
    ytex = textoidl('T_{hot, HIFLUGcS} [keV]')
    ymin = 1.0
    ymax = 20.0
ENDIF ELSE IF ytype EQ "mg500" THEN BEGIN
    y = mg500
    ylo = mg500lo
    yhi = mg500hi
    ytex = textoidl('M_{gas} 10^{13} M_{'+sunsymbol()+'} h_{70}^{-1}')
    ymin = 0.5
    ymax = 50
ENDIF ELSE IF ytype EQ "tm" THEN BEGIN
    y = tm
    ylo = tmlo
    yhi = tmhi
    ytex = textoidl('T_{emi. weighted} [keV]')
    ymin = 1.0
    ymax = 20.0
ENDIF ELSE IF ytype EQ "lx" THEN BEGIN
    y = hilx
    ylo = hilxlo
    yhi = hilxhi
    ytex = textoidl('L_{0.1-2.4keV} 10^{44} ergs sec^{-1}')
    ymin = 0.1
    ymax = 1000.0
ENDIF

FOR i=0,n_elements(xlo)-1 DO BEGIN
    IF xlo[i] GT xhi[i] THEN push,xerr,xlo[i] ELSE push,xerr,xhi[i]
ENDFOR
FOR i=0,n_elements(ylo)-1 DO BEGIN
    IF ylo[i] GT yhi[i] THEN push,yerr,ylo[i] ELSE push,yerr,yhi[i]
ENDFOR
bc = bces(x, y, error=bcerr, xerror=xerr, yerror=yerr)

set_plot,'ps'
device,filename='tf_m.ps'
plotsym,0,/fill
plot, x, y, $
      psym = 8,$
      /xsty, /ysty, $
;      /xlog, /ylog, $
      xran = [xmin,xmax],$
      yran = [ymin,ymax], $
      xtitle = xtex, $
      ytitle = ytex
oploterror, x, y, xlo, ylo, psym=8, /lobar, /nohat
oploterror, x, y, xhi, yhi, psym=8, /hibar, /nohat

;x = indgen(100)
;y = bc[1]*x+bc[0]
;print, bc
;oplot, x, y, linestyle=2
x = frac[where(m500 GT 15 OR frac LT 1 OR m500 LT 2)]
y = m500[where(m500 GT 15 OR frac LT 1 OR m500 LT 2)]
name = hicl[where(m500 GT 15 OR frac LT 1 OR m500 LT 2)]
xyouts,x,y,name

device,/close

END
