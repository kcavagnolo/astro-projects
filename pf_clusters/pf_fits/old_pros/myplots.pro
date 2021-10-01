pro myplots, dat1, dat2, dat3, dat4, dat5, dat6

; ie:
; myplots, '1tpr_nhfrozen_fefree.dat', '1tpr_nhfree_fefree.dat', '1tde_nhfrozen_fefree.dat', '1tde_nhfree_fefree.dat', '1tde_nhfrozen_fetied.dat', 'reference.list'

; restore the fit template and read some variables
restore,"xspecfittemplate.sav"
prfro = read_ascii(dat1, template = xspecfittemplate)
prfre = read_ascii(dat2, template = xspecfittemplate)
defro = read_ascii(dat3, template = xspecfittemplate)
defre = read_ascii(dat4, template = xspecfittemplate)

restore,"xspecfittemplate_ext.sav"
detie = read_ascii(dat5, template = xspecfittemplate_ext)

restore,"reflist_template.sav"
ref = read_ascii(dat6, template = reflist_template)

;******************************************************
;******************************************************
;    PROJECTED VS DEPROJECTED TEMPERATURE NH-FREE
;******************************************************
;******************************************************

; define the labels for the plot axes
xtx = textoidl("Projected T_{X} (N_{H}-free) (keV)")
ytx = textoidl("Deprojected T_{X} (N_{H}-free) (keV)")

; store the hi and lo errors in variables
xerrlo = prfre.tx - prfre.tlo
xerrhi = prfre.thi - prfre.tx
yerrlo = defre.tx - defre.tlo
yerrhi = defre.thi - defre.tx

xmax = max(prfre.thi)
ymax = max(defre.thi)
IF (xmax GT ymax) THEN ranmax = xmax+1. ELSE ranmax = ymax+1.

; plot commands for making a postscript file
set_plot, 'PS'
device, filename = "tfre_de_vs_pr.ps"
!fancy = 4
!linetype = 0
plot, prfre.tx, defre.tx, $
      psym = 4, $
      xtitle = xtx, $
      ytitle = ytx, $
      xstyle = 9, $
      xrange = [0,ranmax], $
      yrange = [0,ranmax], $
      charsize = 0.8

; overplot the hi and lo errorbars in both directions
oploterror, prfre.tx, defre.tx, xerrhi, yerrhi, psym=3, /hibar
oploterror, prfre.tx, defre.tx, xerrlo, yerrlo, psym=3, /lobar

; overplot the line x=y
x = findgen(ranmax)
y = x
oplot, x, y, linestyle=2, psym=0

device, /close
set_plot, "X"

;******************************************************
;******************************************************
;    PROJECTED VS DEPROJECTED TEMPERATURE NH-FIXED
;******************************************************
;******************************************************

; define the labels for the plot axes
xtx = textoidl("Projected T_{X} (N_{H}-fixed) (keV)")
ytx = textoidl("Deprojected T_{X} (N_{H}-fixed) (keV)")

; store the hi and lo errors in variables
xerrlo = prfro.tx - prfro.tlo
xerrhi = prfro.thi - prfro.tx
yerrlo = defro.tx - defro.tlo
yerrhi = defro.thi - defro.tx

xmax = max(prfro.thi)
ymax = max(defro.thi)
IF (xmax GT ymax) THEN ranmax = xmax+1. ELSE ranmax = ymax+1.

; plot commands for making a postscript file
set_plot, 'PS'
device, filename = "tfro_de_vs_pr.ps"
!fancy = 4
!linetype = 0
plot, prfro.tx, defro.tx, $
      psym = 4, $
      xtitle = xtx, $
      ytitle = ytx, $
      xstyle = 9, $
      xrange = [0,ranmax], $
      yrange = [0,ranmax], $
      charsize = 0.8

; overplot the hi and lo errorbars in both directions
oploterror, prfro.tx, defro.tx, xerrhi, yerrhi, psym=3, /hibar
oploterror, prfro.tx, defro.tx, xerrlo, yerrlo, psym=3, /lobar

; overplot the line x=y
x = findgen(ranmax)
y = x
oplot, x, y, linestyle=2, psym=0

device, /close
set_plot, "X"

;******************************************************
;******************************************************
; PROJECTED + DEPROJECTED TEMPERATURE NH-FIXED VS. RMID
;******************************************************
;******************************************************

cname = strcompress(ref.cluster,/remove_all)
cname = str_replace(cname,'ABELL','Abell')
cname = str_replace(cname,'_',' ')
cname = str_replace(cname,'HYDRA','Hydra')

; define the obsid
FOR i = 0, n_elements(ref.obsid)-1 DO BEGIN
obs = strcompress(ref.obsid[i],/remove_all)
name = cname[i]

; define the labels for the plot axes
ytx = textoidl("T_{X} (N_{H}-fixed) (keV)")
xtx = textoidl("R_{out} (arcsec)")

; store the hi and lo errors in variables
prfrotx   = prfro.tx[where(prfro.obsid EQ obs)]
prfrotlo  = prfro.tlo[where(prfro.obsid EQ obs)]
prfrothi  = prfro.thi[where(prfro.obsid EQ obs)]
prfrorout = prfro.rout[where(prfro.obsid EQ obs)]

defrotx   = defro.tx[where(defro.obsid EQ obs)]
defrotlo  = defro.tlo[where(defro.obsid EQ obs)]
defrothi  = defro.thi[where(defro.obsid EQ obs)]
defrorout = defro.rout[where(defro.obsid EQ obs)]

detietx   = detie.tx[where(detie.obsid EQ obs)]
detietlo  = detie.tlo[where(detie.obsid EQ obs)]
detiethi  = detie.thi[where(detie.obsid EQ obs)]
detierout = detie.rout[where(detie.obsid EQ obs)]

; calculate the relative errors needed for plotting
prerrlo = prfrotx - prfrotlo
prerrhi = prfrothi - prfrotx
deerrlo = defrotx - defrotlo
deerrhi = defrothi - defrotx
tierrlo = detietx - detietlo
tierrhi = detiethi - detietlo

; plot commands for making a postscript file
set_plot, 'PS'
device, filename = 'tfro_vs_r_'+strcompress(obs,/remove_all)+'.ps'
!linetype = 0
!fancy = 4
plot, prfrorout, prfrotx, $
      psym = -4, $
      title  = name, $
      xtitle = xtx, $
      ytitle = ytx, $
      xstyle = 9, $
      xrange = [0,max(prfrorout)+0.5], $
      yrange = [0,max(prfrotx)+1.], $
      charsize = 0.8

; draw the legend
items = ["Projected", "Deprojected Fe-free", "Deprojected Fe-tied"]
linearr = [0, 1, 2]
psyarr = [-4, -5, -6]
legend, items, linestyle=linearr, psym=psyarr, box=0, charsize=0.8, /bottom, /right

; overplot the hi and lo errorbars in both directions
oploterror, prfrorout, prfrotx, prerrhi, psym=4, /hibar
oploterror, prfrorout, prfrotx, prerrlo, psym=4, /lobar
oploterror, defrorout, defrotx, deerrhi, psym=5, /hibar
oploterror, defrorout, defrotx, deerrlo, psym=5, /lobar
oploterror, detierout, detietx, tierrlo, psym=6, /hibar
oploterror, detierout, detietx, tierrhi, psym=6, /lobar

; overplot the deprojected temperatures
oplot, defrorout, defrotx, linestyle=1, psym=-5
oplot, detierout, detietx, linestyle=2, psym=-6

device, /close
set_plot, "X"

ENDFOR

; make all these ps files into one ps file
SPAWN, 'ls tfro_*.ps > list'
SPAWN, 'cat list | pscat tfro-plots.ps'
SPAWN, 'rm -f tfro_*.ps'

;******************************************************
;******************************************************
; PROJECTED + DEPROJECTED TEMPERATURE NH-FREE VS. RMID
;******************************************************
;******************************************************

cname = strcompress(ref.cluster,/remove_all)
cname = str_replace(cname,'ABELL','Abell')
cname = str_replace(cname,'_',' ')
cname = str_replace(cname,'HYDRA','Hydra')

; define the obsid
FOR i = 0, n_elements(ref.obsid)-1 DO BEGIN
obs = strcompress(ref.obsid[i],/remove_all)
name = cname[i]

; define the labels for the plot axes
ytx = textoidl("T_{X} (N_{H}-free) (keV)")
xtx = textoidl("R_{out} (arcsec)")

; store the hi and lo errors in variables
prfretx   = prfre.tx[where(prfre.obsid EQ obs)]
prfretlo  = prfre.tlo[where(prfre.obsid EQ obs)]
prfrethi  = prfre.thi[where(prfre.obsid EQ obs)]
defretx   = defre.tx[where(defre.obsid EQ obs)]
defretlo  = defre.tlo[where(defre.obsid EQ obs)]
defrethi  = defre.thi[where(defre.obsid EQ obs)]
prfrerout = prfre.rout[where(prfre.obsid EQ obs)]
defrerout = defre.rout[where(defre.obsid EQ obs)]

; calculate the relative errors needed for plotting
prerrlo = prfretx - prfretlo
prerrhi = prfrethi - prfretx
deerrlo = defretx - defretlo
deerrhi = defrethi - defretx

; plot commands for making a postscript file
set_plot, 'PS'
device, filename = 'tfre_vs_r_'+strcompress(obs,/remove_all)+'.ps'
!linetype = 0
!fancy = 4
plot, prfrerout, prfretx, $
      psym = -4, $
      title  = name, $
      xtitle = xtx, $
      ytitle = ytx, $
      xstyle = 9, $
      xrange = [0,max(prfrerout)+0.5], $
      yrange = [0,max(prfretx)+1.], $
      charsize = 0.8

; draw the legend
items = ["Projected", "Deprojected"]
linearr = [0, 1]
psyarr = [-4, -5]
legend, items, linestyle=linearr, psym=psyarr, box=0, charsize=0.8, /bottom, /right

; overplot the hi and lo errorbars in both directions
oploterror, prfrerout, prfretx, prerrhi, psym=4, /hibar
oploterror, prfrerout, prfretx, prerrlo, psym=4, /lobar
oploterror, defrerout, defretx, deerrhi, psym=5, /hibar
oploterror, defrerout, defretx, deerrlo, psym=5, /lobar

; overplot the deprojected temperatures
oplot, defrerout, defretx, linestyle=1, psym=-5

device, /close
set_plot, "X"

ENDFOR

; make all these ps files into one ps file
SPAWN, 'ls tfre_*.ps > list'
SPAWN, 'cat list | pscat tfre-plots.ps'
SPAWN, 'rm -f tfre_*.ps'

;******************************************************
;******************************************************
; PROJECTED TEMPERATURE NH-FIXED VS. NH-FREE
;******************************************************
;******************************************************

; define the labels for the plot axes
ytx = textoidl("Projected T_{X} (N_{H}-fixed) (keV)")
xtx = textoidl("Projected T_{X} (N_{H}-free) (keV)")

; store the hi and lo errors in variables
xerrlo = prfre.tx-prfre.tlo
xerrhi = prfre.thi-prfre.tx
yerrlo = prfro.tx-prfro.tlo
yerrhi = prfro.thi-prfro.tx

; plot commands for making a postscript file
set_plot, 'PS'
device, filename = "pr_tfro_tfre.ps"
!linetype = 0
!psym = 4
!fancy = 4
plot, prfre.tx, prfro.tx, $
      xtitle = xtx, $
      ytitle = ytx, $
      xstyle = 9, $
      xrange = [0,14], $
      yrange = [0,14], $
      charsize = 0.8

; overplot the hi and lo errorbars in both directions
oploterror, prfre.tx, prfro.tx, xerrhi, yerrhi, psym=3, /hibar
oploterror, prfre.tx, prfro.tx, xerrlo, yerrlo, psym=3, /lobar

; overplot the line x=y
x = findgen(14)
y = findgen(14)
oplot, x, y, linestyle=2, psym=0

device, /close
set_plot, "X"

;******************************************************
;******************************************************
; PROJECTED DELTA TEMPERATURE VS NH-FIXED
;******************************************************
;******************************************************

; define the function to plot: [Tx(fix) - Tx(free)] / Tx(fix)
deltatx = (prfro.tx-prfre.tx)/prfro.tx

; build two arrays to hold the relative error values
deltfre = fltarr(n_elements(deltatx))
deltfro = fltarr(n_elements(deltatx))

; test to see which deviation is larger, the hi or lo, then
; store the larger of the two in the relative error array
FOR i = 0,n_elements(prfre.tx)-1 DO BEGIN
    test1 = abs(prfre.tx[i] - prfre.tlo[i])
    test2 = abs(prfre.thi[i] - prfre.tx[i])
    test3 = abs(prfro.tx[i] - prfro.tlo[i])
    test4 = abs(prfro.thi[i] - prfro.tx[i])
    IF (test1 GT test2) THEN deltfre[i] = (test1) ELSE deltfre[i] = (test2)
    IF (test3 GT test4) THEN deltfro[i] = (test3) ELSE deltfro[i] = (test4)
ENDFOR

; define the error propagation in the plotted function
delterr = (prfre.tx/prfro.tx)*[(deltfre/prfre.tx)^2. + (deltfro/prfro.tx)^2.]^(1./2.)

; make a shifted array of nh values so they don't stack
; on the plot and can be seen more easily
; change the value of j to set the shift
shiftnh = fltarr(n_elements(prfro.nh))
j=0
FOR i = 0,n_elements(prfro.nh)-1 DO BEGIN
    shiftnh[i] = (prfro.nh[i]+j)
    j = j + 0.1
ENDFOR

; define the labels for the axes
ydtx = textoidl("Projected \DeltaT_{X}/T")
xdtx = textoidl("Galactic N_{H} (10^{20} cm^{-2})")

; plotting commands
set_plot, 'PS'
device, filename = "pr_deltx_vs_nh.ps"
!linetype = 0
!psym = 4
!fancy = 4
plot, shiftnh, deltatx, $
      xtitle = xdtx, $
      ytitle = ydtx, $
      xstyle = 9, $
      xrange = [min(shiftnh)-2,max(shiftnh)+2], $
      yrange = [min(deltatx)-0.1,max(deltatx)+0.1], $
      xcharsize = 0.8, $
      ycharsize = 0.8
oploterror, shiftnh, deltatx, delterr, errstyle=0
!psym = 0

; build two arrays, x and y, to plot the line
; [y = 0, x = NH(fix)]
y = fltarr(n_elements(shiftnh))
x = findgen(max(shiftnh)+10)
oplot, x, y, linestyle=5
device, /close
set_plot, "X"

;******************************************************
;******************************************************
; PROJECTED DELTA TEMPERATURE VS TX-FIXED
;******************************************************
;******************************************************

; same Tx plot as above but plotted versus Tx(fix)
; and not NH(fix)
ydtx = textoidl("Projected \DeltaT_{X}/T")
xdtx = textoidl("Projected T_{X} (N_{H}-fixed) (keV)")

set_plot, 'PS'
device, filename = "pr_deltx_vs_tx.ps"
!linetype = 0
!psym = 4
!fancy = 4
plot, prfro.tx, deltatx, $
      xtitle = xdtx, $
      ytitle = ydtx, $
      xstyle = 9, $
      xrange = [min(prfro.tx)-0.5, max(prfro.tx)+0.5], $
      yrange = [min(deltatx)-0.2, max(deltatx)+0.2], $
      xcharsize = 0.8, $
      ycharsize = 0.8
oploterror, prfro.tx, deltatx, delterr, errstyle=0
!psym = 0
y = fltarr(n_elements(prfro.tx))
x = findgen(max(prfro.tx)+2)
oplot, x, y, linestyle=5
device, /close
set_plot, "X"

;******************************************************
;******************************************************
; PROJECTED DELTA NH VS NH-FIXED
;******************************************************
;******************************************************

; exactly the same as Tx plots except for NH
deltanh = (prfro.nh-prfre.nh)/prfro.nh
deltfre = fltarr(n_elements(deltanh))
deltfro = fltarr(n_elements(deltanh))

FOR i = 0,n_elements(prfre.nh)-1 DO BEGIN
    test1 = abs(prfre.nh[i] - prfre.nlo[i])
    test2 = abs(prfre.nhi[i] - prfre.nh[i])
    test3 = abs(prfro.nh[i] - prfro.nlo[i])
    test4 = abs(prfro.nhi[i] - prfro.nh[i])
    IF (test1 GT test2) THEN deltfre[i] = (test1) ELSE deltfre[i] = (test2)
    IF (test3 GT test4) THEN deltfro[i] = (test3) ELSE deltfro[i] = (test4)
    IF (prfre.nh[i] EQ 0.) THEN prfre.nh[i] = prfre.nhi[i]/2.
ENDFOR

delterr = (prfre.nh/prfro.nh)*[(deltfre/prfre.nh)^2. + (deltfro/prfro.nh)^2.]^(1./2.)

shiftnh = fltarr(n_elements(prfro.nh))
j=0
FOR i = 0,n_elements(prfro.nh)-1 DO BEGIN
    shiftnh[i] = (prfro.nh[i]+j)
    j = j + 0.1
ENDFOR

ydnh = textoidl("Projected \DeltaN_{H}/N_{H}")
xdnh = textoidl("Galactic N_{H} (10^{20} cm^{-2})")

set_plot, 'PS'
device, filename = "pr_delnh_vs_nh.ps"
!linetype = 0
!psym = 4
!fancy = 4
plot, shiftnh,deltanh, $
      xtitle = xdnh, $
      ytitle = ydnh, $
      xstyle = 9, $
      xrange = [min(shiftnh)-2,max(shiftnh)+2], $
      yrange = [min(deltanh)-0.1,max(deltanh)+0.1], $
      xcharsize = 0.8, $
      ycharsize = 0.8
oploterror, shiftnh, deltanh, delterr, errstyle=0
!psym = 0
y = fltarr(n_elements(shiftnh))
x = findgen(max(shiftnh)+10)
oplot, x, y, linestyle=5
device, /close
set_plot, "X"

;******************************************************
;******************************************************
; PROJECTED DELTA NH VS TX-FIXED
;******************************************************
;******************************************************

ydnh = textoidl("Projected \DeltaN_{H}/N_{H}")
xdnh = textoidl("Projected T_{X} (N_{H}-fixed) (keV)")

set_plot, 'PS'
device, filename = "pr_delnh_vs_tx.ps"
!linetype = 0
!psym = 4
!fancy = 4
plot, prfro.tx,deltanh, $
      xtitle = xdnh, $
      ytitle = ydnh, $
      xstyle = 9, $
      xrange = [min(prfro.tx)-0.5, max(prfro.tx)+0.5], $
      yrange = [min(deltanh)-0.2, max(deltanh)+0.2], $
      xcharsize = 0.8, $
      ycharsize = 0.8
oploterror, prfro.tx, deltanh, delterr, errstyle=0
!psym = 0
y = fltarr(n_elements(prfro.tx))
x = findgen(max(prfro.tx)+2)
oplot, x, y, linestyle=5
device, /close
set_plot, "X"

;******************************************************
;******************************************************
; DEPROJECTED TEMPERATURE NH-FIXED VS. NH-FREE
;******************************************************
;******************************************************

; define the labels for the plot axes
ytx = textoidl("Deprojected T_{X} (N_{H}-fixed) (keV)")
xtx = textoidl("Deprojected T_{X} (N_{H}-free) (keV)")

; store the hi and lo errors in variables
xerrlo = defre.tx-defre.tlo
xerrhi = defre.thi-defre.tx
yerrlo = defro.tx-defro.tlo
yerrhi = defro.thi-defro.tx

; plot commands for making a postscript file
set_plot, 'PS'
device, filename = "de_tfro_tfre.ps"
!linetype = 0
!psym = 4
!fancy = 4
plot, defre.tx, defro.tx, $
      xtitle = xtx, $
      ytitle = ytx, $
      xstyle = 9, $
      xrange = [0,14], $
      yrange = [0,14], $
      xcharsize = 0.8, $
      ycharsize = 0.8

; overplot the hi and lo errorbars in both directions
oploterror, defre.tx, defro.tx, xerrhi, yerrhi, psym=3, /hibar
oploterror, defre.tx, defro.tx, xerrlo, yerrlo, psym=3, /lobar
!psym = 0

; overplot the line x=y
x = findgen(14)
y = findgen(14)
oplot, x, y, linestyle=2

device, /close
set_plot, "X"

;******************************************************
;******************************************************
; DEPROJECTED DELTA TEMPERATURE VS NH-FIXED
;******************************************************
;******************************************************

; define the function to plot: [Tx(fix) - Tx(free)] / Tx(fix)
deltatx = (defro.tx-defre.tx)/defro.tx

; build two arrays to hold the relative error values
deltfre = fltarr(n_elements(deltatx))
deltfro = fltarr(n_elements(deltatx))

; test to see which deviation is larger, the hi or lo, then
; store the larger of the two in the relative error array
FOR i = 0,n_elements(defre.tx)-1 DO BEGIN
    test1 = abs(defre.tx[i] - defre.tlo[i])
    test2 = abs(defre.thi[i] - defre.tx[i])
    test3 = abs(defro.tx[i] - defro.tlo[i])
    test4 = abs(defro.thi[i] - defro.tx[i])
    IF (test1 GT test2) THEN deltfre[i] = (test1) ELSE deltfre[i] = (test2)
    IF (test3 GT test4) THEN deltfro[i] = (test3) ELSE deltfro[i] = (test4)
ENDFOR

; define the error deopagation in the plotted function
delterr = (defre.tx/defro.tx)*[(deltfre/defre.tx)^2. + (deltfro/defro.tx)^2.]^(1./2.)

; make a shifted array of nh values so they don't stack
; on the plot and can be seen more easily
; change the value of j to set the shift
shiftnh = fltarr(n_elements(defro.nh))
j=0
FOR i = 0,n_elements(defro.nh)-1 DO BEGIN
    shiftnh[i] = (defro.nh[i]+j)
    j = j + 0.1
ENDFOR

; define the labels for the axes
ydtx = textoidl("Deprojected \DeltaT_{X}/T")
xdtx = textoidl("Deprojected Galactic N_{H} (10^{20} cm^{-2})")

; plotting commands
set_plot, 'PS'
device, filename = "de_deltx_vs_nh.ps"
!linetype = 0
!psym = 4
!fancy = 4
plot, shiftnh, deltatx, $
      xtitle = xdtx, $
      ytitle = ydtx, $
      xstyle = 9, $
      xrange = [min(shiftnh)-2,max(shiftnh)+2], $
      yrange = [min(deltatx)-0.1,max(deltatx)+0.1], $
      xcharsize = 0.8, $
      ycharsize = 0.8
oploterror, shiftnh, deltatx, delterr, errstyle=0
!psym = 0

; build two arrays, x and y, to plot the line
; [y = 0, x = NH(fix)]
y = fltarr(n_elements(shiftnh))
x = findgen(max(shiftnh)+10)
oplot, x, y, linestyle=5
device, /close
set_plot, "X"

;******************************************************
;******************************************************
; DEPROJECTED DELTA TEMPERATURE VS TX-FIXED
;******************************************************
;******************************************************

; same Tx plot as above but plotted versus Tx(fix)
; and not NH(fix)
ydtx = textoidl("Deprojected \DeltaT_{X}/T")
xdtx = textoidl("Deprojected T_{X} (N_{H}-fixed) (keV)")

set_plot, 'PS'
device, filename = "de_deltx_vs_tx.ps"
!linetype = 0
!psym = 4
!fancy = 4
plot, defro.tx, deltatx, $
      xtitle = xdtx, $
      ytitle = ydtx, $
      xstyle = 9, $
      xrange = [min(defro.tx)-0.5, max(defro.tx)+0.5], $
      yrange = [min(deltatx)-0.2, max(deltatx)+0.2], $
      xcharsize = 0.8, $
      ycharsize = 0.8
oploterror, defro.tx, deltatx, delterr, errstyle=0
!psym = 0
y = fltarr(n_elements(defro.tx))
x = findgen(max(defro.tx)+2)
oplot, x, y, linestyle=5
device, /close
set_plot, "X"

;******************************************************
;******************************************************
; DEPROJECTED DELTA NH VS NH-FIXED
;******************************************************
;******************************************************

; exactly the same as Tx plots except for NH
deltanh = (defro.nh-defre.nh)/defro.nh
deltfre = fltarr(n_elements(deltanh))
deltfro = fltarr(n_elements(deltanh))

FOR i = 0,n_elements(defre.nh)-1 DO BEGIN
    test1 = abs(defre.nh[i] - defre.nlo[i])
    test2 = abs(defre.nhi[i] - defre.nh[i])
    test3 = abs(defro.nh[i] - defro.nlo[i])
    test4 = abs(defro.nhi[i] - defro.nh[i])
    IF (test1 GT test2) THEN deltfre[i] = (test1) ELSE deltfre[i] = (test2)
    IF (test3 GT test4) THEN deltfro[i] = (test3) ELSE deltfro[i] = (test4)
    IF (defre.nh[i] EQ 0.) THEN defre.nh[i] = defre.nhi[i]/2.
ENDFOR

delterr = (defre.nh/defro.nh)*[(deltfre/defre.nh)^2. + (deltfro/defro.nh)^2.]^(1./2.)

shiftnh = fltarr(n_elements(defro.nh))
j=0
FOR i = 0,n_elements(defro.nh)-1 DO BEGIN
    shiftnh[i] = (defro.nh[i]+j)
    j = j + 0.1
ENDFOR

ydnh = textoidl("Deprojected \DeltaN_{H}/N_{H}")
xdnh = textoidl("Deprojected Galactic N_{H} (10^{20} cm^{-2})")

set_plot, 'PS'
device, filename = "de_delnh_vs_nh.ps"
!linetype = 0
!psym = 4
!fancy = 4
plot, shiftnh,deltanh, $
      xtitle = xdnh, $
      ytitle = ydnh, $
      xstyle = 9, $
      xrange = [min(shiftnh)-2,max(shiftnh)+2], $
      yrange = [min(deltanh)-0.1,max(deltanh)+0.1], $
      xcharsize = 0.8, $
      ycharsize = 0.8
oploterror, shiftnh, deltanh, delterr, errstyle=0
!psym = 0
y = fltarr(n_elements(shiftnh))
x = findgen(max(shiftnh)+10)
oplot, x, y, linestyle=5
device, /close
set_plot, "X"

;******************************************************
;******************************************************
; PROJECTED DELTA TEMPERATURE VS TX-FIXED
;******************************************************
;******************************************************

ydnh = textoidl("Deprojected \DeltaN_{H}/N_{H}")
xdnh = textoidl("Deprojected T_{X} (N_{H}-fixed) (keV)")

set_plot, 'PS'
device, filename = "de_delnh_vs_tx.ps"
!linetype = 0
!psym = 4
!fancy = 4
plot, defro.tx,deltanh, $
      xtitle = xdnh, $
      ytitle = ydnh, $
      xstyle = 9, $
      xrange = [min(defro.tx)-0.5, max(defro.tx)+0.5], $
      yrange = [min(deltanh)-0.2, max(deltanh)+0.2], $
      xcharsize = 0.8, $
      ycharsize = 0.8
oploterror, defro.tx, deltanh, delterr, errstyle=0
!psym = 0
y = fltarr(n_elements(defro.tx))
x = findgen(max(defro.tx)+2)
oplot, x, y, linestyle=5
device, /close
set_plot, "X"

;******************************************************
;******************************************************
; MAKE ALL THE PS FILES INTO ONE PS FILE
;******************************************************
;******************************************************


SPAWN, 'ls pr_*.ps > list'
SPAWN, 'cat list | pscat pr-plots.ps'
SPAWN, 'rm -f pr_*.ps'

SPAWN, 'ls de_*.ps > list'
SPAWN, 'cat list | pscat de-plots.ps'
SPAWN, 'rm -f de_*.ps'

END
